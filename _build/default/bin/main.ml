open Unix

(*Funcã́o recebe um inet_addr e o retorna com +1 no endereço*)
let incrementar_endereco ip =
  let ip_str = string_of_inet_addr ip in
  let ip_in_list = String.split_on_char '.' ip_str in
  let split1, split2, split3, split4 =
    ( int_of_string (List.nth ip_in_list 0)
    , int_of_string (List.nth ip_in_list 1)
    , int_of_string (List.nth ip_in_list 2)
    , int_of_string (List.nth ip_in_list 3) )
  in
  if split4 = 255 then
    let split4 = 0 in
    let split3 = split3 + 1 in
    let ip_int = Printf.sprintf "%d.%d.%d.%d" split1 split2 split3 split4 in
    inet_addr_of_string ip_int
  else
    let split4 = split4 + 1 in
    let ip_int = Printf.sprintf "%d.%d.%d.%d" split1 split2 split3 split4 in
    inet_addr_of_string ip_int

let list_of_range inicio fim =
  let ip_inicio = Unix.inet_addr_of_string inicio in
  let ip_fim = Unix.inet_addr_of_string fim in
  let rec auxiliar acc ainicio afim =
    if ainicio > afim then acc
    else auxiliar (ainicio :: acc) (incrementar_endereco ainicio) afim
  in
  List.rev (auxiliar [] ip_inicio ip_fim)

(* Função para verificar se uma porta está aberta em um endereço IP *)
let porta_aberta ip porta =
  let sockaddr = ADDR_INET (Unix.inet_addr_of_string ip, porta) in
  try
    let sock = socket PF_INET SOCK_STREAM 0 in
    connect sock sockaddr ; close sock ; true
  with
  | Unix_error (ECONNREFUSED, _, _) ->
      false
  | Unix_error (error, _, _) ->
      Printf.printf "Erro: %s\n" (error_message error) ;
      false

let verifica_portas ip portas =
  List.map
    (fun porta -> if porta_aberta ip porta then string_of_int porta else "")
    portas

(* Função para realizar ping em um endereço IP *)
let ping ip =
  let cmd = Printf.sprintf "ping -W 1 -c 1 %s > /dev/null" ip in
  match Unix.system cmd with
  | Unix.WEXITED 0 ->
      true (* Ping bem sucedido *)
  | _ ->
      false (* Ping falhou *)

(* Função principal para verificar o range de IPs *)
let verificar_ip ip portas =
  let ip_string = Unix.string_of_inet_addr ip in
    if ping ip_string then (
      Printf.printf "%s \t %s\n%!" ip_string
        (String.concat " " (verifica_portas ip_string portas)))

let verificar_ips_range ips portas =
  let results = ref [] in
  let paralelo ip =
    let tarefa () = verificar_ip ip portas in
    results := (Domain.spawn tarefa) :: !results
  in
  List.iter paralelo ips;
  List.iter Domain.join !results


let () =
  let ip_inicio = "192.168.1.1" in
  let ip_fim = "192.168.1.100" in
  let ips = list_of_range ip_inicio ip_fim in
  let portas = [22; 80; 443] in
  verificar_ips_range ips portas
