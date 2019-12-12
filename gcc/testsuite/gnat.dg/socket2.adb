-- { dg-do run }
with GNAT.Sockets;
procedure Socket2 is
   Address : GNAT.Sockets.Sock_Addr_Type;
   Server_Socket : GNAT.Sockets.Socket_Type;
begin
   Address.Addr := GNAT.Sockets.Any_Inet_Addr;
   Address.Port := 16#1234#;
   GNAT.Sockets.Create_Socket (Server_Socket);
   GNAT.Sockets.Set_Socket_Option
     (Server_Socket,
      GNAT.Sockets.Socket_Level,
      (GNAT.Sockets.Reuse_Address, True));
   GNAT.Sockets.Bind_Socket (Server_Socket, Address);
   GNAT.Sockets.Close_Socket (Server_Socket);
end Socket2;
