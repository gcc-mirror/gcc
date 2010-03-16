-- { dg-do run { target { ! "*-*-solaris2*" } } }

with GNAT.Sockets; use GNAT.Sockets;
procedure socket1 is
   X : Character;
begin
   X := 'x';
   GNAT.Sockets.Initialize;
   declare
      H : Host_Entry_Type := Get_Host_By_Address (Inet_Addr ("127.0.0.1"));
   begin
      null;
   end;
end socket1;
