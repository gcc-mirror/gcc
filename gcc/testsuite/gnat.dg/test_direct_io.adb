-- { dg-do run }
with Ada.Direct_IO;

procedure Test_Direct_IO is

   package BDIO is new Ada.Direct_IO (Boolean);
   use BDIO;

   FD : File_Type;

begin
   Create (FD, Form => "shared=yes");
   Reset (FD);
   Close (FD);
end Test_Direct_IO;
