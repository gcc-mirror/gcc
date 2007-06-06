--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;

procedure test_fixed_io is
   type FX is delta 0.0001 range -3.0 .. 250.0;
   for FX'Small use 0.0001;
   package FXIO is new Fixed_IO (FX);
   use FXIO;
   ST : String (1 .. 11)  := (others => ' ');
   ST2 : String (1 .. 12) := (others => ' ');

   N : constant FX := -2.345;
begin
   begin
      Put (ST, N, 6, 2);
      Put_Line ("*ERROR* Test1: Exception Layout_Error was not raised");
      Put_Line ("ST = """ & ST & '"');
   exception
      when Layout_Error =>
         null;
      when others =>
         Put_Line ("Test1: Unexpected exception");
   end;

   begin
      Put (ST2, N, 6, 2);
   exception
      when Layout_Error =>
         Put_Line ("*ERROR* Test2: Exception Layout_Error was raised");
      when others =>
         Put_Line ("Test2: Unexpected exception");
   end;
end;
