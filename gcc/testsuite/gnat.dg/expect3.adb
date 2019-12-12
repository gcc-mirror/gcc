--  { dg-do run }

with Ada.Text_IO;

with GNAT.Expect.TTY;
with GNAT.OS_Lib;

procedure Expect3 is
   Pid    : GNAT.Expect.TTY.TTY_Process_Descriptor;
   Args   : GNAT.OS_Lib.Argument_List (1 .. 0);
   Result : GNAT.Expect.Expect_Match;

begin
   Pid.Non_Blocking_Spawn ("true", Args);

   begin
      Pid.Expect (Result, ".*");

      raise Program_Error;

   exception
      when GNAT.Expect.Process_Died =>
         declare
            File : Ada.Text_IO.File_Type;

         begin
            Ada.Text_IO.Create (File);
            Pid.Close;
            Ada.Text_IO.Put_Line (File, "Test of write operation");
            Ada.Text_IO.Close (File);
         end;
   end;
end Expect3;