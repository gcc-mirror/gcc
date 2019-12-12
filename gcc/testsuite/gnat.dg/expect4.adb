--  { dg-do run }

with GNAT.Expect.TTY;
with GNAT.OS_Lib;

procedure Expect4 is
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
         null;
   end;

   begin
      Pid.Expect (Result, ".*");

      raise Program_Error;

   exception
      when GNAT.Expect.Process_Died =>
         null;
   end;

   Pid.Close;
end Expect4;