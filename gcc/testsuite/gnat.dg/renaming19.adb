-- { dg-do run }

with Ada.Text_IO;
with Renaming19_Pkg;

procedure Renaming19 is

  Handler : aliased Renaming19_Pkg.Logging :=
    (Output => Ada.Text_IO.Current_Output);

  Full_Handler : aliased Renaming19_Pkg.Full_Logging :=
    (Output => Ada.Text_IO.Current_Output);

  Generic_Handler : access Renaming19_Pkg.Logging'Class := Handler'Access;

  procedure My_Log_3 (Msg : String) renames Generic_Handler.Log;
  procedure My_Log_4 (Msg : String; Err : Natural) renames Generic_Handler.Log;

begin
  My_Log_3 ("First");
  Generic_Handler := Full_Handler'Access;
  My_Log_3 ("Second");
  My_Log_4 ("Third", 3);
end;
