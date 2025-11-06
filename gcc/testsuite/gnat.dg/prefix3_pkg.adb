package body Prefix3_Pkg is

  My_Handler : aliased Logging := (Output => Ada.Text_IO.Current_Output);
  
  My_Generic_Handler : Logging_Class := My_Handler'Access;

  procedure Log (Handler : Logging; Msg : String) is
  begin
    Ada.Text_IO.Put_Line (Handler.Output.all, Msg);
  end Log;

  function Handler return Logging_Class is (My_Generic_Handler);

  procedure Handler (To : Logging_Class) is null;

end Prefix3_Pkg;
