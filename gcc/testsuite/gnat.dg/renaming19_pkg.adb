package body Renaming19_Pkg is

  procedure Log (Handler : Logging; Msg : String) is
  begin
    Ada.Text_IO.Put_Line (Handler.Output.all, Msg);
  end Log;

  procedure Log (Handler : Logging; Msg : String; Err : Natural) is
  begin
     Ada.Text_IO.Put_Line (Handler.Output.all, Msg & Err'Image);
  end Log;

  procedure Log (Handler : Full_Logging; Msg : String) is
  begin
      raise Program_Error;
  end Log;

end Renaming19_Pkg;
