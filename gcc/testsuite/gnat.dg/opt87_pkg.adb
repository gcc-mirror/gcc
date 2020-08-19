with Text_IO; use Text_IO;

package body Opt87_Pkg is

  procedure Print (Msg : String; Location : String) is
    Final_Msg : constant String :=
      Ascii.Cr & "info: " & Msg & " (" & Location & ")" & Ascii.Cr;
  begin
    Put_Line (Final_Msg);
  end;

end Opt87_Pkg;
