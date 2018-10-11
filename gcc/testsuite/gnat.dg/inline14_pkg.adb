package body Inline14_Pkg is

  I : Integer;

  generic procedure Inner;

  procedure Inner is
  begin
    I := 0;
  end;

  procedure My_Inner is new Inner;

  procedure Proc renames My_Inner;

end Inline14_Pkg;
