package body Opt99_Pkg1 is

  procedure Set (D: in out Derived; C1, C2 : My_Character) is
  begin
    D.I  := 0;
    D.C1 := C1;
    D.C2 := C2;
  end;

end Opt99_Pkg1;
