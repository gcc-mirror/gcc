-- { dg-do run }

procedure Renaming17 is

  function Incr (V : Integer; I : Integer := 1) return Integer is
    (V + I);

  function Incr_Ren (V : Integer; I : Positive := 1) return Positive
    renames Incr;

  I : Integer;

begin
  I := Incr_Ren (-3);
  I := Incr_Ren (-3, 2);
  I := Incr_Ren (-3, 0);
end;
