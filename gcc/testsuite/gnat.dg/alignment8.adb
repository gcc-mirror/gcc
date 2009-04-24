-- { dg-do run }

with System;

procedure Alignment8 is

  type R is record
    I : Integer;
    F : Long_Long_Integer;
  end record;
  for R'Alignment use 8;

  procedure Q (A : System.Address) is
    F : Long_Long_Integer;
    for F'Address use A;
  begin
    F := 0;
  end;

  V : R;

begin
  Q (V.F'Address);
end;
