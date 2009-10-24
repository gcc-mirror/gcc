-- { dg-do run }
-- { dg-options "-O2" }

procedure Opt4 is

  type Rec (D : Natural) is record
    S : String (1..D);
  end record;

  procedure Test (R : Rec) is
  begin
    if R.D /= 9 then
      raise Program_Error;
    end if;
  end;

  R : Rec(9);

begin
  R := (9, "123456789");
  Test (R);
end;
