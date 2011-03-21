-- { dg-do run }
-- { dg-options "-O2" }

procedure Opt14 is

  type Rec is record
    I1, I2, I3 : Integer;
  end record;

  type Ptr is access Rec;

  P : Ptr := new Rec'(0,0,0);

  procedure Sub (R : In Out Rec) is
  begin
    R.I3 := R.I3 - 1;
  end;

begin
  P.all := (1,2,3);
  Sub (P.all);
  if P.all /= (1,2,2) then
    raise Program_Error;
  end if;
end;
