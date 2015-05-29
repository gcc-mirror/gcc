-- { dg-do compile }
-- { dg-options "-O2 -fno-inline -fdump-tree-optimized" }

procedure Opt39 (I : Integer) is

  type Rec is record
    I1 : Integer;
    I2 : Integer;
    I3 : Integer;
    I4 : Integer;
    I5 : Integer;
  end record;

  procedure Set (A : access Rec; I : Integer) is
    Tmp : Rec := A.all;
  begin
    Tmp.I1 := I;
    A.all := Tmp;
  end;

  R : aliased Rec;

begin
  Set (R'Access, I);
  if R.I1 /= I then
    raise Program_Error;
  end if;
end;

-- { dg-final { scan-tree-dump-times "MEM" 1 "optimized" } }
