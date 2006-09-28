-- { dg-do run }

procedure self_aggregate_with_pointer is

  type Arr is array (Natural range <>) of Integer;

  type Rec (N : Natural) is record
    A : Arr (1..N);
  end record;

  type Acc_Rec is access all Rec;

  type SRec is record
    A : Acc_Rec;
    I1, I2, I3, I4, I5, I6, I7: Integer;
  end record;

  R : aliased Rec (1);
  S : Srec := (A => R'Access, others => 0);

begin
  S := (A => S.A, others => 0);
  if S.A /= R'Access then
    raise Program_Error;
  end if;
end;
