-- { dg-do run }
-- { dg-options "-gnat12" }

procedure In_Out_Parameter3 is

  type Arr is array (1..16) of Integer;

  type Rec1 is record
    A : Arr;
    B : Boolean;
  end record;

  type Rec2 is record
    R : Rec1;
  end record;
  pragma Pack (Rec2);

  function F (I : In Out Rec1) return Boolean is
    A : Integer := I.A (1);
  begin
    I.A (1) := I.A (1) + 1;
    return (A > 0);
  end;

  I : Rec2 := (R => (A => (others => 0), B => True));
  B : Boolean;

begin
  B := F (I.R);
  if B then
    raise Program_Error;
  end if;
  if I.R.A (1) /= 1 then
    raise Program_Error;
  end if;
  if F (I.R) = False then
     raise Program_Error;
  end if;
  if I.R.A (1) /= 2 then
    raise Program_Error;
  end if;
end;
