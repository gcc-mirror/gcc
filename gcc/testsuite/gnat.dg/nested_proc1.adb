-- { dg-do run }
-- Test that a static link is correctly passed to a subprogram which is
-- indirectly called through an aggregate.

procedure Nested_Proc1 is

  I : Integer := 0;

  procedure P1 (X : Integer) is
  begin
    I := X;
  end;

  type Func_Ptr is access procedure (X : Integer);

  type Arr is array (1..64) of Integer;

  type Rec is record
    F : Func_Ptr;
    A : Arr;
  end record;

  procedure P2 (R : Rec) is
  begin
     R.F (1);
  end;

begin
  P2 ((F => P1'Access, A => (others => 0)));
  if I /= 1 then
    raise Program_Error;
  end if;
end;
