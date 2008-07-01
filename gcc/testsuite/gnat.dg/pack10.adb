-- { dg-do run }

procedure Pack10 is

  type U16 is mod 2**16;
  type U8 is mod 2**8;

  type R is record
    A : U16;
    B : U8;
  end record;

  type M is array (1..2) of R;
  pragma Pack (M);
  -- This size clause can actually be omitted
  for M'Size use 48;

  type R2 is record
    C : M;
    D : U8;
  end record;
  for R2 use record
    C at 0 range 0 .. 24*2-1;
  end record;

  My_R2 : R2;

begin
  My_R2.D := 1;
  My_R2.C(2).B := 0;
  if My_R2.D /=1 then
    raise Program_Error;
  end if;
end;
