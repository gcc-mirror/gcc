-- { dg-do run }
-- { dg-options "-O2" }

with Concat1_Pkg; use Concat1_Pkg;

procedure Concat1 is

  Ident_1 : Integer := Ident (1);
  Ident_2 : Integer := Ident (2);
  Ident_5 : Integer := Ident (5);

  type Arr is array (Integer range <>) of Integer;
  A : Arr (1..10);

begin
  A := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  A := 0 & A(Ident_1..Ident_2) & A(Ident_1..Ident_2) & A(Ident_1..Ident_5);
  if A /= (0, 1, 2, 1, 2, 1, 2, 3, 4, 5) then
    raise Program_Error;
  end if;
end;
