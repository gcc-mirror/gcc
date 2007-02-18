-- { dg-do run }
-- { dg-options "-O2 -fno-unit-at-a-time" }

with Tail_Call_P; use Tail_Call_P;

procedure Tail_Call is
begin
  Insert (My_Array, 0, 0);
end;
