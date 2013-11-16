-- { dg-do run }

with Aggr21_Pkg; use Aggr21_Pkg;

procedure Aggr21 is
  V : Rec;
begin
  V.A := 12;
  V.S (1 .. 10) := "Hello init";
  V.N := 123;
  Init (V);
  --  Probably not reliable, but the compiler is supposed not to modify V.S
  pragma Assert (V.s (1 .. 5) = "Hello");
end;
