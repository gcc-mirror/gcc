--  { dg-do compile }
--  { dg-options "-gnatws" }

with Pack23_Pkg;

function Pack23 return Integer is

  type Arr is array (1 .. 32) of Boolean with Size => 32, Pack;

  A : Arr;

begin
  return Pack23_Pkg.Func (A (1));
end;
