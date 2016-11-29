-- { dg-do run }
-- { dg-options "-O3" }
-- PR tree-optimization/71083
with Loop_Optimization23_Pkg;
use Loop_Optimization23_Pkg;
procedure Loop_Optimization23 is
  Test : ArrayOfStructB;
begin
  Test (0).b.b := 9999;
  Foo (Test);
  if Test (100).b.b /= 9999 then
    raise Program_Error;
  end if;
end;
