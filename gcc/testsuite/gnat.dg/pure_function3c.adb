-- { dg-do compile }
-- { dg-options "-O -gnatws -fdump-tree-optimized" }

with Pure_Function3_Pkg; use Pure_Function3_Pkg;

procedure Pure_Function3c is
   V : T;
begin
   if F_And_Set (V) = 1 then
      raise Program_Error;
   elsif F_And_Set (V) = 2 then
      raise Program_Error;
   end if;
end;

-- { dg-final { scan-tree-dump-times "pure_function3_pkg.f" 2 "optimized" } }
