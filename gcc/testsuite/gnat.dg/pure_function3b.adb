-- { dg-do compile }
-- { dg-options "-O -gnatws -fdump-tree-optimized" }

with Pure_Function3_Pkg; use Pure_Function3_Pkg;

procedure Pure_Function3b is
   V : T;
begin
   if F (V) = 1 then
      raise Program_Error;
   end if;
   Set (V);
   if F (V) = 2 then
      raise Program_Error;
   end if;
end;

-- { dg-final { scan-tree-dump-times "pure_function3_pkg.f" 2 "optimized" } }
