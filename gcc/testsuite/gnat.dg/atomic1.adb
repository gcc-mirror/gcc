-- { dg-do compile }
-- { dg-options "-O0 -fdump-tree-gimple" }

with Atomic1_Pkg; use Atomic1_Pkg;

procedure Atomic1 is

   C_16 : constant R16 := (2, 3, 5, 7);
   C_32 : constant R32 := (1, 1, 2, 3, 5, 8, 13, 5);

begin
   V_16 := C_16;
   V_32 := C_32;
end;

-- { dg-final { scan-tree-dump-times "v_16" 1 "gimple"} }
-- { dg-final { scan-tree-dump-times "v_32" 1 "gimple"} }
