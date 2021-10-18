-- { dg-do compile }
-- { dg-options "-O2 -funroll-all-loops -fno-tree-vectorize -fdump-rtl-loop2_unroll-details -fdump-tree-cunrolli-details" }

package body Unroll1 is

   function "+" (X, Y : Sarray) return Sarray is
      R : Sarray;
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (No_Unroll);
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (No_Unroll);
         R(I) := X(I) + Y(I);
      end loop;
   end;

end Unroll1;

-- { dg-final { scan-tree-dump-times "Not unrolling loop .: user didn't want it unrolled completely" 2 "cunrolli" } }
-- { dg-final { scan-rtl-dump-times "Not unrolling loop, user didn't want it unrolled" 2 "loop2_unroll" } }
