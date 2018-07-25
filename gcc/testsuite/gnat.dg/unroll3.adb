-- { dg-do compile }
-- { dg-options "-O -fdump-tree-cunroll-details" }

package body Unroll3 is

   function "+" (X, Y : Sarray) return Sarray is
      R : Sarray;
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (Unroll);
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (Unroll);
         R(I) := X(I) + Y(I);
      end loop;
   end;

end Unroll3;

-- { dg-final { scan-tree-dump-times "loop with 3 iterations completely unrolled" 2 "cunroll" } }
