-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-cunrolli-details" }

package body Unroll2 is

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

end Unroll2;

-- { dg-final { scan-tree-dump-times "loop with 3 iterations completely unrolled" 2 "cunrolli" } }
