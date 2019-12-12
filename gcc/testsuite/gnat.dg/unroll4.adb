-- { dg-do compile }
-- { dg-options "-O -fdump-rtl-loop2_unroll-details" }

package body Unroll4 is

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

end Unroll4;

-- { dg-final { scan-rtl-dump-times "optimized: loop unrolled 7 times" 2 "loop2_unroll" } }
