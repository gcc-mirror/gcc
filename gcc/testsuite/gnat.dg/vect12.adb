-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-vect-details" }

package body Vect12 is

   function "+" (X, Y : Sarray) return Sarray is
      R : Sarray;
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (No_Vector);
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (No_Vector);
         R(I) := X(I) + Y(I);
      end loop;
   end;

end Vect12;

-- { dg-final { scan-tree-dump-not "vectorized 1 loops" "vect"  } }
-- { dg-final { cleanup-tree-dump "vect" } }
