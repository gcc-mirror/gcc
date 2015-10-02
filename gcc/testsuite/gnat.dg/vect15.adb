-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-vect-details" }

package body Vect15 is

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

end Vect15;

-- { dg-final { scan-tree-dump-not "possible aliasing" "vect"  } }
