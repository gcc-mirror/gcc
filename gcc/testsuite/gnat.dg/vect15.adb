-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-vect-details" }

package body Vect15 is

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      R(1) := X(5) + Y(5);
      for I in 1 .. 4 loop
         R(I + 1) := X(I) + Y(I);
      end loop;
   end;

end Vect15;

-- { dg-final { scan-tree-dump-not "possible aliasing" "vect"  } }
