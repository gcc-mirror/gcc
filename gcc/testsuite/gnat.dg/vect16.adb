-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-vect-details" }

package body Vect16 is

   procedure Add_Sub (X, Y : Sarray; R,S : out Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
         S(I) := X(I) - Y(I);
      end loop;
   end;

end Vect16;

-- { dg-final { scan-tree-dump-not "possible aliasing" "vect"  } }
