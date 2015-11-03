-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-vect-details" }

package body Vect18 is

   procedure Comp (X, Y : Sarray; R : in out Sarray) is
      Tmp : Long_Float := R(4);
   begin
      for I in 1 .. 3 loop
         R(I+1) := R(I) + X(I) + Y(I);
      end loop;
      R(1) := Tmp + X(4) + Y(4);
   end;

end Vect18;

-- { dg-final { scan-tree-dump "bad data dependence" "vect"  } }
