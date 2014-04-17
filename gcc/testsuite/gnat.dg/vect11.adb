-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-optimized" }

package body Vect11 is

   function "+" (X, Y : Sarray) return Sarray is
      R : Sarray;
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

   procedure Add (X, Y : not null access Sarray; R : not null access Sarray) is
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (Ivdep);
         R(I) := X(I) + Y(I);
      end loop;
   end;

end Vect11;

-- { dg-final { scan-tree-dump-not "goto" "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
