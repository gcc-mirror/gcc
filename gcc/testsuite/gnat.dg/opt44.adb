-- { dg-do compile }
-- { dg-options "-O" }

package body Opt44 is

   procedure Addsub (X, Y : Sarray; R : out Sarray; N : Integer) is
   begin
      for I in Sarray'Range loop
         pragma Loop_Optimize (Ivdep);
         pragma Loop_Optimize (Vector);
         if N > 0 then
           R(I) := X(I) + Y(I);
         else
           R(I) := X(I) - Y(I);
         end if;
      end loop;
   end;

end Opt44;
