--  { dg-do compile }

package body SPARK2 with SPARK_Mode is
   function Factorial (N : Natural) return Natural is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial (N - 1);
      end if;
   end Factorial;
end SPARK2;
