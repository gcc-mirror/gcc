-- { dg-do compile }
-- { dg-options "-cargs --param max-completely-peeled-insns=200 -margs -O3" }

package body Opt42 is

   function "*" (Left, Right : in Array_Type) return Array_Type is
      Temp   : Float;
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Temp := 0.0;
            for K in Index_Type loop
               Temp := Temp + Left (I) (K) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end "*";

end Opt42;
