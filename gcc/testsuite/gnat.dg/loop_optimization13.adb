-- { dg-do compile }
-- { dg-options "-O" }

with Loop_Optimization13_Pkg; use Loop_Optimization13_Pkg;

package body Loop_Optimization13 is

   function F (A : Rec) return Rec is
      N : constant Integer := A.V'Length / L;
      Res : Rec
        := (True, new Complex_Vector' (0 .. A.V'Length / L - 1 => (0.0, 0.0)));
   begin
      for I in 0 .. L - 1 loop
         for J in 0 .. N - 1 loop
            Res.V (J) := Res.V (J) + A.V (I * N + J);
         end loop;
      end loop;
      return Res;
   end;

end Loop_Optimization13;
