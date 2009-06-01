-- { dg-do compile }
-- { dg-final { scan-assembler-not "elabs" } }

package body OCONST5 is

   procedure Check (Arg : R; Bit : U1) is
   begin
      if Arg.Bit /= Bit
        or else Arg.Agg.A /= 3
        or else Arg.Agg.B /= 7
      then
         raise Program_Error;
      end if;
   end;
end;
