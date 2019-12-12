-- { dg-do compile }

package body Case_Optimization3 is

   procedure Proc (Val : T_RANGE) is
   begin
      case Val is
         when 0 =>
            raise Program_Error;
         when 1 =>
            null;
         when 2 =>
            null;
         when 3 =>
            null;
         when 4 =>
            null;
         when others =>
            null;
      end case;
   end;

end Case_Optimization3;

-- { dg-final { scan-assembler-not "__ucmpdi2" } }
