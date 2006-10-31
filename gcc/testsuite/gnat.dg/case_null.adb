-- { dg-do compile }
-- { dg-options "-gnatws" }

package body Case_Null is
   procedure P1 (X : T) is
   begin
      case X is
         when S1 =>
           null;
         when e =>
           null;
         when others =>
           null;
      end case;
   end P1;
end Case_Null;
