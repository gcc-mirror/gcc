-- { dg-do compile }
-- { dg-options "-O" }

package body Opt78 is

   procedure Proc (P : UC; Msg : String) is
      Default : UC := (1, "!");
   begin
      if P = Default then
         raise Program_Error;
      else
         raise Constraint_Error;
      end if;
   end;

end Opt78;
