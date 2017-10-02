-- { dg-do compile }
-- { dg-options "-O" }

package body Opt55 is

   function Cond (B : Boolean; If_True, If_False : Date) return Date is
   begin
      if B then
         return If_True;
      else
         return If_False;
      end if;
   end;

   function F (C : Rec2; B : Boolean) return Date is
   begin
      return Cond (B, C.D1, C.D2);
   end;

end Opt55;
