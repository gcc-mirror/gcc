-- { dg-do compile }

package body Discr9 is

   procedure Proc (From : in R; To : out R) is
   begin
      To := R'(D1 => False, D2 => From.D2, Field => From.Field);
   end;

end Discr9;
