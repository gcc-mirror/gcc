--  { dg-do compile }

package body Overriding_Ops2 is
   overriding procedure Finalize (Self : in out Consumer) is
   begin
      null;
   end Finalize;
end Overriding_Ops2;
