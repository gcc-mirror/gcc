--  { dg-do compile }
--  { dg-options "-gnatwa" }
with Interfaces; use Interfaces;

package body Warn30 is
   procedure Incr (X : in out Interfaces.Integer_64) is
   begin
      X := X + 1;
   end Incr;
end Warn30;