--  { dg-do compile }
--  { dg-options "-gnatwa" }

package body Ghost7 is
   procedure Dummy is null;
end Ghost7;
