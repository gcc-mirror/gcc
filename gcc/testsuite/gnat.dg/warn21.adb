--  { dg-do compile }
--  { dg-options "-gnata -gnatwa" }

package body Warn21 is
   procedure Foo is null;
end Warn21;
