--  { dg-do compile }
--  { dg-options "-gnatn" }

package body Generic_Inst8 is

   package My_G is new Generic_Inst8_G (0);

end Generic_Inst8;
