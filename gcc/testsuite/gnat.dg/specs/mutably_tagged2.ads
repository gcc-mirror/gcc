-- { dg-do compile }
-- { dg-options "-gnatX0" }

generic
package Mutably_Tagged2 is
   generic
   package Config is
      type Mutably_Tagged is tagged null record with Size'Class => 128;
   end Config;

   package My_Config is new Config;

   generic
      type T is private;
   package G is
   end G;

   package My_G is new G (My_Config.Mutably_Tagged'Class);
end Mutably_Tagged2;
