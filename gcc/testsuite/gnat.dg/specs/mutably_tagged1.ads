-- { dg-do compile }
-- { dg-options "-gnatX0" }

package Mutably_Tagged1 is

  generic
    type T is private;
  package G is
  end G;

  type Rec is tagged null record with Size'Class => 128;

  package My_G is new G (Rec'Class);

end Mutably_Tagged1;
