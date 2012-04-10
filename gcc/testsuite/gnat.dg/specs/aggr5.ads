-- { dg-do compile }

pragma Restrictions (No_Elaboration_Code);

package Aggr5 is

  type R is record
    C : Character;
    F : Float;
  end record;

  for R use record
    C at 0 range 0 .. 7;
    F at 1 range 0 .. 31;
  end record;

  My_R : R := (C => 'A', F => 1.0);

end Aggr5;
