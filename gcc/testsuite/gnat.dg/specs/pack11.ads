-- { dg-do compile }

with Ada.Strings.Bounded;

package Pack11 is

  package My_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (4);
  subtype My_Bounded_String is My_Strings.Bounded_String;

  type Rec1 is tagged null record;

  type Rec2 is record
    S : My_Bounded_String;
  end record;
  pragma Pack (Rec2);

  type Rec3 is new Rec1 with record
    R : Rec2;
  end record;

end Pack11;
