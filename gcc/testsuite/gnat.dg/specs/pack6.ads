-- { dg-do compile }

with Ada.Finalization;
with Pack6_Pkg;

package Pack6 is

  package Eight_Bits is new Pack6_Pkg (8);

  type Some_Data is record
    Byte_1 : Eight_Bits.Object;
    Byte_2 : Eight_Bits.Object;
  end record;

  for Some_Data use record
    Byte_1 at 0 range 0 .. 7;
    Byte_2 at 1 range 0 .. 7;
  end record;

  type Top_Object is new Ada.Finalization.Controlled with record
    Data : Some_Data;
  end record;

end Pack6;
