with Pack13_Pkg;

package Pack13 is

  package Four_Bits is new Pack13_Pkg (4);
  package Thirty_Two_Bits is new Pack13_Pkg (32);

  type Object is private;
  type Object_Ptr is access all Object;

  procedure Set (Myself : Object_Ptr; The_Data : Thirty_Two_Bits.Object);

private

  type Some_Record is record
    Data_1     : Thirty_Two_Bits.Object;
    Data_2     : Thirty_Two_Bits.Object;
    Small_Data : Four_Bits.Object;
  end record;
  for Some_Record use record
    Data_1 at 0 range 0 .. 31;
    Data_2 at 4 range 0 .. 31;
    Small_Data at 8 range 0 .. 3;
  end record;

  type Object is record
    Something : Some_Record;
  end record;
  for Object use record
    Something at 0 range 0 .. 67;
  end record;

end Pack13;
