with System;

package SSO19_Pkg is

  subtype Small_Int is Short_Integer range -1000 .. 1000;

  type Data is record
    I : Small_Int;
    F : Float;
  end record;
  for Data use record
    I at 0 range 0 .. 15;
    F at 4 range 0 .. 31;
  end record;
  for Data'Bit_Order use System.High_Order_First;
  for Data'Scalar_Storage_Order use System.High_Order_First;

  type Rec is record
    D : Data;
  end record;

  function Is_Valid (Item : Rec) return Boolean;

end SSO19_Pkg;
