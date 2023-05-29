-- { dg-do compile }

with System.Storage_Elements; use System.Storage_Elements;
with System;

package Storage_Offset1 is

  type Rec is record
    I1, I2 : Integer;
  end record;

  type Ptr is access all Rec;

  R : Ptr := new Rec;

  Offset : constant Storage_Offset := R.I1'Address - R.I2'Address;

end Storage_Offset1;
