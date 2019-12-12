-- { dg-do compile }
-- { dg-options "-gnatws" }

with Ada.Unchecked_Conversion;

package Unchecked_Convert2 is

  type Address is access String;
  for Address'Size use Standard'Address_Size;

  type Rec is record
    A : Address;
  end record;

  function To_Integer is new Ada.Unchecked_Conversion (Address, Integer);

  function F (R : Rec) return Integer is (To_Integer (R.A));

end Unchecked_Convert2;
