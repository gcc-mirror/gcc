-- { dg-do compile }
-- { dg-options "-gnatws" }

with System;
with System.Storage_Elements;
with Unchecked_Conversion;

package UC1 is

  function Conv is
    new Unchecked_Conversion (Source => System.Address, Target => Integer);
  function Conv is
    new Unchecked_Conversion (Source => Integer, Target => System.Address);

  M : constant System.Address := System.Storage_Elements.To_Address(0);
  N : constant System.Address := Conv (Conv (M) + 1);
  A : constant System.Address := Conv (Conv (N) + 1);

  I : Integer;
  for I use at A;

end UC1;
