-- { dg-do compile }
-- { dg-options "-O" }

with Ada.Unchecked_Conversion;

package UC2 is

  subtype Word_Type is Integer range 0 .. 0;
  type Arr is array (1 .. Word_Type'Size) of Boolean;
  pragma Pack(Arr);

  function Conv is
     new Ada.Unchecked_Conversion (Source => Arr, Target => Word_Type);

  A : Arr;
  W : Word_Type := Conv(A);

end UC2;
