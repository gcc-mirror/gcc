with Unchecked_Conversion;

package Atomic5 is

  type Byte is mod 2 ** 8;
  for Byte'Size use 8;

  type Unsigned_32 is mod 2 ** 32;
  for Unsigned_32'Size use 32;

  type R is record
    A,B,C,D : Byte;
  end record;
  for R'Alignment use 4;
  pragma Atomic (R);

  function Conv is new Unchecked_Conversion (R, Unsigned_32);

  procedure Proc1;

  procedure Proc2;

end Atomic5;
