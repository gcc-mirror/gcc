-- { dg-do compile }

with Unchecked_Conversion;

package Static_Initializer3 is

  type Byte is range 0 .. 16#FF#;
  for Byte'Size use 8;

  type Word is range 0 .. 16#FFFF# ;
  for Word'Size use 16;

  type R is record
    b1 : Boolean;
    b2 : Boolean;
  end record;
  for R use record
    b1 at 0 range 0..3;
    b2 at 0 range 4..7;
  end record;
  for R'Size use 8;

  function Conv is new Unchecked_Conversion (R, Byte);

  C1 : constant Byte := Conv ((true, false));

  C2 : constant Word := Word(C1);

end Static_Initializer3;
