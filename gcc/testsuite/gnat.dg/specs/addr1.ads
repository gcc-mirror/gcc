-- { dg-do compile }

with Interfaces; use Interfaces;

package Addr1 is

  type Arr is array (Integer range <>) of Unsigned_16;

  type Rec1 is record
    I1, I2: Integer;
  end record;

  type Rec2 is record
    I1, I2: Integer;
  end record;
  for Rec2'Size use 64;

  A: Arr (1 .. 4);

  Obj1: Rec1;
  for Obj1'Address use A'Address; -- { dg-bogus "alignment" }

  Obj2: Rec2;
  for Obj2'Address use A'Address; -- { dg-bogus "alignment" }

  Obj3: Rec1;
  for Obj3'Address use A(1)'Address; -- { dg-bogus "alignment" }

  Obj4: Rec1;
  for Obj4'Address use A(2)'Address; -- { dg-warning "(alignment|erroneous)" }

  Obj5: Rec1;
  for Obj5'Address use A(3)'Address; -- { dg-bogus "alignment" }

end Addr1;
