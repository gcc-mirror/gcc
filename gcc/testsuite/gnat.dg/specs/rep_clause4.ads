-- { dg-do compile }
-- { dg-options "-O" }

package Rep_Clause4 is

  type Uns16 is mod 2**16;

  type Rec32 is
    record
      W1 : Uns16 := 1;
      W2 : Uns16 := 2;
    end record;
  for Rec32 use
    record
      W1 at 0 range 0..15;
      W2 at 2 range 0..15;
    end record;
  for Rec32'size use 32;

  type Rec48 is
    record
      W1andW2 : Rec32;
      W3      : Uns16;
    end record;
  for Rec48 use
    record
      W1andW2 at 0 range 0..31;
      W3      at 4 range 0..15;
    end record;
  for Rec48'size use 48;

  type Rec_Type is
    record
      Field1 : Rec48;
    end record;
  for Rec_Type use
    record
      Field1 at 0 range 0 .. 47;
    end record;
  for Rec_Type'size use 48;

end Rep_Clause4;
