-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Pack21 is

  type Enum is (ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX,
                SEVEN, EIGHT, NINE, TEN, ELEVEN, TWELVE,
                THIRTEEN, FOURTEEN, FIFTEEN);

  type Rec1 is record
    I1 : INTEGER range 0 .. 800;
    I2 : INTEGER range 0 .. 15 := 0;
    E  : Enum;
  end record;
  pragma PACK (Rec1);

  type Rec2 is record
    F : Rec1;
  end record;

  for Rec2 use record
    F at 0 range 2 .. 19;
  end record;

  R1, R2 : Rec2;

begin
  null;
end;
