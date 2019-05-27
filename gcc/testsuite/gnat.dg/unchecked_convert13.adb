-- { dg-do compile }

with Ada.Unchecked_Conversion;

procedure Unchecked_Convert13 is

  type B16_T is mod 2 ** 16;
  for B16_T'Size use 16;
  for B16_T'Alignment use 1;

  type Rec_T is record
    A : Short_Integer;
  end record;
  for Rec_T use record
    A at 0 range 0 .. 15;
  end record;
  for Rec_T'Size use 16;

  Rec : constant Rec_T := (A => 0);

  function Rec_To_B16 is new Ada.Unchecked_Conversion (Rec_T, B16_T);

  procedure Nested (B16 : B16_T) is
  begin
    null;
  end;

begin
  Nested (Rec_To_B16 (Rec));
end;
