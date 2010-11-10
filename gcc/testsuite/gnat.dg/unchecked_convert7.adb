-- { dg-do compile }
-- { dg-options "-g -gnatVa" }

with Unchecked_Conversion;

procedure Unchecked_Convert7 is

  type BPA is array (1 .. 23) of Boolean;
  pragma Pack (BPA);
  for BPA'Size use 23;

  subtype Byte is Natural range 0 .. 255;

  type R is
    record
      S : Boolean;
      E : Byte;
      F : BPA;
    end record;

  for R use
    record
      S at 0 range 0 .. 0;
      E at 0 range 1 .. 8;
      F at 0 range 9 .. 31;
    end record;
  for R'Size use 32;

  function Conversion
    is new Unchecked_Conversion (Source => R, Target => Float);

  F : Float := Conversion (R'(False, Byte'Last, (others => False)));

begin
  null;
end;
