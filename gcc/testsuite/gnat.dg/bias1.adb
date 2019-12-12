--  { dg-do compile }
--  { dg-options "-cargs -g -dA -gnatws -fgnat-encodings=gdb -margs" }
--  { dg-final { scan-assembler "DW_AT_GNU_bias" } }

procedure Bias1 is
   type Small is range -7 .. -4;
   for Small'Size use 2;
   Y : Small := -5;
   Y1 : Small := -7;

   type Byte is mod 256;
   type Repeat_Count_T is new Byte range 1 .. 2 ** 6;
   for Repeat_Count_T'Size use 6;
   X : Repeat_Count_T := 64;
   X1 : Repeat_Count_T := 1;

   type Char_Range is range 65 .. 68;
   for Char_Range'Size use 2;
   Cval : Char_Range := 65;

   type SomePackedRecord is record
      R: Small;
      S: Small;
   end record;
   pragma Pack (SomePackedRecord);
   SPR : SomePackedRecord := (R => -4, S => -5);

   type Packed_Array is array (1 .. 3) of Small;
   pragma pack (Packed_Array);
   A : Packed_Array := (-7, -5, -4);

begin
   null;
end Bias1;