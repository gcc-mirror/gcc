with Unchecked_Conversion;

package Rep_Clause2 is

   type Tiny is range 0 .. 3;
   for Tiny'Size use 2;

   type Small is range 0 .. 255;
   for Small'Size use 8;

   type Small_Data is record
      D : Tiny;
      N : Small;
   end record;
   pragma Pack (Small_Data);

   type Chunk is
   record
      S : Small_Data;
      C : Character;
   end record;

   for Chunk use record
      S at 0 range  0 .. 15;
      C at 2 range  0 .. 7;
   end record;

   type Index is range 1 .. 10;

   type Data_Array is array (Index) of Chunk;
   for Data_Array'Alignment use 2;
   pragma Pack (Data_Array);

   type Data is record
     D : Data_Array;
   end record;

   type Bit is range 0 .. 1;
   for Bit'Size use 1;

   type Bit_Array is array (Positive range <>) of Bit;
   pragma Pack (Bit_Array);

   type Byte is new Bit_Array (1 .. 8);
   for  Byte'Size use 8;
   for  Byte'Alignment use 1;

   function Conv
     is new Unchecked_Conversion(Source => Small, Target => Byte);

   procedure Assign (From : Data; Offset : Positive; I : Index; To : out Bit_Array);

end Rep_Clause2;
