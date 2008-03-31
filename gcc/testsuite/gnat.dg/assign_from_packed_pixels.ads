
package Assign_From_Packed_Pixels is

   type U16 is mod 2 ** 16;

   type Position is record
      X, Y, Z : U16;
   end record;
   for Position'Size use 48;

   type Pixel is record
      Pos : Position;
   end record;
   pragma Pack (Pixel);

   Minus_One : Integer := -1;
   Pix : Pixel := (Pos => (X => 0, Y => 0, Z => 0));
end;
