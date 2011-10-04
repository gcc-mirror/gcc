with System;

package Frame_Overflow is

   type Bitpos_Range_T is range 1..2**(System.Word_Size-1)-1;
   type Bitmap_Array_T is array (Bitpos_Range_T) of Boolean;

   type Bitmap_T is record
      Bits : Bitmap_Array_T := (others => False);
   end record;

   function
     Set_In (Bitmap : Bitmap_T; Bitpos : Bitpos_Range_T)  return Bitmap_T;

   function Negate (Bitmap : Bitmap_T) return Bitmap_T;

end Frame_Overflow;
