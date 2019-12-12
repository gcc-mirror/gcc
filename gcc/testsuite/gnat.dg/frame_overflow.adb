-- { dg-do compile }

package body Frame_Overflow is

   function -- { dg-error "exceeds" }
     Set_In (Bitmap : Bitmap_T; Bitpos : Bitpos_Range_T)  return Bitmap_T
   is
      Result: Bitmap_T := Bitmap;
   begin
      Result.Bits (Bitpos) := True;
      return Result;
   end;

   function -- { dg-error "exceeds" }
     Negate (Bitmap : Bitmap_T) return Bitmap_T
   is
      Result: Bitmap_T;
   begin
      for E in Bitpos_Range_T loop
        Result.Bits (E) := not Bitmap.Bits (E);
      end loop;
      return Result;
  end;

end Frame_Overflow;
