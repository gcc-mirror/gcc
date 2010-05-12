-- { dg-do compile }
-- { dg-xfail-if "missing late warning" { *-*-* } { "-flto" } { "" } }

with System;

procedure frame_overflow is

   type Bitpos_Range_T is range 1..2**(System.Word_Size-1)-1;
   type Bitmap_Array_T is array (Bitpos_Range_T) of Boolean;

   type Bitmap_T is record
      Bits : Bitmap_Array_T := (others => False);
   end record;
   
   function -- { dg-error "too large" }
     Set_In (Bitmap : Bitmap_T; Bitpos : Bitpos_Range_T)  return Bitmap_T
   is
      Result: Bitmap_T := Bitmap; -- { dg-error "Storage_Error" }
   begin
      Result.Bits (Bitpos) := True;
      return Result;
   end;

   function Negate (Bitmap : Bitmap_T) return Bitmap_T is
      Result: Bitmap_T; -- { dg-error "Storage_Error" }
   begin
      for E in Bitpos_Range_T loop
        Result.Bits (E) := not Bitmap.Bits (E);
      end loop;
      return Result;
  end;

begin
   null;
end;
