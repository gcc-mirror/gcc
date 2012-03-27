-- { dg-do run }

procedure Pack17 is

   type Bitmap_T is array (Natural range <>) of Boolean;
   pragma Pack (Bitmap_T);

   type Uint8 is range 0 .. 2 ** 8 - 1;
   for Uint8'Size use 8;

   type Record_With_QImode_Variants (D : Boolean) is record
      C_Filler : Bitmap_T (1..7);
      C_Map : Bitmap_T (1..3);
      case D is
         when False =>
            F_Bit : Boolean;
            F_Filler : Bitmap_T (1..7);
         when True =>
            T_Int : Uint8;
      end case;
   end record;
   pragma Pack (Record_With_QImode_Variants);

   procedure Fill (R : out Record_With_QImode_Variants) is
   begin
      R.C_Filler := (True, False, True, False, True, False, True);
      R.C_Map := (True, False, True);
      R.T_Int := 17;
   end;

   RT : Record_With_QImode_Variants (D => True);

begin
   Fill (RT);
   if RT.T_Int /= 17 then
     raise Program_Error;
   end if;
end;
