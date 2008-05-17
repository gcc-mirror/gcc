-- { dg-do run }
-- { dg-options "-O2 -gnatp" }

procedure Bit_Packed_Array3 is

   type Bitmap_T is array (1 .. 10) of Boolean;
   pragma Pack (Bitmap_T);

   type Maps_T is record
      M1 : Bitmap_T;
   end record;
   pragma Pack (Maps_T);
   for Maps_T'Size use 10;
   pragma Suppress_Initialization (Maps_T);

   Tmap : constant Bitmap_T := (others => True);
   Fmap : constant Bitmap_T := (others => False);
   Amap : constant Bitmap_T :=
     (1 => False, 2 => True, 3 => False, 4 => True, 5 => False,
      6 => True, 7 => False, 8 => True, 9 => False, 10 => True);

   function Some_Maps return Maps_T is
      Value : Maps_T := (M1 => Amap);
   begin
      return Value;
   end;
   pragma Inline (Some_Maps);

   Maps : Maps_T;
begin
   Maps := Some_Maps;

   for I in Maps.M1'Range loop
      if (I mod 2 = 0 and then not Maps.M1 (I))
        or else (I mod 2 /= 0 and then Maps.M1 (I))
      then
         raise Program_Error;
      end if;
   end loop;
end;
