--  { dg-do run }

with System, Ada.Unchecked_Conversion; use System;

procedure BLKextract_From_Reg is

   type Byte is range 0 .. +255;
   for  Byte'size use 8;

   type RGB is array (1 .. 3) of Byte;
   for RGB'Size use 24;

   type RAW_Packet is range 0 .. 2 ** 32 - 1;
   for  RAW_Packet'Size use 32;

   type Composite_Packet is record
      Values : RGB;
      Pad    : Byte;
   end record;
   for Composite_Packet use record
      Values at 0 range 0 .. 23;
      Pad    at 3 range 0 .. 7;
   end record;
   for Composite_Packet'Size use 32;

   function To_Composite_Packet is
      new Ada.Unchecked_Conversion (RAW_Packet, Composite_Packet);

   function Blob return RGB is
      RAW_Blob : RAW_Packet := 16#01020304#;
   begin
      return To_Composite_Packet (RAW_Blob).Values;
   end;

   Blob_Color : RGB := Blob;
   Expected_Color : RGB;
begin
   if System.Default_Bit_Order = High_Order_First then
      Expected_Color := (1 => 1, 2 => 2, 3 => 3);
   else
      Expected_Color := (1 => 4, 2 => 3, 3 => 2);
   end if;
   
   for I in Blob_Color'Range loop
      if Blob_Color (I) /= Expected_Color (I) then
	 raise Program_Error;
      end if;
   end loop;
end;
