-- { dg-do run }

procedure Packed_Subtype is

   subtype Ubyte is Integer range 0 .. 255;
   type Packet (Id : Ubyte) is record
      A, B : Ubyte;
   end record;
   pragma Pack (Packet);

   subtype My_Packet is Packet (Id => 1);

   MP : My_Packet;
begin
   MP.A := 1;
   MP.B := 2;

   if MP.A /= 1 or else MP.B /= 2 then
      raise Program_Error;
   end if;
end;



