package Bit_Packed_Array4 is

   type Data_Type is array (1 .. 39) of Boolean;
   pragma Pack (Data_Type);
   for Data_Type'Alignment use 1;

   type Message_Type is record
      Valid : Boolean;
      Data  : Data_Type;
   end record;
   for Message_Type use record
      Valid at 0 range 0 .. 0;
      Data  at 0 range 1 .. 39;
   end record;

   procedure Process (M : Message_Type);

end Bit_Packed_Array4;
