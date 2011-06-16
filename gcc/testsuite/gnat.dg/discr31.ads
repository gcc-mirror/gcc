package Discr31 is

   type Byte_List_Type is array(Positive range <>) of Integer;

   type Log_Item_Type(Last : Natural) is record
      Data : Byte_List_Type(1 .. Last) := (others => 0);
      Link : Natural := 0;
   end record;

   type Packet_Data_Type is access Log_Item_Type;

   function Log_Item(Packet : in Packet_Data_Type) return Log_Item_Type;

end Discr31;
