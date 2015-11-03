package Pack22_Pkg is

   type byte is mod 256;
   Temp_buffer : array (0..8) of byte:= (others => 0);
   for Temp_buffer'Alignment use 2;

   subtype Id is Short_integer;

   generic
      Dummy : Integer := 0;
   package Bit_Map_Generic is

      type List is private;
      function "xor" (L, R : List) return List;

   private
      type Offset_T is range 0 .. Id'Last;
      type Counter_T is new short_integer;
      for Counter_T'Size use 16;

      type Bit_List is array (Id range <>) of Boolean;
      pragma Pack (Bit_List);

      type List_Counter_T (Is_Defined : Boolean := True) is
         record
            Dummy : Boolean := False;
            case Is_Defined is
               when True =>
                  Counter : Counter_T := 0;
               when False =>
                  null;
            end case;
         end record;
      for List_Counter_T use
         record
            Is_Defined at 0 range 0 .. 7;
            Dummy at 1 range 0 .. 7;
            Counter at 2 range 0 .. 15;
         end record;

      type List is
         record
            Offset : Offset_T := Offset_T (1) - 1;
            Counter : List_Counter_T;
            Bits : Bit_List (1 .. 6);
         end record;
      for List use
         record
            Offset at 0 range 0 .. 15;
            Counter at 2 range 0 .. 31;
         end record;

      type Iterator is
         record
            No_More_Id : Boolean := True;
            Current_Id : Id;
            The_List : List;
         end record;

   end Bit_Map_Generic;

end Pack22_Pkg;
