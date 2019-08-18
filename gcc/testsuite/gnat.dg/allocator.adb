--  { dg-do compile }

procedure Allocator is
   type Object_Type      is not null access all Integer;
   type Object_Array     is array (Positive range <>) of Object_Type;
   type Object_Array_Ptr is access Object_Array;
   type Data_Ptr         is access Object_Array_Ptr;
   Copy : Data_Ptr := new Object_Array_Ptr;
begin
   Copy.all := new Object_Array (1..2);
end;
