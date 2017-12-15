--  { dg-do run }

procedure Component_Size is

   C_Unsigned_Long_Size : constant := 32;
   type T_Unsigned_Long is range 0 .. (2 ** 31) - 1;
   for T_Unsigned_Long'Size use C_Unsigned_Long_Size;

   C_Unsigned_Byte_Size : constant := 8;
   type T_Unsigned_Byte is range 0 .. (2 ** 8) - 1;
   for T_Unsigned_Byte'Size use C_Unsigned_Byte_Size;

   type T_Unsigned_Byte_Without_Size_Repr is range 0 .. (2 ** 8) - 1;

   C_Nb_Data : constant T_Unsigned_Long := 9;
   subtype T_Nb_Data is T_Unsigned_Long range 1 .. C_Nb_Data;
   
   type T_Wrong_Id is array (T_Nb_Data) of T_Unsigned_Byte;
   for T_Wrong_Id'Component_Size use C_Unsigned_Long_Size;

   type T_Correct_Id is array (T_Nb_Data) of T_Unsigned_Byte_Without_Size_Repr;
   for T_Correct_Id'Component_Size use C_Unsigned_Long_Size;  

   C_Value : constant := 1;

   C_Wrong_Id : constant T_Wrong_Id := T_Wrong_Id'(others => C_Value);
   C_Correct_Id : constant T_Correct_Id := T_Correct_Id'(others => C_Value);

begin
   if C_Correct_Id /= T_Correct_Id'(others => C_Value) then
      raise Program_Error;
   end if;

   if C_Wrong_Id /= T_Wrong_Id'(others => C_Value) then
      raise Program_Error;
   end if;
end;
