package Sizetype3 is

   type Values_Array is array (Positive range <>) of Integer;
   type Values_Array_Access is access all Values_Array;

   procedure Simplify_Type_Of;

end Sizetype3;
