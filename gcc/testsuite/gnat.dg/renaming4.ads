package Renaming4 is

   type Big_Array is array (Natural range <>) of Integer;

   subtype Index is Natural range 1..4;
   subtype My_Array is Big_Array(Index);

   A : constant My_Array := (1, 2, 3, 4);

   subtype Small is Index range 1..2;
   subtype Small_Array is Big_Array(Small);

   B : Small_Array renames A(Index);

end Renaming4;
