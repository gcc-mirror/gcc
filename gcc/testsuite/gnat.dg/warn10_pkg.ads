package Warn10_Pkg is

   Size : constant Natural := 100;
   type My_Array is array(1..Size, 1..Size) of Float;

   type Root is tagged record
      Input_Values : My_Array;
   end record;

   function Get_Input_Value( Driver : Root; I, J : Natural) return Float;

end Warn10_Pkg;
