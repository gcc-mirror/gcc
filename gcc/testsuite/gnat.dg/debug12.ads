package Debug12 is
   type Bit_Array is array (Positive range <>) of Boolean
      with Pack;
   A  : Bit_Array := (1 .. 10 => False);
   A2 : Boolean renames A (2);

   function Get_A2 return Boolean;
end Debug12;
