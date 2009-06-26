package Array7 is

   type Arr is array (Positive range <>) of Integer;
   type Arr_Acc is access Arr;

   subtype My_Range is Integer range 1 .. 25;

   function Get_Arr (Nbr : My_Range) return Arr_Acc;

end Array7;
