package Array7 is

   package Range_Subtype is
      type Arr is array (Positive range <>) of Integer;
      type Arr_Acc is access Arr;

      subtype My_Range is Integer range 1 .. 25;
      function Get_Arr (Nbr : My_Range) return Arr_Acc;
   end;

   package Range_Type is

      type My_Range is range 1 .. 25;
      type Arr is array (My_Range range <>) of Integer;
      type Arr_Acc is access Arr;

      function Get_Arr (Nbr : My_Range) return Arr_Acc;
   end;

end Array7;
