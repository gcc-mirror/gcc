package Opt1 is

   type Dimention_Length is array (1 .. 16) of Natural;

   type Dimension_Indexes is array (Positive range <>) of Positive;

   function De_Linear_Index
     (Index       : Natural;
      D           : Natural;
      Ind_Lengths : Dimention_Length)
      return Dimension_Indexes;

end Opt1;
