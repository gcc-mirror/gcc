package Incomplete4_Pkg is

   type Circular_Type;
   type Ptr is access Circular_Type;
   type Circular_Type is array (1..100) of Ptr;

   A : Circular_Type;

end Incomplete4_Pkg;
