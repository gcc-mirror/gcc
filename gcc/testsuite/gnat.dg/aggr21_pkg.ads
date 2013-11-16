package Aggr21_Pkg is

   type Rec is record
      A : Integer;
      S : String (1 .. 120);
      N : Natural;
   end record;

   procedure Init (R : out Rec);

end Aggr21_Pkg;
