package Aggr24_Pkg is

   type Rec is record
      I1 : Integer;
      I2 : Integer;
      I3 : Integer;
      I4 : Integer;
      I5 : Integer;
      I6 : Integer;
      I7 : Integer;
      S : String (1 .. 5);
   end record;

   procedure Init (R : out Rec);

end Aggr24_Pkg;
