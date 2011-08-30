package Aggr3_Pkg is

   type Root is abstract tagged null record;

   type T is new Root with null record;

   My_T : T;

end Aggr3_Pkg;
