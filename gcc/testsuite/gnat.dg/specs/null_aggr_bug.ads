--  { dg-do compile }
package Null_Aggr_Bug is
   
   type Rec1 is null record;
   
   type Rec2 is tagged null record;
   
   type Rec3 is new Rec2 with null record;
   
   X1 : Rec1 := (null record);
   Y1 : Rec1 := (others => <>);
   
   X2 : Rec2 := (null record);
   Y2 : Rec2 := (others => <>);
   
   X3 : Rec3 := (null record);
   Y3 : Rec3 := (others => <>);
   Z3 : Rec3 := (Rec2 with others => <>);

end Null_Aggr_Bug;
