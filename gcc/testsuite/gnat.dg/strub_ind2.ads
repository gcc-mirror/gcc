package Strub_Ind2 is
   procedure P (X : Integer);
   pragma Machine_Attribute (P, "strub", "internal");

   function F (X : Integer) return Integer;
   pragma Machine_Attribute (F, "strub");

   X : Integer := 0;
   pragma Machine_Attribute (X, "strub");

   type FT is access function (X : Integer) return Integer;
   pragma Machine_Attribute (FT, "strub", "at-calls");

   FP : FT := F'Access;
   pragma Machine_Attribute (FP, "strub", "at-calls");

end Strub_Ind2;
