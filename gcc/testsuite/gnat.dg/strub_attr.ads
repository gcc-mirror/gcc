package Strub_Attr is
   procedure P (X : Integer);
   pragma Machine_Attribute (P, "strub", "internal");

   function F (X : Integer) return Integer;
   pragma Machine_Attribute (F, "strub");

   X : Integer := 0;
   pragma Machine_Attribute (X, "strub");

   function G return Integer;
end Strub_Attr;
