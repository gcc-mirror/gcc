--  { dg-do compile }
--  { dg-options "-fstrub=strict" }
--  { dg-require-effective-target strub }

--  This is essentially the same test as strub_attr.adb, 
--  but with an explicit conversion.

package body Strub_Ind2 is
   E : exception;

   function G return Integer;
   pragma Machine_Attribute (G, "strub", "callable");

   procedure P (X : Integer) is
   begin
      raise E;
   end;
   
   function G return Integer is (FP (X));

   type GT is access function return Integer;
   pragma Machine_Attribute (GT, "strub", "callable");

   type GT_SD is access function return Integer;
   pragma Machine_Attribute (GT_SD, "strub", "disabled");

   GP : GT_SD := GT_SD (GT'(G'Access));
   --  pragma Machine_Attribute (GP, "strub", "disabled"); -- not needed.

   function F (X : Integer) return Integer is
   begin
      return X * GP.all; --  { dg-error "using non-.strub. type" }
   end;
   
end Strub_Ind2;
