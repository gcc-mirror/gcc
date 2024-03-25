--  { dg-do compile }
--  { dg-options "-fstrub=strict" }
--  { dg-require-effective-target strub }

--  This is essentially the same test as strub_attr.adb, 
--  but applying attributes to access types as well.
--  That doesn't quite work yet, so we get an error we shouldn't get.

package body Strub_Ind is
   E : exception;

   function G return Integer;

   procedure P (X : Integer) is
   begin
      raise E;
   end;
   
   function F (X : Integer) return Integer is
   begin
      return X * X;
   end;
   
   function G return Integer is (FP (X));

   type GT is access function return Integer;

   type GT_SAC is access function return Integer;
   pragma Machine_Attribute (GT_SAC, "strub", "at-calls");

   GP : GT_SAC := GT_SAC (GT'(G'Access)); -- { dg-error "incompatible" }
   -- pragma Machine_Attribute (GP, "strub", "at-calls");

end Strub_Ind;
