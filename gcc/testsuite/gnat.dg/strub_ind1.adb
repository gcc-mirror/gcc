--  { dg-do compile }
--  { dg-options "-fstrub=strict -fdump-ipa-strubm" }
--  { dg-require-effective-target strub }

--  This is essentially the same test as strub_attr.adb, 
--  but with an explicit conversion.

package body Strub_Ind1 is
   E : exception;

   type Strub_Int is New Integer;
   pragma Machine_Attribute (Strub_Int, "strub");

   function G return Integer;
   pragma Machine_Attribute (G, "strub", "disabled");

   procedure P (X : Integer) is
   begin
      raise E;
   end;
   
   function G return Integer is (FP (X));

   type GT is access function return Integer;
   pragma Machine_Attribute (GT, "strub", "disabled");

   type GT_SC is access function return Integer;
   pragma Machine_Attribute (GT_SC, "strub", "callable");

   GP : GT_SC := GT_SC (GT'(G'Access));
   --  pragma Machine_Attribute (GP, "strub", "callable"); -- not needed.

   function F (X : Integer) return Integer is
   begin
      return X * GP.all;
   end;
   
end Strub_Ind1;

--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]disabled\[)\]\[)\]" 1 "strubm" } }
--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]internal\[)\]\[)\]" 1 "strubm" } }
--  { dg-final { scan-ipa-dump-times "\[(\]strub\[)\]" 1 "strubm" } }
