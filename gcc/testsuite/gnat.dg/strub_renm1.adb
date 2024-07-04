--  { dg-do compile }
--  { dg-options "-fstrub=relaxed -fdump-ipa-strub" }
--  { dg-require-effective-target strub }

procedure Strub_Renm1 is
   V : Integer := 0;
   pragma Machine_Attribute (V, "strub");

   procedure P (X : Integer);
   pragma Machine_Attribute (P, "strub", "at-calls");

   function F return Integer;

   procedure Q (X : Integer) renames P;
   pragma Machine_Attribute (Q, "strub", "at-calls");

   function G return Integer renames F;
   pragma Machine_Attribute (G, "strub", "internal");

   procedure P (X : Integer) is null;
   function F return Integer is (0);

begin
   P (F);
   Q (G);
end Strub_Renm1;

--  This is for P; Q is an alias.
--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]at-calls\[)\]\[)\]" 1 "strub" } }

--  This is *not* for G, but for Strub_Renm1.
--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]wrapped\[)\]\[)\]" 1 "strub" } }
--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]wrapper\[)\]\[)\]" 1 "strub" } }
