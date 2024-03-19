--  { dg-do compile }
--  { dg-options "-fstrub=strict -fdump-ipa-strub" }
--  { dg-require-effective-target strub }

procedure Strub_Renm2 is
   V : Integer := 0;
   pragma Machine_Attribute (V, "strub");

   procedure P (X : Integer);
   pragma Machine_Attribute (P, "strub", "at-calls");

   function F return Integer;

   procedure Q (X : Integer) renames P;
   pragma Machine_Attribute (Q, "strub", "at-calls");

   type T is access function return Integer;

   type TC is access function return Integer;
   pragma Machine_Attribute (TC, "strub", "callable");

   FCptr : constant TC := TC (T'(F'Access));

   function G return Integer renames FCptr.all;
   pragma Machine_Attribute (G, "strub", "callable");

   procedure P (X : Integer) is null;
   function F return Integer is (0);

begin
   P (F);  -- { dg-error "calling non-.strub." }
   Q (G);  -- ok, G is callable.
end Strub_Renm2;
