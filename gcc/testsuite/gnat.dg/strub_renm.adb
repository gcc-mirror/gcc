--  { dg-do compile }
--  { dg-require-effective-target strub }

procedure Strub_Renm is
   procedure P (X : Integer);
   pragma Machine_Attribute (P, "strub", "at-calls");

   function F return Integer;
   pragma Machine_Attribute (F, "strub", "internal");

   procedure Q (X : Integer) renames P; -- { dg-error "requires the same .strub. mode" }

   function G return Integer renames F;
   pragma Machine_Attribute (G, "strub", "callable"); -- { dg-error "requires the same .strub. mode" }

   procedure P (X : Integer) is null;
   function F return Integer is (0);

begin
   P (F);
   Q (G);
end Strub_Renm;
