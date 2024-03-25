--  { dg-do compile }
--  { dg-options "-fstrub=strict -fdump-ipa-strubm" }
--  { dg-require-effective-target strub }

-- We don't read from the automatic variable, but being an automatic
--  variable, its presence should be enough for the procedure to get
--  strub enabled.

with Strub_Attr;
procedure Strub_Var is
   X : Integer := 0;
   pragma Machine_Attribute (X, "strub");
begin
   X := Strub_Attr.F (0);
end Strub_Var;

--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]internal\[)\]\[)\]" 1 "strubm" } }
