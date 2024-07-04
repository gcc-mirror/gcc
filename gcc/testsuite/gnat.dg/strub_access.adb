--  { dg-do compile }
--  { dg-options "-fstrub=relaxed -fdump-ipa-strubm" }
--  { dg-require-effective-target strub }

--  The main subprogram doesn't read from the automatic variable, but
--  being an automatic variable, its presence should be enough for the
--  procedure to get strub enabled.

procedure Strub_Access is
   type Strub_Int is new Integer;
   pragma Machine_Attribute (Strub_Int, "strub");
   
   X : aliased Strub_Int := 0;

   function F (P : access Strub_Int) return Strub_Int is (P.all);

begin
   X := F (X'Access);
end Strub_Access;

--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]internal\[)\]\[)\]" 1 "strubm" } }
--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]at-calls-opt\[)\]\[)\]" 1 "strubm" } }
