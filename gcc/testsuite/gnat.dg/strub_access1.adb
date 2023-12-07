--  { dg-do compile }
--  { dg-options "-fstrub=relaxed" }
--  { dg-require-effective-target strub }

--  Check that we reject 'Access of a strub variable whose type does
--  not carry a strub modifier.

procedure Strub_Access1 is
   X : aliased Integer := 0;
   pragma Machine_Attribute (X, "strub");

   function F (P : access Integer) return Integer is (P.all);
   
begin
   X := F (X'Unchecked_access); -- OK.
   X := F (X'Access); -- { dg-error "target access type drops .strub. mode" }
end Strub_Access1;
