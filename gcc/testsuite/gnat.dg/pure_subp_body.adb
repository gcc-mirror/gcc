--  { dg-do compile }

with Pure_Subp_Body_Pkg;

procedure Pure_Subp_Body with Pure is
begin
    null;
end Pure_Subp_Body;

--  cannot depend on "Pure_Subp_Body_Pkg" (wrong categorization)
--  { dg-error "cannot depend on \"Pure_Subp_Body_Pkg\" \\(wrong categorization\\)" "" { target *-*-* } 3 }
--  { dg-error "pure unit cannot depend on non-pure unit" "non-pure unit" { target *-*-* } 3 }
