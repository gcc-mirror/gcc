-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }
-- { dg-warning "not marked Inline" "" { target *-*-* } 0 }
-- { dg-warning "cannot be inlined" "" { target *-*-* } 0 }

with Inline7_Pkg1; use Inline7_Pkg1;

procedure Inline7 is
begin
  Test (0);
end;
