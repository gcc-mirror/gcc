-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }
-- { dg-warning "not marked Inline" "" { target *-*-* } 0 }
-- { dg-warning "cannot be inlined" "" { target *-*-* } 0 }

with Inline9_Pkg; use Inline9_Pkg;

procedure Inline9 is
begin
  Test (0);
end;
