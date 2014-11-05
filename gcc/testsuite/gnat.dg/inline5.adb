-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }
-- { dg-warning "not marked Inline" "" { target *-*-* } 0 }
-- { dg-warning "cannot be inlined" "" { target *-*-* } 0 }

with Inline5_Pkg; use Inline5_Pkg;

procedure Inline5 is
begin
  Test (0);
end;
