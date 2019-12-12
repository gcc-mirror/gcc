-- { dg-do compile }
-- { dg-error "not marked 'Inline_Always'" "" { target *-*-* } 0 }
-- { dg-error "cannot be inlined" "" { target *-*-* } 0 }

with Inline3_Pkg; use Inline3_Pkg;

procedure Inline3 is
begin
  Test (0);
end;
