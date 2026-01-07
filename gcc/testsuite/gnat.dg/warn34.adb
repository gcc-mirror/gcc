-- { dg-do compile }
-- { dg-options "-gnatwk" }

function Warn34 (F : Boolean) return String is
  S : String := -- { dg-warning "could be declared constant" }
        (if F then "foo" else "bar");
begin
  return S;
end;
