--  { dg-do compile }

function Pure_Function2 (X : Integer) return Integer is
begin
   return X;
end Pure_Function2;

pragma Pure_Function (Pure_Function2);  --  { dg-error "pragma \"Pure_Function\" argument must be in same declarative part" }
pragma Pure_Function (Pure_Function2);  --  { dg-error "pragma \"Pure_Function\" argument must be in same declarative part" }
pragma Pure_Function (Pure_Function2);  --  { dg-error "pragma \"Pure_Function\" argument must be in same declarative part" }
