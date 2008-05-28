--  { dg-do compile }
--  { dg-options "-gnatws" }
with FRUnaligned1; use FRUnaligned1;
function FRUnaligned return r is
   ss : s;
begin
   return ss.y;
end;
