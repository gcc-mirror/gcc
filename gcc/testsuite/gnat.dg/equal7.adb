--  { dg-do compile }

with Equal7_Pkg; use Equal7_Pkg;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Equal7 is
   X : constant Integer := 42;

begin
   if F (X) /= "" & ASCII.LF then
       null;
   end if;
   if not (F (X) = "" & ASCII.LF) then
       null;
   end if;
end;

-- { dg-error "ambiguous expression \\(cannot resolve \"/=\"\\)" "" { target *-*-* } 9 }
-- { dg-error "possible interpretation at a-strunb.ads:\\d+" "" { target *-*-* } 9 }
-- { dg-error "possible interpretation in package Standard" "" { target *-*-* } 9 }

-- { dg-error "ambiguous expression \\(cannot resolve \"=\"\\)" "" { target *-*-* } 12 }
-- { dg-error "possible interpretation at a-strunb.ads:\\d+" "" { target *-*-* } 12 }
-- { dg-error "possible interpretation in package Standard" "" { target *-*-* } 12 }
