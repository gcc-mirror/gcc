-- { dg-do run }
-- { dg-options "-O2" }

with Opt12_Pkg; use Opt12_Pkg;

procedure Opt12 is

   Static_Target : Static_Integer_Subtype;

begin

   Static_Target := Static_Integer_Subtype(Fix_Half);

   if not Equal(Static_Target, 1) then
     raise Program_Error;
   end if;

end Opt12;
