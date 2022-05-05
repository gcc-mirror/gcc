-- { dg-do run }
-- { dg-options "-O2" }

with Opt96_Pkg; use Opt96_Pkg;

procedure Opt96 is
   R : Rec;
   D : Data;
begin
   D.Foo.Bar := (0.02, 0.01);
   if R.F (D) /= 30 then
     raise Program_Error;
   end if;
end;
