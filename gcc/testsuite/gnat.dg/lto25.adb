-- { dg-do run }
-- { dg-options "-O2 -flto" { target lto } }

with Opt96_Pkg; use Opt96_Pkg;

procedure Lto25 is
   R : Rec;
   D : Data;
begin
   D.Foo.Bar := (0.02, 0.01);
   if R.F (D) /= 30 then
     raise Program_Error;
   end if;
end;
