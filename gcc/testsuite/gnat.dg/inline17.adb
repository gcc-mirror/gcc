--  { dg-do compile }
--  { dg-options "-O -gnatn" }
with Inline17_Pkg1; use Inline17_Pkg1;
with Inline17_Pkg2; use Inline17_Pkg2;

procedure Inline17 is
   use type SQL_Field;
begin
   Test;
end;
