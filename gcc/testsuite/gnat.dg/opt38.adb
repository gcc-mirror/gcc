-- { dg-do run }
-- { dg-options "-O2 -gnatn" }

with Opt38_Pkg; use Opt38_Pkg;

procedure Opt38 is
begin
  Test (-1);
end;
