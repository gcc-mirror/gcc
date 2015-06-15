-- { dg-compile }
-- { dg-options "-g" }

with Debug4_Pkg;

procedure Debug4 is
   package P is new Debug4_Pkg (Natural);
begin
   null;
end;
