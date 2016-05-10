-- { dg-do compile }
-- { dg-options "-g" }

with Debug6_Pkg; use Debug6_Pkg;

procedure Debug6 is
   V : Value := (Kind => Undefined);
begin
   Process (V);
end Debug6;
