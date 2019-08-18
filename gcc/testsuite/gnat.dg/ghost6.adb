--  { dg-do link }
--  { dg-options "-gnata -g" }

with Ghost6_Pkg;

procedure Ghost6 is
   X : Ghost6_Pkg.T with Ghost;
begin
   null;
end Ghost6;
