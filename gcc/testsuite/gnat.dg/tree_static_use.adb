-- { dg-do run }
-- { dg-options "-O1" }

with TREE_STATIC_Def; use TREE_STATIC_Def;

procedure TREE_STATIC_Use is
   I : Int := One;
begin
   check (I, 1);
end;


