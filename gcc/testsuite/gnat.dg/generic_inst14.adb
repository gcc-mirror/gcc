--  { dg-do compile }

with Generic_Inst14_Pkg;
with Generic_Inst14_Pkg.Child;

procedure Generic_Inst14 is

   type T is null record;

   package Tree is new Generic_Inst14_Pkg.Definite_Value_Tree (T);

   package Base is new Generic_Inst14_Pkg.Child.Simple (T, Tree);

   package OK is new Generic_Inst14_Pkg.Child.OK (T, Base.Strat);

   package Not_OK is new Generic_Inst14_Pkg.Child.Not_OK (T, Tree, Base.Strat);

begin
   null;
end;
