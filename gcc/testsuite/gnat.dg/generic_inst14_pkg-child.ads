package Generic_Inst14_Pkg.Child is

   generic
      type Value is private;
      with package Value_Tree is new Definite_Value_Tree (Value => Value);
   package Simple is
      type Node is new Value_Tree.Value_Node with null record;
      package Strat is new Def_Strat (Value, Value_Tree, Node);
   end Simple;

   generic
      type Value is private;
      with package A_Strat is new Def_Strat (Value => Value, others => <>);
   package OK is
      procedure Plop (N : A_Strat.Node) is null;
   end OK;

   generic
      type Value is private;
      with package Value_Tree is new Definite_Value_Tree (Value => Value);
      with package A_Strat is
         new Def_Strat (Value => Value, Value_Tree => Value_Tree, others => <>);
   package Not_OK is
      procedure Plop (N : A_Strat.Node) is null;
   end Not_OK;
   
end Generic_Inst14_Pkg.Child;
