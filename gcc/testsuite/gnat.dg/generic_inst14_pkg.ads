package Generic_Inst14_Pkg is

   generic
      type Value is limited private;
   package Definite_Value_Tree is
      type Value_Node is abstract tagged null record; 
   end Definite_Value_Tree;
   
   generic
      type Value is limited private;
      with package Value_Tree is new Definite_Value_Tree (Value);
      type Node (<>) is new Value_Tree.Value_Node with private;
   package Def_Strat is
   end Def_Strat;

end Generic_Inst14_Pkg;
