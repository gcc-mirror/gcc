------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_OPERATIONS             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

generic
   with package Tree_Types is new Generic_Tree_Types (<>);
   use Tree_Types;

   Null_Node : Node_Access;

   with function  Parent (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Parent (Node : Node_Access; Parent : Node_Access) is <>;
   with function  Left (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Left (Node : Node_Access; Left : Node_Access) is <>;
   with function  Right (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Right (Node : Node_Access; Right : Node_Access) is <>;
   with function  Color (Node : Node_Access) return Color_Type is <>;
   with procedure Set_Color (Node : Node_Access; Color : Color_Type) is <>;

package Ada.Containers.Red_Black_Trees.Generic_Operations is
pragma Pure;

   function Min (Node : Node_Access) return Node_Access;

   function Max (Node : Node_Access) return Node_Access;

   procedure Check_Invariant (Tree : Tree_Type);

   function Next (Node : Node_Access) return Node_Access;

   function Previous (Node : Node_Access) return Node_Access;

   procedure Move (Target, Source : in out Tree_Type);

   generic
      with function Is_Equal (L, R : Node_Access) return Boolean;
   function Generic_Equal (Left, Right : Tree_Type) return Boolean;

   procedure Delete_Node_Sans_Free
     (Tree : in out Tree_Type;
      Node : Node_Access);

   generic
      with procedure Process (Node : Node_Access) is <>;
   procedure Generic_Iteration (Tree : Tree_Type);

   generic
      with procedure Process (Node : Node_Access) is <>;
   procedure Generic_Reverse_Iteration (Tree : Tree_Type);

   generic
      with function New_Node return Node_Access is <>;
   procedure Generic_Read (Tree : in out Tree_Type; N : Count_Type);

   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type;
      Node : Node_Access);

end Ada.Containers.Red_Black_Trees.Generic_Operations;
