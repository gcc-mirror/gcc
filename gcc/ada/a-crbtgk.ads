------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--        A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .     --
--                          G E N E R I C _ K E Y S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Red_Black_Trees.Generic_Operations;

generic
   with package Tree_Operations is new Generic_Operations (<>);

   use Tree_Operations.Tree_Types;

   type Key_Type (<>) is limited private;

   with function Is_Less_Key_Node
     (L : Key_Type;
      R : Node_Access) return Boolean;

   with function Is_Greater_Key_Node
     (L : Key_Type;
      R : Node_Access) return Boolean;

package Ada.Containers.Red_Black_Trees.Generic_Keys is
   pragma Pure;

   generic
      with function New_Node return Node_Access;
   procedure Generic_Insert_Post
     (Tree : in out Tree_Type;
      X, Y : Node_Access;
      Key  : Key_Type;
      Z    : out Node_Access);

   generic
      with procedure Insert_Post
        (Tree : in out Tree_Type;
         X, Y : Node_Access;
         Key  : Key_Type;
         Z    : out Node_Access);

   procedure Generic_Conditional_Insert
     (Tree    : in out Tree_Type;
      Key     : Key_Type;
      Node    : out Node_Access;
      Success : out Boolean);

   generic
      with procedure Insert_Post
        (Tree : in out Tree_Type;
         X, Y : Node_Access;
         Key  : Key_Type;
         Z    : out Node_Access);

   procedure Generic_Unconditional_Insert
     (Tree : in out Tree_Type;
      Key  : Key_Type;
      Node : out Node_Access);

   generic
      with procedure Insert_Post
        (Tree : in out Tree_Type;
         X, Y : Node_Access;
         Key  : Key_Type;
         Z    : out Node_Access);

      with procedure Unconditional_Insert_Sans_Hint
        (Tree    : in out Tree_Type;
         Key     : Key_Type;
         Node    : out Node_Access);

   procedure Generic_Unconditional_Insert_With_Hint
     (Tree : in out Tree_Type;
      Hint : Node_Access;
      Key  : Key_Type;
      Node : out Node_Access);

   generic
      with procedure Insert_Post
        (Tree : in out Tree_Type;
         X, Y : Node_Access;
         Key  : Key_Type;
         Z    : out Node_Access);

      with procedure Conditional_Insert_Sans_Hint
        (Tree    : in out Tree_Type;
         Key     : Key_Type;
         Node    : out Node_Access;
         Success : out Boolean);

   procedure Generic_Conditional_Insert_With_Hint
     (Tree     : in out Tree_Type;
      Position : Node_Access;
      Key      : Key_Type;
      Node     : out Node_Access;
      Success  : out Boolean);

   function Find
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;

   function Ceiling
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;

   function Floor
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;

   function Upper_Bound
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;

   generic
      with procedure Process (Node : Node_Access);
   procedure Generic_Iteration
     (Tree : Tree_Type;
      Key  : Key_Type);

   generic
      with procedure Process (Node : Node_Access);
   procedure Generic_Reverse_Iteration
     (Tree : Tree_Type;
      Key  : Key_Type);

end Ada.Containers.Red_Black_Trees.Generic_Keys;
