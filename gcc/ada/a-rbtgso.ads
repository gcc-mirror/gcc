------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .      --
--               G E N E R I C _ S E T _ O P E R A T I O N S                --
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

   with procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access);

   with function Copy_Tree (Source_Root : Node_Access)
       return Node_Access;

   with procedure Delete_Tree (X : in out Node_Access);

   with function Is_Less (Left, Right : Node_Access) return Boolean;

   with procedure Free (X : in out Node_Access);

package Ada.Containers.Red_Black_Trees.Generic_Set_Operations is
pragma Pure (Generic_Set_Operations);

   procedure Union (Target : in out Tree_Type; Source : Tree_Type);

   function Union (Left, Right : Tree_Type) return Tree_Type;

   procedure Intersection (Target : in out Tree_Type; Source : Tree_Type);

   function Intersection (Left, Right : Tree_Type) return Tree_Type;

   procedure Difference (Target : in out Tree_Type; Source : Tree_Type);

   function Difference (Left, Right : Tree_Type) return Tree_Type;

   procedure Symmetric_Difference
     (Target : in out Tree_Type;
      Source : Tree_Type);

   function Symmetric_Difference (Left, Right : Tree_Type) return Tree_Type;

   function Is_Subset (Subset : Tree_Type; Of_Set : Tree_Type) return Boolean;

   function Overlap (Left, Right : Tree_Type) return Boolean;

end Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
