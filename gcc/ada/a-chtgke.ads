------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.HASH_TABLES.GENERIC_KEYS                  --
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
   with package HT_Types is
     new Generic_Hash_Table_Types (<>);

   type HT_Type is new HT_Types.Hash_Table_Type with private;

   use HT_Types;

   Null_Node : Node_Access;

   with function Next (Node : Node_Access) return Node_Access;

   with procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);

   type Key_Type (<>) is limited private;

   with function Hash (Key : Key_Type) return Hash_Type;

   with function Equivalent_Keys
     (Key  : Key_Type;
      Node : Node_Access) return Boolean;

package Ada.Containers.Hash_Tables.Generic_Keys is
   pragma Preelaborate;

   function Index
     (HT  : HT_Type;
      Key : Key_Type) return Hash_Type;
   pragma Inline (Index);

   procedure Delete_Key_Sans_Free
     (HT   : in out HT_Type;
      Key  : Key_Type;
      X    : out Node_Access);

   function Find (HT  : HT_Type; Key : Key_Type) return Node_Access;

   generic
      with function New_Node
        (Next : Node_Access) return Node_Access;
   procedure Generic_Conditional_Insert
     (HT      : in out HT_Type;
      Key     : Key_Type;
      Node    : out Node_Access;
      Success : out Boolean);

end Ada.Containers.Hash_Tables.Generic_Keys;
