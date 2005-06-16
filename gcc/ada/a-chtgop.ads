------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       A D A . C O N T A I N E R S .                      --
--       H A S H _ T A B L E S . G E N E R I C _ O P E R A T I O N S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

generic

   with package HT_Types is
     new Generic_Hash_Table_Types (<>);

   use HT_Types;

   with function Hash_Node (Node : Node_Access) return Hash_Type;

   with function Next (Node : Node_Access) return Node_Access;

   with procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);

    with function Copy_Node (Source : Node_Access) return Node_Access;

   with procedure Free (X : in out Node_Access);

package Ada.Containers.Hash_Tables.Generic_Operations is
   pragma Preelaborate;

   procedure Free_Hash_Table (Buckets : in out Buckets_Access);

   function Index
     (Buckets : Buckets_Type;
      Node    : Node_Access) return Hash_Type;
   pragma Inline (Index);

   function Index
     (Hash_Table : Hash_Table_Type;
      Node       : Node_Access) return Hash_Type;
   pragma Inline (Index);

   procedure Adjust (HT : in out Hash_Table_Type);

   procedure Finalize (HT : in out Hash_Table_Type);

   generic
      with function Find
        (HT  : Hash_Table_Type;
         Key : Node_Access) return Boolean;
   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean;

   procedure Clear (HT : in out Hash_Table_Type);

   procedure Move (Target, Source : in out Hash_Table_Type);

   function Capacity (HT : Hash_Table_Type) return Count_Type;

   procedure Reserve_Capacity
     (HT : in out Hash_Table_Type;
      N  : Count_Type);

   procedure Delete_Node_Sans_Free
     (HT : in out Hash_Table_Type;
      X  : Node_Access);

   function First (HT : Hash_Table_Type) return Node_Access;

   function Next
     (HT   : Hash_Table_Type;
      Node : Node_Access) return Node_Access;

   generic
      with procedure Process (Node : Node_Access);
   procedure Generic_Iteration (HT : Hash_Table_Type);

   generic
      use Ada.Streams;
      with procedure Write
        (Stream : access Root_Stream_Type'Class;
         Node   : Node_Access);
   procedure Generic_Write
     (Stream : access Root_Stream_Type'Class;
      HT     : Hash_Table_Type);

   generic
      use Ada.Streams;
      with function New_Node (Stream : access Root_Stream_Type'Class)
         return Node_Access;
   procedure Generic_Read
     (Stream : access Root_Stream_Type'Class;
      HT     : out Hash_Table_Type);

end Ada.Containers.Hash_Tables.Generic_Operations;
