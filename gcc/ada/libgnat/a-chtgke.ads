------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.HASH_TABLES.GENERIC_KEYS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  Hash_Table_Type is used to implement hashed containers. This package
--  declares hash-table operations that depend on keys.

generic
   with package HT_Types is
     new Generic_Hash_Table_Types (<>);

   use HT_Types, HT_Types.Implementation;

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
     (HT  : Hash_Table_Type;
      Key : Key_Type) return Hash_Type;
   pragma Inline (Index);
   --  Returns the bucket number (array index value) for the given key

   function Checked_Index
     (HT  : aliased in out Hash_Table_Type;
      Key : Key_Type) return Hash_Type;
   pragma Inline (Checked_Index);
   --  Calls Index, but also locks and unlocks the container, per AI05-0022, in
   --  order to detect element tampering by the generic actual Hash function.

   function Checked_Equivalent_Keys
     (HT   : aliased in out Hash_Table_Type;
      Key  : Key_Type;
      Node : Node_Access) return Boolean;
   --  Calls Equivalent_Keys, but locks and unlocks the container, per
   --  AI05-0022, in order to detect element tampering by that generic actual.

   procedure Delete_Key_Sans_Free
     (HT  : in out Hash_Table_Type;
      Key : Key_Type;
      X   : out Node_Access);
   --  Removes the node (if any) with the given key from the hash table,
   --  without deallocating it. Program_Error is raised if the hash
   --  table is busy.

   function Find
     (HT  : aliased in out Hash_Table_Type;
      Key : Key_Type) return Node_Access;
   --  Returns the node (if any) corresponding to the given key

   generic
      with function New_Node (Next : Node_Access) return Node_Access;
   procedure Generic_Conditional_Insert
     (HT       : in out Hash_Table_Type;
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean);
   --  Attempts to insert a new node with the given key into the hash table.
   --  If a node with that key already exists in the table, then that node
   --  is returned and Inserted returns False. Otherwise New_Node is called
   --  to allocate a new node, and Inserted returns True. Program_Error is
   --  raised if the hash table is busy.

   generic
      with function Hash (Node : Node_Access) return Hash_Type;
      with procedure Assign (Node : Node_Access; Key : Key_Type);
   procedure Generic_Replace_Element
     (HT   : in out Hash_Table_Type;
      Node : Node_Access;
      Key  : Key_Type);
   --  Assigns Key to Node, possibly changing its equivalence class. If Node
   --  is in the same equivalence class as Key (that is, it's already in the
   --  bucket implied by Key), then if the hash table is locked then
   --  Program_Error is raised; otherwise Assign is called to assign Key to
   --  Node. If Node is in a different bucket from Key, then Program_Error is
   --  raised if the hash table is busy. Otherwise it Assigns Key to Node and
   --  moves the Node from its current bucket to the bucket implied by Key.
   --  Note that it is never proper to assign to Node a key value already
   --  in the map, and so if Key is equivalent to some other node then
   --  Program_Error is raised.

end Ada.Containers.Hash_Tables.Generic_Keys;
