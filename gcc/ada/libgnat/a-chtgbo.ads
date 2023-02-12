------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           ADA.CONTAINERS.HASH_TABLES.GENERIC_BOUNDED_OPERATIONS          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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
--  declares hash-table operations that do not depend on keys.

with Ada.Streams;

generic
   with package HT_Types is
     new Generic_Bounded_Hash_Table_Types (<>);

   use HT_Types, HT_Types.Implementation;

   with function Hash_Node (Node : Node_Type) return Hash_Type;

   with function Next (Node : Node_Type) return Count_Type;

   with procedure Set_Next
     (Node : in out Node_Type;
      Next : Count_Type);

package Ada.Containers.Hash_Tables.Generic_Bounded_Operations is
   pragma Pure;

   function Index
     (Buckets : Buckets_Type;
      Node    : Node_Type) return Hash_Type;
   pragma Inline (Index);
   --  Uses the hash value of Node to compute its Buckets array index

   function Index
     (HT   : Hash_Table_Type'Class;
      Node : Node_Type) return Hash_Type;
   pragma Inline (Index);
   --  Uses the hash value of Node to compute its Hash_Table buckets array
   --  index.

   function Checked_Index
     (Hash_Table : aliased in out Hash_Table_Type'Class;
      Node       : Count_Type) return Hash_Type;
   --  Calls Index, but also locks and unlocks the container, per AI05-0022, in
   --  order to detect element tampering by the generic actual Hash function.

   generic
      with function Find
        (HT  : Hash_Table_Type'Class;
         Key : Node_Type) return Boolean;
   function Generic_Equal (L, R : Hash_Table_Type'Class) return Boolean;
   --  Used to implement hashed container equality. For each node in hash table
   --  L, it calls Find to search for an equivalent item in hash table R. If
   --  Find returns False for any node then Generic_Equal terminates
   --  immediately and returns False. Otherwise if Find returns True for every
   --  node then Generic_Equal returns True.

   procedure Clear (HT : in out Hash_Table_Type'Class);
   --  Deallocates each node in hash table HT. (Note that it only deallocates
   --  the nodes, not the buckets array.) Program_Error is raised if the hash
   --  table is busy.

   procedure Delete_Node_At_Index
     (HT   : in out Hash_Table_Type'Class;
      Indx : Hash_Type;
      X    : Count_Type);
   --  Delete a node whose bucket position is known. extracted from following
   --  subprogram, but also used directly to remove a node whose element has
   --  been modified through a key_preserving reference: in that case we cannot
   --  use the value of the element precisely because the current value does
   --  not correspond to the hash code that determines its bucket.

   procedure Delete_Node_Sans_Free
     (HT : in out Hash_Table_Type'Class;
      X  : Count_Type);
   --  Removes node X from the hash table without deallocating the node

   generic
      with procedure Set_Element (Node : in out Node_Type);
   procedure Generic_Allocate
     (HT   : in out Hash_Table_Type'Class;
      Node : out Count_Type);
   --  Claim a node from the free store. Generic_Allocate first
   --  calls Set_Element on the potential node, and then returns
   --  the node's index as the value of the Node parameter.

   procedure Free
     (HT : in out Hash_Table_Type'Class;
      X  : Count_Type);
   --  Return a node back to the free store, from where it had
   --  been previously claimed via Generic_Allocate.

   function First (HT : Hash_Table_Type'Class) return Count_Type;
   --  Returns the head of the list in the first (lowest-index) non-empty
   --  bucket.

   function Next
     (HT   : Hash_Table_Type'Class;
      Node : Count_Type) return Count_Type;
   --  Returns the node that immediately follows Node. This corresponds to
   --  either the next node in the same bucket, or (if Node is the last node in
   --  its bucket) the head of the list in the first non-empty bucket that
   --  follows.

   generic
      with procedure Process (Node : Count_Type);
   procedure Generic_Iteration (HT : Hash_Table_Type'Class);
   --  Calls Process for each node in hash table HT

   generic
      use Ada.Streams;
      with procedure Write
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type);
   procedure Generic_Write
     (Stream : not null access Root_Stream_Type'Class;
      HT     : Hash_Table_Type'Class);
   --  Used to implement the streaming attribute for hashed containers. It
   --  calls Write for each node to write its value into Stream.

   generic
      use Ada.Streams;
      with function New_Node (Stream : not null access Root_Stream_Type'Class)
         return Count_Type;
   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      HT     : out Hash_Table_Type'Class);
   --  Used to implement the streaming attribute for hashed containers. It
   --  first clears hash table HT, then populates the hash table by calling
   --  New_Node for each item in Stream.

end Ada.Containers.Hash_Tables.Generic_Bounded_Operations;
