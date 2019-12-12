------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.HASH_TABLES.GENERIC_OPERATIONS               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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
     new Generic_Hash_Table_Types (<>);

   use HT_Types, HT_Types.Implementation;

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
   --  First frees the nodes in all non-null buckets of Buckets, and then frees
   --  the Buckets array itself.

   function Index
     (Buckets : Buckets_Type;
      Node    : Node_Access) return Hash_Type;
   pragma Inline (Index);
   --  Uses the hash value of Node to compute its Buckets array index

   function Index
     (Hash_Table : Hash_Table_Type;
      Node       : Node_Access) return Hash_Type;
   pragma Inline (Index);
   --  Uses the hash value of Node to compute its Hash_Table buckets array
   --  index.

   function Checked_Index
     (Hash_Table : aliased in out Hash_Table_Type;
      Buckets    : Buckets_Type;
      Node       : Node_Access) return Hash_Type;
   --  Calls Index, but also locks and unlocks the container, per AI05-0022, in
   --  order to detect element tampering by the generic actual Hash function.

   function Checked_Index
     (Hash_Table : aliased in out Hash_Table_Type;
      Node       : Node_Access) return Hash_Type;
   --  Calls Checked_Index using Hash_Table's buckets array.

   procedure Adjust (HT : in out Hash_Table_Type);
   --  Used to implement controlled Adjust. It is assumed that HT has the value
   --  of the bit-wise copy that immediately follows controlled Finalize.
   --  Adjust first allocates a new buckets array for HT (having the same
   --  length as the source), and then allocates a copy of each node of source.

   procedure Finalize (HT : in out Hash_Table_Type);
   --  Used to implement controlled Finalize. It first calls Clear to
   --  deallocate any remaining nodes, and then deallocates the buckets array.

   generic
      with function Find
        (HT  : Hash_Table_Type;
         Key : Node_Access) return Boolean;
   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean;
   --  Used to implement hashed container equality. For each node in hash table
   --  L, it calls Find to search for an equivalent item in hash table R. If
   --  Find returns False for any node then Generic_Equal terminates
   --  immediately and returns False. Otherwise if Find returns True for every
   --  node then Generic_Equal returns True.

   procedure Clear (HT : in out Hash_Table_Type);
   --  Deallocates each node in hash table HT. (Note that it only deallocates
   --  the nodes, not the buckets array. Also note that for bounded containers,
   --  the buckets array is not dynamically allocated). Program_Error is raised
   --  if the hash table is busy.

   procedure Move (Target, Source : in out Hash_Table_Type);
   --  Moves (not copies) the buckets array and nodes from Source to
   --  Target. Program_Error is raised if Source is busy. The Target is first
   --  cleared to deallocate its nodes (implying that Program_Error is also
   --  raised if Target is busy). Source is empty following the move.

   function Capacity (HT : Hash_Table_Type) return Count_Type;
   --  Returns the length of the buckets array

   procedure Reserve_Capacity
     (HT : in out Hash_Table_Type;
      N  : Count_Type);
   --  If N is greater than the current capacity, then it expands the buckets
   --  array to at least the value N. If N is less than the current capacity,
   --  then it contracts the buckets array. In either case existing nodes are
   --  rehashed onto the new buckets array, and the old buckets array is
   --  deallocated. Program_Error is raised if the hash table is busy.

   procedure Delete_Node_At_Index
     (HT   : in out Hash_Table_Type;
      Indx : Hash_Type;
      X    : in out Node_Access);
   --  Delete a node whose bucket position is known. Used to remove a node
   --  whose element has been modified through a key_preserving reference.
   --  We cannot use the value of the element precisely because the current
   --  value does not correspond to the hash code that determines the bucket.

   procedure Delete_Node_Sans_Free
     (HT : in out Hash_Table_Type;
      X  : Node_Access);
   --  Removes node X from the hash table without deallocating the node

   function First
     (HT       : Hash_Table_Type) return Node_Access;
   function First
     (HT       : Hash_Table_Type;
      Position : out Hash_Type) return Node_Access;
   --  Returns the head of the list in the first (lowest-index) non-empty
   --  bucket. Position will be the index of the bucket of the first node.
   --  It is provided so that clients can implement efficient iterators.

   function Next
     (HT   : aliased in out Hash_Table_Type;
      Node : Node_Access) return Node_Access;
   function Next
     (HT       : aliased in out Hash_Table_Type;
      Node     : Node_Access;
      Position : in out Hash_Type) return Node_Access;
   --  Returns the node that immediately follows Node. This corresponds to
   --  either the next node in the same bucket, or (if Node is the last node in
   --  its bucket) the head of the list in the first non-empty bucket that
   --  follows.
   --
   --  If Node_Position is supplied, then it will be used as a starting point
   --  for iteration (Node_Position must be the index of Node's buckets). If it
   --  is not supplied, it will be recomputed. It is provided so that clients
   --  can implement efficient iterators.

   generic
      with procedure Process (Node : Node_Access; Position : Hash_Type);
   procedure Generic_Iteration_With_Position (HT : Hash_Table_Type);
   --  Calls Process for each node in hash table HT

   generic
      with procedure Process (Node : Node_Access);
   procedure Generic_Iteration (HT : Hash_Table_Type);
   --  Calls Process for each node in hash table HT

   generic
      use Ada.Streams;
      with procedure Write
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Access);
   procedure Generic_Write
     (Stream : not null access Root_Stream_Type'Class;
      HT     : Hash_Table_Type);
   --  Used to implement the streaming attribute for hashed containers. It
   --  calls Write for each node to write its value into Stream.

   generic
      use Ada.Streams;
      with function New_Node
             (Stream : not null access Root_Stream_Type'Class)
              return Node_Access;
   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      HT     : out Hash_Table_Type);
   --  Used to implement the streaming attribute for hashed containers. It
   --  first clears hash table HT, then populates the hash table by calling
   --  New_Node for each item in Stream.

   function New_Buckets (Length : Hash_Type) return Buckets_Access;
   pragma Inline (New_Buckets);
   --  Allocate a new Buckets_Type array with bounds 0 .. Length - 1

   procedure Free_Buckets (Buckets : in out Buckets_Access);
   pragma Inline (Free_Buckets);
   --  Unchecked_Deallocate Buckets

   --  Note: New_Buckets and Free_Buckets are needed because Buckets_Access has
   --  an empty pool.

end Ada.Containers.Hash_Tables.Generic_Operations;
