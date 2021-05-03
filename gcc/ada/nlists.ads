------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               N L I S T S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides facilities for manipulating lists of nodes (see
--  package Atree for format and implementation of tree nodes). The Link field
--  of the nodes is used as the forward pointer for these lists. See also
--  package Elists which provides another form of lists that are not threaded
--  through the nodes (and therefore allow nodes to be on multiple lists).

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file nlists.h

with System;
with Types; use Types;

package Nlists is

   --  A node list is a list of nodes in a special format that means that
   --  nodes can be on at most one such list. For each node list, a list
   --  header is allocated in the lists table, and a List_Id value references
   --  this header, which may be used to access the nodes in the list using
   --  the set of routines that define this interface.

   --  Note: node lists can contain either nodes or entities (extended nodes)
   --  or a mixture of nodes and extended nodes.

   function In_Same_List (N1, N2 : Node_Or_Entity_Id) return Boolean;
   pragma Inline (In_Same_List);
   --  Equivalent to List_Containing (N1) = List_Containing (N2)

   function Last_List_Id return List_Id;
   pragma Inline (Last_List_Id);
   --  Returns Id of last allocated list header

   function Lists_Address return System.Address;
   pragma Inline (Lists_Address);
   --  Return address of Lists table (used in Back_End for Gigi call)

   function Num_Lists return Nat;
   pragma Inline (Num_Lists);
   --  Number of currently allocated lists

   function New_List return List_Id;
   --  Creates a new empty node list. Typically this is used to initialize
   --  a field in some other node which points to a node list where the list
   --  is then subsequently filled in using Append calls.

   function Empty_List return List_Id renames New_List;
   --  Used in contexts where an empty list (as opposed to an initially empty
   --  list to be filled in) is required.

   function New_List
     (Node : Node_Or_Entity_Id) return List_Id;
   --  Build a new list initially containing the given node

   function New_List
     (Node1 : Node_Or_Entity_Id;
      Node2 : Node_Or_Entity_Id) return List_Id;
   --  Build a new list initially containing the two given nodes

   function New_List
     (Node1 : Node_Or_Entity_Id;
      Node2 : Node_Or_Entity_Id;
      Node3 : Node_Or_Entity_Id) return List_Id;
   --  Build a new list initially containing the three given nodes

   function New_List
     (Node1 : Node_Or_Entity_Id;
      Node2 : Node_Or_Entity_Id;
      Node3 : Node_Or_Entity_Id;
      Node4 : Node_Or_Entity_Id) return List_Id;

   function New_List
     (Node1 : Node_Or_Entity_Id;
      Node2 : Node_Or_Entity_Id;
      Node3 : Node_Or_Entity_Id;
      Node4 : Node_Or_Entity_Id;
      Node5 : Node_Or_Entity_Id) return List_Id;
   --  Build a new list initially containing the five given nodes

   function New_List
     (Node1 : Node_Or_Entity_Id;
      Node2 : Node_Or_Entity_Id;
      Node3 : Node_Or_Entity_Id;
      Node4 : Node_Or_Entity_Id;
      Node5 : Node_Or_Entity_Id;
      Node6 : Node_Or_Entity_Id) return List_Id;
   --  Build a new list initially containing the six given nodes

   function New_Copy_List (List : List_Id) return List_Id;
   --  Creates a new list containing copies (made with Atree.New_Copy) of every
   --  node in the original list. If the argument is No_List, then the returned
   --  result is No_List. If the argument is an empty list, then the returned
   --  result is a new empty list.

   function New_Copy_List_Original (List : List_Id) return List_Id;
   --  Same as New_Copy_List but copies only nodes coming from source

   function First (List : List_Id) return Node_Or_Entity_Id;
   pragma Inline (First);
   --  Obtains the first element of the given node list or, if the node list
   --  has no items or is equal to No_List, then Empty is returned.

   function First_Non_Pragma (List : List_Id) return Node_Or_Entity_Id;
   --  Used when dealing with a list that can contain pragmas to skip past
   --  any initial pragmas and return the first element that is not a pragma.
   --  If the list is empty, or if it contains only pragmas, then Empty is
   --  returned. It is an error to call First_Non_Pragma with a Node_Id value
   --  or No_List (No_List is not considered to be the same as an empty list).
   --  This function also skips N_Null nodes which can result from rewriting
   --  unrecognized or incorrect pragmas.

   function Last (List : List_Id) return Node_Or_Entity_Id;
   pragma Inline (Last);
   --  Obtains the last element of the given node list or, if the node list
   --  has no items, then Empty is returned. It is an error to call Last with
   --  a Node_Id or No_List. (No_List is not considered to be the same as an
   --  empty node list).

   function Last_Non_Pragma (List : List_Id) return Node_Or_Entity_Id;
   --  Obtains the last element of a given node list that is not a pragma.
   --  If the list is empty, or if it contains only pragmas, then Empty is
   --  returned. It is an error to call Last_Non_Pragma with a Node_Id or
   --  No_List. (No_List is not considered to be the same as an empty list).

   function List_Length (List : List_Id) return Nat;
   --  Returns number of items in the given list. If called on No_List it
   --  returns 0, even though No_List is not considered to be the same as an
   --  empty list.

   function Next (Node : Node_Or_Entity_Id) return Node_Or_Entity_Id;
   pragma Inline (Next);
   --  This function returns the next node on a node list, or Empty if Node is
   --  the last element of the node list. The argument must be a member of a
   --  node list.

   procedure Next (Node : in out Node_Or_Entity_Id);
   pragma Inline (Next);
   --  Equivalent to Node := Next (Node);

   function Next_Non_Pragma
     (Node : Node_Or_Entity_Id) return Node_Or_Entity_Id;
   --  This function returns the next node on a node list, skipping past any
   --  pragmas, or Empty if there is no non-pragma entry left. The argument
   --  must be a member of a node list. This function also skips N_Null nodes
   --  which can result from rewriting unrecognized or incorrect pragmas.

   procedure Next_Non_Pragma (Node : in out Node_Or_Entity_Id);
   pragma Inline (Next_Non_Pragma);
   --  Equivalent to Node := Next_Non_Pragma (Node);

   function Prev (Node : Node_Or_Entity_Id) return Node_Or_Entity_Id;
   pragma Inline (Prev);
   --  This function returns the previous node on a node list, or Empty
   --  if Node is the first element of the node list. The argument must be
   --  a member of a node list. Note: the implementation does maintain back
   --  pointers, so this function executes quickly in constant time.

   function Pick (List : List_Id; Index : Pos) return Node_Or_Entity_Id;
   --  Given a list, picks out the Index'th entry (1 = first entry). The
   --  caller must ensure that Index is in range.

   procedure Prev (Node : in out Node_Or_Entity_Id);
   pragma Inline (Prev);
   --  Equivalent to Node := Prev (Node);

   function Prev_Non_Pragma
     (Node : Node_Or_Entity_Id) return Node_Or_Entity_Id;
   pragma Inline (Prev_Non_Pragma);
   --  This function returns the previous node on a node list, skipping any
   --  pragmas. If Node is the first element of the list, or if the only
   --  elements preceding it are pragmas, then Empty is returned. The
   --  argument must be a member of a node list. Note: the implementation
   --  does maintain back pointers, so this function executes quickly in
   --  constant time.

   procedure Prev_Non_Pragma (Node : in out Node_Or_Entity_Id);
   pragma Inline (Prev_Non_Pragma);
   --  Equivalent to Node := Prev_Non_Pragma (Node);

   function Is_Empty_List (List : List_Id) return Boolean;
   pragma Inline (Is_Empty_List);
   --  This function determines if a given list id references a node list that
   --  contains no items. No_List as an argument returns True.

   function Is_Non_Empty_List (List : List_Id) return Boolean;
   pragma Inline (Is_Non_Empty_List);
   --  This function determines if a given list id references a node list that
   --  contains at least one item. No_List as an argument returns False.

   function Is_List_Member (Node : Node_Or_Entity_Id) return Boolean;
   pragma Inline (Is_List_Member);
   --  This function determines if a given node is a member of a node list.
   --  It is an error for Node to be Empty, or to be a node list.

   function List_Containing (Node : Node_Or_Entity_Id) return List_Id;
   pragma Inline (List_Containing);
   --  This function provides a pointer to the node list containing Node.
   --  Node must be a member of a node list.

   procedure Append (Node : Node_Or_Entity_Id; To : List_Id);
   --  Appends Node at the end of node list To. Node must be a non-empty node
   --  that is not already a member of a node list, and To must be a node list.
   --  An attempt to append an error node is ignored without complaint and the
   --  list is unchanged.

   procedure Append_New (Node : Node_Or_Entity_Id; To : in out List_Id);
   pragma Inline (Append_New);
   --  Appends Node at the end of node list To. If To is non-existent list, a
   --  list is created. Node must be a non-empty node that is not already a
   --  member of a node list, and To must be a node list.

   procedure Append_New_To (To : in out List_Id; Node : Node_Or_Entity_Id);
   pragma Inline (Append_New_To);
   --  Like Append_New, but the arguments are in reverse order

   procedure Append_To (To : List_Id; Node : Node_Or_Entity_Id);
   pragma Inline (Append_To);
   --  Like Append, but arguments are the other way round

   procedure Append_List (List : List_Id; To : List_Id);
   --  Appends node list List to the end of node list To. On return,
   --  List is reset to be empty.

   procedure Append_List_To (To : List_Id; List : List_Id);
   pragma Inline (Append_List_To);
   --  Like Append_List, but arguments are the other way round

   procedure Insert_After
     (After : Node_Or_Entity_Id;
      Node  : Node_Or_Entity_Id);
   --  Insert Node, which must be a non-empty node that is not already a
   --  member of a node list, immediately past node After, which must be a
   --  node that is currently a member of a node list. An attempt to insert
   --  an error node is ignored without complaint (and the list is unchanged).

   procedure Insert_List_After
     (After : Node_Or_Entity_Id;
      List  : List_Id);
   --  Inserts the entire contents of node list List immediately after node
   --  After, which must be a member of a node list. On return, the node list
   --  List is reset to be the empty node list.

   procedure Insert_Before
     (Before : Node_Or_Entity_Id;
      Node   : Node_Or_Entity_Id);
   --  Insert Node, which must be a non-empty node that is not already a
   --  member of a node list, immediately before Before, which must be a node
   --  that is currently a member of a node list. An attempt to insert an
   --  error node is ignored without complaint (and the list is unchanged).

   procedure Insert_List_Before
     (Before : Node_Or_Entity_Id;
      List   : List_Id);
   --  Inserts the entire contents of node list List immediately before node
   --  Before, which must be a member of a node list. On return, the node list
   --  List is reset to be the empty node list.

   procedure Prepend
     (Node : Node_Or_Entity_Id;
      To   : List_Id);
   --  Prepends Node at the start of node list To. Node must be a non-empty
   --  node that is not already a member of a node list, and To must be a
   --  node list. An attempt to prepend an error node is ignored without
   --  complaint and the list is unchanged.

   procedure Prepend_List
     (List : List_Id;
      To   : List_Id);
   --  Prepends node list List to the start of node list To. On return,
   --  List is reset to be empty.

   procedure Prepend_List_To
     (To   : List_Id;
      List : List_Id);
   pragma Inline (Prepend_List_To);
   --  Like Prepend_List, but arguments are the other way round

   procedure Prepend_New (Node : Node_Or_Entity_Id; To : in out List_Id);
   pragma Inline (Prepend_New);
   --  Prepends Node at the end of node list To. If To is non-existent list, a
   --  list is created. Node must be a non-empty node that is not already a
   --  member of a node list, and To must be a node list.

   procedure Prepend_New_To (To : in out List_Id; Node : Node_Or_Entity_Id);
   pragma Inline (Prepend_New_To);
   --  Like Prepend_New, but the arguments are in reverse order

   procedure Prepend_To
     (To   : List_Id;
      Node : Node_Or_Entity_Id);
   pragma Inline (Prepend_To);
   --  Like Prepend, but arguments are the other way round

   procedure Remove (Node : Node_Or_Entity_Id);
   --  Removes Node, which must be a node that is a member of a node list,
   --  from this node list. The contents of Node are not otherwise affected.

   function Remove_Head (List : List_Id) return Node_Or_Entity_Id;
   --  Removes the head element of a node list, and returns the node (whose
   --  contents are not otherwise affected) as the result. If the node list
   --  is empty, then Empty is returned.

   function Remove_Next (Node : Node_Or_Entity_Id) return Node_Or_Entity_Id;
   --  Removes the item immediately following the given node, and returns it
   --  as the result. If Node is the last element of the list, then Empty is
   --  returned. Node must be a member of a list. Unlike Remove, Remove_Next
   --  is fast and does not involve any list traversal.

   procedure Initialize;
   --  Called at the start of compilation of each new main source file to
   --  initialize the allocation of the list table.

   procedure Lock;
   --  Called to lock tables before back end is called

   procedure Lock_Lists;
   --  Called to lock list contents when assertions are enabled. Without
   --  assertions calling this subprogram has no effect. The initial state
   --  of the lock is unlocked.

   procedure Unlock;
   --  Unlock tables, in cases where the back end needs to modify them

   procedure Unlock_Lists;
   --  Called to unlock list contents when assertions are enabled; if
   --  assertions are not enabled calling this subprogram has no effect.

   function Parent (List : List_Id) return Node_Or_Entity_Id;
   pragma Inline (Parent);
   --  Node lists may have a parent in the same way as a node. The function
   --  accesses the Parent value, which is either Empty when a list header
   --  is first created, or the value that has been set by Set_Parent.

   procedure Set_Parent (List : List_Id; Node : Node_Or_Entity_Id);
   pragma Inline (Set_Parent);
   --  Sets the parent field of the given list to reference the given node

   function No (List : List_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with No_List. This allows notations like
   --  "if No (Statements)" as opposed to "if Statements = No_List". Note that
   --  an empty list gives False for this test, as opposed to Is_Empty_List
   --  which gives True either for No_List or for an empty list.

   function Present (List : List_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with No_List. This allows notations like
   --  "if Present (Statements)" as opposed to "if Statements /= No_List".

   procedure Allocate_List_Tables (N : Node_Or_Entity_Id);
   pragma Inline (Allocate_List_Tables);
   --  Called when nodes table is expanded to include node N. This call
   --  makes sure that list structures internal to Nlists are adjusted
   --  appropriately to reflect this increase in the size of the nodes table.

   function Next_Node_Address return System.Address;
   function Prev_Node_Address return System.Address;
   --  These functions return the addresses of the Next_Node and Prev_Node
   --  tables (used in Back_End for Gigi).

end Nlists;
