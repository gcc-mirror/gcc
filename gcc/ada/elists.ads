------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E L I S T S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides facilities for manipulating lists of nodes (see
--  package Atree for format and implementation of tree nodes). Separate list
--  elements are allocated to represent elements of these lists, so it is
--  possible for a given node to be on more than one element list at a time.
--  See also package Nlists, which provides another form that is threaded
--  through the nodes themselves (using the Link field), which is more time
--  and space efficient, but a node can be only one such list.

with Types;  use Types;
with System;

package Elists is

   --  An element list is represented by a header that is allocated in the
   --  Elist header table. This header contains pointers to the first and
   --  last elements in the list, or to No_Elmt if the list is empty.

   --  The elements in the list each contain a pointer to the next element
   --  and a pointer to the referenced node. Putting a node into an element
   --  list causes no change at all to the node itself, so a node may be
   --  included in multiple element lists, and the nodes thus included may
   --  or may not be elements of node lists (see package Nlists).

   procedure Initialize;
   --  Initialize allocation of element list tables. Called at the start of
   --  compiling each new main source file. Note that Initialize must not be
   --  called if Tree_Read is used.

   procedure Lock;
   --  Lock tables used for element lists before calling backend

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using Tree_Read.
   --  Note that Initialize should not be called if Tree_Read is used.
   --  Tree_Read includes all necessary initialization.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using Tree_Write

   function Last_Elist_Id return Elist_Id;
   --  Returns Id of last allocated element list header

   function Elists_Address return System.Address;
   --  Return address of Elists table (used in Back_End for Gigi call)

   function Num_Elists return Nat;
   --  Number of currently allocated element lists

   function Last_Elmt_Id return Elmt_Id;
   --  Returns Id of last allocated list element

   function Elmts_Address return System.Address;
   --  Return address of Elmts table (used in Back_End for Gigi call)

   function Node (Elmt : Elmt_Id) return Node_Id;
   pragma Inline (Node);
   --  Returns the value of a given list element. Returns Empty if Elmt
   --  is set to No_Elmt.

   function New_Elmt_List return Elist_Id;
   --  Creates a new empty element list. Typically this is used to initialize
   --  a field in some other node which points to an element list where the
   --  list is then subsequently filled in using Append calls.

   function First_Elmt (List : Elist_Id) return Elmt_Id;
   pragma Inline (First_Elmt);
   --  Obtains the first element of the given element list or, if the
   --  list has no items, then No_Elmt is returned.

   function Last_Elmt (List : Elist_Id) return Elmt_Id;
   pragma Inline (Last_Elmt);
   --  Obtains the last element of the given element list or, if the
   --  list has no items, then No_Elmt is returned.

   function Next_Elmt (Elmt : Elmt_Id) return Elmt_Id;
   pragma Inline (Next_Elmt);
   --  This function returns the next element on an element list. The argument
   --  must be a list element other than No_Elmt. Returns No_Elmt if the given
   --  element is the last element of the list.

   procedure Next_Elmt (Elmt : in out Elmt_Id);
   pragma Inline (Next_Elmt);
   --  Next_Elmt (Elmt) is equivalent to Elmt := Next_Elmt (Elmt)

   function Is_Empty_Elmt_List (List : Elist_Id) return Boolean;
   pragma Inline (Is_Empty_Elmt_List);
   --  This function determines if a given tree id references an element list
   --  that contains no items.

   procedure Append_Elmt (Node : Node_Id; To : Elist_Id);
   --  Appends Node at the end of To, allocating a new element.

   procedure Prepend_Elmt (Node : Node_Id; To : Elist_Id);
   --  Appends Node at the beginning of To, allocating a new element.

   procedure Insert_Elmt_After (Node : Node_Id; Elmt : Elmt_Id);
   --  Add a new element (Node) right after the pre-existing element Elmt
   --  It is invalid to call this subprogram with Elmt = No_Elmt.

   procedure Replace_Elmt (Elmt : Elmt_Id; New_Node : Node_Id);
   pragma Inline (Replace_Elmt);
   --  Causes the given element of the list to refer to New_Node, the node
   --  which was previously referred to by Elmt is effectively removed from
   --  the list and replaced by New_Node.

   procedure Remove_Elmt (List : Elist_Id; Elmt : Elmt_Id);
   --  Removes Elmt from the given list. The node itself is not affected,
   --  but the space used by the list element may be (but is not required
   --  to be) freed for reuse in a subsequent Append_Elmt call.

   procedure Remove_Last_Elmt (List : Elist_Id);
   --  Removes the last element of the given list. The node itself is not
   --  affected, but the space used by the list element may be (but is not
   --  required to be) freed for reuse in a subsequent Append_Elmt call.

   function No (List : Elist_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with No_Elist. This allows notations like
   --  "if No (Statements)" as opposed to "if Statements = No_Elist".

   function Present (List : Elist_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with No_Elist. This allows notations like
   --  "if Present (Statements)" as opposed to "if Statements /= No_Elist".

   function No (Elmt : Elmt_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with No_Elmt. This allows notations like
   --  "if No (Operation)" as opposed to "if Operation = No_Elmt".

   function Present (Elmt : Elmt_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with No_Elmt. This allows notations like
   --  "if Present (Operation)" as opposed to "if Operation /= No_Elmt".

end Elists;
