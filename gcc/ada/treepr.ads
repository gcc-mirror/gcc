------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T R E E P R                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
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

with Types; use Types;
package Treepr is

--  This package provides printing routines for the abstract syntax tree
--  These routines are intended only for debugging use.

   procedure Tree_Dump;
   --  This routine is called from the GNAT main program to dump trees as
   --  requested by debug options (including tree of Standard if requested).

   procedure Print_Tree_Node (N : Node_Id; Label : String := "");
   --  Prints a single tree node, without printing descendants. The Label
   --  string is used to preface each line of the printed output.

   procedure Print_Node_Briefly (N : Node_Id);
   --  Terse version of Print_Tree_Node

   procedure Print_Tree_List (L : List_Id);
   --  Prints a single node list, without printing the descendants of any
   --  of the nodes in the list

   procedure Print_Tree_Elist (E : Elist_Id);
   --  Prints a single node list, without printing the descendants of any
   --  of the nodes in the list

   procedure Print_Node_Subtree (N : Node_Id);
   --  Prints the subtree routed at a specified tree node, including all
   --  referenced descendants.

   procedure Print_List_Subtree (L : List_Id);
   --  Prints the subtree consisting of the given node list and all its
   --  referenced descendants.

   procedure Print_Elist_Subtree (E : Elist_Id);
   --  Prints the subtree consisting of the given element list and all its
   --  referenced descendants.

   --  The following debugging procedures are intended to be called from gdb.
   --  Note that in several cases there are synonyms which represent historical
   --  development, and we keep them because some people are used to them!

   function p   (N : Union_Id) return Node_Or_Entity_Id;
   function par (N : Union_Id) return Node_Or_Entity_Id;
   pragma Export (Ada, p);
   pragma Export (Ada, par);
   --  Return parent of a list or node (depending on the value of N). If N
   --  is neither a list nor a node id, then prints a message to that effect
   --  and returns Empty.

   procedure pn (N : Union_Id);
   procedure pp (N : Union_Id);
   procedure pe (N : Union_Id);
   pragma Export (Ada, pn);
   pragma Export (Ada, pp);
   pragma Export (Ada, pe);
   --  Print a node, node list, uint, or anything else that falls under
   --  the definition of Union_Id. Historically this was only for printing
   --  nodes, hence the name.

   procedure pt  (N : Union_Id);
   procedure ppp (N : Union_Id);
   pragma Export (Ada, pt);
   pragma Export (Ada, ppp);
   --  Same as pn/pp, except prints subtrees. For Nodes, it is exactly the same
   --  as Print_Node_Subtree. For Elists it is the same as Print_Elist_Subtree.
   --  For Lists, it is the same as Print_Tree_List. If given anything other
   --  than a Node, List, or Elist, same effect as pn.

   procedure pl (L : Int);
   pragma Export (Ada, pl);
   --  Same as Print_Tree_List, except that you can use e.g. 66 instead of
   --  -99999966. In other words for the positive case we fill out to 8 digits
   --  on the left and add a minus sign. This just saves some typing in the
   --  debugger.

end Treepr;
