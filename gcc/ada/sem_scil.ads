------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ S C I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009-2025, Free Software Foundation, Inc.         --
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

--  This package contains routines involved in the frontend addition and
--  verification of SCIL nodes.

with Atree; use Atree;
with Types; use Types;

package Sem_SCIL is

   --  SCIL (Statically Checkable Intermediate Language) is produced by the
   --  CodePeer back end (aka gnat2scil). For some constructs (tagged type
   --  declarations, dispatching calls, classwide membership tests), the
   --  CodePeer back end needs to locate certain nodes in the tree. To allow
   --  CodePeer to do this without introducing unwanted dependencies on the
   --  details of the FE's expansion strategies, SCIL_Nodes are generated.

   --  For example, a dispatching call in the Ada source will, if CodePeer mode
   --  is enabled, result in the FE's generation of an N_Scil_Dispatching_Call
   --  node decorated with semantic attributes which identify the call itself,
   --  the primitive operation being called, the tagged type to which the
   --  operation belongs, and the controlling tag value of the call. If the FE
   --  implements some new expansion strategy for dispatching calls but this
   --  interface is preserved, the CodePeer back end should be unaffected.

   function Check_SCIL_Node (N : Node_Id) return Traverse_Result;
   --  Process a single node during the tree traversal. Done to verify that
   --  SCIL nodes decoration fulfill the requirements of the SCIL backend.

   procedure Check_SCIL_Nodes is new Traverse_Proc (Check_SCIL_Node);
   --  The traversal procedure itself

   function First_Non_SCIL_Node (L : List_Id) return Node_Id;
   --  Returns the first non-SCIL node of list L

   function Next_Non_SCIL_Node (N : Node_Id) return Node_Id;
   --  N must be a member of a list. Returns the next non SCIL node in the list
   --  containing N, or Empty if this is the last non SCIL node in the list.

end Sem_SCIL;
