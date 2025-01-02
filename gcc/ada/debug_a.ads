------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              D E B U G _ A                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains data and subprograms to support the -gnatda switch
--  that is used to generate output showing what node is being analyzed,
--  resolved, evaluated, or expanded.

with Types; use Types;

package Debug_A is

   --  Note: the following subprograms are used in a stack like manner, with
   --  an exit call matching each entry call. This means that they can keep
   --  track of the current node being worked on, with the entry call setting
   --  a new value, by pushing the Node_Id value on a stack, and the exit call
   --  popping this value off. Atree.Current_Error_Node is set by both the
   --  entry and exit routines to point to the current node so that an abort
   --  message indicates the node involved as accurately as possible.

   procedure Debug_A_Entry (S : String; N : Node_Id);
   pragma Inline (Debug_A_Entry);
   --  Generates a message prefixed by a sequence of bars showing the nesting
   --  depth (depth increases by 1 for a Debug_A_Entry call and is decreased
   --  by the corresponding Debug_A_Exit call). Then the string is output
   --  (analyzing, expanding etc), followed by information about the node.
   --  This output is generated only if the -gnatda switch is set. If that
   --  switch is not set, then no output is generated. This call also sets the
   --  Node_Id value in Atree.Current_Error_Node in case a bomb occurs. This is
   --  done unconditionally, whether or not the switch is set.

   procedure Debug_A_Exit (S : String; N : Node_Id; Comment : String);
   pragma Inline (Debug_A_Exit);
   --  Generates the corresponding termination message. The message is preceded
   --  by a sequence of bars, followed by the string S, the node number, and
   --  a trailing comment (e.g. " (already evaluated)"). This output is
   --  generated only if the -gnatda switch is set. If that switch is not
   --  set, then no output is generated. This call also resets the value in
   --  Atree.Current_Error_Node to what it was before the corresponding call
   --  to Debug_A_Entry.

end Debug_A;
