------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R _ S C O                               --
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

--  This package contains the routines used to deal with generation and output
--  of Source Coverage Obligations (SCO's) used for coverage analysis purposes.
--  See package SCOs for full documentation of format of SCO information.

with Types; use Types;

package Par_SCO is

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables for a new compilation

   procedure SCO_Record_Raw (U : Unit_Number_Type);
   --  This procedure scans the tree for the unit identified by U, populating
   --  internal tables recording the SCO information. Note that this is done
   --  before any semantic analysis/expansion happens.

   procedure Set_SCO_Condition (Cond : Node_Id; Val : Boolean);
   --  This procedure is called during semantic analysis to record a condition
   --  which has been identified as always True or always False, as indicated
   --  by Val. The condition is identified by the First_Sloc value in the
   --  original tree associated with Cond.

   procedure Set_SCO_Logical_Operator (Op : Node_Id);
   --  Mark some putative logical operator as a short circuit one

   procedure Set_SCO_Pragma_Enabled (Loc : Source_Ptr);
   --  This procedure is called from Sem_Prag when a pragma is enabled (i.e.
   --  when the Pragma_Enabled flag is set). Loc is the Sloc of the N_Pragma
   --  node. This is used to enable the corresponding statement SCO entry. Note
   --  that we use the Sloc as the key here, since in the generic case, the
   --  analysis is on a copy of the node, which is different from the node
   --  seen by Par_SCO in the parse tree (but the Sloc values are the same).

   function SCO_Pragma_Disabled (Loc : Source_Ptr) return Boolean;
   --  True if Loc is the source location of a disabled pragma

   procedure SCO_Record_Filtered;
   --  This procedure filters remaining putative AND/OR short-circuit operators
   --  from the internal SCO raw table after the semantic analysis and fills
   --  the filtered SCO table.

   procedure SCO_Output;
   --  Outputs SCO lines for all units, with appropriate section headers, as
   --  recorded by previous calls to SCO_Record, possibly modified by calls to
   --  Set_SCO_Condition.

   procedure dsco;
   --  Debug routine to dump internal SCO tables. This is a raw format dump
   --  showing exactly what the tables contain.

   procedure pscos;
   --  Debugging procedure to output contents of SCO binary tables in the
   --  format in which they appear in an ALI file.

end Par_SCO;
