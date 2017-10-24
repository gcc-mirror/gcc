------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E L A B                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2017, Free Software Foundation, Inc.         --
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

--  This package contains routines which handle access-before-elaboration
--  run-time checks and compile-time diagnostics. See the body for details.

with Types; use Types;

package Sem_Elab is

   procedure Build_Call_Marker (N : Node_Id);
   --  Create a call marker for call or requeue statement N and record it for
   --  later processing by the ABE mechanism.

   procedure Check_Elaboration_Scenarios;
   --  Examine each scenario recorded during analysis/resolution and apply the
   --  Ada or SPARK elaboration rules taking into account the model in effect.
   --  This processing detects and diagnoses ABE issues, installs conditional
   --  ABE checks or guaranteed ABE failures, and ensures the elaboration of
   --  units.

   --  The following type classifies the various enclosing levels used in ABE
   --  diagnostics.

   type Enclosing_Level_Kind is
     (Declaration_Level,
      --  A construct is at the "declaration level" when it appears within the
      --  declarations of a block statement, an entry body, a subprogram body,
      --  or a task body, ignoring enclosing packages. Example:

      --    package Pack is
      --       procedure Proc is                 --  subprogram body
      --          package Nested is              --  enclosing package ignored
      --             X ...                       --  at declaration level

      Generic_Package_Spec,
      Generic_Package_Body,
      --  A construct is at the "generic library level" when it appears in a
      --  generic package library unit, ignoring enclosing packages. Example:

      --    generic
      --    package Pack is                      --  generic package spec
      --       package Nested is                 --  enclosing package ignored
      --          X ...                          --  at generic library level

      Instantiation,
      --  A construct is at the "instantiation library level" when it appears
      --  in a library unit which is also an instantiation. Example:

      --    package Inst is new Gen;             --  at instantiation level

      Package_Spec,
      Package_Body,
      --  A construct is at the "library level" when it appears in a package
      --  library unit, ignoring enclosing packages. Example:

      --    package body Pack is                 --  package body
      --       package Nested is                 --  enclosing package ignored
      --          X ...                          --  at library level

      No_Level);
      --  This value is used to indicate that none of the levels above are in
      --  effect.

   subtype Generic_Library_Level is Enclosing_Level_Kind range
     Generic_Package_Spec ..
     Generic_Package_Body;

   subtype Library_Level is Enclosing_Level_Kind range
     Package_Spec ..
     Package_Body;

   subtype Any_Library_Level is Enclosing_Level_Kind range
     Generic_Package_Spec ..
     Package_Body;

   function Find_Enclosing_Level (N : Node_Id) return Enclosing_Level_Kind;
   --  Determine the enclosing level of arbitrary node N

   procedure Initialize;
   --  Initialize the internal structures of this unit

   procedure Kill_Elaboration_Scenario (N : Node_Id);
   --  Determine whether arbitrary node N denotes a scenario which requires
   --  ABE diagnostics or runtime checks and eliminate it from a region with
   --  dead code.

   procedure Record_Elaboration_Scenario (N : Node_Id);
   --  Determine whether atribtray node N denotes a scenario which requires
   --  ABE diagnostics or runtime checks. If this is the case, store N into
   --  a table for later processing.

end Sem_Elab;
