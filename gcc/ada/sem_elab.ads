------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E L A B                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2019, Free Software Foundation, Inc.         --
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

   -----------
   -- Types --
   -----------

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

      Generic_Spec_Level,
      Generic_Body_Level,
      --  A construct is at the "generic level" when it appears in a
      --  generic package library unit, ignoring enclosing packages. Example:

      --    generic
      --    package Pack is                      --  generic package spec
      --       package Nested is                 --  enclosing package ignored
      --          X ...                          --  at generic library level

      Instantiation_Level,
      --  A construct is at the "instantiation library level" when it appears
      --  in a library unit which is also an instantiation. Example:

      --    package Inst is new Gen;             --  at instantiation level

      Library_Spec_Level,
      Library_Body_Level,
      --  A construct is at the "library level" when it appears in a package
      --  library unit, ignoring enclosing packages. Example:

      --    package body Pack is                 --  package body
      --       package Nested is                 --  enclosing package ignored
      --          X ...                          --  at library level

      No_Level);
      --  This value is used to indicate that none of the levels above are in
      --  effect.

   subtype Generic_Level is Enclosing_Level_Kind range
     Generic_Spec_Level ..
     Generic_Body_Level;

   subtype Library_Level is Enclosing_Level_Kind range
     Library_Spec_Level ..
     Library_Body_Level;

   subtype Library_Or_Instantiation_Level is Enclosing_Level_Kind range
     Instantiation_Level ..
     Library_Body_Level;

   procedure Build_Call_Marker (N : Node_Id);
   pragma Inline (Build_Call_Marker);
   --  Create a call marker for call or requeue statement N and record it for
   --  later processing by the ABE mechanism.

   procedure Build_Variable_Reference_Marker
     (N     : Node_Id;
      Read  : Boolean;
      Write : Boolean);
   pragma Inline (Build_Variable_Reference_Marker);
   --  Create a variable reference marker for arbitrary node N if it mentions a
   --  variable, and record it for later processing by the ABE mechanism. Flag
   --  Read should be set when the reference denotes a read. Flag Write should
   --  be set when the reference denotes a write.

   procedure Check_Elaboration_Scenarios;
   --  Examine each scenario recorded during analysis/resolution and apply the
   --  Ada or SPARK elaboration rules taking into account the model in effect.
   --  This processing detects and diagnoses ABE issues, installs conditional
   --  ABE checks or guaranteed ABE failures, and ensures the elaboration of
   --  units.

   function Find_Enclosing_Level (N : Node_Id) return Enclosing_Level_Kind;
   pragma Inline (Find_Enclosing_Level);
   --  Determine the enclosing level of arbitrary node N

   procedure Initialize;
   pragma Inline (Initialize);
   --  Initialize the internal structures of this unit

   procedure Kill_Elaboration_Scenario (N : Node_Id);
   --  Determine whether arbitrary node N denotes a scenario which requires
   --  ABE diagnostics or runtime checks and eliminate it from a region with
   --  dead code.

   procedure Record_Elaboration_Scenario (N : Node_Id);
   pragma Inline (Record_Elaboration_Scenario);
   --  Determine whether atribtray node N denotes a scenario which requires
   --  ABE diagnostics or runtime checks. If this is the case, store N for
   --  later processing.

   ---------------------------------------------------------------------------
   --                                                                       --
   --  L E G A C Y    A C C E S S    B E F O R E    E L A B O R A T I O N   --
   --                                                                       --
   --                          M E C H A N I S M                            --
   --                                                                       --
   ---------------------------------------------------------------------------

   --  This section contains the implementation of the pre-18.x Legacy ABE
   --  Mechanism. The mechanism can be activated using switch -gnatH (legacy
   --  elaboration checking mode enabled).

   procedure Check_Elab_Assign (N : Node_Id);
   --  N is either the left side of an assignment, or a procedure argument for
   --  a mode OUT or IN OUT formal. This procedure checks for a possible case
   --  of access to an entity from elaboration code before the entity has been
   --  initialized, and issues appropriate warnings.

   procedure Check_Elab_Call
     (N            : Node_Id;
      Outer_Scope  : Entity_Id := Empty;
      In_Init_Proc : Boolean   := False);
   --  Check a call for possible elaboration problems. The node N is either an
   --  N_Function_Call or N_Procedure_Call_Statement node or an access
   --  attribute reference whose prefix is a subprogram.
   --
   --  If SPARK_Mode is On, then N can also be a variable reference, since
   --  SPARK requires the use of Elaborate_All for references to variables
   --  in other packages.

   --  The Outer_Scope argument indicates whether this is an outer level
   --  call from Sem_Res (Outer_Scope set to Empty), or an internal recursive
   --  call (Outer_Scope set to entity of outermost call, see body). The flag
   --  In_Init_Proc should be set whenever the current context is a type
   --  init proc.

   --  Note: this might better be called Check_Elab_Reference (to recognize
   --  the SPARK case), but we prefer to keep the original name, since this
   --  is primarily used for checking for calls that could generate an ABE).

   procedure Check_Elab_Calls;
   --  Not all the processing for Check_Elab_Call can be done at the time
   --  of calls to Check_Elab_Call. This is because for internal calls, we
   --  need to wait to complete the check until all generic bodies have been
   --  instantiated. The Check_Elab_Calls procedure cleans up these waiting
   --  checks. It is called once after the completion of instantiation.

   procedure Check_Elab_Instantiation
     (N           : Node_Id;
      Outer_Scope : Entity_Id := Empty);
   --  Check an instantiation for possible elaboration problems. N is an
   --  instantiation node (N_Package_Instantiation, N_Function_Instantiation,
   --  or N_Procedure_Instantiation), and Outer_Scope indicates if this is
   --  an outer level call from Sem_Ch12 (Outer_Scope set to Empty), or an
   --  internal recursive call (Outer_Scope set to scope of outermost call,
   --  see body for further details). The returned value is relevant only
   --  for an outer level call, and is set to False if an elaboration error
   --  is bound to occur on the instantiation, and True otherwise. This is
   --  used by the caller to signal that the body of the instance should
   --  not be generated (see detailed description in body).

   procedure Check_Task_Activation (N : Node_Id);
   --  At the point at which tasks are activated in a package body, check
   --  that the bodies of the tasks are elaborated.

end Sem_Elab;
