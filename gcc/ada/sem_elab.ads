------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E L A B                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2015, Free Software Foundation, Inc.         --
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

--  This package contains the routines used to deal with issuing warnings
--  for cases of calls that may require warnings about possible access
--  before elaboration.

with Types; use Types;

package Sem_Elab is

   -----------------------------
   -- Description of Approach --
   -----------------------------

   --  Every non-static call that is encountered by Sem_Res results in a call
   --  to Check_Elab_Call, with N being the call node, and Outer set to its
   --  default value of True. In addition X'Access is treated like a call
   --  for the access-to-procedure case, and in SPARK mode only we also
   --  check variable references.

   --  The goal of Check_Elab_Call is to determine whether or not the reference
   --  in question can generate an access before elaboration error (raising
   --  Program_Error) either by directly calling a subprogram whose body
   --  has not yet been elaborated, or indirectly, by calling a subprogram
   --  whose body has been elaborated, but which contains a call to such a
   --  subprogram.

   --  In addition, in SPARK mode, we are checking for a variable reference in
   --  another package, which requires an explicit Elaborate_All pragma.

   --  The only references that we need to look at the outer level are
   --  references that occur in elaboration code. There are two cases. The
   --  reference can be at the outer level of elaboration code, or it can
   --  be within another unit, e.g. the elaboration code of a subprogram.

   --  In the case of an elaboration call at the outer level, we must trace
   --  all calls to outer level routines either within the current unit or to
   --  other units that are with'ed. For calls within the current unit, we can
   --  determine if the body has been elaborated or not, and if it has not,
   --  then a warning is generated.

   --  Note that there are two subcases. If the original call directly calls a
   --  subprogram whose body has not been elaborated, then we know that an ABE
   --  will take place, and we replace the call by a raise of Program_Error.
   --  If the call is indirect, then we don't know that the PE will be raised,
   --  since the call might be guarded by a conditional. In this case we set
   --  Do_Elab_Check on the call so that a dynamic check is generated, and
   --  output a warning.

   --  For calls to a subprogram in a with'ed unit or a 'Access or variable
   --  refernece (SPARK mode case), we require that a pragma Elaborate_All
   --  or pragma Elaborate be present, or that the referenced unit have a
   --  pragma Preelaborate, pragma Pure, or pragma Elaborate_Body. If none
   --  of these conditions is met, then a warning is generated that a pragma
   --  Elaborate_All may be needed (error in the SPARK case), or an implicit
   --  pragma is generated.

   --  For the case of an elaboration call at some inner level, we are
   --  interested in tracing only calls to subprograms at the same level,
   --  i.e. those that can be called during elaboration. Any calls to
   --  outer level routines cannot cause ABE's as a result of the original
   --  call (there might be an outer level call to the subprogram from
   --  outside that causes the ABE, but that gets analyzed separately).

   --  Note that we never trace calls to inner level subprograms, since
   --  these cannot result in ABE's unless there is an elaboration problem
   --  at a lower level, which will be separately detected.

   --  Note on pragma Elaborate. The checking here assumes that a pragma
   --  Elaborate on a with'ed unit guarantees that subprograms within the
   --  unit can be called without causing an ABE. This is not in fact the
   --  case since pragma Elaborate does not guarantee the transitive
   --  coverage guaranteed by Elaborate_All. However, we decide to trust
   --  the user in this case.

   --------------------------------------
   -- Instantiation Elaboration Errors --
   --------------------------------------

   --  A special case arises when an instantiation appears in a context
   --  that is known to be before the body is elaborated, e.g.

   --       generic package x is ...
   --       ...
   --       package xx is new x;
   --       ...
   --       package body x is ...

   --  In this situation it is certain that an elaboration error will
   --  occur, and an unconditional raise Program_Error statement is
   --  inserted before the instantiation, and a warning generated.

   --  The problem is that in this case we have no place to put the
   --  body of the instantiation. We can't put it in the normal place,
   --  because it is too early, and will cause errors to occur as a
   --  result of referencing entities before they are declared.

   --  Our approach in this case is simply to avoid creating the body
   --  of the instantiation in such a case. The instantiation spec is
   --  modified to include dummy bodies for all subprograms, so that
   --  the resulting code does not contain subprogram specs with no
   --  corresponding bodies.

   procedure Check_Elab_Call
     (N            : Node_Id;
      Outer_Scope  : Entity_Id := Empty;
      In_Init_Proc : Boolean   := False);
   --  Check a call for possible elaboration problems. The node N is either an
   --  N_Function_Call or N_Procedure_Call_Statement node or an access
   --  attribute reference whose prefix is a subprogram.
   --
   --  If SPARK_Mode is On, then N can also be a variablr reference, since
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

   procedure Check_Elab_Assign (N : Node_Id);
   --  N is either the left side of an assignment, or a procedure argument for
   --  a mode OUT or IN OUT formal. This procedure checks for a possible case
   --  of access to an entity from elaboration code before the entity has been
   --  initialized, and issues appropriate warnings.

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
   --  Tt the point at which tasks are activated in a package body, check
   --  that the bodies of the tasks are elaborated.

end Sem_Elab;
