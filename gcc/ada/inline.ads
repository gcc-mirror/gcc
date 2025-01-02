------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
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

--  This module handles four kinds of inlining activity:

--  a) Instantiation of generic bodies. This is done unconditionally, after
--  analysis and expansion of the main unit.

--  b) Compilation of unit bodies that contain the bodies of inlined sub-
--  programs. This is done only if inlining is enabled (-gnatn). Full inlining
--  requires that a) and b) be mutually recursive, because each step may
--  generate another generic expansion and further inlined calls.

--  c) Front-end inlining for Inline_Always subprograms. This is primarily an
--  expansion activity that is performed for performance reasons, and when the
--  target does not use the GCC back end.

--  d) Front-end inlining for GNATprove, to perform source transformations
--  to simplify formal verification. The machinery used is the same as for
--  Inline_Always subprograms, but there are fewer restrictions on the source
--  of subprograms.

with Opt;    use Opt;
with Sem;    use Sem;
with Types;  use Types;
with Warnsw; use Warnsw;

package Inline is

   --------------------------------
   -- Generic Body Instantiation --
   --------------------------------

   --  The bodies of generic instantiations are built after semantic analysis
   --  of the main unit is complete. Generic instantiations are saved in a
   --  global data structure, and the bodies constructed by means of a separate
   --  analysis and expansion step.

   --  See full description in body of Sem_Ch12 for more details

   type Pending_Body_Info is record
      Inst_Node : Node_Id;
      --  Node for instantiation that requires the body

      Act_Decl : Node_Id;
      --  Declaration for package or subprogram spec for instantiation

      Fin_Scop : Node_Id;
      --  Enclosing finalization scope for package instantiation

      Config_Switches : Config_Switches_Type;
      --  Capture the values of configuration switches

      Current_Sem_Unit : Unit_Number_Type;
      --  The semantic unit within which the instantiation is found. Must be
      --  restored when compiling the body, to insure that internal entities
      --  use the same counter and are unique over spec and body.

      Expander_Status : Boolean;
      --  If the body is instantiated only for semantic checking, expansion
      --  must be inhibited.

      Scope_Suppress           : Suppress_Record;
      Local_Suppress_Stack_Top : Suppress_Stack_Entry_Ptr;
      --  Save suppress information at the point of instantiation. Used to
      --  properly inherit check status active at this point (see RM 11.5
      --  (7.2/2), AI95-00224-01):
      --
      --    "If a checking pragma applies to a generic instantiation, then the
      --    checking pragma also applies to the instance. If a checking pragma
      --    applies to a call to a subprogram that has a pragma Inline applied
      --    to it, then the checking pragma also applies to the inlined
      --    subprogram body".
      --
      --  This means we have to capture this information from the current scope
      --  at the point of instantiation.

      Warnings : Warnings_State;
      --  Capture values of warning flags
   end record;

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling backend

   procedure Instantiate_Bodies;
   --  This procedure is called after semantic analysis is complete, to
   --  instantiate the bodies of generic instantiations that appear in the
   --  compilation unit.

   procedure Add_Inlined_Body (E : Entity_Id; N : Node_Id);
   --  E is an inlined subprogram appearing in a call, either explicitly or in
   --  a discriminant check for which gigi builds a call or an at-end handler.
   --  Add E's enclosing unit to Inlined_Bodies so that E can be subsequently
   --  retrieved and analyzed. N is the node giving rise to the call to E.

   procedure Add_Pending_Instantiation
     (Inst     : Node_Id;
      Act_Decl : Node_Id;
      Fin_Scop : Node_Id := Empty);
   --  Add an entry in the table of generic bodies to be instantiated.

   procedure Analyze_Inlined_Bodies;
   --  At end of compilation, analyze the bodies of all units that contain
   --  inlined subprograms that are actually called.

   procedure Build_Body_To_Inline (N : Node_Id; Spec_Id : Entity_Id);
   --  If a subprogram has pragma Inline and inlining is active, use generic
   --  machinery to build an unexpanded body for the subprogram. This body is
   --  subsequently used for inline expansions at call sites. If subprogram can
   --  be inlined (depending on size and nature of local declarations) the
   --  template body is created. Otherwise subprogram body is treated normally
   --  and calls are not inlined in the frontend. If proper warnings are
   --  enabled and the subprogram contains a construct that cannot be inlined,
   --  the problematic construct is flagged accordingly.

   function Call_Can_Be_Inlined_In_GNATprove_Mode
    (N    : Node_Id;
     Subp : Entity_Id) return Boolean;
   --  Returns False if the call in node N to subprogram Subp cannot be inlined
   --  in GNATprove mode, because it may otherwise lead to missing a check
   --  on type conversion of input parameters, or a missing memory leak on
   --  an output parameter. Returns True otherwise.

   function Can_Be_Inlined_In_GNATprove_Mode
     (Spec_Id : Entity_Id;
      Body_Id : Entity_Id) return Boolean;
   --  Returns True if the subprogram identified by Spec_Id and Body_Id can
   --  be inlined in GNATprove mode. One but not both of Spec_Id and Body_Id
   --  can be Empty. Body_Id is Empty when doing a partial check on a call
   --  to a subprogram whose body has not been seen yet, to know whether this
   --  subprogram could possibly be inlined. GNATprove relies on this to adapt
   --  its treatment of the subprogram.

   procedure Cannot_Inline
     (Msg           : String;
      N             : Node_Id;
      Subp          : Entity_Id;
      Is_Serious    : Boolean := False;
      Suppress_Info : Boolean := False)
     with
       Pre => Msg'First <= Msg'Last
       and then Msg (Msg'Last) = '?';
   --  This procedure is called if the node N, an instance of a call to
   --  subprogram Subp, cannot be inlined. Msg is the message to be issued,
   --  which ends with ? (it does not end with ?p?, this routine takes care of
   --  the need to change ? to ?p?). Suppress_Info is set to True to prevent
   --  issuing an info message in GNATprove mode. The behavior of this routine
   --  depends on the value of Back_End_Inlining:
   --
   --    * If Back_End_Inlining is not set (ie. legacy frontend inlining model)
   --      then if Subp has a pragma Always_Inlined, then an error message is
   --      issued (by removing the last character of Msg). If Subp is not
   --      Always_Inlined, then a warning is issued if the flag Ineffective_
   --      Inline_Warnings is set, adding ?p to the msg, and if not, the call
   --      has no effect.
   --
   --    * If Back_End_Inlining is set then:
   --      - If Is_Serious is true, then an error is reported (by removing the
   --        last character of Msg);
   --
   --      - otherwise:
   --
   --        * Compiling without optimizations if Subp has a pragma
   --          Always_Inlined, then an error message is issued; if Subp is
   --          not Always_Inlined, then a warning is issued if the flag
   --          Ineffective_Inline_Warnings is set (adding p?), and if not,
   --          the call has no effect.
   --
   --        * Compiling with optimizations then a warning is issued if the
   --          flag Ineffective_Inline_Warnings is set (adding p?); otherwise
   --          no effect since inlining may be performed by the backend.

   procedure Check_And_Split_Unconstrained_Function
     (N       : Node_Id;
      Spec_Id : Entity_Id;
      Body_Id : Entity_Id);
   --  Spec_Id and Body_Id are the entities of the specification and body of
   --  the subprogram body N. If N can be inlined by the frontend (supported
   --  cases documented in Check_Body_To_Inline) then build the body-to-inline
   --  associated with N and attach it to the declaration node of Spec_Id.

   procedure Check_Object_Renaming_In_GNATprove_Mode (Spec_Id : Entity_Id)
   with
     Pre => GNATprove_Mode;
   --  This procedure is called only in GNATprove mode, on subprograms for
   --  which a Body_To_Inline was created, to check if the subprogram has
   --  references to object renamings which will be replaced by the special
   --  SPARK expansion into nodes of a different kind, which is not expected
   --  by the inlining mechanism. In that case, the Body_To_Inline is deleted.

   procedure Check_Package_Body_For_Inlining (N : Node_Id; P : Entity_Id);
   --  If front-end inlining is enabled and a package declaration contains
   --  inlined subprograms, load and compile the package body to collect the
   --  bodies of these subprograms, so they are available to inline calls.
   --  N is the compilation unit for the package.

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id);
   --  If called subprogram can be inlined by the front-end, retrieve the
   --  analyzed body, replace formals with actuals and expand call in place.
   --  Generate thunks for actuals that are expressions, and insert the
   --  corresponding constant declarations before the call. If the original
   --  call is to a derived operation, the return type is the one of the
   --  derived operation, but the body is that of the original, so return
   --  expressions in the body must be converted to the desired type (which
   --  is simply not noted in the tree without inline expansion).

   function Has_Excluded_Declaration
     (Subp  : Entity_Id;
      Decls : List_Id) return Boolean;
   --  Check a list of declarations, Decls, that make the inlining of Subp not
   --  worthwhile

   function Has_Excluded_Statement
     (Subp  : Entity_Id;
      Stats : List_Id) return Boolean;
   --  Check a list of statements, Stats, that make inlining of Subp not
   --  worthwhile, including any tasking statement, nested at any level.

   procedure Inline_Static_Function_Call
     (N : Node_Id; Subp : Entity_Id);
   --  Evaluate static call to a static function Subp, substituting actuals in
   --  place of references to their corresponding formals and rewriting the
   --  call N as a fully folded and static result expression.

   procedure List_Inlining_Info;
   --  Generate listing of calls inlined by the frontend plus listing of
   --  calls to inline subprograms passed to the backend.

   procedure Remove_Dead_Instance (N : Node_Id);
   --  If an instantiation appears in unreachable code, delete the pending
   --  body instance.

end Inline;
