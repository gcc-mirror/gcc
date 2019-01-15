------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2014-2019, Free Software Foundation, Inc.         --
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

--  This package contains routines that deal with the static and runtime
--  semantics of Ghost entities.

with Opt;   use Opt;
with Types; use Types;

package Ghost is

   procedure Check_Ghost_Completion
     (Prev_Id  : Entity_Id;
      Compl_Id : Entity_Id);
   --  Verify that the Ghost policy of initial entity Prev_Id is compatible
   --  with the Ghost policy of completing entity Compl_Id. Emit an error if
   --  this is not the case.

   procedure Check_Ghost_Context
     (Ghost_Id  : Entity_Id;
      Ghost_Ref : Node_Id);
   --  Determine whether node Ghost_Ref appears within a Ghost-friendly context
   --  where Ghost entity Ghost_Id can safely reside.

   procedure Check_Ghost_Overriding
     (Subp            : Entity_Id;
      Overridden_Subp : Entity_Id);
   --  Verify that the Ghost policy of parent subprogram Overridden_Subp is
   --  compatible with the Ghost policy of overriding subprogram Subp. Emit
   --  an error if this is not the case.

   procedure Check_Ghost_Primitive (Prim : Entity_Id; Typ : Entity_Id);
   --  Verify that the Ghost policy of primitive operation Prim is the same as
   --  the Ghost policy of tagged type Typ. Emit an error if this is not the
   --  case.

   procedure Check_Ghost_Refinement
     (State      : Node_Id;
      State_Id   : Entity_Id;
      Constit    : Node_Id;
      Constit_Id : Entity_Id);
   --  Verify that the Ghost policy of constituent Constit_Id is compatible
   --  with the Ghost policy of abstract state State_I.

   procedure Check_Ghost_Type (Typ : Entity_Id);
   --  Verify that Ghost type Typ is neither concurrent, nor effectively
   --  volatile.

   function Implements_Ghost_Interface (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ implements at least one Ghost interface

   procedure Initialize;
   --  Initialize internal tables

   procedure Install_Ghost_Region (Mode : Ghost_Mode_Type; N : Node_Id);
   pragma Inline (Install_Ghost_Region);
   --  Install a Ghost region described by mode Mode and ignored region start
   --  node N.

   function Is_Ghost_Assignment (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes an assignment statement whose
   --  target is a Ghost entity.

   function Is_Ghost_Declaration (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a declaration which defines
   --  a Ghost entity.

   function Is_Ghost_Pragma (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a pragma which encloses a
   --  Ghost entity or is associated with a Ghost entity.

   function Is_Ghost_Procedure_Call (N : Node_Id) return Boolean;
   --  Determine whether arbitrary node N denotes a procedure call invoking a
   --  Ghost procedure.

   function Is_Ignored_Ghost_Unit (N : Node_Id) return Boolean;
   --  Determine whether compilation unit N is subject to pragma Ghost with
   --  policy Ignore.

   procedure Lock;
   --  Lock internal tables before calling backend

   procedure Mark_And_Set_Ghost_Assignment (N : Node_Id);
   --  Mark assignment statement N as Ghost when:
   --
   --    * The left hand side denotes a Ghost entity
   --
   --  Install the Ghost mode of the assignment statement. This routine starts
   --  a Ghost region and must be used with routine Restore_Ghost_Region.

   procedure Mark_And_Set_Ghost_Body
     (N       : Node_Id;
      Spec_Id : Entity_Id);
   --  Mark package or subprogram body N as Ghost when:
   --
   --    * The body is subject to pragma Ghost
   --
   --    * The body completes a previous declaration whose spec denoted by
   --      Spec_Id is a Ghost entity.
   --
   --    * The body appears within a Ghost region
   --
   --  Install the Ghost mode of the body. This routine starts a Ghost region
   --  and must be used with routine Restore_Ghost_Region.

   procedure Mark_And_Set_Ghost_Completion
     (N       : Node_Id;
      Prev_Id : Entity_Id);
   --  Mark completion N of a deferred constant or private type [extension]
   --  Ghost when:
   --
   --    * The entity of the previous declaration denoted by Prev_Id is Ghost
   --
   --    * The completion appears within a Ghost region
   --
   --  Install the Ghost mode of the completion. This routine starts a Ghost
   --  region and must be used with routine Restore_Ghost_Region.

   procedure Mark_And_Set_Ghost_Declaration (N : Node_Id);
   --  Mark declaration N as Ghost when:
   --
   --    * The declaration is subject to pragma Ghost
   --
   --    * The declaration denotes a child package or subprogram and the parent
   --      is a Ghost unit.
   --
   --    * The declaration appears within a Ghost region
   --
   --  Install the Ghost mode of the declaration. This routine starts a Ghost
   --  region and must be used with routine Restore_Ghost_Region.

   procedure Mark_And_Set_Ghost_Instantiation
     (N      : Node_Id;
      Gen_Id : Entity_Id);
   --  Mark instantiation N as Ghost when:
   --
   --    * The instantiation is subject to pragma Ghost
   --
   --    * The generic template denoted by Gen_Id is Ghost
   --
   --    * The instantiation appears within a Ghost region
   --
   --  Install the Ghost mode of the instantiation. This routine starts a Ghost
   --  region and must be used with routine Restore_Ghost_Region.

   procedure Mark_And_Set_Ghost_Procedure_Call (N : Node_Id);
   --  Mark procedure call N as Ghost when:
   --
   --    * The procedure being invoked is a Ghost entity
   --
   --  Install the Ghost mode of the procedure call. This routine starts a
   --  Ghost region and must be used with routine Restore_Ghost_Region.

   procedure Mark_Ghost_Clause (N : Node_Id);
   --  Mark use package, use type, or with clause N as Ghost when:
   --
   --    * The clause mentions a Ghost entity

   procedure Mark_Ghost_Pragma
     (N  : Node_Id;
      Id : Entity_Id);
   --  Mark pragma N as Ghost when:
   --
   --    * The pragma encloses Ghost entity Id
   --
   --    * The pragma is associated with Ghost entity Id

   procedure Mark_Ghost_Renaming
     (N  : Node_Id;
      Id : Entity_Id);
   --  Mark renaming declaration N as Ghost when:
   --
   --    * Renamed entity Id denotes a Ghost entity

   procedure Remove_Ignored_Ghost_Code;
   --  Remove all code marked as ignored Ghost from the trees of all qualifying
   --  units (SPARK RM 6.9(4)).
   --
   --  WARNING: this is a separate front end pass, care should be taken to keep
   --  it optimized.

   procedure Restore_Ghost_Region (Mode : Ghost_Mode_Type; N : Node_Id);
   pragma Inline (Restore_Ghost_Region);
   --  Restore a Ghost region to a previous state described by mode Mode and
   --  ignored region start node N. This routine must be used in conjunction
   --  with the following routines:
   --
   --    Install_Ghost_Region
   --    Mark_And_Set_xxx
   --    Set_Ghost_Mode

   procedure Set_Ghost_Mode (N : Node_Or_Entity_Id);
   --  Install the Ghost mode of arbitrary node N. This routine starts a Ghost
   --  region and must be used with routine Restore_Ghost_Region.

   procedure Set_Is_Ghost_Entity (Id : Entity_Id);
   --  Set the relevant Ghost attributes of entity Id depending on the current
   --  Ghost assertion policy in effect.

end Ghost;
