------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2014-2015, Free Software Foundation, Inc.       --
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

with Types; use Types;

package Ghost is

   procedure Add_Ignored_Ghost_Unit (Unit : Node_Id);
   --  Add a single ignored Ghost compilation unit to the internal table for
   --  post processing.

   procedure Check_Ghost_Completion
     (Partial_View : Entity_Id;
      Full_View    : Entity_Id);
   --  Verify that the Ghost policy of a full view or a completion is the same
   --  as the Ghost policy of the partial view. Emit an error if this is not
   --  the case.

   procedure Check_Ghost_Context (Ghost_Id : Entity_Id; Ghost_Ref : Node_Id);
   --  Determine whether node Ghost_Ref appears within a Ghost-friendly context
   --  where Ghost entity Ghost_Id can safely reside.

   procedure Check_Ghost_Derivation (Typ : Entity_Id);
   --  Verify that the parent type and all progenitors of derived type or type
   --  extension Typ are Ghost. If this is not the case, issue an error.

   function Implements_Ghost_Interface (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ implements at least one Ghost interface

   procedure Initialize;
   --  Initialize internal tables

   function Is_Ghost_Entity (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id is Ghost. To qualify as such, the entity
   --  must be subject to pragma Ghost.

   function Is_Subject_To_Ghost (N : Node_Id) return Boolean;
   --  Determine whether declarative node N is subject to aspect or pragma
   --  Ghost. Use this routine in cases where [source] pragma Ghost has not
   --  been analyzed yet, but the context needs to establish the "ghostness"
   --  of N.

   procedure Lock;
   --  Lock internal tables before calling backend

   procedure Remove_Ignored_Ghost_Code;
   --  Remove all code marked as ignored Ghost from the trees of all qualifying
   --  units.
   --
   --  WARNING: this is a separate front end pass, care should be taken to keep
   --  it optimized.

   procedure Set_Ghost_Mode (N : Node_Id; Prev_Id : Entity_Id := Empty);
   --  Set the value of global variable Ghost_Mode depending on the following
   --  scenarios:
   --
   --    If N is a declaration, determine whether N is subject to pragma Ghost.
   --    If this is the case, the Ghost_Mode is set based on the current Ghost
   --    policy in effect. Special cases:
   --
   --      N is the completion of a deferred constant, Prev_Id denotes the
   --      entity of the partial declaration.
   --
   --      N is the full view of a private type, Prev_Id denotes the entity
   --      of the partial declaration.
   --
   --    If N is an assignment statement or a procedure call, determine whether
   --    the name of N denotes a reference to a Ghost entity. If this is the
   --    case, the Global_Mode is set based on the mode of the name.
   --
   --    If N denotes a package or a subprogram body, determine whether the
   --    corresponding spec Prev_Id is a Ghost entity or the body is subject
   --    to pragma Ghost. If this is the case, the Global_Mode is set based on
   --    the current Ghost policy in effect.
   --
   --  WARNING: the caller must save and restore the value of Ghost_Mode in a
   --  a stack-like fasion as this routine may override the existing value.

   procedure Set_Ghost_Mode_For_Freeze (Id : Entity_Id; N : Node_Id);
   --  Set the value of global variable Ghost_Mode depending on the mode of
   --  entity Id. N denotes the context of the freeze.
   --
   --  WARNING: the caller must save and restore the value of Ghost_Mode in a
   --  a stack-like fasion as this routine may override the existing value.

   procedure Set_Is_Ghost_Entity (Id : Entity_Id);
   --  Set the relevant ghost attribute of entity Id depending on the current
   --  Ghost assertion policy in effect.

end Ghost;
