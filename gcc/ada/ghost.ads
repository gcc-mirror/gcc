------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G H O S T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2014-2015, Free Software Foundation, Inc.         --
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

   function Implements_Ghost_Interface (Typ : Entity_Id) return Boolean;
   --  Determine whether type Typ implements at least one Ghost interface

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling backend

   procedure Mark_Full_View_As_Ghost
     (Priv_Typ : Entity_Id;
      Full_Typ : Entity_Id);
   --  Set all Ghost-related attributes of type Full_Typ depending on the Ghost
   --  mode of incomplete or private type Priv_Typ.

   procedure Mark_Pragma_As_Ghost
     (Prag       : Node_Id;
      Context_Id : Entity_Id);
   --  Set all Ghost-related attributes of pragma Prag if its context denoted
   --  by Id is a Ghost entity.

   procedure Mark_Renaming_As_Ghost
     (Ren_Decl : Node_Id;
      Nam_Id   : Entity_Id);
   --  Set all Ghost-related attributes of renaming declaration Ren_Decl if its
   --  renamed name denoted by Nam_Id is a Ghost entity.

   procedure Remove_Ignored_Ghost_Code;
   --  Remove all code marked as ignored Ghost from the trees of all qualifying
   --  units (SPARK RM 6.9(4)).
   --
   --  WARNING: this is a separate front end pass, care should be taken to keep
   --  it optimized.

   procedure Set_Ghost_Mode (N : Node_Id; Id : Entity_Id := Empty);
   --  Set the value of global variable Ghost_Mode depending on the following
   --  scenarios:
   --
   --    If N is a declaration, determine whether N is subject to pragma Ghost.
   --    If this is the case, the Ghost_Mode is set based on the current Ghost
   --    policy in effect. Special cases:
   --
   --      N is the completion of a deferred constant, the Ghost_Mode is set
   --      based on the mode of partial declaration entity denoted by Id.
   --
   --      N is the full view of a private type, the Ghost_Mode is set based
   --      on the mode of the partial declaration entity denoted by Id.
   --
   --    If N is an assignment statement or a procedure call, the Ghost_Mode is
   --    set based on the mode of the name.
   --
   --    If N denotes a package or a subprogram body, the Ghost_Mode is set to
   --    the current Ghost policy in effect if the body is subject to Ghost or
   --    the corresponding spec denoted by Id is a Ghost entity.
   --
   --    If N is a pragma, the Ghost_Mode is set based on the mode of the
   --    pragma.
   --
   --    If N is a freeze node, the Global_Mode is set based on the mode of
   --    entity Id.
   --
   --  WARNING: the caller must save and restore the value of Ghost_Mode in a
   --  a stack-like fasion as this routine may override the existing value.

   procedure Set_Ghost_Mode_From_Entity (Id : Entity_Id);
   --  Set the valye of global variable Ghost_Mode depending on the mode of
   --  entity Id.
   --
   --  WARNING: the caller must save and restore the value of Ghost_Mode in a
   --  a stack-like fasion as this routine may override the existing value.

   procedure Set_Is_Ghost_Entity (Id : Entity_Id);
   --  Set the relevant Ghost attributes of entity Id depending on the current
   --  Ghost assertion policy in effect.

end Ghost;
