------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1992-2001 Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Expand routines for chapter 3 constructs

with Types; use Types;
with Elists; use Elists;

package Exp_Ch3 is

   procedure Expand_N_Object_Declaration         (N : Node_Id);
   procedure Expand_N_Subtype_Indication         (N : Node_Id);
   procedure Expand_N_Variant_Part               (N : Node_Id);
   procedure Expand_N_Full_Type_Declaration      (N : Node_Id);

   procedure Expand_Previous_Access_Type (N : Node_Id; Def_Id : Entity_Id);
   --  For a full type declaration that contains tasks,  or that is a task,
   --  check whether there exists an access type whose designated type is an
   --  incomplete declarations for the current composite type. If so, build
   --  the master for that access type, now that it is known to denote an
   --  object with tasks.

   procedure Expand_Derived_Record (T : Entity_Id; Def : Node_Id);
   --  Add a field _parent in the extension part of the record.

   procedure Build_Discr_Checking_Funcs (N : Node_Id);
   --  Builds function which checks whether the component name is consistent
   --  with the current discriminants. N is the full type declaration node,
   --  and the discriminant checking functions are inserted after this node.

   function Build_Initialization_Call
     (Loc          : Source_Ptr;
      Id_Ref       : Node_Id;
      Typ          : Entity_Id;
      In_Init_Proc : Boolean := False;
      Enclos_Type  : Entity_Id := Empty;
      Discr_Map    : Elist_Id := New_Elmt_List)
      return         List_Id;
   --  Builds a call to the initialization procedure of the Id entity. Id_Ref
   --  is either a new reference to Id (for record fields), or an indexed
   --  component (for array elements). Loc is the source location for the
   --  constructed tree, and Typ is the type of the entity (the initialization
   --  procedure of the base type is the procedure that actually gets called).
   --  In_Init_Proc has to be set to True when the call is itself in an Init
   --  procedure in order to enable the use of discriminals. Enclos_type is
   --  the type of the init_proc and it is used for various expansion cases
   --  including the case where Typ is a task type which is a array component,
   --  the indices of the enclosing type are used to build the string that
   --  identifies each task at runtime.
   --
   --  Discr_Map is used to replace discriminants by their discriminals in
   --  expressions used to constrain record components. In the presence of
   --  entry families bounded by discriminants, protected type discriminants
   --  can appear within expressions in array bounds (not as stand-alone
   --  identifiers) and a general replacement is necessary.

   procedure Freeze_Type (N : Node_Id);
   --  This procedure executes the freezing actions associated with the given
   --  freeze type node N.

   function Needs_Simple_Initialization (T : Entity_Id) return Boolean;
   --  Certain types need initialization even though there is no specific
   --  initialization routine. In this category are access types (which
   --  need initializing to null), packed array types whose implementation
   --  is a modular type, and all scalar types if Normalize_Scalars is set,
   --  as well as private types whose underlying type is present and meets
   --  any of these criteria. Finally, descendants of String and Wide_String
   --  also need initialization in Initialize/Normalize_Scalars mode.

   function Get_Simple_Init_Val
     (T    : Entity_Id;
      Loc  : Source_Ptr)
      return Node_Id;
   --  For a type which Needs_Simple_Initialization (see above), prepares
   --  the tree for an expression representing the required initial value.
   --  Loc is the source location used in constructing this tree which is
   --  returned as the result of the call.

end Exp_Ch3;
