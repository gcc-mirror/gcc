------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  Expand routines for chapter 3 constructs

with Types;  use Types;
with Elists; use Elists;
with Uintp;  use Uintp;

package Exp_Ch3 is

   procedure Expand_N_Object_Declaration         (N : Node_Id);
   procedure Expand_N_Subtype_Indication         (N : Node_Id);
   procedure Expand_N_Variant_Part               (N : Node_Id);
   procedure Expand_N_Full_Type_Declaration      (N : Node_Id);

   procedure Expand_Previous_Access_Type (Def_Id : Entity_Id);
   --  For a full type declaration that contains tasks, or that is a task,
   --  check whether there exists an access type whose designated type is an
   --  incomplete declarations for the current composite type. If so, build the
   --  master for that access type, now that it is known to denote an object
   --  with tasks.

   procedure Expand_Record_Extension (T : Entity_Id; Def : Node_Id);
   --  Add a field _parent in the extension part of the record

   procedure Build_Discr_Checking_Funcs (N : Node_Id);
   --  Builds function which checks whether the component name is consistent
   --  with the current discriminants. N is the full type declaration node,
   --  and the discriminant checking functions are inserted after this node.

   function Build_Initialization_Call
     (Loc               : Source_Ptr;
      Id_Ref            : Node_Id;
      Typ               : Entity_Id;
      In_Init_Proc      : Boolean := False;
      Enclos_Type       : Entity_Id := Empty;
      Discr_Map         : Elist_Id := New_Elmt_List;
      With_Default_Init : Boolean := False;
      Constructor_Ref   : Node_Id := Empty) return List_Id;
   --  Builds a call to the initialization procedure for the base type of Typ,
   --  passing it the object denoted by Id_Ref, plus additional parameters as
   --  appropriate for the type (the _Master, for task types, for example).
   --  Loc is the source location for the constructed tree. In_Init_Proc has
   --  to be set to True when the call is itself in an init proc in order to
   --  enable the use of discriminals. Enclos_Type is the enclosing type when
   --  initializing a component in an outer init proc, and it is used for
   --  various expansion cases including the case where Typ is a task type
   --  which is an array component, the indexes of the enclosing type are
   --  used to build the string that identifies each task at runtime.
   --
   --  Discr_Map is used to replace discriminants by their discriminals in
   --  expressions used to constrain record components. In the presence of
   --  entry families bounded by discriminants, protected type discriminants
   --  can appear within expressions in array bounds (not as stand-alone
   --  identifiers) and a general replacement is necessary.
   --
   --  Ada 2005 (AI-287): With_Default_Init is used to indicate that the
   --  initialization call corresponds to a default initialized component
   --  of an aggregate.
   --
   --  Constructor_Ref is a call to a constructor subprogram. It is currently
   --  used only to support C++ constructors.

   function Build_Variant_Record_Equality
     (Typ         : Entity_Id;
      Body_Id     : Entity_Id;
      Param_Specs : List_Id) return Node_Id;
   --  Build the body of the equality function Body_Id for the untagged variant
   --  record Typ with the given parameters specification list.

   function Freeze_Type (N : Node_Id) return Boolean;
   --  This function executes the freezing actions associated with the given
   --  freeze type node N and returns True if the node is to be deleted. We
   --  delete the node if it is present just for front end purpose and we don't
   --  want Gigi to see the node. This function can't delete the node itself
   --  since it would confuse any remaining processing of the freeze node.

   function Get_Simple_Init_Val
     (Typ  : Entity_Id;
      N    : Node_Id;
      Size : Uint := No_Uint) return Node_Id;
   --  Build an expression which represents the required initial value of type
   --  Typ for which predicate Needs_Simple_Initialization is True. N is a node
   --  whose source location used in the construction of the expression. Size
   --  is utilized as follows:
   --
   --    * If the size of the object to be initialized it is known, it should
   --      be passed to the routine.
   --
   --    * If the size is unknown or is zero, then the Esize of Typ is used as
   --      an estimate of the size.
   --
   --  The object size is needed to prepare a known invalid value for use by
   --  Normalize_Scalars. A call to this routine where Typ denotes a scalar
   --  type is only valid when Normalize_Scalars or Initialize_Scalars is
   --  active, or if N is the node for a 'Invalid_Value attribute node.

   procedure Init_Secondary_Tags
     (Typ            : Entity_Id;
      Target         : Node_Id;
      Init_Tags_List : List_Id;
      Stmts_List     : List_Id;
      Fixed_Comps    : Boolean := True;
      Variable_Comps : Boolean := True);
   --  Ada 2005 (AI-251): Initialize the tags of the secondary dispatch tables
   --  of Typ. The generated code referencing tag fields of Target is appended
   --  to Init_Tags_List and the code required to complete the elaboration of
   --  the dispatch tables of Typ is appended to Stmts_List. If Fixed_Comps is
   --  True then the tag components located at fixed positions of Target are
   --  initialized; if Variable_Comps is True then tags components located at
   --  variable positions of Target are initialized.

   function Make_Tag_Assignment (N : Node_Id) return Node_Id;
   --  An object declaration that has an initialization for a tagged object
   --  requires a separate reassignment of the tag of the given type, because
   --  the expression may include an unchecked conversion. This tag assignment
   --  is inserted after the declaration, but if the object has an address
   --  clause the assignment is handled as part of the freezing of the object,
   --  see Check_Address_Clause.

end Exp_Ch3;
