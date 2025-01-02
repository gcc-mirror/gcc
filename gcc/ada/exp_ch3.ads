------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
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

--  Expand routines for chapter 3 constructs

with Elists;  use Elists;
with Exp_Tss; use Exp_Tss;
with Types;   use Types;
with Uintp;   use Uintp;

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

   procedure Build_Access_Subprogram_Wrapper_Body
     (Decl     : Node_Id;
      New_Decl : Node_Id);
   --  Build the wrapper body, which holds the indirect call through an access-
   --  to-subprogram, and whose expansion incorporates the contracts of the
   --  access type declaration. Called from Build_Access_Subprogram_Wrapper.
   --  Building the wrapper is done during analysis to perform proper semantic
   --  checks on the relevant aspects. The wrapper body could be simplified to
   --  a null body when expansion is disabled ???

   function Build_Default_Initialization
     (N          : Node_Id;
      Typ        : Entity_Id;
      Obj_Id     : Entity_Id;
      For_CW     : Boolean := False;
      Target_Ref : Node_Id := Empty) return List_Id;
   --  Build the code to default-initialize an object of Typ either declared
   --  or allocated by node N if this is necessary. In the former case Obj_Id
   --  is the entity for the object whereas, in the second case, Obj_Id is a
   --  temporary generated to hold the result of the allocator. For_CW is set
   --  to True in the second case if this result is of a class-wide type.

   --  Target_Ref is only passed identically to Build_Initialization_Call, so
   --  its description given for Build_Initialization_Call is also valid here.

   function Build_Default_Simple_Initialization
     (N      : Node_Id;
      Typ    : Entity_Id;
      Obj_Id : Entity_Id) return Node_Id;
   --  Try to build an expression to default-initialize an object of Typ either
   --  declared or allocated by node N if this is necessary. In the former case
   --  Obj_Id is the entity for the object whereas, in the second case, it must
   --  be set to Empty.

   procedure Build_Or_Copy_Discr_Checking_Funcs (N : Node_Id);
   --  For each variant component, builds a function that checks whether
   --  the component name is consistent with the current discriminants
   --  and sets the component's Dcheck_Function attribute to refer to it.
   --  N is the full type declaration node; the discriminant checking
   --  functions are inserted after this node.
   --  In the case of a derived untagged type, copy the attributes that were
   --  set for the components of the parent type onto the components of the
   --  derived type; no new subprograms are constructed in this case.

   function Build_Initialization_Call
     (N                   : Node_Id;
      Id_Ref              : Node_Id;
      Typ                 : Entity_Id;
      In_Init_Proc        : Boolean   := False;
      Enclos_Type         : Entity_Id := Empty;
      Target_Ref          : Node_Id   := Empty;
      Discr_Map           : Elist_Id  := New_Elmt_List;
      With_Default_Init   : Boolean   := False;
      Constructor_Ref     : Node_Id   := Empty;
      Init_Control_Actual : Entity_Id := Empty) return List_Id;
   --  Builds a call to the initialization procedure for the base type of Typ,
   --  passing it the object denoted by Id_Ref, plus additional parameters as
   --  appropriate for the type (the _Master, for task types, for example).
   --  N is the construct for which the call is to be built. In_Init_Proc has
   --  to be set to True when the call is itself in an init proc in order to
   --  enable the use of discriminals.
   --
   --  Enclos_Type is the enclosing type when initializing a component of a
   --  composite type, and is used for the case where Typ is a task type of
   --  an array component: the indices of this enclosing type are then used
   --  to build the image string that identifies each task at run time.
   --
   --  Target_Ref is also used when Typ is a task type if the initialization
   --  call is to be generated for an allocator. It is either the name of a
   --  simple assignment whose expression is the allocator, or the defining
   --  identifier of an object declaration whose initializing expression is
   --  the allocator, or else the allocator's access type. It is used both
   --  to build the image string and to pass the task master.
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
   --
   --  Init_Control_Actual is Empty except in the case where the init proc
   --  for a tagged type calls the init proc for its parent type in order
   --  to initialize its _Parent component. In that case, it is the
   --  actual parameter value corresponding to the Init_Control formal
   --  parameter to be used in the call of the parent type's init proc.

   function Build_Variant_Record_Equality
     (Typ         : Entity_Id;
      Spec_Id     : Entity_Id;
      Body_Id     : Entity_Id;
      Param_Specs : List_Id) return Node_Id;
   --  Build the body of the equality function Body_Id for the untagged variant
   --  record Typ with the given parameters specification list. If Spec_Id is
   --  present, the body is built for a renaming of the equality function.

   function Freeze_Type (N : Node_Id) return Boolean;
   --  This function executes the freezing actions associated with the given
   --  freeze type node N and returns True if the node is to be deleted. We
   --  delete the node if it is present just for front end purpose and we don't
   --  want Gigi to see the node. This function can't delete the node itself
   --  since it would confuse any remaining processing of the freeze node.
   --
   --  Note: for GNATprove we have a minimal variant of this routine in
   --  Exp_SPARK.SPARK_Freeze_Type. They need to be kept in sync.

   function Get_Simple_Init_Val
     (Typ  : Entity_Id;
      N    : Node_Id;
      Size : Uint := No_Uint) return Node_Id;
   --  Build an expression that represents the required initial value of type
   --  Typ for which predicate Needs_Simple_Initialization is True. N is a node
   --  whose source location is used in the construction of the expression.
   --  Size is used as follows:
   --
   --    * If the size of the object to be initialized it is known, it should
   --      be passed to the routine.
   --
   --    * If the size is unknown or is zero, then the Esize of Typ is used as
   --      an estimate of the size.
   --
   --  The object size is needed to prepare a known invalid value for use by
   --  Normalize_Scalars. A call to this routine where Typ denotes a scalar
   --  type is valid only when Normalize_Scalars or Initialize_Scalars is
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

   procedure Make_Controlling_Function_Wrappers
     (Tag_Typ   : Entity_Id;
      Decl_List : out List_Id;
      Body_List : out List_Id);
   --  Ada 2005 (AI-391): Makes specs and bodies for the wrapper functions
   --  associated with inherited functions with controlling results which
   --  are not overridden. The body of each wrapper function consists solely
   --  of a return statement whose expression is an extension aggregate
   --  invoking the inherited subprogram's parent subprogram and extended
   --  with a null association list.

   procedure Make_Predefined_Primitive_Eq_Spec
     (Tag_Typ     : Entity_Id;
      Predef_List : List_Id;
      Renamed_Eq  : out Entity_Id);
   --  Creates spec for the predefined equality on a tagged type Tag_Typ, if
   --  required. If created, it will be appended to Predef_List.
   --
   --  The Parameter Renamed_Eq either returns the value Empty, or else
   --  the defining unit name for the predefined equality function in the
   --  case where the type has a primitive operation that is a renaming
   --  of predefined equality (but only if there is also an overriding
   --  user-defined equality function). The returned Renamed_Eq will be
   --  passed to the corresponding parameter of Predefined_Primitive_Bodies.

   function Make_Tag_Assignment (N : Node_Id) return Node_Id;
   --  An object declaration that has an initialization for a tagged object
   --  requires a separate reassignment of the tag of the given type, because
   --  the expression may include an unchecked conversion. This tag assignment
   --  is inserted after the declaration, but if the object has an address
   --  clause the assignment is handled as part of the freezing of the object,
   --  see Check_Address_Clause.

   procedure Predefined_Primitive_Eq_Body
     (Tag_Typ     : Entity_Id;
      Predef_List : List_Id;
      Renamed_Eq  : Entity_Id);
   --  Creates body for the predefined equality (and ineqality, if required) on
   --  a tagged type Tag_Typ. If created they will be appended to Predef_List.
   --
   --  The spec for the equality function has been created by
   --  Make_Predefined_Primitive_Eq_Spec; see there for description of
   --  the Renamed_Eq parameter.

   function Stream_Operation_OK
     (Typ       : Entity_Id;
      Operation : TSS_Name_Type) return Boolean;
   --  Check whether the named stream operation must be emitted for a given
   --  type. The rules for inheritance of stream attributes by type extensions
   --  are enforced by this function. Furthermore, various restrictions prevent
   --  the generation of these operations, as a useful optimization or for
   --  certification purposes and to save unnecessary generated code.

end Exp_Ch3;
