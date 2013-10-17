------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Aggr; use Exp_Aggr;
with Exp_Atag; use Exp_Atag;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Disp; use Exp_Disp;
with Exp_Dist; use Exp_Dist;
with Exp_Smem; use Exp_Smem;
with Exp_Strm; use Exp_Strm;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Attr; use Sem_Attr;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_SCIL; use Sem_SCIL;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Snames;   use Snames;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Validsw;  use Validsw;

package body Exp_Ch3 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Adjust_Discriminants (Rtype : Entity_Id);
   --  This is used when freezing a record type. It attempts to construct
   --  more restrictive subtypes for discriminants so that the max size of
   --  the record can be calculated more accurately. See the body of this
   --  procedure for details.

   procedure Build_Array_Init_Proc (A_Type : Entity_Id; Nod : Node_Id);
   --  Build initialization procedure for given array type. Nod is a node
   --  used for attachment of any actions required in its construction.
   --  It also supplies the source location used for the procedure.

   function Build_Array_Invariant_Proc
     (A_Type : Entity_Id;
      Nod    : Node_Id) return Node_Id;
   --  If the component of type of array type has invariants, build procedure
   --  that checks invariant on all components of the array. Ada 2012 specifies
   --  that an invariant on some type T must be applied to in-out parameters
   --  and return values that include a part of type T. If the array type has
   --  an otherwise specified invariant, the component check procedure is
   --  called from within the user-specified invariant. Otherwise this becomes
   --  the invariant procedure for the array type.

   function Build_Record_Invariant_Proc
     (R_Type : Entity_Id;
      Nod    : Node_Id) return Node_Id;
   --  Ditto for record types.

   function Build_Discriminant_Formals
     (Rec_Id : Entity_Id;
      Use_Dl : Boolean) return List_Id;
   --  This function uses the discriminants of a type to build a list of
   --  formal parameters, used in Build_Init_Procedure among other places.
   --  If the flag Use_Dl is set, the list is built using the already
   --  defined discriminals of the type, as is the case for concurrent
   --  types with discriminants. Otherwise new identifiers are created,
   --  with the source names of the discriminants.

   function Build_Equivalent_Array_Aggregate (T : Entity_Id) return Node_Id;
   --  This function builds a static aggregate that can serve as the initial
   --  value for an array type whose bounds are static, and whose component
   --  type is a composite type that has a static equivalent aggregate.
   --  The equivalent array aggregate is used both for object initialization
   --  and for component initialization, when used in the following function.

   function Build_Equivalent_Record_Aggregate (T : Entity_Id) return Node_Id;
   --  This function builds a static aggregate that can serve as the initial
   --  value for a record type whose components are scalar and initialized
   --  with compile-time values, or arrays with similar initialization or
   --  defaults. When possible, initialization of an object of the type can
   --  be achieved by using a copy of the aggregate as an initial value, thus
   --  removing the implicit call that would otherwise constitute elaboration
   --  code.

   procedure Build_Record_Init_Proc (N : Node_Id; Rec_Ent : Entity_Id);
   --  Build record initialization procedure. N is the type declaration
   --  node, and Rec_Ent is the corresponding entity for the record type.

   procedure Build_Slice_Assignment (Typ : Entity_Id);
   --  Build assignment procedure for one-dimensional arrays of controlled
   --  types. Other array and slice assignments are expanded in-line, but
   --  the code expansion for controlled components (when control actions
   --  are active) can lead to very large blocks that GCC3 handles poorly.

   procedure Build_Untagged_Equality (Typ : Entity_Id);
   --  AI05-0123: Equality on untagged records composes. This procedure
   --  builds the equality routine for an untagged record that has components
   --  of a record type that has user-defined primitive equality operations.
   --  The resulting operation is a TSS subprogram.

   procedure Build_Variant_Record_Equality (Typ  : Entity_Id);
   --  Create An Equality function for the non-tagged variant record 'Typ'
   --  and attach it to the TSS list

   procedure Check_Stream_Attributes (Typ : Entity_Id);
   --  Check that if a limited extension has a parent with user-defined stream
   --  attributes, and does not itself have user-defined stream-attributes,
   --  then any limited component of the extension also has the corresponding
   --  user-defined stream attributes.

   procedure Clean_Task_Names
     (Typ     : Entity_Id;
      Proc_Id : Entity_Id);
   --  If an initialization procedure includes calls to generate names
   --  for task subcomponents, indicate that secondary stack cleanup is
   --  needed after an initialization. Typ is the component type, and Proc_Id
   --  the initialization procedure for the enclosing composite type.

   procedure Expand_Tagged_Root (T : Entity_Id);
   --  Add a field _Tag at the beginning of the record. This field carries
   --  the value of the access to the Dispatch table. This procedure is only
   --  called on root type, the _Tag field being inherited by the descendants.

   procedure Expand_Freeze_Array_Type (N : Node_Id);
   --  Freeze an array type. Deals with building the initialization procedure,
   --  creating the packed array type for a packed array and also with the
   --  creation of the controlling procedures for the controlled case. The
   --  argument N is the N_Freeze_Entity node for the type.

   procedure Expand_Freeze_Class_Wide_Type (N : Node_Id);
   --  Freeze a class-wide type. Build routine Finalize_Address for the purpose
   --  of finalizing controlled derivations from the class-wide's root type.

   procedure Expand_Freeze_Enumeration_Type (N : Node_Id);
   --  Freeze enumeration type with non-standard representation. Builds the
   --  array and function needed to convert between enumeration pos and
   --  enumeration representation values. N is the N_Freeze_Entity node
   --  for the type.

   procedure Expand_Freeze_Record_Type (N : Node_Id);
   --  Freeze record type. Builds all necessary discriminant checking
   --  and other ancillary functions, and builds dispatch tables where
   --  needed. The argument N is the N_Freeze_Entity node. This processing
   --  applies only to E_Record_Type entities, not to class wide types,
   --  record subtypes, or private types.

   procedure Freeze_Stream_Operations (N : Node_Id; Typ : Entity_Id);
   --  Treat user-defined stream operations as renaming_as_body if the
   --  subprogram they rename is not frozen when the type is frozen.

   procedure Insert_Component_Invariant_Checks
     (N   : Node_Id;
     Typ  : Entity_Id;
     Proc : Node_Id);
   --  If a composite type has invariants and also has components with defined
   --  invariants. the component invariant procedure is inserted into the user-
   --  defined invariant procedure and added to the checks to be performed.

   procedure Initialization_Warning (E : Entity_Id);
   --  If static elaboration of the package is requested, indicate
   --  when a type does meet the conditions for static initialization. If
   --  E is a type, it has components that have no static initialization.
   --  if E is an entity, its initial expression is not compile-time known.

   function Init_Formals (Typ : Entity_Id) return List_Id;
   --  This function builds the list of formals for an initialization routine.
   --  The first formal is always _Init with the given type. For task value
   --  record types and types containing tasks, three additional formals are
   --  added:
   --
   --    _Master    : Master_Id
   --    _Chain     : in out Activation_Chain
   --    _Task_Name : String
   --
   --  The caller must append additional entries for discriminants if required.

   function In_Runtime (E : Entity_Id) return Boolean;
   --  Check if E is defined in the RTL (in a child of Ada or System). Used
   --  to avoid to bring in the overhead of _Input, _Output for tagged types.

   function Is_User_Defined_Equality (Prim : Node_Id) return Boolean;
   --  Returns true if Prim is a user defined equality function

   function Make_Eq_Body
     (Typ     : Entity_Id;
      Eq_Name : Name_Id) return Node_Id;
   --  Build the body of a primitive equality operation for a tagged record
   --  type, or in Ada 2012 for any record type that has components with a
   --  user-defined equality. Factored out of Predefined_Primitive_Bodies.

   function Make_Eq_Case
     (E      : Entity_Id;
      CL     : Node_Id;
      Discrs : Elist_Id := New_Elmt_List) return List_Id;
   --  Building block for variant record equality. Defined to share the code
   --  between the tagged and non-tagged case. Given a Component_List node CL,
   --  it generates an 'if' followed by a 'case' statement that compares all
   --  components of local temporaries named X and Y (that are declared as
   --  formals at some upper level). E provides the Sloc to be used for the
   --  generated code.
   --
   --  IF E is an unchecked_union,  Discrs is the list of formals created for
   --  the inferred discriminants of one operand. These formals are used in
   --  the generated case statements for each variant of the unchecked union.

   function Make_Eq_If
     (E : Entity_Id;
      L : List_Id) return Node_Id;
   --  Building block for variant record equality. Defined to share the code
   --  between the tagged and non-tagged case. Given the list of components
   --  (or discriminants) L, it generates a return statement that compares all
   --  components of local temporaries named X and Y (that are declared as
   --  formals at some upper level). E provides the Sloc to be used for the
   --  generated code.

   function Make_Neq_Body (Tag_Typ : Entity_Id) return Node_Id;
   --  Search for a renaming of the inequality dispatching primitive of
   --  this tagged type. If found then build and return the corresponding
   --  rename-as-body inequality subprogram; otherwise return Empty.

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Entity_Id);
   --  Create a list with the specs of the predefined primitive operations.
   --  For tagged types that are interfaces all these primitives are defined
   --  abstract.
   --
   --  The following entries are present for all tagged types, and provide
   --  the results of the corresponding attribute applied to the object.
   --  Dispatching is required in general, since the result of the attribute
   --  will vary with the actual object subtype.
   --
   --     _size          provides result of 'Size attribute
   --     typSR          provides result of 'Read attribute
   --     typSW          provides result of 'Write attribute
   --     typSI          provides result of 'Input attribute
   --     typSO          provides result of 'Output attribute
   --
   --  The following entries are additionally present for non-limited tagged
   --  types, and implement additional dispatching operations for predefined
   --  operations:
   --
   --     _equality      implements "=" operator
   --     _assign        implements assignment operation
   --     typDF          implements deep finalization
   --     typDA          implements deep adjust
   --
   --  The latter two are empty procedures unless the type contains some
   --  controlled components that require finalization actions (the deep
   --  in the name refers to the fact that the action applies to components).
   --
   --  The list is returned in Predef_List. The Parameter Renamed_Eq either
   --  returns the value Empty, or else the defining unit name for the
   --  predefined equality function in the case where the type has a primitive
   --  operation that is a renaming of predefined equality (but only if there
   --  is also an overriding user-defined equality function). The returned
   --  Renamed_Eq will be passed to the corresponding parameter of
   --  Predefined_Primitive_Bodies.

   function Has_New_Non_Standard_Rep (T : Entity_Id) return Boolean;
   --  returns True if there are representation clauses for type T that are not
   --  inherited. If the result is false, the init_proc and the discriminant
   --  checking functions of the parent can be reused by a derived type.

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

   function Make_Null_Procedure_Specs (Tag_Typ : Entity_Id) return List_Id;
   --  Ada 2005 (AI-251): Makes specs for null procedures associated with any
   --  null procedures inherited from an interface type that have not been
   --  overridden. Only one null procedure will be created for a given set of
   --  inherited null procedures with homographic profiles.

   function Predef_Spec_Or_Body
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : Name_Id;
      Profile  : List_Id;
      Ret_Type : Entity_Id := Empty;
      For_Body : Boolean   := False) return Node_Id;
   --  This function generates the appropriate expansion for a predefined
   --  primitive operation specified by its name, parameter profile and
   --  return type (Empty means this is a procedure). If For_Body is false,
   --  then the returned node is a subprogram declaration. If For_Body is
   --  true, then the returned node is a empty subprogram body containing
   --  no declarations and no statements.

   function Predef_Stream_Attr_Spec
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : TSS_Name_Type;
      For_Body : Boolean := False) return Node_Id;
   --  Specialized version of Predef_Spec_Or_Body that apply to read, write,
   --  input and output attribute whose specs are constructed in Exp_Strm.

   function Predef_Deep_Spec
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : TSS_Name_Type;
      For_Body : Boolean := False) return Node_Id;
   --  Specialized version of Predef_Spec_Or_Body that apply to _deep_adjust
   --  and _deep_finalize

   function Predefined_Primitive_Bodies
     (Tag_Typ    : Entity_Id;
      Renamed_Eq : Entity_Id) return List_Id;
   --  Create the bodies of the predefined primitives that are described in
   --  Predefined_Primitive_Specs. When not empty, Renamed_Eq must denote
   --  the defining unit name of the type's predefined equality as returned
   --  by Make_Predefined_Primitive_Specs.

   function Predefined_Primitive_Freeze (Tag_Typ : Entity_Id) return List_Id;
   --  Freeze entities of all predefined primitive operations. This is needed
   --  because the bodies of these operations do not normally do any freezing.

   function Stream_Operation_OK
     (Typ       : Entity_Id;
      Operation : TSS_Name_Type) return Boolean;
   --  Check whether the named stream operation must be emitted for a given
   --  type. The rules for inheritance of stream attributes by type extensions
   --  are enforced by this function. Furthermore, various restrictions prevent
   --  the generation of these operations, as a useful optimization or for
   --  certification purposes.

   --------------------------
   -- Adjust_Discriminants --
   --------------------------

   --  This procedure attempts to define subtypes for discriminants that are
   --  more restrictive than those declared. Such a replacement is possible if
   --  we can demonstrate that values outside the restricted range would cause
   --  constraint errors in any case. The advantage of restricting the
   --  discriminant types in this way is that the maximum size of the variant
   --  record can be calculated more conservatively.

   --  An example of a situation in which we can perform this type of
   --  restriction is the following:

   --    subtype B is range 1 .. 10;
   --    type Q is array (B range <>) of Integer;

   --    type V (N : Natural) is record
   --       C : Q (1 .. N);
   --    end record;

   --  In this situation, we can restrict the upper bound of N to 10, since
   --  any larger value would cause a constraint error in any case.

   --  There are many situations in which such restriction is possible, but
   --  for now, we just look for cases like the above, where the component
   --  in question is a one dimensional array whose upper bound is one of
   --  the record discriminants. Also the component must not be part of
   --  any variant part, since then the component does not always exist.

   procedure Adjust_Discriminants (Rtype : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (Rtype);
      Comp  : Entity_Id;
      Ctyp  : Entity_Id;
      Ityp  : Entity_Id;
      Lo    : Node_Id;
      Hi    : Node_Id;
      P     : Node_Id;
      Loval : Uint;
      Discr : Entity_Id;
      Dtyp  : Entity_Id;
      Dhi   : Node_Id;
      Dhiv  : Uint;
      Ahi   : Node_Id;
      Ahiv  : Uint;
      Tnn   : Entity_Id;

   begin
      Comp := First_Component (Rtype);
      while Present (Comp) loop

         --  If our parent is a variant, quit, we do not look at components
         --  that are in variant parts, because they may not always exist.

         P := Parent (Comp);   -- component declaration
         P := Parent (P);      -- component list

         exit when Nkind (Parent (P)) = N_Variant;

         --  We are looking for a one dimensional array type

         Ctyp := Etype (Comp);

         if not Is_Array_Type (Ctyp)
           or else Number_Dimensions (Ctyp) > 1
         then
            goto Continue;
         end if;

         --  The lower bound must be constant, and the upper bound is a
         --  discriminant (which is a discriminant of the current record).

         Ityp := Etype (First_Index (Ctyp));
         Lo := Type_Low_Bound (Ityp);
         Hi := Type_High_Bound (Ityp);

         if not Compile_Time_Known_Value (Lo)
           or else Nkind (Hi) /= N_Identifier
           or else No (Entity (Hi))
           or else Ekind (Entity (Hi)) /= E_Discriminant
         then
            goto Continue;
         end if;

         --  We have an array with appropriate bounds

         Loval := Expr_Value (Lo);
         Discr := Entity (Hi);
         Dtyp  := Etype (Discr);

         --  See if the discriminant has a known upper bound

         Dhi := Type_High_Bound (Dtyp);

         if not Compile_Time_Known_Value (Dhi) then
            goto Continue;
         end if;

         Dhiv := Expr_Value (Dhi);

         --  See if base type of component array has known upper bound

         Ahi := Type_High_Bound (Etype (First_Index (Base_Type (Ctyp))));

         if not Compile_Time_Known_Value (Ahi) then
            goto Continue;
         end if;

         Ahiv := Expr_Value (Ahi);

         --  The condition for doing the restriction is that the high bound
         --  of the discriminant is greater than the low bound of the array,
         --  and is also greater than the high bound of the base type index.

         if Dhiv > Loval and then Dhiv > Ahiv then

            --  We can reset the upper bound of the discriminant type to
            --  whichever is larger, the low bound of the component, or
            --  the high bound of the base type array index.

            --  We build a subtype that is declared as

            --     subtype Tnn is discr_type range discr_type'First .. max;

            --  And insert this declaration into the tree. The type of the
            --  discriminant is then reset to this more restricted subtype.

            Tnn := Make_Temporary (Loc, 'T');

            Insert_Action (Declaration_Node (Rtype),
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Tnn,
                Subtype_Indication =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Occurrence_Of (Dtyp, Loc),
                    Constraint   =>
                      Make_Range_Constraint (Loc,
                        Range_Expression =>
                          Make_Range (Loc,
                            Low_Bound =>
                              Make_Attribute_Reference (Loc,
                                Attribute_Name => Name_First,
                                Prefix => New_Occurrence_Of (Dtyp, Loc)),
                            High_Bound =>
                              Make_Integer_Literal (Loc,
                                Intval => UI_Max (Loval, Ahiv)))))));

            Set_Etype (Discr, Tnn);
         end if;

      <<Continue>>
         Next_Component (Comp);
      end loop;
   end Adjust_Discriminants;

   ---------------------------
   -- Build_Array_Init_Proc --
   ---------------------------

   procedure Build_Array_Init_Proc (A_Type : Entity_Id; Nod : Node_Id) is
      Comp_Type        : constant Entity_Id  := Component_Type (A_Type);
      Body_Stmts       : List_Id;
      Has_Default_Init : Boolean;
      Index_List       : List_Id;
      Loc              : Source_Ptr;
      Proc_Id          : Entity_Id;

      function Init_Component return List_Id;
      --  Create one statement to initialize one array component, designated
      --  by a full set of indexes.

      function Init_One_Dimension (N : Int) return List_Id;
      --  Create loop to initialize one dimension of the array. The single
      --  statement in the loop body initializes the inner dimensions if any,
      --  or else the single component. Note that this procedure is called
      --  recursively, with N being the dimension to be initialized. A call
      --  with N greater than the number of dimensions simply generates the
      --  component initialization, terminating the recursion.

      --------------------
      -- Init_Component --
      --------------------

      function Init_Component return List_Id is
         Comp : Node_Id;

      begin
         Comp :=
           Make_Indexed_Component (Loc,
             Prefix      => Make_Identifier (Loc, Name_uInit),
             Expressions => Index_List);

         if Has_Default_Aspect (A_Type) then
            Set_Assignment_OK (Comp);
            return New_List (
              Make_Assignment_Statement (Loc,
                Name       => Comp,
                Expression =>
                  Convert_To (Comp_Type,
                    Default_Aspect_Component_Value (First_Subtype (A_Type)))));

         elsif Needs_Simple_Initialization (Comp_Type) then
            Set_Assignment_OK (Comp);
            return New_List (
              Make_Assignment_Statement (Loc,
                Name       => Comp,
                Expression =>
                  Get_Simple_Init_Val
                    (Comp_Type, Nod, Component_Size (A_Type))));

         else
            Clean_Task_Names (Comp_Type, Proc_Id);
            return
              Build_Initialization_Call
                (Loc, Comp, Comp_Type,
                 In_Init_Proc => True,
                 Enclos_Type  => A_Type);
         end if;
      end Init_Component;

      ------------------------
      -- Init_One_Dimension --
      ------------------------

      function Init_One_Dimension (N : Int) return List_Id is
         Index : Entity_Id;

      begin
         --  If the component does not need initializing, then there is nothing
         --  to do here, so we return a null body. This occurs when generating
         --  the dummy Init_Proc needed for Initialize_Scalars processing.

         if not Has_Non_Null_Base_Init_Proc (Comp_Type)
           and then not Needs_Simple_Initialization (Comp_Type)
           and then not Has_Task (Comp_Type)
           and then not Has_Default_Aspect (A_Type)
         then
            return New_List (Make_Null_Statement (Loc));

         --  If all dimensions dealt with, we simply initialize the component

         elsif N > Number_Dimensions (A_Type) then
            return Init_Component;

         --  Here we generate the required loop

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append (New_Reference_To (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (Nod,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uInit),
                            Attribute_Name  => Name_Range,
                            Expressions     => New_List (
                              Make_Integer_Literal (Loc, N))))),
                Statements =>  Init_One_Dimension (N + 1)));
         end if;
      end Init_One_Dimension;

   --  Start of processing for Build_Array_Init_Proc

   begin
      --  The init proc is created when analyzing the freeze node for the type,
      --  but it properly belongs with the array type declaration. However, if
      --  the freeze node is for a subtype of a type declared in another unit
      --  it seems preferable to use the freeze node as the source location of
      --  the init proc. In any case this is preferable for gcov usage, and
      --  the Sloc is not otherwise used by the compiler.

      if In_Open_Scopes (Scope (A_Type)) then
         Loc := Sloc (A_Type);
      else
         Loc := Sloc (Nod);
      end if;

      --  Nothing to generate in the following cases:

      --    1. Initialization is suppressed for the type
      --    2. The type is a value type, in the CIL sense.
      --    3. The type has CIL/JVM convention.
      --    4. An initialization already exists for the base type

      if Initialization_Suppressed (A_Type)
        or else Is_Value_Type (Comp_Type)
        or else Convention (A_Type) = Convention_CIL
        or else Convention (A_Type) = Convention_Java
        or else Present (Base_Init_Proc (A_Type))
      then
         return;
      end if;

      Index_List := New_List;

      --  We need an initialization procedure if any of the following is true:

      --    1. The component type has an initialization procedure
      --    2. The component type needs simple initialization
      --    3. Tasks are present
      --    4. The type is marked as a public entity
      --    5. The array type has a Default_Component_Value aspect

      --  The reason for the public entity test is to deal properly with the
      --  Initialize_Scalars pragma. This pragma can be set in the client and
      --  not in the declaring package, this means the client will make a call
      --  to the initialization procedure (because one of conditions 1-3 must
      --  apply in this case), and we must generate a procedure (even if it is
      --  null) to satisfy the call in this case.

      --  Exception: do not build an array init_proc for a type whose root
      --  type is Standard.String or Standard.Wide_[Wide_]String, since there
      --  is no place to put the code, and in any case we handle initialization
      --  of such types (in the Initialize_Scalars case, that's the only time
      --  the issue arises) in a special manner anyway which does not need an
      --  init_proc.

      Has_Default_Init := Has_Non_Null_Base_Init_Proc (Comp_Type)
                            or else Needs_Simple_Initialization (Comp_Type)
                            or else Has_Task (Comp_Type)
                            or else Has_Default_Aspect (A_Type);

      if Has_Default_Init
        or else (not Restriction_Active (No_Initialize_Scalars)
                  and then Is_Public (A_Type)
                  and then Root_Type (A_Type) /= Standard_String
                  and then Root_Type (A_Type) /= Standard_Wide_String
                  and then Root_Type (A_Type) /= Standard_Wide_Wide_String)
      then
         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Make_Init_Proc_Name (A_Type));

         --  If No_Default_Initialization restriction is active, then we don't
         --  want to build an init_proc, but we need to mark that an init_proc
         --  would be needed if this restriction was not active (so that we can
         --  detect attempts to call it), so set a dummy init_proc in place.
         --  This is only done though when actual default initialization is
         --  needed (and not done when only Is_Public is True), since otherwise
         --  objects such as arrays of scalars could be wrongly flagged as
         --  violating the restriction.

         if Restriction_Active (No_Default_Initialization) then
            if Has_Default_Init then
               Set_Init_Proc (A_Type, Proc_Id);
            end if;

            return;
         end if;

         Body_Stmts := Init_One_Dimension (1);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Proc_Id,
                 Parameter_Specifications => Init_Formals (A_Type)),
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Body_Stmts)));

         Set_Ekind          (Proc_Id, E_Procedure);
         Set_Is_Public      (Proc_Id, Is_Public (A_Type));
         Set_Is_Internal    (Proc_Id);
         Set_Has_Completion (Proc_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         --  Set inlined unless controlled stuff or tasks around, in which
         --  case we do not want to inline, because nested stuff may cause
         --  difficulties in inter-unit inlining, and furthermore there is
         --  in any case no point in inlining such complex init procs.

         if not Has_Task (Proc_Id)
           and then not Needs_Finalization (Proc_Id)
         then
            Set_Is_Inlined (Proc_Id);
         end if;

         --  Associate Init_Proc with type, and determine if the procedure
         --  is null (happens because of the Initialize_Scalars pragma case,
         --  where we have to generate a null procedure in case it is called
         --  by a client with Initialize_Scalars set). Such procedures have
         --  to be generated, but do not have to be called, so we mark them
         --  as null to suppress the call.

         Set_Init_Proc (A_Type, Proc_Id);

         if List_Length (Body_Stmts) = 1

           --  We must skip SCIL nodes because they may have been added to this
           --  list by Insert_Actions.

           and then Nkind (First_Non_SCIL_Node (Body_Stmts)) = N_Null_Statement
         then
            Set_Is_Null_Init_Proc (Proc_Id);

         else
            --  Try to build a static aggregate to statically initialize
            --  objects of the type. This can only be done for constrained
            --  one-dimensional arrays with static bounds.

            Set_Static_Initialization
              (Proc_Id,
               Build_Equivalent_Array_Aggregate (First_Subtype (A_Type)));
         end if;
      end if;
   end Build_Array_Init_Proc;

   --------------------------------
   -- Build_Array_Invariant_Proc --
   --------------------------------

   function Build_Array_Invariant_Proc
     (A_Type : Entity_Id;
      Nod    : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Object_Name : constant Name_Id := New_Internal_Name ('I');
      --  Name for argument of invariant procedure

      Object_Entity : constant Node_Id :=
                        Make_Defining_Identifier (Loc, Object_Name);
      --  The procedure declaration entity for the argument

      Body_Stmts : List_Id;
      Index_List : List_Id;
      Proc_Id    : Entity_Id;
      Proc_Body  : Node_Id;

      function Build_Component_Invariant_Call return Node_Id;
      --  Create one statement to verify invariant on one array component,
      --  designated by a full set of indexes.

      function Check_One_Dimension (N : Int) return List_Id;
      --  Create loop to check on one dimension of the array. The single
      --  statement in the loop body checks the inner dimensions if any, or
      --  else a single component. This procedure is called recursively, with
      --  N being the dimension to be initialized. A call with N greater than
      --  the number of dimensions generates the component initialization
      --  and terminates the recursion.

      ------------------------------------
      -- Build_Component_Invariant_Call --
      ------------------------------------

      function Build_Component_Invariant_Call return Node_Id is
         Comp : Node_Id;
      begin
         Comp :=
           Make_Indexed_Component (Loc,
             Prefix      => New_Occurrence_Of (Object_Entity, Loc),
             Expressions => Index_List);
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of
                 (Invariant_Procedure (Component_Type (A_Type)), Loc),
             Parameter_Associations => New_List (Comp));
      end Build_Component_Invariant_Call;

      -------------------------
      -- Check_One_Dimension --
      -------------------------

      function Check_One_Dimension (N : Int) return List_Id is
         Index : Entity_Id;

      begin
         --  If all dimensions dealt with, we simply check invariant of the
         --  component.

         if N > Number_Dimensions (A_Type) then
            return New_List (Build_Component_Invariant_Call);

         --  Else generate one loop and recurse

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append (New_Reference_To (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (Nod,
                Identifier       => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier         => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix          =>
                              New_Occurrence_Of (Object_Entity, Loc),
                            Attribute_Name  => Name_Range,
                            Expressions     => New_List (
                              Make_Integer_Literal (Loc, N))))),
                Statements       =>  Check_One_Dimension (N + 1)));
         end if;
      end Check_One_Dimension;

   --  Start of processing for Build_Array_Invariant_Proc

   begin
      Index_List := New_List;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
           Chars => New_External_Name (Chars (A_Type), "CInvariant"));

      Body_Stmts := Check_One_Dimension (1);

      Proc_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Id,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => Object_Entity,
                  Parameter_Type      => New_Occurrence_Of (A_Type, Loc)))),

          Declarations               => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Body_Stmts));

      Set_Ekind          (Proc_Id, E_Procedure);
      Set_Is_Public      (Proc_Id, Is_Public (A_Type));
      Set_Is_Internal    (Proc_Id);
      Set_Has_Completion (Proc_Id);

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Proc_Id);
      end if;

      return Proc_Body;
   end Build_Array_Invariant_Proc;

   --------------------------------
   -- Build_Discr_Checking_Funcs --
   --------------------------------

   procedure Build_Discr_Checking_Funcs (N : Node_Id) is
      Rec_Id            : Entity_Id;
      Loc               : Source_Ptr;
      Enclosing_Func_Id : Entity_Id;
      Sequence          : Nat     := 1;
      Type_Def          : Node_Id;
      V                 : Node_Id;

      function Build_Case_Statement
        (Case_Id : Entity_Id;
         Variant : Node_Id) return Node_Id;
      --  Build a case statement containing only two alternatives. The first
      --  alternative corresponds exactly to the discrete choices given on the
      --  variant with contains the components that we are generating the
      --  checks for. If the discriminant is one of these return False. The
      --  second alternative is an OTHERS choice that will return True
      --  indicating the discriminant did not match.

      function Build_Dcheck_Function
        (Case_Id : Entity_Id;
         Variant : Node_Id) return Entity_Id;
      --  Build the discriminant checking function for a given variant

      procedure Build_Dcheck_Functions (Variant_Part_Node : Node_Id);
      --  Builds the discriminant checking function for each variant of the
      --  given variant part of the record type.

      --------------------------
      -- Build_Case_Statement --
      --------------------------

      function Build_Case_Statement
        (Case_Id : Entity_Id;
         Variant : Node_Id) return Node_Id
      is
         Alt_List       : constant List_Id := New_List;
         Actuals_List   : List_Id;
         Case_Node      : Node_Id;
         Case_Alt_Node  : Node_Id;
         Choice         : Node_Id;
         Choice_List    : List_Id;
         D              : Entity_Id;
         Return_Node    : Node_Id;

      begin
         Case_Node := New_Node (N_Case_Statement, Loc);

         --  Replace the discriminant which controls the variant, with the name
         --  of the formal of the checking function.

         Set_Expression (Case_Node, Make_Identifier (Loc, Chars (Case_Id)));

         Choice := First (Discrete_Choices (Variant));

         if Nkind (Choice) = N_Others_Choice then
            Choice_List := New_Copy_List (Others_Discrete_Choices (Choice));
         else
            Choice_List := New_Copy_List (Discrete_Choices (Variant));
         end if;

         if not Is_Empty_List (Choice_List) then
            Case_Alt_Node := New_Node (N_Case_Statement_Alternative, Loc);
            Set_Discrete_Choices (Case_Alt_Node, Choice_List);

            --  In case this is a nested variant, we need to return the result
            --  of the discriminant checking function for the immediately
            --  enclosing variant.

            if Present (Enclosing_Func_Id) then
               Actuals_List := New_List;

               D := First_Discriminant (Rec_Id);
               while Present (D) loop
                  Append (Make_Identifier (Loc, Chars (D)), Actuals_List);
                  Next_Discriminant (D);
               end loop;

               Return_Node :=
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     Make_Function_Call (Loc,
                       Name =>
                         New_Reference_To (Enclosing_Func_Id,  Loc),
                       Parameter_Associations =>
                         Actuals_List));

            else
               Return_Node :=
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     New_Reference_To (Standard_False, Loc));
            end if;

            Set_Statements (Case_Alt_Node, New_List (Return_Node));
            Append (Case_Alt_Node, Alt_List);
         end if;

         Case_Alt_Node := New_Node (N_Case_Statement_Alternative, Loc);
         Choice_List := New_List (New_Node (N_Others_Choice, Loc));
         Set_Discrete_Choices (Case_Alt_Node, Choice_List);

         Return_Node :=
           Make_Simple_Return_Statement (Loc,
             Expression =>
               New_Reference_To (Standard_True, Loc));

         Set_Statements (Case_Alt_Node, New_List (Return_Node));
         Append (Case_Alt_Node, Alt_List);

         Set_Alternatives (Case_Node, Alt_List);
         return Case_Node;
      end Build_Case_Statement;

      ---------------------------
      -- Build_Dcheck_Function --
      ---------------------------

      function Build_Dcheck_Function
        (Case_Id : Entity_Id;
         Variant : Node_Id) return Entity_Id
      is
         Body_Node           : Node_Id;
         Func_Id             : Entity_Id;
         Parameter_List      : List_Id;
         Spec_Node           : Node_Id;

      begin
         Body_Node := New_Node (N_Subprogram_Body, Loc);
         Sequence := Sequence + 1;

         Func_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Rec_Id), 'D', Sequence));

         Spec_Node := New_Node (N_Function_Specification, Loc);
         Set_Defining_Unit_Name (Spec_Node, Func_Id);

         Parameter_List := Build_Discriminant_Formals (Rec_Id, False);

         Set_Parameter_Specifications (Spec_Node, Parameter_List);
         Set_Result_Definition (Spec_Node,
                                New_Reference_To (Standard_Boolean,  Loc));
         Set_Specification (Body_Node, Spec_Node);
         Set_Declarations (Body_Node, New_List);

         Set_Handled_Statement_Sequence (Body_Node,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List (
               Build_Case_Statement (Case_Id, Variant))));

         Set_Ekind       (Func_Id, E_Function);
         Set_Mechanism   (Func_Id, Default_Mechanism);
         Set_Is_Inlined  (Func_Id, True);
         Set_Is_Pure     (Func_Id, True);
         Set_Is_Public   (Func_Id, Is_Public (Rec_Id));
         Set_Is_Internal (Func_Id, True);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Func_Id);
         end if;

         Analyze (Body_Node);

         Append_Freeze_Action (Rec_Id, Body_Node);
         Set_Dcheck_Function (Variant, Func_Id);
         return Func_Id;
      end Build_Dcheck_Function;

      ----------------------------
      -- Build_Dcheck_Functions --
      ----------------------------

      procedure Build_Dcheck_Functions (Variant_Part_Node : Node_Id) is
         Component_List_Node : Node_Id;
         Decl                : Entity_Id;
         Discr_Name          : Entity_Id;
         Func_Id             : Entity_Id;
         Variant             : Node_Id;
         Saved_Enclosing_Func_Id : Entity_Id;

      begin
         --  Build the discriminant-checking function for each variant, and
         --  label all components of that variant with the function's name.
         --  We only Generate a discriminant-checking function when the
         --  variant is not empty, to prevent the creation of dead code.
         --  The exception to that is when Frontend_Layout_On_Target is set,
         --  because the variant record size function generated in package
         --  Layout needs to generate calls to all discriminant-checking
         --  functions, including those for empty variants.

         Discr_Name := Entity (Name (Variant_Part_Node));
         Variant := First_Non_Pragma (Variants (Variant_Part_Node));

         while Present (Variant) loop
            Component_List_Node := Component_List (Variant);

            if not Null_Present (Component_List_Node)
              or else Frontend_Layout_On_Target
            then
               Func_Id := Build_Dcheck_Function (Discr_Name, Variant);
               Decl :=
                 First_Non_Pragma (Component_Items (Component_List_Node));

               while Present (Decl) loop
                  Set_Discriminant_Checking_Func
                    (Defining_Identifier (Decl), Func_Id);

                  Next_Non_Pragma (Decl);
               end loop;

               if Present (Variant_Part (Component_List_Node)) then
                  Saved_Enclosing_Func_Id := Enclosing_Func_Id;
                  Enclosing_Func_Id := Func_Id;
                  Build_Dcheck_Functions (Variant_Part (Component_List_Node));
                  Enclosing_Func_Id := Saved_Enclosing_Func_Id;
               end if;
            end if;

            Next_Non_Pragma (Variant);
         end loop;
      end Build_Dcheck_Functions;

   --  Start of processing for Build_Discr_Checking_Funcs

   begin
      --  Only build if not done already

      if not Discr_Check_Funcs_Built (N) then
         Type_Def := Type_Definition (N);

         if Nkind (Type_Def) = N_Record_Definition then
            if No (Component_List (Type_Def)) then   -- null record.
               return;
            else
               V := Variant_Part (Component_List (Type_Def));
            end if;

         else pragma Assert (Nkind (Type_Def) = N_Derived_Type_Definition);
            if No (Component_List (Record_Extension_Part (Type_Def))) then
               return;
            else
               V := Variant_Part
                      (Component_List (Record_Extension_Part (Type_Def)));
            end if;
         end if;

         Rec_Id := Defining_Identifier (N);

         if Present (V) and then not Is_Unchecked_Union (Rec_Id) then
            Loc := Sloc (N);
            Enclosing_Func_Id := Empty;
            Build_Dcheck_Functions (V);
         end if;

         Set_Discr_Check_Funcs_Built (N);
      end if;
   end Build_Discr_Checking_Funcs;

   --------------------------------
   -- Build_Discriminant_Formals --
   --------------------------------

   function Build_Discriminant_Formals
     (Rec_Id : Entity_Id;
      Use_Dl : Boolean) return List_Id
   is
      Loc             : Source_Ptr       := Sloc (Rec_Id);
      Parameter_List  : constant List_Id := New_List;
      D               : Entity_Id;
      Formal          : Entity_Id;
      Formal_Type     : Entity_Id;
      Param_Spec_Node : Node_Id;

   begin
      if Has_Discriminants (Rec_Id) then
         D := First_Discriminant (Rec_Id);
         while Present (D) loop
            Loc := Sloc (D);

            if Use_Dl then
               Formal := Discriminal (D);
               Formal_Type := Etype (Formal);
            else
               Formal := Make_Defining_Identifier (Loc, Chars (D));
               Formal_Type := Etype (D);
            end if;

            Param_Spec_Node :=
              Make_Parameter_Specification (Loc,
                  Defining_Identifier => Formal,
                Parameter_Type =>
                  New_Reference_To (Formal_Type, Loc));
            Append (Param_Spec_Node, Parameter_List);
            Next_Discriminant (D);
         end loop;
      end if;

      return Parameter_List;
   end Build_Discriminant_Formals;

   --------------------------------------
   -- Build_Equivalent_Array_Aggregate --
   --------------------------------------

   function Build_Equivalent_Array_Aggregate (T : Entity_Id) return Node_Id is
      Loc        : constant Source_Ptr := Sloc (T);
      Comp_Type  : constant Entity_Id := Component_Type (T);
      Index_Type : constant Entity_Id := Etype (First_Index (T));
      Proc       : constant Entity_Id := Base_Init_Proc (T);
      Lo, Hi     : Node_Id;
      Aggr       : Node_Id;
      Expr       : Node_Id;

   begin
      if not Is_Constrained (T)
        or else Number_Dimensions (T) > 1
        or else No (Proc)
      then
         Initialization_Warning (T);
         return Empty;
      end if;

      Lo := Type_Low_Bound  (Index_Type);
      Hi := Type_High_Bound (Index_Type);

      if not Compile_Time_Known_Value (Lo)
        or else not Compile_Time_Known_Value (Hi)
      then
         Initialization_Warning (T);
         return Empty;
      end if;

      if Is_Record_Type (Comp_Type)
        and then Present (Base_Init_Proc (Comp_Type))
      then
         Expr := Static_Initialization (Base_Init_Proc (Comp_Type));

         if No (Expr) then
            Initialization_Warning (T);
            return Empty;
         end if;

      else
         Initialization_Warning (T);
         return Empty;
      end if;

      Aggr := Make_Aggregate (Loc, No_List, New_List);
      Set_Etype (Aggr, T);
      Set_Aggregate_Bounds (Aggr,
        Make_Range (Loc,
          Low_Bound  => New_Copy (Lo),
          High_Bound => New_Copy (Hi)));
      Set_Parent (Aggr, Parent (Proc));

      Append_To (Component_Associations (Aggr),
         Make_Component_Association (Loc,
              Choices =>
                 New_List (
                   Make_Range (Loc,
                     Low_Bound  => New_Copy (Lo),
                     High_Bound => New_Copy (Hi))),
              Expression => Expr));

      if Static_Array_Aggregate (Aggr) then
         return Aggr;
      else
         Initialization_Warning (T);
         return Empty;
      end if;
   end Build_Equivalent_Array_Aggregate;

   ---------------------------------------
   -- Build_Equivalent_Record_Aggregate --
   ---------------------------------------

   function Build_Equivalent_Record_Aggregate (T : Entity_Id) return Node_Id is
      Agg       : Node_Id;
      Comp      : Entity_Id;
      Comp_Type : Entity_Id;

      --  Start of processing for Build_Equivalent_Record_Aggregate

   begin
      if not Is_Record_Type (T)
        or else Has_Discriminants (T)
        or else Is_Limited_Type (T)
        or else Has_Non_Standard_Rep (T)
      then
         Initialization_Warning (T);
         return Empty;
      end if;

      Comp := First_Component (T);

      --  A null record needs no warning

      if No (Comp) then
         return Empty;
      end if;

      while Present (Comp) loop

         --  Array components are acceptable if initialized by a positional
         --  aggregate with static components.

         if Is_Array_Type (Etype (Comp)) then
            Comp_Type := Component_Type (Etype (Comp));

            if Nkind (Parent (Comp)) /= N_Component_Declaration
              or else No (Expression (Parent (Comp)))
              or else Nkind (Expression (Parent (Comp))) /= N_Aggregate
            then
               Initialization_Warning (T);
               return Empty;

            elsif Is_Scalar_Type (Component_Type (Etype (Comp)))
               and then
                 (not Compile_Time_Known_Value (Type_Low_Bound (Comp_Type))
                   or else
                  not Compile_Time_Known_Value (Type_High_Bound (Comp_Type)))
            then
               Initialization_Warning (T);
               return Empty;

            elsif
              not Static_Array_Aggregate (Expression (Parent (Comp)))
            then
               Initialization_Warning (T);
               return Empty;
            end if;

         elsif Is_Scalar_Type (Etype (Comp)) then
            Comp_Type := Etype (Comp);

            if Nkind (Parent (Comp)) /= N_Component_Declaration
              or else No (Expression (Parent (Comp)))
              or else not Compile_Time_Known_Value (Expression (Parent (Comp)))
              or else not Compile_Time_Known_Value (Type_Low_Bound (Comp_Type))
              or else not
                Compile_Time_Known_Value (Type_High_Bound (Comp_Type))
            then
               Initialization_Warning (T);
               return Empty;
            end if;

         --  For now, other types are excluded

         else
            Initialization_Warning (T);
            return Empty;
         end if;

         Next_Component (Comp);
      end loop;

      --  All components have static initialization. Build positional aggregate
      --  from the given expressions or defaults.

      Agg := Make_Aggregate (Sloc (T), New_List, New_List);
      Set_Parent (Agg, Parent (T));

      Comp := First_Component (T);
      while Present (Comp) loop
         Append
           (New_Copy_Tree (Expression (Parent (Comp))), Expressions (Agg));
         Next_Component (Comp);
      end loop;

      Analyze_And_Resolve (Agg, T);
      return Agg;
   end Build_Equivalent_Record_Aggregate;

   -------------------------------
   -- Build_Initialization_Call --
   -------------------------------

   --  References to a discriminant inside the record type declaration can
   --  appear either in the subtype_indication to constrain a record or an
   --  array, or as part of a larger expression given for the initial value
   --  of a component. In both of these cases N appears in the record
   --  initialization procedure and needs to be replaced by the formal
   --  parameter of the initialization procedure which corresponds to that
   --  discriminant.

   --  In the example below, references to discriminants D1 and D2 in proc_1
   --  are replaced by references to formals with the same name
   --  (discriminals)

   --  A similar replacement is done for calls to any record initialization
   --  procedure for any components that are themselves of a record type.

   --  type R (D1, D2 : Integer) is record
   --     X : Integer := F * D1;
   --     Y : Integer := F * D2;
   --  end record;

   --  procedure proc_1 (Out_2 : out R; D1 : Integer; D2 : Integer) is
   --  begin
   --     Out_2.D1 := D1;
   --     Out_2.D2 := D2;
   --     Out_2.X := F * D1;
   --     Out_2.Y := F * D2;
   --  end;

   function Build_Initialization_Call
     (Loc               : Source_Ptr;
      Id_Ref            : Node_Id;
      Typ               : Entity_Id;
      In_Init_Proc      : Boolean := False;
      Enclos_Type       : Entity_Id := Empty;
      Discr_Map         : Elist_Id := New_Elmt_List;
      With_Default_Init : Boolean := False;
      Constructor_Ref   : Node_Id := Empty) return List_Id
   is
      Res            : constant List_Id := New_List;
      Arg            : Node_Id;
      Args           : List_Id;
      Decls          : List_Id;
      Decl           : Node_Id;
      Discr          : Entity_Id;
      First_Arg      : Node_Id;
      Full_Init_Type : Entity_Id;
      Full_Type      : Entity_Id := Typ;
      Init_Type      : Entity_Id;
      Proc           : Entity_Id;

   begin
      pragma Assert (Constructor_Ref = Empty
        or else Is_CPP_Constructor_Call (Constructor_Ref));

      if No (Constructor_Ref) then
         Proc := Base_Init_Proc (Typ);
      else
         Proc := Base_Init_Proc (Typ, Entity (Name (Constructor_Ref)));
      end if;

      pragma Assert (Present (Proc));
      Init_Type      := Etype (First_Formal (Proc));
      Full_Init_Type := Underlying_Type (Init_Type);

      --  Nothing to do if the Init_Proc is null, unless Initialize_Scalars
      --  is active (in which case we make the call anyway, since in the
      --  actual compiled client it may be non null).
      --  Also nothing to do for value types.

      if (Is_Null_Init_Proc (Proc) and then not Init_Or_Norm_Scalars)
        or else Is_Value_Type (Typ)
        or else
          (Is_Array_Type (Typ) and then Is_Value_Type (Component_Type (Typ)))
      then
         return Empty_List;
      end if;

      --  Go to full view if private type. In the case of successive
      --  private derivations, this can require more than one step.

      while Is_Private_Type (Full_Type)
        and then Present (Full_View (Full_Type))
      loop
         Full_Type := Full_View (Full_Type);
      end loop;

      --  If Typ is derived, the procedure is the initialization procedure for
      --  the root type. Wrap the argument in an conversion to make it type
      --  honest. Actually it isn't quite type honest, because there can be
      --  conflicts of views in the private type case. That is why we set
      --  Conversion_OK in the conversion node.

      if (Is_Record_Type (Typ)
           or else Is_Array_Type (Typ)
           or else Is_Private_Type (Typ))
        and then Init_Type /= Base_Type (Typ)
      then
         First_Arg := OK_Convert_To (Etype (Init_Type), Id_Ref);
         Set_Etype (First_Arg, Init_Type);

      else
         First_Arg := Id_Ref;
      end if;

      Args := New_List (Convert_Concurrent (First_Arg, Typ));

      --  In the tasks case, add _Master as the value of the _Master parameter
      --  and _Chain as the value of the _Chain parameter. At the outer level,
      --  these will be variables holding the corresponding values obtained
      --  from GNARL. At inner levels, they will be the parameters passed down
      --  through the outer routines.

      if Has_Task (Full_Type) then
         if Restriction_Active (No_Task_Hierarchy) then
            Append_To (Args,
              New_Occurrence_Of (RTE (RE_Library_Task_Level), Loc));
         else
            Append_To (Args, Make_Identifier (Loc, Name_uMaster));
         end if;

         --  Add _Chain (not done for sequential elaboration policy, see
         --  comment for Create_Restricted_Task_Sequential in s-tarest.ads).

         if Partition_Elaboration_Policy /= 'S' then
            Append_To (Args, Make_Identifier (Loc, Name_uChain));
         end if;

         --  Ada 2005 (AI-287): In case of default initialized components
         --  with tasks, we generate a null string actual parameter.
         --  This is just a workaround that must be improved later???

         if With_Default_Init then
            Append_To (Args,
              Make_String_Literal (Loc,
                Strval => ""));

         else
            Decls :=
              Build_Task_Image_Decls (Loc, Id_Ref, Enclos_Type, In_Init_Proc);
            Decl  := Last (Decls);

            Append_To (Args,
              New_Occurrence_Of (Defining_Identifier (Decl), Loc));
            Append_List (Decls, Res);
         end if;

      else
         Decls := No_List;
         Decl  := Empty;
      end if;

      --  Add discriminant values if discriminants are present

      if Has_Discriminants (Full_Init_Type) then
         Discr := First_Discriminant (Full_Init_Type);

         while Present (Discr) loop

            --  If this is a discriminated concurrent type, the init_proc
            --  for the corresponding record is being called. Use that type
            --  directly to find the discriminant value, to handle properly
            --  intervening renamed discriminants.

            declare
               T : Entity_Id := Full_Type;

            begin
               if Is_Protected_Type (T) then
                  T := Corresponding_Record_Type (T);

               elsif Is_Private_Type (T)
                 and then Present (Underlying_Full_View (T))
                 and then Is_Protected_Type (Underlying_Full_View (T))
               then
                  T := Corresponding_Record_Type (Underlying_Full_View (T));
               end if;

               Arg :=
                 Get_Discriminant_Value (
                   Discr,
                   T,
                   Discriminant_Constraint (Full_Type));
            end;

            --  If the target has access discriminants, and is constrained by
            --  an access to the enclosing construct, i.e. a current instance,
            --  replace the reference to the type by a reference to the object.

            if Nkind (Arg) = N_Attribute_Reference
              and then Is_Access_Type (Etype (Arg))
              and then Is_Entity_Name (Prefix (Arg))
              and then Is_Type (Entity (Prefix (Arg)))
            then
               Arg :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Copy (Prefix (Id_Ref)),
                   Attribute_Name => Name_Unrestricted_Access);

            elsif In_Init_Proc then

               --  Replace any possible references to the discriminant in the
               --  call to the record initialization procedure with references
               --  to the appropriate formal parameter.

               if Nkind (Arg) = N_Identifier
                  and then Ekind (Entity (Arg)) = E_Discriminant
               then
                  Arg := New_Reference_To (Discriminal (Entity (Arg)), Loc);

               --  Otherwise make a copy of the default expression. Note that
               --  we use the current Sloc for this, because we do not want the
               --  call to appear to be at the declaration point. Within the
               --  expression, replace discriminants with their discriminals.

               else
                  Arg :=
                    New_Copy_Tree (Arg, Map => Discr_Map, New_Sloc => Loc);
               end if;

            else
               if Is_Constrained (Full_Type) then
                  Arg := Duplicate_Subexpr_No_Checks (Arg);
               else
                  --  The constraints come from the discriminant default exps,
                  --  they must be reevaluated, so we use New_Copy_Tree but we
                  --  ensure the proper Sloc (for any embedded calls).

                  Arg := New_Copy_Tree (Arg, New_Sloc => Loc);
               end if;
            end if;

            --  Ada 2005 (AI-287): In case of default initialized components,
            --  if the component is constrained with a discriminant of the
            --  enclosing type, we need to generate the corresponding selected
            --  component node to access the discriminant value. In other cases
            --  this is not required, either  because we are inside the init
            --  proc and we use the corresponding formal, or else because the
            --  component is constrained by an expression.

            if With_Default_Init
              and then Nkind (Id_Ref) = N_Selected_Component
              and then Nkind (Arg) = N_Identifier
              and then Ekind (Entity (Arg)) = E_Discriminant
            then
               Append_To (Args,
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (Prefix (Id_Ref)),
                   Selector_Name => Arg));
            else
               Append_To (Args, Arg);
            end if;

            Next_Discriminant (Discr);
         end loop;
      end if;

      --  If this is a call to initialize the parent component of a derived
      --  tagged type, indicate that the tag should not be set in the parent.

      if Is_Tagged_Type (Full_Init_Type)
        and then not Is_CPP_Class (Full_Init_Type)
        and then Nkind (Id_Ref) = N_Selected_Component
        and then Chars (Selector_Name (Id_Ref)) = Name_uParent
      then
         Append_To (Args, New_Occurrence_Of (Standard_False, Loc));

      elsif Present (Constructor_Ref) then
         Append_List_To (Args,
           New_Copy_List (Parameter_Associations (Constructor_Ref)));
      end if;

      Append_To (Res,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Proc, Loc),
          Parameter_Associations => Args));

      if Needs_Finalization (Typ)
        and then Nkind (Id_Ref) = N_Selected_Component
      then
         if Chars (Selector_Name (Id_Ref)) /= Name_uParent then
            Append_To (Res,
              Make_Init_Call
                (Obj_Ref => New_Copy_Tree (First_Arg),
                 Typ     => Typ));
         end if;
      end if;

      --  When the object is either protected or a task, create static strings
      --  which denote the names of entries and families. Associate the strings
      --  with the concurrent object's Protection_Entries or ATCB. This is a
      --  VMS Debug feature.

      if OpenVMS_On_Target
        and then Is_Concurrent_Type (Typ)
        and then Entry_Names_OK
      then
         Build_Entry_Names (Id_Ref, Typ, Res);
      end if;

      return Res;

   exception
      when RE_Not_Available =>
         return Empty_List;
   end Build_Initialization_Call;

   ----------------------------
   -- Build_Record_Init_Proc --
   ----------------------------

   procedure Build_Record_Init_Proc (N : Node_Id; Rec_Ent : Entity_Id) is
      Decls     : constant List_Id  := New_List;
      Discr_Map : constant Elist_Id := New_Elmt_List;
      Loc       : constant Source_Ptr := Sloc (Rec_Ent);
      Counter   : Int := 0;
      Proc_Id   : Entity_Id;
      Rec_Type  : Entity_Id;
      Set_Tag   : Entity_Id := Empty;

      function Build_Assignment (Id : Entity_Id; N : Node_Id) return List_Id;
      --  Build an assignment statement which assigns the default expression
      --  to its corresponding record component if defined. The left hand side
      --  of the assignment is marked Assignment_OK so that initialization of
      --  limited private records works correctly. This routine may also build
      --  an adjustment call if the component is controlled.

      procedure Build_Discriminant_Assignments (Statement_List : List_Id);
      --  If the record has discriminants, add assignment statements to
      --  Statement_List to initialize the discriminant values from the
      --  arguments of the initialization procedure.

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id;
      --  Build a list representing a sequence of statements which initialize
      --  components of the given component list. This may involve building
      --  case statements for the variant parts. Append any locally declared
      --  objects on list Decls.

      function Build_Init_Call_Thru (Parameters : List_Id) return List_Id;
      --  Given a non-tagged type-derivation that declares discriminants,
      --  such as
      --
      --  type R (R1, R2 : Integer) is record ... end record;
      --
      --  type D (D1 : Integer) is new R (1, D1);
      --
      --  we make the _init_proc of D be
      --
      --       procedure _init_proc (X : D; D1 : Integer) is
      --       begin
      --          _init_proc (R (X), 1, D1);
      --       end _init_proc;
      --
      --  This function builds the call statement in this _init_proc.

      procedure Build_CPP_Init_Procedure;
      --  Build the tree corresponding to the procedure specification and body
      --  of the IC procedure that initializes the C++ part of the dispatch
      --  table of an Ada tagged type that is a derivation of a CPP type.
      --  Install it as the CPP_Init TSS.

      procedure Build_Init_Procedure;
      --  Build the tree corresponding to the procedure specification and body
      --  of the initialization procedure and install it as the _init TSS.

      procedure Build_Offset_To_Top_Functions;
      --  Ada 2005 (AI-251): Build the tree corresponding to the procedure spec
      --  and body of Offset_To_Top, a function used in conjuction with types
      --  having secondary dispatch tables.

      procedure Build_Record_Checks (S : Node_Id; Check_List : List_Id);
      --  Add range checks to components of discriminated records. S is a
      --  subtype indication of a record component. Check_List is a list
      --  to which the check actions are appended.

      function Component_Needs_Simple_Initialization
        (T : Entity_Id) return Boolean;
      --  Determine if a component needs simple initialization, given its type
      --  T. This routine is the same as Needs_Simple_Initialization except for
      --  components of type Tag and Interface_Tag. These two access types do
      --  not require initialization since they are explicitly initialized by
      --  other means.

      function Parent_Subtype_Renaming_Discrims return Boolean;
      --  Returns True for base types N that rename discriminants, else False

      function Requires_Init_Proc (Rec_Id : Entity_Id) return Boolean;
      --  Determine whether a record initialization procedure needs to be
      --  generated for the given record type.

      ----------------------
      -- Build_Assignment --
      ----------------------

      function Build_Assignment (Id : Entity_Id; N : Node_Id) return List_Id is
         N_Loc : constant Source_Ptr := Sloc (N);
         Typ   : constant Entity_Id := Underlying_Type (Etype (Id));
         Exp   : Node_Id := N;
         Kind  : Node_Kind := Nkind (N);
         Lhs   : Node_Id;
         Res   : List_Id;

      begin
         Lhs :=
           Make_Selected_Component (N_Loc,
             Prefix        => Make_Identifier (Loc, Name_uInit),
             Selector_Name => New_Occurrence_Of (Id, N_Loc));
         Set_Assignment_OK (Lhs);

         --  Case of an access attribute applied to the current instance.
         --  Replace the reference to the type by a reference to the actual
         --  object. (Note that this handles the case of the top level of
         --  the expression being given by such an attribute, but does not
         --  cover uses nested within an initial value expression. Nested
         --  uses are unlikely to occur in practice, but are theoretically
         --  possible.) It is not clear how to handle them without fully
         --  traversing the expression. ???

         if Kind = N_Attribute_Reference
           and then Nam_In (Attribute_Name (N), Name_Unchecked_Access,
                                                Name_Unrestricted_Access)
           and then Is_Entity_Name (Prefix (N))
           and then Is_Type (Entity (Prefix (N)))
           and then Entity (Prefix (N)) = Rec_Type
         then
            Exp :=
              Make_Attribute_Reference (N_Loc,
                Prefix         =>
                  Make_Identifier (N_Loc, Name_uInit),
                Attribute_Name => Name_Unrestricted_Access);
         end if;

         --  Take a copy of Exp to ensure that later copies of this component
         --  declaration in derived types see the original tree, not a node
         --  rewritten during expansion of the init_proc. If the copy contains
         --  itypes, the scope of the new itypes is the init_proc being built.

         Exp := New_Copy_Tree (Exp, New_Scope => Proc_Id);

         Res := New_List (
           Make_Assignment_Statement (Loc,
             Name       => Lhs,
             Expression => Exp));

         Set_No_Ctrl_Actions (First (Res));

         --  Adjust the tag if tagged (because of possible view conversions).
         --  Suppress the tag adjustment when VM_Target because VM tags are
         --  represented implicitly in objects.

         if Is_Tagged_Type (Typ)
           and then Tagged_Type_Expansion
         then
            Append_To (Res,
              Make_Assignment_Statement (N_Loc,
                Name       =>
                  Make_Selected_Component (N_Loc,
                    Prefix        =>
                      New_Copy_Tree (Lhs, New_Scope => Proc_Id),
                    Selector_Name =>
                      New_Reference_To (First_Tag_Component (Typ), N_Loc)),

                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Reference_To
                      (Node
                        (First_Elmt
                          (Access_Disp_Table (Underlying_Type (Typ)))),
                       N_Loc))));
         end if;

         --  Adjust the component if controlled except if it is an aggregate
         --  that will be expanded inline.

         if Kind = N_Qualified_Expression then
            Kind := Nkind (Expression (N));
         end if;

         if Needs_Finalization (Typ)
           and then not (Nkind_In (Kind, N_Aggregate, N_Extension_Aggregate))
           and then not Is_Immutably_Limited_Type (Typ)
         then
            Append_To (Res,
              Make_Adjust_Call
                (Obj_Ref => New_Copy_Tree (Lhs),
                 Typ     => Etype (Id)));
         end if;

         return Res;

      exception
         when RE_Not_Available =>
            return Empty_List;
      end Build_Assignment;

      ------------------------------------
      -- Build_Discriminant_Assignments --
      ------------------------------------

      procedure Build_Discriminant_Assignments (Statement_List : List_Id) is
         Is_Tagged : constant Boolean := Is_Tagged_Type (Rec_Type);
         D         : Entity_Id;
         D_Loc     : Source_Ptr;

      begin
         if Has_Discriminants (Rec_Type)
           and then not Is_Unchecked_Union (Rec_Type)
         then
            D := First_Discriminant (Rec_Type);
            while Present (D) loop

               --  Don't generate the assignment for discriminants in derived
               --  tagged types if the discriminant is a renaming of some
               --  ancestor discriminant. This initialization will be done
               --  when initializing the _parent field of the derived record.

               if Is_Tagged
                 and then Present (Corresponding_Discriminant (D))
               then
                  null;

               else
                  D_Loc := Sloc (D);
                  Append_List_To (Statement_List,
                    Build_Assignment (D,
                      New_Reference_To (Discriminal (D), D_Loc)));
               end if;

               Next_Discriminant (D);
            end loop;
         end if;
      end Build_Discriminant_Assignments;

      --------------------------
      -- Build_Init_Call_Thru --
      --------------------------

      function Build_Init_Call_Thru (Parameters : List_Id) return List_Id is
         Parent_Proc : constant Entity_Id :=
                         Base_Init_Proc (Etype (Rec_Type));

         Parent_Type : constant Entity_Id :=
                         Etype (First_Formal (Parent_Proc));

         Uparent_Type : constant Entity_Id :=
                          Underlying_Type (Parent_Type);

         First_Discr_Param : Node_Id;

         Arg          : Node_Id;
         Args         : List_Id;
         First_Arg    : Node_Id;
         Parent_Discr : Entity_Id;
         Res          : List_Id;

      begin
         --  First argument (_Init) is the object to be initialized.
         --  ??? not sure where to get a reasonable Loc for First_Arg

         First_Arg :=
           OK_Convert_To (Parent_Type,
             New_Reference_To (Defining_Identifier (First (Parameters)), Loc));

         Set_Etype (First_Arg, Parent_Type);

         Args := New_List (Convert_Concurrent (First_Arg, Rec_Type));

         --  In the tasks case,
         --    add _Master as the value of the _Master parameter
         --    add _Chain as the value of the _Chain parameter.
         --    add _Task_Name as the value of the _Task_Name parameter.
         --  At the outer level, these will be variables holding the
         --  corresponding values obtained from GNARL or the expander.
         --
         --  At inner levels, they will be the parameters passed down through
         --  the outer routines.

         First_Discr_Param := Next (First (Parameters));

         if Has_Task (Rec_Type) then
            if Restriction_Active (No_Task_Hierarchy) then
               Append_To (Args,
                 New_Occurrence_Of (RTE (RE_Library_Task_Level), Loc));
            else
               Append_To (Args, Make_Identifier (Loc, Name_uMaster));
            end if;

            --  Add _Chain (not done for sequential elaboration policy, see
            --  comment for Create_Restricted_Task_Sequential in s-tarest.ads).

            if Partition_Elaboration_Policy /= 'S' then
               Append_To (Args, Make_Identifier (Loc, Name_uChain));
            end if;

            Append_To (Args, Make_Identifier (Loc, Name_uTask_Name));
            First_Discr_Param := Next (Next (Next (First_Discr_Param)));
         end if;

         --  Append discriminant values

         if Has_Discriminants (Uparent_Type) then
            pragma Assert (not Is_Tagged_Type (Uparent_Type));

            Parent_Discr := First_Discriminant (Uparent_Type);
            while Present (Parent_Discr) loop

               --  Get the initial value for this discriminant
               --  ??? needs to be cleaned up to use parent_Discr_Constr
               --  directly.

               declare
                  Discr       : Entity_Id :=
                                  First_Stored_Discriminant (Uparent_Type);

                  Discr_Value : Elmt_Id :=
                                  First_Elmt (Stored_Constraint (Rec_Type));

               begin
                  while Original_Record_Component (Parent_Discr) /= Discr loop
                     Next_Stored_Discriminant (Discr);
                     Next_Elmt (Discr_Value);
                  end loop;

                  Arg := Node (Discr_Value);
               end;

               --  Append it to the list

               if Nkind (Arg) = N_Identifier
                  and then Ekind (Entity (Arg)) = E_Discriminant
               then
                  Append_To (Args,
                    New_Reference_To (Discriminal (Entity (Arg)), Loc));

               --  Case of access discriminants. We replace the reference
               --  to the type by a reference to the actual object.

               --  Is above comment right??? Use of New_Copy below seems mighty
               --  suspicious ???

               else
                  Append_To (Args, New_Copy (Arg));
               end if;

               Next_Discriminant (Parent_Discr);
            end loop;
         end if;

         Res :=
           New_List (
             Make_Procedure_Call_Statement (Loc,
               Name                   =>
                 New_Occurrence_Of (Parent_Proc, Loc),
               Parameter_Associations => Args));

         return Res;
      end Build_Init_Call_Thru;

      -----------------------------------
      -- Build_Offset_To_Top_Functions --
      -----------------------------------

      procedure Build_Offset_To_Top_Functions is

         procedure Build_Offset_To_Top_Function (Iface_Comp : Entity_Id);
         --  Generate:
         --    function Fxx (O : Address) return Storage_Offset is
         --       type Acc is access all <Typ>;
         --    begin
         --       return Acc!(O).Iface_Comp'Position;
         --    end Fxx;

         ----------------------------------
         -- Build_Offset_To_Top_Function --
         ----------------------------------

         procedure Build_Offset_To_Top_Function (Iface_Comp : Entity_Id) is
            Body_Node : Node_Id;
            Func_Id   : Entity_Id;
            Spec_Node : Node_Id;
            Acc_Type  : Entity_Id;

         begin
            Func_Id := Make_Temporary (Loc, 'F');
            Set_DT_Offset_To_Top_Func (Iface_Comp, Func_Id);

            --  Generate
            --    function Fxx (O : in Rec_Typ) return Storage_Offset;

            Spec_Node := New_Node (N_Function_Specification, Loc);
            Set_Defining_Unit_Name (Spec_Node, Func_Id);
            Set_Parameter_Specifications (Spec_Node, New_List (
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uO),
                In_Present          => True,
                Parameter_Type      =>
                  New_Reference_To (RTE (RE_Address), Loc))));
            Set_Result_Definition (Spec_Node,
              New_Reference_To (RTE (RE_Storage_Offset), Loc));

            --  Generate
            --    function Fxx (O : in Rec_Typ) return Storage_Offset is
            --    begin
            --       return O.Iface_Comp'Position;
            --    end Fxx;

            Body_Node := New_Node (N_Subprogram_Body, Loc);
            Set_Specification (Body_Node, Spec_Node);

            Acc_Type := Make_Temporary (Loc, 'T');
            Set_Declarations (Body_Node, New_List (
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Acc_Type,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present            => True,
                    Null_Exclusion_Present => False,
                    Constant_Present       => False,
                    Subtype_Indication     =>
                      New_Reference_To (Rec_Type, Loc)))));

            Set_Handled_Statement_Sequence (Body_Node,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements     => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression =>
                      Make_Attribute_Reference (Loc,
                        Prefix         =>
                          Make_Selected_Component (Loc,
                            Prefix        =>
                              Unchecked_Convert_To (Acc_Type,
                                Make_Identifier (Loc, Name_uO)),
                            Selector_Name =>
                              New_Reference_To (Iface_Comp, Loc)),
                        Attribute_Name => Name_Position)))));

            Set_Ekind       (Func_Id, E_Function);
            Set_Mechanism   (Func_Id, Default_Mechanism);
            Set_Is_Internal (Func_Id, True);

            if not Debug_Generated_Code then
               Set_Debug_Info_Off (Func_Id);
            end if;

            Analyze (Body_Node);

            Append_Freeze_Action (Rec_Type, Body_Node);
         end Build_Offset_To_Top_Function;

         --  Local variables

         Iface_Comp       : Node_Id;
         Iface_Comp_Elmt  : Elmt_Id;
         Ifaces_Comp_List : Elist_Id;

      --  Start of processing for Build_Offset_To_Top_Functions

      begin
         --  Offset_To_Top_Functions are built only for derivations of types
         --  with discriminants that cover interface types.
         --  Nothing is needed either in case of virtual machines, since
         --  interfaces are handled directly by the VM.

         if not Is_Tagged_Type (Rec_Type)
           or else Etype (Rec_Type) = Rec_Type
           or else not Has_Discriminants (Etype (Rec_Type))
           or else not Tagged_Type_Expansion
         then
            return;
         end if;

         Collect_Interface_Components (Rec_Type, Ifaces_Comp_List);

         --  For each interface type with secondary dispatch table we generate
         --  the Offset_To_Top_Functions (required to displace the pointer in
         --  interface conversions)

         Iface_Comp_Elmt := First_Elmt (Ifaces_Comp_List);
         while Present (Iface_Comp_Elmt) loop
            Iface_Comp := Node (Iface_Comp_Elmt);
            pragma Assert (Is_Interface (Related_Type (Iface_Comp)));

            --  If the interface is a parent of Rec_Type it shares the primary
            --  dispatch table and hence there is no need to build the function

            if not Is_Ancestor (Related_Type (Iface_Comp), Rec_Type,
                                Use_Full_View => True)
            then
               Build_Offset_To_Top_Function (Iface_Comp);
            end if;

            Next_Elmt (Iface_Comp_Elmt);
         end loop;
      end Build_Offset_To_Top_Functions;

      ------------------------------
      -- Build_CPP_Init_Procedure --
      ------------------------------

      procedure Build_CPP_Init_Procedure is
         Body_Node         : Node_Id;
         Body_Stmts        : List_Id;
         Flag_Id           : Entity_Id;
         Flag_Decl         : Node_Id;
         Handled_Stmt_Node : Node_Id;
         Init_Tags_List    : List_Id;
         Proc_Id           : Entity_Id;
         Proc_Spec_Node    : Node_Id;

      begin
         --  Check cases requiring no IC routine

         if not Is_CPP_Class (Root_Type (Rec_Type))
           or else Is_CPP_Class (Rec_Type)
           or else CPP_Num_Prims (Rec_Type) = 0
           or else not Tagged_Type_Expansion
           or else No_Run_Time_Mode
         then
            return;
         end if;

         --  Generate:

         --     Flag : Boolean := False;
         --
         --     procedure Typ_IC is
         --     begin
         --        if not Flag then
         --           Copy C++ dispatch table slots from parent
         --           Update C++ slots of overridden primitives
         --        end if;
         --     end;

         Flag_Id := Make_Temporary (Loc, 'F');

         Flag_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flag_Id,
             Object_Definition =>
               New_Reference_To (Standard_Boolean, Loc),
             Expression =>
               New_Reference_To (Standard_True, Loc));

         Analyze (Flag_Decl);
         Append_Freeze_Action (Rec_Type, Flag_Decl);

         Body_Stmts := New_List;
         Body_Node := New_Node (N_Subprogram_Body, Loc);

         Proc_Spec_Node := New_Node (N_Procedure_Specification, Loc);

         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Make_TSS_Name (Rec_Type, TSS_CPP_Init_Proc));

         Set_Ekind       (Proc_Id, E_Procedure);
         Set_Is_Internal (Proc_Id);

         Set_Defining_Unit_Name (Proc_Spec_Node, Proc_Id);

         Set_Parameter_Specifications (Proc_Spec_Node, New_List);
         Set_Specification (Body_Node, Proc_Spec_Node);
         Set_Declarations (Body_Node, New_List);

         Init_Tags_List := Build_Inherit_CPP_Prims (Rec_Type);

         Append_To (Init_Tags_List,
           Make_Assignment_Statement (Loc,
             Name =>
               New_Reference_To (Flag_Id, Loc),
             Expression =>
               New_Reference_To (Standard_False, Loc)));

         Append_To (Body_Stmts,
           Make_If_Statement (Loc,
             Condition => New_Occurrence_Of (Flag_Id, Loc),
             Then_Statements => Init_Tags_List));

         Handled_Stmt_Node :=
           New_Node (N_Handled_Sequence_Of_Statements, Loc);
         Set_Statements (Handled_Stmt_Node, Body_Stmts);
         Set_Exception_Handlers (Handled_Stmt_Node, No_List);
         Set_Handled_Statement_Sequence (Body_Node, Handled_Stmt_Node);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         --  Associate CPP_Init_Proc with type

         Set_Init_Proc (Rec_Type, Proc_Id);
      end Build_CPP_Init_Procedure;

      --------------------------
      -- Build_Init_Procedure --
      --------------------------

      procedure Build_Init_Procedure is
         Body_Stmts            : List_Id;
         Body_Node             : Node_Id;
         Handled_Stmt_Node     : Node_Id;
         Init_Tags_List        : List_Id;
         Parameters            : List_Id;
         Proc_Spec_Node        : Node_Id;
         Record_Extension_Node : Node_Id;

      begin
         Body_Stmts := New_List;
         Body_Node := New_Node (N_Subprogram_Body, Loc);
         Set_Ekind (Proc_Id, E_Procedure);

         Proc_Spec_Node := New_Node (N_Procedure_Specification, Loc);
         Set_Defining_Unit_Name (Proc_Spec_Node, Proc_Id);

         Parameters := Init_Formals (Rec_Type);
         Append_List_To (Parameters,
           Build_Discriminant_Formals (Rec_Type, True));

         --  For tagged types, we add a flag to indicate whether the routine
         --  is called to initialize a parent component in the init_proc of
         --  a type extension. If the flag is false, we do not set the tag
         --  because it has been set already in the extension.

         if Is_Tagged_Type (Rec_Type) then
            Set_Tag := Make_Temporary (Loc, 'P');

            Append_To (Parameters,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Set_Tag,
                Parameter_Type =>
                  New_Occurrence_Of (Standard_Boolean, Loc),
                Expression =>
                  New_Occurrence_Of (Standard_True, Loc)));
         end if;

         Set_Parameter_Specifications (Proc_Spec_Node, Parameters);
         Set_Specification (Body_Node, Proc_Spec_Node);
         Set_Declarations (Body_Node, Decls);

         --  N is a Derived_Type_Definition that renames the parameters of the
         --  ancestor type. We initialize it by expanding our discriminants and
         --  call the ancestor _init_proc with a type-converted object.

         if Parent_Subtype_Renaming_Discrims then
            Append_List_To (Body_Stmts, Build_Init_Call_Thru (Parameters));

         elsif Nkind (Type_Definition (N)) = N_Record_Definition then
            Build_Discriminant_Assignments (Body_Stmts);

            if not Null_Present (Type_Definition (N)) then
               Append_List_To (Body_Stmts,
                 Build_Init_Statements (
                   Component_List (Type_Definition (N))));
            end if;

         --  N is a Derived_Type_Definition with a possible non-empty
         --  extension. The initialization of a type extension consists in the
         --  initialization of the components in the extension.

         else
            Build_Discriminant_Assignments (Body_Stmts);

            Record_Extension_Node :=
              Record_Extension_Part (Type_Definition (N));

            if not Null_Present (Record_Extension_Node) then
               declare
                  Stmts : constant List_Id :=
                            Build_Init_Statements (
                              Component_List (Record_Extension_Node));

               begin
                  --  The parent field must be initialized first because
                  --  the offset of the new discriminants may depend on it

                  Prepend_To (Body_Stmts, Remove_Head (Stmts));
                  Append_List_To (Body_Stmts, Stmts);
               end;
            end if;
         end if;

         --  Add here the assignment to instantiate the Tag

         --  The assignment corresponds to the code:

         --     _Init._Tag := Typ'Tag;

         --  Suppress the tag assignment when VM_Target because VM tags are
         --  represented implicitly in objects. It is also suppressed in case
         --  of CPP_Class types because in this case the tag is initialized in
         --  the C++ side.

         if Is_Tagged_Type (Rec_Type)
           and then Tagged_Type_Expansion
           and then not No_Run_Time_Mode
         then
            --  Case 1: Ada tagged types with no CPP ancestor. Set the tags of
            --  the actual object and invoke the IP of the parent (in this
            --  order). The tag must be initialized before the call to the IP
            --  of the parent and the assignments to other components because
            --  the initial value of the components may depend on the tag (eg.
            --  through a dispatching operation on an access to the current
            --  type). The tag assignment is not done when initializing the
            --  parent component of a type extension, because in that case the
            --  tag is set in the extension.

            if not Is_CPP_Class (Root_Type (Rec_Type)) then

               --  Initialize the primary tag component

               Init_Tags_List := New_List (
                 Make_Assignment_Statement (Loc,
                   Name =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_uInit),
                       Selector_Name =>
                         New_Reference_To
                           (First_Tag_Component (Rec_Type), Loc)),
                   Expression =>
                     New_Reference_To
                       (Node
                         (First_Elmt (Access_Disp_Table (Rec_Type))), Loc)));

               --  Ada 2005 (AI-251): Initialize the secondary tags components
               --  located at fixed positions (tags whose position depends on
               --  variable size components are initialized later ---see below)

               if Ada_Version >= Ada_2005
                 and then not Is_Interface (Rec_Type)
                 and then Has_Interfaces (Rec_Type)
               then
                  Init_Secondary_Tags
                    (Typ            => Rec_Type,
                     Target         => Make_Identifier (Loc, Name_uInit),
                     Stmts_List     => Init_Tags_List,
                     Fixed_Comps    => True,
                     Variable_Comps => False);
               end if;

               Prepend_To (Body_Stmts,
                 Make_If_Statement (Loc,
                   Condition => New_Occurrence_Of (Set_Tag, Loc),
                   Then_Statements => Init_Tags_List));

            --  Case 2: CPP type. The imported C++ constructor takes care of
            --  tags initialization. No action needed here because the IP
            --  is built by Set_CPP_Constructors; in this case the IP is a
            --  wrapper that invokes the C++ constructor and copies the C++
            --  tags locally. Done to inherit the C++ slots in Ada derivations
            --  (see case 3).

            elsif Is_CPP_Class (Rec_Type) then
               pragma Assert (False);
               null;

            --  Case 3: Combined hierarchy containing C++ types and Ada tagged
            --  type derivations. Derivations of imported C++ classes add a
            --  complication, because we cannot inhibit tag setting in the
            --  constructor for the parent. Hence we initialize the tag after
            --  the call to the parent IP (that is, in reverse order compared
            --  with pure Ada hierarchies ---see comment on case 1).

            else
               --  Initialize the primary tag

               Init_Tags_List := New_List (
                 Make_Assignment_Statement (Loc,
                   Name =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_uInit),
                       Selector_Name =>
                         New_Reference_To
                           (First_Tag_Component (Rec_Type), Loc)),
                   Expression =>
                     New_Reference_To
                       (Node
                         (First_Elmt (Access_Disp_Table (Rec_Type))), Loc)));

               --  Ada 2005 (AI-251): Initialize the secondary tags components
               --  located at fixed positions (tags whose position depends on
               --  variable size components are initialized later ---see below)

               if Ada_Version >= Ada_2005
                 and then not Is_Interface (Rec_Type)
                 and then Has_Interfaces (Rec_Type)
               then
                  Init_Secondary_Tags
                    (Typ            => Rec_Type,
                     Target         => Make_Identifier (Loc, Name_uInit),
                     Stmts_List     => Init_Tags_List,
                     Fixed_Comps    => True,
                     Variable_Comps => False);
               end if;

               --  Initialize the tag component after invocation of parent IP.

               --  Generate:
               --     parent_IP(_init.parent); // Invokes the C++ constructor
               --     [ typIC; ]               // Inherit C++ slots from parent
               --     init_tags

               declare
                  Ins_Nod : Node_Id;

               begin
                  --  Search for the call to the IP of the parent. We assume
                  --  that the first init_proc call is for the parent.

                  Ins_Nod := First (Body_Stmts);
                  while Present (Next (Ins_Nod))
                     and then (Nkind (Ins_Nod) /= N_Procedure_Call_Statement
                                or else not Is_Init_Proc (Name (Ins_Nod)))
                  loop
                     Next (Ins_Nod);
                  end loop;

                  --  The IC routine copies the inherited slots of the C+ part
                  --  of the dispatch table from the parent and updates the
                  --  overridden C++ slots.

                  if CPP_Num_Prims (Rec_Type) > 0 then
                     declare
                        Init_DT : Entity_Id;
                        New_Nod : Node_Id;

                     begin
                        Init_DT := CPP_Init_Proc (Rec_Type);
                        pragma Assert (Present (Init_DT));

                        New_Nod :=
                          Make_Procedure_Call_Statement (Loc,
                            New_Reference_To (Init_DT, Loc));
                        Insert_After (Ins_Nod, New_Nod);

                        --  Update location of init tag statements

                        Ins_Nod := New_Nod;
                     end;
                  end if;

                  Insert_List_After (Ins_Nod, Init_Tags_List);
               end;
            end if;

            --  Ada 2005 (AI-251): Initialize the secondary tag components
            --  located at variable positions. We delay the generation of this
            --  code until here because the value of the attribute 'Position
            --  applied to variable size components of the parent type that
            --  depend on discriminants is only safely read at runtime after
            --  the parent components have been initialized.

            if Ada_Version >= Ada_2005
              and then not Is_Interface (Rec_Type)
              and then Has_Interfaces (Rec_Type)
              and then Has_Discriminants (Etype (Rec_Type))
              and then Is_Variable_Size_Record (Etype (Rec_Type))
            then
               Init_Tags_List := New_List;

               Init_Secondary_Tags
                 (Typ            => Rec_Type,
                  Target         => Make_Identifier (Loc, Name_uInit),
                  Stmts_List     => Init_Tags_List,
                  Fixed_Comps    => False,
                  Variable_Comps => True);

               if Is_Non_Empty_List (Init_Tags_List) then
                  Append_List_To (Body_Stmts, Init_Tags_List);
               end if;
            end if;
         end if;

         Handled_Stmt_Node := New_Node (N_Handled_Sequence_Of_Statements, Loc);
         Set_Statements (Handled_Stmt_Node, Body_Stmts);

         --  Generate:
         --    Local_DF_Id (_init, C1, ..., CN);
         --    raise;

         if Counter > 0
           and then Needs_Finalization (Rec_Type)
           and then not Is_Abstract_Type (Rec_Type)
           and then not Restriction_Active (No_Exception_Propagation)
         then
            declare
               Local_DF_Id : Entity_Id;

            begin
               --  Create a local version of Deep_Finalize which has indication
               --  of partial initialization state.

               Local_DF_Id := Make_Temporary (Loc, 'F');

               Append_To (Decls,
                 Make_Local_Deep_Finalize (Rec_Type, Local_DF_Id));

               Set_Exception_Handlers (Handled_Stmt_Node, New_List (
                 Make_Exception_Handler (Loc,
                   Exception_Choices => New_List (
                     Make_Others_Choice (Loc)),

                   Statements => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name =>
                         New_Reference_To (Local_DF_Id, Loc),

                       Parameter_Associations => New_List (
                         Make_Identifier (Loc, Name_uInit),
                         New_Reference_To (Standard_False, Loc))),

                     Make_Raise_Statement (Loc)))));
            end;
         else
            Set_Exception_Handlers (Handled_Stmt_Node, No_List);
         end if;

         Set_Handled_Statement_Sequence (Body_Node, Handled_Stmt_Node);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         --  Associate Init_Proc with type, and determine if the procedure
         --  is null (happens because of the Initialize_Scalars pragma case,
         --  where we have to generate a null procedure in case it is called
         --  by a client with Initialize_Scalars set). Such procedures have
         --  to be generated, but do not have to be called, so we mark them
         --  as null to suppress the call.

         Set_Init_Proc (Rec_Type, Proc_Id);

         if List_Length (Body_Stmts) = 1

           --  We must skip SCIL nodes because they may have been added to this
           --  list by Insert_Actions.

           and then Nkind (First_Non_SCIL_Node (Body_Stmts)) = N_Null_Statement
           and then VM_Target = No_VM
         then
            --  Even though the init proc may be null at this time it might get
            --  some stuff added to it later by the VM backend.

            Set_Is_Null_Init_Proc (Proc_Id);
         end if;
      end Build_Init_Procedure;

      ---------------------------
      -- Build_Init_Statements --
      ---------------------------

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id is
         Checks     : constant List_Id := New_List;
         Actions    : List_Id   := No_List;
         Comp_Loc   : Source_Ptr;
         Counter_Id : Entity_Id := Empty;
         Decl       : Node_Id;
         Has_POC    : Boolean;
         Id         : Entity_Id;
         Stmts      : List_Id;
         Typ        : Entity_Id;

         procedure Increment_Counter (Loc : Source_Ptr);
         --  Generate an "increment by one" statement for the current counter
         --  and append it to the list Stmts.

         procedure Make_Counter (Loc : Source_Ptr);
         --  Create a new counter for the current component list. The routine
         --  creates a new defining Id, adds an object declaration and sets
         --  the Id generator for the next variant.

         -----------------------
         -- Increment_Counter --
         -----------------------

         procedure Increment_Counter (Loc : Source_Ptr) is
         begin
            --  Generate:
            --    Counter := Counter + 1;

            Append_To (Stmts,
              Make_Assignment_Statement (Loc,
                Name       => New_Reference_To (Counter_Id, Loc),
                Expression =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => New_Reference_To (Counter_Id, Loc),
                    Right_Opnd => Make_Integer_Literal (Loc, 1))));
         end Increment_Counter;

         ------------------
         -- Make_Counter --
         ------------------

         procedure Make_Counter (Loc : Source_Ptr) is
         begin
            --  Increment the Id generator

            Counter := Counter + 1;

            --  Create the entity and declaration

            Counter_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name ('C', Counter));

            --  Generate:
            --    Cnn : Integer := 0;

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Counter_Id,
                Object_Definition   =>
                  New_Reference_To (Standard_Integer, Loc),
                Expression          =>
                  Make_Integer_Literal (Loc, 0)));
         end Make_Counter;

      --  Start of processing for Build_Init_Statements

      begin
         if Null_Present (Comp_List) then
            return New_List (Make_Null_Statement (Loc));
         end if;

         Stmts := New_List;

         --  Loop through visible declarations of task types and protected
         --  types moving any expanded code from the spec to the body of the
         --  init procedure.

         if Is_Task_Record_Type (Rec_Type)
           or else Is_Protected_Record_Type (Rec_Type)
         then
            declare
               Decl : constant Node_Id :=
                        Parent (Corresponding_Concurrent_Type (Rec_Type));
               Def  : Node_Id;
               N1   : Node_Id;
               N2   : Node_Id;

            begin
               if Is_Task_Record_Type (Rec_Type) then
                  Def := Task_Definition (Decl);
               else
                  Def := Protected_Definition (Decl);
               end if;

               if Present (Def) then
                  N1 := First (Visible_Declarations (Def));
                  while Present (N1) loop
                     N2 := N1;
                     N1 := Next (N1);

                     if Nkind (N2) in N_Statement_Other_Than_Procedure_Call
                       or else Nkind (N2) in N_Raise_xxx_Error
                       or else Nkind (N2) = N_Procedure_Call_Statement
                     then
                        Append_To (Stmts,
                          New_Copy_Tree (N2, New_Scope => Proc_Id));
                        Rewrite (N2, Make_Null_Statement (Sloc (N2)));
                        Analyze (N2);
                     end if;
                  end loop;
               end if;
            end;
         end if;

         --  Loop through components, skipping pragmas, in 2 steps. The first
         --  step deals with regular components. The second step deals with
         --  components that have per object constraints and no explicit
         --  initialization.

         Has_POC := False;

         --  First pass : regular components

         Decl := First_Non_Pragma (Component_Items (Comp_List));
         while Present (Decl) loop
            Comp_Loc := Sloc (Decl);
            Build_Record_Checks
              (Subtype_Indication (Component_Definition (Decl)), Checks);

            Id  := Defining_Identifier (Decl);
            Typ := Etype (Id);

            --  Leave any processing of per-object constrained component for
            --  the second pass.

            if Has_Access_Constraint (Id) and then No (Expression (Decl)) then
               Has_POC := True;

            --  Regular component cases

            else
               --  Explicit initialization

               if Present (Expression (Decl)) then
                  if Is_CPP_Constructor_Call (Expression (Decl)) then
                     Actions :=
                       Build_Initialization_Call
                         (Comp_Loc,
                          Id_Ref          =>
                            Make_Selected_Component (Comp_Loc,
                              Prefix        =>
                                Make_Identifier (Comp_Loc, Name_uInit),
                              Selector_Name =>
                                New_Occurrence_Of (Id, Comp_Loc)),
                          Typ             => Typ,
                          In_Init_Proc    => True,
                          Enclos_Type     => Rec_Type,
                          Discr_Map       => Discr_Map,
                          Constructor_Ref => Expression (Decl));
                  else
                     Actions := Build_Assignment (Id, Expression (Decl));
                  end if;

               --  CPU, Dispatching_Domain, Priority and Size components are
               --  filled with the corresponding rep item expression of the
               --  concurrent type (if any).

               elsif Ekind (Scope (Id)) = E_Record_Type
                 and then Present (Corresponding_Concurrent_Type (Scope (Id)))
                 and then Nam_In (Chars (Id), Name_uCPU,
                                              Name_uDispatching_Domain,
                                              Name_uPriority)
               then
                  declare
                     Exp   : Node_Id;
                     Nam   : Name_Id;
                     Ritem : Node_Id;

                  begin
                     if Chars (Id) = Name_uCPU then
                        Nam := Name_CPU;

                     elsif Chars (Id) = Name_uDispatching_Domain then
                        Nam := Name_Dispatching_Domain;

                     elsif Chars (Id) = Name_uPriority then
                        Nam := Name_Priority;
                     end if;

                     --  Get the Rep Item (aspect specification, attribute
                     --  definition clause or pragma) of the corresponding
                     --  concurrent type.

                     Ritem :=
                       Get_Rep_Item
                         (Corresponding_Concurrent_Type (Scope (Id)),
                          Nam,
                          Check_Parents => False);

                     if Present (Ritem) then

                        --  Pragma case

                        if Nkind (Ritem) = N_Pragma then
                           Exp := First (Pragma_Argument_Associations (Ritem));

                           if Nkind (Exp) = N_Pragma_Argument_Association then
                              Exp := Expression (Exp);
                           end if;

                           --  Conversion for Priority expression

                           if Nam = Name_Priority then
                              if Pragma_Name (Ritem) = Name_Priority
                                and then not GNAT_Mode
                              then
                                 Exp := Convert_To (RTE (RE_Priority), Exp);
                              else
                                 Exp :=
                                   Convert_To (RTE (RE_Any_Priority), Exp);
                              end if;
                           end if;

                        --  Aspect/Attribute definition clause case

                        else
                           Exp := Expression (Ritem);

                           --  Conversion for Priority expression

                           if Nam = Name_Priority then
                              if Chars (Ritem) = Name_Priority
                                and then not GNAT_Mode
                              then
                                 Exp := Convert_To (RTE (RE_Priority), Exp);
                              else
                                 Exp :=
                                   Convert_To (RTE (RE_Any_Priority), Exp);
                              end if;
                           end if;
                        end if;

                        --  Conversion for Dispatching_Domain value

                        if Nam = Name_Dispatching_Domain then
                           Exp :=
                             Unchecked_Convert_To
                               (RTE (RE_Dispatching_Domain_Access), Exp);
                        end if;

                        Actions := Build_Assignment (Id, Exp);

                     --  Nothing needed if no Rep Item

                     else
                        Actions := No_List;
                     end if;
                  end;

               --  Composite component with its own Init_Proc

               elsif not Is_Interface (Typ)
                 and then Has_Non_Null_Base_Init_Proc (Typ)
               then
                  Actions :=
                    Build_Initialization_Call
                      (Comp_Loc,
                       Make_Selected_Component (Comp_Loc,
                         Prefix        =>
                           Make_Identifier (Comp_Loc, Name_uInit),
                         Selector_Name => New_Occurrence_Of (Id, Comp_Loc)),
                       Typ,
                       In_Init_Proc => True,
                       Enclos_Type  => Rec_Type,
                       Discr_Map    => Discr_Map);

                  Clean_Task_Names (Typ, Proc_Id);

               --  Simple initialization

               elsif Component_Needs_Simple_Initialization (Typ) then
                  Actions :=
                    Build_Assignment
                      (Id, Get_Simple_Init_Val (Typ, N, Esize (Id)));

               --  Nothing needed for this case

               else
                  Actions := No_List;
               end if;

               if Present (Checks) then
                  Append_List_To (Stmts, Checks);
               end if;

               if Present (Actions) then
                  Append_List_To (Stmts, Actions);

                  --  Preserve the initialization state in the current counter

                  if Chars (Id) /= Name_uParent
                    and then Needs_Finalization (Typ)
                  then
                     if No (Counter_Id) then
                        Make_Counter (Comp_Loc);
                     end if;

                     Increment_Counter (Comp_Loc);
                  end if;
               end if;
            end if;

            Next_Non_Pragma (Decl);
         end loop;

         --  Set up tasks and protected object support. This needs to be done
         --  before any component with a per-object access discriminant
         --  constraint, or any variant part (which may contain such
         --  components) is initialized, because the initialization of these
         --  components may reference the enclosing concurrent object.

         --  For a task record type, add the task create call and calls to bind
         --  any interrupt (signal) entries.

         if Is_Task_Record_Type (Rec_Type) then

            --  In the case of the restricted run time the ATCB has already
            --  been preallocated.

            if Restricted_Profile then
               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_uInit),
                       Selector_Name => Make_Identifier (Loc, Name_uTask_Id)),
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         Make_Selected_Component (Loc,
                           Prefix        => Make_Identifier (Loc, Name_uInit),
                           Selector_Name => Make_Identifier (Loc, Name_uATCB)),
                       Attribute_Name => Name_Unchecked_Access)));
            end if;

            Append_To (Stmts, Make_Task_Create_Call (Rec_Type));

            declare
               Task_Type : constant Entity_Id :=
                             Corresponding_Concurrent_Type (Rec_Type);
               Task_Decl : constant Node_Id := Parent (Task_Type);
               Task_Def  : constant Node_Id := Task_Definition (Task_Decl);
               Decl_Loc  : Source_Ptr;
               Ent       : Entity_Id;
               Vis_Decl  : Node_Id;

            begin
               if Present (Task_Def) then
                  Vis_Decl := First (Visible_Declarations (Task_Def));
                  while Present (Vis_Decl) loop
                     Decl_Loc := Sloc (Vis_Decl);

                     if Nkind (Vis_Decl) = N_Attribute_Definition_Clause then
                        if Get_Attribute_Id (Chars (Vis_Decl)) =
                                                       Attribute_Address
                        then
                           Ent := Entity (Name (Vis_Decl));

                           if Ekind (Ent) = E_Entry then
                              Append_To (Stmts,
                                Make_Procedure_Call_Statement (Decl_Loc,
                                  Name =>
                                    New_Reference_To (RTE (
                                      RE_Bind_Interrupt_To_Entry), Decl_Loc),
                                  Parameter_Associations => New_List (
                                    Make_Selected_Component (Decl_Loc,
                                      Prefix        =>
                                        Make_Identifier (Decl_Loc, Name_uInit),
                                      Selector_Name =>
                                        Make_Identifier
                                         (Decl_Loc, Name_uTask_Id)),
                                    Entry_Index_Expression
                                      (Decl_Loc, Ent, Empty, Task_Type),
                                    Expression (Vis_Decl))));
                           end if;
                        end if;
                     end if;

                     Next (Vis_Decl);
                  end loop;
               end if;
            end;
         end if;

         --  For a protected type, add statements generated by
         --  Make_Initialize_Protection.

         if Is_Protected_Record_Type (Rec_Type) then
            Append_List_To (Stmts,
              Make_Initialize_Protection (Rec_Type));
         end if;

         --  Second pass: components with per-object constraints

         if Has_POC then
            Decl := First_Non_Pragma (Component_Items (Comp_List));
            while Present (Decl) loop
               Comp_Loc := Sloc (Decl);
               Id := Defining_Identifier (Decl);
               Typ := Etype (Id);

               if Has_Access_Constraint (Id)
                 and then No (Expression (Decl))
               then
                  if Has_Non_Null_Base_Init_Proc (Typ) then
                     Append_List_To (Stmts,
                       Build_Initialization_Call (Comp_Loc,
                         Make_Selected_Component (Comp_Loc,
                           Prefix        =>
                             Make_Identifier (Comp_Loc, Name_uInit),
                           Selector_Name => New_Occurrence_Of (Id, Comp_Loc)),
                         Typ,
                         In_Init_Proc => True,
                         Enclos_Type  => Rec_Type,
                         Discr_Map    => Discr_Map));

                     Clean_Task_Names (Typ, Proc_Id);

                     --  Preserve initialization state in the current counter

                     if Needs_Finalization (Typ) then
                        if No (Counter_Id) then
                           Make_Counter (Comp_Loc);
                        end if;

                        Increment_Counter (Comp_Loc);
                     end if;

                  elsif Component_Needs_Simple_Initialization (Typ) then
                     Append_List_To (Stmts,
                       Build_Assignment
                         (Id, Get_Simple_Init_Val (Typ, N, Esize (Id))));
                  end if;
               end if;

               Next_Non_Pragma (Decl);
            end loop;
         end if;

         --  Process the variant part

         if Present (Variant_Part (Comp_List)) then
            declare
               Variant_Alts : constant List_Id := New_List;
               Var_Loc      : Source_Ptr;
               Variant      : Node_Id;

            begin
               Variant :=
                 First_Non_Pragma (Variants (Variant_Part (Comp_List)));
               while Present (Variant) loop
                  Var_Loc := Sloc (Variant);
                  Append_To (Variant_Alts,
                    Make_Case_Statement_Alternative (Var_Loc,
                      Discrete_Choices =>
                        New_Copy_List (Discrete_Choices (Variant)),
                      Statements =>
                        Build_Init_Statements (Component_List (Variant))));
                  Next_Non_Pragma (Variant);
               end loop;

               --  The expression of the case statement which is a reference
               --  to one of the discriminants is replaced by the appropriate
               --  formal parameter of the initialization procedure.

               Append_To (Stmts,
                 Make_Case_Statement (Var_Loc,
                   Expression =>
                     New_Reference_To (Discriminal (
                       Entity (Name (Variant_Part (Comp_List)))), Var_Loc),
                   Alternatives => Variant_Alts));
            end;
         end if;

         --  If no initializations when generated for component declarations
         --  corresponding to this Stmts, append a null statement to Stmts to
         --  to make it a valid Ada tree.

         if Is_Empty_List (Stmts) then
            Append (Make_Null_Statement (Loc), Stmts);
         end if;

         return Stmts;

      exception
         when RE_Not_Available =>
         return Empty_List;
      end Build_Init_Statements;

      -------------------------
      -- Build_Record_Checks --
      -------------------------

      procedure Build_Record_Checks (S : Node_Id; Check_List : List_Id) is
         Subtype_Mark_Id : Entity_Id;

         procedure Constrain_Array
           (SI         : Node_Id;
            Check_List : List_Id);
         --  Apply a list of index constraints to an unconstrained array type.
         --  The first parameter is the entity for the resulting subtype.
         --  Check_List is a list to which the check actions are appended.

         ---------------------
         -- Constrain_Array --
         ---------------------

         procedure Constrain_Array
           (SI         : Node_Id;
            Check_List : List_Id)
         is
            C                     : constant Node_Id := Constraint (SI);
            Number_Of_Constraints : Nat := 0;
            Index                 : Node_Id;
            S, T                  : Entity_Id;

            procedure Constrain_Index
              (Index      : Node_Id;
               S          : Node_Id;
               Check_List : List_Id);
            --  Process an index constraint in a constrained array declaration.
            --  The constraint can be either a subtype name or a range with or
            --  without an explicit subtype mark. Index is the corresponding
            --  index of the unconstrained array. S is the range expression.
            --  Check_List is a list to which the check actions are appended.

            ---------------------
            -- Constrain_Index --
            ---------------------

            procedure Constrain_Index
              (Index        : Node_Id;
               S            : Node_Id;
               Check_List   : List_Id)
            is
               T : constant Entity_Id := Etype (Index);

            begin
               if Nkind (S) = N_Range then
                  Process_Range_Expr_In_Decl (S, T, Check_List);
               end if;
            end Constrain_Index;

         --  Start of processing for Constrain_Array

         begin
            T := Entity (Subtype_Mark (SI));

            if Ekind (T) in Access_Kind then
               T := Designated_Type (T);
            end if;

            S := First (Constraints (C));

            while Present (S) loop
               Number_Of_Constraints := Number_Of_Constraints + 1;
               Next (S);
            end loop;

            --  In either case, the index constraint must provide a discrete
            --  range for each index of the array type and the type of each
            --  discrete range must be the same as that of the corresponding
            --  index. (RM 3.6.1)

            S := First (Constraints (C));
            Index := First_Index (T);
            Analyze (Index);

            --  Apply constraints to each index type

            for J in 1 .. Number_Of_Constraints loop
               Constrain_Index (Index, S, Check_List);
               Next (Index);
               Next (S);
            end loop;
         end Constrain_Array;

      --  Start of processing for Build_Record_Checks

      begin
         if Nkind (S) = N_Subtype_Indication then
            Find_Type (Subtype_Mark (S));
            Subtype_Mark_Id := Entity (Subtype_Mark (S));

            --  Remaining processing depends on type

            case Ekind (Subtype_Mark_Id) is

               when Array_Kind =>
                  Constrain_Array (S, Check_List);

               when others =>
                  null;
            end case;
         end if;
      end Build_Record_Checks;

      -------------------------------------------
      -- Component_Needs_Simple_Initialization --
      -------------------------------------------

      function Component_Needs_Simple_Initialization
        (T : Entity_Id) return Boolean
      is
      begin
         return
           Needs_Simple_Initialization (T)
             and then not Is_RTE (T, RE_Tag)

               --  Ada 2005 (AI-251): Check also the tag of abstract interfaces

             and then not Is_RTE (T, RE_Interface_Tag);
      end Component_Needs_Simple_Initialization;

      --------------------------------------
      -- Parent_Subtype_Renaming_Discrims --
      --------------------------------------

      function Parent_Subtype_Renaming_Discrims return Boolean is
         De : Entity_Id;
         Dp : Entity_Id;

      begin
         if Base_Type (Rec_Ent) /= Rec_Ent then
            return False;
         end if;

         if Etype (Rec_Ent) = Rec_Ent
           or else not Has_Discriminants (Rec_Ent)
           or else Is_Constrained (Rec_Ent)
           or else Is_Tagged_Type (Rec_Ent)
         then
            return False;
         end if;

         --  If there are no explicit stored discriminants we have inherited
         --  the root type discriminants so far, so no renamings occurred.

         if First_Discriminant (Rec_Ent) =
              First_Stored_Discriminant (Rec_Ent)
         then
            return False;
         end if;

         --  Check if we have done some trivial renaming of the parent
         --  discriminants, i.e. something like
         --
         --    type DT (X1, X2: int) is new PT (X1, X2);

         De := First_Discriminant (Rec_Ent);
         Dp := First_Discriminant (Etype (Rec_Ent));
         while Present (De) loop
            pragma Assert (Present (Dp));

            if Corresponding_Discriminant (De) /= Dp then
               return True;
            end if;

            Next_Discriminant (De);
            Next_Discriminant (Dp);
         end loop;

         return Present (Dp);
      end Parent_Subtype_Renaming_Discrims;

      ------------------------
      -- Requires_Init_Proc --
      ------------------------

      function Requires_Init_Proc (Rec_Id : Entity_Id) return Boolean is
         Comp_Decl : Node_Id;
         Id        : Entity_Id;
         Typ       : Entity_Id;

      begin
         --  Definitely do not need one if specifically suppressed

         if Initialization_Suppressed (Rec_Id) then
            return False;
         end if;

         --  If it is a type derived from a type with unknown discriminants,
         --  we cannot build an initialization procedure for it.

         if Has_Unknown_Discriminants (Rec_Id)
           or else Has_Unknown_Discriminants (Etype (Rec_Id))
         then
            return False;
         end if;

         --  Otherwise we need to generate an initialization procedure if
         --  Is_CPP_Class is False and at least one of the following applies:

         --  1. Discriminants are present, since they need to be initialized
         --     with the appropriate discriminant constraint expressions.
         --     However, the discriminant of an unchecked union does not
         --     count, since the discriminant is not present.

         --  2. The type is a tagged type, since the implicit Tag component
         --     needs to be initialized with a pointer to the dispatch table.

         --  3. The type contains tasks

         --  4. One or more components has an initial value

         --  5. One or more components is for a type which itself requires
         --     an initialization procedure.

         --  6. One or more components is a type that requires simple
         --     initialization (see Needs_Simple_Initialization), except
         --     that types Tag and Interface_Tag are excluded, since fields
         --     of these types are initialized by other means.

         --  7. The type is the record type built for a task type (since at
         --     the very least, Create_Task must be called)

         --  8. The type is the record type built for a protected type (since
         --     at least Initialize_Protection must be called)

         --  9. The type is marked as a public entity. The reason we add this
         --     case (even if none of the above apply) is to properly handle
         --     Initialize_Scalars. If a package is compiled without an IS
         --     pragma, and the client is compiled with an IS pragma, then
         --     the client will think an initialization procedure is present
         --     and call it, when in fact no such procedure is required, but
         --     since the call is generated, there had better be a routine
         --     at the other end of the call, even if it does nothing!)

         --  Note: the reason we exclude the CPP_Class case is because in this
         --  case the initialization is performed by the C++ constructors, and
         --  the IP is built by Set_CPP_Constructors.

         if Is_CPP_Class (Rec_Id) then
            return False;

         elsif Is_Interface (Rec_Id) then
            return False;

         elsif (Has_Discriminants (Rec_Id)
                  and then not Is_Unchecked_Union (Rec_Id))
           or else Is_Tagged_Type (Rec_Id)
           or else Is_Concurrent_Record_Type (Rec_Id)
           or else Has_Task (Rec_Id)
         then
            return True;
         end if;

         Id := First_Component (Rec_Id);
         while Present (Id) loop
            Comp_Decl := Parent (Id);
            Typ := Etype (Id);

            if Present (Expression (Comp_Decl))
              or else Has_Non_Null_Base_Init_Proc (Typ)
              or else Component_Needs_Simple_Initialization (Typ)
            then
               return True;
            end if;

            Next_Component (Id);
         end loop;

         --  As explained above, a record initialization procedure is needed
         --  for public types in case Initialize_Scalars applies to a client.
         --  However, such a procedure is not needed in the case where either
         --  of restrictions No_Initialize_Scalars or No_Default_Initialization
         --  applies. No_Initialize_Scalars excludes the possibility of using
         --  Initialize_Scalars in any partition, and No_Default_Initialization
         --  implies that no initialization should ever be done for objects of
         --  the type, so is incompatible with Initialize_Scalars.

         if not Restriction_Active (No_Initialize_Scalars)
           and then not Restriction_Active (No_Default_Initialization)
           and then Is_Public (Rec_Id)
         then
            return True;
         end if;

         return False;
      end Requires_Init_Proc;

   --  Start of processing for Build_Record_Init_Proc

   begin
      --  Check for value type, which means no initialization required

      Rec_Type := Defining_Identifier (N);

      if Is_Value_Type (Rec_Type) then
         return;
      end if;

      --  This may be full declaration of a private type, in which case
      --  the visible entity is a record, and the private entity has been
      --  exchanged with it in the private part of the current package.
      --  The initialization procedure is built for the record type, which
      --  is retrievable from the private entity.

      if Is_Incomplete_Or_Private_Type (Rec_Type) then
         Rec_Type := Underlying_Type (Rec_Type);
      end if;

      --  If there are discriminants, build the discriminant map to replace
      --  discriminants by their discriminals in complex bound expressions.
      --  These only arise for the corresponding records of synchronized types.

      if Is_Concurrent_Record_Type (Rec_Type)
        and then Has_Discriminants (Rec_Type)
      then
         declare
            Disc : Entity_Id;
         begin
            Disc := First_Discriminant (Rec_Type);
            while Present (Disc) loop
               Append_Elmt (Disc, Discr_Map);
               Append_Elmt (Discriminal (Disc), Discr_Map);
               Next_Discriminant (Disc);
            end loop;
         end;
      end if;

      --  Derived types that have no type extension can use the initialization
      --  procedure of their parent and do not need a procedure of their own.
      --  This is only correct if there are no representation clauses for the
      --  type or its parent, and if the parent has in fact been frozen so
      --  that its initialization procedure exists.

      if Is_Derived_Type (Rec_Type)
        and then not Is_Tagged_Type (Rec_Type)
        and then not Is_Unchecked_Union (Rec_Type)
        and then not Has_New_Non_Standard_Rep (Rec_Type)
        and then not Parent_Subtype_Renaming_Discrims
        and then Has_Non_Null_Base_Init_Proc (Etype (Rec_Type))
      then
         Copy_TSS (Base_Init_Proc (Etype (Rec_Type)), Rec_Type);

      --  Otherwise if we need an initialization procedure, then build one,
      --  mark it as public and inlinable and as having a completion.

      elsif Requires_Init_Proc (Rec_Type)
        or else Is_Unchecked_Union (Rec_Type)
      then
         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Make_Init_Proc_Name (Rec_Type));

         --  If No_Default_Initialization restriction is active, then we don't
         --  want to build an init_proc, but we need to mark that an init_proc
         --  would be needed if this restriction was not active (so that we can
         --  detect attempts to call it), so set a dummy init_proc in place.

         if Restriction_Active (No_Default_Initialization) then
            Set_Init_Proc (Rec_Type, Proc_Id);
            return;
         end if;

         Build_Offset_To_Top_Functions;
         Build_CPP_Init_Procedure;
         Build_Init_Procedure;
         Set_Is_Public (Proc_Id, Is_Public (Rec_Ent));

         --  The initialization of protected records is not worth inlining.
         --  In addition, when compiled for another unit for inlining purposes,
         --  it may make reference to entities that have not been elaborated
         --  yet. The initialization of controlled records contains a nested
         --  clean-up procedure that makes it impractical to inline as well,
         --  and leads to undefined symbols if inlined in a different unit.
         --  Similar considerations apply to task types.

         if not Is_Concurrent_Type (Rec_Type)
           and then not Has_Task (Rec_Type)
           and then not Needs_Finalization (Rec_Type)
         then
            Set_Is_Inlined  (Proc_Id);
         end if;

         Set_Is_Internal    (Proc_Id);
         Set_Has_Completion (Proc_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         declare
            Agg : constant Node_Id :=
                    Build_Equivalent_Record_Aggregate (Rec_Type);

            procedure Collect_Itypes (Comp : Node_Id);
            --  Generate references to itypes in the aggregate, because
            --  the first use of the aggregate may be in a nested scope.

            --------------------
            -- Collect_Itypes --
            --------------------

            procedure Collect_Itypes (Comp : Node_Id) is
               Ref      : Node_Id;
               Sub_Aggr : Node_Id;
               Typ      : constant Entity_Id := Etype (Comp);

            begin
               if Is_Array_Type (Typ)
                 and then Is_Itype (Typ)
               then
                  Ref := Make_Itype_Reference (Loc);
                  Set_Itype (Ref, Typ);
                  Append_Freeze_Action (Rec_Type, Ref);

                  Ref := Make_Itype_Reference (Loc);
                  Set_Itype (Ref, Etype (First_Index (Typ)));
                  Append_Freeze_Action (Rec_Type, Ref);

                  Sub_Aggr := First (Expressions (Comp));

                  --  Recurse on nested arrays

                  while Present (Sub_Aggr) loop
                     Collect_Itypes (Sub_Aggr);
                     Next (Sub_Aggr);
                  end loop;
               end if;
            end Collect_Itypes;

         begin
            --  If there is a static initialization aggregate for the type,
            --  generate itype references for the types of its (sub)components,
            --  to prevent out-of-scope errors in the resulting tree.
            --  The aggregate may have been rewritten as a Raise node, in which
            --  case there are no relevant itypes.

            if Present (Agg)
              and then Nkind (Agg) = N_Aggregate
            then
               Set_Static_Initialization (Proc_Id, Agg);

               declare
                  Comp  : Node_Id;
               begin
                  Comp := First (Component_Associations (Agg));
                  while Present (Comp) loop
                     Collect_Itypes (Expression (Comp));
                     Next (Comp);
                  end loop;
               end;
            end if;
         end;
      end if;
   end Build_Record_Init_Proc;

   --------------------------------
   -- Build_Record_Invariant_Proc --
   --------------------------------

   function Build_Record_Invariant_Proc
     (R_Type : Entity_Id;
      Nod    : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Object_Name : constant Name_Id := New_Internal_Name ('I');
      --  Name for argument of invariant procedure

      Object_Entity : constant Node_Id :=
        Make_Defining_Identifier (Loc, Object_Name);
      --  The procedure declaration entity for the argument

      Invariant_Found : Boolean;
      --  Set if any component needs an invariant check.

      Proc_Id   : Entity_Id;
      Proc_Body : Node_Id;
      Stmts     : List_Id;
      Type_Def  : Node_Id;

      function Build_Invariant_Checks (Comp_List : Node_Id) return List_Id;
      --  Recursive procedure that generates a list of checks for components
      --  that need it, and recurses through variant parts when present.

      function Build_Component_Invariant_Call (Comp : Entity_Id)
      return Node_Id;
      --  Build call to invariant procedure for a record component.

      ------------------------------------
      -- Build_Component_Invariant_Call --
      ------------------------------------

      function Build_Component_Invariant_Call (Comp : Entity_Id)
      return Node_Id
      is
         Sel_Comp : Node_Id;
         Typ      : Entity_Id;
         Call     : Node_Id;

      begin
         Invariant_Found := True;
         Typ := Etype (Comp);

         Sel_Comp :=
           Make_Selected_Component (Loc,
             Prefix      => New_Occurrence_Of (Object_Entity, Loc),
             Selector_Name => New_Occurrence_Of (Comp, Loc));

         if Is_Access_Type (Typ) then
            Sel_Comp := Make_Explicit_Dereference (Loc, Sel_Comp);
            Typ := Designated_Type (Typ);
         end if;

         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (Invariant_Procedure (Typ), Loc),
             Parameter_Associations => New_List (Sel_Comp));

         if Is_Access_Type (Etype (Comp)) then
            Call :=
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd   => Make_Null (Loc),
                    Right_Opnd  =>
                       Make_Selected_Component (Loc,
                         Prefix      => New_Occurrence_Of (Object_Entity, Loc),
                         Selector_Name => New_Occurrence_Of (Comp, Loc))),
                Then_Statements => New_List (Call));
         end if;

         return Call;
      end Build_Component_Invariant_Call;

      ----------------------------
      -- Build_Invariant_Checks --
      ----------------------------

      function Build_Invariant_Checks (Comp_List : Node_Id) return List_Id is
         Decl     : Node_Id;
         Id       : Entity_Id;
         Stmts    : List_Id;

      begin
         Stmts := New_List;
         Decl := First_Non_Pragma (Component_Items (Comp_List));
         while Present (Decl) loop
            if Nkind (Decl) = N_Component_Declaration then
               Id  := Defining_Identifier (Decl);

               if Has_Invariants (Etype (Id))
                 and then In_Open_Scopes (Scope (R_Type))
               then
                  Append_To (Stmts, Build_Component_Invariant_Call (Id));

               elsif Is_Access_Type (Etype (Id))
                 and then not Is_Access_Constant (Etype (Id))
                 and then Has_Invariants (Designated_Type (Etype (Id)))
                 and then In_Open_Scopes (Scope (Designated_Type (Etype (Id))))
               then
                  Append_To (Stmts, Build_Component_Invariant_Call (Id));
               end if;
            end if;

            Next (Decl);
         end loop;

         if Present (Variant_Part (Comp_List)) then
            declare
               Variant_Alts  : constant List_Id := New_List;
               Var_Loc       : Source_Ptr;
               Variant       : Node_Id;
               Variant_Stmts : List_Id;

            begin
               Variant :=
                 First_Non_Pragma (Variants (Variant_Part (Comp_List)));
               while Present (Variant) loop
                  Variant_Stmts :=
                    Build_Invariant_Checks (Component_List (Variant));
                  Var_Loc := Sloc (Variant);
                  Append_To (Variant_Alts,
                    Make_Case_Statement_Alternative (Var_Loc,
                      Discrete_Choices =>
                        New_Copy_List (Discrete_Choices (Variant)),
                      Statements => Variant_Stmts));

                  Next_Non_Pragma (Variant);
               end loop;

               --  The expression in the case statement is the reference to
               --  the discriminant of the target object.

               Append_To (Stmts,
                 Make_Case_Statement (Var_Loc,
                   Expression =>
                     Make_Selected_Component (Var_Loc,
                      Prefix => New_Occurrence_Of (Object_Entity, Var_Loc),
                      Selector_Name => New_Occurrence_Of
                        (Entity
                          (Name (Variant_Part (Comp_List))), Var_Loc)),
                      Alternatives => Variant_Alts));
            end;
         end if;

         return Stmts;
      end Build_Invariant_Checks;

   --  Start of processing for Build_Record_Invariant_Proc

   begin
      Invariant_Found := False;
      Type_Def := Type_Definition (Parent (R_Type));

      if Nkind (Type_Def) = N_Record_Definition
        and then not Null_Present (Type_Def)
      then
         Stmts := Build_Invariant_Checks (Component_List (Type_Def));
      else
         return Empty;
      end if;

      if not Invariant_Found then
         return Empty;
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
           Chars => New_External_Name (Chars (R_Type), "Invariant"));

      Proc_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Id,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => Object_Entity,
                  Parameter_Type      => New_Occurrence_Of (R_Type, Loc)))),

          Declarations               => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));

      Set_Ekind          (Proc_Id, E_Procedure);
      Set_Is_Public      (Proc_Id, Is_Public (R_Type));
      Set_Is_Internal    (Proc_Id);
      Set_Has_Completion (Proc_Id);

      return Proc_Body;
      --  Insert_After (Nod, Proc_Body);
      --  Analyze (Proc_Body);
   end Build_Record_Invariant_Proc;

   ----------------------------
   -- Build_Slice_Assignment --
   ----------------------------

   --  Generates the following subprogram:

   --    procedure Assign
   --     (Source,  Target    : Array_Type,
   --      Left_Lo, Left_Hi   : Index;
   --      Right_Lo, Right_Hi : Index;
   --      Rev                : Boolean)
   --    is
   --       Li1 : Index;
   --       Ri1 : Index;

   --    begin

   --       if Left_Hi < Left_Lo then
   --          return;
   --       end if;

   --       if Rev  then
   --          Li1 := Left_Hi;
   --          Ri1 := Right_Hi;
   --       else
   --          Li1 := Left_Lo;
   --          Ri1 := Right_Lo;
   --       end if;

   --       loop
   --          Target (Li1) := Source (Ri1);

   --          if Rev then
   --             exit when Li1 = Left_Lo;
   --             Li1 := Index'pred (Li1);
   --             Ri1 := Index'pred (Ri1);
   --          else
   --             exit when Li1 = Left_Hi;
   --             Li1 := Index'succ (Li1);
   --             Ri1 := Index'succ (Ri1);
   --          end if;
   --       end loop;
   --    end Assign;

   procedure Build_Slice_Assignment (Typ : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Index : constant Entity_Id  := Base_Type (Etype (First_Index (Typ)));

      Larray    : constant Entity_Id := Make_Temporary (Loc, 'A');
      Rarray    : constant Entity_Id := Make_Temporary (Loc, 'R');
      Left_Lo   : constant Entity_Id := Make_Temporary (Loc, 'L');
      Left_Hi   : constant Entity_Id := Make_Temporary (Loc, 'L');
      Right_Lo  : constant Entity_Id := Make_Temporary (Loc, 'R');
      Right_Hi  : constant Entity_Id := Make_Temporary (Loc, 'R');
      Rev       : constant Entity_Id := Make_Temporary (Loc, 'D');
      --  Formal parameters of procedure

      Proc_Name : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      Chars => Make_TSS_Name (Typ, TSS_Slice_Assign));

      Lnn : constant Entity_Id := Make_Temporary (Loc, 'L');
      Rnn : constant Entity_Id := Make_Temporary (Loc, 'R');
      --  Subscripts for left and right sides

      Decls : List_Id;
      Loops : Node_Id;
      Stats : List_Id;

   begin
      --  Build declarations for indexes

      Decls := New_List;

      Append_To (Decls,
         Make_Object_Declaration (Loc,
           Defining_Identifier => Lnn,
           Object_Definition  =>
             New_Occurrence_Of (Index, Loc)));

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Rnn,
          Object_Definition  =>
            New_Occurrence_Of (Index, Loc)));

      Stats := New_List;

      --  Build test for empty slice case

      Append_To (Stats,
        Make_If_Statement (Loc,
          Condition =>
             Make_Op_Lt (Loc,
               Left_Opnd  => New_Occurrence_Of (Left_Hi, Loc),
               Right_Opnd => New_Occurrence_Of (Left_Lo, Loc)),
          Then_Statements => New_List (Make_Simple_Return_Statement (Loc))));

      --  Build initializations for indexes

      declare
         F_Init : constant List_Id := New_List;
         B_Init : constant List_Id := New_List;

      begin
         Append_To (F_Init,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Lnn, Loc),
             Expression => New_Occurrence_Of (Left_Lo, Loc)));

         Append_To (F_Init,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Rnn, Loc),
             Expression => New_Occurrence_Of (Right_Lo, Loc)));

         Append_To (B_Init,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Lnn, Loc),
             Expression => New_Occurrence_Of (Left_Hi, Loc)));

         Append_To (B_Init,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Rnn, Loc),
             Expression => New_Occurrence_Of (Right_Hi, Loc)));

         Append_To (Stats,
           Make_If_Statement (Loc,
             Condition => New_Occurrence_Of (Rev, Loc),
             Then_Statements => B_Init,
             Else_Statements => F_Init));
      end;

      --  Now construct the assignment statement

      Loops :=
        Make_Loop_Statement (Loc,
          Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name =>
                Make_Indexed_Component (Loc,
                  Prefix => New_Occurrence_Of (Larray, Loc),
                  Expressions => New_List (New_Occurrence_Of (Lnn, Loc))),
              Expression =>
                Make_Indexed_Component (Loc,
                  Prefix => New_Occurrence_Of (Rarray, Loc),
                  Expressions => New_List (New_Occurrence_Of (Rnn, Loc))))),
          End_Label  => Empty);

      --  Build the exit condition and increment/decrement statements

      declare
         F_Ass : constant List_Id := New_List;
         B_Ass : constant List_Id := New_List;

      begin
         Append_To (F_Ass,
           Make_Exit_Statement (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Occurrence_Of (Lnn, Loc),
                 Right_Opnd => New_Occurrence_Of (Left_Hi, Loc))));

         Append_To (F_Ass,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Lnn, Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Index, Loc),
                 Attribute_Name => Name_Succ,
                 Expressions => New_List (
                   New_Occurrence_Of (Lnn, Loc)))));

         Append_To (F_Ass,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Rnn, Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Index, Loc),
                 Attribute_Name => Name_Succ,
                 Expressions => New_List (
                   New_Occurrence_Of (Rnn, Loc)))));

         Append_To (B_Ass,
           Make_Exit_Statement (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Occurrence_Of (Lnn, Loc),
                 Right_Opnd => New_Occurrence_Of (Left_Lo, Loc))));

         Append_To (B_Ass,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Lnn, Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Index, Loc),
                 Attribute_Name => Name_Pred,
                   Expressions => New_List (
                     New_Occurrence_Of (Lnn, Loc)))));

         Append_To (B_Ass,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Rnn, Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Index, Loc),
                 Attribute_Name => Name_Pred,
                 Expressions => New_List (
                   New_Occurrence_Of (Rnn, Loc)))));

         Append_To (Statements (Loops),
           Make_If_Statement (Loc,
             Condition => New_Occurrence_Of (Rev, Loc),
             Then_Statements => B_Ass,
             Else_Statements => F_Ass));
      end;

      Append_To (Stats, Loops);

      declare
         Spec    : Node_Id;
         Formals : List_Id := New_List;

      begin
         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Larray,
             Out_Present => True,
             Parameter_Type =>
               New_Reference_To (Base_Type (Typ), Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Rarray,
             Parameter_Type =>
               New_Reference_To (Base_Type (Typ), Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Left_Lo,
             Parameter_Type =>
               New_Reference_To (Index, Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Left_Hi,
             Parameter_Type =>
               New_Reference_To (Index, Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Right_Lo,
             Parameter_Type =>
               New_Reference_To (Index, Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Right_Hi,
             Parameter_Type =>
               New_Reference_To (Index, Loc)));

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Rev,
             Parameter_Type =>
               New_Reference_To (Standard_Boolean, Loc)));

         Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Proc_Name,
             Parameter_Specifications => Formals);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification              => Spec,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stats)));
      end;

      Set_TSS (Typ, Proc_Name);
      Set_Is_Pure (Proc_Name);
   end Build_Slice_Assignment;

   -----------------------------
   -- Build_Untagged_Equality --
   -----------------------------

   procedure Build_Untagged_Equality (Typ : Entity_Id) is
      Build_Eq : Boolean;
      Comp     : Entity_Id;
      Decl     : Node_Id;
      Op       : Entity_Id;
      Prim     : Elmt_Id;
      Eq_Op    : Entity_Id;

      function User_Defined_Eq (T : Entity_Id) return Entity_Id;
      --  Check whether the type T has a user-defined primitive equality. If so
      --  return it, else return Empty. If true for a component of Typ, we have
      --  to build the primitive equality for it.

      ---------------------
      -- User_Defined_Eq --
      ---------------------

      function User_Defined_Eq (T : Entity_Id) return Entity_Id is
         Prim : Elmt_Id;
         Op   : Entity_Id;

      begin
         Op := TSS (T, TSS_Composite_Equality);

         if Present (Op) then
            return Op;
         end if;

         Prim := First_Elmt (Collect_Primitive_Operations (T));
         while Present (Prim) loop
            Op := Node (Prim);

            if Chars (Op) = Name_Op_Eq
              and then Etype (Op) = Standard_Boolean
              and then Etype (First_Formal (Op)) = T
              and then Etype (Next_Formal (First_Formal (Op))) = T
            then
               return Op;
            end if;

            Next_Elmt (Prim);
         end loop;

         return Empty;
      end User_Defined_Eq;

   --  Start of processing for Build_Untagged_Equality

   begin
      --  If a record component has a primitive equality operation, we must
      --  build the corresponding one for the current type.

      Build_Eq := False;
      Comp := First_Component (Typ);
      while Present (Comp) loop
         if Is_Record_Type (Etype (Comp))
           and then Present (User_Defined_Eq (Etype (Comp)))
         then
            Build_Eq := True;
         end if;

         Next_Component (Comp);
      end loop;

      --  If there is a user-defined equality for the type, we do not create
      --  the implicit one.

      Prim := First_Elmt (Collect_Primitive_Operations (Typ));
      Eq_Op := Empty;
      while Present (Prim) loop
         if Chars (Node (Prim)) = Name_Op_Eq
           and then Comes_From_Source (Node (Prim))

         --  Don't we also need to check formal types and return type as in
         --  User_Defined_Eq above???

         then
            Eq_Op := Node (Prim);
            Build_Eq := False;
            exit;
         end if;

         Next_Elmt (Prim);
      end loop;

      --  If the type is derived, inherit the operation, if present, from the
      --  parent type. It may have been declared after the type derivation. If
      --  the parent type itself is derived, it may have inherited an operation
      --  that has itself been overridden, so update its alias and related
      --  flags. Ditto for inequality.

      if No (Eq_Op) and then Is_Derived_Type (Typ) then
         Prim := First_Elmt (Collect_Primitive_Operations (Etype (Typ)));
         while Present (Prim) loop
            if Chars (Node (Prim)) = Name_Op_Eq then
               Copy_TSS (Node (Prim), Typ);
               Build_Eq := False;

               declare
                  Op    : constant Entity_Id := User_Defined_Eq (Typ);
                  Eq_Op : constant Entity_Id := Node (Prim);
                  NE_Op : constant Entity_Id := Next_Entity (Eq_Op);

               begin
                  if Present (Op) then
                     Set_Alias (Op, Eq_Op);
                     Set_Is_Abstract_Subprogram
                       (Op, Is_Abstract_Subprogram (Eq_Op));

                     if Chars (Next_Entity (Op)) = Name_Op_Ne then
                        Set_Is_Abstract_Subprogram
                          (Next_Entity (Op), Is_Abstract_Subprogram (NE_Op));
                     end if;
                  end if;
               end;

               exit;
            end if;

            Next_Elmt (Prim);
         end loop;
      end if;

      --  If not inherited and not user-defined, build body as for a type with
      --  tagged components.

      if Build_Eq then
         Decl :=
           Make_Eq_Body (Typ, Make_TSS_Name (Typ, TSS_Composite_Equality));
         Op := Defining_Entity (Decl);
         Set_TSS (Typ, Op);
         Set_Is_Pure (Op);

         if Is_Library_Level_Entity (Typ) then
            Set_Is_Public (Op);
         end if;
      end if;
   end Build_Untagged_Equality;

   ------------------------------------
   -- Build_Variant_Record_Equality --
   ------------------------------------

   --  Generates:

   --    function _Equality (X, Y : T) return Boolean is
   --    begin
   --       --  Compare discriminants

   --       if False or else X.D1 /= Y.D1 or else X.D2 /= Y.D2 then
   --          return False;
   --       end if;

   --       --  Compare components

   --       if False or else X.C1 /= Y.C1 or else X.C2 /= Y.C2 then
   --          return False;
   --       end if;

   --       --  Compare variant part

   --       case X.D1 is
   --          when V1 =>
   --             if False or else X.C2 /= Y.C2 or else X.C3 /= Y.C3 then
   --                return False;
   --             end if;
   --          ...
   --          when Vn =>
   --             if False or else X.Cn /= Y.Cn then
   --                return False;
   --             end if;
   --       end case;

   --       return True;
   --    end _Equality;

   procedure Build_Variant_Record_Equality (Typ : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (Typ);

      F : constant Entity_Id :=
            Make_Defining_Identifier (Loc,
              Chars => Make_TSS_Name (Typ, TSS_Composite_Equality));

      X : constant Entity_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Name_X);

      Y : constant Entity_Id :=
            Make_Defining_Identifier (Loc,
              Chars => Name_Y);

      Def    : constant Node_Id := Parent (Typ);
      Comps  : constant Node_Id := Component_List (Type_Definition (Def));
      Stmts  : constant List_Id := New_List;
      Pspecs : constant List_Id := New_List;

   begin
      --  Derived Unchecked_Union types no longer inherit the equality function
      --  of their parent.

      if Is_Derived_Type (Typ)
        and then not Is_Unchecked_Union (Typ)
        and then not Has_New_Non_Standard_Rep (Typ)
      then
         declare
            Parent_Eq : constant Entity_Id :=
                          TSS (Root_Type (Typ), TSS_Composite_Equality);

         begin
            if Present (Parent_Eq) then
               Copy_TSS (Parent_Eq, Typ);
               return;
            end if;
         end;
      end if;

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => F,
              Parameter_Specifications => Pspecs,
              Result_Definition => New_Reference_To (Standard_Boolean, Loc)),
          Declarations               => New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Stmts)));

      Append_To (Pspecs,
        Make_Parameter_Specification (Loc,
          Defining_Identifier => X,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      Append_To (Pspecs,
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Y,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      --  Unchecked_Unions require additional machinery to support equality.
      --  Two extra parameters (A and B) are added to the equality function
      --  parameter list for each discriminant of the type, in order to
      --  capture the inferred values of the discriminants in equality calls.
      --  The names of the parameters match the names of the corresponding
      --  discriminant, with an added suffix.

      if Is_Unchecked_Union (Typ) then
         declare
            Discr      : Entity_Id;
            Discr_Type : Entity_Id;
            A, B       : Entity_Id;
            New_Discrs : Elist_Id;

         begin
            New_Discrs := New_Elmt_List;

            Discr := First_Discriminant (Typ);
            while Present (Discr) loop
               Discr_Type := Etype (Discr);
               A := Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (Discr), 'A'));

               B := Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (Discr), 'B'));

               --  Add new parameters to the parameter list

               Append_To (Pspecs,
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier => A,
                   Parameter_Type      => New_Reference_To (Discr_Type, Loc)));

               Append_To (Pspecs,
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier => B,
                   Parameter_Type      => New_Reference_To (Discr_Type, Loc)));

               Append_Elmt (A, New_Discrs);

               --  Generate the following code to compare each of the inferred
               --  discriminants:

               --  if a /= b then
               --     return False;
               --  end if;

               Append_To (Stmts,
                 Make_If_Statement (Loc,
                   Condition       =>
                     Make_Op_Ne (Loc,
                       Left_Opnd  => New_Reference_To (A, Loc),
                       Right_Opnd => New_Reference_To (B, Loc)),
                   Then_Statements => New_List (
                     Make_Simple_Return_Statement (Loc,
                       Expression =>
                         New_Occurrence_Of (Standard_False, Loc)))));
               Next_Discriminant (Discr);
            end loop;

            --  Generate component-by-component comparison. Note that we must
            --  propagate the inferred discriminants formals to act as
            --  the case statement switch. Their value is added when an
            --  equality call on unchecked unions is expanded.

            Append_List_To (Stmts,
              Make_Eq_Case (Typ, Comps, New_Discrs));
         end;

      --  Normal case (not unchecked union)

      else
         Append_To (Stmts,
           Make_Eq_If (Typ,
             Discriminant_Specifications (Def)));

         Append_List_To (Stmts,
           Make_Eq_Case (Typ, Comps));
      end if;

      Append_To (Stmts,
        Make_Simple_Return_Statement (Loc,
          Expression => New_Reference_To (Standard_True, Loc)));

      Set_TSS (Typ, F);
      Set_Is_Pure (F);

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (F);
      end if;
   end Build_Variant_Record_Equality;

   -----------------------------
   -- Check_Stream_Attributes --
   -----------------------------

   procedure Check_Stream_Attributes (Typ : Entity_Id) is
      Comp      : Entity_Id;
      Par_Read  : constant Boolean :=
                    Stream_Attribute_Available (Typ, TSS_Stream_Read)
                      and then not Has_Specified_Stream_Read (Typ);
      Par_Write : constant Boolean :=
                    Stream_Attribute_Available (Typ, TSS_Stream_Write)
                      and then not Has_Specified_Stream_Write (Typ);

      procedure Check_Attr (Nam : Name_Id; TSS_Nam : TSS_Name_Type);
      --  Check that Comp has a user-specified Nam stream attribute

      ----------------
      -- Check_Attr --
      ----------------

      procedure Check_Attr (Nam : Name_Id; TSS_Nam : TSS_Name_Type) is
      begin
         if not Stream_Attribute_Available (Etype (Comp), TSS_Nam) then
            Error_Msg_Name_1 := Nam;
            Error_Msg_N
              ("|component& in limited extension must have% attribute", Comp);
         end if;
      end Check_Attr;

   --  Start of processing for Check_Stream_Attributes

   begin
      if Par_Read or else Par_Write then
         Comp := First_Component (Typ);
         while Present (Comp) loop
            if Comes_From_Source (Comp)
              and then Original_Record_Component (Comp) = Comp
              and then Is_Limited_Type (Etype (Comp))
            then
               if Par_Read then
                  Check_Attr (Name_Read, TSS_Stream_Read);
               end if;

               if Par_Write then
                  Check_Attr (Name_Write, TSS_Stream_Write);
               end if;
            end if;

            Next_Component (Comp);
         end loop;
      end if;
   end Check_Stream_Attributes;

   -----------------------------
   -- Expand_Record_Extension --
   -----------------------------

   --  Add a field _parent at the beginning of the record extension. This is
   --  used to implement inheritance. Here are some examples of expansion:

   --  1. no discriminants
   --      type T2 is new T1 with null record;
   --   gives
   --      type T2 is new T1 with record
   --        _Parent : T1;
   --      end record;

   --  2. renamed discriminants
   --    type T2 (B, C : Int) is new T1 (A => B) with record
   --       _Parent : T1 (A => B);
   --       D : Int;
   --    end;

   --  3. inherited discriminants
   --    type T2 is new T1 with record -- discriminant A inherited
   --       _Parent : T1 (A);
   --       D : Int;
   --    end;

   procedure Expand_Record_Extension (T : Entity_Id; Def : Node_Id) is
      Indic        : constant Node_Id    := Subtype_Indication (Def);
      Loc          : constant Source_Ptr := Sloc (Def);
      Rec_Ext_Part : Node_Id             := Record_Extension_Part (Def);
      Par_Subtype  : Entity_Id;
      Comp_List    : Node_Id;
      Comp_Decl    : Node_Id;
      Parent_N     : Node_Id;
      D            : Entity_Id;
      List_Constr  : constant List_Id    := New_List;

   begin
      --  Expand_Record_Extension is called directly from the semantics, so
      --  we must check to see whether expansion is active before proceeding

      if not Expander_Active then
         return;
      end if;

      --  This may be a derivation of an untagged private type whose full
      --  view is tagged, in which case the Derived_Type_Definition has no
      --  extension part. Build an empty one now.

      if No (Rec_Ext_Part) then
         Rec_Ext_Part :=
           Make_Record_Definition (Loc,
             End_Label      => Empty,
             Component_List => Empty,
             Null_Present   => True);

         Set_Record_Extension_Part (Def, Rec_Ext_Part);
         Mark_Rewrite_Insertion (Rec_Ext_Part);
      end if;

      Comp_List := Component_List (Rec_Ext_Part);

      Parent_N := Make_Defining_Identifier (Loc, Name_uParent);

      --  If the derived type inherits its discriminants the type of the
      --  _parent field must be constrained by the inherited discriminants

      if Has_Discriminants (T)
        and then Nkind (Indic) /= N_Subtype_Indication
        and then not Is_Constrained (Entity (Indic))
      then
         D := First_Discriminant (T);
         while Present (D) loop
            Append_To (List_Constr, New_Occurrence_Of (D, Loc));
            Next_Discriminant (D);
         end loop;

         Par_Subtype :=
           Process_Subtype (
             Make_Subtype_Indication (Loc,
               Subtype_Mark => New_Reference_To (Entity (Indic), Loc),
               Constraint   =>
                 Make_Index_Or_Discriminant_Constraint (Loc,
                   Constraints => List_Constr)),
             Def);

      --  Otherwise the original subtype_indication is just what is needed

      else
         Par_Subtype := Process_Subtype (New_Copy_Tree (Indic), Def);
      end if;

      Set_Parent_Subtype (T, Par_Subtype);

      Comp_Decl :=
        Make_Component_Declaration (Loc,
          Defining_Identifier => Parent_N,
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present => False,
              Subtype_Indication => New_Reference_To (Par_Subtype, Loc)));

      if Null_Present (Rec_Ext_Part) then
         Set_Component_List (Rec_Ext_Part,
           Make_Component_List (Loc,
             Component_Items => New_List (Comp_Decl),
             Variant_Part => Empty,
             Null_Present => False));
         Set_Null_Present (Rec_Ext_Part, False);

      elsif Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Set_Component_Items (Comp_List, New_List (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         Insert_Before (First (Component_Items (Comp_List)), Comp_Decl);
      end if;

      Analyze (Comp_Decl);
   end Expand_Record_Extension;

   ------------------------------------
   -- Expand_N_Full_Type_Declaration --
   ------------------------------------

   procedure Expand_N_Full_Type_Declaration (N : Node_Id) is

      procedure Build_Master (Ptr_Typ : Entity_Id);
      --  Create the master associated with Ptr_Typ

      ------------------
      -- Build_Master --
      ------------------

      procedure Build_Master (Ptr_Typ : Entity_Id) is
         Desig_Typ : Entity_Id := Designated_Type (Ptr_Typ);

      begin
         --  If the designated type is an incomplete view coming from a
         --  limited-with'ed package, we need to use the nonlimited view in
         --  case it has tasks.

         if Ekind (Desig_Typ) in Incomplete_Kind
           and then Present (Non_Limited_View (Desig_Typ))
         then
            Desig_Typ := Non_Limited_View (Desig_Typ);
         end if;

         --  Anonymous access types are created for the components of the
         --  record parameter for an entry declaration. No master is created
         --  for such a type.

         if Comes_From_Source (N)
           and then Has_Task (Desig_Typ)
         then
            Build_Master_Entity (Ptr_Typ);
            Build_Master_Renaming (Ptr_Typ);

         --  Create a class-wide master because a Master_Id must be generated
         --  for access-to-limited-class-wide types whose root may be extended
         --  with task components.

         --  Note: This code covers access-to-limited-interfaces because they
         --        can be used to reference tasks implementing them.

         elsif Is_Limited_Class_Wide_Type (Desig_Typ)
           and then Tasking_Allowed

           --  Do not create a class-wide master for types whose convention is
           --  Java since these types cannot embed Ada tasks anyway. Note that
           --  the following test cannot catch the following case:

           --      package java.lang.Object is
           --         type Typ is tagged limited private;
           --         type Ref is access all Typ'Class;
           --      private
           --         type Typ is tagged limited ...;
           --         pragma Convention (Typ, Java)
           --      end;

           --  Because the convention appears after we have done the
           --  processing for type Ref.

           and then Convention (Desig_Typ) /= Convention_Java
           and then Convention (Desig_Typ) /= Convention_CIL
         then
            Build_Class_Wide_Master (Ptr_Typ);
         end if;
      end Build_Master;

      --  Local declarations

      Def_Id : constant Entity_Id := Defining_Identifier (N);
      B_Id   : constant Entity_Id := Base_Type (Def_Id);
      FN     : Node_Id;
      Par_Id : Entity_Id;

   --  Start of processing for Expand_N_Full_Type_Declaration

   begin
      if Is_Access_Type (Def_Id) then
         Build_Master (Def_Id);

         if Ekind (Def_Id) = E_Access_Protected_Subprogram_Type then
            Expand_Access_Protected_Subprogram_Type (N);
         end if;

      --  Array of anonymous access-to-task pointers

      elsif Ada_Version >= Ada_2005
        and then Is_Array_Type (Def_Id)
        and then Is_Access_Type (Component_Type (Def_Id))
        and then Ekind (Component_Type (Def_Id)) = E_Anonymous_Access_Type
      then
         Build_Master (Component_Type (Def_Id));

      elsif Has_Task (Def_Id) then
         Expand_Previous_Access_Type (Def_Id);

      --  Check the components of a record type or array of records for
      --  anonymous access-to-task pointers.

      elsif Ada_Version >= Ada_2005
        and then (Is_Record_Type (Def_Id)
                   or else
                     (Is_Array_Type (Def_Id)
                       and then Is_Record_Type (Component_Type (Def_Id))))
      then
         declare
            Comp  : Entity_Id;
            First : Boolean;
            M_Id  : Entity_Id;
            Typ   : Entity_Id;

         begin
            if Is_Array_Type (Def_Id) then
               Comp := First_Entity (Component_Type (Def_Id));
            else
               Comp := First_Entity (Def_Id);
            end if;

            --  Examine all components looking for anonymous access-to-task
            --  types.

            First := True;
            while Present (Comp) loop
               Typ := Etype (Comp);

               if Ekind (Typ) = E_Anonymous_Access_Type
                 and then Has_Task (Available_View (Designated_Type (Typ)))
                 and then No (Master_Id (Typ))
               then
                  --  Ensure that the record or array type have a _master

                  if First then
                     Build_Master_Entity (Def_Id);
                     Build_Master_Renaming (Typ);
                     M_Id := Master_Id (Typ);

                     First := False;

                  --  Reuse the same master to service any additional types

                  else
                     Set_Master_Id (Typ, M_Id);
                  end if;
               end if;

               Next_Entity (Comp);
            end loop;
         end;
      end if;

      Par_Id := Etype (B_Id);

      --  The parent type is private then we need to inherit any TSS operations
      --  from the full view.

      if Ekind (Par_Id) in Private_Kind
        and then Present (Full_View (Par_Id))
      then
         Par_Id := Base_Type (Full_View (Par_Id));
      end if;

      if Nkind (Type_Definition (Original_Node (N))) =
                                                   N_Derived_Type_Definition
        and then not Is_Tagged_Type (Def_Id)
        and then Present (Freeze_Node (Par_Id))
        and then Present (TSS_Elist (Freeze_Node (Par_Id)))
      then
         Ensure_Freeze_Node (B_Id);
         FN := Freeze_Node (B_Id);

         if No (TSS_Elist (FN)) then
            Set_TSS_Elist (FN, New_Elmt_List);
         end if;

         declare
            T_E  : constant Elist_Id := TSS_Elist (FN);
            Elmt : Elmt_Id;

         begin
            Elmt := First_Elmt (TSS_Elist (Freeze_Node (Par_Id)));
            while Present (Elmt) loop
               if Chars (Node (Elmt)) /= Name_uInit then
                  Append_Elmt (Node (Elmt), T_E);
               end if;

               Next_Elmt (Elmt);
            end loop;

            --  If the derived type itself is private with a full view, then
            --  associate the full view with the inherited TSS_Elist as well.

            if Ekind (B_Id) in Private_Kind
              and then Present (Full_View (B_Id))
            then
               Ensure_Freeze_Node (Base_Type (Full_View (B_Id)));
               Set_TSS_Elist
                 (Freeze_Node (Base_Type (Full_View (B_Id))), TSS_Elist (FN));
            end if;
         end;
      end if;
   end Expand_N_Full_Type_Declaration;

   ---------------------------------
   -- Expand_N_Object_Declaration --
   ---------------------------------

   procedure Expand_N_Object_Declaration (N : Node_Id) is
      Def_Id   : constant Entity_Id  := Defining_Identifier (N);
      Expr     : constant Node_Id    := Expression (N);
      Loc      : constant Source_Ptr := Sloc (N);
      Typ      : constant Entity_Id  := Etype (Def_Id);
      Base_Typ : constant Entity_Id  := Base_Type (Typ);
      Expr_Q   : Node_Id;
      Id_Ref   : Node_Id;
      New_Ref  : Node_Id;

      Init_After : Node_Id := N;
      --  Node after which the init proc call is to be inserted. This is
      --  normally N, except for the case of a shared passive variable, in
      --  which case the init proc call must be inserted only after the bodies
      --  of the shared variable procedures have been seen.

      function Build_Equivalent_Aggregate return Boolean;
      --  If the object has a constrained discriminated type and no initial
      --  value, it may be possible to build an equivalent aggregate instead,
      --  and prevent an actual call to the initialization procedure.

      function Rewrite_As_Renaming return Boolean;
      --  Indicate whether to rewrite a declaration with initialization into an
      --  object renaming declaration (see below).

      --------------------------------
      -- Build_Equivalent_Aggregate --
      --------------------------------

      function Build_Equivalent_Aggregate return Boolean is
         Aggr      : Node_Id;
         Comp      : Entity_Id;
         Discr     : Elmt_Id;
         Full_Type : Entity_Id;

      begin
         Full_Type := Typ;

         if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
            Full_Type := Full_View (Typ);
         end if;

         --  Only perform this transformation if Elaboration_Code is forbidden
         --  or undesirable, and if this is a global entity of a constrained
         --  record type.

         --  If Initialize_Scalars might be active this  transformation cannot
         --  be performed either, because it will lead to different semantics
         --  or because elaboration code will in fact be created.

         if Ekind (Full_Type) /= E_Record_Subtype
           or else not Has_Discriminants (Full_Type)
           or else not Is_Constrained (Full_Type)
           or else Is_Controlled (Full_Type)
           or else Is_Limited_Type (Full_Type)
           or else not Restriction_Active (No_Initialize_Scalars)
         then
            return False;
         end if;

         if Ekind (Current_Scope) = E_Package
          and then
            (Restriction_Active (No_Elaboration_Code)
              or else Is_Preelaborated (Current_Scope))
         then

            --  Building a static aggregate is possible if the discriminants
            --  have static values and the other components have static
            --  defaults or none.

            Discr := First_Elmt (Discriminant_Constraint (Full_Type));
            while Present (Discr) loop
               if not Is_OK_Static_Expression (Node (Discr)) then
                  return False;
               end if;

               Next_Elmt (Discr);
            end loop;

            --  Check that initialized components are OK, and that non-
            --  initialized components do not require a call to their own
            --  initialization procedure.

            Comp := First_Component (Full_Type);
            while Present (Comp) loop
               if Ekind (Comp) = E_Component
                 and then Present (Expression (Parent (Comp)))
                 and then
                   not Is_OK_Static_Expression (Expression (Parent (Comp)))
               then
                  return False;

               elsif Has_Non_Null_Base_Init_Proc (Etype (Comp)) then
                  return False;

               end if;

               Next_Component (Comp);
            end loop;

            --  Everything is static, assemble the aggregate, discriminant
            --  values first.

            Aggr :=
               Make_Aggregate (Loc,
                Expressions            => New_List,
                Component_Associations => New_List);

            Discr := First_Elmt (Discriminant_Constraint (Full_Type));
            while Present (Discr) loop
               Append_To (Expressions (Aggr), New_Copy (Node (Discr)));
               Next_Elmt (Discr);
            end loop;

            --  Now collect values of initialized components

            Comp := First_Component (Full_Type);
            while Present (Comp) loop
               if Ekind (Comp) = E_Component
                 and then Present (Expression (Parent (Comp)))
               then
                  Append_To (Component_Associations (Aggr),
                    Make_Component_Association (Loc,
                      Choices    => New_List (New_Occurrence_Of (Comp, Loc)),
                      Expression => New_Copy_Tree
                                      (Expression (Parent (Comp)))));
               end if;

               Next_Component (Comp);
            end loop;

            --  Finally, box-initialize remaining components

            Append_To (Component_Associations (Aggr),
              Make_Component_Association (Loc,
                Choices    => New_List (Make_Others_Choice (Loc)),
                Expression => Empty));
            Set_Box_Present (Last (Component_Associations (Aggr)));
            Set_Expression (N, Aggr);

            if Typ /= Full_Type then
               Analyze_And_Resolve (Aggr, Full_View (Base_Type (Full_Type)));
               Rewrite (Aggr, Unchecked_Convert_To (Typ, Aggr));
               Analyze_And_Resolve (Aggr, Typ);
            else
               Analyze_And_Resolve (Aggr, Full_Type);
            end if;

            return True;

         else
            return False;
         end if;
      end Build_Equivalent_Aggregate;

      -------------------------
      -- Rewrite_As_Renaming --
      -------------------------

      function Rewrite_As_Renaming return Boolean is
      begin
         return not Aliased_Present (N)
           and then Is_Entity_Name (Expr_Q)
           and then Ekind (Entity (Expr_Q)) = E_Variable
           and then OK_To_Rename (Entity (Expr_Q))
           and then Is_Entity_Name (Object_Definition (N));
      end Rewrite_As_Renaming;

   --  Start of processing for Expand_N_Object_Declaration

   begin
      --  Don't do anything for deferred constants. All proper actions will be
      --  expanded during the full declaration.

      if No (Expr) and Constant_Present (N) then
         return;
      end if;

      --  First we do special processing for objects of a tagged type where
      --  this is the point at which the type is frozen. The creation of the
      --  dispatch table and the initialization procedure have to be deferred
      --  to this point, since we reference previously declared primitive
      --  subprograms.

      --  Force construction of dispatch tables of library level tagged types

      if Tagged_Type_Expansion
        and then Static_Dispatch_Tables
        and then Is_Library_Level_Entity (Def_Id)
        and then Is_Library_Level_Tagged_Type (Base_Typ)
        and then (Ekind (Base_Typ) = E_Record_Type
                    or else Ekind (Base_Typ) = E_Protected_Type
                    or else Ekind (Base_Typ) = E_Task_Type)
        and then not Has_Dispatch_Table (Base_Typ)
      then
         declare
            New_Nodes : List_Id := No_List;

         begin
            if Is_Concurrent_Type (Base_Typ) then
               New_Nodes := Make_DT (Corresponding_Record_Type (Base_Typ), N);
            else
               New_Nodes := Make_DT (Base_Typ, N);
            end if;

            if not Is_Empty_List (New_Nodes) then
               Insert_List_Before (N, New_Nodes);
            end if;
         end;
      end if;

      --  Make shared memory routines for shared passive variable

      if Is_Shared_Passive (Def_Id) then
         Init_After := Make_Shared_Var_Procs (N);
      end if;

      --  If tasks being declared, make sure we have an activation chain
      --  defined for the tasks (has no effect if we already have one), and
      --  also that a Master variable is established and that the appropriate
      --  enclosing construct is established as a task master.

      if Has_Task (Typ) then
         Build_Activation_Chain_Entity (N);
         Build_Master_Entity (Def_Id);
      end if;

      --  Default initialization required, and no expression present

      if No (Expr) then

         --  For the default initialization case, if we have a private type
         --  with invariants, and invariant checks are enabled, then insert an
         --  invariant check after the object declaration. Note that it is OK
         --  to clobber the object with an invalid value since if the exception
         --  is raised, then the object will go out of scope. In the case where
         --  an array object is initialized with an aggregate, the expression
         --  is removed. Check flag Has_Init_Expression to avoid generating a
         --  junk invariant check.

         if Has_Invariants (Base_Typ)
           and then Present (Invariant_Procedure (Base_Typ))
           and then not Has_Init_Expression (N)
         then
            Insert_After (N,
              Make_Invariant_Call (New_Occurrence_Of (Def_Id, Loc)));
         end if;

         --  Expand Initialize call for controlled objects. One may wonder why
         --  the Initialize Call is not done in the regular Init procedure
         --  attached to the record type. That's because the init procedure is
         --  recursively called on each component, including _Parent, thus the
         --  Init call for a controlled object would generate not only one
         --  Initialize call as it is required but one for each ancestor of
         --  its type. This processing is suppressed if No_Initialization set.

         if not Needs_Finalization (Typ) or else No_Initialization (N) then
            null;

         elsif not Abort_Allowed or else not Comes_From_Source (N) then
            Insert_Action_After (Init_After,
              Make_Init_Call
                (Obj_Ref => New_Occurrence_Of (Def_Id, Loc),
                 Typ     => Base_Typ));

         --  Abort allowed

         else
            --  We need to protect the initialize call

            --  begin
            --     Defer_Abort.all;
            --     Initialize (...);
            --  at end
            --     Undefer_Abort.all;
            --  end;

            --  ??? this won't protect the initialize call for controlled
            --  components which are part of the init proc, so this block
            --  should probably also contain the call to _init_proc but this
            --  requires some code reorganization...

            declare
               L   : constant List_Id := New_List (
                       Make_Init_Call
                         (Obj_Ref => New_Occurrence_Of (Def_Id, Loc),
                          Typ     => Base_Typ));

               Blk : constant Node_Id :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc, L));

            begin
               Prepend_To (L, Build_Runtime_Call (Loc, RE_Abort_Defer));
               Set_At_End_Proc (Handled_Statement_Sequence (Blk),
                 New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc));
               Insert_Actions_After (Init_After, New_List (Blk));
               Expand_At_End_Handler
                 (Handled_Statement_Sequence (Blk), Entity (Identifier (Blk)));
            end;
         end if;

         --  Call type initialization procedure if there is one. We build the
         --  call and put it immediately after the object declaration, so that
         --  it will be expanded in the usual manner. Note that this will
         --  result in proper handling of defaulted discriminants.

         --  Need call if there is a base init proc

         if Has_Non_Null_Base_Init_Proc (Typ)

            --  Suppress call if No_Initialization set on declaration

            and then not No_Initialization (N)

            --  Suppress call for special case of value type for VM

            and then not Is_Value_Type (Typ)

            --  Suppress call if initialization suppressed for the type

            and then not Initialization_Suppressed (Typ)
         then
            --  Return without initializing when No_Default_Initialization
            --  applies. Note that the actual restriction check occurs later,
            --  when the object is frozen, because we don't know yet whether
            --  the object is imported, which is a case where the check does
            --  not apply.

            if Restriction_Active (No_Default_Initialization) then
               return;
            end if;

            --  The call to the initialization procedure does NOT freeze the
            --  object being initialized. This is because the call is not a
            --  source level call. This works fine, because the only possible
            --  statements depending on freeze status that can appear after the
            --  Init_Proc call are rep clauses which can safely appear after
            --  actual references to the object. Note that this call may
            --  subsequently be removed (if a pragma Import is encountered),
            --  or moved to the freeze actions for the object (e.g. if an
            --  address clause is applied to the object, causing it to get
            --  delayed freezing).

            Id_Ref := New_Reference_To (Def_Id, Loc);
            Set_Must_Not_Freeze (Id_Ref);
            Set_Assignment_OK (Id_Ref);

            declare
               Init_Expr : constant Node_Id :=
                             Static_Initialization (Base_Init_Proc (Typ));

            begin
               if Present (Init_Expr) then
                  Set_Expression
                    (N, New_Copy_Tree (Init_Expr, New_Scope => Current_Scope));
                  return;

               --  If type has discriminants, try to build equivalent aggregate
               --  using discriminant values from the declaration. This
               --  is a useful optimization, in particular if restriction
               --  No_Elaboration_Code is active.

               elsif Build_Equivalent_Aggregate then
                  return;

               else
                  Initialization_Warning (Id_Ref);

                  Insert_Actions_After (Init_After,
                    Build_Initialization_Call (Loc, Id_Ref, Typ));
               end if;
            end;

         --  If simple initialization is required, then set an appropriate
         --  simple initialization expression in place. This special
         --  initialization is required even though No_Init_Flag is present,
         --  but is not needed if there was an explicit initialization.

         --  An internally generated temporary needs no initialization because
         --  it will be assigned subsequently. In particular, there is no point
         --  in applying Initialize_Scalars to such a temporary.

         elsif Needs_Simple_Initialization
                 (Typ,
                  Initialize_Scalars
                    and then not Has_Following_Address_Clause (N))
           and then not Is_Internal (Def_Id)
           and then not Has_Init_Expression (N)
         then
            Set_No_Initialization (N, False);
            Set_Expression (N, Get_Simple_Init_Val (Typ, N, Esize (Def_Id)));
            Analyze_And_Resolve (Expression (N), Typ);
         end if;

         --  Generate attribute for Persistent_BSS if needed

         if Persistent_BSS_Mode
           and then Comes_From_Source (N)
           and then Is_Potentially_Persistent_Type (Typ)
           and then not Has_Init_Expression (N)
           and then Is_Library_Level_Entity (Def_Id)
         then
            declare
               Prag : Node_Id;
            begin
               Prag :=
                 Make_Linker_Section_Pragma
                   (Def_Id, Sloc (N), ".persistent.bss");
               Insert_After (N, Prag);
               Analyze (Prag);
            end;
         end if;

         --  If access type, then we know it is null if not initialized

         if Is_Access_Type (Typ) then
            Set_Is_Known_Null (Def_Id);
         end if;

      --  Explicit initialization present

      else
         --  Obtain actual expression from qualified expression

         if Nkind (Expr) = N_Qualified_Expression then
            Expr_Q := Expression (Expr);
         else
            Expr_Q := Expr;
         end if;

         --  When we have the appropriate type of aggregate in the expression
         --  (it has been determined during analysis of the aggregate by
         --  setting the delay flag), let's perform in place assignment and
         --  thus avoid creating a temporary.

         if Is_Delayed_Aggregate (Expr_Q) then
            Convert_Aggr_In_Object_Decl (N);

         --  Ada 2005 (AI-318-02): If the initialization expression is a call
         --  to a build-in-place function, then access to the declared object
         --  must be passed to the function. Currently we limit such functions
         --  to those with constrained limited result subtypes, but eventually
         --  plan to expand the allowed forms of functions that are treated as
         --  build-in-place.

         elsif Ada_Version >= Ada_2005
           and then Is_Build_In_Place_Function_Call (Expr_Q)
         then
            Make_Build_In_Place_Call_In_Object_Declaration (N, Expr_Q);

            --  The previous call expands the expression initializing the
            --  built-in-place object into further code that will be analyzed
            --  later. No further expansion needed here.

            return;

         --  Ada 2005 (AI-251): Rewrite the expression that initializes a
         --  class-wide interface object to ensure that we copy the full
         --  object, unless we are targetting a VM where interfaces are handled
         --  by VM itself. Note that if the root type of Typ is an ancestor of
         --  Expr's type, both types share the same dispatch table and there is
         --  no need to displace the pointer.

         elsif Is_Interface (Typ)

           --  Avoid never-ending recursion because if Equivalent_Type is set
           --  then we've done it already and must not do it again!

           and then not
             (Nkind (Object_Definition (N)) = N_Identifier
               and then
                 Present (Equivalent_Type (Entity (Object_Definition (N)))))
         then
            pragma Assert (Is_Class_Wide_Type (Typ));

            --  If the object is a return object of an inherently limited type,
            --  which implies build-in-place treatment, bypass the special
            --  treatment of class-wide interface initialization below. In this
            --  case, the expansion of the return statement will take care of
            --  creating the object (via allocator) and initializing it.

            if Is_Return_Object (Def_Id)
              and then Is_Immutably_Limited_Type (Typ)
            then
               null;

            elsif Tagged_Type_Expansion then
               declare
                  Iface    : constant Entity_Id := Root_Type (Typ);
                  Expr_N   : Node_Id := Expr;
                  Expr_Typ : Entity_Id;
                  New_Expr : Node_Id;
                  Obj_Id   : Entity_Id;
                  Tag_Comp : Node_Id;

               begin
                  --  If the original node of the expression was a conversion
                  --  to this specific class-wide interface type then restore
                  --  the original node because we must copy the object before
                  --  displacing the pointer to reference the secondary tag
                  --  component. This code must be kept synchronized with the
                  --  expansion done by routine Expand_Interface_Conversion

                  if not Comes_From_Source (Expr_N)
                    and then Nkind (Expr_N) = N_Explicit_Dereference
                    and then Nkind (Original_Node (Expr_N)) = N_Type_Conversion
                    and then Etype (Original_Node (Expr_N)) = Typ
                  then
                     Rewrite (Expr_N, Original_Node (Expression (N)));
                  end if;

                  --  Avoid expansion of redundant interface conversion

                  if Is_Interface (Etype (Expr_N))
                    and then Nkind (Expr_N) = N_Type_Conversion
                    and then Etype (Expr_N) = Typ
                  then
                     Expr_N := Expression (Expr_N);
                     Set_Expression (N, Expr_N);
                  end if;

                  Obj_Id   := Make_Temporary (Loc, 'D', Expr_N);
                  Expr_Typ := Base_Type (Etype (Expr_N));

                  if Is_Class_Wide_Type (Expr_Typ) then
                     Expr_Typ := Root_Type (Expr_Typ);
                  end if;

                  --  Replace
                  --     CW : I'Class := Obj;
                  --  by
                  --     Tmp : T := Obj;
                  --     type Ityp is not null access I'Class;
                  --     CW  : I'Class renames Ityp(Tmp.I_Tag'Address).all;

                  if Comes_From_Source (Expr_N)
                    and then Nkind (Expr_N) = N_Identifier
                    and then not Is_Interface (Expr_Typ)
                    and then Interface_Present_In_Ancestor (Expr_Typ, Typ)
                    and then (Expr_Typ = Etype (Expr_Typ)
                               or else not
                              Is_Variable_Size_Record (Etype (Expr_Typ)))
                  then
                     --  Copy the object

                     Insert_Action (N,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Obj_Id,
                         Object_Definition =>
                           New_Occurrence_Of (Expr_Typ, Loc),
                         Expression =>
                           Relocate_Node (Expr_N)));

                     --  Statically reference the tag associated with the
                     --  interface

                     Tag_Comp :=
                       Make_Selected_Component (Loc,
                         Prefix => New_Occurrence_Of (Obj_Id, Loc),
                         Selector_Name =>
                           New_Reference_To
                             (Find_Interface_Tag (Expr_Typ, Iface), Loc));

                  --  Replace
                  --     IW : I'Class := Obj;
                  --  by
                  --     type Equiv_Record is record ... end record;
                  --     implicit subtype CW is <Class_Wide_Subtype>;
                  --     Tmp : CW := CW!(Obj);
                  --     type Ityp is not null access I'Class;
                  --     IW : I'Class renames
                  --            Ityp!(Displace (Temp'Address, I'Tag)).all;

                  else
                     --  Generate the equivalent record type and update the
                     --  subtype indication to reference it.

                     Expand_Subtype_From_Expr
                       (N             => N,
                        Unc_Type      => Typ,
                        Subtype_Indic => Object_Definition (N),
                        Exp           => Expr_N);

                     if not Is_Interface (Etype (Expr_N)) then
                        New_Expr := Relocate_Node (Expr_N);

                     --  For interface types we use 'Address which displaces
                     --  the pointer to the base of the object (if required)

                     else
                        New_Expr :=
                          Unchecked_Convert_To (Etype (Object_Definition (N)),
                            Make_Explicit_Dereference (Loc,
                              Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                                Make_Attribute_Reference (Loc,
                                  Prefix => Relocate_Node (Expr_N),
                                  Attribute_Name => Name_Address))));
                     end if;

                     --  Copy the object

                     if not Is_Limited_Record (Expr_Typ) then
                        Insert_Action (N,
                          Make_Object_Declaration (Loc,
                            Defining_Identifier => Obj_Id,
                            Object_Definition   =>
                              New_Occurrence_Of
                                (Etype (Object_Definition (N)), Loc),
                            Expression => New_Expr));

                     --  Rename limited type object since they cannot be copied
                     --  This case occurs when the initialization expression
                     --  has been previously expanded into a temporary object.

                     else pragma Assert (not Comes_From_Source (Expr_Q));
                        Insert_Action (N,
                          Make_Object_Renaming_Declaration (Loc,
                            Defining_Identifier => Obj_Id,
                            Subtype_Mark        =>
                              New_Occurrence_Of
                                (Etype (Object_Definition (N)), Loc),
                            Name                =>
                              Unchecked_Convert_To
                                (Etype (Object_Definition (N)), New_Expr)));
                     end if;

                     --  Dynamically reference the tag associated with the
                     --  interface.

                     Tag_Comp :=
                       Make_Function_Call (Loc,
                         Name => New_Reference_To (RTE (RE_Displace), Loc),
                         Parameter_Associations => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix => New_Occurrence_Of (Obj_Id, Loc),
                             Attribute_Name => Name_Address),
                           New_Reference_To
                             (Node (First_Elmt (Access_Disp_Table (Iface))),
                              Loc)));
                  end if;

                  Rewrite (N,
                    Make_Object_Renaming_Declaration (Loc,
                      Defining_Identifier => Make_Temporary (Loc, 'D'),
                      Subtype_Mark        => New_Occurrence_Of (Typ, Loc),
                      Name => Convert_Tag_To_Interface (Typ, Tag_Comp)));

                  --  If the original entity comes from source, then mark the
                  --  new entity as needing debug information, even though it's
                  --  defined by a generated renaming that does not come from
                  --  source, so that Materialize_Entity will be set on the
                  --  entity when Debug_Renaming_Declaration is called during
                  --  analysis.

                  if Comes_From_Source (Def_Id) then
                     Set_Debug_Info_Needed (Defining_Identifier (N));
                  end if;

                  Analyze (N, Suppress => All_Checks);

                  --  Replace internal identifier of rewritten node by the
                  --  identifier found in the sources. We also have to exchange
                  --  entities containing their defining identifiers to ensure
                  --  the correct replacement of the object declaration by this
                  --  object renaming declaration because these identifiers
                  --  were previously added by Enter_Name to the current scope.
                  --  We must preserve the homonym chain of the source entity
                  --  as well. We must also preserve the kind of the entity,
                  --  which may be a constant. Preserve entity chain because
                  --  itypes may have been generated already, and the full
                  --  chain must be preserved for final freezing. Finally,
                  --  preserve Comes_From_Source setting, so that debugging
                  --  and cross-referencing information is properly kept.

                  declare
                     New_Id    : constant Entity_Id := Defining_Identifier (N);
                     Next_Temp : constant Entity_Id := Next_Entity (New_Id);
                     S_Flag    : constant Boolean   :=
                                   Comes_From_Source (Def_Id);

                  begin
                     Set_Next_Entity (New_Id, Next_Entity (Def_Id));
                     Set_Next_Entity (Def_Id, Next_Temp);

                     Set_Chars   (Defining_Identifier (N), Chars   (Def_Id));
                     Set_Homonym (Defining_Identifier (N), Homonym (Def_Id));
                     Set_Ekind   (Defining_Identifier (N), Ekind   (Def_Id));

                     Set_Comes_From_Source (Def_Id, False);
                     Exchange_Entities (Defining_Identifier (N), Def_Id);
                     Set_Comes_From_Source (Def_Id, S_Flag);
                  end;
               end;
            end if;

            return;

         --  Common case of explicit object initialization

         else
            --  In most cases, we must check that the initial value meets any
            --  constraint imposed by the declared type. However, there is one
            --  very important exception to this rule. If the entity has an
            --  unconstrained nominal subtype, then it acquired its constraints
            --  from the expression in the first place, and not only does this
            --  mean that the constraint check is not needed, but an attempt to
            --  perform the constraint check can cause order of elaboration
            --  problems.

            if not Is_Constr_Subt_For_U_Nominal (Typ) then

               --  If this is an allocator for an aggregate that has been
               --  allocated in place, delay checks until assignments are
               --  made, because the discriminants are not initialized.

               if Nkind (Expr) = N_Allocator
                 and then No_Initialization (Expr)
               then
                  null;

               --  Otherwise apply a constraint check now if no prev error

               elsif Nkind (Expr) /= N_Error then
                  Apply_Constraint_Check (Expr, Typ);

                  --  If the expression has been marked as requiring a range
                  --  generate it now and reset the flag.

                  if Do_Range_Check (Expr) then
                     Set_Do_Range_Check (Expr, False);

                     if not Suppress_Assignment_Checks (N) then
                        Generate_Range_Check
                          (Expr, Typ, CE_Range_Check_Failed);
                     end if;
                  end if;
               end if;
            end if;

            --  If the type is controlled and not inherently limited, then
            --  the target is adjusted after the copy and attached to the
            --  finalization list. However, no adjustment is done in the case
            --  where the object was initialized by a call to a function whose
            --  result is built in place, since no copy occurred. (Eventually
            --  we plan to support in-place function results for some cases
            --  of nonlimited types. ???) Similarly, no adjustment is required
            --  if we are going to rewrite the object declaration into a
            --  renaming declaration.

            if Needs_Finalization (Typ)
              and then not Is_Immutably_Limited_Type (Typ)
              and then not Rewrite_As_Renaming
            then
               Insert_Action_After (Init_After,
                 Make_Adjust_Call (
                   Obj_Ref => New_Reference_To (Def_Id, Loc),
                   Typ     => Base_Typ));
            end if;

            --  For tagged types, when an init value is given, the tag has to
            --  be re-initialized separately in order to avoid the propagation
            --  of a wrong tag coming from a view conversion unless the type
            --  is class wide (in this case the tag comes from the init value).
            --  Suppress the tag assignment when VM_Target because VM tags are
            --  represented implicitly in objects. Ditto for types that are
            --  CPP_CLASS, and for initializations that are aggregates, because
            --  they have to have the right tag.

            if Is_Tagged_Type (Typ)
              and then not Is_Class_Wide_Type (Typ)
              and then not Is_CPP_Class (Typ)
              and then Tagged_Type_Expansion
              and then Nkind (Expr) /= N_Aggregate
              and then (Nkind (Expr) /= N_Qualified_Expression
                         or else Nkind (Expression (Expr)) /= N_Aggregate)
            then
               declare
                  Full_Typ : constant Entity_Id := Underlying_Type (Typ);

               begin
                  --  The re-assignment of the tag has to be done even if the
                  --  object is a constant. The assignment must be analyzed
                  --  after the declaration.

                  New_Ref :=
                    Make_Selected_Component (Loc,
                       Prefix => New_Occurrence_Of (Def_Id, Loc),
                       Selector_Name =>
                         New_Reference_To (First_Tag_Component (Full_Typ),
                                           Loc));
                  Set_Assignment_OK (New_Ref);

                  Insert_Action_After (Init_After,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Ref,
                      Expression =>
                        Unchecked_Convert_To (RTE (RE_Tag),
                          New_Reference_To
                            (Node (First_Elmt (Access_Disp_Table (Full_Typ))),
                             Loc))));
               end;

            --  Handle C++ constructor calls. Note that we do not check that
            --  Typ is a tagged type since the equivalent Ada type of a C++
            --  class that has no virtual methods is a non-tagged limited
            --  record type.

            elsif Is_CPP_Constructor_Call (Expr) then

               --  The call to the initialization procedure does NOT freeze the
               --  object being initialized.

               Id_Ref := New_Reference_To (Def_Id, Loc);
               Set_Must_Not_Freeze (Id_Ref);
               Set_Assignment_OK (Id_Ref);

               Insert_Actions_After (Init_After,
                 Build_Initialization_Call (Loc, Id_Ref, Typ,
                   Constructor_Ref => Expr));

               --  We remove here the original call to the constructor
               --  to avoid its management in the backend

               Set_Expression (N, Empty);
               return;

            --  For discrete types, set the Is_Known_Valid flag if the
            --  initializing value is known to be valid.

            elsif Is_Discrete_Type (Typ) and then Expr_Known_Valid (Expr) then
               Set_Is_Known_Valid (Def_Id);

            elsif Is_Access_Type (Typ) then

               --  For access types set the Is_Known_Non_Null flag if the
               --  initializing value is known to be non-null. We can also set
               --  Can_Never_Be_Null if this is a constant.

               if Known_Non_Null (Expr) then
                  Set_Is_Known_Non_Null (Def_Id, True);

                  if Constant_Present (N) then
                     Set_Can_Never_Be_Null (Def_Id);
                  end if;
               end if;
            end if;

            --  If validity checking on copies, validate initial expression.
            --  But skip this if declaration is for a generic type, since it
            --  makes no sense to validate generic types. Not clear if this
            --  can happen for legal programs, but it definitely can arise
            --  from previous instantiation errors.

            if Validity_Checks_On
              and then Validity_Check_Copies
              and then not Is_Generic_Type (Etype (Def_Id))
            then
               Ensure_Valid (Expr);
               Set_Is_Known_Valid (Def_Id);
            end if;
         end if;

         --  Cases where the back end cannot handle the initialization directly
         --  In such cases, we expand an assignment that will be appropriately
         --  handled by Expand_N_Assignment_Statement.

         --  The exclusion of the unconstrained case is wrong, but for now it
         --  is too much trouble ???

         if (Is_Possibly_Unaligned_Slice (Expr)
               or else (Is_Possibly_Unaligned_Object (Expr)
                          and then not Represented_As_Scalar (Etype (Expr))))
           and then not (Is_Array_Type (Etype (Expr))
                           and then not Is_Constrained (Etype (Expr)))
         then
            declare
               Stat : constant Node_Id :=
                       Make_Assignment_Statement (Loc,
                         Name       => New_Reference_To (Def_Id, Loc),
                         Expression => Relocate_Node (Expr));
            begin
               Set_Expression (N, Empty);
               Set_No_Initialization (N);
               Set_Assignment_OK (Name (Stat));
               Set_No_Ctrl_Actions (Stat);
               Insert_After_And_Analyze (Init_After, Stat);
            end;
         end if;

         --  Final transformation, if the initializing expression is an entity
         --  for a variable with OK_To_Rename set, then we transform:

         --     X : typ := expr;

         --  into

         --     X : typ renames expr

         --  provided that X is not aliased. The aliased case has to be
         --  excluded in general because Expr will not be aliased in general.

         if Rewrite_As_Renaming then
            Rewrite (N,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Defining_Identifier (N),
                Subtype_Mark        => Object_Definition (N),
                Name                => Expr_Q));

            --  We do not analyze this renaming declaration, because all its
            --  components have already been analyzed, and if we were to go
            --  ahead and analyze it, we would in effect be trying to generate
            --  another declaration of X, which won't do!

            Set_Renamed_Object (Defining_Identifier (N), Expr_Q);
            Set_Analyzed (N);

            --  We do need to deal with debug issues for this renaming

            --  First, if entity comes from source, then mark it as needing
            --  debug information, even though it is defined by a generated
            --  renaming that does not come from source.

            if Comes_From_Source (Defining_Identifier (N)) then
               Set_Debug_Info_Needed (Defining_Identifier (N));
            end if;

            --  Now call the routine to generate debug info for the renaming

            declare
               Decl : constant Node_Id := Debug_Renaming_Declaration (N);
            begin
               if Present (Decl) then
                  Insert_Action (N, Decl);
               end if;
            end;
         end if;
      end if;

      if Nkind (N) = N_Object_Declaration
        and then Nkind (Object_Definition (N)) = N_Access_Definition
        and then not Is_Local_Anonymous_Access (Etype (Def_Id))
      then
         --  An Ada 2012 stand-alone object of an anonymous access type

         declare
            Loc : constant Source_Ptr := Sloc (N);

            Level : constant Entity_Id :=
                      Make_Defining_Identifier (Sloc (N),
                        Chars =>
                          New_External_Name (Chars (Def_Id), Suffix => "L"));

            Level_Expr : Node_Id;
            Level_Decl : Node_Id;

         begin
            Set_Ekind (Level, Ekind (Def_Id));
            Set_Etype (Level, Standard_Natural);
            Set_Scope (Level, Scope (Def_Id));

            if No (Expr) then

               --  Set accessibility level of null

               Level_Expr :=
                 Make_Integer_Literal (Loc, Scope_Depth (Standard_Standard));

            else
               Level_Expr := Dynamic_Accessibility_Level (Expr);
            end if;

            Level_Decl := Make_Object_Declaration (Loc,
             Defining_Identifier => Level,
             Object_Definition => New_Occurrence_Of (Standard_Natural, Loc),
             Expression => Level_Expr,
             Constant_Present => Constant_Present (N),
             Has_Init_Expression => True);

            Insert_Action_After (Init_After, Level_Decl);

            Set_Extra_Accessibility (Def_Id, Level);
         end;
      end if;

   --  Exception on library entity not available

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Object_Declaration;

   ---------------------------------
   -- Expand_N_Subtype_Indication --
   ---------------------------------

   --  Add a check on the range of the subtype. The static case is partially
   --  duplicated by Process_Range_Expr_In_Decl in Sem_Ch3, but we still need
   --  to check here for the static case in order to avoid generating
   --  extraneous expanded code. Also deal with validity checking.

   procedure Expand_N_Subtype_Indication (N : Node_Id) is
      Ran : constant Node_Id   := Range_Expression (Constraint (N));
      Typ : constant Entity_Id := Entity (Subtype_Mark (N));

   begin
      if Nkind (Constraint (N)) = N_Range_Constraint then
         Validity_Check_Range (Range_Expression (Constraint (N)));
      end if;

      if Nkind_In (Parent (N), N_Constrained_Array_Definition, N_Slice) then
         Apply_Range_Check (Ran, Typ);
      end if;
   end Expand_N_Subtype_Indication;

   ---------------------------
   -- Expand_N_Variant_Part --
   ---------------------------

   --  Note: this procedure no longer has any effect. It used to be that we
   --  would replace the choices in the last variant by a when others, and
   --  also expanded static predicates in variant choices here, but both of
   --  those activities were being done too early, since we can't check the
   --  choices until the statically predicated subtypes are frozen, which can
   --  happen as late as the free point of the record, and we can't change the
   --  last choice to an others before checking the choices, which is now done
   --  at the freeze point of the record.

   procedure Expand_N_Variant_Part (N : Node_Id) is
   begin
      null;
   end Expand_N_Variant_Part;

   ---------------------------------
   -- Expand_Previous_Access_Type --
   ---------------------------------

   procedure Expand_Previous_Access_Type (Def_Id : Entity_Id) is
      Ptr_Typ : Entity_Id;

   begin
      --  Find all access types in the current scope whose designated type is
      --  Def_Id and build master renamings for them.

      Ptr_Typ := First_Entity (Current_Scope);
      while Present (Ptr_Typ) loop
         if Is_Access_Type (Ptr_Typ)
           and then Designated_Type (Ptr_Typ) = Def_Id
           and then No (Master_Id (Ptr_Typ))
         then
            --  Ensure that the designated type has a master

            Build_Master_Entity (Def_Id);

            --  Private and incomplete types complicate the insertion of master
            --  renamings because the access type may precede the full view of
            --  the designated type. For this reason, the master renamings are
            --  inserted relative to the designated type.

            Build_Master_Renaming (Ptr_Typ, Ins_Nod => Parent (Def_Id));
         end if;

         Next_Entity (Ptr_Typ);
      end loop;
   end Expand_Previous_Access_Type;

   ------------------------
   -- Expand_Tagged_Root --
   ------------------------

   procedure Expand_Tagged_Root (T : Entity_Id) is
      Def       : constant Node_Id := Type_Definition (Parent (T));
      Comp_List : Node_Id;
      Comp_Decl : Node_Id;
      Sloc_N    : Source_Ptr;

   begin
      if Null_Present (Def) then
         Set_Component_List (Def,
           Make_Component_List (Sloc (Def),
             Component_Items => Empty_List,
             Variant_Part => Empty,
             Null_Present => True));
      end if;

      Comp_List := Component_List (Def);

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Sloc_N := Sloc (Comp_List);
      else
         Sloc_N := Sloc (First (Component_Items (Comp_List)));
      end if;

      Comp_Decl :=
        Make_Component_Declaration (Sloc_N,
          Defining_Identifier => First_Tag_Component (T),
          Component_Definition =>
            Make_Component_Definition (Sloc_N,
              Aliased_Present => False,
              Subtype_Indication => New_Reference_To (RTE (RE_Tag), Sloc_N)));

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Set_Component_Items (Comp_List, New_List (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         Insert_Before (First (Component_Items (Comp_List)), Comp_Decl);
      end if;

      --  We don't Analyze the whole expansion because the tag component has
      --  already been analyzed previously. Here we just insure that the tree
      --  is coherent with the semantic decoration

      Find_Type (Subtype_Indication (Component_Definition (Comp_Decl)));

   exception
      when RE_Not_Available =>
         return;
   end Expand_Tagged_Root;

   ----------------------
   -- Clean_Task_Names --
   ----------------------

   procedure Clean_Task_Names
     (Typ     : Entity_Id;
      Proc_Id : Entity_Id)
   is
   begin
      if Has_Task (Typ)
        and then not Restriction_Active (No_Implicit_Heap_Allocations)
        and then not Global_Discard_Names
        and then Tagged_Type_Expansion
      then
         Set_Uses_Sec_Stack (Proc_Id);
      end if;
   end Clean_Task_Names;

   ------------------------------
   -- Expand_Freeze_Array_Type --
   ------------------------------

   procedure Expand_Freeze_Array_Type (N : Node_Id) is
      Typ      : constant Entity_Id := Entity (N);
      Comp_Typ : constant Entity_Id := Component_Type (Typ);
      Base     : constant Entity_Id := Base_Type (Typ);

   begin
      if not Is_Bit_Packed_Array (Typ) then

         --  If the component contains tasks, so does the array type. This may
         --  not be indicated in the array type because the component may have
         --  been a private type at the point of definition. Same if component
         --  type is controlled.

         Set_Has_Task (Base, Has_Task (Comp_Typ));
         Set_Has_Controlled_Component (Base,
           Has_Controlled_Component (Comp_Typ)
             or else Is_Controlled (Comp_Typ));

         if No (Init_Proc (Base)) then

            --  If this is an anonymous array created for a declaration with
            --  an initial value, its init_proc will never be called. The
            --  initial value itself may have been expanded into assignments,
            --  in which case the object declaration is carries the
            --  No_Initialization flag.

            if Is_Itype (Base)
              and then Nkind (Associated_Node_For_Itype (Base)) =
                                                    N_Object_Declaration
              and then (Present (Expression (Associated_Node_For_Itype (Base)))
                          or else
                        No_Initialization (Associated_Node_For_Itype (Base)))
            then
               null;

            --  We do not need an init proc for string or wide [wide] string,
            --  since the only time these need initialization in normalize or
            --  initialize scalars mode, and these types are treated specially
            --  and do not need initialization procedures.

            elsif Root_Type (Base) = Standard_String
              or else Root_Type (Base) = Standard_Wide_String
              or else Root_Type (Base) = Standard_Wide_Wide_String
            then
               null;

            --  Otherwise we have to build an init proc for the subtype

            else
               Build_Array_Init_Proc (Base, N);
            end if;
         end if;

         if Typ = Base then
            if Has_Controlled_Component (Base) then
               Build_Controlling_Procs (Base);

               if not Is_Limited_Type (Comp_Typ)
                 and then Number_Dimensions (Typ) = 1
               then
                  Build_Slice_Assignment (Typ);
               end if;
            end if;

            --  Create a finalization master to service the anonymous access
            --  components of the array.

            if Ekind (Comp_Typ) = E_Anonymous_Access_Type
              and then Needs_Finalization (Designated_Type (Comp_Typ))
            then
               Build_Finalization_Master
                 (Typ        => Comp_Typ,
                  Ins_Node   => Parent (Typ),
                  Encl_Scope => Scope (Typ));
            end if;
         end if;

      --  For packed case, default initialization, except if the component type
      --  is itself a packed structure with an initialization procedure, or
      --  initialize/normalize scalars active, and we have a base type, or the
      --  type is public, because in that case a client might specify
      --  Normalize_Scalars and there better be a public Init_Proc for it.

      elsif (Present (Init_Proc (Component_Type (Base)))
               and then No (Base_Init_Proc (Base)))
        or else (Init_Or_Norm_Scalars and then Base = Typ)
        or else Is_Public (Typ)
      then
         Build_Array_Init_Proc (Base, N);
      end if;

      if Has_Invariants (Component_Type (Base))
        and then In_Open_Scopes (Scope (Component_Type (Base)))
      then
         --  Generate component invariant checking procedure. This is only
         --  relevant if the array type is within the scope of the component
         --  type. Otherwise an array object can only be built using the public
         --  subprograms for the component type, and calls to those will have
         --  invariant checks.

         Insert_Component_Invariant_Checks
           (N, Base, Build_Array_Invariant_Proc (Base, N));
      end if;
   end Expand_Freeze_Array_Type;

   -----------------------------------
   -- Expand_Freeze_Class_Wide_Type --
   -----------------------------------

   procedure Expand_Freeze_Class_Wide_Type (N : Node_Id) is
      Typ  : constant Entity_Id := Entity (N);
      Root : constant Entity_Id := Root_Type (Typ);

      function Is_C_Derivation (Typ : Entity_Id) return Boolean;
      --  Given a type, determine whether it is derived from a C or C++ root

      ---------------------
      -- Is_C_Derivation --
      ---------------------

      function Is_C_Derivation (Typ : Entity_Id) return Boolean is
         T : Entity_Id := Typ;

      begin
         loop
            if Is_CPP_Class (T)
              or else Convention (T) = Convention_C
              or else Convention (T) = Convention_CPP
            then
               return True;
            end if;

            exit when T = Etype (T);

            T := Etype (T);
         end loop;

         return False;
      end Is_C_Derivation;

   --  Start of processing for Expand_Freeze_Class_Wide_Type

   begin
      --  Certain run-time configurations and targets do not provide support
      --  for controlled types.

      if Restriction_Active (No_Finalization) then
         return;

      --  Do not create TSS routine Finalize_Address when dispatching calls are
      --  disabled since the core of the routine is a dispatching call.

      elsif Restriction_Active (No_Dispatching_Calls) then
         return;

      --  Do not create TSS routine Finalize_Address for concurrent class-wide
      --  types. Ignore C, C++, CIL and Java types since it is assumed that the
      --  non-Ada side will handle their destruction.

      elsif Is_Concurrent_Type (Root)
        or else Is_C_Derivation (Root)
        or else Convention (Typ) = Convention_CIL
        or else Convention (Typ) = Convention_CPP
        or else Convention (Typ) = Convention_Java
      then
         return;

      --  Do not create TSS routine Finalize_Address for .NET/JVM because these
      --  targets do not support address arithmetic and unchecked conversions.

      elsif VM_Target /= No_VM then
         return;

      --  Do not create TSS routine Finalize_Address when compiling in CodePeer
      --  mode since the routine contains an Unchecked_Conversion.

      elsif CodePeer_Mode then
         return;
      end if;

      --  Create the body of TSS primitive Finalize_Address. This automatically
      --  sets the TSS entry for the class-wide type.

      Make_Finalize_Address_Body (Typ);
   end Expand_Freeze_Class_Wide_Type;

   ------------------------------------
   -- Expand_Freeze_Enumeration_Type --
   ------------------------------------

   procedure Expand_Freeze_Enumeration_Type (N : Node_Id) is
      Typ           : constant Entity_Id  := Entity (N);
      Loc           : constant Source_Ptr := Sloc (Typ);
      Ent           : Entity_Id;
      Lst           : List_Id;
      Num           : Nat;
      Arr           : Entity_Id;
      Fent          : Entity_Id;
      Ityp          : Entity_Id;
      Is_Contiguous : Boolean;
      Pos_Expr      : Node_Id;
      Last_Repval   : Uint;

      Func : Entity_Id;
      pragma Warnings (Off, Func);

   begin
      --  Various optimizations possible if given representation is contiguous

      Is_Contiguous := True;

      Ent := First_Literal (Typ);
      Last_Repval := Enumeration_Rep (Ent);

      Next_Literal (Ent);
      while Present (Ent) loop
         if Enumeration_Rep (Ent) - Last_Repval /= 1 then
            Is_Contiguous := False;
            exit;
         else
            Last_Repval := Enumeration_Rep (Ent);
         end if;

         Next_Literal (Ent);
      end loop;

      if Is_Contiguous then
         Set_Has_Contiguous_Rep (Typ);
         Ent := First_Literal (Typ);
         Num := 1;
         Lst := New_List (New_Reference_To (Ent, Sloc (Ent)));

      else
         --  Build list of literal references

         Lst := New_List;
         Num := 0;

         Ent := First_Literal (Typ);
         while Present (Ent) loop
            Append_To (Lst, New_Reference_To (Ent, Sloc (Ent)));
            Num := Num + 1;
            Next_Literal (Ent);
         end loop;
      end if;

      --  Now build an array declaration

      --    typA : array (Natural range 0 .. num - 1) of ctype :=
      --             (v, v, v, v, v, ....)

      --  where ctype is the corresponding integer type. If the representation
      --  is contiguous, we only keep the first literal, which provides the
      --  offset for Pos_To_Rep computations.

      Arr :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (Typ), 'A'));

      Append_Freeze_Action (Typ,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Arr,
          Constant_Present    => True,

          Object_Definition   =>
            Make_Constrained_Array_Definition (Loc,
              Discrete_Subtype_Definitions => New_List (
                Make_Subtype_Indication (Loc,
                  Subtype_Mark => New_Reference_To (Standard_Natural, Loc),
                  Constraint =>
                    Make_Range_Constraint (Loc,
                      Range_Expression =>
                        Make_Range (Loc,
                          Low_Bound  =>
                            Make_Integer_Literal (Loc, 0),
                          High_Bound =>
                            Make_Integer_Literal (Loc, Num - 1))))),

              Component_Definition =>
                Make_Component_Definition (Loc,
                  Aliased_Present => False,
                  Subtype_Indication => New_Reference_To (Typ, Loc))),

          Expression =>
            Make_Aggregate (Loc,
              Expressions => Lst)));

      Set_Enum_Pos_To_Rep (Typ, Arr);

      --  Now we build the function that converts representation values to
      --  position values. This function has the form:

      --    function _Rep_To_Pos (A : etype; F : Boolean) return Integer is
      --    begin
      --       case ityp!(A) is
      --         when enum-lit'Enum_Rep => return posval;
      --         when enum-lit'Enum_Rep => return posval;
      --         ...
      --         when others   =>
      --           [raise Constraint_Error when F "invalid data"]
      --           return -1;
      --       end case;
      --    end;

      --  Note: the F parameter determines whether the others case (no valid
      --  representation) raises Constraint_Error or returns a unique value
      --  of minus one. The latter case is used, e.g. in 'Valid code.

      --  Note: the reason we use Enum_Rep values in the case here is to avoid
      --  the code generator making inappropriate assumptions about the range
      --  of the values in the case where the value is invalid. ityp is a
      --  signed or unsigned integer type of appropriate width.

      --  Note: if exceptions are not supported, then we suppress the raise
      --  and return -1 unconditionally (this is an erroneous program in any
      --  case and there is no obligation to raise Constraint_Error here!) We
      --  also do this if pragma Restrictions (No_Exceptions) is active.

      --  Is this right??? What about No_Exception_Propagation???

      --  Representations are signed

      if Enumeration_Rep (First_Literal (Typ)) < 0 then

         --  The underlying type is signed. Reset the Is_Unsigned_Type
         --  explicitly, because it might have been inherited from
         --  parent type.

         Set_Is_Unsigned_Type (Typ, False);

         if Esize (Typ) <= Standard_Integer_Size then
            Ityp := Standard_Integer;
         else
            Ityp := Universal_Integer;
         end if;

      --  Representations are unsigned

      else
         if Esize (Typ) <= Standard_Integer_Size then
            Ityp := RTE (RE_Unsigned);
         else
            Ityp := RTE (RE_Long_Long_Unsigned);
         end if;
      end if;

      --  The body of the function is a case statement. First collect case
      --  alternatives, or optimize the contiguous case.

      Lst := New_List;

      --  If representation is contiguous, Pos is computed by subtracting
      --  the representation of the first literal.

      if Is_Contiguous then
         Ent := First_Literal (Typ);

         if Enumeration_Rep (Ent) = Last_Repval then

            --  Another special case: for a single literal, Pos is zero

            Pos_Expr := Make_Integer_Literal (Loc, Uint_0);

         else
            Pos_Expr :=
              Convert_To (Standard_Integer,
                Make_Op_Subtract (Loc,
                  Left_Opnd  =>
                    Unchecked_Convert_To
                     (Ityp, Make_Identifier (Loc, Name_uA)),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc,
                      Intval => Enumeration_Rep (First_Literal (Typ)))));
         end if;

         Append_To (Lst,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => New_List (
                  Make_Range (Sloc (Enumeration_Rep_Expr (Ent)),
                    Low_Bound =>
                      Make_Integer_Literal (Loc,
                       Intval =>  Enumeration_Rep (Ent)),
                    High_Bound =>
                      Make_Integer_Literal (Loc, Intval => Last_Repval))),

                Statements => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression => Pos_Expr))));

      else
         Ent := First_Literal (Typ);
         while Present (Ent) loop
            Append_To (Lst,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => New_List (
                  Make_Integer_Literal (Sloc (Enumeration_Rep_Expr (Ent)),
                    Intval => Enumeration_Rep (Ent))),

                Statements => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression =>
                      Make_Integer_Literal (Loc,
                        Intval => Enumeration_Pos (Ent))))));

            Next_Literal (Ent);
         end loop;
      end if;

      --  In normal mode, add the others clause with the test

      if not No_Exception_Handlers_Set then
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements => New_List (
               Make_Raise_Constraint_Error (Loc,
                 Condition => Make_Identifier (Loc, Name_uF),
                 Reason    => CE_Invalid_Data),
               Make_Simple_Return_Statement (Loc,
                 Expression =>
                   Make_Integer_Literal (Loc, -1)))));

      --  If either of the restrictions No_Exceptions_Handlers/Propagation is
      --  active then return -1 (we cannot usefully raise Constraint_Error in
      --  this case). See description above for further details.

      else
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements => New_List (
               Make_Simple_Return_Statement (Loc,
                 Expression =>
                   Make_Integer_Literal (Loc, -1)))));
      end if;

      --  Now we can build the function body

      Fent :=
        Make_Defining_Identifier (Loc, Make_TSS_Name (Typ, TSS_Rep_To_Pos));

      Func :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Fent,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_uA),
                  Parameter_Type => New_Reference_To (Typ, Loc)),
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_uF),
                  Parameter_Type => New_Reference_To (Standard_Boolean, Loc))),

              Result_Definition => New_Reference_To (Standard_Integer, Loc)),

            Declarations => Empty_List,

            Handled_Statement_Sequence =>
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Case_Statement (Loc,
                    Expression =>
                      Unchecked_Convert_To
                        (Ityp, Make_Identifier (Loc, Name_uA)),
                    Alternatives => Lst))));

      Set_TSS (Typ, Fent);

      --  Set Pure flag (it will be reset if the current context is not Pure).
      --  We also pretend there was a pragma Pure_Function so that for purposes
      --  of optimization and constant-folding, we will consider the function
      --  Pure even if we are not in a Pure context).

      Set_Is_Pure (Fent);
      Set_Has_Pragma_Pure_Function (Fent);

      --  Unless we are in -gnatD mode, where we are debugging generated code,
      --  this is an internal entity for which we don't need debug info.

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Fent);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Freeze_Enumeration_Type;

   -------------------------------
   -- Expand_Freeze_Record_Type --
   -------------------------------

   procedure Expand_Freeze_Record_Type (N : Node_Id) is
      Def_Id      : constant Node_Id := Entity (N);
      Type_Decl   : constant Node_Id := Parent (Def_Id);
      Comp        : Entity_Id;
      Comp_Typ    : Entity_Id;
      Has_AACC    : Boolean;
      Predef_List : List_Id;

      Renamed_Eq : Node_Id := Empty;
      --  Defining unit name for the predefined equality function in the case
      --  where the type has a primitive operation that is a renaming of
      --  predefined equality (but only if there is also an overriding
      --  user-defined equality function). Used to pass this entity from
      --  Make_Predefined_Primitive_Specs to Predefined_Primitive_Bodies.

      Wrapper_Decl_List : List_Id := No_List;
      Wrapper_Body_List : List_Id := No_List;

   --  Start of processing for Expand_Freeze_Record_Type

   begin
      --  Build discriminant checking functions if not a derived type (for
      --  derived types that are not tagged types, always use the discriminant
      --  checking functions of the parent type). However, for untagged types
      --  the derivation may have taken place before the parent was frozen, so
      --  we copy explicitly the discriminant checking functions from the
      --  parent into the components of the derived type.

      if not Is_Derived_Type (Def_Id)
        or else Has_New_Non_Standard_Rep (Def_Id)
        or else Is_Tagged_Type (Def_Id)
      then
         Build_Discr_Checking_Funcs (Type_Decl);

      elsif Is_Derived_Type (Def_Id)
        and then not Is_Tagged_Type (Def_Id)

        --  If we have a derived Unchecked_Union, we do not inherit the
        --  discriminant checking functions from the parent type since the
        --  discriminants are non existent.

        and then not Is_Unchecked_Union (Def_Id)
        and then Has_Discriminants (Def_Id)
      then
         declare
            Old_Comp : Entity_Id;

         begin
            Old_Comp :=
              First_Component (Base_Type (Underlying_Type (Etype (Def_Id))));
            Comp := First_Component (Def_Id);
            while Present (Comp) loop
               if Ekind (Comp) = E_Component
                 and then Chars (Comp) = Chars (Old_Comp)
               then
                  Set_Discriminant_Checking_Func (Comp,
                    Discriminant_Checking_Func (Old_Comp));
               end if;

               Next_Component (Old_Comp);
               Next_Component (Comp);
            end loop;
         end;
      end if;

      if Is_Derived_Type (Def_Id)
        and then Is_Limited_Type (Def_Id)
        and then Is_Tagged_Type (Def_Id)
      then
         Check_Stream_Attributes (Def_Id);
      end if;

      --  Update task and controlled component flags, because some of the
      --  component types may have been private at the point of the record
      --  declaration. Detect anonymous access-to-controlled components.

      Has_AACC := False;

      Comp := First_Component (Def_Id);
      while Present (Comp) loop
         Comp_Typ := Etype (Comp);

         if Has_Task (Comp_Typ) then
            Set_Has_Task (Def_Id);

         --  Do not set Has_Controlled_Component on a class-wide equivalent
         --  type. See Make_CW_Equivalent_Type.

         elsif not Is_Class_Wide_Equivalent_Type (Def_Id)
           and then (Has_Controlled_Component (Comp_Typ)
                      or else (Chars (Comp) /= Name_uParent
                                and then Is_Controlled (Comp_Typ)))
         then
            Set_Has_Controlled_Component (Def_Id);

         --  Non-self-referential anonymous access-to-controlled component

         elsif Ekind (Comp_Typ) = E_Anonymous_Access_Type
           and then Needs_Finalization (Designated_Type (Comp_Typ))
           and then Designated_Type (Comp_Typ) /= Def_Id
         then
            Has_AACC := True;
         end if;

         Next_Component (Comp);
      end loop;

      --  Handle constructors of non-tagged CPP_Class types

      if not Is_Tagged_Type (Def_Id) and then Is_CPP_Class (Def_Id) then
         Set_CPP_Constructors (Def_Id);
      end if;

      --  Creation of the Dispatch Table. Note that a Dispatch Table is built
      --  for regular tagged types as well as for Ada types deriving from a C++
      --  Class, but not for tagged types directly corresponding to C++ classes
      --  In the later case we assume that it is created in the C++ side and we
      --  just use it.

      if Is_Tagged_Type (Def_Id) then

         --  Add the _Tag component

         if Underlying_Type (Etype (Def_Id)) = Def_Id then
            Expand_Tagged_Root (Def_Id);
         end if;

         if Is_CPP_Class (Def_Id) then
            Set_All_DT_Position (Def_Id);

            --  Create the tag entities with a minimum decoration

            if Tagged_Type_Expansion then
               Append_Freeze_Actions (Def_Id, Make_Tags (Def_Id));
            end if;

            Set_CPP_Constructors (Def_Id);

         else
            if not Building_Static_DT (Def_Id) then

               --  Usually inherited primitives are not delayed but the first
               --  Ada extension of a CPP_Class is an exception since the
               --  address of the inherited subprogram has to be inserted in
               --  the new Ada Dispatch Table and this is a freezing action.

               --  Similarly, if this is an inherited operation whose parent is
               --  not frozen yet, it is not in the DT of the parent, and we
               --  generate an explicit freeze node for the inherited operation
               --  so it is properly inserted in the DT of the current type.

               declare
                  Elmt : Elmt_Id;
                  Subp : Entity_Id;

               begin
                  Elmt := First_Elmt (Primitive_Operations (Def_Id));
                  while Present (Elmt) loop
                     Subp := Node (Elmt);

                     if Present (Alias (Subp)) then
                        if Is_CPP_Class (Etype (Def_Id)) then
                           Set_Has_Delayed_Freeze (Subp);

                        elsif Has_Delayed_Freeze (Alias (Subp))
                          and then not Is_Frozen (Alias (Subp))
                        then
                           Set_Is_Frozen (Subp, False);
                           Set_Has_Delayed_Freeze (Subp);
                        end if;
                     end if;

                     Next_Elmt (Elmt);
                  end loop;
               end;
            end if;

            --  Unfreeze momentarily the type to add the predefined primitives
            --  operations. The reason we unfreeze is so that these predefined
            --  operations will indeed end up as primitive operations (which
            --  must be before the freeze point).

            Set_Is_Frozen (Def_Id, False);

            --  Do not add the spec of predefined primitives in case of
            --  CPP tagged type derivations that have convention CPP.

            if Is_CPP_Class (Root_Type (Def_Id))
              and then Convention (Def_Id) = Convention_CPP
            then
               null;

            --  Do not add the spec of predefined primitives in case of
            --  CIL and Java tagged types

            elsif Convention (Def_Id) = Convention_CIL
              or else Convention (Def_Id) = Convention_Java
            then
               null;

            --  Do not add the spec of the predefined primitives if we are
            --  compiling under restriction No_Dispatching_Calls.

            elsif not Restriction_Active (No_Dispatching_Calls) then
               Make_Predefined_Primitive_Specs
                 (Def_Id, Predef_List, Renamed_Eq);
               Insert_List_Before_And_Analyze (N, Predef_List);
            end if;

            --  Ada 2005 (AI-391): For a nonabstract null extension, create
            --  wrapper functions for each nonoverridden inherited function
            --  with a controlling result of the type. The wrapper for such
            --  a function returns an extension aggregate that invokes the
            --  parent function.

            if Ada_Version >= Ada_2005
              and then not Is_Abstract_Type (Def_Id)
              and then Is_Null_Extension (Def_Id)
            then
               Make_Controlling_Function_Wrappers
                 (Def_Id, Wrapper_Decl_List, Wrapper_Body_List);
               Insert_List_Before_And_Analyze (N, Wrapper_Decl_List);
            end if;

            --  Ada 2005 (AI-251): For a nonabstract type extension, build
            --  null procedure declarations for each set of homographic null
            --  procedures that are inherited from interface types but not
            --  overridden. This is done to ensure that the dispatch table
            --  entry associated with such null primitives are properly filled.

            if Ada_Version >= Ada_2005
              and then Etype (Def_Id) /= Def_Id
              and then not Is_Abstract_Type (Def_Id)
              and then Has_Interfaces (Def_Id)
            then
               Insert_Actions (N, Make_Null_Procedure_Specs (Def_Id));
            end if;

            Set_Is_Frozen (Def_Id);
            if not Is_Derived_Type (Def_Id)
              or else Is_Tagged_Type (Etype (Def_Id))
            then
               Set_All_DT_Position (Def_Id);
            end if;

            --  Create and decorate the tags. Suppress their creation when
            --  VM_Target because the dispatching mechanism is handled
            --  internally by the VMs.

            if Tagged_Type_Expansion then
               Append_Freeze_Actions (Def_Id, Make_Tags (Def_Id));

               --  Generate dispatch table of locally defined tagged type.
               --  Dispatch tables of library level tagged types are built
               --  later (see Analyze_Declarations).

               if not Building_Static_DT (Def_Id) then
                  Append_Freeze_Actions (Def_Id, Make_DT (Def_Id));
               end if;

            elsif VM_Target /= No_VM then
               Append_Freeze_Actions (Def_Id, Make_VM_TSD (Def_Id));
            end if;

            --  If the type has unknown discriminants, propagate dispatching
            --  information to its underlying record view, which does not get
            --  its own dispatch table.

            if Is_Derived_Type (Def_Id)
              and then Has_Unknown_Discriminants (Def_Id)
              and then Present (Underlying_Record_View (Def_Id))
            then
               declare
                  Rep : constant Entity_Id := Underlying_Record_View (Def_Id);
               begin
                  Set_Access_Disp_Table
                    (Rep, Access_Disp_Table       (Def_Id));
                  Set_Dispatch_Table_Wrappers
                    (Rep, Dispatch_Table_Wrappers (Def_Id));
                  Set_Direct_Primitive_Operations
                    (Rep, Direct_Primitive_Operations (Def_Id));
               end;
            end if;

            --  Make sure that the primitives Initialize, Adjust and Finalize
            --  are Frozen before other TSS subprograms. We don't want them
            --  Frozen inside.

            if Is_Controlled (Def_Id) then
               if not Is_Limited_Type (Def_Id) then
                  Append_Freeze_Actions (Def_Id,
                    Freeze_Entity
                      (Find_Prim_Op (Def_Id, Name_Adjust), Def_Id));
               end if;

               Append_Freeze_Actions (Def_Id,
                 Freeze_Entity
                   (Find_Prim_Op (Def_Id, Name_Initialize), Def_Id));

               Append_Freeze_Actions (Def_Id,
                 Freeze_Entity
                   (Find_Prim_Op (Def_Id, Name_Finalize), Def_Id));
            end if;

            --  Freeze rest of primitive operations. There is no need to handle
            --  the predefined primitives if we are compiling under restriction
            --  No_Dispatching_Calls.

            if not Restriction_Active (No_Dispatching_Calls) then
               Append_Freeze_Actions
                 (Def_Id, Predefined_Primitive_Freeze (Def_Id));
            end if;
         end if;

      --  In the non-tagged case, ever since Ada 83 an equality function must
      --  be  provided for variant records that are not unchecked unions.
      --  In Ada 2012 the equality function composes, and thus must be built
      --  explicitly just as for tagged records.

      elsif Has_Discriminants (Def_Id)
        and then not Is_Limited_Type (Def_Id)
      then
         declare
            Comps : constant Node_Id :=
                      Component_List (Type_Definition (Type_Decl));
         begin
            if Present (Comps)
              and then Present (Variant_Part (Comps))
            then
               Build_Variant_Record_Equality (Def_Id);
            end if;
         end;

      --  Otherwise create primitive equality operation (AI05-0123)

      --  This is done unconditionally to ensure that tools can be linked
      --  properly with user programs compiled with older language versions.
      --  In addition, this is needed because "=" composes for bounded strings
      --  in all language versions (see Exp_Ch4.Expand_Composite_Equality).

      elsif Comes_From_Source (Def_Id)
        and then Convention (Def_Id) = Convention_Ada
        and then not Is_Limited_Type (Def_Id)
      then
         Build_Untagged_Equality (Def_Id);
      end if;

      --  Before building the record initialization procedure, if we are
      --  dealing with a concurrent record value type, then we must go through
      --  the discriminants, exchanging discriminals between the concurrent
      --  type and the concurrent record value type. See the section "Handling
      --  of Discriminants" in the Einfo spec for details.

      if Is_Concurrent_Record_Type (Def_Id)
        and then Has_Discriminants (Def_Id)
      then
         declare
            Ctyp       : constant Entity_Id :=
                           Corresponding_Concurrent_Type (Def_Id);
            Conc_Discr : Entity_Id;
            Rec_Discr  : Entity_Id;
            Temp       : Entity_Id;

         begin
            Conc_Discr := First_Discriminant (Ctyp);
            Rec_Discr  := First_Discriminant (Def_Id);
            while Present (Conc_Discr) loop
               Temp := Discriminal (Conc_Discr);
               Set_Discriminal (Conc_Discr, Discriminal (Rec_Discr));
               Set_Discriminal (Rec_Discr, Temp);

               Set_Discriminal_Link (Discriminal (Conc_Discr), Conc_Discr);
               Set_Discriminal_Link (Discriminal (Rec_Discr),  Rec_Discr);

               Next_Discriminant (Conc_Discr);
               Next_Discriminant (Rec_Discr);
            end loop;
         end;
      end if;

      if Has_Controlled_Component (Def_Id) then
         Build_Controlling_Procs (Def_Id);
      end if;

      Adjust_Discriminants (Def_Id);

      if Tagged_Type_Expansion or else not Is_Interface (Def_Id) then

         --  Do not need init for interfaces on e.g. CIL since they're
         --  abstract. Helps operation of peverify (the PE Verify tool).

         Build_Record_Init_Proc (Type_Decl, Def_Id);
      end if;

      --  For tagged type that are not interfaces, build bodies of primitive
      --  operations. Note: do this after building the record initialization
      --  procedure, since the primitive operations may need the initialization
      --  routine. There is no need to add predefined primitives of interfaces
      --  because all their predefined primitives are abstract.

      if Is_Tagged_Type (Def_Id)
        and then not Is_Interface (Def_Id)
      then
         --  Do not add the body of predefined primitives in case of
         --  CPP tagged type derivations that have convention CPP.

         if Is_CPP_Class (Root_Type (Def_Id))
           and then Convention (Def_Id) = Convention_CPP
         then
            null;

         --  Do not add the body of predefined primitives in case of
         --  CIL and Java tagged types.

         elsif Convention (Def_Id) = Convention_CIL
           or else Convention (Def_Id) = Convention_Java
         then
            null;

         --  Do not add the body of the predefined primitives if we are
         --  compiling under restriction No_Dispatching_Calls or if we are
         --  compiling a CPP tagged type.

         elsif not Restriction_Active (No_Dispatching_Calls) then

            --  Create the body of TSS primitive Finalize_Address. This must
            --  be done before the bodies of all predefined primitives are
            --  created. If Def_Id is limited, Stream_Input and Stream_Read
            --  may produce build-in-place allocations and for those the
            --  expander needs Finalize_Address.

            Make_Finalize_Address_Body (Def_Id);
            Predef_List := Predefined_Primitive_Bodies (Def_Id, Renamed_Eq);
            Append_Freeze_Actions (Def_Id, Predef_List);
         end if;

         --  Ada 2005 (AI-391): If any wrappers were created for nonoverridden
         --  inherited functions, then add their bodies to the freeze actions.

         if Present (Wrapper_Body_List) then
            Append_Freeze_Actions (Def_Id, Wrapper_Body_List);
         end if;

         --  Create extra formals for the primitive operations of the type.
         --  This must be done before analyzing the body of the initialization
         --  procedure, because a self-referential type might call one of these
         --  primitives in the body of the init_proc itself.

         declare
            Elmt : Elmt_Id;
            Subp : Entity_Id;

         begin
            Elmt := First_Elmt (Primitive_Operations (Def_Id));
            while Present (Elmt) loop
               Subp := Node (Elmt);
               if not Has_Foreign_Convention (Subp)
                 and then not Is_Predefined_Dispatching_Operation (Subp)
               then
                  Create_Extra_Formals (Subp);
               end if;

               Next_Elmt (Elmt);
            end loop;
         end;
      end if;

      --  Create a heterogeneous finalization master to service the anonymous
      --  access-to-controlled components of the record type.

      if Has_AACC then
         declare
            Encl_Scope : constant Entity_Id  := Scope (Def_Id);
            Ins_Node   : constant Node_Id    := Parent (Def_Id);
            Loc        : constant Source_Ptr := Sloc (Def_Id);
            Fin_Mas_Id : Entity_Id;

            Attributes_Set : Boolean := False;
            Master_Built   : Boolean := False;
            --  Two flags which control the creation and initialization of a
            --  common heterogeneous master.

         begin
            Comp := First_Component (Def_Id);
            while Present (Comp) loop
               Comp_Typ := Etype (Comp);

               --  A non-self-referential anonymous access-to-controlled
               --  component.

               if Ekind (Comp_Typ) = E_Anonymous_Access_Type
                 and then Needs_Finalization (Designated_Type (Comp_Typ))
                 and then Designated_Type (Comp_Typ) /= Def_Id
               then
                  if VM_Target = No_VM then

                     --  Build a homogeneous master for the first anonymous
                     --  access-to-controlled component. This master may be
                     --  converted into a heterogeneous collection if more
                     --  components are to follow.

                     if not Master_Built then
                        Master_Built := True;

                        --  All anonymous access-to-controlled types allocate
                        --  on the global pool.

                        Set_Associated_Storage_Pool (Comp_Typ,
                          Get_Global_Pool_For_Access_Type (Comp_Typ));

                        Build_Finalization_Master
                          (Typ        => Comp_Typ,
                           Ins_Node   => Ins_Node,
                           Encl_Scope => Encl_Scope);

                        Fin_Mas_Id := Finalization_Master (Comp_Typ);

                     --  Subsequent anonymous access-to-controlled components
                     --  reuse the already available master.

                     else
                        --  All anonymous access-to-controlled types allocate
                        --  on the global pool.

                        Set_Associated_Storage_Pool (Comp_Typ,
                          Get_Global_Pool_For_Access_Type (Comp_Typ));

                        --  Shared the master among multiple components

                        Set_Finalization_Master (Comp_Typ, Fin_Mas_Id);

                        --  Convert the master into a heterogeneous collection.
                        --  Generate:
                        --
                        --    Set_Is_Heterogeneous (<Fin_Mas_Id>);

                        if not Attributes_Set then
                           Attributes_Set := True;

                           Insert_Action (Ins_Node,
                             Make_Procedure_Call_Statement (Loc,
                               Name =>
                                 New_Reference_To
                                   (RTE (RE_Set_Is_Heterogeneous), Loc),
                               Parameter_Associations => New_List (
                                 New_Reference_To (Fin_Mas_Id, Loc))));
                        end if;
                     end if;

                  --  Since .NET/JVM targets do not support heterogeneous
                  --  masters, each component must have its own master.

                  else
                     Build_Finalization_Master
                       (Typ        => Comp_Typ,
                        Ins_Node   => Ins_Node,
                        Encl_Scope => Encl_Scope);
                  end if;
               end if;

               Next_Component (Comp);
            end loop;
         end;
      end if;

      --  Check whether individual components have a defined invariant,
      --  and add the corresponding component invariant checks.

      Insert_Component_Invariant_Checks
        (N, Def_Id, Build_Record_Invariant_Proc (Def_Id, N));
   end Expand_Freeze_Record_Type;

   ------------------------------
   -- Freeze_Stream_Operations --
   ------------------------------

   procedure Freeze_Stream_Operations (N : Node_Id; Typ : Entity_Id) is
      Names     : constant array (1 .. 4) of TSS_Name_Type :=
                    (TSS_Stream_Input,
                     TSS_Stream_Output,
                     TSS_Stream_Read,
                     TSS_Stream_Write);
      Stream_Op : Entity_Id;

   begin
      --  Primitive operations of tagged types are frozen when the dispatch
      --  table is constructed.

      if not Comes_From_Source (Typ)
        or else Is_Tagged_Type (Typ)
      then
         return;
      end if;

      for J in Names'Range loop
         Stream_Op := TSS (Typ, Names (J));

         if Present (Stream_Op)
           and then Is_Subprogram (Stream_Op)
           and then Nkind (Unit_Declaration_Node (Stream_Op)) =
                      N_Subprogram_Declaration
           and then not Is_Frozen (Stream_Op)
         then
            Append_Freeze_Actions (Typ, Freeze_Entity (Stream_Op, N));
         end if;
      end loop;
   end Freeze_Stream_Operations;

   -----------------
   -- Freeze_Type --
   -----------------

   --  Full type declarations are expanded at the point at which the type is
   --  frozen. The formal N is the Freeze_Node for the type. Any statements or
   --  declarations generated by the freezing (e.g. the procedure generated
   --  for initialization) are chained in the Actions field list of the freeze
   --  node using Append_Freeze_Actions.

   function Freeze_Type (N : Node_Id) return Boolean is
      Def_Id    : constant Entity_Id := Entity (N);
      RACW_Seen : Boolean := False;
      Result    : Boolean := False;

   begin
      --  Process associated access types needing special processing

      if Present (Access_Types_To_Process (N)) then
         declare
            E : Elmt_Id := First_Elmt (Access_Types_To_Process (N));
         begin
            while Present (E) loop

               if Is_Remote_Access_To_Class_Wide_Type (Node (E)) then
                  Validate_RACW_Primitives (Node (E));
                  RACW_Seen := True;
               end if;

               E := Next_Elmt (E);
            end loop;
         end;

         if RACW_Seen then

            --  If there are RACWs designating this type, make stubs now

            Remote_Types_Tagged_Full_View_Encountered (Def_Id);
         end if;
      end if;

      --  Freeze processing for record types

      if Is_Record_Type (Def_Id) then
         if Ekind (Def_Id) = E_Record_Type then
            Expand_Freeze_Record_Type (N);

         elsif Is_Class_Wide_Type (Def_Id) then
            Expand_Freeze_Class_Wide_Type (N);
         end if;

      --  Freeze processing for array types

      elsif Is_Array_Type (Def_Id) then
         Expand_Freeze_Array_Type (N);

      --  Freeze processing for access types

      --  For pool-specific access types, find out the pool object used for
      --  this type, needs actual expansion of it in some cases. Here are the
      --  different cases :

      --  1. Rep Clause "for Def_Id'Storage_Size use 0;"
      --      ---> don't use any storage pool

      --  2. Rep Clause : for Def_Id'Storage_Size use Expr.
      --     Expand:
      --      Def_Id__Pool : Stack_Bounded_Pool (Expr, DT'Size, DT'Alignment);

      --  3. Rep Clause "for Def_Id'Storage_Pool use a_Pool_Object"
      --      ---> Storage Pool is the specified one

      --  See GNAT Pool packages in the Run-Time for more details

      elsif Ekind_In (Def_Id, E_Access_Type, E_General_Access_Type) then
         declare
            Loc         : constant Source_Ptr := Sloc (N);
            Desig_Type  : constant Entity_Id  := Designated_Type (Def_Id);
            Pool_Object : Entity_Id;

            Freeze_Action_Typ : Entity_Id;

         begin
            --  Case 1

            --    Rep Clause "for Def_Id'Storage_Size use 0;"
            --    ---> don't use any storage pool

            if No_Pool_Assigned (Def_Id) then
               null;

            --  Case 2

            --    Rep Clause : for Def_Id'Storage_Size use Expr.
            --    ---> Expand:
            --           Def_Id__Pool : Stack_Bounded_Pool
            --                            (Expr, DT'Size, DT'Alignment);

            elsif Has_Storage_Size_Clause (Def_Id) then
               declare
                  DT_Size  : Node_Id;
                  DT_Align : Node_Id;

               begin
                  --  For unconstrained composite types we give a size of zero
                  --  so that the pool knows that it needs a special algorithm
                  --  for variable size object allocation.

                  if Is_Composite_Type (Desig_Type)
                    and then not Is_Constrained (Desig_Type)
                  then
                     DT_Size :=
                       Make_Integer_Literal (Loc, 0);

                     DT_Align :=
                       Make_Integer_Literal (Loc, Maximum_Alignment);

                  else
                     DT_Size :=
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Desig_Type, Loc),
                         Attribute_Name => Name_Max_Size_In_Storage_Elements);

                     DT_Align :=
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Desig_Type, Loc),
                         Attribute_Name => Name_Alignment);
                  end if;

                  Pool_Object :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (Def_Id), 'P'));

                  --  We put the code associated with the pools in the entity
                  --  that has the later freeze node, usually the access type
                  --  but it can also be the designated_type; because the pool
                  --  code requires both those types to be frozen

                  if Is_Frozen (Desig_Type)
                    and then (No (Freeze_Node (Desig_Type))
                               or else Analyzed (Freeze_Node (Desig_Type)))
                  then
                     Freeze_Action_Typ := Def_Id;

                  --  A Taft amendment type cannot get the freeze actions
                  --  since the full view is not there.

                  elsif Is_Incomplete_Or_Private_Type (Desig_Type)
                    and then No (Full_View (Desig_Type))
                  then
                     Freeze_Action_Typ := Def_Id;

                  else
                     Freeze_Action_Typ := Desig_Type;
                  end if;

                  Append_Freeze_Action (Freeze_Action_Typ,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Pool_Object,
                      Object_Definition =>
                        Make_Subtype_Indication (Loc,
                          Subtype_Mark =>
                            New_Reference_To
                              (RTE (RE_Stack_Bounded_Pool), Loc),

                          Constraint =>
                            Make_Index_Or_Discriminant_Constraint (Loc,
                              Constraints => New_List (

                              --  First discriminant is the Pool Size

                                New_Reference_To (
                                  Storage_Size_Variable (Def_Id), Loc),

                              --  Second discriminant is the element size

                                DT_Size,

                              --  Third discriminant is the alignment

                                DT_Align)))));
               end;

               Set_Associated_Storage_Pool (Def_Id, Pool_Object);

            --  Case 3

            --    Rep Clause "for Def_Id'Storage_Pool use a_Pool_Object"
            --    ---> Storage Pool is the specified one

            --  When compiling in Ada 2012 mode, ensure that the accessibility
            --  level of the subpool access type is not deeper than that of the
            --  pool_with_subpools.

            elsif Ada_Version >= Ada_2012
              and then Present (Associated_Storage_Pool (Def_Id))

              --  Omit this check on .NET/JVM where pools are not supported

              and then VM_Target = No_VM

              --  Omit this check for the case of a configurable run-time that
              --  does not provide package System.Storage_Pools.Subpools.

              and then RTE_Available (RE_Root_Storage_Pool_With_Subpools)
            then
               declare
                  Loc   : constant Source_Ptr := Sloc (Def_Id);
                  Pool  : constant Entity_Id :=
                            Associated_Storage_Pool (Def_Id);
                  RSPWS : constant Entity_Id :=
                            RTE (RE_Root_Storage_Pool_With_Subpools);

               begin
                  --  It is known that the accessibility level of the access
                  --  type is deeper than that of the pool.

                  if Type_Access_Level (Def_Id) > Object_Access_Level (Pool)
                    and then not Accessibility_Checks_Suppressed (Def_Id)
                    and then not Accessibility_Checks_Suppressed (Pool)
                  then
                     --  Static case: the pool is known to be a descendant of
                     --  Root_Storage_Pool_With_Subpools.

                     if Is_Ancestor (RSPWS, Etype (Pool)) then
                        Error_Msg_N
                          ("??subpool access type has deeper accessibility " &
                           "level than pool", Def_Id);

                        Append_Freeze_Action (Def_Id,
                          Make_Raise_Program_Error (Loc,
                            Reason => PE_Accessibility_Check_Failed));

                     --  Dynamic case: when the pool is of a class-wide type,
                     --  it may or may not support subpools depending on the
                     --  path of derivation. Generate:

                     --    if Def_Id in RSPWS'Class then
                     --       raise Program_Error;
                     --    end if;

                     elsif Is_Class_Wide_Type (Etype (Pool)) then
                        Append_Freeze_Action (Def_Id,
                          Make_If_Statement (Loc,
                            Condition =>
                              Make_In (Loc,
                                Left_Opnd =>
                                  New_Reference_To (Pool, Loc),
                                Right_Opnd =>
                                  New_Reference_To
                                    (Class_Wide_Type (RSPWS), Loc)),

                            Then_Statements => New_List (
                              Make_Raise_Program_Error (Loc,
                                Reason => PE_Accessibility_Check_Failed))));
                     end if;
                  end if;
               end;
            end if;

            --  For access-to-controlled types (including class-wide types and
            --  Taft-amendment types, which potentially have controlled
            --  components), expand the list controller object that will store
            --  the dynamically allocated objects. Don't do this transformation
            --  for expander-generated access types, but do it for types that
            --  are the full view of types derived from other private types.
            --  Also suppress the list controller in the case of a designated
            --  type with convention Java, since this is used when binding to
            --  Java API specs, where there's no equivalent of a finalization
            --  list and we don't want to pull in the finalization support if
            --  not needed.

            if not Comes_From_Source (Def_Id)
              and then not Has_Private_Declaration (Def_Id)
            then
               null;

            --  An exception is made for types defined in the run-time because
            --  Ada.Tags.Tag itself is such a type and cannot afford this
            --  unnecessary overhead that would generates a loop in the
            --  expansion scheme. Another exception is if Restrictions
            --  (No_Finalization) is active, since then we know nothing is
            --  controlled.

            elsif Restriction_Active (No_Finalization)
              or else In_Runtime (Def_Id)
            then
               null;

            --  Assume that incomplete and private types are always completed
            --  by a controlled full view.

            elsif Needs_Finalization (Desig_Type)
              or else
                (Is_Incomplete_Or_Private_Type (Desig_Type)
                  and then No (Full_View (Desig_Type)))
              or else
                (Is_Array_Type (Desig_Type)
                  and then Needs_Finalization (Component_Type (Desig_Type)))
            then
               Build_Finalization_Master (Def_Id);
            end if;
         end;

      --  Freeze processing for enumeration types

      elsif Ekind (Def_Id) = E_Enumeration_Type then

         --  We only have something to do if we have a non-standard
         --  representation (i.e. at least one literal whose pos value
         --  is not the same as its representation)

         if Has_Non_Standard_Rep (Def_Id) then
            Expand_Freeze_Enumeration_Type (N);
         end if;

      --  Private types that are completed by a derivation from a private
      --  type have an internally generated full view, that needs to be
      --  frozen. This must be done explicitly because the two views share
      --  the freeze node, and the underlying full view is not visible when
      --  the freeze node is analyzed.

      elsif Is_Private_Type (Def_Id)
        and then Is_Derived_Type (Def_Id)
        and then Present (Full_View (Def_Id))
        and then Is_Itype (Full_View (Def_Id))
        and then Has_Private_Declaration (Full_View (Def_Id))
        and then Freeze_Node (Full_View (Def_Id)) = N
      then
         Set_Entity (N, Full_View (Def_Id));
         Result := Freeze_Type (N);
         Set_Entity (N, Def_Id);

      --  All other types require no expander action. There are such cases
      --  (e.g. task types and protected types). In such cases, the freeze
      --  nodes are there for use by Gigi.

      end if;

      Freeze_Stream_Operations (N, Def_Id);
      return Result;

   exception
      when RE_Not_Available =>
         return False;
   end Freeze_Type;

   -------------------------
   -- Get_Simple_Init_Val --
   -------------------------

   function Get_Simple_Init_Val
     (T    : Entity_Id;
      N    : Node_Id;
      Size : Uint := No_Uint) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Val    : Node_Id;
      Result : Node_Id;
      Val_RE : RE_Id;

      Size_To_Use : Uint;
      --  This is the size to be used for computation of the appropriate
      --  initial value for the Normalize_Scalars and Initialize_Scalars case.

      IV_Attribute : constant Boolean :=
                       Nkind (N) = N_Attribute_Reference
                         and then Attribute_Name (N) = Name_Invalid_Value;

      Lo_Bound : Uint;
      Hi_Bound : Uint;
      --  These are the values computed by the procedure Check_Subtype_Bounds

      procedure Check_Subtype_Bounds;
      --  This procedure examines the subtype T, and its ancestor subtypes and
      --  derived types to determine the best known information about the
      --  bounds of the subtype. After the call Lo_Bound is set either to
      --  No_Uint if no information can be determined, or to a value which
      --  represents a known low bound, i.e. a valid value of the subtype can
      --  not be less than this value. Hi_Bound is similarly set to a known
      --  high bound (valid value cannot be greater than this).

      --------------------------
      -- Check_Subtype_Bounds --
      --------------------------

      procedure Check_Subtype_Bounds is
         ST1  : Entity_Id;
         ST2  : Entity_Id;
         Lo   : Node_Id;
         Hi   : Node_Id;
         Loval : Uint;
         Hival : Uint;

      begin
         Lo_Bound := No_Uint;
         Hi_Bound := No_Uint;

         --  Loop to climb ancestor subtypes and derived types

         ST1 := T;
         loop
            if not Is_Discrete_Type (ST1) then
               return;
            end if;

            Lo := Type_Low_Bound (ST1);
            Hi := Type_High_Bound (ST1);

            if Compile_Time_Known_Value (Lo) then
               Loval := Expr_Value (Lo);

               if Lo_Bound = No_Uint or else Lo_Bound < Loval then
                  Lo_Bound := Loval;
               end if;
            end if;

            if Compile_Time_Known_Value (Hi) then
               Hival := Expr_Value (Hi);

               if Hi_Bound = No_Uint or else Hi_Bound > Hival then
                  Hi_Bound := Hival;
               end if;
            end if;

            ST2 := Ancestor_Subtype (ST1);

            if No (ST2) then
               ST2 := Etype (ST1);
            end if;

            exit when ST1 = ST2;
            ST1 := ST2;
         end loop;
      end Check_Subtype_Bounds;

   --  Start of processing for Get_Simple_Init_Val

   begin
      --  For a private type, we should always have an underlying type
      --  (because this was already checked in Needs_Simple_Initialization).
      --  What we do is to get the value for the underlying type and then do
      --  an Unchecked_Convert to the private type.

      if Is_Private_Type (T) then
         Val := Get_Simple_Init_Val (Underlying_Type (T), N, Size);

         --  A special case, if the underlying value is null, then qualify it
         --  with the underlying type, so that the null is properly typed
         --  Similarly, if it is an aggregate it must be qualified, because an
         --  unchecked conversion does not provide a context for it.

         if Nkind_In (Val, N_Null, N_Aggregate) then
            Val :=
              Make_Qualified_Expression (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Underlying_Type (T), Loc),
                Expression => Val);
         end if;

         Result := Unchecked_Convert_To (T, Val);

         --  Don't truncate result (important for Initialize/Normalize_Scalars)

         if Nkind (Result) = N_Unchecked_Type_Conversion
           and then Is_Scalar_Type (Underlying_Type (T))
         then
            Set_No_Truncation (Result);
         end if;

         return Result;

      --  Scalars with Default_Value aspect. The first subtype may now be
      --   private, so retrieve value from underlying type.

      elsif Is_Scalar_Type (T) and then Has_Default_Aspect (T) then
         if Is_Private_Type (First_Subtype (T)) then
            return Unchecked_Convert_To (T,
              Default_Aspect_Value (Full_View (First_Subtype (T))));
         else
            return
              Convert_To (T, Default_Aspect_Value (First_Subtype (T)));
         end if;

      --  Otherwise, for scalars, we must have normalize/initialize scalars
      --  case, or if the node N is an 'Invalid_Value attribute node.

      elsif Is_Scalar_Type (T) then
         pragma Assert (Init_Or_Norm_Scalars or IV_Attribute);

         --  Compute size of object. If it is given by the caller, we can use
         --  it directly, otherwise we use Esize (T) as an estimate. As far as
         --  we know this covers all cases correctly.

         if Size = No_Uint or else Size <= Uint_0 then
            Size_To_Use := UI_Max (Uint_1, Esize (T));
         else
            Size_To_Use := Size;
         end if;

         --  Maximum size to use is 64 bits, since we will create values of
         --  type Unsigned_64 and the range must fit this type.

         if Size_To_Use /= No_Uint and then Size_To_Use > Uint_64 then
            Size_To_Use := Uint_64;
         end if;

         --  Check known bounds of subtype

         Check_Subtype_Bounds;

         --  Processing for Normalize_Scalars case

         if Normalize_Scalars and then not IV_Attribute then

            --  If zero is invalid, it is a convenient value to use that is
            --  for sure an appropriate invalid value in all situations.

            if Lo_Bound /= No_Uint and then Lo_Bound > Uint_0 then
               Val := Make_Integer_Literal (Loc, 0);

            --  Cases where all one bits is the appropriate invalid value

            --  For modular types, all 1 bits is either invalid or valid. If
            --  it is valid, then there is nothing that can be done since there
            --  are no invalid values (we ruled out zero already).

            --  For signed integer types that have no negative values, either
            --  there is room for negative values, or there is not. If there
            --  is, then all 1-bits may be interpreted as minus one, which is
            --  certainly invalid. Alternatively it is treated as the largest
            --  positive value, in which case the observation for modular types
            --  still applies.

            --  For float types, all 1-bits is a NaN (not a number), which is
            --  certainly an appropriately invalid value.

            elsif Is_Unsigned_Type (T)
              or else Is_Floating_Point_Type (T)
              or else Is_Enumeration_Type (T)
            then
               Val := Make_Integer_Literal (Loc, 2 ** Size_To_Use - 1);

               --  Resolve as Unsigned_64, because the largest number we can
               --  generate is out of range of universal integer.

               Analyze_And_Resolve (Val, RTE (RE_Unsigned_64));

            --  Case of signed types

            else
               declare
                  Signed_Size : constant Uint :=
                                  UI_Min (Uint_63, Size_To_Use - 1);

               begin
                  --  Normally we like to use the most negative number. The one
                  --  exception is when this number is in the known subtype
                  --  range and the largest positive number is not in the known
                  --  subtype range.

                  --  For this exceptional case, use largest positive value

                  if Lo_Bound /= No_Uint and then Hi_Bound /= No_Uint
                    and then Lo_Bound <= (-(2 ** Signed_Size))
                    and then Hi_Bound < 2 ** Signed_Size
                  then
                     Val := Make_Integer_Literal (Loc, 2 ** Signed_Size - 1);

                  --  Normal case of largest negative value

                  else
                     Val := Make_Integer_Literal (Loc, -(2 ** Signed_Size));
                  end if;
               end;
            end if;

         --  Here for Initialize_Scalars case (or Invalid_Value attribute used)

         else
            --  For float types, use float values from System.Scalar_Values

            if Is_Floating_Point_Type (T) then
               if Root_Type (T) = Standard_Short_Float then
                  Val_RE := RE_IS_Isf;
               elsif Root_Type (T) = Standard_Float then
                  Val_RE := RE_IS_Ifl;
               elsif Root_Type (T) = Standard_Long_Float then
                  Val_RE := RE_IS_Ilf;
               else pragma Assert (Root_Type (T) = Standard_Long_Long_Float);
                  Val_RE := RE_IS_Ill;
               end if;

            --  If zero is invalid, use zero values from System.Scalar_Values

            elsif Lo_Bound /= No_Uint and then Lo_Bound > Uint_0 then
               if Size_To_Use <= 8 then
                  Val_RE := RE_IS_Iz1;
               elsif Size_To_Use <= 16 then
                  Val_RE := RE_IS_Iz2;
               elsif Size_To_Use <= 32 then
                  Val_RE := RE_IS_Iz4;
               else
                  Val_RE := RE_IS_Iz8;
               end if;

            --  For unsigned, use unsigned values from System.Scalar_Values

            elsif Is_Unsigned_Type (T) then
               if Size_To_Use <= 8 then
                  Val_RE := RE_IS_Iu1;
               elsif Size_To_Use <= 16 then
                  Val_RE := RE_IS_Iu2;
               elsif Size_To_Use <= 32 then
                  Val_RE := RE_IS_Iu4;
               else
                  Val_RE := RE_IS_Iu8;
               end if;

            --  For signed, use signed values from System.Scalar_Values

            else
               if Size_To_Use <= 8 then
                  Val_RE := RE_IS_Is1;
               elsif Size_To_Use <= 16 then
                  Val_RE := RE_IS_Is2;
               elsif Size_To_Use <= 32 then
                  Val_RE := RE_IS_Is4;
               else
                  Val_RE := RE_IS_Is8;
               end if;
            end if;

            Val := New_Occurrence_Of (RTE (Val_RE), Loc);
         end if;

         --  The final expression is obtained by doing an unchecked conversion
         --  of this result to the base type of the required subtype. We use
         --  the base type to prevent the unchecked conversion from chopping
         --  bits, and then we set Kill_Range_Check to preserve the "bad"
         --  value.

         Result := Unchecked_Convert_To (Base_Type (T), Val);

         --  Ensure result is not truncated, since we want the "bad" bits, and
         --  also kill range check on result.

         if Nkind (Result) = N_Unchecked_Type_Conversion then
            Set_No_Truncation (Result);
            Set_Kill_Range_Check (Result, True);
         end if;

         return Result;

      --  String or Wide_[Wide]_String (must have Initialize_Scalars set)

      elsif Root_Type (T) = Standard_String
              or else
            Root_Type (T) = Standard_Wide_String
              or else
            Root_Type (T) = Standard_Wide_Wide_String
      then
         pragma Assert (Init_Or_Norm_Scalars);

         return
           Make_Aggregate (Loc,
             Component_Associations => New_List (
               Make_Component_Association (Loc,
                 Choices => New_List (
                   Make_Others_Choice (Loc)),
                 Expression =>
                   Get_Simple_Init_Val
                     (Component_Type (T), N, Esize (Root_Type (T))))));

      --  Access type is initialized to null

      elsif Is_Access_Type (T) then
         return Make_Null (Loc);

      --  No other possibilities should arise, since we should only be calling
      --  Get_Simple_Init_Val if Needs_Simple_Initialization returned True,
      --  indicating one of the above cases held.

      else
         raise Program_Error;
      end if;

   exception
      when RE_Not_Available =>
         return Empty;
   end Get_Simple_Init_Val;

   ------------------------------
   -- Has_New_Non_Standard_Rep --
   ------------------------------

   function Has_New_Non_Standard_Rep (T : Entity_Id) return Boolean is
   begin
      if not Is_Derived_Type (T) then
         return Has_Non_Standard_Rep (T)
           or else Has_Non_Standard_Rep (Root_Type (T));

      --  If Has_Non_Standard_Rep is not set on the derived type, the
      --  representation is fully inherited.

      elsif not Has_Non_Standard_Rep (T) then
         return False;

      else
         return First_Rep_Item (T) /= First_Rep_Item (Root_Type (T));

         --  May need a more precise check here: the First_Rep_Item may
         --  be a stream attribute, which does not affect the representation
         --  of the type ???
      end if;
   end Has_New_Non_Standard_Rep;

   ----------------
   -- In_Runtime --
   ----------------

   function In_Runtime (E : Entity_Id) return Boolean is
      S1 : Entity_Id;

   begin
      S1 := Scope (E);
      while Scope (S1) /= Standard_Standard loop
         S1 := Scope (S1);
      end loop;

      return Is_RTU (S1, System) or else Is_RTU (S1, Ada);
   end In_Runtime;

   ---------------------------------------
   -- Insert_Component_Invariant_Checks --
   ---------------------------------------

   procedure Insert_Component_Invariant_Checks
     (N   : Node_Id;
     Typ  : Entity_Id;
     Proc : Node_Id)
   is
      Loc     : constant Source_Ptr := Sloc (Typ);
      Proc_Id : Entity_Id;

   begin
      if Present (Proc) then
         Proc_Id := Defining_Entity (Proc);

         if not Has_Invariants (Typ) then
            Set_Has_Invariants (Typ);
            Set_Is_Invariant_Procedure (Proc_Id);
            Set_Invariant_Procedure (Typ, Proc_Id);
            Insert_After (N, Proc);
            Analyze (Proc);

         else

            --  Find already created invariant body, insert body of component
            --  invariant proc in it, and add call after other checks.

            declare
               Bod : Node_Id;
               Inv_Id : constant Entity_Id := Invariant_Procedure (Typ);
               Call   : constant Node_Id :=
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Occurrence_Of (Proc_Id, Loc),
                   Parameter_Associations =>
                     New_List
                       (New_Reference_To (First_Formal (Inv_Id), Loc)));

            begin

               --  The invariant  body has not been analyzed yet, so we do a
               --  sequential search forward, and retrieve it by name.

               Bod := Next (N);
               while Present (Bod) loop
                  exit when Nkind (Bod) = N_Subprogram_Body
                    and then Chars (Defining_Entity (Bod)) = Chars (Inv_Id);
                  Next (Bod);
               end loop;

               Append_To (Declarations (Bod), Proc);
               Append_To (Statements (Handled_Statement_Sequence (Bod)), Call);
            end;
         end if;
      end if;
   end Insert_Component_Invariant_Checks;

   ----------------------------
   -- Initialization_Warning --
   ----------------------------

   procedure Initialization_Warning (E : Entity_Id) is
      Warning_Needed : Boolean;

   begin
      Warning_Needed := False;

      if Ekind (Current_Scope) = E_Package
        and then Static_Elaboration_Desired (Current_Scope)
      then
         if Is_Type (E) then
            if Is_Record_Type (E) then
               if Has_Discriminants (E)
                 or else Is_Limited_Type (E)
                 or else Has_Non_Standard_Rep (E)
               then
                  Warning_Needed := True;

               else
                  --  Verify that at least one component has an initialization
                  --  expression. No need for a warning on a type if all its
                  --  components have no initialization.

                  declare
                     Comp : Entity_Id;

                  begin
                     Comp := First_Component (E);
                     while Present (Comp) loop
                        if Ekind (Comp) = E_Discriminant
                          or else
                            (Nkind (Parent (Comp)) = N_Component_Declaration
                               and then Present (Expression (Parent (Comp))))
                        then
                           Warning_Needed := True;
                           exit;
                        end if;

                        Next_Component (Comp);
                     end loop;
                  end;
               end if;

               if Warning_Needed then
                  Error_Msg_N
                    ("Objects of the type cannot be initialized "
                     & "statically by default??", Parent (E));
               end if;
            end if;

         else
            Error_Msg_N ("Object cannot be initialized statically??", E);
         end if;
      end if;
   end Initialization_Warning;

   ------------------
   -- Init_Formals --
   ------------------

   function Init_Formals (Typ : Entity_Id) return List_Id is
      Loc     : constant Source_Ptr := Sloc (Typ);
      Formals : List_Id;

   begin
      --  First parameter is always _Init : in out typ. Note that we need
      --  this to be in/out because in the case of the task record value,
      --  there are default record fields (_Priority, _Size, -Task_Info)
      --  that may be referenced in the generated initialization routine.

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uInit),
          In_Present  => True,
          Out_Present => True,
          Parameter_Type => New_Reference_To (Typ, Loc)));

      --  For task record value, or type that contains tasks, add two more
      --  formals, _Master : Master_Id and _Chain : in out Activation_Chain
      --  We also add these parameters for the task record type case.

      if Has_Task (Typ)
        or else (Is_Record_Type (Typ) and then Is_Task_Record_Type (Typ))
      then
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uMaster),
             Parameter_Type      =>
               New_Reference_To (RTE (RE_Master_Id), Loc)));

         --  Add _Chain (not done for sequential elaboration policy, see
         --  comment for Create_Restricted_Task_Sequential in s-tarest.ads).

         if Partition_Elaboration_Policy /= 'S' then
            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uChain),
                In_Present          => True,
                Out_Present         => True,
                Parameter_Type      =>
                  New_Reference_To (RTE (RE_Activation_Chain), Loc)));
         end if;

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uTask_Name),
             In_Present          => True,
             Parameter_Type      => New_Reference_To (Standard_String, Loc)));
      end if;

      return Formals;

   exception
      when RE_Not_Available =>
         return Empty_List;
   end Init_Formals;

   -------------------------
   -- Init_Secondary_Tags --
   -------------------------

   procedure Init_Secondary_Tags
     (Typ            : Entity_Id;
      Target         : Node_Id;
      Stmts_List     : List_Id;
      Fixed_Comps    : Boolean := True;
      Variable_Comps : Boolean := True)
   is
      Loc : constant Source_Ptr := Sloc (Target);

      --  Inherit the C++ tag of the secondary dispatch table of Typ associated
      --  with Iface. Tag_Comp is the component of Typ that stores Iface_Tag.

      procedure Initialize_Tag
        (Typ       : Entity_Id;
         Iface     : Entity_Id;
         Tag_Comp  : Entity_Id;
         Iface_Tag : Node_Id);
      --  Initialize the tag of the secondary dispatch table of Typ associated
      --  with Iface. Tag_Comp is the component of Typ that stores Iface_Tag.
      --  Compiling under the CPP full ABI compatibility mode, if the ancestor
      --  of Typ CPP tagged type we generate code to inherit the contents of
      --  the dispatch table directly from the ancestor.

      --------------------
      -- Initialize_Tag --
      --------------------

      procedure Initialize_Tag
        (Typ       : Entity_Id;
         Iface     : Entity_Id;
         Tag_Comp  : Entity_Id;
         Iface_Tag : Node_Id)
      is
         Comp_Typ           : Entity_Id;
         Offset_To_Top_Comp : Entity_Id := Empty;

      begin
         --  Initialize the pointer to the secondary DT associated with the
         --  interface.

         if not Is_Ancestor (Iface, Typ, Use_Full_View => True) then
            Append_To (Stmts_List,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => New_Copy_Tree (Target),
                    Selector_Name => New_Reference_To (Tag_Comp, Loc)),
                Expression =>
                  New_Reference_To (Iface_Tag, Loc)));
         end if;

         Comp_Typ := Scope (Tag_Comp);

         --  Initialize the entries of the table of interfaces. We generate a
         --  different call when the parent of the type has variable size
         --  components.

         if Comp_Typ /= Etype (Comp_Typ)
           and then Is_Variable_Size_Record (Etype (Comp_Typ))
           and then Chars (Tag_Comp) /= Name_uTag
         then
            pragma Assert (Present (DT_Offset_To_Top_Func (Tag_Comp)));

            --  Issue error if Set_Dynamic_Offset_To_Top is not available in a
            --  configurable run-time environment.

            if not RTE_Available (RE_Set_Dynamic_Offset_To_Top) then
               Error_Msg_CRT
                 ("variable size record with interface types", Typ);
               return;
            end if;

            --  Generate:
            --    Set_Dynamic_Offset_To_Top
            --      (This         => Init,
            --       Interface_T  => Iface'Tag,
            --       Offset_Value => n,
            --       Offset_Func  => Fn'Address)

            Append_To (Stmts_List,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To
                          (RTE (RE_Set_Dynamic_Offset_To_Top), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Copy_Tree (Target),
                    Attribute_Name => Name_Address),

                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Reference_To
                      (Node (First_Elmt (Access_Disp_Table (Iface))),
                       Loc)),

                  Unchecked_Convert_To
                    (RTE (RE_Storage_Offset),
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         Make_Selected_Component (Loc,
                           Prefix        => New_Copy_Tree (Target),
                           Selector_Name =>
                             New_Reference_To (Tag_Comp, Loc)),
                       Attribute_Name => Name_Position)),

                  Unchecked_Convert_To (RTE (RE_Offset_To_Top_Function_Ptr),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To
                                  (DT_Offset_To_Top_Func (Tag_Comp), Loc),
                      Attribute_Name => Name_Address)))));

            --  In this case the next component stores the value of the
            --  offset to the top.

            Offset_To_Top_Comp := Next_Entity (Tag_Comp);
            pragma Assert (Present (Offset_To_Top_Comp));

            Append_To (Stmts_List,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => New_Copy_Tree (Target),
                    Selector_Name => New_Reference_To
                                       (Offset_To_Top_Comp, Loc)),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix       =>
                      Make_Selected_Component (Loc,
                        Prefix        => New_Copy_Tree (Target),
                        Selector_Name => New_Reference_To (Tag_Comp, Loc)),
                  Attribute_Name => Name_Position)));

         --  Normal case: No discriminants in the parent type

         else
            --  Don't need to set any value if this interface shares the
            --  primary dispatch table.

            if not Is_Ancestor (Iface, Typ, Use_Full_View => True) then
               Append_To (Stmts_List,
                 Build_Set_Static_Offset_To_Top (Loc,
                   Iface_Tag    => New_Reference_To (Iface_Tag, Loc),
                   Offset_Value =>
                     Unchecked_Convert_To (RTE (RE_Storage_Offset),
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           Make_Selected_Component (Loc,
                             Prefix        => New_Copy_Tree (Target),
                             Selector_Name =>
                               New_Reference_To (Tag_Comp, Loc)),
                         Attribute_Name => Name_Position))));
            end if;

            --  Generate:
            --    Register_Interface_Offset
            --      (This         => Init,
            --       Interface_T  => Iface'Tag,
            --       Is_Constant  => True,
            --       Offset_Value => n,
            --       Offset_Func  => null);

            if RTE_Available (RE_Register_Interface_Offset) then
               Append_To (Stmts_List,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To
                             (RTE (RE_Register_Interface_Offset), Loc),
                   Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Copy_Tree (Target),
                       Attribute_Name => Name_Address),

                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To
                         (Node (First_Elmt (Access_Disp_Table (Iface))), Loc)),

                     New_Occurrence_Of (Standard_True, Loc),

                     Unchecked_Convert_To
                       (RTE (RE_Storage_Offset),
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            Make_Selected_Component (Loc,
                              Prefix         => New_Copy_Tree (Target),
                              Selector_Name  =>
                                New_Reference_To (Tag_Comp, Loc)),
                         Attribute_Name => Name_Position)),

                     Make_Null (Loc))));
            end if;
         end if;
      end Initialize_Tag;

      --  Local variables

      Full_Typ         : Entity_Id;
      Ifaces_List      : Elist_Id;
      Ifaces_Comp_List : Elist_Id;
      Ifaces_Tag_List  : Elist_Id;
      Iface_Elmt       : Elmt_Id;
      Iface_Comp_Elmt  : Elmt_Id;
      Iface_Tag_Elmt   : Elmt_Id;
      Tag_Comp         : Node_Id;
      In_Variable_Pos  : Boolean;

   --  Start of processing for Init_Secondary_Tags

   begin
      --  Handle private types

      if Present (Full_View (Typ)) then
         Full_Typ := Full_View (Typ);
      else
         Full_Typ := Typ;
      end if;

      Collect_Interfaces_Info
        (Full_Typ, Ifaces_List, Ifaces_Comp_List, Ifaces_Tag_List);

      Iface_Elmt      := First_Elmt (Ifaces_List);
      Iface_Comp_Elmt := First_Elmt (Ifaces_Comp_List);
      Iface_Tag_Elmt  := First_Elmt (Ifaces_Tag_List);
      while Present (Iface_Elmt) loop
         Tag_Comp := Node (Iface_Comp_Elmt);

         --  Check if parent of record type has variable size components

         In_Variable_Pos := Scope (Tag_Comp) /= Etype (Scope (Tag_Comp))
           and then Is_Variable_Size_Record (Etype (Scope (Tag_Comp)));

         --  If we are compiling under the CPP full ABI compatibility mode and
         --  the ancestor is a CPP_Pragma tagged type then we generate code to
         --  initialize the secondary tag components from tags that reference
         --  secondary tables filled with copy of parent slots.

         if Is_CPP_Class (Root_Type (Full_Typ)) then

            --  Reject interface components located at variable offset in
            --  C++ derivations. This is currently unsupported.

            if not Fixed_Comps and then In_Variable_Pos then

               --  Locate the first dynamic component of the record. Done to
               --  improve the text of the warning.

               declare
                  Comp     : Entity_Id;
                  Comp_Typ : Entity_Id;

               begin
                  Comp := First_Entity (Typ);
                  while Present (Comp) loop
                     Comp_Typ := Etype (Comp);

                     if Ekind (Comp) /= E_Discriminant
                       and then not Is_Tag (Comp)
                     then
                        exit when
                          (Is_Record_Type (Comp_Typ)
                             and then Is_Variable_Size_Record
                                        (Base_Type (Comp_Typ)))
                         or else
                           (Is_Array_Type (Comp_Typ)
                              and then Is_Variable_Size_Array (Comp_Typ));
                     end if;

                     Next_Entity (Comp);
                  end loop;

                  pragma Assert (Present (Comp));
                  Error_Msg_Node_2 := Comp;
                  Error_Msg_NE
                    ("parent type & with dynamic component & cannot be parent"
                       & " of 'C'P'P derivation if new interfaces are present",
                     Typ, Scope (Original_Record_Component (Comp)));

                  Error_Msg_Sloc :=
                    Sloc (Scope (Original_Record_Component (Comp)));
                  Error_Msg_NE
                    ("type derived from 'C'P'P type & defined #",
                     Typ, Scope (Original_Record_Component (Comp)));

                  --  Avoid duplicated warnings

                  exit;
               end;

            --  Initialize secondary tags

            else
               Append_To (Stmts_List,
                 Make_Assignment_Statement (Loc,
                   Name =>
                     Make_Selected_Component (Loc,
                       Prefix => New_Copy_Tree (Target),
                       Selector_Name =>
                         New_Reference_To (Node (Iface_Comp_Elmt), Loc)),
                   Expression =>
                     New_Reference_To (Node (Iface_Tag_Elmt), Loc)));
            end if;

         --  Otherwise generate code to initialize the tag

         else
            if (In_Variable_Pos and then Variable_Comps)
              or else (not In_Variable_Pos and then Fixed_Comps)
            then
               Initialize_Tag (Full_Typ,
                 Iface     => Node (Iface_Elmt),
                 Tag_Comp  => Tag_Comp,
                 Iface_Tag => Node (Iface_Tag_Elmt));
            end if;
         end if;

         Next_Elmt (Iface_Elmt);
         Next_Elmt (Iface_Comp_Elmt);
         Next_Elmt (Iface_Tag_Elmt);
      end loop;
   end Init_Secondary_Tags;

   ------------------------
   -- Is_User_Defined_Eq --
   ------------------------

   function Is_User_Defined_Equality (Prim : Node_Id) return Boolean is
   begin
      return Chars (Prim) = Name_Op_Eq
        and then Etype (First_Formal (Prim)) =
                 Etype (Next_Formal (First_Formal (Prim)))
        and then Base_Type (Etype (Prim)) = Standard_Boolean;
   end Is_User_Defined_Equality;

   ----------------------------------------
   -- Make_Controlling_Function_Wrappers --
   ----------------------------------------

   procedure Make_Controlling_Function_Wrappers
     (Tag_Typ   : Entity_Id;
      Decl_List : out List_Id;
      Body_List : out List_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Tag_Typ);
      Prim_Elmt   : Elmt_Id;
      Subp        : Entity_Id;
      Actual_List : List_Id;
      Formal_List : List_Id;
      Formal      : Entity_Id;
      Par_Formal  : Entity_Id;
      Formal_Node : Node_Id;
      Func_Body   : Node_Id;
      Func_Decl   : Node_Id;
      Func_Spec   : Node_Id;
      Return_Stmt : Node_Id;

   begin
      Decl_List := New_List;
      Body_List := New_List;

      Prim_Elmt := First_Elmt (Primitive_Operations (Tag_Typ));

      while Present (Prim_Elmt) loop
         Subp := Node (Prim_Elmt);

         --  If a primitive function with a controlling result of the type has
         --  not been overridden by the user, then we must create a wrapper
         --  function here that effectively overrides it and invokes the
         --  (non-abstract) parent function. This can only occur for a null
         --  extension. Note that functions with anonymous controlling access
         --  results don't qualify and must be overridden. We also exclude
         --  Input attributes, since each type will have its own version of
         --  Input constructed by the expander. The test for Comes_From_Source
         --  is needed to distinguish inherited operations from renamings
         --  (which also have Alias set). We exclude internal entities with
         --  Interface_Alias to avoid generating duplicated wrappers since
         --  the primitive which covers the interface is also available in
         --  the list of primitive operations.

         --  The function may be abstract, or require_Overriding may be set
         --  for it, because tests for null extensions may already have reset
         --  the Is_Abstract_Subprogram_Flag. If Requires_Overriding is not
         --  set, functions that need wrappers are recognized by having an
         --  alias that returns the parent type.

         if Comes_From_Source (Subp)
           or else No (Alias (Subp))
           or else Present (Interface_Alias (Subp))
           or else Ekind (Subp) /= E_Function
           or else not Has_Controlling_Result (Subp)
           or else Is_Access_Type (Etype (Subp))
           or else Is_Abstract_Subprogram (Alias (Subp))
           or else Is_TSS (Subp, TSS_Stream_Input)
         then
            goto Next_Prim;

         elsif Is_Abstract_Subprogram (Subp)
           or else Requires_Overriding (Subp)
           or else
             (Is_Null_Extension (Etype (Subp))
               and then Etype (Alias (Subp)) /= Etype (Subp))
         then
            Formal_List := No_List;
            Formal := First_Formal (Subp);

            if Present (Formal) then
               Formal_List := New_List;

               while Present (Formal) loop
                  Append
                    (Make_Parameter_Specification
                       (Loc,
                        Defining_Identifier =>
                          Make_Defining_Identifier (Sloc (Formal),
                            Chars => Chars (Formal)),
                        In_Present  => In_Present (Parent (Formal)),
                        Out_Present => Out_Present (Parent (Formal)),
                        Null_Exclusion_Present =>
                          Null_Exclusion_Present (Parent (Formal)),
                        Parameter_Type =>
                          New_Reference_To (Etype (Formal), Loc),
                        Expression =>
                          New_Copy_Tree (Expression (Parent (Formal)))),
                     Formal_List);

                  Next_Formal (Formal);
               end loop;
            end if;

            Func_Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name       =>
                  Make_Defining_Identifier (Loc,
                    Chars => Chars (Subp)),
                Parameter_Specifications => Formal_List,
                Result_Definition        =>
                  New_Reference_To (Etype (Subp), Loc));

            Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
            Append_To (Decl_List, Func_Decl);

            --  Build a wrapper body that calls the parent function. The body
            --  contains a single return statement that returns an extension
            --  aggregate whose ancestor part is a call to the parent function,
            --  passing the formals as actuals (with any controlling arguments
            --  converted to the types of the corresponding formals of the
            --  parent function, which might be anonymous access types), and
            --  having a null extension.

            Formal      := First_Formal (Subp);
            Par_Formal  := First_Formal (Alias (Subp));
            Formal_Node := First (Formal_List);

            if Present (Formal) then
               Actual_List := New_List;
            else
               Actual_List := No_List;
            end if;

            while Present (Formal) loop
               if Is_Controlling_Formal (Formal) then
                  Append_To (Actual_List,
                    Make_Type_Conversion (Loc,
                      Subtype_Mark =>
                        New_Occurrence_Of (Etype (Par_Formal), Loc),
                      Expression   =>
                        New_Reference_To
                          (Defining_Identifier (Formal_Node), Loc)));
               else
                  Append_To
                    (Actual_List,
                     New_Reference_To
                       (Defining_Identifier (Formal_Node), Loc));
               end if;

               Next_Formal (Formal);
               Next_Formal (Par_Formal);
               Next (Formal_Node);
            end loop;

            Return_Stmt :=
              Make_Simple_Return_Statement (Loc,
                Expression =>
                  Make_Extension_Aggregate (Loc,
                    Ancestor_Part =>
                      Make_Function_Call (Loc,
                        Name => New_Reference_To (Alias (Subp), Loc),
                        Parameter_Associations => Actual_List),
                    Null_Record_Present => True));

            Func_Body :=
              Make_Subprogram_Body (Loc,
                Specification => New_Copy_Tree (Func_Spec),
                Declarations => Empty_List,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Return_Stmt)));

            Set_Defining_Unit_Name
              (Specification (Func_Body),
                Make_Defining_Identifier (Loc, Chars (Subp)));

            Append_To (Body_List, Func_Body);

            --  Replace the inherited function with the wrapper function in the
            --  primitive operations list. We add the minimum decoration needed
            --  to override interface primitives.

            Set_Ekind (Defining_Unit_Name (Func_Spec), E_Function);

            Override_Dispatching_Operation
              (Tag_Typ, Subp, New_Op => Defining_Unit_Name (Func_Spec),
               Is_Wrapper => True);
         end if;

      <<Next_Prim>>
         Next_Elmt (Prim_Elmt);
      end loop;
   end Make_Controlling_Function_Wrappers;

   -------------------
   --  Make_Eq_Body --
   -------------------

   function Make_Eq_Body
     (Typ     : Entity_Id;
      Eq_Name : Name_Id) return Node_Id
   is
      Loc          : constant Source_Ptr := Sloc (Parent (Typ));
      Decl         : Node_Id;
      Def          : constant Node_Id := Parent (Typ);
      Stmts        : constant List_Id := New_List;
      Variant_Case : Boolean := Has_Discriminants (Typ);
      Comps        : Node_Id := Empty;
      Typ_Def      : Node_Id := Type_Definition (Def);

   begin
      Decl :=
        Predef_Spec_Or_Body (Loc,
          Tag_Typ => Typ,
          Name    => Eq_Name,
          Profile => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_X),
              Parameter_Type      => New_Reference_To (Typ, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_Y),
              Parameter_Type      => New_Reference_To (Typ, Loc))),

          Ret_Type => Standard_Boolean,
          For_Body => True);

      if Variant_Case then
         if Nkind (Typ_Def) = N_Derived_Type_Definition then
            Typ_Def := Record_Extension_Part (Typ_Def);
         end if;

         if Present (Typ_Def) then
            Comps := Component_List (Typ_Def);
         end if;

         Variant_Case :=
           Present (Comps) and then Present (Variant_Part (Comps));
      end if;

      if Variant_Case then
         Append_To (Stmts,
           Make_Eq_If (Typ, Discriminant_Specifications (Def)));
         Append_List_To (Stmts, Make_Eq_Case (Typ, Comps));
         Append_To (Stmts,
           Make_Simple_Return_Statement (Loc,
             Expression => New_Reference_To (Standard_True, Loc)));

      else
         Append_To (Stmts,
           Make_Simple_Return_Statement (Loc,
             Expression =>
               Expand_Record_Equality
                 (Typ,
                  Typ    => Typ,
                  Lhs    => Make_Identifier (Loc, Name_X),
                  Rhs    => Make_Identifier (Loc, Name_Y),
                  Bodies => Declarations (Decl))));
      end if;

      Set_Handled_Statement_Sequence
        (Decl, Make_Handled_Sequence_Of_Statements (Loc, Stmts));
      return Decl;
   end Make_Eq_Body;

   ------------------
   -- Make_Eq_Case --
   ------------------

   --  <Make_Eq_If shared components>
   --  case X.D1 is
   --     when V1 => <Make_Eq_Case> on subcomponents
   --     ...
   --     when Vn => <Make_Eq_Case> on subcomponents
   --  end case;

   function Make_Eq_Case
     (E      : Entity_Id;
      CL     : Node_Id;
      Discrs : Elist_Id := New_Elmt_List) return List_Id
   is
      Loc      : constant Source_Ptr := Sloc (E);
      Result   : constant List_Id    := New_List;
      Variant  : Node_Id;
      Alt_List : List_Id;

      function Corresponding_Formal (C : Node_Id) return Entity_Id;
      --  Given the discriminant that controls a given variant of an unchecked
      --  union, find the formal of the equality function that carries the
      --  inferred value of the discriminant.

      function External_Name (E : Entity_Id) return Name_Id;
      --  The value of a given discriminant is conveyed in the corresponding
      --  formal parameter of the equality routine. The name of this formal
      --  parameter carries a one-character suffix which is removed here.

      --------------------------
      -- Corresponding_Formal --
      --------------------------

      function Corresponding_Formal (C : Node_Id) return Entity_Id is
         Discr : constant Entity_Id := Entity (Name (Variant_Part (C)));
         Elm   : Elmt_Id;

      begin
         Elm := First_Elmt (Discrs);
         while Present (Elm) loop
            if Chars (Discr) = External_Name (Node (Elm)) then
               return Node (Elm);
            end if;
            Next_Elmt (Elm);
         end loop;

         --  A formal of the proper name must be found

         raise Program_Error;
      end Corresponding_Formal;

      -------------------
      -- External_Name --
      -------------------

      function External_Name (E : Entity_Id) return Name_Id is
      begin
         Get_Name_String (Chars (E));
         Name_Len := Name_Len - 1;
         return Name_Find;
      end External_Name;

   --  Start of processing for Make_Eq_Case

   begin
      Append_To (Result, Make_Eq_If (E, Component_Items (CL)));

      if No (Variant_Part (CL)) then
         return Result;
      end if;

      Variant := First_Non_Pragma (Variants (Variant_Part (CL)));

      if No (Variant) then
         return Result;
      end if;

      Alt_List := New_List;

      while Present (Variant) loop
         Append_To (Alt_List,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_Copy_List (Discrete_Choices (Variant)),
             Statements =>
               Make_Eq_Case (E, Component_List (Variant), Discrs)));

         Next_Non_Pragma (Variant);
      end loop;

      --  If we have an Unchecked_Union, use one of the parameters of the
      --  enclosing equality routine that captures the discriminant, to use
      --  as the expression in the generated case statement.

      if Is_Unchecked_Union (E) then
         Append_To (Result,
           Make_Case_Statement (Loc,
             Expression =>
               New_Reference_To (Corresponding_Formal (CL), Loc),
             Alternatives => Alt_List));

      else
         Append_To (Result,
           Make_Case_Statement (Loc,
             Expression =>
               Make_Selected_Component (Loc,
                 Prefix        => Make_Identifier (Loc, Name_X),
                 Selector_Name => New_Copy (Name (Variant_Part (CL)))),
             Alternatives => Alt_List));
      end if;

      return Result;
   end Make_Eq_Case;

   ----------------
   -- Make_Eq_If --
   ----------------

   --  Generates:

   --    if
   --      X.C1 /= Y.C1
   --        or else
   --      X.C2 /= Y.C2
   --        ...
   --    then
   --       return False;
   --    end if;

   --  or a null statement if the list L is empty

   function Make_Eq_If
     (E : Entity_Id;
      L : List_Id) return Node_Id
   is
      Loc        : constant Source_Ptr := Sloc (E);
      C          : Node_Id;
      Field_Name : Name_Id;
      Cond       : Node_Id;

   begin
      if No (L) then
         return Make_Null_Statement (Loc);

      else
         Cond := Empty;

         C := First_Non_Pragma (L);
         while Present (C) loop
            Field_Name := Chars (Defining_Identifier (C));

            --  The tags must not be compared: they are not part of the value.
            --  Ditto for parent interfaces because their equality operator is
            --  abstract.

            --  Note also that in the following, we use Make_Identifier for
            --  the component names. Use of New_Reference_To to identify the
            --  components would be incorrect because the wrong entities for
            --  discriminants could be picked up in the private type case.

            if Field_Name = Name_uParent
              and then Is_Interface (Etype (Defining_Identifier (C)))
            then
               null;

            elsif Field_Name /= Name_uTag then
               Evolve_Or_Else (Cond,
                 Make_Op_Ne (Loc,
                   Left_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_X),
                       Selector_Name => Make_Identifier (Loc, Field_Name)),

                   Right_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_Y),
                       Selector_Name => Make_Identifier (Loc, Field_Name))));
            end if;

            Next_Non_Pragma (C);
         end loop;

         if No (Cond) then
            return Make_Null_Statement (Loc);

         else
            return
              Make_Implicit_If_Statement (E,
                Condition => Cond,
                Then_Statements => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))));
         end if;
      end if;
   end Make_Eq_If;

   --------------------
   --  Make_Neq_Body --
   --------------------

   function Make_Neq_Body (Tag_Typ : Entity_Id) return Node_Id is

      function Is_Predefined_Neq_Renaming (Prim : Node_Id) return Boolean;
      --  Returns true if Prim is a renaming of an unresolved predefined
      --  inequality operation.

      --------------------------------
      -- Is_Predefined_Neq_Renaming --
      --------------------------------

      function Is_Predefined_Neq_Renaming (Prim : Node_Id) return Boolean is
      begin
         return Chars (Prim) /= Name_Op_Ne
           and then Present (Alias (Prim))
           and then Comes_From_Source (Prim)
           and then Is_Intrinsic_Subprogram (Alias (Prim))
           and then Chars (Alias (Prim)) = Name_Op_Ne;
      end Is_Predefined_Neq_Renaming;

      --  Local variables

      Loc           : constant Source_Ptr := Sloc (Parent (Tag_Typ));
      Stmts         : constant List_Id    := New_List;
      Decl          : Node_Id;
      Eq_Prim       : Entity_Id;
      Left_Op       : Entity_Id;
      Renaming_Prim : Entity_Id;
      Right_Op      : Entity_Id;
      Target        : Entity_Id;

   --  Start of processing for Make_Neq_Body

   begin
      --  For a call on a renaming of a dispatching subprogram that is
      --  overridden, if the overriding occurred before the renaming, then
      --  the body executed is that of the overriding declaration, even if the
      --  overriding declaration is not visible at the place of the renaming;
      --  otherwise, the inherited or predefined subprogram is called, see
      --  (RM 8.5.4(8))

      --  Stage 1: Search for a renaming of the inequality primitive and also
      --  search for an overriding of the equality primitive located before the
      --  renaming declaration.

      declare
         Elmt : Elmt_Id;
         Prim : Node_Id;

      begin
         Eq_Prim       := Empty;
         Renaming_Prim := Empty;

         Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Elmt) loop
            Prim := Node (Elmt);

            if Is_User_Defined_Equality (Prim)
              and then No (Alias (Prim))
            then
               if No (Renaming_Prim) then
                  pragma Assert (No (Eq_Prim));
                  Eq_Prim := Prim;
               end if;

            elsif Is_Predefined_Neq_Renaming (Prim) then
               Renaming_Prim := Prim;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end;

      --  No further action needed if no renaming was found

      if No (Renaming_Prim) then
         return Empty;
      end if;

      --  Stage 2: Replace the renaming declaration by a subprogram declaration
      --  (required to add its body)

      Decl := Parent (Parent (Renaming_Prim));
      Rewrite (Decl,
        Make_Subprogram_Declaration (Loc,
          Specification => Specification (Decl)));
      Set_Analyzed (Decl);

      --  Remove the decoration of intrinsic renaming subprogram

      Set_Is_Intrinsic_Subprogram (Renaming_Prim, False);
      Set_Convention (Renaming_Prim, Convention_Ada);
      Set_Alias (Renaming_Prim, Empty);
      Set_Has_Completion (Renaming_Prim, False);

      --  Stage 3: Build the corresponding body

      Left_Op  := First_Formal (Renaming_Prim);
      Right_Op := Next_Formal (Left_Op);

      Decl :=
        Predef_Spec_Or_Body (Loc,
          Tag_Typ => Tag_Typ,
          Name    => Chars (Renaming_Prim),
          Profile => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Chars (Left_Op)),
              Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Chars (Right_Op)),
              Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

          Ret_Type => Standard_Boolean,
          For_Body => True);

      --  If the overriding of the equality primitive occurred before the
      --  renaming, then generate:

      --    function <Neq_Name> (X : Y : Typ) return Boolean is
      --    begin
      --       return not Oeq (X, Y);
      --    end;

      if Present (Eq_Prim) then
         Target := Eq_Prim;

      --  Otherwise build a nested subprogram which performs the predefined
      --  evaluation of the equality operator. That is, generate:

      --    function <Neq_Name> (X : Y : Typ) return Boolean is
      --       function Oeq (X : Y) return Boolean is
      --       begin
      --          <<body of default implementation>>
      --       end;
      --    begin
      --       return not Oeq (X, Y);
      --    end;

      else
         declare
            Local_Subp : Node_Id;
         begin
            Local_Subp := Make_Eq_Body (Tag_Typ, Name_Op_Eq);
            Set_Declarations (Decl, New_List (Local_Subp));
            Target := Defining_Entity (Local_Subp);
         end;
      end if;

      Append_To (Stmts,
        Make_Simple_Return_Statement (Loc,
          Expression =>
            Make_Op_Not (Loc,
              Make_Function_Call (Loc,
                Name => New_Reference_To (Target, Loc),
                Parameter_Associations => New_List (
                  Make_Identifier (Loc, Chars (Left_Op)),
                  Make_Identifier (Loc, Chars (Right_Op)))))));

      Set_Handled_Statement_Sequence
        (Decl, Make_Handled_Sequence_Of_Statements (Loc, Stmts));
      return Decl;
   end Make_Neq_Body;

   -------------------------------
   -- Make_Null_Procedure_Specs --
   -------------------------------

   function Make_Null_Procedure_Specs (Tag_Typ : Entity_Id) return List_Id is
      Decl_List      : constant List_Id    := New_List;
      Loc            : constant Source_Ptr := Sloc (Tag_Typ);
      Formal         : Entity_Id;
      Formal_List    : List_Id;
      New_Param_Spec : Node_Id;
      Parent_Subp    : Entity_Id;
      Prim_Elmt      : Elmt_Id;
      Subp           : Entity_Id;

   begin
      Prim_Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Prim_Elmt) loop
         Subp := Node (Prim_Elmt);

         --  If a null procedure inherited from an interface has not been
         --  overridden, then we build a null procedure declaration to
         --  override the inherited procedure.

         Parent_Subp := Alias (Subp);

         if Present (Parent_Subp)
           and then Is_Null_Interface_Primitive (Parent_Subp)
         then
            Formal_List := No_List;
            Formal := First_Formal (Subp);

            if Present (Formal) then
               Formal_List := New_List;

               while Present (Formal) loop

                  --  Copy the parameter spec including default expressions

                  New_Param_Spec :=
                    New_Copy_Tree (Parent (Formal), New_Sloc => Loc);

                  --  Generate a new defining identifier for the new formal.
                  --  required because New_Copy_Tree does not duplicate
                  --  semantic fields (except itypes).

                  Set_Defining_Identifier (New_Param_Spec,
                    Make_Defining_Identifier (Sloc (Formal),
                      Chars => Chars (Formal)));

                  --  For controlling arguments we must change their
                  --  parameter type to reference the tagged type (instead
                  --  of the interface type)

                  if Is_Controlling_Formal (Formal) then
                     if Nkind (Parameter_Type (Parent (Formal)))
                       = N_Identifier
                     then
                        Set_Parameter_Type (New_Param_Spec,
                          New_Occurrence_Of (Tag_Typ, Loc));

                     else pragma Assert
                            (Nkind (Parameter_Type (Parent (Formal)))
                               = N_Access_Definition);
                        Set_Subtype_Mark (Parameter_Type (New_Param_Spec),
                          New_Occurrence_Of (Tag_Typ, Loc));
                     end if;
                  end if;

                  Append (New_Param_Spec, Formal_List);

                  Next_Formal (Formal);
               end loop;
            end if;

            Append_To (Decl_List,
              Make_Subprogram_Declaration (Loc,
                Make_Procedure_Specification (Loc,
                  Defining_Unit_Name =>
                    Make_Defining_Identifier (Loc, Chars (Subp)),
                  Parameter_Specifications => Formal_List,
                  Null_Present => True)));
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;

      return Decl_List;
   end Make_Null_Procedure_Specs;

   -------------------------------------
   -- Make_Predefined_Primitive_Specs --
   -------------------------------------

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Entity_Id)
   is
      function Is_Predefined_Eq_Renaming (Prim : Node_Id) return Boolean;
      --  Returns true if Prim is a renaming of an unresolved predefined
      --  equality operation.

      -------------------------------
      -- Is_Predefined_Eq_Renaming --
      -------------------------------

      function Is_Predefined_Eq_Renaming (Prim : Node_Id) return Boolean is
      begin
         return Chars (Prim) /= Name_Op_Eq
           and then Present (Alias (Prim))
           and then Comes_From_Source (Prim)
           and then Is_Intrinsic_Subprogram (Alias (Prim))
           and then Chars (Alias (Prim)) = Name_Op_Eq;
      end Is_Predefined_Eq_Renaming;

      --  Local variables

      Loc       : constant Source_Ptr := Sloc (Tag_Typ);
      Res       : constant List_Id    := New_List;
      Eq_Name   : Name_Id := Name_Op_Eq;
      Eq_Needed : Boolean;
      Eq_Spec   : Node_Id;
      Prim      : Elmt_Id;

      Has_Predef_Eq_Renaming : Boolean := False;
      --  Set to True if Tag_Typ has a primitive that renames the predefined
      --  equality operator. Used to implement (RM 8-5-4(8)).

   --  Start of processing for Make_Predefined_Primitive_Specs

   begin
      Renamed_Eq := Empty;

      --  Spec of _Size

      Append_To (Res, Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uSize,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Long_Long_Integer));

      --  Specs for dispatching stream attributes

      declare
         Stream_Op_TSS_Names :
           constant array (Integer range <>) of TSS_Name_Type :=
             (TSS_Stream_Read,
              TSS_Stream_Write,
              TSS_Stream_Input,
              TSS_Stream_Output);

      begin
         for Op in Stream_Op_TSS_Names'Range loop
            if Stream_Operation_OK (Tag_Typ, Stream_Op_TSS_Names (Op)) then
               Append_To (Res,
                 Predef_Stream_Attr_Spec (Loc, Tag_Typ,
                  Stream_Op_TSS_Names (Op)));
            end if;
         end loop;
      end;

      --  Spec of "=" is expanded if the type is not limited and if a user
      --  defined "=" was not already declared for the non-full view of a
      --  private extension

      if not Is_Limited_Type (Tag_Typ) then
         Eq_Needed := True;
         Prim := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Prim) loop

            --  If a primitive is encountered that renames the predefined
            --  equality operator before reaching any explicit equality
            --  primitive, then we still need to create a predefined equality
            --  function, because calls to it can occur via the renaming. A
            --  new name is created for the equality to avoid conflicting with
            --  any user-defined equality. (Note that this doesn't account for
            --  renamings of equality nested within subpackages???)

            if Is_Predefined_Eq_Renaming (Node (Prim)) then
               Has_Predef_Eq_Renaming := True;
               Eq_Name := New_External_Name (Chars (Node (Prim)), 'E');

            --  User-defined equality

            elsif Is_User_Defined_Equality (Node (Prim)) then
               if No (Alias (Node (Prim)))
                 or else Nkind (Unit_Declaration_Node (Node (Prim))) =
                           N_Subprogram_Renaming_Declaration
               then
                  Eq_Needed := False;
                  exit;

               --  If the parent is not an interface type and has an abstract
               --  equality function, the inherited equality is abstract as
               --  well, and no body can be created for it.

               elsif not Is_Interface (Etype (Tag_Typ))
                 and then Present (Alias (Node (Prim)))
                 and then Is_Abstract_Subprogram (Alias (Node (Prim)))
               then
                  Eq_Needed := False;
                  exit;

               --  If the type has an equality function corresponding with
               --  a primitive defined in an interface type, the inherited
               --  equality is abstract as well, and no body can be created
               --  for it.

               elsif Present (Alias (Node (Prim)))
                 and then Comes_From_Source (Ultimate_Alias (Node (Prim)))
                 and then
                   Is_Interface
                     (Find_Dispatching_Type (Ultimate_Alias (Node (Prim))))
               then
                  Eq_Needed := False;
                  exit;
               end if;
            end if;

            Next_Elmt (Prim);
         end loop;

         --  If a renaming of predefined equality was found but there was no
         --  user-defined equality (so Eq_Needed is still true), then set the
         --  name back to Name_Op_Eq. But in the case where a user-defined
         --  equality was located after such a renaming, then the predefined
         --  equality function is still needed, so Eq_Needed must be set back
         --  to True.

         if Eq_Name /= Name_Op_Eq then
            if Eq_Needed then
               Eq_Name := Name_Op_Eq;
            else
               Eq_Needed := True;
            end if;
         end if;

         if Eq_Needed then
            Eq_Spec := Predef_Spec_Or_Body (Loc,
              Tag_Typ => Tag_Typ,
              Name    => Eq_Name,
              Profile => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_X),
                    Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_Y),
                    Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),
                Ret_Type => Standard_Boolean);
            Append_To (Res, Eq_Spec);

            if Has_Predef_Eq_Renaming then
               Renamed_Eq := Defining_Unit_Name (Specification (Eq_Spec));

               Prim := First_Elmt (Primitive_Operations (Tag_Typ));
               while Present (Prim) loop

                  --  Any renamings of equality that appeared before an
                  --  overriding equality must be updated to refer to the
                  --  entity for the predefined equality, otherwise calls via
                  --  the renaming would get incorrectly resolved to call the
                  --  user-defined equality function.

                  if Is_Predefined_Eq_Renaming (Node (Prim)) then
                     Set_Alias (Node (Prim), Renamed_Eq);

                  --  Exit upon encountering a user-defined equality

                  elsif Chars (Node (Prim)) = Name_Op_Eq
                    and then No (Alias (Node (Prim)))
                  then
                     exit;
                  end if;

                  Next_Elmt (Prim);
               end loop;
            end if;
         end if;

         --  Spec for dispatching assignment

         Append_To (Res, Predef_Spec_Or_Body (Loc,
           Tag_Typ => Tag_Typ,
           Name    => Name_uAssign,
           Profile => New_List (
             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
               Out_Present         => True,
               Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),

             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
               Parameter_Type      => New_Reference_To (Tag_Typ, Loc)))));
      end if;

      --  Ada 2005: Generate declarations for the following primitive
      --  operations for limited interfaces and synchronized types that
      --  implement a limited interface.

      --    Disp_Asynchronous_Select
      --    Disp_Conditional_Select
      --    Disp_Get_Prim_Op_Kind
      --    Disp_Get_Task_Id
      --    Disp_Requeue
      --    Disp_Timed_Select

      --  Disable the generation of these bodies if No_Dispatching_Calls,
      --  Ravenscar or ZFP is active.

      if Ada_Version >= Ada_2005
        and then not Restriction_Active (No_Dispatching_Calls)
        and then not Restriction_Active (No_Select_Statements)
        and then RTE_Available (RE_Select_Specific_Data)
      then
         --  These primitives are defined abstract in interface types

         if Is_Interface (Tag_Typ)
           and then Is_Limited_Record (Tag_Typ)
         then
            Append_To (Res,
              Make_Abstract_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Asynchronous_Select_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Abstract_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Conditional_Select_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Abstract_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Get_Prim_Op_Kind_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Abstract_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Get_Task_Id_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Abstract_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Requeue_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Abstract_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Timed_Select_Spec (Tag_Typ)));

         --  If the ancestor is an interface type we declare non-abstract
         --  primitives to override the abstract primitives of the interface
         --  type.

         --  In VM targets we define these primitives in all root tagged types
         --  that are not interface types. Done because in VM targets we don't
         --  have secondary dispatch tables and any derivation of Tag_Typ may
         --  cover limited interfaces (which always have these primitives since
         --  they may be ancestors of synchronized interface types).

         elsif (not Is_Interface (Tag_Typ)
                 and then Is_Interface (Etype (Tag_Typ))
                 and then Is_Limited_Record (Etype (Tag_Typ)))
             or else
               (Is_Concurrent_Record_Type (Tag_Typ)
                 and then Has_Interfaces (Tag_Typ))
             or else
               (not Tagged_Type_Expansion
                 and then not Is_Interface (Tag_Typ)
                 and then Tag_Typ = Root_Type (Tag_Typ))
         then
            Append_To (Res,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Asynchronous_Select_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Conditional_Select_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Get_Prim_Op_Kind_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Get_Task_Id_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Requeue_Spec (Tag_Typ)));

            Append_To (Res,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Disp_Timed_Select_Spec (Tag_Typ)));
         end if;
      end if;

      --  All tagged types receive their own Deep_Adjust and Deep_Finalize
      --  regardless of whether they are controlled or may contain controlled
      --  components.

      --  Do not generate the routines if finalization is disabled

      if Restriction_Active (No_Finalization) then
         null;

      --  Finalization is not available for CIL value types

      elsif Is_Value_Type (Tag_Typ) then
         null;

      else
         if not Is_Limited_Type (Tag_Typ) then
            Append_To (Res, Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Adjust));
         end if;

         Append_To (Res, Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Finalize));
      end if;

      Predef_List := Res;
   end Make_Predefined_Primitive_Specs;

   ---------------------------------
   -- Needs_Simple_Initialization --
   ---------------------------------

   function Needs_Simple_Initialization
     (T           : Entity_Id;
      Consider_IS : Boolean := True) return Boolean
   is
      Consider_IS_NS : constant Boolean :=
                         Normalize_Scalars
                           or (Initialize_Scalars and Consider_IS);

   begin
      --  Never need initialization if it is suppressed

      if Initialization_Suppressed (T) then
         return False;
      end if;

      --  Check for private type, in which case test applies to the underlying
      --  type of the private type.

      if Is_Private_Type (T) then
         declare
            RT : constant Entity_Id := Underlying_Type (T);

         begin
            if Present (RT) then
               return Needs_Simple_Initialization (RT);
            else
               return False;
            end if;
         end;

      --  Scalar type with Default_Value aspect requires initialization

      elsif Is_Scalar_Type (T) and then Has_Default_Aspect (T) then
         return True;

      --  Cases needing simple initialization are access types, and, if pragma
      --  Normalize_Scalars or Initialize_Scalars is in effect, then all scalar
      --  types.

      elsif Is_Access_Type (T)
        or else (Consider_IS_NS and then (Is_Scalar_Type (T)))
      then
         return True;

      --  If Initialize/Normalize_Scalars is in effect, string objects also
      --  need initialization, unless they are created in the course of
      --  expanding an aggregate (since in the latter case they will be
      --  filled with appropriate initializing values before they are used).

      elsif Consider_IS_NS
        and then
          (Root_Type (T) = Standard_String
             or else Root_Type (T) = Standard_Wide_String
             or else Root_Type (T) = Standard_Wide_Wide_String)
        and then
          (not Is_Itype (T)
            or else Nkind (Associated_Node_For_Itype (T)) /= N_Aggregate)
      then
         return True;

      else
         return False;
      end if;
   end Needs_Simple_Initialization;

   ----------------------
   -- Predef_Deep_Spec --
   ----------------------

   function Predef_Deep_Spec
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : TSS_Name_Type;
      For_Body : Boolean := False) return Node_Id
   is
      Formals : List_Id;

   begin
      --  V : in out Tag_Typ

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          In_Present          => True,
          Out_Present         => True,
          Parameter_Type      => New_Reference_To (Tag_Typ, Loc)));

      --  F : Boolean := True

      if Name = TSS_Deep_Adjust
        or else Name = TSS_Deep_Finalize
      then
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_F),
             Parameter_Type      => New_Reference_To (Standard_Boolean, Loc),
             Expression          => New_Reference_To (Standard_True, Loc)));
      end if;

      return
        Predef_Spec_Or_Body (Loc,
          Name     => Make_TSS_Name (Tag_Typ, Name),
          Tag_Typ  => Tag_Typ,
          Profile  => Formals,
          For_Body => For_Body);

   exception
      when RE_Not_Available =>
         return Empty;
   end Predef_Deep_Spec;

   -------------------------
   -- Predef_Spec_Or_Body --
   -------------------------

   function Predef_Spec_Or_Body
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : Name_Id;
      Profile  : List_Id;
      Ret_Type : Entity_Id := Empty;
      For_Body : Boolean := False) return Node_Id
   is
      Id   : constant Entity_Id := Make_Defining_Identifier (Loc, Name);
      Spec : Node_Id;

   begin
      Set_Is_Public (Id, Is_Public (Tag_Typ));

      --  The internal flag is set to mark these declarations because they have
      --  specific properties. First, they are primitives even if they are not
      --  defined in the type scope (the freezing point is not necessarily in
      --  the same scope). Second, the predefined equality can be overridden by
      --  a user-defined equality, no body will be generated in this case.

      Set_Is_Internal (Id);

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Id);
      end if;

      if No (Ret_Type) then
         Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Id,
             Parameter_Specifications => Profile);
      else
         Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Id,
             Parameter_Specifications => Profile,
             Result_Definition        => New_Reference_To (Ret_Type, Loc));
      end if;

      if Is_Interface (Tag_Typ) then
         return Make_Abstract_Subprogram_Declaration (Loc, Spec);

      --  If body case, return empty subprogram body. Note that this is ill-
      --  formed, because there is not even a null statement, and certainly not
      --  a return in the function case. The caller is expected to do surgery
      --  on the body to add the appropriate stuff.

      elsif For_Body then
         return Make_Subprogram_Body (Loc, Spec, Empty_List, Empty);

      --  For the case of an Input attribute predefined for an abstract type,
      --  generate an abstract specification. This will never be called, but we
      --  need the slot allocated in the dispatching table so that attributes
      --  typ'Class'Input and typ'Class'Output will work properly.

      elsif Is_TSS (Name, TSS_Stream_Input)
        and then Is_Abstract_Type (Tag_Typ)
      then
         return Make_Abstract_Subprogram_Declaration (Loc, Spec);

      --  Normal spec case, where we return a subprogram declaration

      else
         return Make_Subprogram_Declaration (Loc, Spec);
      end if;
   end Predef_Spec_Or_Body;

   -----------------------------
   -- Predef_Stream_Attr_Spec --
   -----------------------------

   function Predef_Stream_Attr_Spec
     (Loc      : Source_Ptr;
      Tag_Typ  : Entity_Id;
      Name     : TSS_Name_Type;
      For_Body : Boolean := False) return Node_Id
   is
      Ret_Type : Entity_Id;

   begin
      if Name = TSS_Stream_Input then
         Ret_Type := Tag_Typ;
      else
         Ret_Type := Empty;
      end if;

      return
        Predef_Spec_Or_Body
          (Loc,
           Name     => Make_TSS_Name (Tag_Typ, Name),
           Tag_Typ  => Tag_Typ,
           Profile  => Build_Stream_Attr_Profile (Loc, Tag_Typ, Name),
           Ret_Type => Ret_Type,
           For_Body => For_Body);
   end Predef_Stream_Attr_Spec;

   ---------------------------------
   -- Predefined_Primitive_Bodies --
   ---------------------------------

   function Predefined_Primitive_Bodies
     (Tag_Typ    : Entity_Id;
      Renamed_Eq : Entity_Id) return List_Id
   is
      Loc       : constant Source_Ptr := Sloc (Tag_Typ);
      Res       : constant List_Id    := New_List;
      Decl      : Node_Id;
      Prim      : Elmt_Id;
      Eq_Needed : Boolean;
      Eq_Name   : Name_Id;
      Ent       : Entity_Id;

      pragma Warnings (Off, Ent);

   begin
      pragma Assert (not Is_Interface (Tag_Typ));

      --  See if we have a predefined "=" operator

      if Present (Renamed_Eq) then
         Eq_Needed := True;
         Eq_Name   := Chars (Renamed_Eq);

      --  If the parent is an interface type then it has defined all the
      --  predefined primitives abstract and we need to check if the type
      --  has some user defined "=" function to avoid generating it.

      elsif Is_Interface (Etype (Tag_Typ)) then
         Eq_Needed := True;
         Eq_Name := Name_Op_Eq;

         Prim := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Prim) loop
            if Chars (Node (Prim)) = Name_Op_Eq
              and then not Is_Internal (Node (Prim))
            then
               Eq_Needed := False;
               Eq_Name := No_Name;
               exit;
            end if;

            Next_Elmt (Prim);
         end loop;

      else
         Eq_Needed := False;
         Eq_Name   := No_Name;

         Prim := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Prim) loop
            if Chars (Node (Prim)) = Name_Op_Eq
              and then Is_Internal (Node (Prim))
            then
               Eq_Needed := True;
               Eq_Name := Name_Op_Eq;
               exit;
            end if;

            Next_Elmt (Prim);
         end loop;
      end if;

      --  Body of _Size

      Decl := Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uSize,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Long_Long_Integer,
        For_Body => True);

      Set_Handled_Statement_Sequence (Decl,
        Make_Handled_Sequence_Of_Statements (Loc, New_List (
          Make_Simple_Return_Statement (Loc,
            Expression =>
              Make_Attribute_Reference (Loc,
                Prefix          => Make_Identifier (Loc, Name_X),
                Attribute_Name  => Name_Size)))));

      Append_To (Res, Decl);

      --  Bodies for Dispatching stream IO routines. We need these only for
      --  non-limited types (in the limited case there is no dispatching).
      --  We also skip them if dispatching or finalization are not available.

      if Stream_Operation_OK (Tag_Typ, TSS_Stream_Read)
        and then No (TSS (Tag_Typ, TSS_Stream_Read))
      then
         Build_Record_Read_Procedure (Loc, Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      if Stream_Operation_OK (Tag_Typ, TSS_Stream_Write)
        and then No (TSS (Tag_Typ, TSS_Stream_Write))
      then
         Build_Record_Write_Procedure (Loc, Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      --  Skip body of _Input for the abstract case, since the corresponding
      --  spec is abstract (see Predef_Spec_Or_Body).

      if not Is_Abstract_Type (Tag_Typ)
        and then Stream_Operation_OK (Tag_Typ, TSS_Stream_Input)
        and then No (TSS (Tag_Typ, TSS_Stream_Input))
      then
         Build_Record_Or_Elementary_Input_Function
           (Loc, Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      if Stream_Operation_OK (Tag_Typ, TSS_Stream_Output)
        and then No (TSS (Tag_Typ, TSS_Stream_Output))
      then
         Build_Record_Or_Elementary_Output_Procedure
           (Loc, Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      --  Ada 2005: Generate bodies for the following primitive operations for
      --  limited interfaces and synchronized types that implement a limited
      --  interface.

      --    disp_asynchronous_select
      --    disp_conditional_select
      --    disp_get_prim_op_kind
      --    disp_get_task_id
      --    disp_timed_select

      --  The interface versions will have null bodies

      --  Disable the generation of these bodies if No_Dispatching_Calls,
      --  Ravenscar or ZFP is active.

      --  In VM targets we define these primitives in all root tagged types
      --  that are not interface types. Done because in VM targets we don't
      --  have secondary dispatch tables and any derivation of Tag_Typ may
      --  cover limited interfaces (which always have these primitives since
      --  they may be ancestors of synchronized interface types).

      if Ada_Version >= Ada_2005
        and then not Is_Interface (Tag_Typ)
        and then
          ((Is_Interface (Etype (Tag_Typ))
             and then Is_Limited_Record (Etype (Tag_Typ)))
           or else
             (Is_Concurrent_Record_Type (Tag_Typ)
               and then Has_Interfaces (Tag_Typ))
           or else
             (not Tagged_Type_Expansion
               and then Tag_Typ = Root_Type (Tag_Typ)))
        and then not Restriction_Active (No_Dispatching_Calls)
        and then not Restriction_Active (No_Select_Statements)
        and then RTE_Available (RE_Select_Specific_Data)
      then
         Append_To (Res, Make_Disp_Asynchronous_Select_Body (Tag_Typ));
         Append_To (Res, Make_Disp_Conditional_Select_Body  (Tag_Typ));
         Append_To (Res, Make_Disp_Get_Prim_Op_Kind_Body    (Tag_Typ));
         Append_To (Res, Make_Disp_Get_Task_Id_Body         (Tag_Typ));
         Append_To (Res, Make_Disp_Requeue_Body             (Tag_Typ));
         Append_To (Res, Make_Disp_Timed_Select_Body        (Tag_Typ));
      end if;

      if not Is_Limited_Type (Tag_Typ)
        and then not Is_Interface (Tag_Typ)
      then
         --  Body for equality

         if Eq_Needed then
            Decl := Make_Eq_Body (Tag_Typ, Eq_Name);
            Append_To (Res, Decl);
         end if;

         --  Body for inequality (if required!)

         Decl := Make_Neq_Body (Tag_Typ);

         if Present (Decl) then
            Append_To (Res, Decl);
         end if;

         --  Body for dispatching assignment

         Decl :=
           Predef_Spec_Or_Body (Loc,
             Tag_Typ => Tag_Typ,
             Name    => Name_uAssign,
             Profile => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
                 Out_Present         => True,
                 Parameter_Type      => New_Reference_To (Tag_Typ, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
                 Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),
             For_Body => True);

         Set_Handled_Statement_Sequence (Decl,
           Make_Handled_Sequence_Of_Statements (Loc, New_List (
             Make_Assignment_Statement (Loc,
               Name       => Make_Identifier (Loc, Name_X),
               Expression => Make_Identifier (Loc, Name_Y)))));

         Append_To (Res, Decl);
      end if;

      --  Generate empty bodies of routines Deep_Adjust and Deep_Finalize for
      --  tagged types which do not contain controlled components.

      --  Do not generate the routines if finalization is disabled

      if Restriction_Active (No_Finalization) then
         null;

      elsif not Has_Controlled_Component (Tag_Typ) then
         if not Is_Limited_Type (Tag_Typ) then
            Decl := Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Adjust, True);

            if Is_Controlled (Tag_Typ) then
               Set_Handled_Statement_Sequence (Decl,
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => New_List (
                     Make_Adjust_Call (
                       Obj_Ref => Make_Identifier (Loc, Name_V),
                       Typ     => Tag_Typ))));
            else
               Set_Handled_Statement_Sequence (Decl,
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => New_List (
                     Make_Null_Statement (Loc))));
            end if;

            Append_To (Res, Decl);
         end if;

         Decl := Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Finalize, True);

         if Is_Controlled (Tag_Typ) then
            Set_Handled_Statement_Sequence (Decl,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Final_Call
                    (Obj_Ref => Make_Identifier (Loc, Name_V),
                     Typ     => Tag_Typ))));
         else
            Set_Handled_Statement_Sequence (Decl,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (Make_Null_Statement (Loc))));
         end if;

         Append_To (Res, Decl);
      end if;

      return Res;
   end Predefined_Primitive_Bodies;

   ---------------------------------
   -- Predefined_Primitive_Freeze --
   ---------------------------------

   function Predefined_Primitive_Freeze
     (Tag_Typ : Entity_Id) return List_Id
   is
      Res     : constant List_Id := New_List;
      Prim    : Elmt_Id;
      Frnodes : List_Id;

   begin
      Prim := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Prim) loop
         if Is_Predefined_Dispatching_Operation (Node (Prim)) then
            Frnodes := Freeze_Entity (Node (Prim), Tag_Typ);

            if Present (Frnodes) then
               Append_List_To (Res, Frnodes);
            end if;
         end if;

         Next_Elmt (Prim);
      end loop;

      return Res;
   end Predefined_Primitive_Freeze;

   -------------------------
   -- Stream_Operation_OK --
   -------------------------

   function Stream_Operation_OK
     (Typ       : Entity_Id;
      Operation : TSS_Name_Type) return Boolean
   is
      Has_Predefined_Or_Specified_Stream_Attribute : Boolean := False;

   begin
      --  Special case of a limited type extension: a default implementation
      --  of the stream attributes Read or Write exists if that attribute
      --  has been specified or is available for an ancestor type; a default
      --  implementation of the attribute Output (resp. Input) exists if the
      --  attribute has been specified or Write (resp. Read) is available for
      --  an ancestor type. The last condition only applies under Ada 2005.

      if Is_Limited_Type (Typ)
        and then Is_Tagged_Type (Typ)
      then
         if Operation = TSS_Stream_Read then
            Has_Predefined_Or_Specified_Stream_Attribute :=
              Has_Specified_Stream_Read (Typ);

         elsif Operation = TSS_Stream_Write then
            Has_Predefined_Or_Specified_Stream_Attribute :=
              Has_Specified_Stream_Write (Typ);

         elsif Operation = TSS_Stream_Input then
            Has_Predefined_Or_Specified_Stream_Attribute :=
              Has_Specified_Stream_Input (Typ)
                or else
                  (Ada_Version >= Ada_2005
                    and then Stream_Operation_OK (Typ, TSS_Stream_Read));

         elsif Operation = TSS_Stream_Output then
            Has_Predefined_Or_Specified_Stream_Attribute :=
              Has_Specified_Stream_Output (Typ)
                or else
                  (Ada_Version >= Ada_2005
                    and then Stream_Operation_OK (Typ, TSS_Stream_Write));
         end if;

         --  Case of inherited TSS_Stream_Read or TSS_Stream_Write

         if not Has_Predefined_Or_Specified_Stream_Attribute
           and then Is_Derived_Type (Typ)
           and then (Operation = TSS_Stream_Read
                      or else Operation = TSS_Stream_Write)
         then
            Has_Predefined_Or_Specified_Stream_Attribute :=
              Present
                (Find_Inherited_TSS (Base_Type (Etype (Typ)), Operation));
         end if;
      end if;

      --  If the type is not limited, or else is limited but the attribute is
      --  explicitly specified or is predefined for the type, then return True,
      --  unless other conditions prevail, such as restrictions prohibiting
      --  streams or dispatching operations. We also return True for limited
      --  interfaces, because they may be extended by nonlimited types and
      --  permit inheritance in this case (addresses cases where an abstract
      --  extension doesn't get 'Input declared, as per comments below, but
      --  'Class'Input must still be allowed). Note that attempts to apply
      --  stream attributes to a limited interface or its class-wide type
      --  (or limited extensions thereof) will still get properly rejected
      --  by Check_Stream_Attribute.

      --  We exclude the Input operation from being a predefined subprogram in
      --  the case where the associated type is an abstract extension, because
      --  the attribute is not callable in that case, per 13.13.2(49/2). Also,
      --  we don't want an abstract version created because types derived from
      --  the abstract type may not even have Input available (for example if
      --  derived from a private view of the abstract type that doesn't have
      --  a visible Input), but a VM such as .NET or the Java VM can treat the
      --  operation as inherited anyway, and we don't want an abstract function
      --  to be (implicitly) inherited in that case because it can lead to a VM
      --  exception.

      --  Do not generate stream routines for type Finalization_Master because
      --  a master may never appear in types and therefore cannot be read or
      --  written.

      return
          (not Is_Limited_Type (Typ)
            or else Is_Interface (Typ)
            or else Has_Predefined_Or_Specified_Stream_Attribute)
        and then
          (Operation /= TSS_Stream_Input
            or else not Is_Abstract_Type (Typ)
            or else not Is_Derived_Type (Typ))
        and then not Has_Unknown_Discriminants (Typ)
        and then not
          (Is_Interface (Typ)
            and then
              (Is_Task_Interface (Typ)
                or else Is_Protected_Interface (Typ)
                or else Is_Synchronized_Interface (Typ)))
        and then not Restriction_Active (No_Streams)
        and then not Restriction_Active (No_Dispatch)
        and then not No_Run_Time_Mode
        and then RTE_Available (RE_Tag)
        and then No (Type_Without_Stream_Operation (Typ))
        and then RTE_Available (RE_Root_Stream_Type)
        and then not Is_RTE (Typ, RE_Finalization_Master);
   end Stream_Operation_OK;

end Exp_Ch3;
