------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Accessibility;  use Accessibility;
with Aspects;        use Aspects;
with Atree;          use Atree;
with Checks;         use Checks;
with Contracts;      use Contracts;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Exp_Aggr;       use Exp_Aggr;
with Exp_Atag;       use Exp_Atag;
with Exp_Ch4;        use Exp_Ch4;
with Exp_Ch6;        use Exp_Ch6;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Ch9;        use Exp_Ch9;
with Exp_Dbug;       use Exp_Dbug;
with Exp_Disp;       use Exp_Disp;
with Exp_Dist;       use Exp_Dist;
with Exp_Put_Image;
with Exp_Smem;       use Exp_Smem;
with Exp_Strm;       use Exp_Strm;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Ghost;          use Ghost;
with Lib;            use Lib;
with Mutably_Tagged; use Mutably_Tagged;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Attr;       use Sem_Attr;
with Sem_Cat;        use Sem_Cat;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Disp;       use Sem_Disp;
with Sem_Eval;       use Sem_Eval;
with Sem_Mech;       use Sem_Mech;
with Sem_Res;        use Sem_Res;
with Sem_SCIL;       use Sem_SCIL;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Stand;          use Stand;
with Snames;         use Snames;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Validsw;        use Validsw;

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

   function Build_Discriminant_Formals
     (Rec_Id : Entity_Id;
      Use_Dl : Boolean) return List_Id;
   --  This function uses the discriminants of a type to build a list of
   --  formal parameters, used in Build_Init_Procedure among other places.
   --  If the flag Use_Dl is set, the list is built using the already
   --  defined discriminals of the type, as is the case for concurrent
   --  types with discriminants. Otherwise new identifiers are created,
   --  with the source names of the discriminants.

   procedure Build_Discr_Checking_Funcs (N : Node_Id);
   --  For each variant component, builds a function which checks whether
   --  the component name is consistent with the current discriminants
   --  and sets the component's Dcheck_Function attribute to refer to it.
   --  N is the full type declaration node; the discriminant checking
   --  functions are inserted after this node.

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
   --  are active) can lead to very large blocks that GCC handles poorly.

   procedure Build_Untagged_Record_Equality (Typ : Entity_Id);
   --  AI05-0123: Equality on untagged records composes. This procedure
   --  builds the equality routine for an untagged record that has components
   --  of a record type that has user-defined primitive equality operations.
   --  The resulting operation is a TSS subprogram.

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

   procedure Copy_Discr_Checking_Funcs (N : Node_Id);
   --  For a derived untagged type, copy the attributes that were set
   --  for the components of the parent type onto the components of the
   --  derived type. No new subprograms are constructed.
   --  N is the full type declaration node, as for Build_Discr_Checking_Funcs.

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

   procedure Expand_Tagged_Root (T : Entity_Id);
   --  Add a field _Tag at the beginning of the record. This field carries
   --  the value of the access to the Dispatch table. This procedure is only
   --  called on root type, the _Tag field being inherited by the descendants.

   procedure Freeze_Stream_Operations (N : Node_Id; Typ : Entity_Id);
   --  Treat user-defined stream operations as renaming_as_body if the
   --  subprogram they rename is not frozen when the type is frozen.

   package Initialization_Control is

      function Requires_Late_Init
        (Decl : Node_Id; Rec_Type : Entity_Id) return Boolean;
      --  Return True iff the given component declaration requires late
      --  initialization, as defined by 3.3.1 (8.1/5).

      function Has_Late_Init_Component
        (Tagged_Rec_Type : Entity_Id) return Boolean;
      --  Return True iff the given tagged record type has at least one
      --  component that requires late initialization; this includes
      --  components of ancestor types.

      type Initialization_Mode is
        (Full_Init, Full_Init_Except_Tag, Early_Init_Only, Late_Init_Only);
      --  The initialization routine for a tagged type is passed in a
      --  formal parameter of this type, indicating what initialization
      --  is to be performed. This parameter defaults to Full_Init in all
      --  cases except when the init proc of a type extension (let's call
      --  that type T2) calls the init proc of its parent (let's call that
      --  type T1). In that case, one of the other 3 values will
      --  be passed in. In all three of those cases, the Tag component has
      --  already been initialized before the call and is therefore not to be
      --  modified. T2's init proc will either call T1's init proc
      --  once (with Full_Init_Except_Tag as the parameter value) or twice
      --  (first with Early_Init_Only, then later with Late_Init_Only),
      --  depending on the result returned by Has_Late_Init_Component (T1).
      --  In the latter case, the first call does not initialize any
      --  components that require late initialization and the second call
      --  then performs that deferred initialization.
      --  Strictly speaking, the formal parameter subtype is actually Natural
      --  but calls will only pass in values corresponding to literals
      --  of this enumeration type.

      function Make_Mode_Literal
        (Loc : Source_Ptr; Mode : Initialization_Mode) return Node_Id
      is (Make_Integer_Literal (Loc, Initialization_Mode'Pos (Mode)));
      --  Generate an integer literal for a given mode value.

      function Tag_Init_Condition
        (Loc : Source_Ptr;
         Init_Control_Formal : Entity_Id) return Node_Id;
      function Early_Init_Condition
        (Loc : Source_Ptr;
         Init_Control_Formal : Entity_Id) return Node_Id;
      function Late_Init_Condition
        (Loc : Source_Ptr;
         Init_Control_Formal : Entity_Id) return Node_Id;
      --  These three functions each return a Boolean expression that
      --  can be used to determine whether a given call to the initialization
      --  expression for a tagged type should initialize (respectively)
      --  the Tag component, the non-Tag components that do not require late
      --  initialization, and the components that do require late
      --  initialization.

   end Initialization_Control;

   procedure Initialization_Warning (E : Entity_Id);
   --  If static elaboration of the package is requested, indicate
   --  when a type does meet the conditions for static initialization. If
   --  E is a type, it has components that have no static initialization.
   --  if E is an entity, its initial expression is not compile-time known.

   function Init_Formals (Typ : Entity_Id; Proc_Id : Entity_Id) return List_Id;
   --  This function builds the list of formals for an initialization routine.
   --  The first formal is always _Init with the given type. For task value
   --  record types and types containing tasks, three additional formals are
   --  added and Proc_Id is decorated with attribute Has_Master_Entity:
   --
   --    _Master    : Master_Id
   --    _Chain     : in out Activation_Chain
   --    _Task_Name : String
   --
   --  The caller must append additional entries for discriminants if required.

   function Inline_Init_Proc (Typ : Entity_Id) return Boolean;
   --  Returns true if the initialization procedure of Typ should be inlined

   function In_Runtime (E : Entity_Id) return Boolean;
   --  Check if E is defined in the RTL (in a child of Ada or System). Used
   --  to avoid to bring in the overhead of _Input, _Output for tagged types.

   function Is_Null_Statement_List (Stmts : List_Id) return Boolean;
   --  Returns true if Stmts is made of null statements only, possibly wrapped
   --  in a case statement, recursively. This latter pattern may occur for the
   --  initialization procedure of an unchecked union.

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
   --  between the tagged and untagged case. Given a Component_List node CL,
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
   --  between the tagged and untagged case. Given the list of components
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
   --     typPI          provides result of 'Put_Image attribute
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
   --  The list of specs is returned in Predef_List

   function Has_New_Non_Standard_Rep (T : Entity_Id) return Boolean;
   --  Returns True if there are representation clauses for type T that are not
   --  inherited. If the result is false, the init_proc and the discriminant
   --  checking functions of the parent can be reused by a derived type.

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
     (Loc     : Source_Ptr;
      Tag_Typ : Entity_Id;
      Name    : TSS_Name_Type) return Node_Id;
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

         if not Is_Array_Type (Ctyp) or else Number_Dimensions (Ctyp) > 1 then
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

   ------------------------------------------
   -- Build_Access_Subprogram_Wrapper_Body --
   ------------------------------------------

   procedure Build_Access_Subprogram_Wrapper_Body
     (Decl     : Node_Id;
      New_Decl : Node_Id)
   is
      Loc       : constant Source_Ptr := Sloc (Decl);
      Actuals   : constant List_Id    := New_List;
      Type_Def  : constant Node_Id    := Type_Definition (Decl);
      Type_Id   : constant Entity_Id  := Defining_Identifier (Decl);
      Spec_Node : constant Node_Id    :=
                    Copy_Subprogram_Spec (Specification (New_Decl));
      --  This copy creates new identifiers for formals and subprogram.

      Act       : Node_Id;
      Body_Node : Node_Id;
      Call_Stmt : Node_Id;
      Ptr       : Entity_Id;

   begin
      --  Create List of actuals for indirect call. The last parameter of the
      --  subprogram declaration is the access value for the indirect call.

      Act := First (Parameter_Specifications (Spec_Node));

      while Present (Act) loop
         exit when Act = Last (Parameter_Specifications (Spec_Node));
         Append_To (Actuals,
           Make_Identifier (Loc, Chars (Defining_Identifier (Act))));
         Next (Act);
      end loop;

      Ptr :=
        Defining_Identifier
          (Last (Parameter_Specifications (Specification (New_Decl))));

      if Nkind (Type_Def) = N_Access_Procedure_Definition then
         Call_Stmt := Make_Procedure_Call_Statement (Loc,
           Name =>
              Make_Explicit_Dereference
                (Loc, New_Occurrence_Of (Ptr, Loc)),
           Parameter_Associations => Actuals);
      else
         Call_Stmt := Make_Simple_Return_Statement (Loc,
           Expression =>
             Make_Function_Call (Loc,
           Name => Make_Explicit_Dereference
                    (Loc, New_Occurrence_Of (Ptr, Loc)),
           Parameter_Associations => Actuals));
      end if;

      Body_Node := Make_Subprogram_Body (Loc,
        Specification => Spec_Node,
        Declarations  => New_List,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc,
            Statements    => New_List (Call_Stmt)));

      --  Place body in list of freeze actions for the type.

      Append_Freeze_Action (Type_Id, Body_Node);
   end Build_Access_Subprogram_Wrapper_Body;

   ---------------------------
   -- Build_Array_Init_Proc --
   ---------------------------

   procedure Build_Array_Init_Proc (A_Type : Entity_Id; Nod : Node_Id) is
      --  Obtain the corresponding mutably tagged type's parent subtype to
      --  handle default initialization.

      Comp_Type : constant Entity_Id :=
        Get_Corresponding_Tagged_Type_If_Present (Component_Type (A_Type));

      Comp_Simple_Init : constant Boolean :=
        Needs_Simple_Initialization
          (Typ         => Comp_Type,
           Consider_IS =>
             not (Validity_Check_Copies and Is_Bit_Packed_Array (A_Type)));
      --  True if the component needs simple initialization, based on its type,
      --  plus the fact that we do not do simple initialization for components
      --  of bit-packed arrays when validity checks are enabled, because the
      --  initialization with deliberately out-of-range values would raise
      --  Constraint_Error.

      Body_Stmts       : List_Id;
      Has_Default_Init : Boolean;
      Index_List       : List_Id;
      Loc              : Source_Ptr;
      Parameters       : List_Id;
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

         elsif Comp_Simple_Init then
            Set_Assignment_OK (Comp);
            return New_List (
              Make_Assignment_Statement (Loc,
                Name       => Comp,
                Expression =>
                  Get_Simple_Init_Val
                    (Typ  => Comp_Type,
                     N    => Nod,
                     Size => Component_Size (A_Type))));

         else
            Clean_Task_Names (Comp_Type, Proc_Id);
            return
              Build_Initialization_Call
                (N            => Nod,
                 Id_Ref       => Comp,
                 Typ          => Comp_Type,
                 In_Init_Proc => True,
                 Enclos_Type  => A_Type);
         end if;
      end Init_Component;

      ------------------------
      -- Init_One_Dimension --
      ------------------------

      function Init_One_Dimension (N : Int) return List_Id is
         Index       : Entity_Id;
         DIC_Call    : Node_Id;
         Result_List : List_Id;

         function Possible_DIC_Call return Node_Id;
         --  If the component type has Default_Initial_Conditions and a DIC
         --  procedure that is not an empty body, then builds a call to the
         --  DIC procedure and returns it.

         -----------------------
         -- Possible_DIC_Call --
         -----------------------

         function Possible_DIC_Call return Node_Id is
         begin
            --  When the component's type has a Default_Initial_Condition, then
            --  create a call for the DIC check.

            if Has_DIC (Comp_Type)
              --  In GNATprove mode, the component DICs are checked by other
              --  means. They should not be added to the record type DIC
              --  procedure, so that the procedure can be used to check the
              --  record type invariants or DICs if any.

              and then not GNATprove_Mode

              --  DIC checks for components of controlled types are done later
              --  (see Exp_Ch7.Make_Deep_Array_Body).

              and then not Is_Controlled (Comp_Type)

              and then Present (DIC_Procedure (Comp_Type))

              and then not Has_Null_Body (DIC_Procedure (Comp_Type))
            then
               return
                 Build_DIC_Call (Loc,
                   Make_Indexed_Component (Loc,
                     Prefix      => Make_Identifier (Loc, Name_uInit),
                     Expressions => Index_List),
                   Comp_Type);
            else
               return Empty;
            end if;
         end Possible_DIC_Call;

      --  Start of processing for Init_One_Dimension

      begin
         --  If the component does not need initializing, then there is nothing
         --  to do here, so we return a null body. This occurs when generating
         --  the dummy Init_Proc needed for Initialize_Scalars processing.
         --  An exception is if component type has a Default_Initial_Condition,
         --  in which case we generate a call to the type's DIC procedure.

         if not Has_Non_Null_Base_Init_Proc (Comp_Type)
           and then not Comp_Simple_Init
           and then not Has_Task (Comp_Type)
           and then not Has_Default_Aspect (A_Type)
           and then (not Has_DIC (Comp_Type)
                      or else N > Number_Dimensions (A_Type))
         then
            DIC_Call := Possible_DIC_Call;

            if Present (DIC_Call) then
               return New_List (DIC_Call);
            else
               return New_List (Make_Null_Statement (Loc));
            end if;

         --  If all dimensions dealt with, we simply initialize the component
         --  and append a call to component type's DIC procedure when needed.

         elsif N > Number_Dimensions (A_Type) then
            DIC_Call := Possible_DIC_Call;

            if Present (DIC_Call) then
               Result_List := Init_Component;
               Append (DIC_Call, Result_List);
               return Result_List;

            else
               return Init_Component;
            end if;

         --  Here we generate the required loop

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append (New_Occurrence_Of (Index, Loc), Index_List);

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
                              Make_Identifier (Loc, Name_uInit),
                            Attribute_Name  => Name_Range,
                            Expressions     => New_List (
                              Make_Integer_Literal (Loc, N))))),
                Statements       => Init_One_Dimension (N + 1)));
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
      --    2. An initialization already exists for the base type

      if Initialization_Suppressed (A_Type)
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
      --    6. The array component type has a Default_Initialization_Condition

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
                            or else Comp_Simple_Init
                            or else Has_Task (Comp_Type)
                            or else Has_Default_Aspect (A_Type)
                            or else Has_DIC (Comp_Type);

      if Has_Default_Init
        or else (not Restriction_Active (No_Initialize_Scalars)
                  and then Is_Public (A_Type)
                  and then not Is_Standard_String_Type (A_Type))
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
         Parameters := Init_Formals (A_Type, Proc_Id);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Proc_Id,
                 Parameter_Specifications => Parameters),
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Body_Stmts)));

         Mutate_Ekind       (Proc_Id, E_Procedure);
         Set_Is_Public      (Proc_Id, Is_Public (A_Type));
         Set_Is_Internal    (Proc_Id);
         Set_Has_Completion (Proc_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         --  Set Inlined on Init_Proc if it is set on the Init_Proc of the
         --  component type itself (see also Build_Record_Init_Proc).

         Set_Is_Inlined (Proc_Id, Inline_Init_Proc (Comp_Type));

         --  Associate Init_Proc with type, and determine if the procedure
         --  is null (happens because of the Initialize_Scalars pragma case,
         --  where we have to generate a null procedure in case it is called
         --  by a client with Initialize_Scalars set). Such procedures have
         --  to be generated, but do not have to be called, so we mark them
         --  as null to suppress the call. Kill also warnings for the _Init
         --  out parameter, which is left entirely uninitialized.

         Set_Init_Proc (A_Type, Proc_Id);

         if Is_Null_Statement_List (Body_Stmts) then
            Set_Is_Null_Init_Proc (Proc_Id);
            Set_Warnings_Off (Defining_Identifier (First (Parameters)));

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

   ----------------------------------
   -- Build_Default_Initialization --
   ----------------------------------

   function Build_Default_Initialization
     (N          : Node_Id;
      Typ        : Entity_Id;
      Obj_Id     : Entity_Id;
      For_CW     : Boolean := False;
      Target_Ref : Node_Id := Empty) return List_Id
   is
      Exceptions_OK : constant Boolean :=
                        not Restriction_Active (No_Exception_Propagation);
      Loc           : constant Source_Ptr := Sloc (N);

      function New_Object_Reference return Node_Id;
      --  Return either a reference to Obj_Id or a dereference of Obj_Id

      --------------------------
      -- New_Object_Reference --
      --------------------------

      function New_Object_Reference return Node_Id is
         Obj_Ref : Node_Id := New_Occurrence_Of (Obj_Id, Loc);

      begin
         if Nkind (N) = N_Object_Declaration then
            --  The call to the type init proc or [Deep_]Finalize must not
            --  freeze the object since the call is internally generated.
            --  This prevents representation clauses from being rejected.
            --  Note that the initialization call may be removed if pragma
            --  Import is encountered or moved to the freeze actions of
            --  the object if an address clause is encountered.

            Set_Assignment_OK   (Obj_Ref);
            Set_Must_Not_Freeze (Obj_Ref);

         else pragma Assert (Nkind (N) = N_Allocator);
            Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);

            --  If the designated subtype is unconstrained and the allocator
            --  specifies a constrained subtype, or such a subtype has been
            --  created, associate that subtype with the dereference of the
            --  allocator's access value. This is needed by the expander for
            --  cases where the access type has a Designated_Storage_Model
            --  in order to support allocation of a host object of the right
            --  size for passing to the initialization procedure.

            if not Is_Constrained (Designated_Type (Etype (N)))
              and then Is_Constrained (Typ)
            then
               Set_Actual_Designated_Subtype (Obj_Ref, Typ);
            end if;

            --  The initialization procedure expects a specific type so.
            --  if the context is access to class-wide, indicate that the
            --  object being initialized has the right specific type.

            if For_CW then
               Obj_Ref := Unchecked_Convert_To (Typ, Obj_Ref);
            end if;
         end if;

         return Obj_Ref;
      end New_Object_Reference;

      --  Local variables

      Comp_Init  : List_Id := No_List;
      Fin_Block  : Node_Id;
      Fin_Call   : Node_Id;
      Init_Stmts : List_Id := No_List;
      Obj_Init   : Node_Id := Empty;
      Obj_Ref    : Node_Id;

   --  Start of processing for Build_Default_Initialization

   begin
      --  The expansion performed by this routine is as follows:

      --    begin
      --       Abort_Defer;
      --       Type_Init_Proc (Obj);

      --       begin
      --          [Deep_]Initialize (Obj);

      --       exception
      --          when others =>
      --             [Deep_]Finalize (Obj, Self => False);
      --             raise;
      --       end;
      --    at end
      --       Abort_Undefer_Direct;
      --    end;

      --  Initialize the components of the object

      if Has_Non_Null_Base_Init_Proc (Typ)
        and then not Initialization_Suppressed (Typ)
      then
         --  Do not initialize the components if No_Default_Initialization
         --  applies as the actual restriction check will occur later when
         --  the object is frozen as it is not known yet whether the object
         --  is imported or not.

         if not Restriction_Active (No_Default_Initialization) then

            --  Invoke the type init proc, generate:
            --    Type_Init_Proc (Obj);

            Obj_Ref := New_Object_Reference;

            if Comes_From_Source (Obj_Id) then
               Initialization_Warning (Obj_Ref);
            end if;

            Comp_Init :=
              Build_Initialization_Call (N,
                Obj_Ref, Typ, Target_Ref => Target_Ref);
         end if;
      end if;

      --  Initialize the object, generate:
      --    [Deep_]Initialize (Obj);

      if Needs_Finalization (Typ) then
         Obj_Init :=
           Make_Init_Call
             (Obj_Ref => New_Object_Reference,
              Typ     => Typ);
      end if;

      --  Build a special finalization block when both the object and its
      --  controlled components are to be initialized. The block finalizes
      --  the components if the object initialization fails. Generate:

      --    begin
      --       <Obj_Init>

      --    exception
      --       when others =>
      --          <Fin_Call>
      --          raise;
      --    end;

      if Has_Controlled_Component (Typ)
        and then Present (Comp_Init)
        and then Present (Obj_Init)
        and then Exceptions_OK
      then
         Init_Stmts := Comp_Init;

         Fin_Call :=
           Make_Final_Call
             (Obj_Ref   => New_Object_Reference,
              Typ       => Typ,
              Skip_Self => True);

         if Present (Fin_Call) then

            --  Do not emit warnings related to the elaboration order when a
            --  controlled object is declared before the body of Finalize is
            --  seen.

            if Legacy_Elaboration_Checks then
               Set_No_Elaboration_Check (Fin_Call);
            end if;

            Fin_Block :=
              Make_Block_Statement (Loc,
                Declarations               => No_List,

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements         => New_List (Obj_Init),

                    Exception_Handlers => New_List (
                      Make_Exception_Handler (Loc,
                        Exception_Choices => New_List (
                          Make_Others_Choice (Loc)),

                        Statements        => New_List (
                          Fin_Call,
                          Make_Raise_Statement (Loc))))));

            --  Signal the ABE mechanism that the block carries out
            --  initialization actions.

            Set_Is_Initialization_Block (Fin_Block);

            Append_To (Init_Stmts, Fin_Block);
         end if;

      --  Otherwise finalization is not required, the initialization calls
      --  are passed to the abort block building circuitry, generate:

      --    Type_Init_Proc (Obj);
      --    [Deep_]Initialize (Obj);

      else
         if Present (Comp_Init) then
            Init_Stmts := Comp_Init;
         end if;

         if Present (Obj_Init) then
            if No (Init_Stmts) then
               Init_Stmts := New_List;
            end if;

            Append_To (Init_Stmts, Obj_Init);
         end if;
      end if;

      --  Build an abort block to protect the initialization calls, except for
      --  a finalization collection, which does not need any protection.

      if Abort_Allowed
        and then Present (Comp_Init)
        and then Present (Obj_Init)
        and then not Is_RTE (Typ, RE_Finalization_Collection)
      then
         --  Generate:
         --    Abort_Defer;

         Prepend_To (Init_Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));

         --  When exceptions are propagated, abort deferral must take place
         --  in the presence of initialization or finalization exceptions.
         --  Generate:

         --    begin
         --       Abort_Defer;
         --       <Init_Stmts>
         --    at end
         --       Abort_Undefer_Direct;
         --    end;

         if Exceptions_OK then
            Init_Stmts := New_List (
              Build_Abort_Undefer_Block (Loc,
                Stmts   => Init_Stmts,
                Context => N));

         --  Otherwise exceptions are not propagated. Generate:

         --    Abort_Defer;
         --    <Init_Stmts>
         --    Abort_Undefer;

         else
            Append_To (Init_Stmts,
              Build_Runtime_Call (Loc, RE_Abort_Undefer));
         end if;
      end if;

      return Init_Stmts;
   end Build_Default_Initialization;

   -----------------------------------------
   -- Build_Default_Simple_Initialization --
   -----------------------------------------

   function Build_Default_Simple_Initialization
     (N      : Node_Id;
      Typ    : Entity_Id;
      Obj_Id : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

      function Build_Equivalent_Aggregate return Node_Id;
      --  If the object has a constrained discriminated type and no initial
      --  value, it may be possible to build an equivalent aggregate instead,
      --  and prevent an actual call to the initialization procedure.

      function Simple_Initialization_OK (Typ : Entity_Id) return Boolean;
      --  Determine whether object declaration N with entity Obj_Id if set, or
      --  object allocation N if Obj_Id is empty, needs simple initialization,
      --  assuming that it is of type Typ.

      --------------------------------
      -- Build_Equivalent_Aggregate --
      --------------------------------

      function Build_Equivalent_Aggregate return Node_Id is
         Aggr     : Node_Id;
         Comp     : Entity_Id;
         Discr    : Elmt_Id;
         Full_Typ : Entity_Id;

      begin
         if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
            Full_Typ := Full_View (Typ);
         else
            Full_Typ := Typ;
         end if;

         --  Only do this transformation for a package entity of a constrained
         --  record type and if Elaboration_Code is forbidden or undesirable.

         --  If Initialize_Scalars might be active this transformation cannot
         --  be performed either, because it will lead to different semantics
         --  or because elaboration code will in fact be created.

         if Ekind (Full_Typ) /= E_Record_Subtype
           or else not Has_Discriminants (Full_Typ)
           or else not Is_Constrained (Full_Typ)
           or else Is_Controlled (Full_Typ)
           or else Is_Limited_Type (Full_Typ)
           or else Ekind (Current_Scope) /= E_Package
           or else not (Is_Preelaborated (Current_Scope)
                         or else Restriction_Active (No_Elaboration_Code))
           or else not Restriction_Active (No_Initialize_Scalars)
         then
            return Empty;
         end if;

         --  Building a static aggregate is possible if the discriminants
         --  have static values and the other components have static
         --  defaults or none.

         Discr := First_Elmt (Discriminant_Constraint (Full_Typ));
         while Present (Discr) loop
            if not Is_OK_Static_Expression (Node (Discr)) then
               return Empty;
            end if;

            Next_Elmt (Discr);
         end loop;

         --  Check that initialized components are OK, and that non-
         --  initialized components do not require a call to their own
         --  initialization procedure.

         Comp := First_Component (Full_Typ);
         while Present (Comp) loop
            if Present (Expression (Parent (Comp)))
              and then not Is_OK_Static_Expression (Expression (Parent (Comp)))
            then
               return Empty;

            elsif Has_Non_Null_Base_Init_Proc (Etype (Comp)) then
               return Empty;

            end if;

            Next_Component (Comp);
         end loop;

         --  Everything is static, assemble the aggregate, discriminant
         --  values first.

         Aggr :=
            Make_Aggregate (Loc,
             Expressions            => New_List,
              Component_Associations => New_List);
         Set_Parent (Aggr, N);

         Discr := First_Elmt (Discriminant_Constraint (Full_Typ));
         while Present (Discr) loop
            Append_To (Expressions (Aggr), New_Copy (Node (Discr)));
            Next_Elmt (Discr);
         end loop;

         --  Now collect values of initialized components

         Comp := First_Component (Full_Typ);
         while Present (Comp) loop
            if Present (Expression (Parent (Comp))) then
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

         if Typ /= Full_Typ then
            Analyze_And_Resolve (Aggr, Full_View (Base_Type (Full_Typ)));
            Rewrite (Aggr, Unchecked_Convert_To (Typ, Aggr));
         end if;

         return Aggr;
      end Build_Equivalent_Aggregate;

      ------------------------------
      -- Simple_Initialization_OK --
      ------------------------------

      function Simple_Initialization_OK (Typ : Entity_Id) return Boolean is
      begin
         --  Skip internal entities as specified in Einfo

         return
           not (Present (Obj_Id) and then Is_Internal (Obj_Id))
             and then not Is_Mutably_Tagged_CW_Equivalent_Type (Typ)
             and then
               Needs_Simple_Initialization
                 (Typ         => Typ,
                  Consider_IS =>
                    Initialize_Scalars
                      and then (No (Obj_Id)
                                 or else No (Following_Address_Clause (N))));
      end Simple_Initialization_OK;

      --  Local variables

      Aggr_Init  : Node_Id;

   --  Start of processing for Build_Default_Simple_Initialization

   begin
      if Has_Non_Null_Base_Init_Proc (Typ)
        and then not Is_Dispatching_Operation (Base_Init_Proc (Typ))
        and then not Initialization_Suppressed (Typ)
      then
         --  Do not initialize the components if No_Default_Initialization
         --  applies as the actual restriction check will occur later when
         --  the object is frozen as it is not known yet whether the object
         --  is imported or not.

         if not Restriction_Active (No_Default_Initialization) then

            --  If the values of the components are compile-time known, use
            --  their prebuilt aggregate form directly.

            Aggr_Init := Static_Initialization (Base_Init_Proc (Typ));
            if Present (Aggr_Init) then
               return New_Copy_Tree (Aggr_Init, New_Scope => Current_Scope);
            end if;

            --  If type has discriminants, try to build an equivalent
            --  aggregate using discriminant values from the declaration.
            --  This is a useful optimization, in particular if restriction
            --  No_Elaboration_Code is active.

            Aggr_Init := Build_Equivalent_Aggregate;
            if Present (Aggr_Init) then
               return Aggr_Init;
            end if;

            --  Optimize the default initialization of an array object when
            --  pragma Initialize_Scalars or Normalize_Scalars is in effect.
            --  Construct an in-place initialization aggregate which may be
            --  convert into a fast memset by the backend.

            if Init_Or_Norm_Scalars
              and then Is_Array_Type (Typ)

              --  The array must lack atomic components because they are
              --  treated as non-static, and as a result the backend will
              --  not initialize the memory in one go.

              and then not Has_Atomic_Components (Typ)

              --  The array must not be packed because the invalid values
              --  in System.Scalar_Values are multiples of Storage_Unit.

              and then not Is_Packed (Typ)

              --  The array must have static non-empty ranges, otherwise
              --  the backend cannot initialize the memory in one go.

              and then Has_Static_Non_Empty_Array_Bounds (Typ)

              --  The optimization is only relevant for arrays of scalar
              --  types.

              and then Is_Scalar_Type (Component_Type (Typ))

              --  Similar to regular array initialization using a type
              --  init proc, predicate checks are not performed because the
              --  initialization values are intentionally invalid, and may
              --  violate the predicate.

              and then not Has_Predicates (Component_Type (Typ))

              --  Array default component value takes precedence over
              --  Init_Or_Norm_Scalars.

              and then No (Find_Aspect (Typ, Aspect_Default_Component_Value))

              --  The component type must have a single initialization value

              and then Simple_Initialization_OK (Component_Type (Typ))
            then
               return
                 Get_Simple_Init_Val
                   (Typ  => Typ,
                    N    => N,
                    Size => (if Known_Esize (Typ)
                             then Esize (Typ)
                             else Uint_0));
            end if;
         end if;

      --  Provide a default value if the object needs simple initialization

      elsif Simple_Initialization_OK (Typ) then
         return
           Get_Simple_Init_Val
             (Typ  => Typ,
              N    => N,
              Size => (if Known_Esize (Typ)
                       then Esize (Typ)
                       else Uint_0));
      end if;

      return Empty;
   end Build_Default_Simple_Initialization;

   --------------------------------
   -- Build_Discr_Checking_Funcs --
   --------------------------------

   procedure Build_Discr_Checking_Funcs (N : Node_Id) is
      Rec_Id            : Entity_Id;
      Loc               : Source_Ptr;
      Enclosing_Func_Id : Entity_Id;
      Sequence          : Nat := 1;
      Type_Def          : Node_Id;
      V                 : Node_Id;

      function Build_Case_Statement
        (Case_Id : Entity_Id;
         Variant : Node_Id) return Node_Id;
      --  Build a case statement containing only two alternatives. The first
      --  alternative corresponds to the discrete choices given on the variant
      --  that contains the components that we are generating the checks
      --  for. If the discriminant is one of these return False. The second
      --  alternative is an OTHERS choice that returns True indicating the
      --  discriminant did not match.

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
         Set_End_Span (Case_Node, Uint_0);

         --  Replace the discriminant which controls the variant with the name
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
                         New_Occurrence_Of (Enclosing_Func_Id,  Loc),
                       Parameter_Associations =>
                         Actuals_List));

            else
               Return_Node :=
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     New_Occurrence_Of (Standard_False, Loc));
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
               New_Occurrence_Of (Standard_True, Loc));

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
         Body_Node      : Node_Id;
         Func_Id        : Entity_Id;
         Parameter_List : List_Id;
         Spec_Node      : Node_Id;

      begin
         Body_Node := New_Node (N_Subprogram_Body, Loc);
         Sequence := Sequence + 1;

         Func_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Rec_Id), 'D', Sequence));
         Set_Is_Discriminant_Check_Function (Func_Id);

         Spec_Node := New_Node (N_Function_Specification, Loc);
         Set_Defining_Unit_Name (Spec_Node, Func_Id);

         Parameter_List := Build_Discriminant_Formals (Rec_Id, False);

         Set_Parameter_Specifications (Spec_Node, Parameter_List);
         Set_Result_Definition (Spec_Node,
                                New_Occurrence_Of (Standard_Boolean,  Loc));
         Set_Specification (Body_Node, Spec_Node);
         Set_Declarations (Body_Node, New_List);

         Set_Handled_Statement_Sequence (Body_Node,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List (
               Build_Case_Statement (Case_Id, Variant))));

         Mutate_Ekind    (Func_Id, E_Function);
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

         Discr_Name := Entity (Name (Variant_Part_Node));
         Variant := First_Non_Pragma (Variants (Variant_Part_Node));

         while Present (Variant) loop
            Component_List_Node := Component_List (Variant);

            if not Null_Present (Component_List_Node) then
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

   ----------------------------------------
   -- Build_Or_Copy_Discr_Checking_Funcs --
   ----------------------------------------

   procedure Build_Or_Copy_Discr_Checking_Funcs (N : Node_Id) is
      Typ : constant Entity_Id := Defining_Identifier (N);
   begin
      if Is_Unchecked_Union (Typ) or else not Has_Discriminants (Typ) then
         null;
      elsif not Is_Derived_Type (Typ)
        or else Has_New_Non_Standard_Rep (Typ)
        or else Is_Tagged_Type (Typ)
      then
         Build_Discr_Checking_Funcs (N);
      else
         Copy_Discr_Checking_Funcs (N);
      end if;
   end Build_Or_Copy_Discr_Checking_Funcs;

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
                  New_Occurrence_Of (Formal_Type, Loc));
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

   begin
      if not Is_Record_Type (T)
        or else Has_Discriminants (T)
        or else Is_Limited_Type (T)
        or else Has_Non_Standard_Rep (T)
        or else Needs_Finalization (T)
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
                 (not Compile_Time_Known_Value (Type_Low_Bound  (Comp_Type))
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

               --  We need to return empty if the type has predicates because
               --  this would otherwise duplicate calls to the predicate
               --  function. If the type hasn't been frozen before being
               --  referenced in the current record, the extraneous call to
               --  the predicate function would be inserted somewhere before
               --  the predicate function is elaborated, which would result in
               --  an invalid tree.

            elsif Has_Predicates (Etype (Comp)) then
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
              or else Has_Predicates (Etype (Comp))
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
     (N                   : Node_Id;
      Id_Ref              : Node_Id;
      Typ                 : Entity_Id;
      In_Init_Proc        : Boolean   := False;
      Enclos_Type         : Entity_Id := Empty;
      Target_Ref          : Node_Id   := Empty;
      Discr_Map           : Elist_Id  := New_Elmt_List;
      With_Default_Init   : Boolean   := False;
      Constructor_Ref     : Node_Id   := Empty;
      Init_Control_Actual : Entity_Id := Empty) return List_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
      Res : constant List_Id    := New_List;

      Full_Type : Entity_Id;

      procedure Check_Predicated_Discriminant
        (Val   : Node_Id;
         Discr : Entity_Id);
      --  Discriminants whose subtypes have predicates are checked in two
      --  cases:
      --    a) When an object is default-initialized and assertions are enabled
      --       we check that the value of the discriminant obeys the predicate.

      --    b) In all cases, if the discriminant controls a variant and the
      --       variant has no others_choice, Constraint_Error must be raised if
      --       the predicate is violated, because there is no variant covered
      --       by the illegal discriminant value.

      -----------------------------------
      -- Check_Predicated_Discriminant --
      -----------------------------------

      procedure Check_Predicated_Discriminant
        (Val   : Node_Id;
         Discr : Entity_Id)
      is
         Typ : constant Entity_Id := Etype (Discr);

         procedure Check_Missing_Others (V : Node_Id);
         --  Check that a given variant and its nested variants have an others
         --  choice, and generate a constraint error raise when it does not.

         --------------------------
         -- Check_Missing_Others --
         --------------------------

         procedure Check_Missing_Others (V : Node_Id) is
            Alt      : Node_Id;
            Choice   : Node_Id;
            Last_Var : Node_Id;

         begin
            Last_Var := Last_Non_Pragma (Variants (V));
            Choice   := First (Discrete_Choices (Last_Var));

            --  An others_choice is added during expansion for gcc use, but
            --  does not cover the illegality.

            if Entity (Name (V)) = Discr then
               if Present (Choice)
                 and then (Nkind (Choice) /= N_Others_Choice
                            or else not Comes_From_Source (Choice))
               then
                  Check_Expression_Against_Static_Predicate (Val, Typ);

                  if not Is_Static_Expression (Val) then
                     Prepend_To (Res,
                        Make_Raise_Constraint_Error (Loc,
                          Condition =>
                            Make_Op_Not (Loc,
                              Right_Opnd => Make_Predicate_Call (Typ, Val)),
                          Reason    => CE_Invalid_Data));
                  end if;
               end if;
            end if;

            --  Check whether some nested variant is ruled by the predicated
            --  discriminant.

            Alt := First (Variants (V));
            while Present (Alt) loop
               if Nkind (Alt) = N_Variant
                 and then Present (Variant_Part (Component_List (Alt)))
               then
                  Check_Missing_Others
                    (Variant_Part (Component_List (Alt)));
               end if;

               Next (Alt);
            end loop;
         end Check_Missing_Others;

         --  Local variables

         Def : Node_Id;

      --  Start of processing for Check_Predicated_Discriminant

      begin
         if Ekind (Base_Type (Full_Type)) = E_Record_Type then
            Def := Type_Definition (Parent (Base_Type (Full_Type)));
         else
            return;
         end if;

         if Policy_In_Effect (Name_Assert) = Name_Check
           and then not Predicates_Ignored (Etype (Discr))
         then
            Prepend_To (Res, Make_Predicate_Check (Typ, Val));
         end if;

         --  If discriminant controls a variant, verify that predicate is
         --  obeyed or else an Others_Choice is present.

         if Nkind (Def) = N_Record_Definition
           and then Present (Variant_Part (Component_List (Def)))
           and then Policy_In_Effect (Name_Assert) = Name_Ignore
         then
            Check_Missing_Others (Variant_Part (Component_List (Def)));
         end if;
      end Check_Predicated_Discriminant;

      --  Local variables

      A_Type         : Entity_Id;
      Arg            : Node_Id;
      Args           : List_Id;
      Decls          : List_Id;
      Decl           : Node_Id;
      Discr          : Entity_Id;
      First_Arg      : Node_Id;
      Full_Init_Type : Entity_Id;
      Init_Call      : Node_Id;
      Init_Type      : Entity_Id;
      Proc           : Entity_Id;

   --  Start of processing for Build_Initialization_Call

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

      if Is_Null_Init_Proc (Proc) and then not Init_Or_Norm_Scalars then
         return Empty_List;

      --  Nothing to do for an array of controlled components that have only
      --  the inherited Initialize primitive. This is a useful optimization
      --  for CodePeer.

      elsif Is_Trivial_Subprogram (Proc)
        and then Is_Array_Type (Full_Init_Type)
      then
         return New_List (Make_Null_Statement (Loc));
      end if;

      --  Use the [underlying] full view when dealing with a private type. This
      --  may require several steps depending on derivations.

      Full_Type := Typ;
      loop
         if Is_Private_Type (Full_Type) then
            if Present (Full_View (Full_Type)) then
               Full_Type := Full_View (Full_Type);

            elsif Present (Underlying_Full_View (Full_Type)) then
               Full_Type := Underlying_Full_View (Full_Type);

            --  When a private type acts as a generic actual and lacks a full
            --  view, use the base type.

            elsif Is_Generic_Actual_Type (Full_Type) then
               Full_Type := Base_Type (Full_Type);

            elsif Ekind (Full_Type) = E_Private_Subtype
              and then (not Has_Discriminants (Full_Type)
                         or else No (Discriminant_Constraint (Full_Type)))
            then
               Full_Type := Etype (Full_Type);

            --  The loop has recovered the [underlying] full view, stop the
            --  traversal.

            else
               exit;
            end if;

         --  The type is not private, nothing to do

         else
            exit;
         end if;
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
            Append_To (Args, Make_Integer_Literal (Loc, Library_Task_Level));
         elsif Present (Target_Ref) then
            Append_To (Args,
              New_Occurrence_Of
                (Master_Id (Base_Type (Root_Type (Etype (Target_Ref)))), Loc));
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
            Append_To (Args, Make_String_Literal (Loc, Strval => ""));

         else
            if Present (Enclos_Type) then
               A_Type := Enclos_Type;

            elsif Present (Target_Ref)
              and then Nkind (Target_Ref) in N_Indexed_Component
                                           | N_Selected_Component
            then
               A_Type := Etype (Prefix (Target_Ref));

            else
               A_Type := Full_Type;
            end if;

            Decls :=
              Build_Task_Image_Decls (Loc,
                (if Present (Target_Ref) then Target_Ref else Id_Ref),
                A_Type,
                In_Init_Proc);
            Decl  := Last (Decls);

            Append_To (Args,
              New_Occurrence_Of (Defining_Identifier (Decl), Loc));
            Append_List (Decls, Res);
         end if;

      else
         Decls := No_List;
         Decl  := Empty;
      end if;

      --  Handle the optionally generated formal *_skip_null_excluding_checks

      --  Look at the associated node for the object we are referencing and
      --  verify that we are expanding a call to an Init_Proc for an internally
      --  generated object declaration before passing True and skipping the
      --  relevant checks.

      if Needs_Conditional_Null_Excluding_Check (Full_Init_Type)
        and then Nkind (Id_Ref) in N_Has_Entity
        and then (Comes_From_Source (Id_Ref)
                   or else (Present (Associated_Node (Id_Ref))
                             and then Comes_From_Source
                                        (Associated_Node (Id_Ref))))
      then
         Append_To (Args, New_Occurrence_Of (Standard_True, Loc));
      end if;

      --  Add discriminant values if discriminants are present

      if Has_Discriminants (Full_Init_Type) then
         --  If an allocated object will be constrained by the default
         --  values for discriminants, then build a subtype with those
         --  defaults, and change the allocated subtype to that. Note
         --  that this happens in fewer cases in Ada 2005 (AI95-0363).

         if Nkind (N) = N_Allocator
           and then not Is_Constrained (Full_Type)
           and then
             Present
               (Discriminant_Default_Value (First_Discriminant (Full_Type)))
           and then (Ada_Version < Ada_2005
                      or else not Object_Type_Has_Constrained_Partial_View
                                    (Full_Type, Current_Scope))
         then
            Full_Type := Build_Default_Subtype (Full_Type, N);
            Set_Expression (N, New_Occurrence_Of (Full_Type, Loc));
         end if;

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
                  Arg := New_Occurrence_Of (Discriminal (Entity (Arg)), Loc);

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
                  --  In addition, if a predicate check is needed on the value
                  --  of the discriminant, insert it ahead of the call.

                  Arg := New_Copy_Tree (Arg, New_Sloc => Loc);
               end if;

               if Has_Predicates (Etype (Discr)) then
                  Check_Predicated_Discriminant (Arg, Discr);
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
                   Prefix        => New_Copy_Tree (Prefix (Id_Ref)),
                   Selector_Name => Arg));
            else
               Append_To (Args, Arg);
            end if;

            Next_Discriminant (Discr);
         end loop;
      end if;

      --  If this is a call to initialize the parent component of a derived
      --  tagged type, indicate that the tag should not be set in the parent.
      --  This is done via the actual parameter value for the Init_Control
      --  formal parameter, which is also used to deal with late initialization
      --  requirements.
      --
      --  We pass in Full_Init_Except_Tag unless the caller tells us to do
      --  otherwise (by passing in a nonempty Init_Control_Actual parameter).

      if Is_Tagged_Type (Full_Init_Type)
        and then not Is_CPP_Class (Full_Init_Type)
        and then Nkind (Id_Ref) = N_Selected_Component
        and then Chars (Selector_Name (Id_Ref)) = Name_uParent
      then
         declare
            use Initialization_Control;
         begin
            Append_To (Args,
              (if Present (Init_Control_Actual)
               then Init_Control_Actual
               else Make_Mode_Literal (Loc, Full_Init_Except_Tag)));
         end;
      elsif Present (Constructor_Ref) then
         Append_List_To (Args,
           New_Copy_List (Parameter_Associations (Constructor_Ref)));
      end if;

      --  Pass the extra accessibility level parameter associated with the
      --  level of the object being initialized when required.

      if Is_Entity_Name (Id_Ref)
        and then Present (Init_Proc_Level_Formal (Proc))
      then
         Append_To (Args,
           Make_Parameter_Association (Loc,
             Selector_Name             =>
               Make_Identifier (Loc, Name_uInit_Level),
             Explicit_Actual_Parameter =>
               Accessibility_Level (Id_Ref, Dynamic_Level)));
      end if;

      Append_To (Res,
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc, Loc),
          Parameter_Associations => Args));

      if Needs_Finalization (Typ)
        and then Nkind (Id_Ref) = N_Selected_Component
      then
         if Chars (Selector_Name (Id_Ref)) /= Name_uParent then
            Init_Call :=
              Make_Init_Call
                (Obj_Ref => New_Copy_Tree (First_Arg),
                 Typ     => Typ);

            --  Guard against a missing [Deep_]Initialize when the type was not
            --  properly frozen.

            if Present (Init_Call) then
               Append_To (Res, Init_Call);
            end if;
         end if;
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
      Counter   : Nat := 0;
      Proc_Id   : Entity_Id;
      Rec_Type  : Entity_Id;

      Init_Control_Formal : Entity_Id := Empty; -- set in Build_Init_Statements
      Has_Late_Init_Comp  : Boolean := False;   -- set in Build_Init_Statements

      function Build_Assignment
        (Id      : Entity_Id;
         Default : Node_Id) return List_Id;
      --  Build an assignment statement that assigns the default expression to
      --  its corresponding record component if defined. The left-hand side of
      --  the assignment is marked Assignment_OK so that initialization of
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
      --  Given an untagged type-derivation that declares discriminants, e.g.
      --
      --     type R (R1, R2 : Integer) is record ... end record;
      --     type D (D1 : Integer) is new R (1, D1);
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

      function Build_Assignment
        (Id      : Entity_Id;
         Default : Node_Id) return List_Id
      is
         Default_Loc : constant Source_Ptr := Sloc (Default);
         Typ         : constant Entity_Id  := Underlying_Type (Etype (Id));

         Adj_Call : Node_Id;
         Exp      : Node_Id;
         Exp_Q    : Node_Id;
         Lhs      : Node_Id;
         Res      : List_Id;

      begin
         Lhs :=
           Make_Selected_Component (Default_Loc,
             Prefix        => Make_Identifier (Loc, Name_uInit),
             Selector_Name => New_Occurrence_Of (Id, Default_Loc));
         Set_Assignment_OK (Lhs);

         --  Take copy of Default to ensure that later copies of this component
         --  declaration in derived types see the original tree, not a node
         --  rewritten during expansion of the init_proc. If the copy contains
         --  itypes, the scope of the new itypes is the init_proc being built.

         declare
            Map : Elist_Id := No_Elist;

         begin
            if Has_Late_Init_Comp then
               --  Map the type to the _Init parameter in order to
               --  handle "current instance" references.

               Map := New_Elmt_List
                        (Elmt1 => Rec_Type,
                         Elmt2 => Defining_Identifier (First
                                   (Parameter_Specifications
                                      (Parent (Proc_Id)))));

               --  If the type has an incomplete view, a current instance
               --  may have an incomplete type. In that case, it must also be
               --  replaced by the formal of the Init_Proc.

               if Nkind (Parent (Rec_Type)) = N_Full_Type_Declaration
                 and then Present (Incomplete_View (Parent (Rec_Type)))
               then
                  Append_Elmt (
                    N  => Incomplete_View (Parent (Rec_Type)),
                    To => Map);
                  Append_Elmt (
                    N  => Defining_Identifier
                            (First
                              (Parameter_Specifications
                                (Parent (Proc_Id)))),
                    To => Map);
               end if;
            end if;

            Exp := New_Copy_Tree (Default, New_Scope => Proc_Id, Map => Map);
         end;

         Res := New_List (
           Make_Assignment_Statement (Loc,
             Name       => Lhs,
             Expression => Exp));

         Set_No_Ctrl_Actions (First (Res));

         Exp_Q := Unqualify (Exp);

         --  Adjust the tag if tagged (because of possible view conversions).
         --  Suppress the tag adjustment when not Tagged_Type_Expansion because
         --  tags are represented implicitly in objects, and when the record is
         --  initialized with a raise expression.

         if Is_Tagged_Type (Typ)
           and then Tagged_Type_Expansion
           and then Nkind (Exp_Q) /= N_Raise_Expression
         then
            --  Get the relevant type for the call to
            --  Make_Tag_Assignment_From_Type, which, for concurrent types is
            --  their corresponding record.

            declare
               T : Entity_Id := Underlying_Type (Typ);
            begin
               if Ekind (T) in E_Protected_Type | E_Task_Type then
                  T := Corresponding_Record_Type (T);
               end if;

               Append_To (Res,
                 Make_Tag_Assignment_From_Type
                   (Default_Loc,
                    New_Copy_Tree (Lhs, New_Scope => Proc_Id),
                    T));
            end;
         end if;

         --  Adjust the component if controlled except if it is an aggregate
         --  that will be expanded inline (but note that the case of container
         --  aggregates does require component adjustment).

         if Needs_Finalization (Typ)
           and then (Nkind (Exp_Q) not in N_Aggregate | N_Extension_Aggregate
                      or else Is_Container_Aggregate (Exp_Q))
           and then not Is_Build_In_Place_Function_Call (Exp)
         then
            Adj_Call :=
              Make_Adjust_Call
                (Obj_Ref => New_Copy_Tree (Lhs),
                 Typ     => Etype (Id));

            --  Guard against a missing [Deep_]Adjust when the component type
            --  was not properly frozen.

            if Present (Adj_Call) then
               Append_To (Res, Adj_Call);
            end if;
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
                      New_Occurrence_Of (Discriminal (D), D_Loc)));
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
             New_Occurrence_Of
               (Defining_Identifier (First (Parameters)), Loc));

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
               Append_To
                 (Args, Make_Integer_Literal (Loc, Library_Task_Level));
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
                    New_Occurrence_Of (Discriminal (Entity (Arg)), Loc));

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
                  New_Occurrence_Of (RTE (RE_Address), Loc))));
            Set_Result_Definition (Spec_Node,
              New_Occurrence_Of (RTE (RE_Storage_Offset), Loc));

            --  Generate
            --    function Fxx (O : in Rec_Typ) return Storage_Offset is
            --    begin
            --       return -O.Iface_Comp'Position;
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
                      New_Occurrence_Of (Rec_Type, Loc)))));

            Set_Handled_Statement_Sequence (Body_Node,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements     => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression =>
                      Make_Op_Minus (Loc,
                        Make_Attribute_Reference (Loc,
                          Prefix         =>
                            Make_Selected_Component (Loc,
                              Prefix        =>
                                Make_Explicit_Dereference (Loc,
                                  Unchecked_Convert_To (Acc_Type,
                                    Make_Identifier (Loc, Name_uO))),
                              Selector_Name =>
                                New_Occurrence_Of (Iface_Comp, Loc)),
                          Attribute_Name => Name_Position))))));

            Mutate_Ekind    (Func_Id, E_Function);
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
         --  Nothing is needed either in case of virtual targets, since
         --  interfaces are handled directly by the target.

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

         Append_Freeze_Action (Rec_Type,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flag_Id,
             Object_Definition =>
               New_Occurrence_Of (Standard_Boolean, Loc),
             Expression =>
               New_Occurrence_Of (Standard_True, Loc)));

         Body_Stmts := New_List;
         Body_Node  := New_Node (N_Subprogram_Body, Loc);

         Proc_Spec_Node := New_Node (N_Procedure_Specification, Loc);

         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Make_TSS_Name (Rec_Type, TSS_CPP_Init_Proc));

         Mutate_Ekind    (Proc_Id, E_Procedure);
         Set_Is_Internal (Proc_Id);

         Set_Defining_Unit_Name (Proc_Spec_Node, Proc_Id);

         Set_Parameter_Specifications (Proc_Spec_Node, New_List);
         Set_Specification (Body_Node, Proc_Spec_Node);
         Set_Declarations  (Body_Node, New_List);

         Init_Tags_List := Build_Inherit_CPP_Prims (Rec_Type);

         Append_To (Init_Tags_List,
           Make_Assignment_Statement (Loc,
             Name =>
               New_Occurrence_Of (Flag_Id, Loc),
             Expression =>
               New_Occurrence_Of (Standard_False, Loc)));

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

         use Initialization_Control;
      begin
         Body_Stmts := New_List;
         Body_Node := New_Node (N_Subprogram_Body, Loc);
         Mutate_Ekind (Proc_Id, E_Procedure);

         Proc_Spec_Node := New_Node (N_Procedure_Specification, Loc);
         Set_Defining_Unit_Name (Proc_Spec_Node, Proc_Id);

         Parameters := Init_Formals (Rec_Type, Proc_Id);
         Append_List_To (Parameters,
           Build_Discriminant_Formals (Rec_Type, True));

         --  For tagged types, we add a parameter to indicate what
         --  portion of the object's initialization is to be performed.
         --  This is used for two purposes:
         --   1)  When a type extension's initialization procedure calls
         --       the initialization procedure of the parent type, we do
         --       not want the parent to initialize the Tag component;
         --       it has been set already.
         --   2)  If an ancestor type has at least one component that requires
         --       late initialization, then we need to be able to initialize
         --       those components separately after initializing any other
         --       components.

         if Is_Tagged_Type (Rec_Type) then
            Init_Control_Formal := Make_Temporary (Loc, 'P');

            Append_To (Parameters,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Init_Control_Formal,
                Parameter_Type =>
                  New_Occurrence_Of (Standard_Natural, Loc),
                Expression => Make_Mode_Literal (Loc, Full_Init)));
         end if;

         --  Create an extra accessibility parameter to capture the level of
         --  the object being initialized when its type is a limited record.

         if Is_Limited_Record (Rec_Type) then
            Append_To (Parameters,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Make_Defining_Identifier
                                         (Loc, Name_uInit_Level),
                Parameter_Type      =>
                  New_Occurrence_Of (Standard_Natural, Loc),
                Expression          =>
                  Make_Integer_Literal
                    (Loc, Scope_Depth (Standard_Standard))));
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
                 Build_Init_Statements (Component_List (Type_Definition (N))));
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
                  --  The parent field must be initialized first because the
                  --  offset of the new discriminants may depend on it. This is
                  --  not needed if the parent is an interface type because in
                  --  such case the initialization of the _parent field was not
                  --  generated.

                  if not Is_Interface (Etype (Rec_Ent)) then
                     declare
                        Parent_IP : constant Name_Id :=
                                      Make_Init_Proc_Name (Etype (Rec_Ent));
                        Stmt      : Node_Id := First (Stmts);
                        IP_Call   : Node_Id := Empty;
                     begin
                        --  Look for a call to the parent IP associated with
                        --  the record extension.
                        --  The call will be inside not one but two
                        --  if-statements (with the same condition). Testing
                        --  the same Early_Init condition twice might seem
                        --  redundant. However, as soon as we exit this loop,
                        --  we are going to hoist the inner if-statement out
                        --  of the outer one; the "redundant" test was built
                        --  in anticipation of this hoisting.

                        while Present (Stmt) loop
                           if Nkind (Stmt) = N_If_Statement then
                              declare
                                 Then_Stmt1 : Node_Id :=
                                   First (Then_Statements (Stmt));
                                 Then_Stmt2 : Node_Id;
                              begin
                                 while Present (Then_Stmt1) loop
                                    if Nkind (Then_Stmt1) = N_If_Statement then
                                       Then_Stmt2 :=
                                         First (Then_Statements (Then_Stmt1));

                                       if Nkind (Then_Stmt2) =
                                            N_Procedure_Call_Statement
                                         and then Chars (Name (Then_Stmt2)) =
                                           Parent_IP
                                       then
                                          --  IP_Call is a call wrapped in an
                                          --  if statement.
                                          IP_Call := Then_Stmt1;
                                          exit;
                                       end if;
                                    end if;
                                    Next (Then_Stmt1);
                                 end loop;
                              end;
                           end if;

                           Next (Stmt);
                        end loop;

                        --  If found then move it to the beginning of the
                        --  statements of this IP routine

                        if Present (IP_Call) then
                           Remove (IP_Call);
                           Prepend_List_To (Body_Stmts, New_List (IP_Call));
                        end if;
                     end;
                  end if;

                  Append_List_To (Body_Stmts, Stmts);
               end;
            end if;
         end if;

         --  Add here the assignment to instantiate the Tag

         --  The assignment corresponds to the code:

         --     _Init._Tag := Typ'Tag;

         --  Suppress the tag assignment when not Tagged_Type_Expansion because
         --  tags are represented implicitly in objects. It is also suppressed
         --  in case of CPP_Class types because in this case the tag is
         --  initialized in the C++ side.

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
                 Make_Tag_Assignment_From_Type
                   (Loc, Make_Identifier (Loc, Name_uInit), Rec_Type));

               --  Ada 2005 (AI-251): Initialize the secondary tags components
               --  located at fixed positions (tags whose position depends on
               --  variable size components are initialized later ---see below)

               if Ada_Version >= Ada_2005
                 and then not Is_Interface (Rec_Type)
                 and then Has_Interfaces (Rec_Type)
               then
                  declare
                     Elab_Sec_DT_Stmts_List : constant List_Id := New_List;
                     Elab_List              : List_Id          := New_List;

                  begin
                     Init_Secondary_Tags
                       (Typ            => Rec_Type,
                        Target         => Make_Identifier (Loc, Name_uInit),
                        Init_Tags_List => Init_Tags_List,
                        Stmts_List     => Elab_Sec_DT_Stmts_List,
                        Fixed_Comps    => True,
                        Variable_Comps => False);

                     Elab_List := New_List (
                       Make_If_Statement (Loc,
                         Condition       =>
                           Tag_Init_Condition (Loc, Init_Control_Formal),
                         Then_Statements => Init_Tags_List));

                     if Elab_Flag_Needed (Rec_Type) then
                        Append_To (Elab_Sec_DT_Stmts_List,
                          Make_Assignment_Statement (Loc,
                            Name       =>
                              New_Occurrence_Of
                                (Access_Disp_Table_Elab_Flag (Rec_Type),
                                 Loc),
                            Expression =>
                              New_Occurrence_Of (Standard_False, Loc)));

                        Append_To (Elab_List,
                          Make_If_Statement (Loc,
                            Condition       =>
                              New_Occurrence_Of
                                (Access_Disp_Table_Elab_Flag (Rec_Type), Loc),
                            Then_Statements => Elab_Sec_DT_Stmts_List));
                     end if;

                     Prepend_List_To (Body_Stmts, Elab_List);
                  end;
               else
                  Prepend_To (Body_Stmts,
                    Make_If_Statement (Loc,
                      Condition =>
                        Tag_Init_Condition (Loc, Init_Control_Formal),
                      Then_Statements => Init_Tags_List));
               end if;

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
                 Make_Tag_Assignment_From_Type
                   (Loc, Make_Identifier (Loc, Name_uInit), Rec_Type));

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
                     Init_Tags_List => Init_Tags_List,
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
                  --  It is wrapped in an "if Early_Init_Condition"
                  --  if-statement.

                  Ins_Nod := First (Body_Stmts);
                  while Present (Next (Ins_Nod))
                    and then
                      (Nkind (Ins_Nod) /= N_If_Statement
                        or else Nkind (First (Then_Statements (Ins_Nod)))
                                  /= N_Procedure_Call_Statement
                        or else not Is_Init_Proc
                                      (Name (First (Then_Statements
                                         (Ins_Nod)))))
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
                            New_Occurrence_Of (Init_DT, Loc));
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
                  Init_Tags_List => Init_Tags_List,
                  Stmts_List     => Init_Tags_List,
                  Fixed_Comps    => False,
                  Variable_Comps => True);

               Append_List_To (Body_Stmts, Init_Tags_List);
            end if;
         end if;

         Handled_Stmt_Node := New_Node (N_Handled_Sequence_Of_Statements, Loc);
         Set_Statements (Handled_Stmt_Node, Body_Stmts);

         --  Generate:
         --    Deep_Finalize (_init, C1, ..., CN);
         --    raise;

         if Counter > 0
           and then Needs_Finalization (Rec_Type)
           and then not Is_Abstract_Type (Rec_Type)
           and then not Restriction_Active (No_Exception_Propagation)
         then
            declare
               DF_Call : Node_Id;
               DF_Id   : Entity_Id;

            begin
               --  Create a local version of Deep_Finalize which has indication
               --  of partial initialization state.

               DF_Id :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Name_uFinalizer));
               Set_Is_Finalizer (DF_Id);

               Append_To (Decls, Make_Local_Deep_Finalize (Rec_Type, DF_Id));

               DF_Call :=
                 Make_Procedure_Call_Statement (Loc,
                   Name                   => New_Occurrence_Of (DF_Id, Loc),
                   Parameter_Associations => New_List (
                     Make_Identifier (Loc, Name_uInit),
                     New_Occurrence_Of (Standard_False, Loc)));

               --  Do not emit warnings related to the elaboration order when a
               --  controlled object is declared before the body of Finalize is
               --  seen.

               if Legacy_Elaboration_Checks then
                  Set_No_Elaboration_Check (DF_Call);
               end if;

               Set_Exception_Handlers (Handled_Stmt_Node, New_List (
                 Make_Exception_Handler (Loc,
                   Exception_Choices => New_List (
                     Make_Others_Choice (Loc)),
                   Statements        => New_List (
                     DF_Call,
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
         --  as null to suppress the call. Kill also warnings for the _Init
         --  out parameter, which is left entirely uninitialized.

         Set_Init_Proc (Rec_Type, Proc_Id);

         if Is_Null_Statement_List (Body_Stmts) then
            Set_Is_Null_Init_Proc (Proc_Id);
            Set_Warnings_Off (Defining_Identifier (First (Parameters)));
         end if;
      end Build_Init_Procedure;

      ---------------------------
      -- Build_Init_Statements --
      ---------------------------

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id is
         Checks             : constant List_Id := New_List;
         Actions            : List_Id          := No_List;
         Counter_Id         : Entity_Id        := Empty;
         Comp_Loc           : Source_Ptr;
         Decl               : Node_Id;
         Id                 : Entity_Id;
         Parent_Stmts       : List_Id;
         Parent_Id          : Entity_Id := Empty;
         Stmts, Late_Stmts  : List_Id := Empty_List;
         Typ                : Entity_Id;

         procedure Increment_Counter
           (Loc  : Source_Ptr; Late : Boolean := False);
         --  Generate an "increment by one" statement for the current counter
         --  and append it to the appropriate statement list.

         procedure Make_Counter (Loc : Source_Ptr);
         --  Create a new counter for the current component list. The routine
         --  creates a new defining Id, adds an object declaration and sets
         --  the Id generator for the next variant.

         -----------------------
         -- Increment_Counter --
         -----------------------

         procedure Increment_Counter
           (Loc  : Source_Ptr; Late : Boolean := False) is
         begin
            --  Generate:
            --    Counter := Counter + 1;

            Append_To ((if Late then Late_Stmts else Stmts),
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Counter_Id, Loc),
                Expression =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
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
                  New_Occurrence_Of (Standard_Integer, Loc),
                Expression          =>
                  Make_Integer_Literal (Loc, 0)));
         end Make_Counter;

      --  Start of processing for Build_Init_Statements

      begin
         if Null_Present (Comp_List) then
            return New_List (Make_Null_Statement (Loc));
         end if;

         Parent_Stmts := New_List;
         Stmts := New_List;

         --  Loop through visible declarations of task types and protected
         --  types moving any expanded code from the spec to the body of the
         --  init procedure.

         if Is_Concurrent_Record_Type (Rec_Type) then
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
         --  components that require late initialization.

         --  First pass : regular components

         Decl := First_Non_Pragma (Component_Items (Comp_List));
         while Present (Decl) loop
            Comp_Loc := Sloc (Decl);
            Build_Record_Checks
              (Subtype_Indication (Component_Definition (Decl)), Checks);

            Id  := Defining_Identifier (Decl);

            --  Obtain the corresponding mutably tagged type's parent subtype
            --  to handle default initialization.

            Typ := Get_Corresponding_Tagged_Type_If_Present (Etype (Id));

            --  Leave any processing of component requiring late initialization
            --  for the second pass.

            if Initialization_Control.Requires_Late_Init (Decl, Rec_Type) then
               if not Has_Late_Init_Comp then
                  Late_Stmts := New_List;
               end if;
               Has_Late_Init_Comp := True;

            --  Regular component cases

            else
               --  In the context of the init proc, references to discriminants
               --  resolve to denote the discriminals: this is where we can
               --  freeze discriminant dependent component subtypes.

               if not Is_Frozen (Typ) then
                  Append_List_To (Stmts, Freeze_Entity (Typ, N));
               end if;

               --  Explicit initialization

               if Present (Expression (Decl)) then
                  if Is_CPP_Constructor_Call (Expression (Decl)) then
                     Actions :=
                       Build_Initialization_Call
                         (Decl,
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

               --  CPU, Dispatching_Domain, Priority, and Secondary_Stack_Size
               --  components are filled in with the corresponding rep-item
               --  expression of the concurrent type (if any).

               elsif Ekind (Scope (Id)) = E_Record_Type
                 and then Present (Corresponding_Concurrent_Type (Scope (Id)))
                 and then Chars (Id) in Name_uCPU
                                      | Name_uDispatching_Domain
                                      | Name_uPriority
                                      | Name_uSecondary_Stack_Size
               then
                  declare
                     Exp   : Node_Id;
                     Nam   : Name_Id;
                     pragma Warnings (Off, Nam);
                     Ritem : Node_Id;

                  begin
                     if Chars (Id) = Name_uCPU then
                        Nam := Name_CPU;

                     elsif Chars (Id) = Name_uDispatching_Domain then
                        Nam := Name_Dispatching_Domain;

                     elsif Chars (Id) = Name_uPriority then
                        Nam := Name_Priority;

                     elsif Chars (Id) = Name_uSecondary_Stack_Size then
                        Nam := Name_Secondary_Stack_Size;
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
                           Exp :=
                             Get_Pragma_Arg
                               (First (Pragma_Argument_Associations (Ritem)));

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

                        --  Conversion for Secondary_Stack_Size value

                        elsif Nam = Name_Secondary_Stack_Size then
                           Exp := Convert_To (RTE (RE_Size_Type), Exp);
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
                  declare
                     use Initialization_Control;
                     Init_Control_Actual : Node_Id := Empty;
                     Is_Parent : constant Boolean := Chars (Id) = Name_uParent;
                     Init_Call_Stmts : List_Id;
                  begin
                     if Is_Parent and then Has_Late_Init_Component (Etype (Id))
                     then
                        Init_Control_Actual :=
                          Make_Mode_Literal (Comp_Loc, Early_Init_Only);
                        --  Parent_Id used later in second call to parent's
                        --  init proc to initialize late-init components.
                        Parent_Id := Id;
                     end if;

                     Init_Call_Stmts :=
                       Build_Initialization_Call
                         (Decl,
                          Make_Selected_Component (Comp_Loc,
                            Prefix        =>
                              Make_Identifier (Comp_Loc, Name_uInit),
                            Selector_Name => New_Occurrence_Of (Id, Comp_Loc)),
                          Typ,
                          In_Init_Proc        => True,
                          Enclos_Type         => Rec_Type,
                          Discr_Map           => Discr_Map,
                          Init_Control_Actual => Init_Control_Actual);

                     if Is_Parent then
                        --  This is tricky. At first it looks like
                        --  we are going to end up with nested
                        --  if-statements with the same condition:
                        --    if Early_Init_Condition then
                        --       if Early_Init_Condition then
                        --          Parent_TypeIP (...);
                        --       end if;
                        --    end if;
                        --  But later we will hoist the inner if-statement
                        --  out of the outer one; we do this  because the
                        --  init-proc call for the _Parent component of a type
                        --  extension has to precede any other initialization.
                        Actions :=
                          New_List (Make_If_Statement (Loc,
                            Condition =>
                              Early_Init_Condition (Loc, Init_Control_Formal),
                            Then_Statements => Init_Call_Stmts));
                     else
                        Actions := Init_Call_Stmts;
                     end if;
                  end;

                  Clean_Task_Names (Typ, Proc_Id);

               --  Simple initialization. If the Esize is not yet set, we pass
               --  Uint_0 as expected by Get_Simple_Init_Val.

               elsif Component_Needs_Simple_Initialization (Typ) then
                  Actions :=
                    Build_Assignment
                      (Id      => Id,
                       Default =>
                         Get_Simple_Init_Val
                           (Typ  => Typ,
                            N    => N,
                            Size =>
                              (if Known_Esize (Id) then Esize (Id)
                               else Uint_0)));

               --  Nothing needed for this case

               else
                  Actions := No_List;
               end if;

               --  When the component's type has a Default_Initial_Condition,
               --  and the component is default initialized, then check the
               --  DIC here.

               if Has_DIC (Typ)
                 and then No (Expression (Decl))
                 and then Present (DIC_Procedure (Typ))
                 and then not Has_Null_Body (DIC_Procedure (Typ))

                 --  The DICs of ancestors are checked as part of the type's
                 --  DIC procedure.

                 and then Chars (Id) /= Name_uParent

                 --  In GNATprove mode, the component DICs are checked by other
                 --  means. They should not be added to the record type DIC
                 --  procedure, so that the procedure can be used to check the
                 --  record type invariants or DICs if any.

                 and then not GNATprove_Mode
               then
                  Append_New_To (Actions,
                     Build_DIC_Call
                       (Comp_Loc,
                        Make_Selected_Component (Comp_Loc,
                          Prefix        =>
                            Make_Identifier (Comp_Loc, Name_uInit),
                          Selector_Name =>
                            New_Occurrence_Of (Id, Comp_Loc)),
                        Typ));
               end if;

               if Present (Checks) then
                  if Chars (Id) = Name_uParent then
                     Append_List_To (Parent_Stmts, Checks);
                  else
                     Append_List_To (Stmts, Checks);
                  end if;
               end if;

               if Present (Actions) then
                  if Chars (Id) = Name_uParent then
                     Append_List_To (Parent_Stmts, Actions);
                  else
                     Append_List_To (Stmts, Actions);

                     --  Preserve initialization state in the current counter

                     if Needs_Finalization (Typ) then
                        if No (Counter_Id) then
                           Make_Counter (Comp_Loc);
                        end if;

                        Increment_Counter (Comp_Loc);
                     end if;
                  end if;
               end if;
            end if;

            Next_Non_Pragma (Decl);
         end loop;

         --  The parent field must be initialized first because variable
         --  size components of the parent affect the location of all the
         --  new components.

         Prepend_List_To (Stmts, Parent_Stmts);

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
                                    New_Occurrence_Of (RTE (
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

         --  For a protected type, add statements generated by
         --  Make_Initialize_Protection.

         elsif Is_Protected_Record_Type (Rec_Type) then
            Append_List_To (Stmts,
              Make_Initialize_Protection (Rec_Type));
         end if;

         --  Second pass: components that require late initialization

         if Present (Parent_Id) then
            declare
               Parent_Loc : constant Source_Ptr := Sloc (Parent (Parent_Id));
               use Initialization_Control;
            begin
               --  We are building the init proc for a type extension.
               --  Call the parent type's init proc a second time, this
               --  time to initialize the parent's components that require
               --  late initialization.

               Append_List_To (Late_Stmts,
                 Build_Initialization_Call
                   (N                    => Parent (Parent_Id),
                    Id_Ref               =>
                      Make_Selected_Component (Parent_Loc,
                        Prefix        => Make_Identifier
                                           (Parent_Loc, Name_uInit),
                        Selector_Name => New_Occurrence_Of (Parent_Id,
                                                            Parent_Loc)),
                    Typ                 => Etype (Parent_Id),
                    In_Init_Proc        => True,
                    Enclos_Type         => Rec_Type,
                    Discr_Map           => Discr_Map,
                    Init_Control_Actual => Make_Mode_Literal
                                             (Parent_Loc, Late_Init_Only)));
            end;
         end if;

         if Has_Late_Init_Comp then
            Decl := First_Non_Pragma (Component_Items (Comp_List));
            while Present (Decl) loop
               Comp_Loc := Sloc (Decl);
               Id := Defining_Identifier (Decl);

               --  Obtain the corresponding mutably tagged type's parent
               --  subtype to handle default initialization.

               Typ := Get_Corresponding_Tagged_Type_If_Present (Etype (Id));

               if Initialization_Control.Requires_Late_Init (Decl, Rec_Type)
               then
                  if Present (Expression (Decl)) then
                     Append_List_To (Late_Stmts,
                       Build_Assignment (Id, Expression (Decl)));

                  elsif Has_Non_Null_Base_Init_Proc (Typ) then
                     Append_List_To (Late_Stmts,
                       Build_Initialization_Call (Decl,
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

                        Increment_Counter (Comp_Loc, Late => True);
                     end if;
                  elsif Component_Needs_Simple_Initialization (Typ) then
                     Append_List_To (Late_Stmts,
                       Build_Assignment
                         (Id      => Id,
                          Default =>
                            Get_Simple_Init_Val
                              (Typ  => Typ,
                               N    => N,
                               Size => Esize (Id))));
                  end if;
               end if;

               Next_Non_Pragma (Decl);
            end loop;
         end if;

         --  Process the variant part (incorrectly ignoring late
         --  initialization requirements for components therein).

         if Present (Variant_Part (Comp_List)) then
            declare
               Variant_Alts : constant List_Id := New_List;
               Var_Loc      : Source_Ptr := No_Location;
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
                     New_Occurrence_Of (Discriminal (
                       Entity (Name (Variant_Part (Comp_List)))), Var_Loc),
                   Alternatives => Variant_Alts));
            end;
         end if;

         if No (Init_Control_Formal) then
            Append_List_To (Stmts, Late_Stmts);

            --  If no initializations were generated for component declarations
            --  and included in Stmts, then append a null statement to Stmts
            --  to make it a valid Ada tree.

            if Is_Empty_List (Stmts) then
               Append (Make_Null_Statement (Loc), Stmts);
            end if;

            return Stmts;
         else
            declare
               use Initialization_Control;

               If_Early : constant Node_Id :=
                  (if Is_Empty_List (Stmts) then
                      Make_Null_Statement (Loc)
                   else
                      Make_If_Statement (Loc,
                        Condition =>
                          Early_Init_Condition (Loc, Init_Control_Formal),
                        Then_Statements => Stmts));
               If_Late : constant Node_Id :=
                  (if Is_Empty_List (Late_Stmts) then
                      Make_Null_Statement (Loc)
                   else
                      Make_If_Statement (Loc,
                        Condition =>
                          Late_Init_Condition (Loc, Init_Control_Formal),
                        Then_Statements => Late_Stmts));
            begin
               return New_List (If_Early, If_Late);
            end;
         end if;
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
                  Process_Range_Expr_In_Decl (S, T, Check_List => Check_List);
               end if;
            end Constrain_Index;

         --  Start of processing for Constrain_Array

         begin
            T := Entity (Subtype_Mark (SI));

            if Is_Access_Type (T) then
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
         --     at the other end of the call, even if it does nothing).

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
      Rec_Type := Defining_Identifier (N);

      --  This may be full declaration of a private type, in which case
      --  the visible entity is a record, and the private entity has been
      --  exchanged with it in the private part of the current package.
      --  The initialization procedure is built for the record type, which
      --  is retrievable from the private entity.

      if Is_Incomplete_Or_Private_Type (Rec_Type) then
         Rec_Type := Underlying_Type (Rec_Type);
      end if;

      --  If we have a variant record with restriction No_Implicit_Conditionals
      --  in effect, then we skip building the procedure. This is safe because
      --  if we can see the restriction, so can any caller, calls to initialize
      --  such records are not allowed for variant records if this restriction
      --  is active.

      if Has_Variant_Part (Rec_Type)
        and then Restriction_Active (No_Implicit_Conditionals)
      then
         return;
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
        and then Present (Base_Init_Proc (Etype (Rec_Type)))
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

         Set_Is_Public      (Proc_Id, Is_Public (Rec_Ent));
         Set_Is_Internal    (Proc_Id);
         Set_Has_Completion (Proc_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;

         Set_Is_Inlined (Proc_Id, Inline_Init_Proc (Rec_Type));

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
               if Is_Array_Type (Typ) and then Is_Itype (Typ) then
                  Ref := Make_Itype_Reference (Loc);
                  Set_Itype (Ref, Typ);
                  Append_Freeze_Action (Rec_Type, Ref);

                  Ref := Make_Itype_Reference (Loc);
                  Set_Itype (Ref, Etype (First_Index (Typ)));
                  Append_Freeze_Action (Rec_Type, Ref);

                  --  Recurse on nested arrays

                  Sub_Aggr := First (Expressions (Comp));
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

            if Present (Agg) and then Nkind (Agg) = N_Aggregate then
               Set_Static_Initialization (Proc_Id, Agg);

               declare
                  Comp : Node_Id;
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

   ----------------------------
   -- Build_Slice_Assignment --
   ----------------------------

   --  Generates the following subprogram:

   --    procedure array_typeSA
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

   --       if Rev then
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
   --    end array_typeSA;

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
         Formals : List_Id;

      begin
         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Larray,
             Out_Present => True,
             Parameter_Type =>
               New_Occurrence_Of (Base_Type (Typ), Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Rarray,
             Parameter_Type =>
               New_Occurrence_Of (Base_Type (Typ), Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Left_Lo,
             Parameter_Type =>
               New_Occurrence_Of (Index, Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Left_Hi,
             Parameter_Type =>
               New_Occurrence_Of (Index, Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Right_Lo,
             Parameter_Type =>
               New_Occurrence_Of (Index, Loc)),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => Right_Hi,
             Parameter_Type =>
               New_Occurrence_Of (Index, Loc)));

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Rev,
             Parameter_Type =>
               New_Occurrence_Of (Standard_Boolean, Loc)));

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

   ------------------------------------
   -- Build_Untagged_Record_Equality --
   ------------------------------------

   procedure Build_Untagged_Record_Equality (Typ : Entity_Id) is
      Build_Eq : Boolean;
      Comp     : Entity_Id;
      Decl     : Node_Id;
      Op       : Entity_Id;
      Eq_Op    : Entity_Id;

      function User_Defined_Eq (T : Entity_Id) return Entity_Id;
      --  Check whether the type T has a user-defined primitive equality. If so
      --  return it, else return Empty. If true for a component of Typ, we have
      --  to build the primitive equality for it.

      ---------------------
      -- User_Defined_Eq --
      ---------------------

      function User_Defined_Eq (T : Entity_Id) return Entity_Id is
         Op : constant Entity_Id := TSS (T, TSS_Composite_Equality);

      begin
         if Present (Op) then
            return Op;
         else
            return Get_User_Defined_Equality (T);
         end if;
      end User_Defined_Eq;

   --  Start of processing for Build_Untagged_Record_Equality

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
            exit;
         end if;

         Next_Component (Comp);
      end loop;

      --  If there is a user-defined equality for the type, we do not create
      --  the implicit one.

      Eq_Op := Get_User_Defined_Equality (Typ);
      if Present (Eq_Op) then
         if Comes_From_Source (Eq_Op) then
            Build_Eq := False;
         else
            Eq_Op := Empty;
         end if;
      end if;

      --  If the type is derived, inherit the operation, if present, from the
      --  parent type. It may have been declared after the type derivation. If
      --  the parent type itself is derived, it may have inherited an operation
      --  that has itself been overridden, so update its alias and related
      --  flags. Ditto for inequality.

      if No (Eq_Op) and then Is_Derived_Type (Typ) then
         Eq_Op := Get_User_Defined_Equality (Etype (Typ));
         if Present (Eq_Op) then
            Copy_TSS (Eq_Op, Typ);
            Build_Eq := False;

            declare
               Op    : constant Entity_Id := User_Defined_Eq (Typ);
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
         end if;
      end if;

      --  If not inherited and not user-defined, build body as for a type with
      --  components of record type (i.e. a type for which "=" composes when
      --  used as a component in an outer composite type).

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
   end Build_Untagged_Record_Equality;

   -----------------------------------
   -- Build_Variant_Record_Equality --
   -----------------------------------

   --  Generates:

   --    function <<Body_Id>> (Left, Right : T) return Boolean is
   --       [ X : T renames Left;  ]
   --       [ Y : T renames Right; ]
   --       --  The above renamings are generated only if the parameters of
   --       --  this built function (which are passed by the caller) are not
   --       --  named 'X' and 'Y'; these names are required to reuse several
   --       --  expander routines when generating this body.

   --    begin
   --       --  Compare discriminants

   --       if X.D1 /= Y.D1 or else X.D2 /= Y.D2 or else ... then
   --          return False;
   --       end if;

   --       --  Compare components

   --       if X.C1 /= Y.C1 or else X.C2 /= Y.C2 or else ... then
   --          return False;
   --       end if;

   --       --  Compare variant part

   --       case X.D1 is
   --          when V1 =>
   --             if X.C2 /= Y.C2 or else X.C3 /= Y.C3 or else ... then
   --                return False;
   --             end if;
   --          ...
   --          when Vn =>
   --             if X.Cn /= Y.Cn or else ... then
   --                return False;
   --             end if;
   --       end case;

   --       return True;
   --    end _Equality;

   function Build_Variant_Record_Equality
     (Typ         : Entity_Id;
      Spec_Id     : Entity_Id;
      Body_Id     : Entity_Id;
      Param_Specs : List_Id) return Node_Id
   is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Def   : constant Node_Id    := Parent (Typ);
      Comps : constant Node_Id    := Component_List (Type_Definition (Def));
      Left  : constant Entity_Id  := Defining_Identifier (First (Param_Specs));
      Right : constant Entity_Id  :=
                    Defining_Identifier (Next (First (Param_Specs)));
      Decls : constant List_Id    := New_List;
      Stmts : constant List_Id    := New_List;

      Subp_Body : Node_Id;

   begin
      pragma Assert (not Is_Tagged_Type (Typ));

      --  In order to reuse the expander routines Make_Eq_If and Make_Eq_Case
      --  the name of the formals must be X and Y; otherwise we generate two
      --  renaming declarations for such purpose.

      if Chars (Left) /= Name_X then
         Append_To (Decls,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
             Subtype_Mark        => New_Occurrence_Of (Typ, Loc),
             Name                => Make_Identifier (Loc, Chars (Left))));
      end if;

      if Chars (Right) /= Name_Y then
         Append_To (Decls,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
             Subtype_Mark        => New_Occurrence_Of (Typ, Loc),
             Name                => Make_Identifier (Loc, Chars (Right))));
      end if;

      --  Unchecked_Unions require additional machinery to support equality.
      --  Two extra parameters (A and B) are added to the equality function
      --  parameter list for each discriminant of the type, in order to
      --  capture the inferred values of the discriminants in equality calls.
      --  The names of the parameters match the names of the corresponding
      --  discriminant, with an added suffix.

      if Is_Unchecked_Union (Typ) then
         declare
            Right_Formal : constant Entity_Id :=
              (if Present (Spec_Id) then Last_Formal (Spec_Id) else Right);
            Scop : constant Entity_Id :=
              (if Present (Spec_Id) then Spec_Id else Body_Id);

            procedure Decorate_Extra_Formal (F, F_Typ : Entity_Id);
            --  Decorate extra formal F with type F_Typ

            ---------------------------
            -- Decorate_Extra_Formal --
            ---------------------------

            procedure Decorate_Extra_Formal (F, F_Typ : Entity_Id) is
            begin
               Mutate_Ekind  (F, E_In_Parameter);
               Set_Etype     (F, F_Typ);
               Set_Scope     (F, Scop);
               Set_Mechanism (F, By_Copy);
            end Decorate_Extra_Formal;

            A          : Entity_Id;
            B          : Entity_Id;
            Discr      : Entity_Id;
            Discr_Type : Entity_Id;
            Last_Extra : Entity_Id := Empty;
            New_Discrs : Elist_Id;

         begin
            Mutate_Ekind (Body_Id, E_Subprogram_Body);
            New_Discrs := New_Elmt_List;

            Discr := First_Discriminant (Typ);
            while Present (Discr) loop
               Discr_Type := Etype (Discr);

               --  Add the new parameters as extra formals

               A :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (Discr), 'A'));

               Decorate_Extra_Formal (A, Discr_Type);

               if Present (Last_Extra) then
                  Set_Extra_Formal (Last_Extra, A);
               else
                  Set_Extra_Formal (Right_Formal, A);
                  Set_Extra_Formals (Scop, A);
               end if;

               Append_Elmt (A, New_Discrs);

               B :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (Discr), 'B'));

               Decorate_Extra_Formal (B, Discr_Type);

               Set_Extra_Formal (A, B);
               Last_Extra := B;

               --  Generate the following code to compare each of the inferred
               --  discriminants:

               --  if a /= b then
               --     return False;
               --  end if;

               Append_To (Stmts,
                 Make_If_Statement (Loc,
                   Condition       =>
                     Make_Op_Ne (Loc,
                       Left_Opnd  => New_Occurrence_Of (A, Loc),
                       Right_Opnd => New_Occurrence_Of (B, Loc)),
                   Then_Statements => New_List (
                     Make_Simple_Return_Statement (Loc,
                       Expression =>
                         New_Occurrence_Of (Standard_False, Loc)))));

               Next_Discriminant (Discr);
            end loop;

            --  Generate component-by-component comparison. Note that we must
            --  propagate the inferred discriminants formals to act as the case
            --  statement switch. Their value is added when an equality call on
            --  unchecked unions is expanded.

            Append_List_To (Stmts, Make_Eq_Case (Typ, Comps, New_Discrs));
         end;

      --  Normal case (not unchecked union)

      else
         Append_To (Stmts,
           Make_Eq_If (Typ, Discriminant_Specifications (Def)));
         Append_List_To (Stmts, Make_Eq_Case (Typ, Comps));
      end if;

      Append_To (Stmts,
        Make_Simple_Return_Statement (Loc,
          Expression => New_Occurrence_Of (Standard_True, Loc)));

      Subp_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Body_Id,
              Parameter_Specifications => Param_Specs,
              Result_Definition        =>
                New_Occurrence_Of (Standard_Boolean, Loc)),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));

      return Subp_Body;
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
         --  Move this check to sem???

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

   -------------------------------
   -- Copy_Discr_Checking_Funcs --
   -------------------------------

   procedure Copy_Discr_Checking_Funcs (N : Node_Id) is
      Typ      : constant Entity_Id := Defining_Identifier (N);
      Comp     : Entity_Id := First_Component (Typ);
      Old_Comp : Entity_Id := First_Component
                                (Base_Type (Underlying_Type (Etype (Typ))));
   begin
      while Present (Comp) loop
         if Chars (Comp) = Chars (Old_Comp) then
            Set_Discriminant_Checking_Func
              (Comp, Discriminant_Checking_Func (Old_Comp));
         end if;

         Next_Component (Old_Comp);
         Next_Component (Comp);
      end loop;
   end Copy_Discr_Checking_Funcs;

   ------------------------------
   -- Expand_Freeze_Array_Type --
   ------------------------------

   procedure Expand_Freeze_Array_Type (N : Node_Id) is
      Typ      : constant Entity_Id := Entity (N);
      Base     : constant Entity_Id := Base_Type (Typ);

      --  Obtain the corresponding mutably tagged type if necessary

      Comp_Typ : constant Entity_Id :=
        Get_Corresponding_Mutably_Tagged_Type_If_Present
          (Component_Type (Typ));

   begin
      if not Is_Bit_Packed_Array (Typ) then
         if No (Init_Proc (Base)) then

            --  If this is an anonymous array built for an object declaration
            --  with an initial value, its Init_Proc will never be called. The
            --  initial value itself may have been expanded into assignments,
            --  in which case the declaration has the No_Initialization flag.
            --  The exception is when the initial value is a 2-pass aggregate,
            --  because the special expansion used for it creates a temporary
            --  that needs a fully-fledged initialization.

            if Is_Itype (Base)
              and then Nkind (Associated_Node_For_Itype (Base)) =
                                                    N_Object_Declaration
              and then
                ((Present (Expression (Associated_Node_For_Itype (Base)))
                    and then not
                      Is_Two_Pass_Aggregate
                        (Expression (Associated_Node_For_Itype (Base))))
                  or else No_Initialization (Associated_Node_For_Itype (Base)))
            then
               null;

            --  We do not need an init proc for string or wide [wide] string,
            --  since the only time these need initialization in normalize or
            --  initialize scalars mode, and these types are treated specially
            --  and do not need initialization procedures.

            elsif Is_Standard_String_Type (Base) then
               null;

            --  Otherwise we have to build an init proc for the subtype

            else
               Build_Array_Init_Proc (Base, N);
            end if;
         end if;

         if Typ = Base and then Has_Controlled_Component (Base) then
            Build_Controlling_Procs (Base);

            if not Is_Limited_Type (Comp_Typ)
              and then Number_Dimensions (Typ) = 1
            then
               Build_Slice_Assignment (Typ);
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
   end Expand_Freeze_Array_Type;

   -----------------------------------
   -- Expand_Freeze_Class_Wide_Type --
   -----------------------------------

   procedure Expand_Freeze_Class_Wide_Type (N : Node_Id) is
      function Is_C_Derivation (Typ : Entity_Id) return Boolean;
      --  Given a type, determine whether it is derived from a C or C++ root

      ---------------------
      -- Is_C_Derivation --
      ---------------------

      function Is_C_Derivation (Typ : Entity_Id) return Boolean is
         T : Entity_Id;

      begin
         T := Typ;
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

      --  Local variables

      Typ  : constant Entity_Id := Entity (N);
      Root : constant Entity_Id := Root_Type (Typ);

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
      --
      --  Concurrent Ada types are functionally represented by an associated
      --  "corresponding record type" (typenameV), which owns the actual TSS
      --  finalize bodies for the type (and technically class-wide type).

      elsif Is_Concurrent_Type (Root)
        or else Is_C_Derivation (Root)
        or else Convention (Typ) = Convention_CPP
      then
         return;

      --  Do not create TSS routine Finalize_Address when compiling in CodePeer
      --  mode since the routine contains an Unchecked_Conversion.

      elsif CodePeer_Mode then
         return;
      end if;

      --  Create the body of TSS primitive Finalize_Address. This automatically
      --  sets the TSS entry for the class-wide type.

      if No (Finalize_Address (Typ)) then
         Make_Finalize_Address_Body (Typ);
      end if;
   end Expand_Freeze_Class_Wide_Type;

   ------------------------------------
   -- Expand_Freeze_Enumeration_Type --
   ------------------------------------

   procedure Expand_Freeze_Enumeration_Type (N : Node_Id) is
      Typ : constant Entity_Id  := Entity (N);
      Loc : constant Source_Ptr := Sloc (Typ);

      Arr           : Entity_Id;
      Ent           : Entity_Id;
      Fent          : Entity_Id;
      Is_Contiguous : Boolean;
      Index_Typ     : Entity_Id;
      Ityp          : Entity_Id;
      Last_Repval   : Uint;
      Lst           : List_Id;
      Num           : Nat;
      Pos_Expr      : Node_Id;

      Func : Entity_Id;
      pragma Warnings (Off, Func);

   begin
      --  Various optimizations possible if given representation is contiguous

      Is_Contiguous := True;

      Ent := First_Literal (Typ);
      Last_Repval := Enumeration_Rep (Ent);
      Num := 1;
      Next_Literal (Ent);

      while Present (Ent) loop
         if Enumeration_Rep (Ent) - Last_Repval /= 1 then
            Is_Contiguous := False;
         else
            Last_Repval := Enumeration_Rep (Ent);
         end if;

         Num := Num + 1;
         Next_Literal (Ent);
      end loop;

      if Is_Contiguous then
         Set_Has_Contiguous_Rep (Typ);

         --  Now build a subtype declaration

         --    subtype typI is new Natural range 0 .. num - 1

         Index_Typ :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Typ), 'I'));

         Append_Freeze_Action (Typ,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Index_Typ,
             Subtype_Indication =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (Standard_Natural,  Loc),
                 Constraint  =>
                   Make_Range_Constraint (Loc,
                     Range_Expression =>
                       Make_Range (Loc,
                         Low_Bound  =>
                           Make_Integer_Literal (Loc, 0),
                         High_Bound =>
                           Make_Integer_Literal (Loc, Num - 1))))));

         Set_Enum_Pos_To_Rep (Typ, Index_Typ);

      else
         --  Build list of literal references

         Lst := New_List;
         Ent := First_Literal (Typ);
         while Present (Ent) loop
            Append_To (Lst, New_Occurrence_Of (Ent, Sloc (Ent)));
            Next_Literal (Ent);
         end loop;

         --  Now build an array declaration

         --    typA : constant array (Natural range 0 .. num - 1) of typ :=
         --             (v, v, v, v, v, ....)

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
                     Subtype_Mark =>
                       New_Occurrence_Of (Standard_Natural, Loc),
                     Constraint   =>
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
                     Subtype_Indication => New_Occurrence_Of (Typ, Loc))),

             Expression =>
               Make_Aggregate (Loc,
                 Expressions => Lst)));

         Set_Enum_Pos_To_Rep (Typ, Arr);
      end if;

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
      --  case and there is no obligation to raise Constraint_Error here). We
      --  also do this if pragma Restrictions (No_Exceptions) is active.

      --  Is this right??? What about No_Exception_Propagation???

      --  The underlying type is signed. Reset the Is_Unsigned_Type explicitly
      --  because it might have been inherited from the parent type.

      if Enumeration_Rep (First_Literal (Typ)) < 0 then
         Set_Is_Unsigned_Type (Typ, False);
      end if;

      Ityp := Integer_Type_For (Esize (Typ), Is_Unsigned_Type (Typ));

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
                    Intval => Enumeration_Rep (Ent)),
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

      --  In normal mode, add the others clause with the test.
      --  If Predicates_Ignored is True, validity checks do not apply to
      --  the subtype.

      if not No_Exception_Handlers_Set
        and then not Predicates_Ignored (Typ)
      then
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements       => New_List (
               Make_Raise_Constraint_Error (Loc,
                 Condition => Make_Identifier (Loc, Name_uF),
                 Reason    => CE_Invalid_Data),
               Make_Simple_Return_Statement (Loc,
                 Expression => Make_Integer_Literal (Loc, -1)))));

      --  If either of the restrictions No_Exceptions_Handlers/Propagation is
      --  active then return -1 (we cannot usefully raise Constraint_Error in
      --  this case). See description above for further details.

      else
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements       => New_List (
               Make_Simple_Return_Statement (Loc,
                 Expression => Make_Integer_Literal (Loc, -1)))));
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
                  Parameter_Type => New_Occurrence_Of (Typ, Loc)),
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_uF),
                  Parameter_Type =>
                    New_Occurrence_Of (Standard_Boolean, Loc))),

              Result_Definition => New_Occurrence_Of (Standard_Integer, Loc)),

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

      Set_Is_Inlined (Fent);

   exception
      when RE_Not_Available =>
         return;
   end Expand_Freeze_Enumeration_Type;

   -------------------------------
   -- Expand_Freeze_Record_Type --
   -------------------------------

   procedure Expand_Freeze_Record_Type (N : Node_Id) is

      procedure Build_Class_Condition_Subprograms (Typ : Entity_Id);
      --  Create internal subprograms of Typ primitives that have class-wide
      --  preconditions or postconditions; they are invoked by the caller to
      --  evaluate the conditions.

      procedure Build_Variant_Record_Equality (Typ  : Entity_Id);
      --  Create an equality function for the untagged variant record Typ and
      --  attach it to the TSS list.

      procedure Register_Dispatch_Table_Wrappers (Typ : Entity_Id);
      --  Register dispatch-table wrappers in the dispatch table of Typ

      procedure Validate_Tagged_Type_Extra_Formals (Typ : Entity_Id);
      --  Check extra formals of dispatching primitives of tagged type Typ.
      --  Used in pragma Debug.

      ---------------------------------------
      -- Build_Class_Condition_Subprograms --
      ---------------------------------------

      procedure Build_Class_Condition_Subprograms (Typ : Entity_Id) is
         Prim_List : constant Elist_Id := Primitive_Operations (Typ);
         Prim_Elmt : Elmt_Id           := First_Elmt (Prim_List);
         Prim      : Entity_Id;

      begin
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);

            --  Primitive with class-wide preconditions

            if Comes_From_Source (Prim)
              and then Has_Significant_Contract (Prim)
              and then
                (Present (Class_Preconditions (Prim))
                   or else Present (Ignored_Class_Preconditions (Prim)))
            then
               if Expander_Active then
                  Make_Class_Precondition_Subps (Prim);
               end if;

            --  Wrapper of a primitive that has or inherits class-wide
            --  preconditions.

            elsif Is_Primitive_Wrapper (Prim)
              and then
                (Present (Nearest_Class_Condition_Subprogram
                           (Spec_Id => Prim,
                            Kind    => Class_Precondition))
                   or else
                 Present (Nearest_Class_Condition_Subprogram
                           (Spec_Id => Prim,
                            Kind    => Ignored_Class_Precondition)))
            then
               if Expander_Active then
                  Make_Class_Precondition_Subps (Prim);
               end if;
            end if;

            Next_Elmt (Prim_Elmt);
         end loop;
      end Build_Class_Condition_Subprograms;

      -----------------------------------
      -- Build_Variant_Record_Equality --
      -----------------------------------

      procedure Build_Variant_Record_Equality (Typ : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (Typ);
         F   : constant Entity_Id  :=
                 Make_Defining_Identifier (Loc,
                   Chars => Make_TSS_Name (Typ, TSS_Composite_Equality));
      begin
         --  For a variant record with restriction No_Implicit_Conditionals
         --  in effect we skip building the procedure. This is safe because
         --  if we can see the restriction, so can any caller, and calls to
         --  equality test routines are not allowed for variant records if
         --  this restriction is active.

         if Restriction_Active (No_Implicit_Conditionals) then
            return;
         end if;

         --  Derived Unchecked_Union types no longer inherit the equality
         --  function of their parent.

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
           Build_Variant_Record_Equality
             (Typ         => Typ,
              Spec_Id     => Empty,
              Body_Id     => F,
              Param_Specs => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_X),
                  Parameter_Type      => New_Occurrence_Of (Typ, Loc)),

                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_Y),
                  Parameter_Type      => New_Occurrence_Of (Typ, Loc)))));

         Set_TSS (Typ, F);
         Set_Is_Pure (F);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (F);
         end if;
      end Build_Variant_Record_Equality;

      --------------------------------------
      -- Register_Dispatch_Table_Wrappers --
      --------------------------------------

      procedure Register_Dispatch_Table_Wrappers (Typ : Entity_Id) is
         Elmt : Elmt_Id := First_Elmt (Primitive_Operations (Typ));
         Subp : Entity_Id;

      begin
         while Present (Elmt) loop
            Subp := Node (Elmt);

            if Is_Dispatch_Table_Wrapper (Subp) then
               Append_Freeze_Actions (Typ,
                 Register_Primitive (Sloc (Subp), Subp));
            end if;

            Next_Elmt (Elmt);
         end loop;
      end Register_Dispatch_Table_Wrappers;

      ----------------------------------------
      -- Validate_Tagged_Type_Extra_Formals --
      ----------------------------------------

      procedure Validate_Tagged_Type_Extra_Formals (Typ : Entity_Id) is
         Ovr_Subp : Entity_Id;
         Elmt     : Elmt_Id;
         Subp     : Entity_Id;

      begin
         pragma Assert (not Is_Class_Wide_Type (Typ));

         --  No check required if expansion is not active since we never
         --  generate extra formals in such case.

         if not Expander_Active then
            return;
         end if;

         Elmt := First_Elmt (Primitive_Operations (Typ));
         while Present (Elmt) loop
            Subp := Node (Elmt);

            --  Extra formals of a dispatching primitive must match:

            --  1) The extra formals of its covered interface primitive

            if Present (Interface_Alias (Subp)) then
               pragma Assert
                 (Extra_Formals_Match_OK
                   (E     => Interface_Alias (Subp),
                    Ref_E => Alias (Subp)));
            end if;

            --  2) The extra formals of its renamed primitive

            if Present (Alias (Subp)) then
               pragma Assert
                 (Extra_Formals_Match_OK
                   (E     => Subp,
                    Ref_E => Ultimate_Alias (Subp)));
            end if;

            --  3) The extra formals of its overridden primitive

            if Present (Overridden_Operation (Subp)) then
               Ovr_Subp := Overridden_Operation (Subp);

               --  Handle controlling function wrapper

               if Is_Wrapper (Subp)
                 and then Ultimate_Alias (Ovr_Subp) = Subp
               then
                  if Present (Overridden_Operation (Ovr_Subp)) then
                     pragma Assert
                       (Extra_Formals_Match_OK
                         (E     => Subp,
                          Ref_E => Overridden_Operation (Ovr_Subp)));
                  end if;

               else
                  pragma Assert
                    (Extra_Formals_Match_OK
                      (E     => Subp,
                       Ref_E => Ovr_Subp));
               end if;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end Validate_Tagged_Type_Extra_Formals;

      --  Local variables

      Typ      : constant Node_Id := Entity (N);
      Typ_Decl : constant Node_Id := Parent (Typ);

      Predef_List : List_Id;

      Wrapper_Decl_List : List_Id;
      Wrapper_Body_List : List_Id := No_List;

      Renamed_Eq : Node_Id := Empty;
      --  Defining unit name for the predefined equality function in the case
      --  where the type has a primitive operation that is a renaming of
      --  predefined equality (but only if there is also an overriding
      --  user-defined equality function). Used to pass this entity from
      --  Make_Predefined_Primitive_Specs to Predefined_Primitive_Bodies.

   --  Start of processing for Expand_Freeze_Record_Type

   begin
      --  Build discriminant checking functions if not a derived type (for
      --  derived types that are not tagged types, always use the discriminant
      --  checking functions of the parent type). However, for untagged types
      --  the derivation may have taken place before the parent was frozen, so
      --  we copy explicitly the discriminant checking functions from the
      --  parent into the components of the derived type.

      Build_Or_Copy_Discr_Checking_Funcs (Typ_Decl);

      if Is_Derived_Type (Typ)
        and then Is_Limited_Type (Typ)
        and then Is_Tagged_Type (Typ)
      then
         Check_Stream_Attributes (Typ);
      end if;

      --  Handle constructors of untagged CPP_Class types

      if not Is_Tagged_Type (Typ) and then Is_CPP_Class (Typ) then
         Set_CPP_Constructors (Typ);
      end if;

      --  Creation of the Dispatch Table. Note that a Dispatch Table is built
      --  for regular tagged types as well as for Ada types deriving from a C++
      --  Class, but not for tagged types directly corresponding to C++ classes
      --  In the later case we assume that it is created in the C++ side and we
      --  just use it.

      if Is_Tagged_Type (Typ) then

         --  Add the _Tag component

         if Underlying_Type (Etype (Typ)) = Typ then
            Expand_Tagged_Root (Typ);
         end if;

         if Is_CPP_Class (Typ) then
            Set_All_DT_Position (Typ);

            --  Create the tag entities with a minimum decoration

            if Tagged_Type_Expansion then
               Append_Freeze_Actions (Typ, Make_Tags (Typ));
            end if;

            Set_CPP_Constructors (Typ);

         else
            if not Building_Static_DT (Typ) then

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
                  Elmt := First_Elmt (Primitive_Operations (Typ));
                  while Present (Elmt) loop
                     Subp := Node (Elmt);

                     if Present (Alias (Subp)) then
                        if Is_CPP_Class (Etype (Typ)) then
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

            Set_Is_Frozen (Typ, False);

            --  Do not add the spec of predefined primitives in case of
            --  CPP tagged type derivations that have convention CPP.

            if Is_CPP_Class (Root_Type (Typ))
              and then Convention (Typ) = Convention_CPP
            then
               null;

            --  Do not add the spec of the predefined primitives if we are
            --  compiling under restriction No_Dispatching_Calls.

            elsif not Restriction_Active (No_Dispatching_Calls) then
               Make_Predefined_Primitive_Specs (Typ, Predef_List, Renamed_Eq);
               Insert_List_Before_And_Analyze (N, Predef_List);
            end if;

            --  Ada 2005 (AI-391): For a nonabstract null extension, create
            --  wrapper functions for each nonoverridden inherited function
            --  with a controlling result of the type. The wrapper for such
            --  a function returns an extension aggregate that invokes the
            --  parent function.

            if Ada_Version >= Ada_2005
              and then not Is_Abstract_Type (Typ)
              and then Is_Null_Extension (Typ)
            then
               Make_Controlling_Function_Wrappers
                 (Typ, Wrapper_Decl_List, Wrapper_Body_List);
               Insert_List_Before_And_Analyze (N, Wrapper_Decl_List);
            end if;

            --  Ada 2005 (AI-251): For a nonabstract type extension, build
            --  null procedure declarations for each set of homographic null
            --  procedures that are inherited from interface types but not
            --  overridden. This is done to ensure that the dispatch table
            --  entry associated with such null primitives are properly filled.

            if Ada_Version >= Ada_2005
              and then Etype (Typ) /= Typ
              and then not Is_Abstract_Type (Typ)
              and then Has_Interfaces (Typ)
            then
               Insert_Actions (N, Make_Null_Procedure_Specs (Typ));
            end if;

            Set_Is_Frozen (Typ);

            if not Is_Derived_Type (Typ)
              or else Is_Tagged_Type (Etype (Typ))
            then
               Set_All_DT_Position (Typ);

            --  If this is a type derived from an untagged private type whose
            --  full view is tagged, the type is marked tagged for layout
            --  reasons, but it has no dispatch table.

            elsif Is_Derived_Type (Typ)
              and then Is_Private_Type (Etype (Typ))
              and then not Is_Tagged_Type (Etype (Typ))
            then
               return;
            end if;

            --  Create and decorate the tags. Suppress their creation when
            --  not Tagged_Type_Expansion because the dispatching mechanism is
            --  handled internally by the virtual target.

            if Tagged_Type_Expansion then
               Append_Freeze_Actions (Typ, Make_Tags (Typ));

               --  Generate dispatch table of locally defined tagged type.
               --  Dispatch tables of library level tagged types are built
               --  later (see Build_Static_Dispatch_Tables).

               if not Building_Static_DT (Typ) then
                  Append_Freeze_Actions (Typ, Make_DT (Typ));

                  --  Register dispatch table wrappers in the dispatch table.
                  --  It could not be done when these wrappers were built
                  --  because, at that stage, the dispatch table was not
                  --  available.

                  Register_Dispatch_Table_Wrappers (Typ);
               end if;
            end if;

            --  If the type has unknown discriminants, propagate dispatching
            --  information to its underlying record view, which does not get
            --  its own dispatch table.

            if Is_Derived_Type (Typ)
              and then Has_Unknown_Discriminants (Typ)
              and then Present (Underlying_Record_View (Typ))
            then
               declare
                  Rep : constant Entity_Id := Underlying_Record_View (Typ);
               begin
                  Set_Access_Disp_Table
                    (Rep, Access_Disp_Table           (Typ));
                  Set_Dispatch_Table_Wrappers
                    (Rep, Dispatch_Table_Wrappers     (Typ));
                  Set_Direct_Primitive_Operations
                    (Rep, Direct_Primitive_Operations (Typ));
               end;
            end if;

            --  Make sure that the primitives Initialize, Adjust and Finalize
            --  are Frozen before other TSS subprograms. We don't want them
            --  frozen inside.

            if Is_Controlled (Typ) then
               Append_Freeze_Actions (Typ,
                 Freeze_Entity
                   (Find_Controlled_Prim_Op (Typ, Name_Initialize), Typ));

               if not Is_Limited_Type (Typ) then
                  Append_Freeze_Actions (Typ,
                    Freeze_Entity
                      (Find_Controlled_Prim_Op (Typ, Name_Adjust), Typ));
               end if;

               Append_Freeze_Actions (Typ,
                 Freeze_Entity
                   (Find_Controlled_Prim_Op (Typ, Name_Finalize), Typ));
            end if;

            --  Freeze rest of primitive operations. There is no need to handle
            --  the predefined primitives if we are compiling under restriction
            --  No_Dispatching_Calls.

            if not Restriction_Active (No_Dispatching_Calls) then
               Append_Freeze_Actions (Typ, Predefined_Primitive_Freeze (Typ));
            end if;
         end if;

      --  In the untagged case, ever since Ada 83 an equality function must
      --  be provided for variant records that are not unchecked unions.

      elsif Has_Discriminants (Typ)
        and then not Is_Limited_Type (Typ)
        and then Present (Component_List (Type_Definition (Typ_Decl)))
        and then
          Present (Variant_Part (Component_List (Type_Definition (Typ_Decl))))
      then
         Build_Variant_Record_Equality (Typ);

      --  In Ada 2012 the equality function composes, and thus must be built
      --  explicitly just as for tagged records.

      --  This is done unconditionally to ensure that tools can be linked
      --  properly with user programs compiled with older language versions.
      --  In addition, this is needed because "=" composes for bounded strings
      --  in all language versions (see Exp_Ch4.Expand_Composite_Equality).

      elsif Comes_From_Source (Typ)
        and then Convention (Typ) = Convention_Ada
        and then not Is_Limited_Type (Typ)
      then
         Build_Untagged_Record_Equality (Typ);
      end if;

      --  Before building the record initialization procedure, if we are
      --  dealing with a concurrent record value type, then we must go through
      --  the discriminants, exchanging discriminals between the concurrent
      --  type and the concurrent record value type. See the section "Handling
      --  of Discriminants" in the Einfo spec for details.

      if Is_Concurrent_Record_Type (Typ) and then Has_Discriminants (Typ) then
         declare
            Ctyp       : constant Entity_Id :=
                           Corresponding_Concurrent_Type (Typ);
            Conc_Discr : Entity_Id;
            Rec_Discr  : Entity_Id;
            Temp       : Entity_Id;

         begin
            Conc_Discr := First_Discriminant (Ctyp);
            Rec_Discr  := First_Discriminant (Typ);
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

      if Has_Controlled_Component (Typ) then
         Build_Controlling_Procs (Typ);
      end if;

      Adjust_Discriminants (Typ);

      --  Do not need init for interfaces on virtual targets since they're
      --  abstract.

      if not Is_Mutably_Tagged_CW_Equivalent_Type (Typ)
        and then (Tagged_Type_Expansion or else not Is_Interface (Typ))
      then
         Build_Record_Init_Proc (Typ_Decl, Typ);
      end if;

     --  Create the body of TSS primitive Finalize_Address. This must be done
     --  before the bodies of all predefined primitives are created. If Typ
     --  is limited, Stream_Input and Stream_Read may produce build-in-place
     --  allocations and for those the expander needs Finalize_Address.

      if Is_Controlled (Typ) then
         Make_Finalize_Address_Body (Typ);
      end if;

      --  For tagged type that are not interfaces, build bodies of primitive
      --  operations. Note: do this after building the record initialization
      --  procedure, since the primitive operations may need the initialization
      --  routine. There is no need to add predefined primitives of interfaces
      --  because all their predefined primitives are abstract.

      if Is_Tagged_Type (Typ) and then not Is_Interface (Typ) then

         --  Do not add the body of predefined primitives in case of CPP tagged
         --  type derivations that have convention CPP.

         if Is_CPP_Class (Root_Type (Typ))
           and then Convention (Typ) = Convention_CPP
         then
            null;

         --  Do not add the body of the predefined primitives if we are
         --  compiling under restriction No_Dispatching_Calls.

         elsif not Restriction_Active (No_Dispatching_Calls) then
            --  Create the body of the class-wide type's TSS primitive
            --  Finalize_Address. This must be done before any class-wide
            --  precondition functions are created.

            Make_Finalize_Address_Body (Class_Wide_Type (Typ));

            Predef_List := Predefined_Primitive_Bodies (Typ, Renamed_Eq);
            Append_Freeze_Actions (Typ, Predef_List);
         end if;

         --  Ada 2005 (AI-391): If any wrappers were created for nonoverridden
         --  inherited functions, then add their bodies to the freeze actions.

         Append_Freeze_Actions (Typ, Wrapper_Body_List);

      --  Create body of an interface type's class-wide type's TSS primitive
      --  Finalize_Address.

      elsif Is_Tagged_Type (Typ)
        and then Is_Interface (Typ)
        and then not Restriction_Active (No_Dispatching_Calls)
      then
         Make_Finalize_Address_Body (Class_Wide_Type (Typ));
      end if;

      --  Create extra formals for the primitive operations of the type.
      --  This must be done before analyzing the body of the initialization
      --  procedure, because a self-referential type might call one of these
      --  primitives in the body of the init_proc itself.
      --
      --  This is not needed:
      --    1) If expansion is disabled, because extra formals are only added
      --       when we are generating code.
      --
      --    2) For types with foreign convention since primitives with foreign
      --       convention don't have extra formals and AI95-117 requires that
      --       all primitives of a tagged type inherit the convention.

      if Expander_Active
        and then Is_Tagged_Type (Typ)
        and then not Has_Foreign_Convention (Typ)
      then
         declare
            Elmt : Elmt_Id;
            E    : Entity_Id;

         begin
            --  Add extra formals to primitive operations

            Elmt := First_Elmt (Primitive_Operations (Typ));
            while Present (Elmt) loop
               Create_Extra_Formals (Node (Elmt));
               Next_Elmt (Elmt);
            end loop;

            --  Add extra formals to renamings of primitive operations. The
            --  addition of extra formals is done in two steps to minimize
            --  the compile time required for this action; the evaluation of
            --  Find_Dispatching_Type() and Contains() is only done here for
            --  renamings that are not primitive operations.

            E := First_Entity (Scope (Typ));
            while Present (E) loop
               if Is_Dispatching_Operation (E)
                 and then Present (Alias (E))
                 and then Find_Dispatching_Type (E) = Typ
                 and then not Contains (Primitive_Operations (Typ), E)
               then
                  Create_Extra_Formals (E);
               end if;

               Next_Entity (E);
            end loop;

            pragma Debug (Validate_Tagged_Type_Extra_Formals (Typ));
         end;
      end if;

      --  Build internal subprograms of primitives with class-wide
      --  pre/postconditions.

      if Is_Tagged_Type (Typ) then
         Build_Class_Condition_Subprograms (Typ);
      end if;
   end Expand_Freeze_Record_Type;

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

         if Is_Incomplete_Type (Desig_Typ)
           and then Present (Non_Limited_View (Desig_Typ))
         then
            Desig_Typ := Non_Limited_View (Desig_Typ);
         end if;

         --  Anonymous access types are created for the components of the
         --  record parameter for an entry declaration. No master is created
         --  for such a type.

         if Has_Task (Desig_Typ) then
            Build_Master_Entity (Ptr_Typ);
            Build_Master_Renaming (Ptr_Typ);

         --  Create a class-wide master because a Master_Id must be generated
         --  for access-to-limited-class-wide types whose root may be extended
         --  with task components.

         --  Note: This code covers access-to-limited-interfaces because they
         --        can be used to reference tasks implementing them.

         --  Suppress the master creation for access types created for entry
         --  formal parameters (parameter block component types). Seems like
         --  suppression should be more general for compiler-generated types,
         --  but testing Comes_From_Source may be too general in this case
         --  (affects some test output)???

         elsif not Is_Param_Block_Component_Type (Ptr_Typ)
           and then Is_Limited_Class_Wide_Type (Desig_Typ)
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
            M_Id  : Entity_Id := Empty;
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
                 and then Might_Have_Tasks
                            (Available_View (Designated_Type (Typ)))
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
                     pragma Assert (Present (M_Id));
                     Set_Master_Id (Typ, M_Id);
                  end if;
               end if;

               Next_Entity (Comp);
            end loop;
         end;
      end if;

      --  Handle mutably tagged types by replacing their declarations with
      --  their class-wide equivalent types.

      declare
         Comp : Entity_Id;
      begin
         if Is_Array_Type (Def_Id) then
            Comp := First_Entity (Component_Type (Def_Id));
         else
            Comp := First_Entity (Def_Id);
         end if;

         while Present (Comp) loop
            if Ekind (Etype (Comp)) /= E_Void
              and then Is_Mutably_Tagged_Type (Etype (Comp))
            then
               Set_Etype
                 (Comp, Class_Wide_Equivalent_Type (Etype (Comp)));
            end if;
            Next_Entity (Comp);
         end loop;
      end;

      Par_Id := Etype (B_Id);

      --  The parent type is private then we need to inherit any TSS operations
      --  from the full view.

      if Is_Private_Type (Par_Id)
        and then Present (Full_View (Par_Id))
      then
         Par_Id := Base_Type (Full_View (Par_Id));
      end if;

      if Nkind (Type_Definition (N)) = N_Derived_Type_Definition
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

            if Is_Private_Type (B_Id)
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
      Loc      : constant Source_Ptr := Sloc (N);
      Def_Id   : constant Entity_Id  := Defining_Identifier (N);
      Expr     : constant Node_Id    := Expression (N);
      Obj_Def  : constant Node_Id    := Object_Definition (N);
      Typ      : constant Entity_Id  := Etype (Def_Id);
      Base_Typ : constant Entity_Id  := Base_Type (Typ);
      Next_N   : constant Node_Id    := Next (N);

      Special_Ret_Obj : constant Boolean := Is_Special_Return_Object (Def_Id);
      --  If this is a special return object, it will be allocated differently
      --  and ultimately rewritten as a renaming, so initialization activities
      --  need to be deferred until after that is done.

      Func_Id : constant Entity_Id :=
       (if Special_Ret_Obj then Return_Applies_To (Scope (Def_Id)) else Empty);
      --  The function if this is a special return object, otherwise Empty

      function Build_Heap_Or_Pool_Allocator
        (Temp_Id    : Entity_Id;
         Temp_Typ   : Entity_Id;
         Ret_Typ    : Entity_Id;
         Alloc_Expr : Node_Id) return Node_Id;
      --  Create the statements necessary to allocate a return object on the
      --  heap or user-defined storage pool. The object may need finalization
      --  actions depending on the return type.
      --
      --    * Controlled case
      --
      --       if BIPcollection = null then
      --          Temp_Id := <Alloc_Expr>;
      --       else
      --          declare
      --             type Ptr_Typ is access Ret_Typ;
      --             for Ptr_Typ'Storage_Pool use BIPstoragepool.all;
      --             Local : Ptr_Typ;
      --
      --          begin
      --             procedure Allocate (...) is
      --             begin
      --                System.Storage_Pools.Subpools.Allocate_Any (...);
      --             end Allocate;
      --
      --             Local := <Alloc_Expr>;
      --             Temp_Id := Temp_Typ (Local);
      --          end;
      --       end if;
      --
      --    * Non-controlled case
      --
      --       Temp_Id := <Alloc_Expr>;
      --
      --  Temp_Id is the temporary which is used to reference the internally
      --  created object in all allocation forms. Temp_Typ is the type of the
      --  temporary. Func_Id is the enclosing function. Ret_Typ is the return
      --  type of Func_Id. Alloc_Expr is the actual allocator.

      function BIP_Function_Call_Id return Entity_Id;
      --  If the object initialization expression is a call to a build-in-place
      --  function, return the id of the called function; otherwise return
      --  Empty.

      procedure Count_Default_Sized_Task_Stacks
        (Typ         : Entity_Id;
         Pri_Stacks  : out Int;
         Sec_Stacks  : out Int);
      --  Count the number of default-sized primary and secondary task stacks
      --  required for task objects contained within type Typ. If the number of
      --  task objects contained within the type is not known at compile time
      --  the procedure will return the stack counts of zero.

      procedure Default_Initialize_Object (After : Node_Id);
      --  Generate all default initialization actions for object Def_Id. Any
      --  new code is inserted after node After.

      procedure Initialize_Return_Object
        (Tag_Assign : Node_Id;
         Adj_Call   : Node_Id;
         Expr       : Node_Id;
         Init_Stmt  : Node_Id;
         After      : Node_Id);
      --  Generate all initialization actions for return object Def_Id. Any
      --  new code is inserted after node After.

      function Is_Renamable_Function_Call (Expr : Node_Id) return Boolean;
      --  If we are not at library level and the object declaration originally
      --  appears in the form:

      --    Obj : Typ := Func (...);

      --  and has been rewritten as the dereference of a captured reference
      --  to the function result built either on the primary or the secondary
      --  stack, then the declaration can be rewritten as the renaming of this
      --  dereference:

      --    type Ann is access all Typ;
      --    Rnn : constant Axx := Func (...)'reference;
      --    Obj : Typ renames Rnn.all;

      --  This will avoid making an extra copy and, in the case where Typ needs
      --  finalization, a pair of calls to the Adjust and Finalize primitives,
      --  or Deep_Adjust and Deep_Finalize routines, depending on whether Typ
      --  has components that themselves need finalization.

      --  However, in the case of a special return object, we need to make sure
      --  that the object Rnn is recognized by the Is_Related_To_Func_Return
      --  predicate; otherwise, if it is of a type that needs finalization,
      --  then Requires_Cleanup_Actions would return true because of this and
      --  Build_Finalizer would finalize it prematurely because of this (see
      --  also Expand_Simple_Function_Return for the same test in the case of
      --  a simple return).

      --  Finally, in the case of a special return object, we also need to make
      --  sure that the two functions return on the same stack, otherwise we
      --  would create a dangling reference.

      function Make_Allocator_For_Return (Expr : Node_Id) return Node_Id;
      --  Make an allocator for a return object initialized with Expr

      function OK_To_Rename_Ref (N : Node_Id) return Boolean;
      --  Return True if N denotes an entity with OK_To_Rename set

      ----------------------------------
      -- Build_Heap_Or_Pool_Allocator --
      ----------------------------------

      function Build_Heap_Or_Pool_Allocator
        (Temp_Id    : Entity_Id;
         Temp_Typ   : Entity_Id;
         Ret_Typ    : Entity_Id;
         Alloc_Expr : Node_Id) return Node_Id
      is
      begin
         pragma Assert (Is_Build_In_Place_Function (Func_Id));

         --  Processing for objects that require finalization actions

         if Needs_Finalization (Ret_Typ) then
            declare
               Decls       : constant List_Id := New_List;
               Fin_Coll_Id : constant Entity_Id :=
                 Build_In_Place_Formal (Func_Id, BIP_Collection);
               Orig_Expr   : constant Node_Id := New_Copy_Tree (Alloc_Expr);
               Stmts       : constant List_Id := New_List;
               Local_Id    : Entity_Id;
               Pool_Id     : Entity_Id;
               Ptr_Typ     : Entity_Id;

            begin
               --  Generate:
               --    Pool_Id renames BIPstoragepool.all;

               --  This formal is not added on ZFP as those targets do not
               --  support pools.

               if RTE_Available (RE_Root_Storage_Pool_Ptr) then
                  Pool_Id := Make_Temporary (Loc, 'P');

                  Append_To (Decls,
                    Make_Object_Renaming_Declaration (Loc,
                      Defining_Identifier => Pool_Id,
                      Subtype_Mark        =>
                        New_Occurrence_Of (RTE (RE_Root_Storage_Pool), Loc),
                      Name                =>
                        Make_Explicit_Dereference (Loc,
                          New_Occurrence_Of
                            (Build_In_Place_Formal
                               (Func_Id, BIP_Storage_Pool), Loc))));
               else
                  Pool_Id := Empty;
               end if;

               --  Create an access type which uses the storage pool of the
               --  caller. This additional type is necessary because the
               --  finalization collection cannot be associated with the type
               --  of the temporary. Otherwise the secondary stack allocation
               --  will fail.

               --  Generate:
               --    type Ptr_Typ is access Ret_Typ;

               Ptr_Typ := Make_Temporary (Loc, 'P');

               Append_To (Decls,
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Ptr_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Occurrence_Of (Ret_Typ, Loc))));

               --  Perform minor decoration in order to set the collection and
               --  the storage pool attributes.

               Mutate_Ekind                (Ptr_Typ, E_Access_Type);
               Set_Finalization_Collection (Ptr_Typ, Fin_Coll_Id);
               Set_Associated_Storage_Pool (Ptr_Typ, Pool_Id);

               --  Create the temporary, generate:
               --    Local_Id : Ptr_Typ;

               Local_Id := Make_Temporary (Loc, 'T');

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Local_Id,
                   Object_Definition   =>
                     New_Occurrence_Of (Ptr_Typ, Loc)));
               Set_No_Initialization (Last (Decls));

               --  Allocate the object, generate:
               --    Local_Id := <Alloc_Expr>;

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (Local_Id, Loc),
                   Expression => Alloc_Expr));

               --  Generate:
               --    Temp_Id := Temp_Typ (Local_Id);

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (Temp_Id, Loc),
                   Expression =>
                     Unchecked_Convert_To (Temp_Typ,
                       New_Occurrence_Of (Local_Id, Loc))));

               --  Wrap the allocation in a block to make it conditioned by the
               --  presence of the caller's collection at run time.

               --  Generate:
               --    if BIPcollection = null then
               --       Temp_Id := <Orig_Expr>;
               --    else
               --       declare
               --          <Decls>
               --       begin
               --          <Stmts>
               --       end;
               --    end if;

               return
                 Make_If_Statement (Loc,
                   Condition       =>
                     Make_Op_Eq (Loc,
                       Left_Opnd  => New_Occurrence_Of (Fin_Coll_Id, Loc),
                       Right_Opnd => Make_Null (Loc)),

                   Then_Statements => New_List (
                     Make_Assignment_Statement (Loc,
                       Name       => New_Occurrence_Of (Temp_Id, Loc),
                       Expression => Orig_Expr)),

                   Else_Statements => New_List (
                     Make_Block_Statement (Loc,
                       Declarations               => Decls,
                       Handled_Statement_Sequence =>
                         Make_Handled_Sequence_Of_Statements (Loc,
                           Statements => Stmts))));
            end;

         --  For all other cases, generate:
         --    Temp_Id := <Alloc_Expr>;

         else
            return
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Temp_Id, Loc),
                Expression => Alloc_Expr);
         end if;
      end Build_Heap_Or_Pool_Allocator;

      --------------------------
      -- BIP_Function_Call_Id --
      --------------------------

      function BIP_Function_Call_Id return Entity_Id is

         function Func_Call_Id (Function_Call : Node_Id) return Entity_Id;
         --  Return the id of the called function.

         function Func_Call_Id (Function_Call : Node_Id) return Entity_Id is
            Call_Node : constant Node_Id := Unqual_Conv (Function_Call);

         begin
            if Is_Entity_Name (Name (Call_Node)) then
               return Entity (Name (Call_Node));

            elsif Nkind (Name (Call_Node)) = N_Explicit_Dereference then
               return Etype (Name (Call_Node));

            else
               pragma Assert (Nkind (Name (Call_Node)) = N_Selected_Component);
               return Etype (Entity (Selector_Name (Name (Call_Node))));
            end if;
         end Func_Call_Id;

         --  Local declarations

         BIP_Func_Call : Node_Id;
         Expr_Q        : constant Node_Id := Unqual_Conv (Expr);

      --  Start of processing for BIP_Function_Call_Id

      begin
         if Is_Build_In_Place_Function_Call (Expr_Q) then
            return Func_Call_Id (Expr_Q);
         end if;

         BIP_Func_Call := Unqual_BIP_Iface_Function_Call (Expr_Q);

         if Present (BIP_Func_Call) then

            --  In the case of an explicitly dereferenced call, return the
            --  subprogram type.

            if Nkind (Name (BIP_Func_Call)) = N_Explicit_Dereference then
               return Etype (Name (BIP_Func_Call));
            else
               pragma Assert (Is_Entity_Name (Name (BIP_Func_Call)));
               return Entity (Name (BIP_Func_Call));
            end if;

         elsif Nkind (Expr_Q) = N_Reference
                 and then Is_Build_In_Place_Function_Call (Prefix (Expr_Q))
         then
            return Func_Call_Id (Prefix (Expr_Q));

         else
            return Empty;
         end if;
      end BIP_Function_Call_Id;

      -------------------------------------
      -- Count_Default_Sized_Task_Stacks --
      -------------------------------------

      procedure Count_Default_Sized_Task_Stacks
        (Typ         : Entity_Id;
         Pri_Stacks  : out Int;
         Sec_Stacks  : out Int)
      is
         Component : Entity_Id;

      begin
         --  To calculate the number of default-sized task stacks required for
         --  an object of Typ, a depth-first recursive traversal of the AST
         --  from the Typ entity node is undertaken. Only type nodes containing
         --  task objects are visited.

         Pri_Stacks := 0;
         Sec_Stacks := 0;

         if not Has_Task (Typ) then
            return;
         end if;

         case Ekind (Typ) is
            when E_Task_Subtype
               | E_Task_Type
            =>
               --  A task type is found marking the bottom of the descent. If
               --  the type has no representation aspect for the corresponding
               --  stack then that stack is using the default size.

               if Present (Get_Rep_Item (Typ, Name_Storage_Size)) then
                  Pri_Stacks := 0;
               else
                  Pri_Stacks := 1;
               end if;

               if Present (Get_Rep_Item (Typ, Name_Secondary_Stack_Size)) then
                  Sec_Stacks := 0;
               else
                  Sec_Stacks := 1;
               end if;

            when E_Array_Subtype
               | E_Array_Type
            =>
               --  First find the number of default stacks contained within an
               --  array component.

               Count_Default_Sized_Task_Stacks
                 (Component_Type (Typ),
                  Pri_Stacks,
                  Sec_Stacks);

               --  Then multiply the result by the size of the array

               declare
                  Quantity : constant Nat := Number_Of_Elements_In_Array (Typ);
                  --  Number_Of_Elements_In_Array is non-trival, consequently
                  --  its result is captured as an optimization.

               begin
                  Pri_Stacks := Pri_Stacks * Quantity;
                  Sec_Stacks := Sec_Stacks * Quantity;
               end;

            when E_Protected_Subtype
               | E_Protected_Type
               | E_Record_Subtype
               | E_Record_Type
            =>
               Component := First_Component (Typ);

               --  Recursively descend each component of the composite type
               --  looking for tasks.

               while Present (Component) loop
                  declare
                     P : Int;
                     S : Int;

                  begin
                     Count_Default_Sized_Task_Stacks (Etype (Component), P, S);
                     Pri_Stacks := Pri_Stacks + P;
                     Sec_Stacks := Sec_Stacks + S;
                  end;

                  Next_Component (Component);
               end loop;

            when E_Limited_Private_Subtype
               | E_Limited_Private_Type
               | E_Record_Subtype_With_Private
               | E_Record_Type_With_Private
            =>
               --  Switch to the full view of the private type to continue
               --  search.

               Count_Default_Sized_Task_Stacks
                 (Full_View (Typ), Pri_Stacks, Sec_Stacks);

            --  Other types should not contain tasks

            when others =>
               raise Program_Error;
         end case;
      end Count_Default_Sized_Task_Stacks;

      -------------------------------
      -- Default_Initialize_Object --
      -------------------------------

      procedure Default_Initialize_Object (After : Node_Id) is
         Init_Expr  : Node_Id;
         Init_Stmts : List_Id;

      begin
         --  Nothing to do if the object has an initialization expression or
         --  need not be initialized.

         if Has_Init_Expression (N) or else No_Initialization (N) then
            return;

         --  Default initialization is suppressed for objects that are already
         --  known to be imported (i.e. whose declaration specifies the Import
         --  aspect). Note that for objects with a pragma Import, we generate
         --  initialization here, and then remove it downstream when processing
         --  the pragma. It is also suppressed for variables for which a pragma
         --  Suppress_Initialization has been explicitly given

         elsif Is_Imported (Def_Id)
           or else Suppress_Initialization (Def_Id)
         then
            return;

         --  Nothing to do if the object being initialized is of a task type
         --  and restriction No_Tasking is in effect, because this is a direct
         --  violation of the restriction.

         elsif Is_Task_Type (Base_Typ)
           and then Restriction_Active (No_Tasking)
         then
            return;
         end if;

         --  First try a simple initialization; if it succeeds, then we just
         --  set the value as the expression of the declaration and let the
         --  code generator do the rest.

         Init_Expr := Build_Default_Simple_Initialization (N, Typ, Def_Id);

         if Present (Init_Expr) then
            Set_Expression (N, Init_Expr);
            Analyze_And_Resolve (Init_Expr, Typ);
            return;
         end if;

         --  Or else build the fully-fledged initialization if need be

         if Is_Mutably_Tagged_Type (Typ) then
            Init_Stmts :=
              Build_Default_Initialization (N, Etype (Typ), Def_Id);
         else
            Init_Stmts := Build_Default_Initialization (N, Typ, Def_Id);
         end if;

         --  Insert the whole initialization sequence into the tree. If the
         --  object has a delayed freeze, as will be the case when it has
         --  aspect specifications, the initialization sequence is part of
         --  the freeze actions.

         if Present (Init_Stmts) then
            if Has_Delayed_Freeze (Def_Id) then
               Append_Freeze_Actions (Def_Id, Init_Stmts);
            else
               Insert_Actions_After (After, Init_Stmts);
            end if;
         end if;
      end Default_Initialize_Object;

      ------------------------------
      -- Initialize_Return_Object --
      ------------------------------

      procedure Initialize_Return_Object
        (Tag_Assign : Node_Id;
         Adj_Call   : Node_Id;
         Expr       : Node_Id;
         Init_Stmt  : Node_Id;
         After      : Node_Id)
      is
      begin
         if Present (Tag_Assign) then
            Insert_Action_After (After, Tag_Assign);
         end if;

         if Present (Adj_Call) then
            Insert_Action_After (After, Adj_Call);
         end if;

         if No (Expr) then
            Default_Initialize_Object (After);

         elsif Is_Delayed_Aggregate (Expr)
           and then not No_Initialization (N)
         then
            Convert_Aggr_In_Object_Decl (N);

         elsif Present (Init_Stmt) then
            Insert_Action_After (After, Init_Stmt);
            Set_Expression (N, Empty);
         end if;
      end Initialize_Return_Object;

      --------------------------------
      -- Is_Renamable_Function_Call --
      --------------------------------

      function Is_Renamable_Function_Call (Expr : Node_Id) return Boolean is
      begin
         return not Is_Library_Level_Entity (Def_Id)
           and then Is_Captured_Function_Call (Expr)
           and then (not Special_Ret_Obj
                      or else
                        (Is_Related_To_Func_Return (Entity (Prefix (Expr)))
                          and then Needs_Secondary_Stack (Etype (Expr)) =
                                   Needs_Secondary_Stack (Etype (Func_Id))));
      end Is_Renamable_Function_Call;

      -------------------------------
      -- Make_Allocator_For_Return --
      -------------------------------

      function Make_Allocator_For_Return (Expr : Node_Id) return Node_Id is
         Alloc      : Node_Id;
         Alloc_Expr : Entity_Id;
         Alloc_Typ  : Entity_Id;

      begin
         --  If the return object's declaration does not include an expression,
         --  then we use its subtype for the allocation. Likewise in the case
         --  of a degenerate expression like a raise expression.

         if No (Expr)
           or else Nkind (Original_Node (Expr)) = N_Raise_Expression
         then
            Alloc_Typ := Typ;

         --  If the return object's declaration includes an expression, then
         --  there are two cases: either the nominal subtype of the object is
         --  definite and we can use it for the allocation directly, or it is
         --  not and Analyze_Object_Declaration should have built an actual
         --  subtype from the expression.

         --  However, there are exceptions in the latter case for interfaces
         --  (see Analyze_Object_Declaration), as well as class-wide types and
         --  types with unknown discriminants if they have no underlying record
         --  view or are inherently limited (see Expand_Subtype_From_Expr), so
         --  we must cope with them.

         elsif Is_Interface (Typ) then
            pragma Assert (Is_Class_Wide_Type (Typ));

            --  For interfaces, we use the type of the expression, except if
            --  we need to put back a conversion that we have removed earlier
            --  in the processing.

            if Is_Class_Wide_Type (Etype (Expr)) then
               Alloc_Typ := Typ;
            else
               Alloc_Typ := Etype (Expr);
            end if;

         elsif Is_Class_Wide_Type (Typ) then

            --  For class-wide types, we have to make sure that we use the
            --  dynamic type of the expression for the allocation, either by
            --  means of its (static) subtype or through the actual subtype.

            if Has_Tag_Of_Type (Expr) then
               Alloc_Typ := Etype (Expr);

            else pragma Assert (Ekind (Typ) = E_Class_Wide_Subtype
              and then Present (Equivalent_Type (Typ)));

               Alloc_Typ := Typ;
            end if;

         else pragma Assert (Is_Definite_Subtype (Typ)
           or else (Has_Unknown_Discriminants (Typ)
                     and then (No (Underlying_Record_View (Typ))
                                or else Is_Inherently_Limited_Type (Typ))));

            Alloc_Typ := Typ;
         end if;

         --  If the return object's declaration includes an expression and the
         --  declaration isn't marked as No_Initialization, then we generate an
         --  allocator with a qualified expression. Although this is necessary
         --  only in the case where the result type is an interface (or class-
         --  wide interface), we do it in all cases for the sake of consistency
         --  instead of subsequently generating a separate assignment.

         if Present (Expr)
           and then not Is_Delayed_Aggregate (Expr)
           and then not No_Initialization (N)
         then
            --  Ada 2005 (AI95-344): If the result type is class-wide, insert
            --  a check that the level of the return expression's underlying
            --  type is not deeper than the level of the master enclosing the
            --  function.

            --  AI12-043: The check is made immediately after the return object
            --  is created.

            if Is_Class_Wide_Type (Etype (Func_Id)) then
               Apply_CW_Accessibility_Check (Expr, Func_Id);
            end if;

            Alloc_Expr := New_Copy_Tree (Expr);

            if Etype (Alloc_Expr) /= Alloc_Typ then
               Alloc_Expr := Convert_To (Alloc_Typ, Alloc_Expr);
            end if;

            Alloc :=
              Make_Allocator (Loc,
                Expression =>
                  Make_Qualified_Expression (Loc,
                    Subtype_Mark =>
                      New_Occurrence_Of (Alloc_Typ, Loc),
                    Expression   => Alloc_Expr));

         else
            Alloc :=
              Make_Allocator (Loc,
                Expression => New_Occurrence_Of (Alloc_Typ, Loc));

            --  If the return object requires default initialization, then it
            --  will happen later following the elaboration of the renaming.
            --  If we don't turn it off here, then the object will be default
            --  initialized twice.

            Set_No_Initialization (Alloc);
         end if;

         --  Set the flag indicating that the allocator is made for a special
         --  return object. This is used to bypass various legality checks as
         --  well as to make sure that the result is not adjusted twice.

         Set_For_Special_Return_Object (Alloc);

         return Alloc;
      end Make_Allocator_For_Return;

      ----------------------
      -- OK_To_Rename_Ref --
      ----------------------

      function OK_To_Rename_Ref (N : Node_Id) return Boolean is
      begin
         return Is_Entity_Name (N)
           and then Ekind (Entity (N)) = E_Variable
           and then OK_To_Rename (Entity (N));
      end OK_To_Rename_Ref;

      --  Local variables

      Adj_Call   : Node_Id := Empty;
      Expr_Q     : Node_Id := Empty;
      Tag_Assign : Node_Id := Empty;

      Init_After : Node_Id := N;
      --  Node after which the initialization actions are to be inserted. This
      --  is normally N, except for the case of a shared passive variable, in
      --  which case the init proc call must be inserted only after the bodies
      --  of the shared variable procedures have been seen.

      Has_BIP_Init_Expr : Boolean := False;
      --  Whether the object is initialized with a BIP function call

      Rewrite_As_Renaming : Boolean := False;
      --  Whether to turn the declaration into a renaming at the end

   --  Start of processing for Expand_N_Object_Declaration

   begin
      --  Don't do anything for deferred constants. All proper actions will be
      --  expanded during the full declaration.

      if No (Expr) and Constant_Present (N) then
         return;
      end if;

      --  The type of the object cannot be abstract. This is diagnosed at the
      --  point the object is frozen, which happens after the declaration is
      --  fully expanded, so simply return now.

      if Is_Abstract_Type (Typ) then
         return;
      end if;

      --  No action needed for the internal imported dummy object added by
      --  Make_DT to compute the offset of the components that reference
      --  secondary dispatch tables; required to avoid never-ending loop
      --  processing this internal object declaration.

      if Tagged_Type_Expansion
        and then Is_Internal (Def_Id)
        and then Is_Imported (Def_Id)
        and then Related_Type (Def_Id) = Implementation_Base_Type (Typ)
      then
         return;
      end if;

      --  Make shared memory routines for shared passive variable

      if Is_Shared_Passive (Def_Id) then
         Init_After := Make_Shared_Var_Procs (N);
      end if;

      --  Determine whether the object is initialized with a BIP function call

      if Present (Expr) then
         Expr_Q := Unqualify (Expr);

         Has_BIP_Init_Expr :=
           Is_Build_In_Place_Function_Call (Expr_Q)
             or else Present (Unqual_BIP_Iface_Function_Call (Expr_Q))
             or else (Nkind (Expr_Q) = N_Reference
                        and then
                      Is_Build_In_Place_Function_Call (Prefix (Expr_Q)));
      end if;

      --  If tasks are being declared, make sure we have an activation chain
      --  defined for the tasks (has no effect if we already have one), and
      --  also that a Master variable is established (and that the appropriate
      --  enclosing construct is established as a task master).

      if Has_Task (Typ)
        or else Might_Have_Tasks (Typ)
        or else (Has_BIP_Init_Expr
                   and then Needs_BIP_Task_Actuals (BIP_Function_Call_Id))
      then
         Build_Activation_Chain_Entity (N);

         if Has_Task (Typ) then
            Build_Master_Entity (Def_Id);

         --  Handle objects initialized with BIP function calls

         elsif Has_BIP_Init_Expr then
            Build_Master_Entity (Def_Id);
         end if;
      end if;

      --  If No_Implicit_Heap_Allocations or No_Implicit_Task_Allocations
      --  restrictions are active then default-sized secondary stacks are
      --  generated by the binder and allocated by SS_Init. To provide the
      --  binder the number of stacks to generate, the number of default-sized
      --  stacks required for task objects contained within the object
      --  declaration N is calculated here as it is at this point where
      --  unconstrained types become constrained. The result is stored in the
      --  enclosing unit's Unit_Record.

      --  Note if N is an array object declaration that has an initialization
      --  expression, a second object declaration for the initialization
      --  expression is created by the compiler. To prevent double counting
      --  of the stacks in this scenario, the stacks of the first array are
      --  not counted.

      if Might_Have_Tasks (Typ)
        and then not Restriction_Active (No_Secondary_Stack)
        and then (Restriction_Active (No_Implicit_Heap_Allocations)
          or else Restriction_Active (No_Implicit_Task_Allocations))
        and then not (Is_Array_Type (Typ) and then Has_Init_Expression (N))
      then
         declare
            PS_Count, SS_Count : Int;
         begin
            Count_Default_Sized_Task_Stacks (Typ, PS_Count, SS_Count);
            Increment_Primary_Stack_Count (PS_Count);
            Increment_Sec_Stack_Count (SS_Count);
         end;
      end if;

      --  Default initialization required, and no expression present

      if No (Expr) then
         --  If we have a type with a variant part, the initialization proc
         --  will contain implicit tests of the discriminant values, which
         --  counts as a violation of the restriction No_Implicit_Conditionals.

         if Has_Variant_Part (Typ) then
            declare
               Msg : Boolean;

            begin
               Check_Restriction (Msg, No_Implicit_Conditionals, Obj_Def);

               if Msg then
                  Error_Msg_N
                    ("\initialization of variant record tests discriminants",
                     Obj_Def);
                  return;
               end if;
            end;
         end if;

         --  For the default initialization case, if we have a private type
         --  with invariants, and invariant checks are enabled, then insert an
         --  invariant check after the object declaration. Note that it is OK
         --  to clobber the object with an invalid value since if the exception
         --  is raised, then the object will go out of scope. In the case where
         --  an array object is initialized with an aggregate, the expression
         --  is removed. Check flag Has_Init_Expression to avoid generating a
         --  junk invariant check and flag No_Initialization to avoid checking
         --  an uninitialized object such as a compiler temporary used for an
         --  aggregate.

         if Has_Invariants (Base_Typ)
           and then Present (Invariant_Procedure (Base_Typ))
           and then not Has_Init_Expression (N)
           and then not No_Initialization (N)
         then
            --  If entity has an address clause or aspect, make invariant
            --  call into a freeze action for the explicit freeze node for
            --  object. Otherwise insert invariant check after declaration.

            if Present (Following_Address_Clause (N))
              or else Has_Aspect (Def_Id, Aspect_Address)
            then
               Ensure_Freeze_Node (Def_Id);
               Set_Has_Delayed_Freeze (Def_Id);
               Set_Is_Frozen (Def_Id, False);

               if not Partial_View_Has_Unknown_Discr (Typ) then
                  Append_Freeze_Action (Def_Id,
                    Make_Invariant_Call (New_Occurrence_Of (Def_Id, Loc)));
               end if;

            elsif not Partial_View_Has_Unknown_Discr (Typ) then
               Insert_After (N,
                 Make_Invariant_Call (New_Occurrence_Of (Def_Id, Loc)));
            end if;
         end if;

         --  For a special return object, the initialization must wait until
         --  after the object is turned into an allocator.

         if not Special_Ret_Obj then
            Default_Initialize_Object (Init_After);

            --  Check whether the object has been initialized above

            if Present (Expression (N)) then
               if Is_Access_Type (Typ) then
                  if Known_Non_Null (Expression (N)) then
                     Set_Is_Known_Non_Null (Def_Id);
                  elsif Known_Null (Expression (N)) then
                     Set_Is_Known_Null (Def_Id);
                  end if;
               end if;

               if Is_Delayed_Aggregate (Expression (N)) then
                  Convert_Aggr_In_Object_Decl (N);
               end if;
            end if;
         end if;

         --  Generate attribute for Persistent_BSS if needed

         if Persistent_BSS_Mode
           and then Comes_From_Source (N)
           and then Is_Potentially_Persistent_Type (Typ)
           and then not Has_Init_Expression (N)
           and then Is_Library_Level_Entity (Def_Id)
         then
            declare
               Prag : constant Node_Id :=
                 Make_Linker_Section_Pragma (Def_Id, Loc, ".persistent.bss");
            begin
               Insert_After (N, Prag);
               Analyze (Prag);
            end;
         end if;

      --  Explicit initialization present

      else
         --  Obtain actual expression from qualified expression

         Expr_Q := Unqualify (Expr);

         --  When we have the appropriate kind of aggregate in the expression
         --  (this has been determined during analysis of the aggregate by
         --  setting the Expansion_Delayed flag), let's perform in place
         --  assignment and thus avoid creating a temporary.

         if Is_Delayed_Aggregate (Expr_Q) then

            --  For a special return object, the transformation must wait until
            --  after the object is turned into an allocator.

            if not Special_Ret_Obj then
               Convert_Aggr_In_Object_Decl (N);
            end if;

         --  If the initialization expression is a conditional expression whose
         --  expansion has been delayed, assign it explicitly to the object but
         --  only after analyzing it again and expanding it.

         elsif Is_Delayed_Conditional_Expression (Expr_Q) then
            --  For a special return object, the transformation must wait until
            --  after the object is turned into an allocator, and will be done
            --  during the expansion of the allocator.

            if not Special_Ret_Obj then
               declare
                  Assign : constant Node_Id :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Def_Id, Loc),
                      Expression => Relocate_Node (Expr));

               begin
                  Set_Assignment_OK (Name (Assign));
                  Set_Analyzed (Expression (Assign), False);
                  Set_No_Finalize_Actions (Assign);
                  Insert_Action_After (Init_After, Assign);

                  --  Save the assignment statement when declaring a controlled
                  --  object. This reference is used later by the finalization
                  --  machinery to mark the object as successfully initialized

                  if Needs_Finalization (Typ) then
                     Set_Last_Aggregate_Assignment (Def_Id, Assign);
                  end if;

                  Set_Expression (N, Empty);
                  Set_No_Initialization (N);
               end;
            end if;

         --  Ada 2005 (AI-318-02): If the initialization expression is a call
         --  to a build-in-place function, then access to the declared object
         --  must be passed to the function. Currently we limit such functions
         --  to those with constrained limited result subtypes, but eventually
         --  plan to expand the allowed forms of functions that are treated as
         --  build-in-place.

         elsif Is_Build_In_Place_Function_Call (Expr_Q) then
            Make_Build_In_Place_Call_In_Object_Declaration (N, Expr_Q);

            --  The previous call expands the expression initializing the
            --  built-in-place object into further code that will be analyzed
            --  later. No further expansion needed here.

            return;

         --  This is the same as the previous 'elsif', except that the call has
         --  been transformed by other expansion activities into something like
         --  F(...)'Reference.

         elsif Nkind (Expr_Q) = N_Reference
           and then Is_Build_In_Place_Function_Call (Prefix (Expr_Q))
           and then not Is_Expanded_Build_In_Place_Call
                          (Unqual_Conv (Prefix (Expr_Q)))
         then
            Make_Build_In_Place_Call_In_Anonymous_Context (Prefix (Expr_Q));

            --  The previous call expands the expression initializing the
            --  built-in-place object into further code that will be analyzed
            --  later. No further expansion needed here.

            return;

         --  Ada 2005 (AI-318-02): Specialization of the previous case for
         --  expressions containing a build-in-place function call whose
         --  returned object covers interface types, and Expr_Q has calls to
         --  Ada.Tags.Displace to displace the pointer to the returned build-
         --  in-place object to reference the secondary dispatch table of a
         --  covered interface type.

         elsif Present (Unqual_BIP_Iface_Function_Call (Expr_Q)) then
            Make_Build_In_Place_Iface_Call_In_Object_Declaration (N, Expr_Q);

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
           --  then we've done it already and must not do it again.

           and then not
             (Nkind (Obj_Def) = N_Identifier
               and then Present (Equivalent_Type (Entity (Obj_Def))))
         then
            pragma Assert (Is_Class_Wide_Type (Typ));

            --  If the original node of the expression was a conversion
            --  to this specific class-wide interface type then restore
            --  the original node because we must copy the object before
            --  displacing the pointer to reference the secondary tag
            --  component. This code must be kept synchronized with the
            --  expansion done by routine Expand_Interface_Conversion

            if not Comes_From_Source (Expr)
              and then Nkind (Expr) = N_Explicit_Dereference
              and then Nkind (Original_Node (Expr)) = N_Type_Conversion
              and then Etype (Original_Node (Expr)) = Typ
            then
               Rewrite (Expr, Original_Node (Expression (N)));
            end if;

            --  Avoid expansion of redundant interface conversion

            if Nkind (Expr) = N_Type_Conversion
              and then Etype (Expr) = Typ
            then
               Expr_Q := Expression (Expr);
            else
               Expr_Q := Expr;
            end if;

            --  We may use a renaming if the initialization expression is a
            --  captured function call that meets a few conditions.

            Rewrite_As_Renaming := Is_Renamable_Function_Call (Expr_Q);

            --  If the object is a special return object, then bypass special
            --  treatment of class-wide interface initialization below. In this
            --  case, the expansion of the return object will take care of this
            --  initialization via the expansion of the allocator.

            if Special_Ret_Obj and then not Rewrite_As_Renaming then

               --  If the type needs finalization and is not inherently
               --  limited, then the target is adjusted after the copy
               --  and attached to the finalization list.

               if Needs_Finalization (Typ)
                 and then not Is_Inherently_Limited_Type (Typ)
               then
                  Adj_Call :=
                    Make_Adjust_Call (
                      Obj_Ref => New_Occurrence_Of (Def_Id, Loc),
                      Typ     => Base_Typ);
               end if;

            --  Renaming an expression of the object's type is immediate

            elsif Rewrite_As_Renaming
              and then Base_Type (Etype (Expr_Q)) = Base_Type (Typ)
            then
               null;

            elsif Tagged_Type_Expansion then
               declare
                  Iface : constant Entity_Id := Root_Type (Typ);

                  Expr_Typ     : Entity_Id;
                  New_Expr     : Node_Id;
                  Obj_Id       : Entity_Id;
                  Ptr_Obj_Decl : Node_Id;
                  Ptr_Obj_Id   : Entity_Id;
                  Tag_Comp     : Node_Id;

               begin
                  Expr_Typ := Base_Type (Etype (Expr_Q));
                  if Is_Class_Wide_Type (Expr_Typ) then
                     Expr_Typ := Root_Type (Expr_Typ);
                  end if;

                  --  Rename limited objects since they cannot be copied

                  if Is_Limited_Record (Expr_Typ) then
                     Rewrite_As_Renaming := True;
                  end if;

                  Obj_Id := Make_Temporary (Loc, 'D', Expr_Q);

                  --  Replace
                  --     IW : I'Class := Expr;
                  --  by
                  --     Dnn : Tag renames Tag_Ptr!(Expr'Address).all;
                  --     type Ityp is not null access I'Class;
                  --     Rnn : constant Ityp :=
                  --             Ityp!(Displace (Dnn'Address, I'Tag));
                  --     IW : I'Class renames Rnn.all;

                  if Rewrite_As_Renaming then
                     New_Expr :=
                       Make_Explicit_Dereference (Loc,
                         Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                           Make_Attribute_Reference (Loc,
                             Prefix => Relocate_Node (Expr_Q),
                             Attribute_Name => Name_Address)));

                     --  Suppress junk access checks on RE_Tag_Ptr

                     Insert_Action (N,
                       Make_Object_Renaming_Declaration (Loc,
                         Defining_Identifier => Obj_Id,
                         Subtype_Mark        =>
                           New_Occurrence_Of (RTE (RE_Tag), Loc),
                         Name                => New_Expr),
                       Suppress => Access_Check);

                     --  Dynamically reference the tag associated with the
                     --  interface.

                     Tag_Comp :=
                       Make_Function_Call (Loc,
                         Name => New_Occurrence_Of (RTE (RE_Displace), Loc),
                         Parameter_Associations => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix => New_Occurrence_Of (Obj_Id, Loc),
                             Attribute_Name => Name_Address),
                           New_Occurrence_Of
                             (Node (First_Elmt (Access_Disp_Table (Iface))),
                              Loc)));

                  --  Replace
                  --     IW : I'Class := Expr;
                  --  by
                  --     Dnn : Typ := Expr;
                  --     type Ityp is not null access I'Class;
                  --     Rnn : constant Ityp := Ityp (Dnn.I_Tag'Address);
                  --     IW  : I'Class renames Rnn.all;

                  elsif Has_Tag_Of_Type (Expr_Q)
                    and then Interface_Present_In_Ancestor (Expr_Typ, Typ)
                    and then (Expr_Typ = Etype (Expr_Typ)
                               or else not
                                 Is_Variable_Size_Record (Etype (Expr_Typ)))
                  then
                     Insert_Action (N,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Obj_Id,
                         Object_Definition   =>
                           New_Occurrence_Of (Expr_Typ, Loc),
                         Expression          => Relocate_Node (Expr_Q)));

                     --  Statically reference the tag associated with the
                     --  interface

                     Tag_Comp :=
                       Make_Selected_Component (Loc,
                         Prefix        => New_Occurrence_Of (Obj_Id, Loc),
                         Selector_Name =>
                           New_Occurrence_Of
                             (Find_Interface_Tag (Expr_Typ, Iface), Loc));

                  --  Replace
                  --     IW : I'Class := Expr;
                  --  by
                  --     type Equiv_Record is record ... end record;
                  --     implicit subtype CW is <Class_Wide_Subtype>;
                  --     Dnn : CW := CW!(Expr);
                  --     type Ityp is not null access I'Class;
                  --     Rnn : constant Ityp :=
                  --             Ityp!(Displace (Dnn'Address, I'Tag));
                  --     IW : I'Class renames Rnn.all;

                  else
                     --  Generate the equivalent record type and update the
                     --  subtype indication to reference it.

                     Expand_Subtype_From_Expr
                       (N             => N,
                        Unc_Type      => Typ,
                        Subtype_Indic => Obj_Def,
                        Exp           => Expr_Q);

                     --  For interface types we use 'Address which displaces
                     --  the pointer to the base of the object (if required).

                     if Is_Interface (Etype (Expr_Q)) then
                        New_Expr :=
                          Unchecked_Convert_To (Etype (Obj_Def),
                            Make_Explicit_Dereference (Loc,
                              Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                                Make_Attribute_Reference (Loc,
                                  Prefix => Relocate_Node (Expr_Q),
                                  Attribute_Name => Name_Address))));

                     --  For other types, no displacement is needed

                     else
                        New_Expr := Relocate_Node (Expr_Q);
                     end if;

                     --  Suppress junk access checks on RE_Tag_Ptr

                     Insert_Action (N,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Obj_Id,
                         Object_Definition   =>
                           New_Occurrence_Of (Etype (Obj_Def), Loc),
                         Expression          => New_Expr),
                       Suppress => Access_Check);

                     --  Dynamically reference the tag associated with the
                     --  interface.

                     Tag_Comp :=
                       Make_Function_Call (Loc,
                         Name => New_Occurrence_Of (RTE (RE_Displace), Loc),
                         Parameter_Associations => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix => New_Occurrence_Of (Obj_Id, Loc),
                             Attribute_Name => Name_Address),
                           New_Occurrence_Of
                             (Node (First_Elmt (Access_Disp_Table (Iface))),
                              Loc)));
                  end if;

                  --  As explained in Exp_Disp, we use Convert_Tag_To_Interface
                  --  to do the final conversion, but we insert an intermediate
                  --  temporary before the dereference so that we can process
                  --  the expansion as part of the analysis of the declaration
                  --  of this temporary, and then rewrite manually the original
                  --  object as the simple renaming of this dereference.

                  Tag_Comp := Convert_Tag_To_Interface (Typ, Tag_Comp);
                  pragma Assert (Nkind (Tag_Comp) = N_Explicit_Dereference
                    and then
                      Nkind (Prefix (Tag_Comp)) = N_Unchecked_Type_Conversion);

                  Ptr_Obj_Id := Make_Temporary (Loc, 'R');

                  Ptr_Obj_Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Ptr_Obj_Id,
                      Constant_Present    => True,
                      Object_Definition   =>
                        New_Occurrence_Of
                          (Entity (Subtype_Mark (Prefix (Tag_Comp))), Loc),
                      Expression => Prefix (Tag_Comp));

                  Insert_Action (N, Ptr_Obj_Decl, Suppress => All_Checks);

                  Set_Prefix (Tag_Comp, New_Occurrence_Of (Ptr_Obj_Id, Loc));
                  Expr_Q := Tag_Comp;
                  Set_Etype (Expr_Q, Typ);
                  Set_Parent (Expr_Q, N);

                  Rewrite_As_Renaming := True;
               end;

            else
               return;
            end if;

         --  Common case of explicit object initialization

         else
            --  Small optimization: if the expression is a function call and
            --  the object is stand-alone, not declared at library level and of
            --  a class-wide type, then we capture the result of the call into
            --  a temporary, with the benefit that, if the result's type does
            --  not need finalization, nothing will be finalized and, if it
            --  does, the temporary only will be finalized by means of a direct
            --  call to the Finalize primitive if the result's type is not a
            --  class-wide type; whereas, in both cases, the stand-alone object
            --  itself would be finalized by means of a dispatching call to the
            --  Deep_Finalize routine.

            if Nkind (Expr_Q) = N_Function_Call
              and then not Special_Ret_Obj
              and then not Is_Library_Level_Entity (Def_Id)
              and then Is_Class_Wide_Type (Typ)
            then
               Remove_Side_Effects (Expr_Q);
            end if;

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

                  --  Deal with possible range check

                  if Do_Range_Check (Expr) then

                     --  If assignment checks are suppressed, turn off flag

                     if Suppress_Assignment_Checks (N) then
                        Set_Do_Range_Check (Expr, False);

                     --  Otherwise generate the range check

                     else
                        Generate_Range_Check
                          (Expr, Typ, CE_Range_Check_Failed);
                     end if;
                  end if;
               end if;
            end if;

            --  For tagged types, when an init value is given, the tag has to
            --  be re-initialized separately in order to avoid the propagation
            --  of a wrong tag coming from a view conversion unless the type
            --  is class wide (in this case the tag comes from the init value).
            --  Suppress the tag assignment when not Tagged_Type_Expansion
            --  because tags are represented implicitly in objects. Ditto for
            --  types that are CPP_CLASS, and for initializations that are
            --  aggregates, because they have to have the right tag.

            --  The re-assignment of the tag has to be done even if the object
            --  is a constant. The assignment must be analyzed after the
            --  declaration. If an address clause follows, this is handled as
            --  part of the freeze actions for the object, otherwise insert
            --  tag assignment here.

            Tag_Assign := Make_Tag_Assignment (N);

            if Present (Tag_Assign) then
               if Present (Following_Address_Clause (N)) then
                  Ensure_Freeze_Node (Def_Id);
               elsif not Special_Ret_Obj then
                  Insert_Action_After (Init_After, Tag_Assign);
               end if;

            --  Handle C++ constructor calls. Note that we do not check that
            --  Typ is a tagged type since the equivalent Ada type of a C++
            --  class that has no virtual methods is an untagged limited
            --  record type.

            elsif Is_CPP_Constructor_Call (Expr) then
               declare
                  Id_Ref : constant Node_Id := New_Occurrence_Of (Def_Id, Loc);

               begin
                  --  The call to the initialization procedure does NOT freeze
                  --  the object being initialized.

                  Set_Must_Not_Freeze (Id_Ref);
                  Set_Assignment_OK (Id_Ref);

                  Insert_Actions_After (Init_After,
                    Build_Initialization_Call (N, Id_Ref, Typ,
                      Constructor_Ref => Expr));

                  --  We remove here the original call to the constructor
                  --  to avoid its management in the backend

                  Set_Expression (N, Empty);
                  return;
               end;

            --  Handle initialization of limited tagged types

            elsif Is_Tagged_Type (Typ)
              and then Is_Class_Wide_Type (Typ)
              and then Is_Limited_Record (Typ)
              and then not Is_Limited_Interface (Typ)
            then
               --  Given that the type is limited we cannot perform a copy. If
               --  Expr_Q is the reference to a variable we mark the variable
               --  as OK_To_Rename to expand this declaration into a renaming
               --  declaration (see below).

               if Is_Entity_Name (Expr_Q) then
                  Set_OK_To_Rename (Entity (Expr_Q));

               --  If we cannot convert the expression into a renaming we must
               --  consider it an internal error because the backend does not
               --  have support to handle it. But avoid crashing on a raise
               --  expression or conditional expression.

               elsif Nkind (Original_Node (Expr_Q)) not in
                 N_Raise_Expression | N_If_Expression | N_Case_Expression
               then
                  raise Program_Error;
               end if;

            --  For discrete types, set the Is_Known_Valid flag if the
            --  initializing value is known to be valid. Only do this for
            --  source assignments, since otherwise we can end up turning
            --  on the known valid flag prematurely from inserted code.

            elsif Comes_From_Source (N)
              and then Is_Discrete_Type (Typ)
              and then Expr_Known_Valid (Expr)
              and then Safe_To_Capture_Value (N, Def_Id)
            then
               Set_Is_Known_Valid (Def_Id);

            --  For access types, set the Is_Known_Non_Null flag if the
            --  initializing value is known to be non-null. We can also
            --  set Can_Never_Be_Null if this is a constant.

            elsif Is_Access_Type (Typ) and then Known_Non_Null (Expr) then
               Set_Is_Known_Non_Null (Def_Id, True);

               if Constant_Present (N) then
                  Set_Can_Never_Be_Null (Def_Id);
               end if;
            end if;

            --  If validity checking on copies, validate initial expression.
            --  But skip this if declaration is for a generic type, since it
            --  makes no sense to validate generic types. Not clear if this
            --  can happen for legal programs, but it definitely can arise
            --  from previous instantiation errors.

            if Validity_Checks_On
              and then Comes_From_Source (N)
              and then Validity_Check_Copies
              and then not Is_Generic_Type (Typ)
            then
               Ensure_Valid (Expr);

               if Safe_To_Capture_Value (N, Def_Id) then
                  Set_Is_Known_Valid (Def_Id);
               end if;
            end if;

            --  Now determine whether we will use a renaming

            Rewrite_As_Renaming :=

              --  The declaration cannot be rewritten if it has got constraints

              Is_Entity_Name (Original_Node (Obj_Def))

                --  If we have "X : S := ...;", and S is a constrained array
                --  subtype, then we cannot rename, because renamings ignore
                --  the constraints of S, so that would change the semantics
                --  (sliding would not occur on the initial value). This is
                --  only a problem for source objects though, the others have
                --  the correct bounds.

                and then not (Comes_From_Source (Obj_Def)
                               and then Is_Array_Type (Typ)
                               and then Is_Constrained (Typ))

                --  Moreover, if we have "X : aliased S := "...;" and S is an
                --  unconstrained array type, then we can rename only if the
                --  initialization expression has an unconstrained subtype too,
                --  because the bounds must be present within X.

                and then not (Is_Constr_Array_Subt_With_Bounds (Typ)
                               and then Is_Constrained (Etype (Expr_Q)))

                --  We may use a renaming if the initialization expression is a
                --  captured function call that meets a few conditions.

                and then
                  (Is_Renamable_Function_Call (Expr_Q)

                    --  Or else if it is a variable with OK_To_Rename set

                    or else (OK_To_Rename_Ref (Expr_Q)
                              and then not Special_Ret_Obj)

                    --  Or else if it is a slice of such a variable

                    or else (Nkind (Expr_Q) = N_Slice
                              and then OK_To_Rename_Ref (Prefix (Expr_Q))
                              and then not Special_Ret_Obj));

            --  If the type needs finalization and is not inherently limited,
            --  then the target is adjusted after the copy and attached to the
            --  finalization list. However, no adjustment is needed in the case
            --  where the object has been initialized by a call to a function
            --  returning on the primary stack (see Expand_Ctrl_Function_Call)
            --  since no copy occurred, given that the type is by-reference.
            --  Similarly, no adjustment is needed if we are going to rewrite
            --  the object declaration into a renaming declaration.

            if Needs_Finalization (Typ)
              and then not Is_Inherently_Limited_Type (Typ)
              and then Nkind (Expr_Q) /= N_Function_Call
              and then not Rewrite_As_Renaming
            then
               Adj_Call :=
                 Make_Adjust_Call (
                   Obj_Ref => New_Occurrence_Of (Def_Id, Loc),
                   Typ     => Base_Typ);

               if Present (Adj_Call) and then not Special_Ret_Obj then
                  Insert_Action_After (Init_After, Adj_Call);
               end if;
            end if;
         end if;

         --  Cases where the back end cannot handle the initialization
         --  directly. In such cases, we expand an assignment that will
         --  be appropriately handled by Expand_N_Assignment_Statement.

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
                         Name       => New_Occurrence_Of (Def_Id, Loc),
                         Expression => Relocate_Node (Expr));
            begin
               Set_Assignment_OK (Name (Stat));
               Set_No_Ctrl_Actions (Stat);
               Insert_Action_After (Init_After, Stat);
               Set_Expression (N, Empty);
               Set_No_Initialization (N);
            end;
         end if;
      end if;

      if Nkind (Obj_Def) = N_Access_Definition
        and then not Is_Local_Anonymous_Access (Typ)
      then
         --  An Ada 2012 stand-alone object of an anonymous access type

         declare
            Level : constant Entity_Id :=
                      Make_Defining_Identifier (Loc,
                        Chars =>
                          New_External_Name (Chars (Def_Id), Suffix => "L"));

            Level_Decl : Node_Id;
            Level_Expr : Node_Id;

         begin
            Mutate_Ekind (Level, Ekind (Def_Id));
            Set_Etype (Level, Standard_Natural);
            Set_Scope (Level, Scope (Def_Id));

            --  Set accessibility level of null

            if No (Expr) then
               Level_Expr :=
                 Make_Integer_Literal
                   (Loc, Scope_Depth (Standard_Standard));

            --  When the expression of the object is a function which returns
            --  an anonymous access type the master of the call is the object
            --  being initialized instead of the type.

            elsif Nkind (Expr) = N_Function_Call
              and then Ekind (Etype (Name (Expr))) = E_Anonymous_Access_Type
            then
               Level_Expr := Accessibility_Level
                               (Def_Id, Object_Decl_Level);

            --  General case

            else
               Level_Expr := Accessibility_Level (Expr, Dynamic_Level);
            end if;

            Level_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Level,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Natural, Loc),
                Expression          => Level_Expr,
                Constant_Present    => Constant_Present (N),
                Has_Init_Expression => True);

            Insert_Action_After (Init_After, Level_Decl);

            Set_Extra_Accessibility (Def_Id, Level);
         end;
      end if;

      --  If the object is default initialized and its type is subject to
      --  pragma Default_Initial_Condition, add a runtime check to verify
      --  the assumption of the pragma (SPARK RM 7.3.3). Generate:

      --    <Base_Typ>DIC (<Base_Typ> (Def_Id));

      --  Note that the check is generated for source objects only

      if Comes_From_Source (Def_Id)
        and then Has_DIC (Typ)
        and then Present (DIC_Procedure (Typ))
        and then not Has_Null_Body (DIC_Procedure (Typ))
        and then not Has_Init_Expression (N)
        and then No (Expr)
        and then not Is_Imported (Def_Id)
      then
         declare
            DIC_Call : constant Node_Id :=
                         Build_DIC_Call
                           (Loc, New_Occurrence_Of (Def_Id, Loc), Typ);
         begin
            if Present (Next_N) then
               Insert_Before_And_Analyze (Next_N, DIC_Call);

            --  The object declaration is the last node in a declarative or a
            --  statement list.

            else
               Append_To (List_Containing (N), DIC_Call);
               Analyze (DIC_Call);
            end if;
         end;
      end if;

      --  If this is the return object of a build-in-place function, locate the
      --  implicit BIPaccess parameter designating the caller-supplied return
      --  object and convert the declaration to a renaming of a dereference of
      --  this parameter. If the declaration includes an expression, add an
      --  assignment statement to ensure the return object gets initialized.

      --    Result : T [:= <expression>];

      --  is converted to

      --    Result : T renames BIPaccess.all;
      --    [Result := <expression>;]

      --  in the constrained case, or to

      --    type Txx is access all ...;
      --    Rxx : Txx := null;

      --    if BIPalloc = 1 then
      --       Rxx := BIPaccess;
      --       Rxx.all := <expression>;
      --    elsif BIPalloc = 2 then
      --       Rxx := new <expression-type>'(<expression>)[storage_pool =
      --         system__secondary_stack__ss_pool][procedure_to_call =
      --         system__secondary_stack__ss_allocate];
      --    elsif BIPalloc = 3 then
      --       Rxx := new <expression-type>'(<expression>)
      --    elsif BIPalloc = 4 then
      --       Pxx : system__storage_pools__root_storage_pool renames
      --         BIPstoragepool.all;
      --       Rxx := new <expression-type>'(<expression>)[storage_pool =
      --         Pxx][procedure_to_call =
      --         system__storage_pools__allocate_any];
      --    else
      --       [program_error "build in place mismatch"]
      --    end if;

      --    Result : T renames Rxx.all;

      --  in the unconstrained case.

      if Is_Build_In_Place_Return_Object (Def_Id) then
         declare
            Init_Stmt      : Node_Id;
            Obj_Acc_Formal : Entity_Id;

         begin
            --  Retrieve the implicit access parameter passed by the caller

            Obj_Acc_Formal :=
              Build_In_Place_Formal (Func_Id, BIP_Object_Access);

            --  If the return object's declaration includes an expression
            --  and the declaration isn't marked as No_Initialization, then
            --  we need to generate an assignment to the object and insert
            --  it after the declaration before rewriting it as a renaming
            --  (otherwise we'll lose the initialization). The case where
            --  the result type is an interface (or class-wide interface)
            --  is also excluded because the context of the function call
            --  must be unconstrained, so the initialization will always
            --  be done as part of an allocator evaluation (storage pool
            --  or secondary stack), never to a constrained target object
            --  passed in by the caller. Besides the assignment being
            --  unneeded in this case, it avoids problems with trying to
            --  generate a dispatching assignment when the return expression
            --  is a nonlimited descendant of a limited interface (the
            --  interface has no assignment operation).

            if Present (Expr_Q)
              and then not Is_Delayed_Aggregate (Expr_Q)
              and then not No_Initialization (N)
              and then not Is_Interface (Typ)
            then
               if Is_Class_Wide_Type (Typ)
                 and then not Is_Class_Wide_Type (Etype (Expr_Q))
               then
                  Init_Stmt :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Def_Id, Loc),
                      Expression =>
                        Make_Type_Conversion (Loc,
                          Subtype_Mark =>
                            New_Occurrence_Of (Typ, Loc),
                          Expression   => New_Copy_Tree (Expr_Q)));

               else
                  Init_Stmt :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Def_Id, Loc),
                      Expression => New_Copy_Tree (Expr_Q));
               end if;

               Set_Assignment_OK (Name (Init_Stmt));
               Set_No_Ctrl_Actions (Init_Stmt);

            else
               Init_Stmt := Empty;
            end if;

            --  When the function's subtype is unconstrained, a run-time
            --  test may be needed to decide the form of allocation to use
            --  for the return object. The function has an implicit formal
            --  parameter indicating this. If the BIP_Alloc_Form formal has
            --  the value one, then the caller has passed access to an
            --  existing object for use as the return object. If the value
            --  is two, then the return object must be allocated on the
            --  secondary stack. If the value is three, then the return
            --  object must be allocated on the heap. Otherwise, the object
            --  must be allocated in a storage pool. We generate an if
            --  statement to test the BIP_Alloc_Form formal and initialize
            --  a local access value appropriately.

            if Needs_BIP_Alloc_Form (Func_Id) then
               declare
                  Desig_Typ : constant Entity_Id :=
                    (if Ekind (Typ) = E_Array_Subtype
                     then Etype (Func_Id) else Typ);
                  --  Ensure that the we use a fat pointer when allocating
                  --  an unconstrained array on the heap. In this case the
                  --  result object's type is a constrained array type even
                  --  though the function's type is unconstrained.

                  Obj_Alloc_Formal : constant Entity_Id :=
                    Build_In_Place_Formal (Func_Id, BIP_Alloc_Form);
                  Pool_Id          : constant Entity_Id :=
                    Make_Temporary (Loc, 'P');

                  Acc_Typ        : Entity_Id;
                  Alloc_Obj_Decl : Node_Id;
                  Alloc_Obj_Id   : Entity_Id;
                  Alloc_Stmt     : Node_Id;
                  Guard_Except   : Node_Id;
                  Heap_Allocator : Node_Id;
                  Pool_Allocator : Node_Id;
                  Pool_Decl      : Node_Id;
                  Ptr_Typ_Decl   : Node_Id;
                  SS_Allocator   : Node_Id;

               begin
                  --  Create an access type designating the function's
                  --  result subtype.

                  Acc_Typ := Make_Temporary (Loc, 'A');

                  Ptr_Typ_Decl :=
                    Make_Full_Type_Declaration (Loc,
                      Defining_Identifier => Acc_Typ,
                      Type_Definition     =>
                        Make_Access_To_Object_Definition (Loc,
                          All_Present        => True,
                          Subtype_Indication =>
                            New_Occurrence_Of (Desig_Typ, Loc)));

                  Insert_Action (N, Ptr_Typ_Decl, Suppress => All_Checks);

                  --  Create an access object that will be initialized to an
                  --  access value denoting the return object, either coming
                  --  from an implicit access value passed in by the caller
                  --  or from the result of an allocator.

                  Alloc_Obj_Id := Make_Temporary (Loc, 'R');

                  Alloc_Obj_Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Alloc_Obj_Id,
                      Object_Definition   =>
                        New_Occurrence_Of (Acc_Typ, Loc));

                  Insert_Action (N, Alloc_Obj_Decl, Suppress => All_Checks);

                  --  First create the Heap_Allocator

                  Heap_Allocator := Make_Allocator_For_Return (Expr_Q);

                  --  The Pool_Allocator is just like the Heap_Allocator,
                  --  except we set Storage_Pool and Procedure_To_Call so
                  --  it will use the user-defined storage pool.

                  Pool_Allocator := Make_Allocator_For_Return (Expr_Q);

                  --  Do not generate the renaming of the build-in-place
                  --  pool parameter on ZFP because the parameter is not
                  --  created in the first place.

                  if RTE_Available (RE_Root_Storage_Pool_Ptr) then
                     Pool_Decl :=
                       Make_Object_Renaming_Declaration (Loc,
                         Defining_Identifier => Pool_Id,
                         Subtype_Mark        =>
                           New_Occurrence_Of
                             (RTE (RE_Root_Storage_Pool), Loc),
                         Name                =>
                           Make_Explicit_Dereference (Loc,
                             New_Occurrence_Of
                               (Build_In_Place_Formal
                                  (Func_Id, BIP_Storage_Pool), Loc)));
                     Set_Storage_Pool (Pool_Allocator, Pool_Id);
                     Set_Procedure_To_Call
                       (Pool_Allocator, RTE (RE_Allocate_Any));
                  else
                     Pool_Decl := Make_Null_Statement (Loc);
                  end if;

                  --  If the No_Allocators restriction is active, then only
                  --  an allocator for secondary stack allocation is needed.
                  --  It's OK for such allocators to have Comes_From_Source
                  --  set to False, because gigi knows not to flag them as
                  --  being a violation of No_Implicit_Heap_Allocations.

                  if Restriction_Active (No_Allocators) then
                     SS_Allocator   := Heap_Allocator;
                     Heap_Allocator := Make_Null (Loc);
                     Pool_Allocator := Make_Null (Loc);

                  --  Otherwise the heap and pool allocators may be needed,
                  --  so we make another allocator for secondary stack
                  --  allocation.

                  else
                     SS_Allocator := Make_Allocator_For_Return (Expr_Q);

                     --  The heap and pool allocators are marked as
                     --  Comes_From_Source since they correspond to an
                     --  explicit user-written allocator (that is, it will
                     --  only be executed on behalf of callers that call the
                     --  function as initialization for such an allocator).
                     --  Prevents errors when No_Implicit_Heap_Allocations
                     --  is in force.

                     Set_Comes_From_Source (Heap_Allocator, True);
                     Set_Comes_From_Source (Pool_Allocator, True);
                  end if;

                  --  The allocator is returned on the secondary stack

                  Check_Restriction (No_Secondary_Stack, N);
                  Set_Storage_Pool (SS_Allocator, RTE (RE_SS_Pool));
                  Set_Procedure_To_Call
                    (SS_Allocator, RTE (RE_SS_Allocate));

                  --  The allocator is returned on the secondary stack,
                  --  so indicate that the function return, as well as
                  --  all blocks that encloses the allocator, must not
                  --  release it. The flags must be set now because
                  --  the decision to use the secondary stack is done
                  --  very late in the course of expanding the return
                  --  statement, past the point where these flags are
                  --  normally set.

                  Set_Uses_Sec_Stack (Func_Id);
                  Set_Uses_Sec_Stack (Scope (Def_Id));
                  Set_Sec_Stack_Needed_For_Return (Scope (Def_Id));

                  --  Guard against poor expansion on the caller side by
                  --  using a raise statement to catch out-of-range values
                  --  of formal parameter BIP_Alloc_Form.

                  if Exceptions_OK then
                     Guard_Except :=
                       Make_Raise_Program_Error (Loc,
                         Reason => PE_Build_In_Place_Mismatch);
                  else
                     Guard_Except := Make_Null_Statement (Loc);
                  end if;

                  --  Create an if statement to test the BIP_Alloc_Form
                  --  formal and initialize the access object to either the
                  --  BIP_Object_Access formal (BIP_Alloc_Form =
                  --  Caller_Allocation), the result of allocating the
                  --  object in the secondary stack (BIP_Alloc_Form =
                  --  Secondary_Stack), or else an allocator to create the
                  --  return object in the heap or user-defined pool
                  --  (BIP_Alloc_Form = Global_Heap or User_Storage_Pool).

                  --  ??? An unchecked type conversion must be made in the
                  --  case of assigning the access object formal to the
                  --  local access object, because a normal conversion would
                  --  be illegal in some cases (such as converting access-
                  --  to-unconstrained to access-to-constrained), but the
                  --  the unchecked conversion will presumably fail to work
                  --  right in just such cases. It's not clear at all how to
                  --  handle this.

                  Alloc_Stmt :=
                    Make_If_Statement (Loc,
                      Condition =>
                        Make_Op_Eq (Loc,
                          Left_Opnd  =>
                            New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                          Right_Opnd =>
                            Make_Integer_Literal (Loc,
                              UI_From_Int (BIP_Allocation_Form'Pos
                                             (Caller_Allocation)))),

                      Then_Statements => New_List (
                        Make_Assignment_Statement (Loc,
                          Name       =>
                            New_Occurrence_Of (Alloc_Obj_Id, Loc),
                          Expression =>
                            Unchecked_Convert_To
                              (Acc_Typ,
                               New_Occurrence_Of (Obj_Acc_Formal, Loc)))),

                      Elsif_Parts => New_List (
                        Make_Elsif_Part (Loc,
                          Condition =>
                            Make_Op_Eq (Loc,
                              Left_Opnd  =>
                                New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                              Right_Opnd =>
                                Make_Integer_Literal (Loc,
                                  UI_From_Int (BIP_Allocation_Form'Pos
                                                 (Secondary_Stack)))),

                          Then_Statements => New_List (
                            Make_Assignment_Statement (Loc,
                              Name       =>
                                New_Occurrence_Of (Alloc_Obj_Id, Loc),
                              Expression => SS_Allocator))),

                        Make_Elsif_Part (Loc,
                          Condition =>
                            Make_Op_Eq (Loc,
                              Left_Opnd  =>
                                New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                              Right_Opnd =>
                                Make_Integer_Literal (Loc,
                                  UI_From_Int (BIP_Allocation_Form'Pos
                                                 (Global_Heap)))),

                          Then_Statements => New_List (
                            Build_Heap_Or_Pool_Allocator
                              (Temp_Id    => Alloc_Obj_Id,
                               Temp_Typ   => Acc_Typ,
                               Ret_Typ    => Desig_Typ,
                               Alloc_Expr => Heap_Allocator))),

                        --  ??? If all is well, we can put the following
                        --  'elsif' in the 'else', but this is a useful
                        --  self-check in case caller and callee don't agree
                        --  on whether BIPAlloc and so on should be passed.

                        Make_Elsif_Part (Loc,
                          Condition =>
                            Make_Op_Eq (Loc,
                              Left_Opnd  =>
                                New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                              Right_Opnd =>
                                Make_Integer_Literal (Loc,
                                  UI_From_Int (BIP_Allocation_Form'Pos
                                                 (User_Storage_Pool)))),

                          Then_Statements => New_List (
                            Pool_Decl,
                            Build_Heap_Or_Pool_Allocator
                              (Temp_Id    => Alloc_Obj_Id,
                               Temp_Typ   => Acc_Typ,
                               Ret_Typ    => Desig_Typ,
                               Alloc_Expr => Pool_Allocator)))),

                      --  Raise Program_Error if it's none of the above;
                      --  this is a compiler bug.

                      Else_Statements => New_List (Guard_Except));

                     --  If a separate initialization assignment was created
                     --  earlier, append that following the assignment of the
                     --  implicit access formal to the access object, to ensure
                     --  that the return object is initialized in that case. In
                     --  this situation, the target of the assignment must be
                     --  rewritten to denote a dereference of the access to the
                     --  return object passed in by the caller.

                     if Present (Init_Stmt) then
                        Set_Name (Init_Stmt,
                          Make_Explicit_Dereference (Loc,
                            Prefix => New_Occurrence_Of (Alloc_Obj_Id, Loc)));
                        Set_Assignment_OK (Name (Init_Stmt));

                        Append_To (Then_Statements (Alloc_Stmt), Init_Stmt);
                        Init_Stmt := Empty;
                     end if;

                  Insert_Action (N, Alloc_Stmt, Suppress => All_Checks);

                  --  From now on, the type of the return object is the
                  --  designated type.

                  if Desig_Typ /= Typ then
                     Set_Etype (Def_Id, Desig_Typ);
                     Set_Actual_Subtype (Def_Id, Typ);
                  end if;

                  --  Remember the local access object for use in the
                  --  dereference of the renaming created below.

                  Obj_Acc_Formal := Alloc_Obj_Id;
               end;

            --  When the function's type is unconstrained and a run-time test
            --  is not needed, we nevertheless need to build the return using
            --  the return object's type.

            elsif not Is_Constrained (Underlying_Type (Etype (Func_Id))) then
               declare
                  Acc_Typ        : Entity_Id;
                  Alloc_Obj_Decl : Node_Id;
                  Alloc_Obj_Id   : Entity_Id;
                  Ptr_Typ_Decl   : Node_Id;

               begin
                  --  Create an access type designating the function's
                  --  result subtype.

                  Acc_Typ := Make_Temporary (Loc, 'A');

                  Ptr_Typ_Decl :=
                    Make_Full_Type_Declaration (Loc,
                      Defining_Identifier => Acc_Typ,
                      Type_Definition     =>
                        Make_Access_To_Object_Definition (Loc,
                          All_Present        => True,
                          Subtype_Indication =>
                            New_Occurrence_Of (Typ, Loc)));

                  Insert_Action (N, Ptr_Typ_Decl, Suppress => All_Checks);

                  --  Create an access object initialized to the conversion
                  --  of the implicit access value passed in by the caller.

                  Alloc_Obj_Id := Make_Temporary (Loc, 'R');

                  --  See the ??? comment a few lines above about the use of
                  --  an unchecked conversion here.

                  Alloc_Obj_Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Alloc_Obj_Id,
                      Constant_Present    => True,
                      Object_Definition   =>
                        New_Occurrence_Of (Acc_Typ, Loc),
                      Expression =>
                        Unchecked_Convert_To
                          (Acc_Typ, New_Occurrence_Of (Obj_Acc_Formal, Loc)));

                  Insert_Action (N, Alloc_Obj_Decl, Suppress => All_Checks);

                  --  Remember the local access object for use in the
                  --  dereference of the renaming created below.

                  Obj_Acc_Formal := Alloc_Obj_Id;
               end;
            end if;

            --  Initialize the object now that it has got its final subtype,
            --  but before rewriting it as a renaming.

            Initialize_Return_Object
              (Tag_Assign, Adj_Call, Expr_Q, Init_Stmt, Init_After);

            --  Save the assignment statement when returning a controlled
            --  object. This reference is used later by the finalization
            --  machinery to mark the object as successfully initialized.

            if Present (Init_Stmt) and then Needs_Finalization (Typ) then
               Set_Last_Aggregate_Assignment (Def_Id, Init_Stmt);
            end if;

            --  Replace the return object declaration with a renaming of a
            --  dereference of the access value designating the return object.

            Expr_Q :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Obj_Acc_Formal, Loc));
            Set_Etype (Expr_Q, Etype (Def_Id));

            Rewrite_As_Renaming := True;
         end;

      --  If we can rename the initialization expression, we need to make sure
      --  that we use the proper type in the case of a return object that lives
      --  on the secondary stack (see other cases below for a similar handling)
      --  and that the tag is assigned in the case of any return object.

      elsif Rewrite_As_Renaming then
         if Special_Ret_Obj then
            declare
               Desig_Typ : constant Entity_Id :=
                 (if Ekind (Typ) = E_Array_Subtype
                  then Etype (Func_Id) else Typ);

            begin
               --  From now on, the type of the return object is the
               --  designated type.

               if Desig_Typ /= Typ then
                  Set_Etype (Def_Id, Desig_Typ);
                  Set_Actual_Subtype (Def_Id, Typ);
               end if;

               if Present (Tag_Assign) then
                  Insert_Action_After (Init_After, Tag_Assign);
               end if;

               --  Ada 2005 (AI95-344): If the result type is class-wide,
               --  insert a check that the level of the return expression's
               --  underlying type is not deeper than the level of the master
               --  enclosing the function.

               --  AI12-043: The check is made immediately after the return
               --  object is created.

               if Is_Class_Wide_Type (Etype (Func_Id)) then
                  Apply_CW_Accessibility_Check (Expr_Q, Func_Id);
               end if;
            end;
         end if;

      --  If this is the return object of a function returning on the secondary
      --  stack, convert the declaration to a renaming of the dereference of ah
      --  allocator for the secondary stack.

      --    Result : T [:= <expression>];

      --  is converted to

      --    type Txx is access all ...;
      --    Rxx : constant Txx :=
      --      new <expression-type>['(<expression>)][storage_pool =
      --        system__secondary_stack__ss_pool][procedure_to_call =
      --        system__secondary_stack__ss_allocate];

      --    Result : T renames Rxx.all;

      elsif Is_Secondary_Stack_Return_Object (Def_Id) then
         declare
            Desig_Typ : constant Entity_Id :=
              (if Ekind (Typ) = E_Array_Subtype
               then Etype (Func_Id) else Typ);
            --  Ensure that the we use a fat pointer when allocating
            --  an unconstrained array on the heap. In this case the
            --  result object's type is a constrained array type even
            --  though the function's type is unconstrained.

            Acc_Typ        : Entity_Id;
            Alloc_Obj_Decl : Node_Id;
            Alloc_Obj_Id   : Entity_Id;
            Ptr_Type_Decl  : Node_Id;

         begin
            --  Create an access type designating the function's
            --  result subtype.

            Acc_Typ := Make_Temporary (Loc, 'A');

            Ptr_Type_Decl :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Acc_Typ,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present        => True,
                    Subtype_Indication =>
                      New_Occurrence_Of (Desig_Typ, Loc)));

            Insert_Action (N, Ptr_Type_Decl, Suppress => All_Checks);

            Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));

            Alloc_Obj_Id := Make_Temporary (Loc, 'R');

            Alloc_Obj_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Alloc_Obj_Id,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (Acc_Typ, Loc),
                Expression => Make_Allocator_For_Return (Expr_Q));

            Insert_Action (N, Alloc_Obj_Decl, Suppress => All_Checks);

            Set_Uses_Sec_Stack (Func_Id);
            Set_Uses_Sec_Stack (Scope (Def_Id));
            Set_Sec_Stack_Needed_For_Return (Scope (Def_Id));

            --  From now on, the type of the return object is the
            --  designated type.

            if Desig_Typ /= Typ then
               Set_Etype (Def_Id, Desig_Typ);
               Set_Actual_Subtype (Def_Id, Typ);
            end if;

            --  Initialize the object now that it has got its final subtype,
            --  but before rewriting it as a renaming.

            Initialize_Return_Object
              (Tag_Assign, Adj_Call, Expr_Q, Empty, Init_After);

            --  Replace the return object declaration with a renaming of a
            --  dereference of the access value designating the return object.

            Expr_Q :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Alloc_Obj_Id, Loc));
            Set_Etype (Expr_Q, Etype (Def_Id));

            Rewrite_As_Renaming := True;
         end;

      --  If this is the return object of a function returning a by-reference
      --  type, convert the declaration to a renaming of the dereference of ah
      --  allocator for the return stack.

      --    Result : T [:= <expression>];

      --  is converted to

      --    type Txx is access all ...;
      --    Rxx : constant Txx :=
      --      new <expression-type>['(<expression>)][storage_pool =
      --        system__return_stack__rs_pool][procedure_to_call =
      --        system__return_stack__rs_allocate];

      --    Result : T renames Rxx.all;

      elsif Back_End_Return_Slot
        and then Is_By_Reference_Return_Object (Def_Id)
      then
         declare
            Acc_Typ        : Entity_Id;
            Alloc_Obj_Decl : Node_Id;
            Alloc_Obj_Id   : Entity_Id;
            Ptr_Type_Decl  : Node_Id;

         begin
            --  Create an access type designating the function's
            --  result subtype.

            Acc_Typ := Make_Temporary (Loc, 'A');

            Ptr_Type_Decl :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Acc_Typ,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present        => True,
                    Subtype_Indication =>
                      New_Occurrence_Of (Typ, Loc)));

            Insert_Action (N, Ptr_Type_Decl, Suppress => All_Checks);

            Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_RS_Pool));

            Alloc_Obj_Id := Make_Temporary (Loc, 'R');

            Alloc_Obj_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Alloc_Obj_Id,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (Acc_Typ, Loc),
                Expression => Make_Allocator_For_Return (Expr_Q));

            Insert_Action (N, Alloc_Obj_Decl, Suppress => All_Checks);

            --  Initialize the object now that it has got its final subtype,
            --  but before rewriting it as a renaming.

            Initialize_Return_Object
              (Tag_Assign, Adj_Call, Expr_Q, Empty, Init_After);

            --  Replace the return object declaration with a renaming of a
            --  dereference of the access value designating the return object.

            Expr_Q :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Alloc_Obj_Id, Loc));
            Set_Etype (Expr_Q, Etype (Def_Id));

            Rewrite_As_Renaming := True;
         end;
      end if;

      --  Final transformation - turn the object declaration into a renaming
      --  if appropriate. If this is the completion of a deferred constant
      --  declaration, then this transformation generates what would be
      --  illegal code if written by hand, but that's OK.

      if Rewrite_As_Renaming then
         Rewrite (N,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Subtype_Mark        => New_Occurrence_Of (Etype (Def_Id), Loc),
             Name                => Expr_Q));

         --  Keep original aspects

         Move_Aspects (Original_Node (N), N);

         --  We do not analyze this renaming declaration, because all its
         --  components have already been analyzed, and if we were to go
         --  ahead and analyze it, we would in effect be trying to generate
         --  another declaration of X, which won't do.

         Set_Renamed_Object (Def_Id, Expr_Q);
         Set_Analyzed (N);

         --  We do need to deal with debug issues for this renaming

         --  First, if entity comes from source, then mark it as needing
         --  debug information, even though it is defined by a generated
         --  renaming that does not come from source.

         Set_Debug_Info_Defining_Id (N);

         --  Now call the routine to generate debug info for the renaming

         Insert_Action (N, Debug_Renaming_Declaration (N));
      end if;

   --  Exception on library entity not available

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Object_Declaration;

   ---------------------------------
   -- Expand_N_Subtype_Indication --
   ---------------------------------

   --  Add a check on the range of the subtype and deal with validity checking

   procedure Expand_N_Subtype_Indication (N : Node_Id) is
      Ran : constant Node_Id   := Range_Expression (Constraint (N));
      Typ : constant Entity_Id := Entity (Subtype_Mark (N));

   begin
      if Nkind (Constraint (N)) = N_Range_Constraint then
         Validity_Check_Range (Range_Expression (Constraint (N)));
      end if;

      --  Do not duplicate the work of Process_Range_Expr_In_Decl in Sem_Ch3

      if Nkind (Parent (N)) in N_Constrained_Array_Definition | N_Slice
        and then Nkind (Parent (Parent (N))) not in
                   N_Full_Type_Declaration | N_Object_Declaration
      then
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
      --  we must check to see whether expansion is active before proceeding,
      --  because this affects the visibility of selected components in bodies
      --  of instances. Within a generic we still need to set Parent_Subtype
      --  link because the visibility of inherited components will have to be
      --  verified in subsequent instances.

      if not Expander_Active then
         if Inside_A_Generic and then Ekind (T) = E_Record_Type then
            Set_Parent_Subtype (T, Etype (T));
         end if;
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
               Subtype_Mark => New_Occurrence_Of (Entity (Indic), Loc),
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
              Subtype_Indication => New_Occurrence_Of (Par_Subtype, Loc)));

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
              Subtype_Indication => New_Occurrence_Of (RTE (RE_Tag), Sloc_N)));

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

      if not Comes_From_Source (Typ) or else Is_Tagged_Type (Typ) then
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

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   function Freeze_Type (N : Node_Id) return Boolean is
      procedure Process_RACW_Types (Typ : Entity_Id);
      --  Validate and generate stubs for all RACW types associated with type
      --  Typ.

      ------------------------
      -- Process_RACW_Types --
      ------------------------

      procedure Process_RACW_Types (Typ : Entity_Id) is
         List : constant Elist_Id := Access_Types_To_Process (N);
         E    : Elmt_Id;
         Seen : Boolean := False;

      begin
         if Present (List) then
            E := First_Elmt (List);
            while Present (E) loop
               if Is_Remote_Access_To_Class_Wide_Type (Node (E)) then
                  Validate_RACW_Primitives (Node (E));
                  Seen := True;
               end if;

               Next_Elmt (E);
            end loop;
         end if;

         --  If there are RACWs designating this type, make stubs now

         if Seen then
            Remote_Types_Tagged_Full_View_Encountered (Typ);
         end if;
      end Process_RACW_Types;

      --  Local variables

      Def_Id : constant Entity_Id := Entity (N);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      Result : Boolean := False;

   --  Start of processing for Freeze_Type

   begin
      --  The type being frozen may be subject to pragma Ghost. Set the mode
      --  now to ensure that any nodes generated during freezing are properly
      --  marked as Ghost.

      Set_Ghost_Mode (Def_Id);

      --  Process any remote access-to-class-wide types designating the type
      --  being frozen.

      Process_RACW_Types (Def_Id);

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

      elsif Ekind (Def_Id) in E_Access_Type | E_General_Access_Type then
         declare
            Loc        : constant Source_Ptr := Sloc (N);
            Desig_Type : constant Entity_Id  := Designated_Type (Def_Id);

            Freeze_Action_Typ : Entity_Id;
            Pool_Object       : Entity_Id;

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
                  DT_Align : Node_Id;
                  DT_Size  : Node_Id;

               begin
                  --  For unconstrained composite types we give a size of zero
                  --  so that the pool knows that it needs a special algorithm
                  --  for variable size object allocation.

                  if Is_Composite_Type (Desig_Type)
                    and then not Is_Constrained (Desig_Type)
                  then
                     DT_Size  := Make_Integer_Literal (Loc, 0);
                     DT_Align := Make_Integer_Literal (Loc, Maximum_Alignment);

                  else
                     DT_Size :=
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Desig_Type, Loc),
                         Attribute_Name => Name_Max_Size_In_Storage_Elements);

                     DT_Align :=
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Desig_Type, Loc),
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
                      Object_Definition   =>
                        Make_Subtype_Indication (Loc,
                          Subtype_Mark =>
                            New_Occurrence_Of
                              (RTE (RE_Stack_Bounded_Pool), Loc),

                          Constraint   =>
                            Make_Index_Or_Discriminant_Constraint (Loc,
                              Constraints => New_List (

                                --  First discriminant is the Pool Size

                                New_Occurrence_Of (
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
              and then RTU_Loaded (System_Storage_Pools_Subpools)
            then
               declare
                  Loc   : constant Source_Ptr := Sloc (Def_Id);
                  Pool  : constant Entity_Id :=
                            Associated_Storage_Pool (Def_Id);

               begin
                  --  It is known that the accessibility level of the access
                  --  type is deeper than that of the pool.

                  if Type_Access_Level (Def_Id)
                       > Static_Accessibility_Level (Pool, Object_Decl_Level)
                    and then Is_Class_Wide_Type (Etype (Pool))
                    and then not Accessibility_Checks_Suppressed (Def_Id)
                    and then not Accessibility_Checks_Suppressed (Pool)
                  then
                     --  When the pool is of a class-wide type, it may or may
                     --  not support subpools depending on the path of
                     --  derivation. Generate:

                     --    if Def_Id in RSPWS'Class then
                     --       raise Program_Error;
                     --    end if;

                     Append_Freeze_Action (Def_Id,
                       Make_If_Statement (Loc,
                         Condition       =>
                           Make_In (Loc,
                             Left_Opnd  => New_Occurrence_Of (Pool, Loc),
                             Right_Opnd =>
                               New_Occurrence_Of
                                 (Class_Wide_Type
                                   (RTE
                                     (RE_Root_Storage_Pool_With_Subpools)),
                                  Loc)),
                         Then_Statements => New_List (
                           Make_Raise_Program_Error (Loc,
                             Reason => PE_Accessibility_Check_Failed))));
                  end if;
               end;
            end if;

            --  For access-to-controlled types (including class-wide types and
            --  Taft-amendment types, which potentially have controlled
            --  components), expand the list controller object that will store
            --  the dynamically allocated objects. Don't do this transformation
            --  for expander-generated access types, except do it for types
            --  that are the full view of types derived from other private
            --  types and for access types used to implement indirect temps.
            --  Also suppress the list controller in the case of a designated
            --  type with convention Java, since this is used when binding to
            --  Java API specs, where there's no equivalent of a finalization
            --  list and we don't want to pull in the finalization support if
            --  not needed.

            if not Comes_From_Source (Def_Id)
              and then not Has_Private_Declaration (Def_Id)
              and then not Old_Attr_Util.Indirect_Temps
                             .Is_Access_Type_For_Indirect_Temp (Def_Id)
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

            --  Create a finalization collection for an access-to-controlled
            --  type or an access-to-incomplete type. It is assumed that the
            --  full view will be controlled.

            elsif Needs_Finalization (Desig_Type)
              or else (Is_Incomplete_Type (Desig_Type)
                        and then No (Full_View (Desig_Type)))
            then
               Build_Finalization_Collection (Def_Id);

            --  Also create a finalization collection when the designated type
            --  contains a private component. It is assumed that the full view
            --  will be controlled.

            elsif Has_Private_Component (Desig_Type) then
               Build_Finalization_Collection
                 (Typ            => Def_Id,
                  For_Private    => True,
                  Context_Scope  => Scope (Def_Id),
                  Insertion_Node => Declaration_Node (Desig_Type));
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

      --  Generate the [spec and] body of the invariant procedure tasked with
      --  the runtime verification of all invariants that pertain to the type.
      --  This includes invariants on the partial and full view, inherited
      --  class-wide invariants from parent types or interfaces, and invariants
      --  on array elements or record components. But skip internal types.

      if Is_Itype (Def_Id) then
         null;

      elsif Is_Interface (Def_Id) then

         --  Interfaces are treated as the partial view of a private type in
         --  order to achieve uniformity with the general case. As a result, an
         --  interface receives only a "partial" invariant procedure which is
         --  never called.

         if Has_Own_Invariants (Def_Id) then
            Build_Invariant_Procedure_Body
              (Typ               => Def_Id,
               Partial_Invariant => Is_Interface (Def_Id));
         end if;

      --  Non-interface types

      --  Do not generate invariant procedure within other assertion
      --  subprograms, which may involve local declarations of local
      --  subtypes to which these checks do not apply.

      else
         if Has_Invariants (Def_Id) then
            if not Predicate_Check_In_Scope (Def_Id)
              or else (Ekind (Current_Scope) = E_Function
                        and then Is_Predicate_Function (Current_Scope))
            then
               null;
            else
               Build_Invariant_Procedure_Body (Def_Id);
            end if;
         end if;

         --  Generate the [spec and] body of the procedure tasked with the
         --  run-time verification of pragma Default_Initial_Condition's
         --  expression.

         if Has_DIC (Def_Id) then
            Build_DIC_Procedure_Body (Def_Id);
         end if;
      end if;

      Restore_Ghost_Region (Saved_GM, Saved_IGR);

      return Result;

   exception
      when RE_Not_Available =>
         Restore_Ghost_Region (Saved_GM, Saved_IGR);

         return False;
   end Freeze_Type;

   -------------------------
   -- Get_Simple_Init_Val --
   -------------------------

   function Get_Simple_Init_Val
     (Typ  : Entity_Id;
      N    : Node_Id;
      Size : Uint := No_Uint) return Node_Id
   is
      IV_Attribute : constant Boolean :=
                       Nkind (N) = N_Attribute_Reference
                         and then Attribute_Name (N) = Name_Invalid_Value;

      Loc : constant Source_Ptr := Sloc (N);

      procedure Extract_Subtype_Bounds
        (Lo_Bound : out Uint;
         Hi_Bound : out Uint);
      --  Inspect subtype Typ as well its ancestor subtypes and derived types
      --  to determine the best known information about the bounds of the type.
      --  The output parameters are set as follows:
      --
      --    * Lo_Bound - Set to No_Unit when there is no information available,
      --      or to the known low bound.
      --
      --    * Hi_Bound - Set to No_Unit when there is no information available,
      --      or to the known high bound.

      function Simple_Init_Array_Type return Node_Id;
      --  Build an expression to initialize array type Typ

      function Simple_Init_Defaulted_Type return Node_Id;
      --  Build an expression to initialize type Typ which is subject to
      --  aspect Default_Value.

      function Simple_Init_Initialize_Scalars_Type
        (Size_To_Use : Uint) return Node_Id;
      --  Build an expression to initialize scalar type Typ which is subject to
      --  pragma Initialize_Scalars. Size_To_Use is the size of the object.

      function Simple_Init_Normalize_Scalars_Type
        (Size_To_Use : Uint) return Node_Id;
      --  Build an expression to initialize scalar type Typ which is subject to
      --  pragma Normalize_Scalars. Size_To_Use is the size of the object.

      function Simple_Init_Private_Type return Node_Id;
      --  Build an expression to initialize private type Typ

      function Simple_Init_Scalar_Type return Node_Id;
      --  Build an expression to initialize scalar type Typ

      ----------------------------
      -- Extract_Subtype_Bounds --
      ----------------------------

      procedure Extract_Subtype_Bounds
        (Lo_Bound : out Uint;
         Hi_Bound : out Uint)
      is
         ST1    : Entity_Id;
         ST2    : Entity_Id;
         Lo     : Node_Id;
         Hi     : Node_Id;
         Lo_Val : Uint;
         Hi_Val : Uint;

      begin
         Lo_Bound := No_Uint;
         Hi_Bound := No_Uint;

         --  Loop to climb ancestor subtypes and derived types

         ST1 := Typ;
         loop
            if not Is_Discrete_Type (ST1) then
               return;
            end if;

            Lo := Type_Low_Bound (ST1);
            Hi := Type_High_Bound (ST1);

            if Compile_Time_Known_Value (Lo) then
               Lo_Val := Expr_Value (Lo);

               if No (Lo_Bound) or else Lo_Bound < Lo_Val then
                  Lo_Bound := Lo_Val;
               end if;
            end if;

            if Compile_Time_Known_Value (Hi) then
               Hi_Val := Expr_Value (Hi);

               if No (Hi_Bound) or else Hi_Bound > Hi_Val then
                  Hi_Bound := Hi_Val;
               end if;
            end if;

            ST2 := Ancestor_Subtype (ST1);

            if No (ST2) then
               ST2 := Etype (ST1);
            end if;

            exit when ST1 = ST2;
            ST1 := ST2;
         end loop;
      end Extract_Subtype_Bounds;

      ----------------------------
      -- Simple_Init_Array_Type --
      ----------------------------

      function Simple_Init_Array_Type return Node_Id is
         Comp_Typ : constant Entity_Id := Component_Type (Typ);

         function Simple_Init_Dimension (Index : Node_Id) return Node_Id;
         --  Initialize a single array dimension with index constraint Index

         --------------------
         -- Simple_Init_Dimension --
         --------------------

         function Simple_Init_Dimension (Index : Node_Id) return Node_Id is
         begin
            --  Process the current dimension

            if Present (Index) then

               --  Build a suitable "others" aggregate for the next dimension,
               --  or initialize the component itself. Generate:
               --
               --    (others => ...)

               return
                 Make_Aggregate (Loc,
                   Component_Associations => New_List (
                     Make_Component_Association (Loc,
                       Choices    => New_List (Make_Others_Choice (Loc)),
                       Expression =>
                         Simple_Init_Dimension (Next_Index (Index)))));

            --  Otherwise all dimensions have been processed. Initialize the
            --  component itself.

            else
               return
                 Get_Simple_Init_Val
                   (Typ  => Comp_Typ,
                    N    => N,
                    Size => Esize (Comp_Typ));
            end if;
         end Simple_Init_Dimension;

      --  Start of processing for Simple_Init_Array_Type

      begin
         return Simple_Init_Dimension (First_Index (Typ));
      end Simple_Init_Array_Type;

      --------------------------------
      -- Simple_Init_Defaulted_Type --
      --------------------------------

      function Simple_Init_Defaulted_Type return Node_Id is
         Subtyp : Entity_Id := First_Subtype (Typ);

      begin
         --  When the first subtype is private, retrieve the expression of the
         --  Default_Value from the underlying type.

         if Is_Private_Type (Subtyp) then
            Subtyp := Full_View (Subtyp);
         end if;

         --  Use the Sloc of the context node when constructing the initial
         --  value because the expression of Default_Value may come from a
         --  different unit. Updating the Sloc will result in accurate error
         --  diagnostics.

         return
           OK_Convert_To
             (Typ  => Typ,
              Expr =>
                New_Copy_Tree
                  (Source   => Default_Aspect_Value (Subtyp),
                   New_Sloc => Loc));
      end Simple_Init_Defaulted_Type;

      -----------------------------------------
      -- Simple_Init_Initialize_Scalars_Type --
      -----------------------------------------

      function Simple_Init_Initialize_Scalars_Type
        (Size_To_Use : Uint) return Node_Id
      is
         Float_Typ : Entity_Id;
         Hi_Bound  : Uint;
         Lo_Bound  : Uint;
         Scal_Typ  : Scalar_Id;

      begin
         Extract_Subtype_Bounds (Lo_Bound, Hi_Bound);

         --  Float types

         if Is_Floating_Point_Type (Typ) then
            Float_Typ := Root_Type (Typ);

            if Float_Typ = Standard_Short_Float then
               Scal_Typ := Name_Short_Float;
            elsif Float_Typ = Standard_Float then
               Scal_Typ := Name_Float;
            elsif Float_Typ = Standard_Long_Float then
               Scal_Typ := Name_Long_Float;
            else pragma Assert (Float_Typ = Standard_Long_Long_Float);
               Scal_Typ := Name_Long_Long_Float;
            end if;

         --  If zero is invalid, it is a convenient value to use that is for
         --  sure an appropriate invalid value in all situations.

         elsif Present (Lo_Bound) and then Lo_Bound > Uint_0 then
            return Make_Integer_Literal (Loc, 0);

         --  Unsigned types

         elsif Is_Unsigned_Type (Typ) then
            if Size_To_Use <= 8 then
               Scal_Typ := Name_Unsigned_8;
            elsif Size_To_Use <= 16 then
               Scal_Typ := Name_Unsigned_16;
            elsif Size_To_Use <= 32 then
               Scal_Typ := Name_Unsigned_32;
            elsif Size_To_Use <= 64 then
               Scal_Typ := Name_Unsigned_64;
            else
               Scal_Typ := Name_Unsigned_128;
            end if;

         --  Signed types

         else
            if Size_To_Use <= 8 then
               Scal_Typ := Name_Signed_8;
            elsif Size_To_Use <= 16 then
               Scal_Typ := Name_Signed_16;
            elsif Size_To_Use <= 32 then
               Scal_Typ := Name_Signed_32;
            elsif Size_To_Use <= 64 then
               Scal_Typ := Name_Signed_64;
            else
               Scal_Typ := Name_Signed_128;
            end if;
         end if;

         --  Use the values specified by pragma Initialize_Scalars or the ones
         --  provided by the binder. Higher precedence is given to the pragma.

         return Invalid_Scalar_Value (Loc, Scal_Typ);
      end Simple_Init_Initialize_Scalars_Type;

      ----------------------------------------
      -- Simple_Init_Normalize_Scalars_Type --
      ----------------------------------------

      function Simple_Init_Normalize_Scalars_Type
        (Size_To_Use : Uint) return Node_Id
      is
         Signed_Size : constant Uint := UI_Min (Uint_63, Size_To_Use - 1);

         Expr     : Node_Id;
         Hi_Bound : Uint;
         Lo_Bound : Uint;

      begin
         Extract_Subtype_Bounds (Lo_Bound, Hi_Bound);

         --  If zero is invalid, it is a convenient value to use that is for
         --  sure an appropriate invalid value in all situations.

         if Present (Lo_Bound) and then Lo_Bound > Uint_0 then
            Expr := Make_Integer_Literal (Loc, 0);

         --  Cases where all one bits is the appropriate invalid value

         --  For modular types, all 1 bits is either invalid or valid. If it
         --  is valid, then there is nothing that can be done since there are
         --  no invalid values (we ruled out zero already).

         --  For signed integer types that have no negative values, either
         --  there is room for negative values, or there is not. If there
         --  is, then all 1-bits may be interpreted as minus one, which is
         --  certainly invalid. Alternatively it is treated as the largest
         --  positive value, in which case the observation for modular types
         --  still applies.

         --  For float types, all 1-bits is a NaN (not a number), which is
         --  certainly an appropriately invalid value.

         elsif Is_Enumeration_Type (Typ)
           or else Is_Floating_Point_Type (Typ)
           or else Is_Unsigned_Type (Typ)
         then
            Expr := Make_Integer_Literal (Loc, 2 ** Size_To_Use - 1);

            --  Resolve as Long_Long_Long_Unsigned, because the largest number
            --  we can generate is out of range of universal integer.

            Analyze_And_Resolve (Expr, Standard_Long_Long_Long_Unsigned);

         --  Case of signed types

         else
            --  Normally we like to use the most negative number. The one
            --  exception is when this number is in the known subtype range and
            --  the largest positive number is not in the known subtype range.

            --  For this exceptional case, use largest positive value

            if Present (Lo_Bound) and then Present (Hi_Bound)
              and then Lo_Bound <= (-(2 ** Signed_Size))
              and then Hi_Bound < 2 ** Signed_Size
            then
               Expr := Make_Integer_Literal (Loc, 2 ** Signed_Size - 1);

            --  Normal case of largest negative value

            else
               Expr := Make_Integer_Literal (Loc, -(2 ** Signed_Size));
            end if;
         end if;

         return Expr;
      end Simple_Init_Normalize_Scalars_Type;

      ------------------------------
      -- Simple_Init_Private_Type --
      ------------------------------

      function Simple_Init_Private_Type return Node_Id is
         Under_Typ : constant Entity_Id := Underlying_Type (Typ);
         Expr      : Node_Id;

      begin
         --  The availability of the underlying view must be checked by routine
         --  Needs_Simple_Initialization.

         pragma Assert (Present (Under_Typ));

         Expr := Get_Simple_Init_Val (Under_Typ, N, Size);

         --  If the initial value is null or an aggregate, qualify it with the
         --  underlying type in order to provide a proper context.

         if Nkind (Expr) in N_Aggregate | N_Null then
            Expr :=
              Make_Qualified_Expression (Loc,
                Subtype_Mark => New_Occurrence_Of (Under_Typ, Loc),
                Expression   => Expr);
         end if;

         Expr := Unchecked_Convert_To (Typ, Expr);

         --  Do not truncate the result when scalar types are involved and
         --  Initialize/Normalize_Scalars is in effect.

         if Nkind (Expr) = N_Unchecked_Type_Conversion
           and then Is_Scalar_Type (Under_Typ)
         then
            Set_Kill_Range_Check (Expr);
            Set_No_Truncation    (Expr);
         end if;

         return Expr;
      end Simple_Init_Private_Type;

      -----------------------------
      -- Simple_Init_Scalar_Type --
      -----------------------------

      function Simple_Init_Scalar_Type return Node_Id is
         Expr        : Node_Id;
         Size_To_Use : Uint;

      begin
         pragma Assert (Init_Or_Norm_Scalars or IV_Attribute);

         --  Determine the size of the object. This is either the size provided
         --  by the caller, or the Esize of the scalar type.

         if No (Size) or else Size <= Uint_0 then
            Size_To_Use := UI_Max (Uint_1, Esize (Typ));
         else
            Size_To_Use := Size;
         end if;

         --  The maximum size to use is System_Max_Integer_Size bits. This
         --  will create values of type Long_Long_Long_Unsigned and the range
         --  must fit this type.

         if Present (Size_To_Use)
           and then Size_To_Use > System_Max_Integer_Size
         then
            Size_To_Use := UI_From_Int (System_Max_Integer_Size);
         end if;

         if Normalize_Scalars and then not IV_Attribute then
            Expr := Simple_Init_Normalize_Scalars_Type (Size_To_Use);
         else
            Expr := Simple_Init_Initialize_Scalars_Type (Size_To_Use);
         end if;

         --  The final expression is obtained by doing an unchecked conversion
         --  of this result to the base type of the required subtype. Use the
         --  base type to prevent the unchecked conversion from chopping bits,
         --  and then we set Kill_Range_Check to preserve the "bad" value.

         Expr := Unchecked_Convert_To (Base_Type (Typ), Expr);

         --  Ensure that the expression is not truncated since the "bad" bits
         --  are desired, and also kill the range checks.

         if Nkind (Expr) = N_Unchecked_Type_Conversion then
            Set_Kill_Range_Check (Expr);
            Set_No_Truncation    (Expr);
         end if;

         return Expr;
      end Simple_Init_Scalar_Type;

   --  Start of processing for Get_Simple_Init_Val

   begin
      if Is_Private_Type (Typ) then
         return Simple_Init_Private_Type;

      elsif Is_Scalar_Type (Typ) then
         if Has_Default_Aspect (Typ) then
            return Simple_Init_Defaulted_Type;
         else
            return Simple_Init_Scalar_Type;
         end if;

      --  Array type with Initialize or Normalize_Scalars

      elsif Is_Array_Type (Typ) then
         pragma Assert (Init_Or_Norm_Scalars);
         return Simple_Init_Array_Type;

      --  Access type is initialized to null

      elsif Is_Access_Type (Typ) then
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

         --  May need a more precise check here: the First_Rep_Item may be a
         --  stream attribute, which does not affect the representation of the
         --  type ???

      end if;
   end Has_New_Non_Standard_Rep;

   ----------------------
   -- Inline_Init_Proc --
   ----------------------

   function Inline_Init_Proc (Typ : Entity_Id) return Boolean is
   begin
      --  The initialization proc of protected records is not worth inlining.
      --  In addition, when compiled for another unit for inlining purposes,
      --  it may make reference to entities that have not been elaborated yet.
      --  The initialization proc of records that need finalization contains
      --  a nested clean-up procedure that makes it impractical to inline as
      --  well, except for simple controlled types themselves. And similar
      --  considerations apply to task types.

      if Is_Concurrent_Type (Typ) then
         return False;

      elsif Needs_Finalization (Typ) and then not Is_Controlled (Typ) then
         return False;

      elsif Has_Task (Typ) then
         return False;

      else
         return True;
      end if;
   end Inline_Init_Proc;

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

   package body Initialization_Control is

      ------------------------
      -- Requires_Late_Init --
      ------------------------

      function Requires_Late_Init
        (Decl     : Node_Id;
         Rec_Type : Entity_Id) return Boolean
      is
         References_Current_Instance : Boolean := False;
         Has_Access_Discriminant     : Boolean := False;
         Has_Internal_Call           : Boolean := False;

         function Find_Access_Discriminant
           (N : Node_Id) return Traverse_Result;
         --  Look for a name denoting an access discriminant

         function Find_Current_Instance
           (N : Node_Id) return Traverse_Result;
         --  Look for a reference to the current instance of the type

         function Find_Internal_Call
           (N : Node_Id) return Traverse_Result;
         --  Look for an internal protected function call

         ------------------------------
         -- Find_Access_Discriminant --
         ------------------------------

         function Find_Access_Discriminant
           (N : Node_Id) return Traverse_Result is
         begin
            if Is_Entity_Name (N)
              and then Denotes_Discriminant (N)
              and then Is_Access_Type (Etype (N))
            then
               Has_Access_Discriminant := True;
               return Abandon;
            else
               return OK;
            end if;
         end Find_Access_Discriminant;

         ---------------------------
         -- Find_Current_Instance --
         ---------------------------

         function Find_Current_Instance
           (N : Node_Id) return Traverse_Result is
         begin
            if Is_Entity_Name (N)
              and then Present (Entity (N))
              and then Is_Current_Instance (N)
            then
               References_Current_Instance := True;
               return Abandon;
            else
               return OK;
            end if;
         end Find_Current_Instance;

         ------------------------
         -- Find_Internal_Call --
         ------------------------

         function Find_Internal_Call (N : Node_Id) return Traverse_Result is

            function Call_Scope (N : Node_Id) return Entity_Id;
            --  Return the scope enclosing a given call node N

            ----------------
            -- Call_Scope --
            ----------------

            function Call_Scope (N : Node_Id) return Entity_Id is
               Nam : constant Node_Id := Name (N);
            begin
               if Nkind (Nam) = N_Selected_Component then
                  return Scope (Entity (Prefix (Nam)));
               else
                  return Scope (Entity (Nam));
               end if;
            end Call_Scope;

         begin
            if Nkind (N) = N_Function_Call
              and then Call_Scope (N)
                         = Corresponding_Concurrent_Type (Rec_Type)
            then
               Has_Internal_Call := True;
               return Abandon;
            else
               return OK;
            end if;
         end Find_Internal_Call;

         procedure Search_Access_Discriminant is new
           Traverse_Proc (Find_Access_Discriminant);

         procedure Search_Current_Instance is new
           Traverse_Proc (Find_Current_Instance);

         procedure Search_Internal_Call is new
           Traverse_Proc (Find_Internal_Call);

         --  Start of processing for Requires_Late_Init

      begin
         --  A component of an object is said to require late initialization
         --  if:

         --  it has an access discriminant value constrained by a per-object
         --  expression;

         if Has_Access_Constraint (Defining_Identifier (Decl))
           and then No (Expression (Decl))
         then
            return True;

         elsif Present (Expression (Decl)) then

            --  it has an initialization expression that includes a name
            --  denoting an access discriminant;

            Search_Access_Discriminant (Expression (Decl));

            if Has_Access_Discriminant then
               return True;
            end if;

            --  or it has an initialization expression that includes a
            --  reference to the current instance of the type either by
            --  name...

            Search_Current_Instance (Expression (Decl));

            if References_Current_Instance then
               return True;
            end if;

            --  ...or implicitly as the target object of a call.

            if Is_Protected_Record_Type (Rec_Type) then
               Search_Internal_Call (Expression (Decl));

               if Has_Internal_Call then
                  return True;
               end if;
            end if;
         end if;

         return False;
      end Requires_Late_Init;

      -----------------------------
      -- Has_Late_Init_Component --
      -----------------------------

      function Has_Late_Init_Component
        (Tagged_Rec_Type : Entity_Id) return Boolean
      is
         Comp_Id : Entity_Id :=
           First_Component (Implementation_Base_Type (Tagged_Rec_Type));
      begin
         while Present (Comp_Id) loop
            if Requires_Late_Init (Decl     => Parent (Comp_Id),
                                   Rec_Type => Tagged_Rec_Type)
            then
               return True; -- found a component that requires late init

            elsif Chars (Comp_Id) = Name_uParent
              and then Has_Late_Init_Component (Etype (Comp_Id))
            then
               return True; -- an ancestor type has a late init component
            end if;

            Next_Component (Comp_Id);
         end loop;

         return False;
      end Has_Late_Init_Component;

      ------------------------
      -- Tag_Init_Condition --
      ------------------------

      function Tag_Init_Condition
        (Loc : Source_Ptr;
         Init_Control_Formal : Entity_Id) return Node_Id is
      begin
         return Make_Op_Eq (Loc,
                  New_Occurrence_Of (Init_Control_Formal, Loc),
                  Make_Mode_Literal (Loc, Full_Init));
      end Tag_Init_Condition;

      --------------------------
      -- Early_Init_Condition --
      --------------------------

      function Early_Init_Condition
        (Loc : Source_Ptr;
         Init_Control_Formal : Entity_Id) return Node_Id is
      begin
         return Make_Op_Ne (Loc,
                  New_Occurrence_Of (Init_Control_Formal, Loc),
                  Make_Mode_Literal (Loc, Late_Init_Only));
      end Early_Init_Condition;

      -------------------------
      -- Late_Init_Condition --
      -------------------------

      function Late_Init_Condition
        (Loc : Source_Ptr;
         Init_Control_Formal : Entity_Id) return Node_Id is
      begin
         return Make_Op_Ne (Loc,
                  New_Occurrence_Of (Init_Control_Formal, Loc),
                  Make_Mode_Literal (Loc, Early_Init_Only));
      end Late_Init_Condition;

   end Initialization_Control;

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
                        pragma Assert
                          (Nkind (Parent (Comp)) = N_Component_Declaration);

                        if Present (Expression (Parent (Comp))) then
                           Warning_Needed := True;
                           exit;
                        end if;

                        Next_Component (Comp);
                     end loop;
                  end;
               end if;

               if Warning_Needed then
                  Error_Msg_N
                    ("objects of the type cannot be initialized statically "
                     & "by default??", Parent (E));
               end if;
            end if;

         else
            Error_Msg_N ("object cannot be initialized statically??", E);
         end if;
      end if;
   end Initialization_Warning;

   ------------------
   -- Init_Formals --
   ------------------

   function Init_Formals (Typ : Entity_Id; Proc_Id : Entity_Id) return List_Id
   is
      Loc        : constant Source_Ptr := Sloc (Typ);
      Unc_Arr    : constant Boolean :=
                     Is_Array_Type (Typ) and then not Is_Constrained (Typ);
      With_Prot  : constant Boolean :=
                     Has_Protected (Typ)
                       or else (Is_Record_Type (Typ)
                                 and then Is_Protected_Record_Type (Typ));
      With_Task  : constant Boolean :=
                     not Global_No_Tasking
                       and then
                     (Has_Task (Typ)
                        or else (Is_Record_Type (Typ)
                                   and then Is_Task_Record_Type (Typ)));
      Formals : List_Id;

   begin
      --  The first parameter is always _Init : [in] out Typ. Note that we need
      --  it to be in/out in the case of an unconstrained array, because of the
      --  need to have the bounds, and in the case of protected or task record
      --  value, because there are default record fields that may be referenced
      --  in the generated initialization routine.

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uInit),
          In_Present          => Unc_Arr or else With_Prot or else With_Task,
          Out_Present         => True,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)));

      --  For task record value, or type that contains tasks, add two more
      --  formals, _Master : Master_Id and _Chain : in out Activation_Chain
      --  We also add these parameters for the task record type case.

      if With_Task then
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uMaster),
             Parameter_Type      =>
               New_Occurrence_Of (Standard_Integer, Loc)));

         Set_Has_Master_Entity (Proc_Id);

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
                  New_Occurrence_Of (RTE (RE_Activation_Chain), Loc)));
         end if;

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uTask_Name),
             In_Present          => True,
             Parameter_Type      => New_Occurrence_Of (Standard_String, Loc)));
      end if;

      --  Due to certain edge cases such as arrays with null-excluding
      --  components being built with the secondary stack it becomes necessary
      --  to add a formal to the Init_Proc which controls whether we raise
      --  Constraint_Errors on generated calls for internal object
      --  declarations.

      if Needs_Conditional_Null_Excluding_Check (Typ) then
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc,
                 New_External_Name (Chars
                   (Component_Type (Typ)), "_skip_null_excluding_check")),
             Expression          => New_Occurrence_Of (Standard_False, Loc),
             In_Present          => True,
             Parameter_Type      =>
               New_Occurrence_Of (Standard_Boolean, Loc)));
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
      Init_Tags_List : List_Id;
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
         --  Initialize pointer to secondary DT associated with the interface

         if not Is_Ancestor (Iface, Typ, Use_Full_View => True) then
            Append_To (Init_Tags_List,
              Make_Assignment_Statement (Loc,
                Name       =>
                  Make_Selected_Component (Loc,
                    Prefix        => New_Copy_Tree (Target),
                    Selector_Name => New_Occurrence_Of (Tag_Comp, Loc)),
                Expression =>
                  New_Occurrence_Of (Iface_Tag, Loc)));
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
            --       Prim_T       => Typ'Tag,
            --       Interface_T  => Iface'Tag,
            --       Offset_Value => n,
            --       Offset_Func  => Fn'Unrestricted_Access)

            Append_To (Stmts_List,
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Set_Dynamic_Offset_To_Top), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Copy_Tree (Target),
                    Attribute_Name => Name_Address),

                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Occurrence_Of
                      (Node (First_Elmt (Access_Disp_Table (Typ))), Loc)),

                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Occurrence_Of
                      (Node (First_Elmt (Access_Disp_Table (Iface))),
                       Loc)),

                  Unchecked_Convert_To
                    (RTE (RE_Storage_Offset),
                     Make_Op_Minus (Loc,
                       Make_Attribute_Reference (Loc,
                         Prefix         =>
                           Make_Selected_Component (Loc,
                             Prefix        => New_Copy_Tree (Target),
                             Selector_Name =>
                               New_Occurrence_Of (Tag_Comp, Loc)),
                         Attribute_Name => Name_Position))),

                  Unchecked_Convert_To (RTE (RE_Offset_To_Top_Function_Ptr),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of
                                  (DT_Offset_To_Top_Func (Tag_Comp), Loc),
                      Attribute_Name => Name_Unrestricted_Access)))));

            --  In this case the next component stores the value of the offset
            --  to the top.

            Offset_To_Top_Comp := Next_Entity (Tag_Comp);
            pragma Assert (Present (Offset_To_Top_Comp));

            Append_To (Init_Tags_List,
              Make_Assignment_Statement (Loc,
                Name       =>
                  Make_Selected_Component (Loc,
                    Prefix        => New_Copy_Tree (Target),
                    Selector_Name =>
                      New_Occurrence_Of (Offset_To_Top_Comp, Loc)),

                Expression =>
                  Make_Op_Minus (Loc,
                    Make_Attribute_Reference (Loc,
                      Prefix       =>
                        Make_Selected_Component (Loc,
                          Prefix        => New_Copy_Tree (Target),
                          Selector_Name => New_Occurrence_Of (Tag_Comp, Loc)),
                    Attribute_Name => Name_Position))));

         --  Normal case: No discriminants in the parent type

         else
            --  Don't need to set any value if the offset-to-top field is
            --  statically set or if this interface shares the primary
            --  dispatch table.

            if not Building_Static_Secondary_DT (Typ)
              and then not Is_Ancestor (Iface, Typ, Use_Full_View => True)
            then
               Append_To (Stmts_List,
                 Build_Set_Static_Offset_To_Top (Loc,
                   Iface_Tag    => New_Occurrence_Of (Iface_Tag, Loc),
                   Offset_Value =>
                     Unchecked_Convert_To (RTE (RE_Storage_Offset),
                       Make_Op_Minus (Loc,
                         Make_Attribute_Reference (Loc,
                           Prefix         =>
                             Make_Selected_Component (Loc,
                               Prefix        => New_Copy_Tree (Target),
                               Selector_Name =>
                                 New_Occurrence_Of (Tag_Comp, Loc)),
                           Attribute_Name => Name_Position)))));
            end if;

            --  Generate:
            --    Register_Interface_Offset
            --      (Prim_T       => Typ'Tag,
            --       Interface_T  => Iface'Tag,
            --       Is_Constant  => True,
            --       Offset_Value => n,
            --       Offset_Func  => null);

            if not Building_Static_Secondary_DT (Typ)
              and then RTE_Available (RE_Register_Interface_Offset)
            then
               Append_To (Stmts_List,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of
                       (RTE (RE_Register_Interface_Offset), Loc),
                   Parameter_Associations => New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Occurrence_Of
                         (Node (First_Elmt (Access_Disp_Table (Typ))), Loc)),

                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Occurrence_Of
                         (Node (First_Elmt (Access_Disp_Table (Iface))), Loc)),

                     New_Occurrence_Of (Standard_True, Loc),

                     Unchecked_Convert_To (RTE (RE_Storage_Offset),
                       Make_Op_Minus (Loc,
                         Make_Attribute_Reference (Loc,
                           Prefix         =>
                             Make_Selected_Component (Loc,
                               Prefix         => New_Copy_Tree (Target),
                               Selector_Name  =>
                                 New_Occurrence_Of (Tag_Comp, Loc)),
                           Attribute_Name => Name_Position))),

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
                            and then
                              Is_Variable_Size_Record (Base_Type (Comp_Typ)))
                         or else
                           (Is_Array_Type (Comp_Typ)
                             and then Is_Variable_Size_Array (Comp_Typ));
                     end if;

                     Next_Entity (Comp);
                  end loop;

                  pragma Assert (Present (Comp));

                  --  Move this check to sem???
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
               Initialize_Tag
                 (Typ       => Full_Typ,
                  Iface     => Node (Iface_Elmt),
                  Tag_Comp  => Tag_Comp,
                  Iface_Tag => Node (Iface_Tag_Elmt));
            end if;

         --  Otherwise generate code to initialize the tag

         else
            if (In_Variable_Pos and then Variable_Comps)
              or else (not In_Variable_Pos and then Fixed_Comps)
            then
               Initialize_Tag
                 (Typ       => Full_Typ,
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

   ----------------------------
   -- Is_Null_Statement_List --
   ----------------------------

   function Is_Null_Statement_List (Stmts : List_Id) return Boolean is
      Stmt : Node_Id;

   begin
      --  We must skip SCIL nodes because they may have been added to the list
      --  by Insert_Actions.

      Stmt := First_Non_SCIL_Node (Stmts);
      while Present (Stmt) loop
         if Nkind (Stmt) = N_Case_Statement then
            declare
               Alt : Node_Id;
            begin
               Alt := First (Alternatives (Stmt));
               while Present (Alt) loop
                  if not Is_Null_Statement_List (Statements (Alt)) then
                     return False;
                  end if;

                  Next (Alt);
               end loop;
            end;

         elsif Nkind (Stmt) /= N_Null_Statement then
            return False;
         end if;

         Stmt := Next_Non_SCIL_Node (Stmt);
      end loop;

      return True;
   end Is_Null_Statement_List;

   ----------------------------------------
   -- Make_Controlling_Function_Wrappers --
   ----------------------------------------

   procedure Make_Controlling_Function_Wrappers
     (Tag_Typ   : Entity_Id;
      Decl_List : out List_Id;
      Body_List : out List_Id)
   is
      Loc : constant Source_Ptr := Sloc (Tag_Typ);

      function Make_Wrapper_Specification (Subp : Entity_Id) return Node_Id;
      --  Returns a function specification with the same profile as Subp

      --------------------------------
      -- Make_Wrapper_Specification --
      --------------------------------

      function Make_Wrapper_Specification (Subp : Entity_Id) return Node_Id is
      begin
         return
           Make_Function_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc,
                 Chars => Chars (Subp)),
             Parameter_Specifications =>
               Copy_Parameter_List (Subp),
             Result_Definition        =>
               New_Occurrence_Of (Etype (Subp), Loc));
      end Make_Wrapper_Specification;

      Prim_Elmt   : Elmt_Id;
      Subp        : Entity_Id;
      Actual_List : List_Id;
      Formal      : Entity_Id;
      Par_Formal  : Entity_Id;
      Ext_Aggr    : Node_Id;
      Formal_Node : Node_Id;
      Func_Body   : Node_Id;
      Func_Decl   : Node_Id;
      Func_Id     : Entity_Id;

   --  Start of processing for Make_Controlling_Function_Wrappers

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
            --  If there is a non-overloadable homonym in the current
            --  scope, the implicit declaration remains invisible.
            --  We check the current entity with the same name, or its
            --  homonym in case the derivation takes place after the
            --  hiding object declaration.

            if Present (Current_Entity (Subp)) then
               declare
                  Curr : constant Entity_Id := Current_Entity (Subp);
                  Prev : constant Entity_Id := Homonym (Curr);
               begin
                  if (Comes_From_Source (Curr)
                    and then Scope (Curr) = Current_Scope
                    and then not Is_Overloadable (Curr))
                  or else
                    (Present (Prev)
                      and then Comes_From_Source (Prev)
                      and then Scope (Prev) = Current_Scope
                      and then not Is_Overloadable (Prev))
                  then
                     goto Next_Prim;
                  end if;
               end;
            end if;

            Func_Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification => Make_Wrapper_Specification (Subp));

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
            Formal_Node :=
              First (Parameter_Specifications (Specification (Func_Decl)));

            if Present (Formal) then
               Actual_List := New_List;

               while Present (Formal) loop
                  if Is_Controlling_Formal (Formal) then
                     Append_To (Actual_List,
                       Make_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of (Etype (Par_Formal), Loc),
                         Expression   =>
                           New_Occurrence_Of
                             (Defining_Identifier (Formal_Node), Loc)));
                  else
                     Append_To
                       (Actual_List,
                        New_Occurrence_Of
                          (Defining_Identifier (Formal_Node), Loc));
                  end if;

                  Next_Formal (Formal);
                  Next_Formal (Par_Formal);
                  Next (Formal_Node);
               end loop;
            else
               Actual_List := No_List;
            end if;

            Ext_Aggr :=
              Make_Extension_Aggregate (Loc,
                Ancestor_Part       =>
                  Make_Function_Call (Loc,
                    Name                   =>
                      New_Occurrence_Of (Alias (Subp), Loc),
                    Parameter_Associations => Actual_List),
                Null_Record_Present => True);

            --  GNATprove will use expression of an expression function as an
            --  implicit postcondition. GNAT will also benefit from expression
            --  function to avoid premature freezing, but would struggle if we
            --  added an expression function to freezing actions, so we create
            --  the expanded form directly.

            if GNATprove_Mode then
               Func_Body :=
                 Make_Expression_Function (Loc,
                   Specification =>
                     Make_Wrapper_Specification (Subp),
                   Expression => Ext_Aggr);
            else
               Func_Body :=
                 Make_Subprogram_Body (Loc,
                   Specification              =>
                     Make_Wrapper_Specification (Subp),
                   Declarations               => Empty_List,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Simple_Return_Statement (Loc,
                           Expression => Ext_Aggr))));
               Set_Was_Expression_Function (Func_Body);
            end if;

            Append_To (Body_List, Func_Body);

            --  Replace the inherited function with the wrapper function in the
            --  primitive operations list. We add the minimum decoration needed
            --  to override interface primitives.

            Func_Id := Defining_Unit_Name (Specification (Func_Decl));

            Mutate_Ekind (Func_Id, E_Function);
            Set_Is_Wrapper (Func_Id);

            --  Corresponding_Spec will be set again to the same value during
            --  analysis, but we need this information earlier.
            --  Expand_N_Freeze_Entity needs to know whether a subprogram body
            --  is a wrapper's body in order to get check suppression right.

            Set_Corresponding_Spec (Func_Body, Func_Id);
         end if;

      <<Next_Prim>>
         Next_Elmt (Prim_Elmt);
      end loop;
   end Make_Controlling_Function_Wrappers;

   ------------------
   -- Make_Eq_Body --
   ------------------

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
          Tag_Typ  => Typ,
          Name     => Eq_Name,
          Profile  => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_X),
              Parameter_Type      => New_Occurrence_Of (Typ, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_Y),
              Parameter_Type      => New_Occurrence_Of (Typ, Loc))),

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
             Expression => New_Occurrence_Of (Standard_True, Loc)));

      else
         Append_To (Stmts,
           Make_Simple_Return_Statement (Loc,
             Expression =>
               Expand_Record_Equality
                 (Typ,
                  Typ => Typ,
                  Lhs => Make_Identifier (Loc, Name_X),
                  Rhs => Make_Identifier (Loc, Name_Y))));
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
               New_Occurrence_Of (Corresponding_Formal (CL), Loc),
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

   --  Equality may be user-defined for a given component type, in which case
   --  a function call is constructed instead of an operator node. This is an
   --  Ada 2012 change in the composability of equality for untagged composite
   --  types.

   function Make_Eq_If
     (E : Entity_Id;
      L : List_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (E);

      C          : Node_Id;
      Cond       : Node_Id;
      Field_Name : Name_Id;
      Next_Test  : Node_Id;
      Typ        : Entity_Id;

   begin
      if No (L) then
         return Make_Null_Statement (Loc);

      else
         Cond := Empty;

         C := First_Non_Pragma (L);
         while Present (C) loop
            Typ        := Etype (Defining_Identifier (C));
            Field_Name := Chars (Defining_Identifier (C));

            --  The tags must not be compared: they are not part of the value.
            --  Ditto for parent interfaces because their equality operator is
            --  abstract.

            --  Note also that in the following, we use Make_Identifier for
            --  the component names. Use of New_Occurrence_Of to identify the
            --  components would be incorrect because the wrong entities for
            --  discriminants could be picked up in the private type case.

            if Field_Name = Name_uParent
              and then Is_Interface (Typ)
            then
               null;

            elsif Field_Name /= Name_uTag then
               declare
                  Lhs : constant Node_Id :=
                    Make_Selected_Component (Loc,
                      Prefix        => Make_Identifier (Loc, Name_X),
                      Selector_Name => Make_Identifier (Loc, Field_Name));

                  Rhs : constant Node_Id :=
                    Make_Selected_Component (Loc,
                      Prefix        => Make_Identifier (Loc, Name_Y),
                      Selector_Name => Make_Identifier (Loc, Field_Name));
                  Eq_Call : Node_Id;

               begin
                  --  Build equality code with a user-defined operator, if
                  --  available, and with the predefined "=" otherwise. For
                  --  compatibility with older Ada versions, we also use the
                  --  predefined operation if the component-type equality is
                  --  abstract, rather than raising Program_Error.

                  if Ada_Version < Ada_2012 then
                     Next_Test := Make_Op_Ne (Loc, Lhs, Rhs);

                  else
                     Eq_Call := Build_Eq_Call (Typ, Loc, Lhs, Rhs);

                     if No (Eq_Call) then
                        Next_Test := Make_Op_Ne (Loc, Lhs, Rhs);

                     --  If a component has a defined abstract equality, its
                     --  application raises Program_Error on that component
                     --  and therefore on the current variant.

                     elsif Nkind (Eq_Call) = N_Raise_Program_Error then
                        Set_Etype (Eq_Call, Standard_Boolean);
                        Next_Test := Make_Op_Not (Loc, Eq_Call);

                     else
                        Next_Test := Make_Op_Not (Loc, Eq_Call);
                     end if;
                  end if;
               end;

               Evolve_Or_Else (Cond, Next_Test);
            end if;

            Next_Non_Pragma (C);
         end loop;

         if No (Cond) then
            return Make_Null_Statement (Loc);

         else
            return
              Make_Implicit_If_Statement (E,
                Condition       => Cond,
                Then_Statements => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))));
         end if;
      end if;
   end Make_Eq_If;

   -------------------
   -- Make_Neq_Body --
   -------------------

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
      --  (RM 8.5.4(8)).

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

            if Is_User_Defined_Equality (Prim) and then No (Alias (Prim)) then
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
          Tag_Typ  => Tag_Typ,
          Name     => Chars (Renaming_Prim),
          Profile  => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Chars (Left_Op)),
              Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Chars (Right_Op)),
              Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc))),

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

      Set_Handled_Statement_Sequence
        (Decl,
         Make_Handled_Sequence_Of_Statements (Loc, New_List (
           Make_Simple_Return_Statement (Loc,
              Expression =>
                Make_Op_Not (Loc,
                  Make_Function_Call (Loc,
                  Name                   => New_Occurrence_Of (Target, Loc),
                  Parameter_Associations => New_List (
                    Make_Identifier (Loc, Chars (Left_Op)),
                    Make_Identifier (Loc, Chars (Right_Op)))))))));

      return Decl;
   end Make_Neq_Body;

   -------------------------------
   -- Make_Null_Procedure_Specs --
   -------------------------------

   function Make_Null_Procedure_Specs (Tag_Typ : Entity_Id) return List_Id is
      Decl_List      : constant List_Id    := New_List;
      Loc            : constant Source_Ptr := Sloc (Tag_Typ);
      Formal         : Entity_Id;
      New_Param_Spec : Node_Id;
      New_Spec       : Node_Id;
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
            --  The null procedure spec is copied from the inherited procedure,
            --  except for the IS NULL (which must be added) and the overriding
            --  indicators (which must be removed, if present).

            New_Spec :=
              Copy_Subprogram_Spec (Subprogram_Specification (Subp), Loc);

            Set_Null_Present      (New_Spec, True);
            Set_Must_Override     (New_Spec, False);
            Set_Must_Not_Override (New_Spec, False);

            Formal := First_Formal (Subp);
            New_Param_Spec := First (Parameter_Specifications (New_Spec));

            while Present (Formal) loop

               --  For controlling arguments we must change their parameter
               --  type to reference the tagged type (instead of the interface
               --  type).

               if Is_Controlling_Formal (Formal) then
                  if Nkind (Parameter_Type (Parent (Formal))) = N_Identifier
                  then
                     Set_Parameter_Type (New_Param_Spec,
                       New_Occurrence_Of (Tag_Typ, Loc));

                  else pragma Assert
                         (Nkind (Parameter_Type (Parent (Formal))) =
                                                     N_Access_Definition);
                     Set_Subtype_Mark (Parameter_Type (New_Param_Spec),
                       New_Occurrence_Of (Tag_Typ, Loc));
                  end if;
               end if;

               Next_Formal (Formal);
               Next (New_Param_Spec);
            end loop;

            Append_To (Decl_List,
              Make_Subprogram_Declaration (Loc,
                Specification => New_Spec));
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;

      return Decl_List;
   end Make_Null_Procedure_Specs;

   ---------------------------------------
   -- Make_Predefined_Primitive_Eq_Spec --
   ---------------------------------------

   procedure Make_Predefined_Primitive_Eq_Spec
     (Tag_Typ     : Entity_Id;
      Predef_List : List_Id;
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

      Loc : constant Source_Ptr := Sloc (Tag_Typ);

      Eq_Name   : Name_Id := Name_Op_Eq;
      Eq_Needed : Boolean := True;
      Eq_Spec   : Node_Id;
      Prim      : Elmt_Id;

      Has_Predef_Eq_Renaming : Boolean := False;
      --  Set to True if Tag_Typ has a primitive that renames the predefined
      --  equality operator. Used to implement (RM 8-5-4(8)).

   --  Start of processing for Make_Predefined_Primitive_Specs

   begin
      Renamed_Eq := Empty;

      Prim := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Prim) loop

         --  If a primitive is encountered that renames the predefined equality
         --  operator before reaching any explicit equality primitive, then we
         --  still need to create a predefined equality function, because calls
         --  to it can occur via the renaming. A new name is created for the
         --  equality to avoid conflicting with any user-defined equality.
         --  (Note that this doesn't account for renamings of equality nested
         --  within subpackages???)

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
            --  equality function explicitly defined in the sources, then the
            --  inherited equality is abstract as well, and no body can be
            --  created for it.

            elsif not Is_Interface (Etype (Tag_Typ))
              and then Present (Alias (Node (Prim)))
              and then Comes_From_Source (Alias (Node (Prim)))
              and then Is_Abstract_Subprogram (Alias (Node (Prim)))
            then
               Eq_Needed := False;
               exit;

            --  If the type has an equality function corresponding with a
            --  primitive defined in an interface type, the inherited equality
            --  is abstract as well, and no body can be created for it.

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
      --  user-defined equality (so Eq_Needed is still true), then set the name
      --  back to Name_Op_Eq. But in the case where a user-defined equality was
      --  located after such a renaming, then the predefined equality function
      --  is still needed, so Eq_Needed must be set back to True.

      if Eq_Name /= Name_Op_Eq then
         if Eq_Needed then
            Eq_Name := Name_Op_Eq;
         else
            Eq_Needed := True;
         end if;
      end if;

      if Eq_Needed then
         Eq_Spec := Predef_Spec_Or_Body (Loc,
           Tag_Typ  => Tag_Typ,
           Name     => Eq_Name,
           Profile  => New_List (
             Make_Parameter_Specification (Loc,
               Defining_Identifier =>
                 Make_Defining_Identifier (Loc, Name_X),
               Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc)),

             Make_Parameter_Specification (Loc,
               Defining_Identifier =>
                 Make_Defining_Identifier (Loc, Name_Y),
               Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc))),
           Ret_Type => Standard_Boolean);
         Append_To (Predef_List, Eq_Spec);

         if Has_Predef_Eq_Renaming then
            Renamed_Eq := Defining_Unit_Name (Specification (Eq_Spec));

            Prim := First_Elmt (Primitive_Operations (Tag_Typ));
            while Present (Prim) loop

               --  Any renamings of equality that appeared before an overriding
               --  equality must be updated to refer to the entity for the
               --  predefined equality, otherwise calls via the renaming would
               --  get incorrectly resolved to call the user-defined equality
               --  function.

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
   end Make_Predefined_Primitive_Eq_Spec;

   -------------------------------------
   -- Make_Predefined_Primitive_Specs --
   -------------------------------------

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Tag_Typ);
      Res : constant List_Id    := New_List;

      use Exp_Put_Image;

   begin
      Renamed_Eq := Empty;

      --  Spec of _Size

      Append_To (Res, Predef_Spec_Or_Body (Loc,
        Tag_Typ  => Tag_Typ,
        Name     => Name_uSize,
        Profile  => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc))),

        Ret_Type => Standard_Long_Long_Integer));

      --  Spec of Put_Image

      if not No_Run_Time_Mode
        and then RTE_Available (RE_Root_Buffer_Type)
      then
         --  No_Run_Time_Mode implies that the declaration of Tag_Typ
         --  (like any tagged type) will be rejected. Given this, avoid
         --  cascading errors associated with the Tag_Typ's TSS_Put_Image
         --  procedure.

         Append_To (Res, Predef_Spec_Or_Body (Loc,
           Tag_Typ => Tag_Typ,
           Name    => Make_TSS_Name (Tag_Typ, TSS_Put_Image),
           Profile => Build_Put_Image_Profile (Loc, Tag_Typ)));
      end if;

      --  Specs for dispatching stream attributes

      declare
         Stream_Op_TSS_Names :
           constant array (Positive range <>) of TSS_Name_Type :=
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
      --  private extension.

      if not Is_Limited_Type (Tag_Typ) then
         Make_Predefined_Primitive_Eq_Spec (Tag_Typ, Res, Renamed_Eq);

         --  Spec for dispatching assignment

         Append_To (Res, Predef_Spec_Or_Body (Loc,
           Tag_Typ => Tag_Typ,
           Name    => Name_uAssign,
           Profile => New_List (
             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
               Out_Present         => True,
               Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc)),

             Make_Parameter_Specification (Loc,
               Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
               Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc)))));
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

      --  Disable the generation of these bodies if Ravenscar or ZFP is active

      if Ada_Version >= Ada_2005
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

         --  If ancestor is an interface type, declare non-abstract primitives
         --  to override the abstract primitives of the interface type.

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

      else
         if not Is_Limited_Type (Tag_Typ) then
            Append_To (Res, Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Adjust));
         end if;

         Append_To (Res, Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Finalize));
      end if;

      Predef_List := Res;
   end Make_Predefined_Primitive_Specs;

   -------------------------
   -- Make_Tag_Assignment --
   -------------------------

   function Make_Tag_Assignment (N : Node_Id) return Node_Id is
      Loc      : constant Source_Ptr := Sloc (N);
      Def_Id   : constant Entity_Id  := Defining_Identifier (N);
      Expr     : constant Node_Id    := Expression (N);
      Typ      : constant Entity_Id  := Etype (Def_Id);
      Full_Typ : constant Entity_Id  := Underlying_Type (Typ);

   begin
      --  This expansion activity is called during analysis

      if Is_Tagged_Type (Typ)
        and then not Is_Class_Wide_Type (Typ)
        and then not Is_CPP_Class (Typ)
        and then Tagged_Type_Expansion
        and then Nkind (Unqualify (Expr)) /= N_Aggregate
      then
         return
           Make_Tag_Assignment_From_Type
             (Loc, New_Occurrence_Of (Def_Id, Loc), Full_Typ);

      else
         return Empty;
      end if;
   end Make_Tag_Assignment;

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
          Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc)));

      --  F : Boolean := True

      if Name = TSS_Deep_Adjust
        or else Name = TSS_Deep_Finalize
      then
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_F),
             Parameter_Type      => New_Occurrence_Of (Standard_Boolean, Loc),
             Expression          => New_Occurrence_Of (Standard_True, Loc)));
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
             Result_Definition        => New_Occurrence_Of (Ret_Type, Loc));
      end if;

      --  Declare an abstract subprogram for primitive subprograms of an
      --  interface type (except for "=").

      if Is_Interface (Tag_Typ) then
         if Name /= Name_Op_Eq then
            return Make_Abstract_Subprogram_Declaration (Loc, Spec);

         --  The equality function (if any) for an interface type is defined
         --  to be nonabstract, so we create an expression function for it that
         --  always returns False. Note that the function can never actually be
         --  invoked because interface types are abstract, so there aren't any
         --  objects of such types (and their equality operation will always
         --  dispatch).

         else
            return Make_Expression_Function
                     (Loc, Spec, New_Occurrence_Of (Standard_False, Loc));
         end if;

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
     (Loc     : Source_Ptr;
      Tag_Typ : Entity_Id;
      Name    : TSS_Name_Type) return Node_Id
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
           For_Body => False);
   end Predef_Stream_Attr_Spec;

   ----------------------------------
   -- Predefined_Primitive_Eq_Body --
   ----------------------------------

   procedure Predefined_Primitive_Eq_Body
     (Tag_Typ     : Entity_Id;
      Predef_List : List_Id;
      Renamed_Eq  : Entity_Id)
   is
      Decl      : Node_Id;
      Eq_Needed : Boolean;
      Eq_Name   : Name_Id;
      Prim      : Elmt_Id;

   begin
      --  See if we have a predefined "=" operator

      if Present (Renamed_Eq) then
         Eq_Needed := True;
         Eq_Name   := Chars (Renamed_Eq);

      --  If the parent is an interface type then it has defined all the
      --  predefined primitives abstract and we need to check if the type
      --  has some user defined "=" function which matches the profile of
      --  the Ada predefined equality operator to avoid generating it.

      elsif Is_Interface (Etype (Tag_Typ)) then
         Eq_Needed := True;
         Eq_Name := Name_Op_Eq;

         Prim := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Prim) loop
            if Is_User_Defined_Equality (Node (Prim))
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
            if Is_User_Defined_Equality (Node (Prim))
              and then Is_Internal (Node (Prim))
            then
               Eq_Needed := True;
               Eq_Name := Name_Op_Eq;
               exit;
            end if;

            Next_Elmt (Prim);
         end loop;
      end if;

      --  If equality is needed, we will have its name

      pragma Assert (Eq_Needed = Present (Eq_Name));

      --  Body for equality

      if Eq_Needed then
         Decl := Make_Eq_Body (Tag_Typ, Eq_Name);
         Append_To (Predef_List, Decl);
      end if;

      --  Body for inequality (if required)

      Decl := Make_Neq_Body (Tag_Typ);

      if Present (Decl) then
         Append_To (Predef_List, Decl);
      end if;
   end Predefined_Primitive_Eq_Body;

   ---------------------------------
   -- Predefined_Primitive_Bodies --
   ---------------------------------

   function Predefined_Primitive_Bodies
     (Tag_Typ    : Entity_Id;
      Renamed_Eq : Entity_Id) return List_Id
   is
      Loc      : constant Source_Ptr := Sloc (Tag_Typ);
      Res      : constant List_Id    := New_List;
      Adj_Call : Node_Id;
      Decl     : Node_Id;
      Fin_Call : Node_Id;
      Ent      : Entity_Id;

      pragma Warnings (Off, Ent);

      use Exp_Put_Image;

   begin
      pragma Assert (not Is_Interface (Tag_Typ));

      --  Body of _Size

      Decl := Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uSize,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc))),

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

      --  Body of Put_Image

      if No (TSS (Tag_Typ, TSS_Put_Image))
         and then not No_Run_Time_Mode
         and then RTE_Available (RE_Root_Buffer_Type)
      then
         Build_Record_Put_Image_Procedure (Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      --  Bodies for Dispatching stream IO routines. We need these only for
      --  non-limited types (in the limited case there is no dispatching).
      --  We also skip them if dispatching or finalization are not available
      --  or if stream operations are prohibited by restriction No_Streams or
      --  from use of pragma/aspect No_Tagged_Streams.

      if Stream_Operation_OK (Tag_Typ, TSS_Stream_Read)
        and then No (TSS (Tag_Typ, TSS_Stream_Read))
      then
         Build_Record_Read_Procedure (Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      if Stream_Operation_OK (Tag_Typ, TSS_Stream_Write)
        and then No (TSS (Tag_Typ, TSS_Stream_Write))
      then
         Build_Record_Write_Procedure (Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      --  Skip body of _Input for the abstract case, since the corresponding
      --  spec is abstract (see Predef_Spec_Or_Body).

      if not Is_Abstract_Type (Tag_Typ)
        and then Stream_Operation_OK (Tag_Typ, TSS_Stream_Input)
        and then No (TSS (Tag_Typ, TSS_Stream_Input))
      then
         Build_Record_Or_Elementary_Input_Function (Tag_Typ, Decl, Ent);
         Append_To (Res, Decl);
      end if;

      if Stream_Operation_OK (Tag_Typ, TSS_Stream_Output)
        and then No (TSS (Tag_Typ, TSS_Stream_Output))
      then
         Build_Record_Or_Elementary_Output_Procedure (Tag_Typ, Decl, Ent);
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

      --  Disable the generation of these bodies if Ravenscar or ZFP is active

      --  In VM targets we define these primitives in all root tagged types
      --  that are not interface types. Done because in VM targets we don't
      --  have secondary dispatch tables and any derivation of Tag_Typ may
      --  cover limited interfaces (which always have these primitives since
      --  they may be ancestors of synchronized interface types).

      if Ada_Version >= Ada_2005
        and then
          ((Is_Interface (Etype (Tag_Typ))
             and then Is_Limited_Record (Etype (Tag_Typ)))
           or else
             (Is_Concurrent_Record_Type (Tag_Typ)
               and then Has_Interfaces (Tag_Typ))
           or else
             (not Tagged_Type_Expansion
               and then Tag_Typ = Root_Type (Tag_Typ)))
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

      if not Is_Limited_Type (Tag_Typ) then
         --  Body for equality and inequality

         Predefined_Primitive_Eq_Body (Tag_Typ, Res, Renamed_Eq);

         --  Body for dispatching assignment

         Decl :=
           Predef_Spec_Or_Body (Loc,
             Tag_Typ => Tag_Typ,
             Name    => Name_uAssign,
             Profile => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
                 Out_Present         => True,
                 Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Make_Defining_Identifier (Loc, Name_Y),
                 Parameter_Type      => New_Occurrence_Of (Tag_Typ, Loc))),
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
            Adj_Call := Empty;
            Decl     := Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Adjust, True);

            if Is_Controlled (Tag_Typ) then
               Adj_Call :=
                 Make_Adjust_Call (
                   Obj_Ref => Make_Identifier (Loc, Name_V),
                   Typ     => Tag_Typ);
            end if;

            if No (Adj_Call) then
               Adj_Call := Make_Null_Statement (Loc);
            end if;

            Set_Handled_Statement_Sequence (Decl,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (Adj_Call)));

            Append_To (Res, Decl);
         end if;

         Fin_Call := Empty;
         Decl     := Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Finalize, True);

         if Is_Controlled (Tag_Typ) then
            Fin_Call :=
              Make_Final_Call
                (Obj_Ref => Make_Identifier (Loc, Name_V),
                 Typ     => Tag_Typ);
         end if;

         if No (Fin_Call) then
            Fin_Call := Make_Null_Statement (Loc);
         end if;

         Set_Handled_Statement_Sequence (Decl,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List (Fin_Call)));

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

      if Is_Limited_Type (Typ) and then Is_Tagged_Type (Typ) then
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
      --  a visible Input).

      return
          (not Is_Limited_Type (Typ)
            or else Is_Interface (Typ)
            or else Has_Predefined_Or_Specified_Stream_Attribute)
        and then
          (Operation /= TSS_Stream_Input
            or else not Is_Abstract_Type (Typ)
            or else not Is_Derived_Type (Typ))
        and then not Has_Unknown_Discriminants (Typ)
        and then not Is_Concurrent_Interface (Typ)
        and then not Restriction_Active (No_Streams)
        and then not Restriction_Active (No_Dispatch)
        and then No (No_Tagged_Streams_Pragma (Typ))
        and then not No_Run_Time_Mode
        and then RTE_Available (RE_Tag)
        and then
          (not Restriction_Active (No_Default_Stream_Attributes)
             or else No (Type_Without_Stream_Operation (Typ)))
        and then RTE_Available (RE_Root_Stream_Type);
   end Stream_Operation_OK;

end Exp_Ch3;
