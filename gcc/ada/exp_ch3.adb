------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Aggr; use Exp_Aggr;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Disp; use Exp_Disp;
with Exp_Dist; use Exp_Dist;
with Exp_Smem; use Exp_Smem;
with Exp_Strm; use Exp_Strm;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Hostparm; use Hostparm;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Snames;   use Snames;
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

   procedure Build_Class_Wide_Master (T : Entity_Id);
   --  for access to class-wide limited types we must build a task master
   --  because some subsequent extension may add a task component. To avoid
   --  bringing in the tasking run-time whenever an access-to-class-wide
   --  limited type is used, we use the soft-link mechanism and add a level
   --  of indirection to calls to routines that manipulate Master_Ids.

   function Build_Discriminant_Formals
     (Rec_Id : Entity_Id;
      Use_Dl : Boolean) return List_Id;
   --  This function uses the discriminants of a type to build a list of
   --  formal parameters, used in the following function. If the flag Use_Dl
   --  is set, the list is built using the already defined discriminals
   --  of the type. Otherwise new identifiers are created, with the source
   --  names of the discriminants.

   procedure Build_Master_Renaming (N : Node_Id; T : Entity_Id);
   --  If the designated type of an access type is a task type or contains
   --  tasks, we make sure that a _Master variable is declared in the current
   --  scope, and then declare a renaming for it:
   --
   --    atypeM : Master_Id renames _Master;
   --
   --  where atyp is the name of the access type. This declaration is
   --  used when an allocator for the access type is expanded. The node N
   --  is the full declaration of the designated type that contains tasks.
   --  The renaming declaration is inserted before N, and after the Master
   --  declaration.

   procedure Build_Record_Init_Proc (N : Node_Id; Pe : Entity_Id);
   --  Build record initialization procedure. N is the type declaration
   --  node, and Pe is the corresponding entity for the record type.

   procedure Build_Slice_Assignment (Typ : Entity_Id);
   --  Build assignment procedure for one-dimensional arrays of controlled
   --  types. Other array and slice assignments are expanded in-line, but
   --  the code expansion for controlled components (when control actions
   --  are active) can lead to very large blocks that GCC3 handles poorly.

   procedure Build_Variant_Record_Equality (Typ  : Entity_Id);
   --  Create An Equality function for the non-tagged variant record 'Typ'
   --  and attach it to the TSS list

   procedure Check_Stream_Attributes (Typ : Entity_Id);
   --  Check that if a limited extension has a parent with user-defined
   --  stream attributes, any limited component of the extension also has
   --  the corresponding user-defined stream attributes.

   procedure Expand_Tagged_Root (T : Entity_Id);
   --  Add a field _Tag at the beginning of the record. This field carries
   --  the value of the access to the Dispatch table. This procedure is only
   --  called on root (non CPP_Class) types, the _Tag field being inherited
   --  by the descendants.

   procedure Expand_Record_Controller (T : Entity_Id);
   --  T must be a record type that Has_Controlled_Component. Add a field
   --  _controller of type Record_Controller or Limited_Record_Controller
   --  in the record T.

   procedure Freeze_Array_Type (N : Node_Id);
   --  Freeze an array type. Deals with building the initialization procedure,
   --  creating the packed array type for a packed array and also with the
   --  creation of the controlling procedures for the controlled case. The
   --  argument N is the N_Freeze_Entity node for the type.

   procedure Freeze_Enumeration_Type (N : Node_Id);
   --  Freeze enumeration type with non-standard representation. Builds the
   --  array and function needed to convert between enumeration pos and
   --  enumeration representation values. N is the N_Freeze_Entity node
   --  for the type.

   procedure Freeze_Record_Type (N : Node_Id);
   --  Freeze record type. Builds all necessary discriminant checking
   --  and other ancillary functions, and builds dispatch tables where
   --  needed. The argument N is the N_Freeze_Entity node. This processing
   --  applies only to E_Record_Type entities, not to class wide types,
   --  record subtypes, or private types.

   procedure Freeze_Stream_Operations (N : Node_Id; Typ : Entity_Id);
   --  Treat user-defined stream operations as renaming_as_body if the
   --  subprogram they rename is not frozen when the type is frozen.

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

   function Make_Eq_Case
     (E     : Entity_Id;
      CL    : Node_Id;
      Discr : Entity_Id := Empty) return List_Id;
   --  Building block for variant record equality. Defined to share the
   --  code between the tagged and non-tagged case. Given a Component_List
   --  node CL, it generates an 'if' followed by a 'case' statement that
   --  compares all components of local temporaries named X and Y (that
   --  are declared as formals at some upper level). E provides the Sloc to be
   --  used for the generated code. Discr is used as the case statement switch
   --  in the case of Unchecked_Union equality.

   function Make_Eq_If
     (E : Entity_Id;
      L : List_Id) return Node_Id;
   --  Building block for variant record equality. Defined to share the
   --  code between the tagged and non-tagged case. Given the list of
   --  components (or discriminants) L, it generates a return statement
   --  that compares all components of local temporaries named X and Y
   --  (that are declared as formals at some upper level). E provides the Sloc
   --  to be used for the generated code.

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Node_Id);
   --  Create a list with the specs of the predefined primitive operations.
   --  The following entries are present for all tagged types, and provide
   --  the results of the corresponding attribute applied to the object.
   --  Dispatching is required in general, since the result of the attribute
   --  will vary with the actual object subtype.
   --
   --     _alignment     provides result of 'Alignment attribute
   --     _size          provides result of 'Size attribute
   --     typSR          provides result of 'Read attribute
   --     typSW          provides result of 'Write attribute
   --     typSI          provides result of 'Input attribute
   --     typSO          provides result of 'Output attribute
   --
   --  The following entries are additionally present for non-limited
   --  tagged types, and implement additional dispatching operations
   --  for predefined operations:
   --
   --     _equality      implements "=" operator
   --     _assign        implements assignment operation
   --     typDF          implements deep finalization
   --     typDA          implements deep adust
   --
   --  The latter two are empty procedures unless the type contains some
   --  controlled components that require finalization actions (the deep
   --  in the name refers to the fact that the action applies to components).
   --
   --  The list is returned in Predef_List. The Parameter Renamed_Eq
   --  either returns the value Empty, or else the defining unit name
   --  for the predefined equality function in the case where the type
   --  has a primitive operation that is a renaming of predefined equality
   --  (but only if there is also an overriding user-defined equality
   --  function). The returned Renamed_Eq will be passed to the
   --  corresponding parameter of Predefined_Primitive_Bodies.

   function Has_New_Non_Standard_Rep (T : Entity_Id) return Boolean;
   --  returns True if there are representation clauses for type T that
   --  are not inherited. If the result is false, the init_proc and the
   --  discriminant_checking functions of the parent can be reused by
   --  a derived type.

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
      Renamed_Eq : Node_Id) return List_Id;
   --  Create the bodies of the predefined primitives that are described in
   --  Predefined_Primitive_Specs. When not empty, Renamed_Eq must denote
   --  the defining unit name of the type's predefined equality as returned
   --  by Make_Predefined_Primitive_Specs.

   function Predefined_Primitive_Freeze (Tag_Typ : Entity_Id) return List_Id;
   --  Freeze entities of all predefined primitive operations. This is needed
   --  because the bodies of these operations do not normally do any freezeing.

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

   --  This procedure attempts to define subtypes for discriminants that
   --  are more restrictive than those declared. Such a replacement is
   --  possible if we can demonstrate that values outside the restricted
   --  range would cause constraint errors in any case. The advantage of
   --  restricting the discriminant types in this way is tha the maximum
   --  size of the variant record can be calculated more conservatively.

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

            Tnn := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

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
      Loc        : constant Source_Ptr := Sloc (Nod);
      Comp_Type  : constant Entity_Id  := Component_Type (A_Type);
      Index_List : List_Id;
      Proc_Id    : Entity_Id;
      Body_Stmts : List_Id;

      function Init_Component return List_Id;
      --  Create one statement to initialize one array component, designated
      --  by a full set of indices.

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
             Prefix => Make_Identifier (Loc, Name_uInit),
             Expressions => Index_List);

         if Needs_Simple_Initialization (Comp_Type) then
            Set_Assignment_OK (Comp);
            return New_List (
              Make_Assignment_Statement (Loc,
                Name => Comp,
                Expression =>
                  Get_Simple_Init_Val
                    (Comp_Type, Loc, Component_Size (A_Type))));

         else
            return
              Build_Initialization_Call (Loc, Comp, Comp_Type, True, A_Type);
         end if;
      end Init_Component;

      ------------------------
      -- Init_One_Dimension --
      ------------------------

      function Init_One_Dimension (N : Int) return List_Id is
         Index      : Entity_Id;

      begin
         --  If the component does not need initializing, then there is nothing
         --  to do here, so we return a null body. This occurs when generating
         --  the dummy Init_Proc needed for Initialize_Scalars processing.

         if not Has_Non_Null_Base_Init_Proc (Comp_Type)
           and then not Needs_Simple_Initialization (Comp_Type)
           and then not Has_Task (Comp_Type)
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
                            Expressions => New_List (
                              Make_Integer_Literal (Loc, N))))),
                Statements =>  Init_One_Dimension (N + 1)));
         end if;
      end Init_One_Dimension;

   --  Start of processing for Build_Array_Init_Proc

   begin
      if Suppress_Init_Proc (A_Type) then
         return;
      end if;

      Index_List := New_List;

      --  We need an initialization procedure if any of the following is true:

      --    1. The component type has an initialization procedure
      --    2. The component type needs simple initialization
      --    3. Tasks are present
      --    4. The type is marked as a publc entity

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

      if Has_Non_Null_Base_Init_Proc (Comp_Type)
        or else Needs_Simple_Initialization (Comp_Type)
        or else Has_Task (Comp_Type)
        or else (not Restriction_Active (No_Initialize_Scalars)
                   and then Is_Public (A_Type)
                   and then Root_Type (A_Type) /= Standard_String
                   and then Root_Type (A_Type) /= Standard_Wide_String
                   and then Root_Type (A_Type) /= Standard_Wide_Wide_String)
      then
         Proc_Id :=
           Make_Defining_Identifier (Loc, Make_Init_Proc_Name (A_Type));

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
         --  difficulties in interunit inlining, and furthermore there is
         --  in any case no point in inlining such complex init procs.

         if not Has_Task (Proc_Id)
           and then not Controlled_Type (Proc_Id)
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
           and then Nkind (First (Body_Stmts)) = N_Null_Statement
         then
            Set_Is_Null_Init_Proc (Proc_Id);
         end if;
      end if;
   end Build_Array_Init_Proc;

   -----------------------------
   -- Build_Class_Wide_Master --
   -----------------------------

   procedure Build_Class_Wide_Master (T : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (T);
      M_Id : Entity_Id;
      Decl : Node_Id;
      P    : Node_Id;

   begin
      --  Nothing to do if there is no task hierarchy

      if Restriction_Active (No_Task_Hierarchy) then
         return;
      end if;

      --  Nothing to do if we already built a master entity for this scope

      if not Has_Master_Entity (Scope (T)) then

         --  first build the master entity
         --    _Master : constant Master_Id := Current_Master.all;
         --  and insert it just before the current declaration

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uMaster),
             Constant_Present => True,
             Object_Definition => New_Reference_To (Standard_Integer, Loc),
             Expression =>
               Make_Explicit_Dereference (Loc,
                 New_Reference_To (RTE (RE_Current_Master), Loc)));

         P := Parent (T);
         Insert_Before (P, Decl);
         Analyze (Decl);
         Set_Has_Master_Entity (Scope (T));

         --  Now mark the containing scope as a task master

         while Nkind (P) /= N_Compilation_Unit loop
            P := Parent (P);

            --  If we fall off the top, we are at the outer level, and the
            --  environment task is our effective master, so nothing to mark.

            if Nkind (P) = N_Task_Body
              or else Nkind (P) = N_Block_Statement
              or else Nkind (P) = N_Subprogram_Body
            then
               Set_Is_Task_Master (P, True);
               exit;
            end if;
         end loop;
      end if;

      --  Now define the renaming of the master_id

      M_Id :=
        Make_Defining_Identifier (Loc,
          New_External_Name (Chars (T), 'M'));

      Decl :=
        Make_Object_Renaming_Declaration (Loc,
          Defining_Identifier => M_Id,
          Subtype_Mark => New_Reference_To (Standard_Integer, Loc),
          Name => Make_Identifier (Loc, Name_uMaster));
      Insert_Before (Parent (T), Decl);
      Analyze (Decl);

      Set_Master_Id (T, M_Id);

   exception
      when RE_Not_Available =>
         return;
   end Build_Class_Wide_Master;

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
      --  Build a case statement containing only two alternatives. The
      --  first alternative corresponds exactly to the discrete choices
      --  given on the variant with contains the components that we are
      --  generating the checks for. If the discriminant is one of these
      --  return False. The second alternative is an OTHERS choice that
      --  will return True indicating the discriminant did not match.

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

         --  Replace the discriminant which controls the variant, with the
         --  name of the formal of the checking function.

         Set_Expression (Case_Node,
           Make_Identifier (Loc, Chars (Case_Id)));

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
                 Make_Return_Statement (Loc,
                   Expression =>
                     Make_Function_Call (Loc,
                       Name =>
                         New_Reference_To (Enclosing_Func_Id,  Loc),
                       Parameter_Associations =>
                         Actuals_List));

            else
               Return_Node :=
                 Make_Return_Statement (Loc,
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
           Make_Return_Statement (Loc,
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
         Set_Subtype_Mark (Spec_Node,
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
         --  Build the discriminant checking function for each variant, label
         --  all components of that variant with the function's name.

         Discr_Name := Entity (Name (Variant_Part_Node));
         Variant := First_Non_Pragma (Variants (Variant_Part_Node));

         while Present (Variant) loop
            Func_Id := Build_Dcheck_Function (Discr_Name, Variant);
            Component_List_Node := Component_List (Variant);

            if not Null_Present (Component_List_Node) then
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
      Param_Spec_Node : Node_Id;

   begin
      if Has_Discriminants (Rec_Id) then
         D := First_Discriminant (Rec_Id);
         while Present (D) loop
            Loc := Sloc (D);

            if Use_Dl then
               Formal := Discriminal (D);
            else
               Formal := Make_Defining_Identifier (Loc, Chars (D));
            end if;

            Param_Spec_Node :=
              Make_Parameter_Specification (Loc,
                  Defining_Identifier => Formal,
                Parameter_Type =>
                  New_Reference_To (Etype (D), Loc));
            Append (Param_Spec_Node, Parameter_List);
            Next_Discriminant (D);
         end loop;
      end if;

      return Parameter_List;
   end Build_Discriminant_Formals;

   -------------------------------
   -- Build_Initialization_Call --
   -------------------------------

   --  References to a discriminant inside the record type declaration
   --  can appear either in the subtype_indication to constrain a
   --  record or an array, or as part of a larger expression given for
   --  the initial value of a component. In both of these cases N appears
   --  in the record initialization procedure and needs to be replaced by
   --  the formal parameter of the initialization procedure which
   --  corresponds to that discriminant.

   --  In the example below, references to discriminants D1 and D2 in proc_1
   --  are replaced by references to formals with the same name
   --  (discriminals)

   --  A similar replacement is done for calls to any record
   --  initialization procedure for any components that are themselves
   --  of a record type.

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
      With_Default_Init : Boolean := False) return List_Id
   is
      First_Arg      : Node_Id;
      Args           : List_Id;
      Decls          : List_Id;
      Decl           : Node_Id;
      Discr          : Entity_Id;
      Arg            : Node_Id;
      Proc           : constant Entity_Id := Base_Init_Proc (Typ);
      Init_Type      : constant Entity_Id := Etype (First_Formal (Proc));
      Full_Init_Type : constant Entity_Id := Underlying_Type (Init_Type);
      Res            : constant List_Id   := New_List;
      Full_Type      : Entity_Id := Typ;
      Controller_Typ : Entity_Id;

   begin
      --  Nothing to do if the Init_Proc is null, unless Initialize_Scalars
      --  is active (in which case we make the call anyway, since in the
      --  actual compiled client it may be non null).

      if Is_Null_Init_Proc (Proc) and then not Init_Or_Norm_Scalars then
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

            --  See comments in System.Tasking.Initialization.Init_RTS
            --  for the value 3 (should be rtsfindable constant ???)

            Append_To (Args, Make_Integer_Literal (Loc, 3));
         else
            Append_To (Args, Make_Identifier (Loc, Name_uMaster));
         end if;

         Append_To (Args, Make_Identifier (Loc, Name_uChain));

         --  Ada 2005 (AI-287): In case of default initialized components
         --  with tasks, we generate a null string actual parameter.
         --  This is just a workaround that must be improved later???

         if With_Default_Init then
            Append_To (Args,
              Make_String_Literal (Loc,
                Strval => ""));

         else
            Decls := Build_Task_Image_Decls (Loc, Id_Ref, Enclos_Type);
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
            --  for the corresponding record is being called. Use that
            --  type directly to find the discriminant value, to handle
            --  properly intervening renamed discriminants.

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

            if In_Init_Proc then

               --  Replace any possible references to the discriminant in the
               --  call to the record initialization procedure with references
               --  to the appropriate formal parameter.

               if Nkind (Arg) = N_Identifier
                  and then Ekind (Entity (Arg)) = E_Discriminant
               then
                  Arg := New_Reference_To (Discriminal (Entity (Arg)), Loc);

               --  Case of access discriminants. We replace the reference
               --  to the type by a reference to the actual object

               elsif Nkind (Arg) = N_Attribute_Reference
                 and then Is_Access_Type (Etype (Arg))
                 and then Is_Entity_Name (Prefix (Arg))
                 and then Is_Type (Entity (Prefix (Arg)))
               then
                  Arg :=
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Copy (Prefix (Id_Ref)),
                      Attribute_Name => Name_Unrestricted_Access);

               --  Otherwise make a copy of the default expression. Note
               --  that we use the current Sloc for this, because we do not
               --  want the call to appear to be at the declaration point.
               --  Within the expression, replace discriminants with their
               --  discriminals.

               else
                  Arg :=
                    New_Copy_Tree (Arg, Map => Discr_Map, New_Sloc => Loc);
               end if;

            else
               if Is_Constrained (Full_Type) then
                  Arg := Duplicate_Subexpr_No_Checks (Arg);
               else
                  --  The constraints come from the discriminant default
                  --  exps, they must be reevaluated, so we use New_Copy_Tree
                  --  but we ensure the proper Sloc (for any embedded calls).

                  Arg := New_Copy_Tree (Arg, New_Sloc => Loc);
               end if;
            end if;

            --  Ada 2005 (AI-287) In case of default initialized components,
            --  we need to generate the corresponding selected component node
            --  to access the discriminant value. In other cases this is not
            --  required because we are inside the init proc and we use the
            --  corresponding formal.

            if With_Default_Init
              and then Nkind (Id_Ref) = N_Selected_Component
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
      end if;

      Append_To (Res,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Proc, Loc),
          Parameter_Associations => Args));

      if Controlled_Type (Typ)
        and then Nkind (Id_Ref) = N_Selected_Component
      then
         if Chars (Selector_Name (Id_Ref)) /= Name_uParent then
            Append_List_To (Res,
              Make_Init_Call (
                Ref         => New_Copy_Tree (First_Arg),
                Typ         => Typ,
                Flist_Ref   =>
                  Find_Final_List (Typ, New_Copy_Tree (First_Arg)),
                With_Attach => Make_Integer_Literal (Loc, 1)));

         --  If the enclosing type is an extension with new controlled
         --  components, it has his own record controller. If the parent
         --  also had a record controller, attach it to the new one.
         --  Build_Init_Statements relies on the fact that in this specific
         --  case the last statement of the result is the attach call to
         --  the controller. If this is changed, it must be synchronized.

         elsif Present (Enclos_Type)
           and then Has_New_Controlled_Component (Enclos_Type)
           and then Has_Controlled_Component (Typ)
         then
            if Is_Return_By_Reference_Type (Typ) then
               Controller_Typ := RTE (RE_Limited_Record_Controller);
            else
               Controller_Typ := RTE (RE_Record_Controller);
            end if;

            Append_List_To (Res,
              Make_Init_Call (
                Ref       =>
                  Make_Selected_Component (Loc,
                    Prefix        => New_Copy_Tree (First_Arg),
                    Selector_Name => Make_Identifier (Loc, Name_uController)),
                Typ       => Controller_Typ,
                Flist_Ref => Find_Final_List (Typ, New_Copy_Tree (First_Arg)),
                With_Attach => Make_Integer_Literal (Loc, 1)));
         end if;
      end if;

      return Res;

   exception
      when RE_Not_Available =>
         return Empty_List;
   end Build_Initialization_Call;

   ---------------------------
   -- Build_Master_Renaming --
   ---------------------------

   procedure Build_Master_Renaming (N : Node_Id; T : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      M_Id : Entity_Id;
      Decl : Node_Id;

   begin
      --  Nothing to do if there is no task hierarchy

      if Restriction_Active (No_Task_Hierarchy) then
         return;
      end if;

      M_Id :=
        Make_Defining_Identifier (Loc,
          New_External_Name (Chars (T), 'M'));

      Decl :=
        Make_Object_Renaming_Declaration (Loc,
          Defining_Identifier => M_Id,
          Subtype_Mark => New_Reference_To (RTE (RE_Master_Id), Loc),
          Name => Make_Identifier (Loc, Name_uMaster));
      Insert_Before (N, Decl);
      Analyze (Decl);

      Set_Master_Id (T, M_Id);

   exception
      when RE_Not_Available =>
         return;
   end Build_Master_Renaming;

   ----------------------------
   -- Build_Record_Init_Proc --
   ----------------------------

   procedure Build_Record_Init_Proc (N : Node_Id; Pe : Entity_Id) is
      Loc         : Source_Ptr := Sloc (N);
      Discr_Map   : constant Elist_Id := New_Elmt_List;
      Proc_Id     : Entity_Id;
      Rec_Type    : Entity_Id;
      Set_Tag     : Entity_Id := Empty;

      function Build_Assignment (Id : Entity_Id; N : Node_Id) return List_Id;
      --  Build a assignment statement node which assigns to record
      --  component its default expression if defined. The left hand side
      --  of the assignment is marked Assignment_OK so that initialization
      --  of limited private records works correctly, Return also the
      --  adjustment call for controlled objects

      procedure Build_Discriminant_Assignments (Statement_List : List_Id);
      --  If the record has discriminants, adds assignment statements to
      --  statement list to initialize the discriminant values from the
      --  arguments of the initialization procedure.

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id;
      --  Build a list representing a sequence of statements which initialize
      --  components of the given component list. This may involve building
      --  case statements for the variant parts.

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
      --       procedure _init_proc(X : D; D1 : Integer) is
      --       begin
      --          _init_proc( R(X), 1, D1);
      --       end _init_proc;
      --
      --  This function builds the call statement in this _init_proc.

      procedure Build_Init_Procedure;
      --  Build the tree corresponding to the procedure specification and body
      --  of the initialization procedure (by calling all the preceding
      --  auxiliary routines), and install it as the _init TSS.

      procedure Build_Record_Checks (S : Node_Id; Check_List : List_Id);
      --  Add range checks to components of disciminated records. S is a
      --  subtype indication of a record component. Check_List is a list
      --  to which the check actions are appended.

      function Component_Needs_Simple_Initialization
        (T : Entity_Id) return Boolean;
      --  Determines if a component needs simple initialization, given its
      --  type T. This is the same as Needs_Simple_Initialization except
      --  for the following difference: the types Tag and Vtable_Ptr, which
      --  are access types which would normally require simple initialization
      --  to null, do not require initialization as components, since they
      --  are explicitly initialized by other means.

      procedure Constrain_Array
        (SI         : Node_Id;
         Check_List : List_Id);
      --  Called from Build_Record_Checks.
      --  Apply a list of index constraints to an unconstrained array type.
      --  The first parameter is the entity for the resulting subtype.
      --  Check_List is a list to which the check actions are appended.

      procedure Constrain_Index
        (Index      : Node_Id;
         S          : Node_Id;
         Check_List : List_Id);
      --  Called from Build_Record_Checks.
      --  Process an index constraint in a constrained array declaration.
      --  The constraint can be a subtype name, or a range with or without
      --  an explicit subtype mark. The index is the corresponding index of the
      --  unconstrained array. S is the range expression. Check_List is a list
      --  to which the check actions are appended.

      function Parent_Subtype_Renaming_Discrims return Boolean;
      --  Returns True for base types N that rename discriminants, else False

      function Requires_Init_Proc (Rec_Id : Entity_Id) return Boolean;
      --  Determines whether a record initialization procedure needs to be
      --  generated for the given record type.

      ----------------------
      -- Build_Assignment --
      ----------------------

      function Build_Assignment (Id : Entity_Id; N : Node_Id) return List_Id is
         Exp  : Node_Id := N;
         Lhs  : Node_Id;
         Typ  : constant Entity_Id := Underlying_Type (Etype (Id));
         Kind : Node_Kind := Nkind (N);
         Res  : List_Id;

      begin
         Loc := Sloc (N);
         Lhs :=
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Selector_Name => New_Occurrence_Of (Id, Loc));
         Set_Assignment_OK (Lhs);

         --  Case of an access attribute applied to the current instance.
         --  Replace the reference to the type by a reference to the actual
         --  object. (Note that this handles the case of the top level of
         --  the expression being given by such an attribute, but does not
         --  cover uses nested within an initial value expression. Nested
         --  uses are unlikely to occur in practice, but are theoretically
         --  possible. It is not clear how to handle them without fully
         --  traversing the expression. ???

         if Kind = N_Attribute_Reference
           and then (Attribute_Name (N) = Name_Unchecked_Access
                       or else
                     Attribute_Name (N) = Name_Unrestricted_Access)
           and then Is_Entity_Name (Prefix (N))
           and then Is_Type (Entity (Prefix (N)))
           and then Entity (Prefix (N)) = Rec_Type
         then
            Exp :=
              Make_Attribute_Reference (Loc,
                Prefix         => Make_Identifier (Loc, Name_uInit),
                Attribute_Name => Name_Unrestricted_Access);
         end if;

         --  Ada 2005 (AI-231): Generate conversion to the null-excluding
         --  type to force the corresponding run-time check.

         if Ada_Version >= Ada_05
           and then Can_Never_Be_Null (Etype (Id))  -- Lhs
           and then Present (Etype (Exp))
           and then not Can_Never_Be_Null (Etype (Exp))
         then
            Rewrite (Exp, Convert_To (Etype (Id), Relocate_Node (Exp)));
            Analyze_And_Resolve (Exp, Etype (Id));
         end if;

         --  Take a copy of Exp to ensure that later copies of this
         --  component_declaration in derived types see the original tree,
         --  not a node rewritten during expansion of the init_proc.

         Exp := New_Copy_Tree (Exp);

         Res := New_List (
           Make_Assignment_Statement (Loc,
             Name       => Lhs,
             Expression => Exp));

         Set_No_Ctrl_Actions (First (Res));

         --  Adjust the tag if tagged (because of possible view conversions).
         --  Suppress the tag adjustment when Java_VM because JVM tags are
         --  represented implicitly in objects.

         if Is_Tagged_Type (Typ) and then not Java_VM then
            Append_To (Res,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix =>  New_Copy_Tree (Lhs),
                    Selector_Name =>
                      New_Reference_To (First_Tag_Component (Typ), Loc)),

                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Reference_To
                      (Node (First_Elmt (Access_Disp_Table (Typ))), Loc))));
         end if;

         --  Adjust the component if controlled except if it is an
         --  aggregate that will be expanded inline

         if Kind = N_Qualified_Expression then
            Kind := Nkind (Expression (N));
         end if;

         if Controlled_Type (Typ)
         and then not (Kind = N_Aggregate or else Kind = N_Extension_Aggregate)
         then
            Append_List_To (Res,
              Make_Adjust_Call (
               Ref          => New_Copy_Tree (Lhs),
               Typ          => Etype (Id),
               Flist_Ref    =>
                 Find_Final_List (Etype (Id), New_Copy_Tree (Lhs)),
               With_Attach  => Make_Integer_Literal (Loc, 1)));
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
         D         : Entity_Id;
         Is_Tagged : constant Boolean := Is_Tagged_Type (Rec_Type);

      begin
         if Has_Discriminants (Rec_Type)
           and then not Is_Unchecked_Union (Rec_Type)
         then
            D := First_Discriminant (Rec_Type);

            while Present (D) loop
               --  Don't generate the assignment for discriminants in derived
               --  tagged types if the discriminant is a renaming of some
               --  ancestor discriminant.  This initialization will be done
               --  when initializing the _parent field of the derived record.

               if Is_Tagged and then
                 Present (Corresponding_Discriminant (D))
               then
                  null;

               else
                  Loc := Sloc (D);
                  Append_List_To (Statement_List,
                    Build_Assignment (D,
                      New_Reference_To (Discriminal (D), Loc)));
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

         Parent_Discr : Entity_Id;
         First_Arg    : Node_Id;
         Args         : List_Id;
         Arg          : Node_Id;
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

               --  See comments in System.Tasking.Initialization.Init_RTS
               --  for the value 3.

               Append_To (Args, Make_Integer_Literal (Loc, 3));
            else
               Append_To (Args, Make_Identifier (Loc, Name_uMaster));
            end if;

            Append_To (Args, Make_Identifier (Loc, Name_uChain));
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
                  Discr_Value : Elmt_Id :=
                                  First_Elmt
                                    (Stored_Constraint (Rec_Type));

                  Discr       : Entity_Id :=
                                  First_Stored_Discriminant (Uparent_Type);
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
               --  to the type by a reference to the actual object

--     ??? why is this code deleted without comment

--               elsif Nkind (Arg) = N_Attribute_Reference
--                 and then Is_Entity_Name (Prefix (Arg))
--                 and then Is_Type (Entity (Prefix (Arg)))
--               then
--                  Append_To (Args,
--                    Make_Attribute_Reference (Loc,
--                      Prefix         => New_Copy (Prefix (Id_Ref)),
--                      Attribute_Name => Name_Unrestricted_Access));

               else
                  Append_To (Args, New_Copy (Arg));
               end if;

               Next_Discriminant (Parent_Discr);
            end loop;
         end if;

         Res :=
            New_List (
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Parent_Proc, Loc),
                Parameter_Associations => Args));

         return Res;
      end Build_Init_Call_Thru;

      --------------------------
      -- Build_Init_Procedure --
      --------------------------

      procedure Build_Init_Procedure is
         Body_Node             : Node_Id;
         Handled_Stmt_Node     : Node_Id;
         Parameters            : List_Id;
         Proc_Spec_Node        : Node_Id;
         Body_Stmts            : List_Id;
         Record_Extension_Node : Node_Id;
         Init_Tag              : Node_Id;

      begin
         Body_Stmts := New_List;
         Body_Node := New_Node (N_Subprogram_Body, Loc);

         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Make_Init_Proc_Name (Rec_Type));
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

         if Is_Tagged_Type (Rec_Type)
           and then not Is_CPP_Class (Rec_Type)
         then
            Set_Tag :=
                  Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

            Append_To (Parameters,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Set_Tag,
                Parameter_Type => New_Occurrence_Of (Standard_Boolean, Loc),
                Expression => New_Occurrence_Of (Standard_True, Loc)));
         end if;

         Set_Parameter_Specifications (Proc_Spec_Node, Parameters);
         Set_Specification (Body_Node, Proc_Spec_Node);
         Set_Declarations (Body_Node, New_List);

         if Parent_Subtype_Renaming_Discrims then

            --  N is a Derived_Type_Definition that renames the parameters
            --  of the ancestor type.  We init it by expanding our discrims
            --  and call the ancestor _init_proc with a type-converted object

            Append_List_To (Body_Stmts,
              Build_Init_Call_Thru (Parameters));

         elsif Nkind (Type_Definition (N)) = N_Record_Definition then
            Build_Discriminant_Assignments (Body_Stmts);

            if not Null_Present (Type_Definition (N)) then
               Append_List_To (Body_Stmts,
                 Build_Init_Statements (
                   Component_List (Type_Definition (N))));
            end if;

         else
            --  N is a Derived_Type_Definition with a possible non-empty
            --  extension. The initialization of a type extension consists
            --  in the initialization of the components in the extension.

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

         --  The assignement corresponds to the code:

         --     _Init._Tag := Typ'Tag;

         --  Suppress the tag assignment when Java_VM because JVM tags are
         --  represented implicitly in objects.

         if Is_Tagged_Type (Rec_Type)
           and then not Is_CPP_Class (Rec_Type)
           and then not Java_VM
         then
            Init_Tag :=
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => Make_Identifier (Loc, Name_uInit),
                    Selector_Name =>
                      New_Reference_To (First_Tag_Component (Rec_Type), Loc)),

                Expression =>
                  New_Reference_To
                    (Node (First_Elmt (Access_Disp_Table (Rec_Type))), Loc));

            --  The tag must be inserted before the assignments to other
            --  components,  because the initial value of the component may
            --  depend ot the tag (eg. through a dispatching operation on
            --  an access to the current type). The tag assignment is not done
            --  when initializing the parent component of a type extension,
            --  because in that case the tag is set in the extension.
            --  Extensions of imported C++ classes add a final complication,
            --  because we cannot inhibit tag setting in the constructor for
            --  the parent. In that case we insert the tag initialization
            --  after the calls to initialize the parent.

            Init_Tag :=
              Make_If_Statement (Loc,
                Condition => New_Occurrence_Of (Set_Tag, Loc),
                Then_Statements => New_List (Init_Tag));

            if not Is_CPP_Class (Etype (Rec_Type)) then
               Prepend_To (Body_Stmts, Init_Tag);

            else
               declare
                  Nod : Node_Id := First (Body_Stmts);

               begin
                  --  We assume the first init_proc call is for the parent

                  while Present (Next (Nod))
                    and then (Nkind (Nod) /= N_Procedure_Call_Statement
                               or else not Is_Init_Proc (Name (Nod)))
                  loop
                     Nod := Next (Nod);
                  end loop;

                  Insert_After (Nod, Init_Tag);
               end;
            end if;
         end if;

         Handled_Stmt_Node := New_Node (N_Handled_Sequence_Of_Statements, Loc);
         Set_Statements (Handled_Stmt_Node, Body_Stmts);
         Set_Exception_Handlers (Handled_Stmt_Node, No_List);
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
           and then Nkind (First (Body_Stmts)) = N_Null_Statement
         then
            Set_Is_Null_Init_Proc (Proc_Id);
         end if;
      end Build_Init_Procedure;

      ---------------------------
      -- Build_Init_Statements --
      ---------------------------

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id is
         Check_List     : constant List_Id := New_List;
         Alt_List       : List_Id;
         Statement_List : List_Id;
         Stmts          : List_Id;

         Per_Object_Constraint_Components : Boolean;

         Decl     : Node_Id;
         Variant  : Node_Id;

         Id  : Entity_Id;
         Typ : Entity_Id;

         function Has_Access_Constraint (E : Entity_Id) return Boolean;
         --  Components with access discriminants that depend on the current
         --  instance must be initialized after all other components.

         ---------------------------
         -- Has_Access_Constraint --
         ---------------------------

         function Has_Access_Constraint (E : Entity_Id) return Boolean is
            Disc : Entity_Id;
            T    : constant Entity_Id := Etype (E);

         begin
            if Has_Per_Object_Constraint (E)
              and then Has_Discriminants (T)
            then
               Disc := First_Discriminant (T);
               while Present (Disc) loop
                  if Is_Access_Type (Etype (Disc)) then
                     return True;
                  end if;

                  Next_Discriminant (Disc);
               end loop;

               return False;
            else
               return False;
            end if;
         end Has_Access_Constraint;

      --  Start of processing for Build_Init_Statements

      begin
         if Null_Present (Comp_List) then
            return New_List (Make_Null_Statement (Loc));
         end if;

         Statement_List := New_List;

         --  Loop through components, skipping pragmas, in 2 steps. The first
         --  step deals with regular components. The second step deals with
         --  components have per object constraints, and no explicit initia-
         --  lization.

         Per_Object_Constraint_Components := False;

         --  First step : regular components

         Decl := First_Non_Pragma (Component_Items (Comp_List));
         while Present (Decl) loop
            Loc := Sloc (Decl);
            Build_Record_Checks
              (Subtype_Indication (Component_Definition (Decl)), Check_List);

            Id := Defining_Identifier (Decl);
            Typ := Etype (Id);

            if Has_Access_Constraint (Id)
              and then No (Expression (Decl))
            then
               --  Skip processing for now and ask for a second pass

               Per_Object_Constraint_Components := True;

            else
               --  Case of explicit initialization

               if Present (Expression (Decl)) then
                  Stmts := Build_Assignment (Id, Expression (Decl));

               --  Case of composite component with its own Init_Proc

               elsif Has_Non_Null_Base_Init_Proc (Typ) then
                  Stmts :=
                    Build_Initialization_Call
                      (Loc,
                       Make_Selected_Component (Loc,
                         Prefix => Make_Identifier (Loc, Name_uInit),
                         Selector_Name => New_Occurrence_Of (Id, Loc)),
                       Typ,
                       True,
                       Rec_Type,
                       Discr_Map => Discr_Map);

               --  Case of component needing simple initialization

               elsif Component_Needs_Simple_Initialization (Typ) then
                  Stmts :=
                    Build_Assignment
                      (Id, Get_Simple_Init_Val (Typ, Loc, Esize (Id)));

               --  Nothing needed for this case

               else
                  Stmts := No_List;
               end if;

               if Present (Check_List) then
                  Append_List_To (Statement_List, Check_List);
               end if;

               if Present (Stmts) then

                  --  Add the initialization of the record controller before
                  --  the _Parent field is attached to it when the attachment
                  --  can occur. It does not work to simply initialize the
                  --  controller first: it must be initialized after the parent
                  --  if the parent holds discriminants that can be used
                  --  to compute the offset of the controller. We assume here
                  --  that the last statement of the initialization call is the
                  --  attachement of the parent (see Build_Initialization_Call)

                  if Chars (Id) = Name_uController
                    and then Rec_Type /= Etype (Rec_Type)
                    and then Has_Controlled_Component (Etype (Rec_Type))
                    and then Has_New_Controlled_Component (Rec_Type)
                  then
                     Insert_List_Before (Last (Statement_List), Stmts);
                  else
                     Append_List_To (Statement_List, Stmts);
                  end if;
               end if;
            end if;

            Next_Non_Pragma (Decl);
         end loop;

         if Per_Object_Constraint_Components then

            --  Second pass: components with per-object constraints

            Decl := First_Non_Pragma (Component_Items (Comp_List));

            while Present (Decl) loop
               Loc := Sloc (Decl);
               Id := Defining_Identifier (Decl);
               Typ := Etype (Id);

               if Has_Access_Constraint (Id)
                 and then No (Expression (Decl))
               then
                  if Has_Non_Null_Base_Init_Proc (Typ) then
                     Append_List_To (Statement_List,
                       Build_Initialization_Call (Loc,
                         Make_Selected_Component (Loc,
                           Prefix => Make_Identifier (Loc, Name_uInit),
                           Selector_Name => New_Occurrence_Of (Id, Loc)),
                         Typ, True, Rec_Type, Discr_Map => Discr_Map));

                  elsif Component_Needs_Simple_Initialization (Typ) then
                     Append_List_To (Statement_List,
                       Build_Assignment
                         (Id, Get_Simple_Init_Val (Typ, Loc, Esize (Id))));
                  end if;
               end if;

               Next_Non_Pragma (Decl);
            end loop;
         end if;

         --  Process the variant part

         if Present (Variant_Part (Comp_List)) then
            Alt_List := New_List;
            Variant := First_Non_Pragma (Variants (Variant_Part (Comp_List)));

            while Present (Variant) loop
               Loc := Sloc (Variant);
               Append_To (Alt_List,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices =>
                     New_Copy_List (Discrete_Choices (Variant)),
                   Statements =>
                     Build_Init_Statements (Component_List (Variant))));

               Next_Non_Pragma (Variant);
            end loop;

            --  The expression of the case statement which is a reference
            --  to one of the discriminants is replaced by the appropriate
            --  formal parameter of the initialization procedure.

            Append_To (Statement_List,
              Make_Case_Statement (Loc,
                Expression =>
                  New_Reference_To (Discriminal (
                    Entity (Name (Variant_Part (Comp_List)))), Loc),
                Alternatives => Alt_List));
         end if;

         --  For a task record type, add the task create call and calls
         --  to bind any interrupt (signal) entries.

         if Is_Task_Record_Type (Rec_Type) then

            --  In the case of the restricted run time the ATCB has already
            --  been preallocated.

            if Restricted_Profile then
               Append_To (Statement_List,
                 Make_Assignment_Statement (Loc,
                   Name => Make_Selected_Component (Loc,
                     Prefix => Make_Identifier (Loc, Name_uInit),
                     Selector_Name => Make_Identifier (Loc, Name_uTask_Id)),
                   Expression => Make_Attribute_Reference (Loc,
                     Prefix =>
                       Make_Selected_Component (Loc,
                         Prefix => Make_Identifier (Loc, Name_uInit),
                         Selector_Name =>
                           Make_Identifier (Loc, Name_uATCB)),
                     Attribute_Name => Name_Unchecked_Access)));
            end if;

            Append_To (Statement_List, Make_Task_Create_Call (Rec_Type));

            declare
               Task_Type : constant Entity_Id :=
                             Corresponding_Concurrent_Type (Rec_Type);
               Task_Decl : constant Node_Id := Parent (Task_Type);
               Task_Def  : constant Node_Id := Task_Definition (Task_Decl);
               Vis_Decl  : Node_Id;
               Ent       : Entity_Id;

            begin
               if Present (Task_Def) then
                  Vis_Decl := First (Visible_Declarations (Task_Def));
                  while Present (Vis_Decl) loop
                     Loc := Sloc (Vis_Decl);

                     if Nkind (Vis_Decl) = N_Attribute_Definition_Clause then
                        if Get_Attribute_Id (Chars (Vis_Decl)) =
                                                       Attribute_Address
                        then
                           Ent := Entity (Name (Vis_Decl));

                           if Ekind (Ent) = E_Entry then
                              Append_To (Statement_List,
                                Make_Procedure_Call_Statement (Loc,
                                  Name => New_Reference_To (
                                    RTE (RE_Bind_Interrupt_To_Entry), Loc),
                                  Parameter_Associations => New_List (
                                    Make_Selected_Component (Loc,
                                      Prefix =>
                                        Make_Identifier (Loc, Name_uInit),
                                      Selector_Name =>
                                        Make_Identifier (Loc, Name_uTask_Id)),
                                    Entry_Index_Expression (
                                      Loc, Ent, Empty, Task_Type),
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
            Append_List_To (Statement_List,
              Make_Initialize_Protection (Rec_Type));
         end if;

         --  If no initializations when generated for component declarations
         --  corresponding to this Statement_List, append a null statement
         --  to the Statement_List to make it a valid Ada tree.

         if Is_Empty_List (Statement_List) then
            Append (New_Node (N_Null_Statement, Loc), Statement_List);
         end if;

         return Statement_List;

      exception
         when RE_Not_Available =>
         return Empty_List;
      end Build_Init_Statements;

      -------------------------
      -- Build_Record_Checks --
      -------------------------

      procedure Build_Record_Checks (S : Node_Id; Check_List : List_Id) is
         Subtype_Mark_Id : Entity_Id;

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
             and then not Is_RTE (T, RE_Vtable_Ptr);
      end Component_Needs_Simple_Initialization;

      ---------------------
      -- Constrain_Array --
      ---------------------

      procedure Constrain_Array
        (SI          : Node_Id;
         Check_List  : List_Id)
      is
         C                     : constant Node_Id := Constraint (SI);
         Number_Of_Constraints : Nat := 0;
         Index                 : Node_Id;
         S, T                  : Entity_Id;

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

      --------------------------------------
      -- Parent_Subtype_Renaming_Discrims --
      --------------------------------------

      function Parent_Subtype_Renaming_Discrims return Boolean is
         De : Entity_Id;
         Dp : Entity_Id;

      begin
         if Base_Type (Pe) /= Pe then
            return False;
         end if;

         if Etype (Pe) = Pe
           or else not Has_Discriminants (Pe)
           or else Is_Constrained (Pe)
           or else Is_Tagged_Type (Pe)
         then
            return False;
         end if;

         --  If there are no explicit stored discriminants we have inherited
         --  the root type discriminants so far, so no renamings occurred.

         if First_Discriminant (Pe) = First_Stored_Discriminant (Pe) then
            return False;
         end if;

         --  Check if we have done some trivial renaming of the parent
         --  discriminants, i.e. someting like
         --
         --    type DT (X1,X2: int) is new PT (X1,X2);

         De := First_Discriminant (Pe);
         Dp := First_Discriminant (Etype (Pe));

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

         if Suppress_Init_Proc (Rec_Id) then
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
         --     that types Tag and Vtable_Ptr are excluded, since fields
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

         --  Note: the reason we exclude the CPP_Class case is ???

         if Is_CPP_Class (Rec_Id) then
            return False;

         elsif not Restriction_Active (No_Initialize_Scalars)
           and then Is_Public (Rec_Id)
         then
            return True;

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

      --  If there are discriminants, build the discriminant map to replace
      --  discriminants by their discriminals in complex bound expressions.
      --  These only arise for the corresponding records of protected types.

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
         Build_Init_Procedure;
         Set_Is_Public (Proc_Id, Is_Public (Pe));

         --  The initialization of protected records is not worth inlining.
         --  In addition, when compiled for another unit for inlining purposes,
         --  it may make reference to entities that have not been elaborated
         --  yet. The initialization of controlled records contains a nested
         --  clean-up procedure that makes it impractical to inline as well,
         --  and leads to undefined symbols if inlined in a different unit.
         --  Similar considerations apply to task types.

         if not Is_Concurrent_Type (Rec_Type)
           and then not Has_Task (Rec_Type)
           and then not Controlled_Type (Rec_Type)
         then
            Set_Is_Inlined  (Proc_Id);
         end if;

         Set_Is_Internal    (Proc_Id);
         Set_Has_Completion (Proc_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (Proc_Id);
         end if;
      end if;
   end Build_Record_Init_Proc;

   ----------------------------
   -- Build_Slice_Assignment --
   ----------------------------

   --  Generates the following subprogram:

   --    procedure Assign
   --     (Source,   Target   : Array_Type,
   --      Left_Lo,  Left_Hi, Right_Lo, Right_Hi : Index;
   --      Rev :     Boolean)
   --    is
   --       Li1 : Index;
   --       Ri1 : Index;

   --    begin
   --       if Rev  then
   --          Li1 := Left_Hi;
   --          Ri1 := Right_Hi;
   --       else
   --          Li1 := Left_Lo;
   --          Ri1 := Right_Lo;
   --       end if;

   --       loop
   --             if Rev then
   --                exit when Li1 < Left_Lo;
   --             else
   --                exit when Li1 > Left_Hi;
   --             end if;

   --             Target (Li1) := Source (Ri1);

   --             if Rev then
   --                Li1 := Index'pred (Li1);
   --                Ri1 := Index'pred (Ri1);
   --             else
   --                Li1 := Index'succ (Li1);
   --                Ri1 := Index'succ (Ri1);
   --             end if;
   --       end loop;
   --    end Assign;

   procedure Build_Slice_Assignment (Typ : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Index : constant Entity_Id  := Base_Type (Etype (First_Index (Typ)));

      --  Build formal parameters of procedure

      Larray   : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('A'));
      Rarray   : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('R'));
      Left_Lo  : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('L'));
      Left_Hi  : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('L'));
      Right_Lo : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('R'));
      Right_Hi : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('R'));
      Rev      : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, Chars => New_Internal_Name ('D'));
      Proc_Name : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      Chars => Make_TSS_Name (Typ, TSS_Slice_Assign));

      Lnn : constant Entity_Id :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('L'));
      Rnn : constant Entity_Id :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
      --  Subscripts for left and right sides

      Decls : List_Id;
      Loops : Node_Id;
      Stats : List_Id;

   begin
      --  Build declarations for indices

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

      --  Build initializations for indices

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

      --  Build exit condition

      declare
         F_Ass : constant List_Id := New_List;
         B_Ass : constant List_Id := New_List;

      begin
         Append_To (F_Ass,
           Make_Exit_Statement (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => New_Occurrence_Of (Lnn, Loc),
                 Right_Opnd => New_Occurrence_Of (Left_Hi, Loc))));

         Append_To (B_Ass,
           Make_Exit_Statement (Loc,
             Condition =>
               Make_Op_Lt (Loc,
                 Left_Opnd  => New_Occurrence_Of (Lnn, Loc),
                 Right_Opnd => New_Occurrence_Of (Left_Lo, Loc))));

         Prepend_To (Statements (Loops),
           Make_If_Statement (Loc,
             Condition       => New_Occurrence_Of (Rev, Loc),
             Then_Statements => B_Ass,
             Else_Statements => F_Ass));
      end;

      --  Build the increment/decrement statements

      declare
         F_Ass : constant List_Id := New_List;
         B_Ass : constant List_Id := New_List;

      begin
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
      Loc   : constant Source_Ptr := Sloc (Typ);

      F : constant Entity_Id :=
            Make_Defining_Identifier (Loc,
              Chars => Make_TSS_Name (Typ, TSS_Composite_Equality));

      X : constant Entity_Id :=
           Make_Defining_Identifier (Loc,
             Chars => Name_X);

      Y : constant Entity_Id :=
            Make_Defining_Identifier (Loc,
              Chars => Name_Y);

      Def   : constant Node_Id := Parent (Typ);
      Comps : constant Node_Id := Component_List (Type_Definition (Def));
      Stmts : constant List_Id := New_List;
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
              Subtype_Mark => New_Reference_To (Standard_Boolean, Loc)),
          Declarations               => New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts)));

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
      --  parameter list in order to capture the inferred values of the
      --  discriminants in later calls.

      if Is_Unchecked_Union (Typ) then
         declare
            Discr_Type : constant Node_Id := Etype (First_Discriminant (Typ));

            A : constant Node_Id :=
                  Make_Defining_Identifier (Loc,
                    Chars => Name_A);

            B : constant Node_Id :=
                  Make_Defining_Identifier (Loc,
                    Chars => Name_B);

         begin
            --  Add A and B to the parameter list

            Append_To (Pspecs,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => A,
                Parameter_Type => New_Reference_To (Discr_Type, Loc)));

            Append_To (Pspecs,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => B,
                Parameter_Type => New_Reference_To (Discr_Type, Loc)));

            --  Generate the following header code to compare the inferred
            --  discriminants:

            --  if a /= b then
            --     return False;
            --  end if;

            Append_To (Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd => New_Reference_To (A, Loc),
                    Right_Opnd => New_Reference_To (B, Loc)),
                Then_Statements => New_List (
                  Make_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc)))));

            --  Generate component-by-component comparison. Note that we must
            --  propagate one of the inferred discriminant formals to act as
            --  the case statement switch.

            Append_List_To (Stmts,
              Make_Eq_Case (Typ, Comps, A));

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
        Make_Return_Statement (Loc,
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
      Par       : constant Entity_Id := Root_Type (Base_Type (Typ));
      Par_Read  : constant Boolean   := Present (TSS (Par, TSS_Stream_Read));
      Par_Write : constant Boolean   := Present (TSS (Par, TSS_Stream_Write));

      procedure Check_Attr (Nam : Name_Id; TSS_Nam : TSS_Name_Type);
      --  Check that Comp has a user-specified Nam stream attribute

      procedure Check_Attr (Nam : Name_Id; TSS_Nam : TSS_Name_Type) is
      begin
         if No (TSS (Base_Type (Etype (Comp)), TSS_Nam)) then
            Error_Msg_Name_1 := Nam;
            Error_Msg_N
              ("|component& in limited extension must have% attribute", Comp);
         end if;
      end Check_Attr;

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
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      B_Id   : constant Entity_Id := Base_Type (Def_Id);
      Par_Id : Entity_Id;
      FN     : Node_Id;

   begin
      if Is_Access_Type (Def_Id) then

         --  Anonymous access types are created for the components of the
         --  record parameter for an entry declaration.  No master is created
         --  for such a type.

         if Has_Task (Designated_Type (Def_Id))
           and then Comes_From_Source (N)
         then
            Build_Master_Entity (Def_Id);
            Build_Master_Renaming (Parent (Def_Id), Def_Id);

         --  Create a class-wide master because a Master_Id must be generated
         --  for access-to-limited-class-wide types, whose root may be extended
         --  with task components.

         elsif Is_Class_Wide_Type (Designated_Type (Def_Id))
           and then Is_Limited_Type (Designated_Type (Def_Id))
           and then Tasking_Allowed

            --  Don't create a class-wide master for types whose convention is
            --  Java since these types cannot embed Ada tasks anyway. Note that
            --  the following test cannot catch the following case:
            --
            --      package java.lang.Object is
            --         type Typ is tagged limited private;
            --         type Ref is access all Typ'Class;
            --      private
            --         type Typ is tagged limited ...;
            --         pragma Convention (Typ, Java)
            --      end;
            --
            --  Because the convention appears after we have done the
            --  processing for type Ref.

           and then Convention (Designated_Type (Def_Id)) /= Convention_Java
         then
            Build_Class_Wide_Master (Def_Id);

         elsif Ekind (Def_Id) = E_Access_Protected_Subprogram_Type then
            Expand_Access_Protected_Subprogram_Type (N);
         end if;

      elsif Has_Task (Def_Id) then
         Expand_Previous_Access_Type (Def_Id);
      end if;

      Par_Id := Etype (B_Id);

      --  The parent type is private then we need to inherit
      --  any TSS operations from the full view.

      if Ekind (Par_Id) in Private_Kind
        and then Present (Full_View (Par_Id))
      then
         Par_Id := Base_Type (Full_View (Par_Id));
      end if;

      if Nkind (Type_Definition (Original_Node (N)))
         = N_Derived_Type_Definition
        and then not Is_Tagged_Type (Def_Id)
        and then Present (Freeze_Node (Par_Id))
        and then Present (TSS_Elist (Freeze_Node (Par_Id)))
      then
         Ensure_Freeze_Node (B_Id);
         FN :=  Freeze_Node (B_Id);

         if No (TSS_Elist (FN)) then
            Set_TSS_Elist (FN, New_Elmt_List);
         end if;

         declare
            T_E   : constant Elist_Id := TSS_Elist (FN);
            Elmt  : Elmt_Id;

         begin
            Elmt  := First_Elmt (TSS_Elist (Freeze_Node (Par_Id)));

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

   --  First we do special processing for objects of a tagged type where this
   --  is the point at which the type is frozen. The creation of the dispatch
   --  table and the initialization procedure have to be deferred to this
   --  point, since we reference previously declared primitive subprograms.

   --  For all types, we call an initialization procedure if there is one

   procedure Expand_N_Object_Declaration (N : Node_Id) is
      Def_Id  : constant Entity_Id  := Defining_Identifier (N);
      Typ     : constant Entity_Id  := Etype (Def_Id);
      Loc     : constant Source_Ptr := Sloc (N);
      Expr    : constant Node_Id    := Expression (N);
      New_Ref : Node_Id;
      Id_Ref  : Node_Id;
      Expr_Q  : Node_Id;

   begin
      --  Don't do anything for deferred constants. All proper actions will
      --  be expanded during the full declaration.

      if No (Expr) and Constant_Present (N) then
         return;
      end if;

      --  Make shared memory routines for shared passive variable

      if Is_Shared_Passive (Def_Id) then
         Make_Shared_Var_Procs (N);
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

         --  Expand Initialize call for controlled objects.  One may wonder why
         --  the Initialize Call is not done in the regular Init procedure
         --  attached to the record type. That's because the init procedure is
         --  recursively called on each component, including _Parent, thus the
         --  Init call for a controlled object would generate not only one
         --  Initialize call as it is required but one for each ancestor of
         --  its type. This processing is suppressed if No_Initialization set.

         if not Controlled_Type (Typ)
           or else No_Initialization (N)
         then
            null;

         elsif not Abort_Allowed
           or else not Comes_From_Source (N)
         then
            Insert_Actions_After (N,
              Make_Init_Call (
                Ref         => New_Occurrence_Of (Def_Id, Loc),
                Typ         => Base_Type (Typ),
                Flist_Ref   => Find_Final_List (Def_Id),
                With_Attach => Make_Integer_Literal (Loc, 1)));

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
               L   : constant List_Id :=
                      Make_Init_Call (
                        Ref         => New_Occurrence_Of (Def_Id, Loc),
                        Typ         => Base_Type (Typ),
                        Flist_Ref   => Find_Final_List (Def_Id),
                        With_Attach => Make_Integer_Literal (Loc, 1));

               Blk : constant Node_Id :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc, L));

            begin
               Prepend_To (L, Build_Runtime_Call (Loc, RE_Abort_Defer));
               Set_At_End_Proc (Handled_Statement_Sequence (Blk),
                 New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc));
               Insert_Actions_After (N, New_List (Blk));
               Expand_At_End_Handler
                 (Handled_Statement_Sequence (Blk), Entity (Identifier (Blk)));
            end;
         end if;

         --  Call type initialization procedure if there is one. We build the
         --  call and put it immediately after the object declaration, so that
         --  it will be expanded in the usual manner. Note that this will
         --  result in proper handling of defaulted discriminants. The call
         --  to the Init_Proc is suppressed if No_Initialization is set.

         if Has_Non_Null_Base_Init_Proc (Typ)
           and then not No_Initialization (N)
         then
            --  The call to the initialization procedure does NOT freeze
            --  the object being initialized. This is because the call is
            --  not a source level call. This works fine, because the only
            --  possible statements depending on freeze status that can
            --  appear after the _Init call are rep clauses which can
            --  safely appear after actual references to the object.

            Id_Ref := New_Reference_To (Def_Id, Loc);
            Set_Must_Not_Freeze (Id_Ref);
            Set_Assignment_OK (Id_Ref);

            Insert_Actions_After (N,
              Build_Initialization_Call (Loc, Id_Ref, Typ));

         --  If simple initialization is required, then set an appropriate
         --  simple initialization expression in place. This special
         --  initialization is required even though No_Init_Flag is present.

         elsif Needs_Simple_Initialization (Typ) then
            Set_No_Initialization (N, False);
            Set_Expression (N, Get_Simple_Init_Val (Typ, Loc, Esize (Def_Id)));
            Analyze_And_Resolve (Expression (N), Typ);
         end if;

      --  Explicit initialization present

      else
         --  Obtain actual expression from qualified expression

         if Nkind (Expr) = N_Qualified_Expression then
            Expr_Q := Expression (Expr);
         else
            Expr_Q := Expr;
         end if;

         --  When we have the appropriate type of aggregate in the
         --  expression (it has been determined during analysis of the
         --  aggregate by setting the delay flag), let's perform in
         --  place assignment and thus avoid creating a temporary.

         if Is_Delayed_Aggregate (Expr_Q) then
            Convert_Aggr_In_Object_Decl (N);

         else
            --  In most cases, we must check that the initial value meets
            --  any constraint imposed by the declared type. However, there
            --  is one very important exception to this rule. If the entity
            --  has an unconstrained nominal subtype, then it acquired its
            --  constraints from the expression in the first place, and not
            --  only does this mean that the constraint check is not needed,
            --  but an attempt to perform the constraint check can
            --  cause order of elaboration problems.

            if not Is_Constr_Subt_For_U_Nominal (Typ) then

               --  If this is an allocator for an aggregate that has been
               --  allocated in place, delay checks until assignments are
               --  made, because the discriminants are not initialized.

               if Nkind (Expr) = N_Allocator
                 and then No_Initialization (Expr)
               then
                  null;
               else
                  Apply_Constraint_Check (Expr, Typ);
               end if;
            end if;

            --  If the type is controlled we attach the object to the final
            --  list and adjust the target after the copy. This

            if Controlled_Type (Typ) then
               declare
                  Flist : Node_Id;
                  F     : Entity_Id;

               begin
                  --  Attach the result to a dummy final list which will never
                  --  be finalized if Delay_Finalize_Attachis set. It is
                  --  important to attach to a dummy final list rather than
                  --  not attaching at all in order to reset the pointers
                  --  coming from the initial value. Equivalent code exists
                  --  in the sec-stack case in Exp_Ch4.Expand_N_Allocator.

                  if Delay_Finalize_Attach (N) then
                     F :=
                       Make_Defining_Identifier (Loc, New_Internal_Name ('F'));
                     Insert_Action (N,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => F,
                         Object_Definition   =>
                           New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));

                     Flist := New_Reference_To (F, Loc);

                  else
                     Flist := Find_Final_List (Def_Id);
                  end if;

                  Insert_Actions_After (N,
                    Make_Adjust_Call (
                      Ref          => New_Reference_To (Def_Id, Loc),
                      Typ          => Base_Type (Typ),
                      Flist_Ref    => Flist,
                      With_Attach  => Make_Integer_Literal (Loc, 1)));
               end;
            end if;

            --  For tagged types, when an init value is given, the tag has to
            --  be re-initialized separately in order to avoid the propagation
            --  of a wrong tag coming from a view conversion unless the type
            --  is class wide (in this case the tag comes from the init
            --  value). Suppress the tag assignment when Java_VM because JVM
            --  tags are represented implicitly in objects. Ditto for types
            --  that are CPP_CLASS, and for initializations that are
            --  aggregates, because they have to have the right tag.

            if Is_Tagged_Type (Typ)
              and then not Is_Class_Wide_Type (Typ)
              and then not Is_CPP_Class (Typ)
              and then not Java_VM
              and then Nkind (Expr) /= N_Aggregate
            then
               --  The re-assignment of the tag has to be done even if
               --  the object is a constant

               New_Ref :=
                 Make_Selected_Component (Loc,
                    Prefix => New_Reference_To (Def_Id, Loc),
                    Selector_Name =>
                      New_Reference_To (First_Tag_Component (Typ), Loc));

               Set_Assignment_OK (New_Ref);

               Insert_After (N,
                 Make_Assignment_Statement (Loc,
                   Name => New_Ref,
                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To
                         (Node
                           (First_Elmt
                             (Access_Disp_Table (Base_Type (Typ)))),
                          Loc))));

            --  For discrete types, set the Is_Known_Valid flag if the
            --  initializing value is known to be valid.

            elsif Is_Discrete_Type (Typ)
              and then Expr_Known_Valid (Expr)
            then
               Set_Is_Known_Valid (Def_Id);

            elsif Is_Access_Type (Typ) then

               --  Ada 2005 (AI-231): Generate conversion to the null-excluding
               --  type to force the corresponding run-time check

               if Ada_Version >= Ada_05
                 and then (Can_Never_Be_Null (Def_Id)
                             or else Can_Never_Be_Null (Typ))
               then
                  Rewrite
                    (Expr_Q,
                     Convert_To (Etype (Def_Id), Relocate_Node (Expr_Q)));
                  Analyze_And_Resolve (Expr_Q, Etype (Def_Id));
               end if;

               --  For access types set the Is_Known_Non_Null flag if the
               --  initializing value is known to be non-null. We can also set
               --  Can_Never_Be_Null if this is a constant.

               if Known_Non_Null (Expr) then
                  Set_Is_Known_Non_Null (Def_Id);

                  if Constant_Present (N) then
                     Set_Can_Never_Be_Null (Def_Id);
                  end if;
               end if;
            end if;

            --  If validity checking on copies, validate initial expression

            if Validity_Checks_On
               and then Validity_Check_Copies
            then
               Ensure_Valid (Expr);
               Set_Is_Known_Valid (Def_Id);
            end if;
         end if;

         --  Cases where the back end cannot handle the initialization
         --  directly. In such cases, we expand an assignment that will
         --  be appropriately handled by Expand_N_Assignment_Statement.

         --  The exclusion of the unconstrained case is wrong, but for
         --  now it is too much trouble ???

         if (Is_Possibly_Unaligned_Slice (Expr)
               or else (Is_Possibly_Unaligned_Object (Expr)
                          and then not Represented_As_Scalar (Etype (Expr))))

            --  The exclusion of the unconstrained case is wrong, but for
            --  now it is too much trouble ???

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
               Insert_After (N, Stat);
               Analyze (Stat);
            end;
         end if;
      end if;

      --  For array type, check for size too large
      --  We really need this for record types too???

      if Is_Array_Type (Typ) then
         Apply_Array_Size_Check (N, Typ);
      end if;

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
   --  extraneous expanded code.

   procedure Expand_N_Subtype_Indication (N : Node_Id) is
      Ran : constant Node_Id   := Range_Expression (Constraint (N));
      Typ : constant Entity_Id := Entity (Subtype_Mark (N));

   begin
      if Nkind (Parent (N)) = N_Constrained_Array_Definition or else
         Nkind (Parent (N)) = N_Slice
      then
         Resolve (Ran, Typ);
         Apply_Range_Check (Ran, Typ);
      end if;
   end Expand_N_Subtype_Indication;

   ---------------------------
   -- Expand_N_Variant_Part --
   ---------------------------

   --  If the last variant does not contain the Others choice, replace it with
   --  an N_Others_Choice node since Gigi always wants an Others. Note that we
   --  do not bother to call Analyze on the modified variant part, since it's
   --  only effect would be to compute the contents of the
   --  Others_Discrete_Choices node laboriously, and of course we already know
   --  the list of choices that corresponds to the others choice (it's the
   --  list we are replacing!)

   procedure Expand_N_Variant_Part (N : Node_Id) is
      Last_Var    : constant Node_Id := Last_Non_Pragma (Variants (N));
      Others_Node : Node_Id;
   begin
      if Nkind (First (Discrete_Choices (Last_Var))) /= N_Others_Choice then
         Others_Node := Make_Others_Choice (Sloc (Last_Var));
         Set_Others_Discrete_Choices
           (Others_Node, Discrete_Choices (Last_Var));
         Set_Discrete_Choices (Last_Var, New_List (Others_Node));
      end if;
   end Expand_N_Variant_Part;

   ---------------------------------
   -- Expand_Previous_Access_Type --
   ---------------------------------

   procedure Expand_Previous_Access_Type (Def_Id : Entity_Id) is
      T : Entity_Id := First_Entity (Current_Scope);

   begin
      --  Find all access types declared in the current scope, whose
      --  designated type is Def_Id.

      while Present (T) loop
         if Is_Access_Type (T)
           and then Designated_Type (T) = Def_Id
         then
            Build_Master_Entity (Def_Id);
            Build_Master_Renaming (Parent (Def_Id), T);
         end if;

         Next_Entity (T);
      end loop;
   end Expand_Previous_Access_Type;

   ------------------------------
   -- Expand_Record_Controller --
   ------------------------------

   procedure Expand_Record_Controller (T : Entity_Id) is
      Def             : Node_Id := Type_Definition (Parent (T));
      Comp_List       : Node_Id;
      Comp_Decl       : Node_Id;
      Loc             : Source_Ptr;
      First_Comp      : Node_Id;
      Controller_Type : Entity_Id;
      Ent             : Entity_Id;

   begin
      if Nkind (Def) = N_Derived_Type_Definition then
         Def := Record_Extension_Part (Def);
      end if;

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
         Loc := Sloc (Comp_List);
      else
         Loc := Sloc (First (Component_Items (Comp_List)));
      end if;

      if Is_Return_By_Reference_Type (T) then
         Controller_Type := RTE (RE_Limited_Record_Controller);
      else
         Controller_Type := RTE (RE_Record_Controller);
      end if;

      Ent := Make_Defining_Identifier (Loc, Name_uController);

      Comp_Decl :=
        Make_Component_Declaration (Loc,
          Defining_Identifier =>  Ent,
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present => False,
              Subtype_Indication => New_Reference_To (Controller_Type, Loc)));

      if Null_Present (Comp_List)
        or else Is_Empty_List (Component_Items (Comp_List))
      then
         Set_Component_Items (Comp_List, New_List (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         --  The controller cannot be placed before the _Parent field since
         --  gigi lays out field in order and _parent must be first to
         --  preserve the polymorphism of tagged types.

         First_Comp := First (Component_Items (Comp_List));

         if Chars (Defining_Identifier (First_Comp)) /= Name_uParent
           and then Chars (Defining_Identifier (First_Comp)) /= Name_uTag
         then
            Insert_Before (First_Comp, Comp_Decl);
         else
            Insert_After (First_Comp, Comp_Decl);
         end if;
      end if;

      New_Scope (T);
      Analyze (Comp_Decl);
      Set_Ekind (Ent, E_Component);
      Init_Component_Location (Ent);

      --  Move the _controller entity ahead in the list of internal entities
      --  of the enclosing record so that it is selected instead of a
      --  potentially inherited one.

      declare
         E    : constant Entity_Id := Last_Entity (T);
         Comp : Entity_Id;

      begin
         pragma Assert (Chars (E) = Name_uController);

         Set_Next_Entity (E, First_Entity (T));
         Set_First_Entity (T, E);

         Comp := Next_Entity (E);
         while Next_Entity (Comp) /= E loop
            Next_Entity (Comp);
         end loop;

         Set_Next_Entity (Comp, Empty);
         Set_Last_Entity (T, Comp);
      end;

      End_Scope;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Record_Controller;

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

   -----------------------
   -- Freeze_Array_Type --
   -----------------------

   procedure Freeze_Array_Type (N : Node_Id) is
      Typ  : constant Entity_Id  := Entity (N);
      Base : constant Entity_Id  := Base_Type (Typ);

   begin
      if not Is_Bit_Packed_Array (Typ) then

         --  If the component contains tasks, so does the array type. This may
         --  not be indicated in the array type because the component may have
         --  been a private type at the point of definition. Same if component
         --  type is controlled.

         Set_Has_Task (Base, Has_Task (Component_Type (Typ)));
         Set_Has_Controlled_Component (Base,
           Has_Controlled_Component (Component_Type (Typ))
             or else Is_Controlled (Component_Type (Typ)));

         if No (Init_Proc (Base)) then

            --  If this is an anonymous array created for a declaration with
            --  an initial value, its init_proc will never be called. The
            --  initial value itself may have been expanded into assign-
            --  ments, in which case the object declaration is carries the
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

         if Typ = Base and then Has_Controlled_Component (Base) then
            Build_Controlling_Procs (Base);

            if not Is_Limited_Type (Component_Type (Typ))
              and then Number_Dimensions (Typ) = 1
            then
               Build_Slice_Assignment (Typ);
            end if;
         end if;

      --  For packed case, there is a default initialization, except if the
      --  component type is itself a packed structure with an initialization
      --  procedure.

      elsif Present (Init_Proc (Component_Type (Base)))
        and then No (Base_Init_Proc (Base))
      then
         Build_Array_Init_Proc (Base, N);
      end if;
   end Freeze_Array_Type;

   -----------------------------
   -- Freeze_Enumeration_Type --
   -----------------------------

   procedure Freeze_Enumeration_Type (N : Node_Id) is
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
      --  Various optimization are possible if the given representation is
      --  contiguous.

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
                  Left_Opnd =>
                     Unchecked_Convert_To (Ityp,
                       Make_Identifier (Loc, Name_uA)),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                        Intval =>
                          Enumeration_Rep (First_Literal (Typ)))));
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
                  Make_Return_Statement (Loc,
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
                  Make_Return_Statement (Loc,
                    Expression =>
                      Make_Integer_Literal (Loc,
                        Intval => Enumeration_Pos (Ent))))));

            Next_Literal (Ent);
         end loop;
      end if;

      --  In normal mode, add the others clause with the test

      if not Restriction_Active (No_Exception_Handlers) then
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements => New_List (
               Make_Raise_Constraint_Error (Loc,
                 Condition => Make_Identifier (Loc, Name_uF),
                 Reason    => CE_Invalid_Data),
               Make_Return_Statement (Loc,
                 Expression =>
                   Make_Integer_Literal (Loc, -1)))));

      --  If Restriction (No_Exceptions_Handlers) is active then we always
      --  return -1 (since we cannot usefully raise Constraint_Error in
      --  this case). See description above for further details.

      else
         Append_To (Lst,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements => New_List (
               Make_Return_Statement (Loc,
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

              Subtype_Mark => New_Reference_To (Standard_Integer, Loc)),

            Declarations => Empty_List,

            Handled_Statement_Sequence =>
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Case_Statement (Loc,
                    Expression =>
                      Unchecked_Convert_To (Ityp,
                        Make_Identifier (Loc, Name_uA)),
                    Alternatives => Lst))));

      Set_TSS (Typ, Fent);
      Set_Is_Pure (Fent);

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Fent);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Freeze_Enumeration_Type;

   ------------------------
   -- Freeze_Record_Type --
   ------------------------

   procedure Freeze_Record_Type (N : Node_Id) is
      Def_Id      : constant Node_Id := Entity (N);
      Comp        : Entity_Id;
      Type_Decl   : constant Node_Id := Parent (Def_Id);
      Predef_List : List_Id;

      Renamed_Eq  : Node_Id := Empty;
      --  Could use some comments ???

   begin
      --  Build discriminant checking functions if not a derived type (for
      --  derived types that are not tagged types, we always use the
      --  discriminant checking functions of the parent type). However, for
      --  untagged types the derivation may have taken place before the
      --  parent was frozen, so we copy explicitly the discriminant checking
      --  functions from the parent into the components of the derived type.

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
      --  declaration.

      Comp := First_Component (Def_Id);

      while Present (Comp) loop
         if Has_Task (Etype (Comp)) then
            Set_Has_Task (Def_Id);

         elsif Has_Controlled_Component (Etype (Comp))
           or else (Chars (Comp) /= Name_uParent
                     and then Is_Controlled (Etype (Comp)))
         then
            Set_Has_Controlled_Component (Def_Id);
         end if;

         Next_Component (Comp);
      end loop;

      --  Creation of the Dispatch Table. Note that a Dispatch Table is
      --  created for regular tagged types as well as for Ada types deriving
      --  from a C++ Class, but not for tagged types directly corresponding to
      --  the C++ classes. In the later case we assume that the Vtable is
      --  created in the C++ side and we just use it.

      if Is_Tagged_Type (Def_Id) then
         if Is_CPP_Class (Def_Id) then
            Set_All_DT_Position (Def_Id);
            Set_Default_Constructor (Def_Id);

         else
            --  Usually inherited primitives are not delayed but the first Ada
            --  extension of a CPP_Class is an exception since the address of
            --  the inherited subprogram has to be inserted in the new Ada
            --  Dispatch Table and this is a freezing action (usually the
            --  inherited primitive address is inserted in the DT by
            --  Inherit_DT)

            --  Similarly, if this is an inherited operation whose parent is
            --  not frozen yet, it is not in the DT of the parent, and we
            --  generate an explicit freeze node for the inherited operation,
            --  so that it is properly inserted in the DT of the current type.

            declare
               Elmt : Elmt_Id := First_Elmt (Primitive_Operations (Def_Id));
               Subp : Entity_Id;

            begin
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

            if Underlying_Type (Etype (Def_Id)) = Def_Id then
               Expand_Tagged_Root (Def_Id);
            end if;

            --  Unfreeze momentarily the type to add the predefined primitives
            --  operations. The reason we unfreeze is so that these predefined
            --  operations will indeed end up as primitive operations (which
            --  must be before the freeze point).

            Set_Is_Frozen (Def_Id, False);
            Make_Predefined_Primitive_Specs
              (Def_Id, Predef_List, Renamed_Eq);
            Insert_List_Before_And_Analyze (N, Predef_List);
            Set_Is_Frozen (Def_Id, True);
            Set_All_DT_Position (Def_Id);

            --  Add the controlled component before the freezing actions
            --  referenced in those actions.

            if Has_New_Controlled_Component (Def_Id) then
               Expand_Record_Controller (Def_Id);
            end if;

            --  Suppress creation of a dispatch table when Java_VM because the
            --  dispatching mechanism is handled internally by the JVM.

            if not Java_VM then
               Append_Freeze_Actions (Def_Id, Make_DT (Def_Id));
            end if;

            --  Make sure that the primitives Initialize, Adjust and Finalize
            --  are Frozen before other TSS subprograms. We don't want them
            --  Frozen inside.

            if Is_Controlled (Def_Id) then
               if not Is_Limited_Type (Def_Id) then
                  Append_Freeze_Actions (Def_Id,
                    Freeze_Entity
                      (Find_Prim_Op (Def_Id, Name_Adjust), Sloc (Def_Id)));
               end if;

               Append_Freeze_Actions (Def_Id,
                 Freeze_Entity
                   (Find_Prim_Op (Def_Id, Name_Initialize), Sloc (Def_Id)));

               Append_Freeze_Actions (Def_Id,
                 Freeze_Entity
                   (Find_Prim_Op (Def_Id, Name_Finalize), Sloc (Def_Id)));
            end if;

            --  Freeze rest of primitive operations

            Append_Freeze_Actions
              (Def_Id, Predefined_Primitive_Freeze (Def_Id));
         end if;

      --  In the non-tagged case, an equality function is provided only for
      --  variant records (that are not unchecked unions).

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
            Ctyp : constant Entity_Id :=
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
         if No (Controller_Component (Def_Id)) then
            Expand_Record_Controller (Def_Id);
         end if;

         Build_Controlling_Procs (Def_Id);
      end if;

      Adjust_Discriminants (Def_Id);
      Build_Record_Init_Proc (Type_Decl, Def_Id);

      --  For tagged type, build bodies of primitive operations. Note that we
      --  do this after building the record initialization experiment, since
      --  the primitive operations may need the initialization routine

      if Is_Tagged_Type (Def_Id) then
         Predef_List := Predefined_Primitive_Bodies (Def_Id, Renamed_Eq);
         Append_Freeze_Actions (Def_Id, Predef_List);
      end if;

   end Freeze_Record_Type;

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
            Append_Freeze_Actions
               (Typ, Freeze_Entity (Stream_Op, Sloc (N)));
         end if;
      end loop;
   end Freeze_Stream_Operations;

   -----------------
   -- Freeze_Type --
   -----------------

   --  Full type declarations are expanded at the point at which the type is
   --  frozen. The formal N is the Freeze_Node for the type. Any statements or
   --  declarations generated by the freezing (e.g. the procedure generated
   --  for initialization) are chained in the Acions field list of the freeze
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
            Freeze_Record_Type (N);

         --  The subtype may have been declared before the type was frozen. If
         --  the type has controlled components it is necessary to create the
         --  entity for the controller explicitly because it did not exist at
         --  the point of the subtype declaration. Only the entity is needed,
         --  the back-end will obtain the layout from the type. This is only
         --  necessary if this is constrained subtype whose component list is
         --  not shared with the base type.

         elsif Ekind (Def_Id) = E_Record_Subtype
           and then Has_Discriminants (Def_Id)
           and then Last_Entity (Def_Id) /= Last_Entity (Base_Type (Def_Id))
           and then Present (Controller_Component (Def_Id))
         then
            declare
               Old_C : constant Entity_Id := Controller_Component (Def_Id);
               New_C : Entity_Id;

            begin
               if Scope (Old_C) = Base_Type (Def_Id) then

                  --  The entity is the one in the parent. Create new one

                  New_C := New_Copy (Old_C);
                  Set_Parent (New_C, Parent (Old_C));
                  New_Scope (Def_Id);
                  Enter_Name (New_C);
                  End_Scope;
               end if;
            end;

            if Is_Itype (Def_Id)
              and then Is_Record_Type (Underlying_Type (Scope (Def_Id)))
            then
               --  The freeze node is only used to introduce the controller,
               --  the back-end has no use for it for a discriminated
               --   component.

               Set_Freeze_Node (Def_Id, Empty);
               Set_Has_Delayed_Freeze (Def_Id, False);
               Result := True;
            end if;

         --  Similar process if the controller of the subtype is not present
         --  but the parent has it. This can happen with constrained
         --  record components where the subtype is an itype.

         elsif Ekind (Def_Id) = E_Record_Subtype
           and then Is_Itype (Def_Id)
           and then No (Controller_Component (Def_Id))
           and then Present (Controller_Component (Etype (Def_Id)))
         then
            declare
               Old_C : constant Entity_Id :=
                         Controller_Component (Etype (Def_Id));
               New_C : constant Entity_Id := New_Copy (Old_C);

            begin
               Set_Next_Entity  (New_C, First_Entity (Def_Id));
               Set_First_Entity (Def_Id, New_C);

               --  The freeze node is only used to introduce the controller,
               --  the back-end has no use for it for a discriminated
               --   component.

               Set_Freeze_Node (Def_Id, Empty);
               Set_Has_Delayed_Freeze (Def_Id, False);
               Result := True;
            end;
         end if;

      --  Freeze processing for array types

      elsif Is_Array_Type (Def_Id) then
         Freeze_Array_Type (N);

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

      elsif Ekind (Def_Id) = E_Access_Type
        or else Ekind (Def_Id) = E_General_Access_Type
      then
         declare
            Loc         : constant Source_Ptr := Sloc (N);
            Desig_Type  : constant Entity_Id := Designated_Type (Def_Id);
            Pool_Object : Entity_Id;
            Siz_Exp     : Node_Id;

            Freeze_Action_Typ : Entity_Id;

         begin
            if Has_Storage_Size_Clause (Def_Id) then
               Siz_Exp := Expression (Parent (Storage_Size_Variable (Def_Id)));
            else
               Siz_Exp := Empty;
            end if;

            --  Case 1

            --    Rep Clause "for Def_Id'Storage_Size use 0;"
            --    ---> don't use any storage pool

            if Has_Storage_Size_Clause (Def_Id)
              and then Compile_Time_Known_Value (Siz_Exp)
              and then Expr_Value (Siz_Exp) = 0
            then
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
                  --  that has the later freeze node, usually the acces type
                  --  but it can also be the designated_type; because the pool
                  --  code requires both those types to be frozen

                  if Is_Frozen (Desig_Type)
                    and then (not Present (Freeze_Node (Desig_Type))
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

            elsif Present (Associated_Storage_Pool (Def_Id)) then

               --  Nothing to do the associated storage pool has been attached
               --  when analyzing the rep. clause

               null;
            end if;

            --  For access-to-controlled types (including class-wide types and
            --  Taft-amendment types which potentially have controlled
            --  components), expand the list controller object that will store
            --  the dynamically allocated objects. Do not do this
            --  transformation for expander-generated access types, but do it
            --  for types that are the full view of types derived from other
            --  private types. Also suppress the list controller in the case
            --  of a designated type with convention Java, since this is used
            --  when binding to Java API specs, where there's no equivalent of
            --  a finalization list and we don't want to pull in the
            --  finalization support if not needed.

            if not Comes_From_Source (Def_Id)
               and then not Has_Private_Declaration (Def_Id)
            then
               null;

            elsif (Controlled_Type (Desig_Type)
                    and then Convention (Desig_Type) /= Convention_Java)
              or else
                (Is_Incomplete_Or_Private_Type (Desig_Type)
                   and then No (Full_View (Desig_Type))

                  --  An exception is made for types defined in the run-time
                  --  because Ada.Tags.Tag itself is such a type and cannot
                  --  afford this unnecessary overhead that would generates a
                  --  loop in the expansion scheme...

                  and then not In_Runtime (Def_Id)

                  --  Another exception is if Restrictions (No_Finalization)
                  --  is active, since then we know nothing is controlled.

                  and then not Restriction_Active (No_Finalization))

               --  If the designated type is not frozen yet, its controlled
               --  status must be retrieved explicitly.

              or else (Is_Array_Type (Desig_Type)
                and then not Is_Frozen (Desig_Type)
                and then Controlled_Type (Component_Type (Desig_Type)))
            then
               Set_Associated_Final_Chain (Def_Id,
                 Make_Defining_Identifier (Loc,
                   New_External_Name (Chars (Def_Id), 'L')));

               Append_Freeze_Action (Def_Id,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Associated_Final_Chain (Def_Id),
                   Object_Definition   =>
                     New_Reference_To (RTE (RE_List_Controller), Loc)));
            end if;
         end;

      --  Freeze processing for enumeration types

      elsif Ekind (Def_Id) = E_Enumeration_Type then

         --  We only have something to do if we have a non-standard
         --  representation (i.e. at least one literal whose pos value
         --  is not the same as its representation)

         if Has_Non_Standard_Rep (Def_Id) then
            Freeze_Enumeration_Type (N);
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
      Loc  : Source_Ptr;
      Size : Uint := No_Uint) return Node_Id
   is
      Val    : Node_Id;
      Result : Node_Id;
      Val_RE : RE_Id;

      Size_To_Use : Uint;
      --  This is the size to be used for computation of the appropriate
      --  initial value for the Normalize_Scalars and Initialize_Scalars case.

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
         Val := Get_Simple_Init_Val (Underlying_Type (T), Loc, Size);

         --  A special case, if the underlying value is null, then qualify it
         --  with the underlying type, so that the null is properly typed
         --  Similarly, if it is an aggregate it must be qualified, because an
         --  unchecked conversion does not provide a context for it.

         if Nkind (Val) = N_Null
           or else Nkind (Val) = N_Aggregate
         then
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

      --  For scalars, we must have normalize/initialize scalars case

      elsif Is_Scalar_Type (T) then
         pragma Assert (Init_Or_Norm_Scalars);

         --  Compute size of object. If it is given by the caller, we can use
         --  it directly, otherwise we use Esize (T) as an estimate. As far as
         --  we know this covers all cases correctly.

         if Size = No_Uint or else Size <= Uint_0 then
            Size_To_Use := UI_Max (Uint_1, Esize (T));
         else
            Size_To_Use := Size;
         end if;

         --  Maximum size to use is 64 bits, since we will create values
         --  of type Unsigned_64 and the range must fit this type.

         if Size_To_Use /= No_Uint and then Size_To_Use > Uint_64 then
            Size_To_Use := Uint_64;
         end if;

         --  Check known bounds of subtype

         Check_Subtype_Bounds;

         --  Processing for Normalize_Scalars case

         if Normalize_Scalars then

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
            --  is, then all 1 bits may be interpretecd as minus one, which is
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

               --  Resolve as Unsigned_64, because the largest number we
               --  can generate is out of range of universal integer.

               Analyze_And_Resolve (Val, RTE (RE_Unsigned_64));

            --  Case of signed types

            else
               declare
                  Signed_Size : constant Uint :=
                                  UI_Min (Uint_63, Size_To_Use - 1);

               begin
                  --  Normally we like to use the most negative number. The
                  --  one exception is when this number is in the known
                  --  subtype range and the largest positive number is not in
                  --  the known subtype range.

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

         --  Here for Initialize_Scalars case

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
         --  the base type to avoid the unchecked conversion from chopping
         --  bits, and then we set Kill_Range_Check to preserve the "bad"
         --  value.

         Result := Unchecked_Convert_To (Base_Type (T), Val);

         --  Ensure result is not truncated, since we want the "bad" bits
         --  and also kill range check on result.

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
                     (Component_Type (T), Loc, Esize (Root_Type (T))))));

      --  Access type is initialized to null

      elsif Is_Access_Type (T) then
         return
           Make_Null (Loc);

      --  No other possibilities should arise, since we should only be
      --  calling Get_Simple_Init_Val if Needs_Simple_Initialization
      --  returned True, indicating one of the above cases held.

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
      S1 : Entity_Id := Scope (E);

   begin
      while Scope (S1) /= Standard_Standard loop
         S1 := Scope (S1);
      end loop;

      return Chars (S1) = Name_System or else Chars (S1) = Name_Ada;
   end In_Runtime;

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
             Parameter_Type => New_Reference_To (RTE (RE_Master_Id), Loc)));

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uChain),
             In_Present => True,
             Out_Present => True,
             Parameter_Type =>
               New_Reference_To (RTE (RE_Activation_Chain), Loc)));

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uTask_Name),
             In_Present => True,
             Parameter_Type =>
               New_Reference_To (Standard_String, Loc)));
      end if;

      return Formals;

   exception
      when RE_Not_Available =>
         return Empty_List;
   end Init_Formals;

   ------------------
   -- Make_Eq_Case --
   ------------------

   --  <Make_Eq_if shared components>
   --  case X.D1 is
   --     when V1 => <Make_Eq_Case> on subcomponents
   --     ...
   --     when Vn => <Make_Eq_Case> on subcomponents
   --  end case;

   function Make_Eq_Case
     (E     : Entity_Id;
      CL    : Node_Id;
      Discr : Entity_Id := Empty) return List_Id
   is
      Loc      : constant Source_Ptr := Sloc (E);
      Result   : constant List_Id    := New_List;
      Variant  : Node_Id;
      Alt_List : List_Id;

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
             Statements => Make_Eq_Case (E, Component_List (Variant))));

         Next_Non_Pragma (Variant);
      end loop;

      --  If we have an Unchecked_Union, use one of the parameters that
      --  captures the discriminants.

      if Is_Unchecked_Union (E) then
         Append_To (Result,
           Make_Case_Statement (Loc,
             Expression => New_Reference_To (Discr, Loc),
             Alternatives => Alt_List));

      else
         Append_To (Result,
           Make_Case_Statement (Loc,
             Expression =>
               Make_Selected_Component (Loc,
                 Prefix => Make_Identifier (Loc, Name_X),
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

            --  The tags must not be compared they are not part of the value.
            --  Note also that in the following, we use Make_Identifier for
            --  the component names. Use of New_Reference_To to identify the
            --  components would be incorrect because the wrong entities for
            --  discriminants could be picked up in the private type case.

            if Field_Name /= Name_uTag then
               Evolve_Or_Else (Cond,
                 Make_Op_Ne (Loc,
                   Left_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_X),
                       Selector_Name =>
                         Make_Identifier (Loc, Field_Name)),

                   Right_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_Y),
                       Selector_Name =>
                         Make_Identifier (Loc, Field_Name))));
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
                  Make_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))));
         end if;
      end if;
   end Make_Eq_If;

   -------------------------------------
   -- Make_Predefined_Primitive_Specs --
   -------------------------------------

   procedure Make_Predefined_Primitive_Specs
     (Tag_Typ     : Entity_Id;
      Predef_List : out List_Id;
      Renamed_Eq  : out Node_Id)
   is
      Loc       : constant Source_Ptr := Sloc (Tag_Typ);
      Res       : constant List_Id    := New_List;
      Prim      : Elmt_Id;
      Eq_Needed : Boolean;
      Eq_Spec   : Node_Id;
      Eq_Name   : Name_Id := Name_Op_Eq;

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

      --  Spec of _Alignment

      Append_To (Res, Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uAlignment,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Integer));

      --  Specs for dispatching stream attributes.

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

      --  Spec of "=" if expanded if the type is not limited and if a
      --  user defined "=" was not already declared for the non-full
      --  view of a private extension

      if not Is_Limited_Type (Tag_Typ) then
         Eq_Needed := True;

         Prim := First_Elmt (Primitive_Operations (Tag_Typ));
         while Present (Prim) loop

            --  If a primitive is encountered that renames the predefined
            --  equality operator before reaching any explicit equality
            --  primitive, then we still need to create a predefined
            --  equality function, because calls to it can occur via
            --  the renaming. A new name is created for the equality
            --  to avoid conflicting with any user-defined equality.
            --  (Note that this doesn't account for renamings of
            --  equality nested within subpackages???)

            if Is_Predefined_Eq_Renaming (Node (Prim)) then
               Eq_Name := New_External_Name (Chars (Node (Prim)), 'E');

            elsif Chars (Node (Prim)) = Name_Op_Eq
              and then (No (Alias (Node (Prim)))
                         or else Nkind (Unit_Declaration_Node (Node (Prim))) =
                                            N_Subprogram_Renaming_Declaration)
              and then Etype (First_Formal (Node (Prim))) =
                         Etype (Next_Formal (First_Formal (Node (Prim))))
              and then Base_Type (Etype (Node (Prim))) = Standard_Boolean

            then
               Eq_Needed := False;
               exit;

            --  If the parent equality is abstract, the inherited equality is
            --  abstract as well, and no body can be created for for it.

            elsif Chars (Node (Prim)) = Name_Op_Eq
              and then Present (Alias (Node (Prim)))
              and then Is_Abstract (Alias (Node (Prim)))
            then
               Eq_Needed := False;
               exit;
            end if;

            Next_Elmt (Prim);
         end loop;

         --  If a renaming of predefined equality was found
         --  but there was no user-defined equality (so Eq_Needed
         --  is still true), then set the name back to Name_Op_Eq.
         --  But in the case where a user-defined equality was
         --  located after such a renaming, then the predefined
         --  equality function is still needed, so Eq_Needed must
         --  be set back to True.

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

            if Eq_Name /= Name_Op_Eq then
               Renamed_Eq := Defining_Unit_Name (Specification (Eq_Spec));

               Prim := First_Elmt (Primitive_Operations (Tag_Typ));
               while Present (Prim) loop

                  --  Any renamings of equality that appeared before an
                  --  overriding equality must be updated to refer to
                  --  the entity for the predefined equality, otherwise
                  --  calls via the renaming would get incorrectly
                  --  resolved to call the user-defined equality function.

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

      --  Specs for finalization actions that may be required in case a
      --  future extension contain a controlled element. We generate those
      --  only for root tagged types where they will get dummy bodies or
      --  when the type has controlled components and their body must be
      --  generated. It is also impossible to provide those for tagged
      --  types defined within s-finimp since it would involve circularity
      --  problems

      if In_Finalization_Root (Tag_Typ) then
         null;

      --  We also skip these if finalization is not available

      elsif Restriction_Active (No_Finalization) then
         null;

      elsif Etype (Tag_Typ) = Tag_Typ or else Controlled_Type (Tag_Typ) then
         if not Is_Limited_Type (Tag_Typ) then
            Append_To (Res,
              Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Adjust));
         end if;

         Append_To (Res, Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Finalize));
      end if;

      Predef_List := Res;
   end Make_Predefined_Primitive_Specs;

   ---------------------------------
   -- Needs_Simple_Initialization --
   ---------------------------------

   function Needs_Simple_Initialization (T : Entity_Id) return Boolean is
   begin
      --  Check for private type, in which case test applies to the
      --  underlying type of the private type.

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

      --  Cases needing simple initialization are access types, and, if pragma
      --  Normalize_Scalars or Initialize_Scalars is in effect, then all scalar
      --  types.

      elsif Is_Access_Type (T)
        or else (Init_Or_Norm_Scalars and then (Is_Scalar_Type (T)))
      then
         return True;

      --  If Initialize/Normalize_Scalars is in effect, string objects also
      --  need initialization, unless they are created in the course of
      --  expanding an aggregate (since in the latter case they will be
      --  filled with appropriate initializing values before they are used).

      elsif Init_Or_Norm_Scalars
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
      Prof   : List_Id;
      Type_B : Entity_Id;

   begin
      if Name = TSS_Deep_Finalize then
         Prof := New_List;
         Type_B := Standard_Boolean;

      else
         Prof := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_L),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));
         Type_B := Standard_Short_Short_Integer;
      end if;

      Append_To (Prof,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      => New_Reference_To (Tag_Typ, Loc)));

      Append_To (Prof,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_B),
             Parameter_Type      => New_Reference_To (Type_B, Loc)));

      return Predef_Spec_Or_Body (Loc,
        Name     => Make_TSS_Name (Tag_Typ, Name),
        Tag_Typ  => Tag_Typ,
        Profile  => Prof,
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

      --  The internal flag is set to mark these declarations because
      --  they have specific properties. First they are primitives even
      --  if they are not defined in the type scope (the freezing point
      --  is not necessarily in the same scope), furthermore the
      --  predefined equality can be overridden by a user-defined
      --  equality, no body will be generated in this case.

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
             Subtype_Mark             =>
               New_Reference_To (Ret_Type, Loc));
      end if;

      --  If body case, return empty subprogram body. Note that this is
      --  ill-formed, because there is not even a null statement, and
      --  certainly not a return in the function case. The caller is
      --  expected to do surgery on the body to add the appropriate stuff.

      if For_Body then
         return Make_Subprogram_Body (Loc, Spec, Empty_List, Empty);

      --  For the case of Input/Output attributes applied to an abstract type,
      --  generate abstract specifications. These will never be called,
      --  but we need the slots allocated in the dispatching table so
      --  that typ'Class'Input and typ'Class'Output will work properly.

      elsif (Is_TSS (Name, TSS_Stream_Input)
              or else
             Is_TSS (Name, TSS_Stream_Output))
        and then Is_Abstract (Tag_Typ)
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

      return Predef_Spec_Or_Body (Loc,
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
      Renamed_Eq : Node_Id) return List_Id
   is
      Loc       : constant Source_Ptr := Sloc (Tag_Typ);
      Res       : constant List_Id    := New_List;
      Decl      : Node_Id;
      Prim      : Elmt_Id;
      Eq_Needed : Boolean;
      Eq_Name   : Name_Id;
      Ent       : Entity_Id;

   begin
      --  See if we have a predefined "=" operator

      if Present (Renamed_Eq) then
         Eq_Needed := True;
         Eq_Name   := Chars (Renamed_Eq);

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
            end if;

            Next_Elmt (Prim);
         end loop;
      end if;

      --  Body of _Alignment

      Decl := Predef_Spec_Or_Body (Loc,
        Tag_Typ => Tag_Typ,
        Name    => Name_uAlignment,
        Profile => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_X),
            Parameter_Type      => New_Reference_To (Tag_Typ, Loc))),

        Ret_Type => Standard_Integer,
        For_Body => True);

      Set_Handled_Statement_Sequence (Decl,
        Make_Handled_Sequence_Of_Statements (Loc, New_List (
          Make_Return_Statement (Loc,
            Expression =>
              Make_Attribute_Reference (Loc,
                Prefix => Make_Identifier (Loc, Name_X),
                Attribute_Name  => Name_Alignment)))));

      Append_To (Res, Decl);

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
          Make_Return_Statement (Loc,
            Expression =>
              Make_Attribute_Reference (Loc,
                Prefix => Make_Identifier (Loc, Name_X),
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

      --  Skip bodies of _Input and _Output for the abstract case, since
      --  the corresponding specs are abstract (see Predef_Spec_Or_Body)

      if not Is_Abstract (Tag_Typ) then
         if Stream_Operation_OK (Tag_Typ, TSS_Stream_Input)
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
      end if;

      if not Is_Limited_Type (Tag_Typ) then

         --  Body for equality

         if Eq_Needed then

            Decl := Predef_Spec_Or_Body (Loc,
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

              Ret_Type => Standard_Boolean,
              For_Body => True);

            declare
               Def          : constant Node_Id := Parent (Tag_Typ);
               Stmts        : constant List_Id := New_List;
               Variant_Case : Boolean := Has_Discriminants (Tag_Typ);
               Comps        : Node_Id := Empty;
               Typ_Def      : Node_Id := Type_Definition (Def);

            begin
               if Variant_Case then
                  if Nkind (Typ_Def) = N_Derived_Type_Definition then
                     Typ_Def := Record_Extension_Part (Typ_Def);
                  end if;

                  if Present (Typ_Def) then
                     Comps := Component_List (Typ_Def);
                  end if;

                  Variant_Case := Present (Comps)
                    and then Present (Variant_Part (Comps));
               end if;

               if Variant_Case then
                  Append_To (Stmts,
                    Make_Eq_If (Tag_Typ, Discriminant_Specifications (Def)));
                  Append_List_To (Stmts, Make_Eq_Case (Tag_Typ, Comps));
                  Append_To (Stmts,
                    Make_Return_Statement (Loc,
                      Expression => New_Reference_To (Standard_True, Loc)));

               else
                  Append_To (Stmts,
                    Make_Return_Statement (Loc,
                      Expression =>
                        Expand_Record_Equality (Tag_Typ,
                          Typ => Tag_Typ,
                          Lhs => Make_Identifier (Loc, Name_X),
                          Rhs => Make_Identifier (Loc, Name_Y),
                          Bodies => Declarations (Decl))));
               end if;

               Set_Handled_Statement_Sequence (Decl,
                 Make_Handled_Sequence_Of_Statements (Loc, Stmts));
            end;
            Append_To (Res, Decl);
         end if;

         --  Body for dispatching assignment

         Decl := Predef_Spec_Or_Body (Loc,
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

      --  Generate dummy bodies for finalization actions of types that have
      --  no controlled components.

      --  Skip this processing if we are in the finalization routine in the
      --  runtime itself, otherwise we get hopelessly circularly confused!

      if In_Finalization_Root (Tag_Typ) then
         null;

      --  Skip this if finalization is not available

      elsif Restriction_Active (No_Finalization) then
         null;

      elsif (Etype (Tag_Typ) = Tag_Typ or else Is_Controlled (Tag_Typ))
        and then not Has_Controlled_Component (Tag_Typ)
      then
         if not Is_Limited_Type (Tag_Typ) then
            Decl := Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Adjust, True);

            if Is_Controlled (Tag_Typ) then
               Set_Handled_Statement_Sequence (Decl,
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Make_Adjust_Call (
                     Ref          => Make_Identifier (Loc, Name_V),
                     Typ          => Tag_Typ,
                     Flist_Ref    => Make_Identifier (Loc, Name_L),
                     With_Attach  => Make_Identifier (Loc, Name_B))));

            else
               Set_Handled_Statement_Sequence (Decl,
                 Make_Handled_Sequence_Of_Statements (Loc, New_List (
                   Make_Null_Statement (Loc))));
            end if;

            Append_To (Res, Decl);
         end if;

         Decl := Predef_Deep_Spec (Loc, Tag_Typ, TSS_Deep_Finalize, True);

         if Is_Controlled (Tag_Typ) then
            Set_Handled_Statement_Sequence (Decl,
              Make_Handled_Sequence_Of_Statements (Loc,
                Make_Final_Call (
                  Ref         => Make_Identifier (Loc, Name_V),
                  Typ         => Tag_Typ,
                  With_Detach => Make_Identifier (Loc, Name_B))));

         else
            Set_Handled_Statement_Sequence (Decl,
              Make_Handled_Sequence_Of_Statements (Loc, New_List (
                Make_Null_Statement (Loc))));
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
      Loc     : constant Source_Ptr := Sloc (Tag_Typ);
      Res     : constant List_Id    := New_List;
      Prim    : Elmt_Id;
      Frnodes : List_Id;

   begin
      Prim := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Prim) loop
         if Is_Internal (Node (Prim)) then
            Frnodes := Freeze_Entity (Node (Prim), Loc);

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
      Has_Inheritable_Stream_Attribute : Boolean := False;

   begin
      if Is_Limited_Type (Typ)
        and then Is_Tagged_Type (Typ)
        and then Is_Derived_Type (Typ)
      then
         --  Special case of a limited type extension: a default implementation
         --  of the stream attributes Read and Write exists if the attribute
         --  has been specified for an ancestor type.

         Has_Inheritable_Stream_Attribute :=
           Present (Find_Inherited_TSS (Base_Type (Etype (Typ)), Operation));
      end if;

      return
        not (Is_Limited_Type (Typ)
               and then not Has_Inheritable_Stream_Attribute)
          and then RTE_Available (RE_Tag)
          and then RTE_Available (RE_Root_Stream_Type)
          and then not Restriction_Active (No_Dispatch)
          and then not Restriction_Active (No_Streams);
   end Stream_Operation_OK;
end Exp_Ch3;
