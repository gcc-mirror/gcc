------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Eval_Fat;
with Exp_Dist; use Exp_Dist;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Freeze;   use Freeze;
with Gnatvsn;  use Gnatvsn;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sdefault; use Sdefault;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Dim;  use Sem_Dim;
with Sem_Dist; use Sem_Dist;
with Sem_Elab; use Sem_Elab;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with System;
with Stringt;  use Stringt;
with Style;
with Stylesw;  use Stylesw;
with Targparm; use Targparm;
with Ttypes;   use Ttypes;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;   use Urealp;

with System.CRC32; use System.CRC32;

package body Sem_Attr is

   True_Value  : constant Uint := Uint_1;
   False_Value : constant Uint := Uint_0;
   --  Synonyms to be used when these constants are used as Boolean values

   Bad_Attribute : exception;
   --  Exception raised if an error is detected during attribute processing,
   --  used so that we can abandon the processing so we don't run into
   --  trouble with cascaded errors.

   --  The following array is the list of attributes defined in the Ada 83 RM.
   --  In Ada 83 mode, these are the only recognized attributes. In other Ada
   --  modes all these attributes are recognized, even if removed in Ada 95.

   Attribute_83 : constant Attribute_Class_Array := Attribute_Class_Array'(
      Attribute_Address                      |
      Attribute_Aft                          |
      Attribute_Alignment                    |
      Attribute_Base                         |
      Attribute_Callable                     |
      Attribute_Constrained                  |
      Attribute_Count                        |
      Attribute_Delta                        |
      Attribute_Digits                       |
      Attribute_Emax                         |
      Attribute_Epsilon                      |
      Attribute_First                        |
      Attribute_First_Bit                    |
      Attribute_Fore                         |
      Attribute_Image                        |
      Attribute_Large                        |
      Attribute_Last                         |
      Attribute_Last_Bit                     |
      Attribute_Leading_Part                 |
      Attribute_Length                       |
      Attribute_Machine_Emax                 |
      Attribute_Machine_Emin                 |
      Attribute_Machine_Mantissa             |
      Attribute_Machine_Overflows            |
      Attribute_Machine_Radix                |
      Attribute_Machine_Rounds               |
      Attribute_Mantissa                     |
      Attribute_Pos                          |
      Attribute_Position                     |
      Attribute_Pred                         |
      Attribute_Range                        |
      Attribute_Safe_Emax                    |
      Attribute_Safe_Large                   |
      Attribute_Safe_Small                   |
      Attribute_Size                         |
      Attribute_Small                        |
      Attribute_Storage_Size                 |
      Attribute_Succ                         |
      Attribute_Terminated                   |
      Attribute_Val                          |
      Attribute_Value                        |
      Attribute_Width                        => True,
      others                                 => False);

   --  The following array is the list of attributes defined in the Ada 2005
   --  RM which are not defined in Ada 95. These are recognized in Ada 95 mode,
   --  but in Ada 95 they are considered to be implementation defined.

   Attribute_05 : constant Attribute_Class_Array := Attribute_Class_Array'(
      Attribute_Machine_Rounding             |
      Attribute_Mod                          |
      Attribute_Priority                     |
      Attribute_Stream_Size                  |
      Attribute_Wide_Wide_Width              => True,
      others                                 => False);

   --  The following array is the list of attributes defined in the Ada 2012
   --  RM which are not defined in Ada 2005. These are recognized in Ada 95
   --  and Ada 2005 modes, but are considered to be implementation defined.

   Attribute_12 : constant Attribute_Class_Array := Attribute_Class_Array'(
      Attribute_First_Valid                  |
      Attribute_Has_Same_Storage             |
      Attribute_Last_Valid                   |
      Attribute_Max_Alignment_For_Allocation => True,
      others                                 => False);

   --  The following array contains all attributes that imply a modification
   --  of their prefixes or result in an access value. Such prefixes can be
   --  considered as lvalues.

   Attribute_Name_Implies_Lvalue_Prefix : constant Attribute_Class_Array :=
      Attribute_Class_Array'(
      Attribute_Access                       |
      Attribute_Address                      |
      Attribute_Input                        |
      Attribute_Read                         |
      Attribute_Unchecked_Access             |
      Attribute_Unrestricted_Access          => True,
      others                                 => False);

   -----------------------
   -- Local_Subprograms --
   -----------------------

   procedure Eval_Attribute (N : Node_Id);
   --  Performs compile time evaluation of attributes where possible, leaving
   --  the Is_Static_Expression/Raises_Constraint_Error flags appropriately
   --  set, and replacing the node with a literal node if the value can be
   --  computed at compile time. All static attribute references are folded,
   --  as well as a number of cases of non-static attributes that can always
   --  be computed at compile time (e.g. floating-point model attributes that
   --  are applied to non-static subtypes). Of course in such cases, the
   --  Is_Static_Expression flag will not be set on the resulting literal.
   --  Note that the only required action of this procedure is to catch the
   --  static expression cases as described in the RM. Folding of other cases
   --  is done where convenient, but some additional non-static folding is in
   --  Expand_N_Attribute_Reference in cases where this is more convenient.

   function Is_Anonymous_Tagged_Base
     (Anon : Entity_Id;
      Typ  : Entity_Id) return Boolean;
   --  For derived tagged types that constrain parent discriminants we build
   --  an anonymous unconstrained base type. We need to recognize the relation
   --  between the two when analyzing an access attribute for a constrained
   --  component, before the full declaration for Typ has been analyzed, and
   --  where therefore the prefix of the attribute does not match the enclosing
   --  scope.

   procedure Set_Boolean_Result (N : Node_Id; B : Boolean);
   --  Rewrites node N with an occurrence of either Standard_False or
   --  Standard_True, depending on the value of the parameter B. The
   --  result is marked as a static expression.

   function Statically_Denotes_Object (N : Node_Id) return Boolean;
   --  Predicate used to check the legality of the prefix to 'Loop_Entry and
   --  'Old, when the prefix is not an entity name. Current RM specfies that
   --  the prefix must be a direct or expanded name, but it has been proposed
   --  that the prefix be allowed to be a selected component that does not
   --  depend on a discriminant, or an indexed component with static indices.
   --  Current code for this predicate implements this more permissive
   --  implementation.

   -----------------------
   -- Analyze_Attribute --
   -----------------------

   procedure Analyze_Attribute (N : Node_Id) is
      Loc     : constant Source_Ptr   := Sloc (N);
      Aname   : constant Name_Id      := Attribute_Name (N);
      P       : constant Node_Id      := Prefix (N);
      Exprs   : constant List_Id      := Expressions (N);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
      E1      : Node_Id;
      E2      : Node_Id;

      P_Type : Entity_Id;
      --  Type of prefix after analysis

      P_Base_Type : Entity_Id;
      --  Base type of prefix after analysis

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Address_Checks;
      --  Semantic checks for valid use of Address attribute. This was made
      --  a separate routine with the idea of using it for unrestricted access
      --  which seems like it should follow the same rules, but that turned
      --  out to be impractical. So now this is only used for Address.

      procedure Analyze_Access_Attribute;
      --  Used for Access, Unchecked_Access, Unrestricted_Access attributes.
      --  Internally, Id distinguishes which of the three cases is involved.

      procedure Analyze_Attribute_Old_Result
        (Legal   : out Boolean;
         Spec_Id : out Entity_Id);
      --  Common processing for attributes 'Old and 'Result. The routine checks
      --  that the attribute appears in a postcondition-like aspect or pragma
      --  associated with a suitable subprogram or a body. Flag Legal is set
      --  when the above criteria are met. Spec_Id denotes the entity of the
      --  subprogram [body] or Empty if the attribute is illegal.

      procedure Bad_Attribute_For_Predicate;
      --  Output error message for use of a predicate (First, Last, Range) not
      --  allowed with a type that has predicates. If the type is a generic
      --  actual, then the message is a warning, and we generate code to raise
      --  program error with an appropriate reason. No error message is given
      --  for internally generated uses of the attributes. This legality rule
      --  only applies to scalar types.

      procedure Check_Array_Or_Scalar_Type;
      --  Common procedure used by First, Last, Range attribute to check
      --  that the prefix is a constrained array or scalar type, or a name
      --  of an array object, and that an argument appears only if appropriate
      --  (i.e. only in the array case).

      procedure Check_Array_Type;
      --  Common semantic checks for all array attributes. Checks that the
      --  prefix is a constrained array type or the name of an array object.
      --  The error message for non-arrays is specialized appropriately.

      procedure Check_Asm_Attribute;
      --  Common semantic checks for Asm_Input and Asm_Output attributes

      procedure Check_Component;
      --  Common processing for Bit_Position, First_Bit, Last_Bit, and
      --  Position. Checks prefix is an appropriate selected component.

      procedure Check_Decimal_Fixed_Point_Type;
      --  Check that prefix of attribute N is a decimal fixed-point type

      procedure Check_Dereference;
      --  If the prefix of attribute is an object of an access type, then
      --  introduce an explicit dereference, and adjust P_Type accordingly.

      procedure Check_Discrete_Type;
      --  Verify that prefix of attribute N is a discrete type

      procedure Check_E0;
      --  Check that no attribute arguments are present

      procedure Check_Either_E0_Or_E1;
      --  Check that there are zero or one attribute arguments present

      procedure Check_E1;
      --  Check that exactly one attribute argument is present

      procedure Check_E2;
      --  Check that two attribute arguments are present

      procedure Check_Enum_Image;
      --  If the prefix type of 'Image is an enumeration type, set all its
      --  literals as referenced, since the image function could possibly end
      --  up referencing any of the literals indirectly. Same for Enum_Val.
      --  Set the flag only if the reference is in the main code unit. Same
      --  restriction when resolving 'Value; otherwise an improperly set
      --  reference when analyzing an inlined body will lose a proper
      --  warning on a useless with_clause.

      procedure Check_First_Last_Valid;
      --  Perform all checks for First_Valid and Last_Valid attributes

      procedure Check_Fixed_Point_Type;
      --  Verify that prefix of attribute N is a fixed type

      procedure Check_Fixed_Point_Type_0;
      --  Verify that prefix of attribute N is a fixed type and that
      --  no attribute expressions are present.

      procedure Check_Floating_Point_Type;
      --  Verify that prefix of attribute N is a float type

      procedure Check_Floating_Point_Type_0;
      --  Verify that prefix of attribute N is a float type and that
      --  no attribute expressions are present.

      procedure Check_Floating_Point_Type_1;
      --  Verify that prefix of attribute N is a float type and that
      --  exactly one attribute expression is present.

      procedure Check_Floating_Point_Type_2;
      --  Verify that prefix of attribute N is a float type and that
      --  two attribute expressions are present

      procedure Check_SPARK_05_Restriction_On_Attribute;
      --  Issue an error in formal mode because attribute N is allowed

      procedure Check_Integer_Type;
      --  Verify that prefix of attribute N is an integer type

      procedure Check_Modular_Integer_Type;
      --  Verify that prefix of attribute N is a modular integer type

      procedure Check_Not_CPP_Type;
      --  Check that P (the prefix of the attribute) is not an CPP type
      --  for which no Ada predefined primitive is available.

      procedure Check_Not_Incomplete_Type;
      --  Check that P (the prefix of the attribute) is not an incomplete
      --  type or a private type for which no full view has been given.

      procedure Check_Object_Reference (P : Node_Id);
      --  Check that P is an object reference

      procedure Check_Object_Reference_Image (Str_Typ : Entity_Id);
      --  Verify that the prefix of an image attribute is an object reference
      --  and set the Etype of the prefix to that specified by Str_Typ.

      procedure Check_PolyORB_Attribute;
      --  Validity checking for PolyORB/DSA attribute

      procedure Check_Program_Unit;
      --  Verify that prefix of attribute N is a program unit

      procedure Check_Real_Type;
      --  Verify that prefix of attribute N is fixed or float type

      procedure Check_Scalar_Type;
      --  Verify that prefix of attribute N is a scalar type

      procedure Check_Standard_Prefix;
      --  Verify that prefix of attribute N is package Standard. Also checks
      --  that there are no arguments.

      procedure Check_Stream_Attribute (Nam : TSS_Name_Type);
      --  Validity checking for stream attribute. Nam is the TSS name of the
      --  corresponding possible defined attribute function (e.g. for the
      --  Read attribute, Nam will be TSS_Stream_Read).

      procedure Check_System_Prefix;
      --  Verify that prefix of attribute N is package System

      procedure Check_Task_Prefix;
      --  Verify that prefix of attribute N is a task or task type

      procedure Check_Type;
      --  Verify that the prefix of attribute N is a type

      procedure Check_Unit_Name (Nod : Node_Id);
      --  Check that Nod is of the form of a library unit name, i.e that
      --  it is an identifier, or a selected component whose prefix is
      --  itself of the form of a library unit name. Note that this is
      --  quite different from Check_Program_Unit, since it only checks
      --  the syntactic form of the name, not the semantic identity. This
      --  is because it is used with attributes (Elab_Body, Elab_Spec and
      --  Elaborated) which can refer to non-visible unit.

      procedure Error_Attr (Msg : String; Error_Node : Node_Id);
      pragma No_Return (Error_Attr);
      procedure Error_Attr;
      pragma No_Return (Error_Attr);
      --  Posts error using Error_Msg_N at given node, sets type of attribute
      --  node to Any_Type, and then raises Bad_Attribute to avoid any further
      --  semantic processing. The message typically contains a % insertion
      --  character which is replaced by the attribute name. The call with
      --  no arguments is used when the caller has already generated the
      --  required error messages.

      procedure Error_Attr_P (Msg : String);
      pragma No_Return (Error_Attr);
      --  Like Error_Attr, but error is posted at the start of the prefix

      procedure Legal_Formal_Attribute;
      --  Common processing for attributes Definite and Has_Discriminants.
      --  Checks that prefix is generic indefinite formal type.

      procedure Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements;
      --  Common processing for attributes Max_Alignment_For_Allocation and
      --  Max_Size_In_Storage_Elements.

      procedure Min_Max;
      --  Common processing for attributes Max and Min

      procedure Standard_Attribute (Val : Int);
      --  Used to process attributes whose prefix is package Standard which
      --  yield values of type Universal_Integer. The attribute reference
      --  node is rewritten with an integer literal of the given value which
      --  is marked as static.

      procedure Uneval_Old_Msg;
      --  Called when Loop_Entry or Old is used in a potentially unevaluated
      --  expression. Generates appropriate message or warning depending on
      --  the setting of Opt.Uneval_Old (or flags in an N_Aspect_Specification
      --  node in the aspect case).

      procedure Unexpected_Argument (En : Node_Id);
      --  Signal unexpected attribute argument (En is the argument)

      procedure Validate_Non_Static_Attribute_Function_Call;
      --  Called when processing an attribute that is a function call to a
      --  non-static function, i.e. an attribute function that either takes
      --  non-scalar arguments or returns a non-scalar result. Verifies that
      --  such a call does not appear in a preelaborable context.

      --------------------
      -- Address_Checks --
      --------------------

      procedure Address_Checks is
      begin
         --  An Address attribute created by expansion is legal even when it
         --  applies to other entity-denoting expressions.

         if not Comes_From_Source (N) then
            return;

         --  Address attribute on a protected object self reference is legal

         elsif Is_Protected_Self_Reference (P) then
            return;

         --  Address applied to an entity

         elsif Is_Entity_Name (P) then
            declare
               Ent : constant Entity_Id := Entity (P);

            begin
               if Is_Subprogram (Ent) then
                  Set_Address_Taken (Ent);
                  Kill_Current_Values (Ent);

                  --  An Address attribute is accepted when generated by the
                  --  compiler for dispatching operation, and an error is
                  --  issued once the subprogram is frozen (to avoid confusing
                  --  errors about implicit uses of Address in the dispatch
                  --  table initialization).

                  if Has_Pragma_Inline_Always (Entity (P))
                    and then Comes_From_Source (P)
                  then
                     Error_Attr_P
                       ("prefix of % attribute cannot be Inline_Always "
                        & "subprogram");

                  --  It is illegal to apply 'Address to an intrinsic
                  --  subprogram. This is now formalized in AI05-0095.
                  --  In an instance, an attempt to obtain 'Address of an
                  --  intrinsic subprogram (e.g the renaming of a predefined
                  --  operator that is an actual) raises Program_Error.

                  elsif Convention (Ent) = Convention_Intrinsic then
                     if In_Instance then
                        Rewrite (N,
                          Make_Raise_Program_Error (Loc,
                            Reason => PE_Address_Of_Intrinsic));

                     else
                        Error_Msg_Name_1 := Aname;
                        Error_Msg_N
                         ("cannot take % of intrinsic subprogram", N);
                     end if;

                  --  Issue an error if prefix denotes an eliminated subprogram

                  else
                     Check_For_Eliminated_Subprogram (P, Ent);
                  end if;

               --  Object or label reference

               elsif Is_Object (Ent) or else Ekind (Ent) = E_Label then
                  Set_Address_Taken (Ent);

                  --  Deal with No_Implicit_Aliasing restriction

                  if Restriction_Check_Required (No_Implicit_Aliasing) then
                     if not Is_Aliased_View (P) then
                        Check_Restriction (No_Implicit_Aliasing, P);
                     else
                        Check_No_Implicit_Aliasing (P);
                     end if;
                  end if;

                  --  If we have an address of an object, and the attribute
                  --  comes from source, then set the object as potentially
                  --  source modified. We do this because the resulting address
                  --  can potentially be used to modify the variable and we
                  --  might not detect this, leading to some junk warnings.

                  Set_Never_Set_In_Source (Ent, False);

               --  Allow Address to be applied to task or protected type,
               --  returning null address (what is that about???)

               elsif (Is_Concurrent_Type (Etype (Ent))
                       and then Etype (Ent) = Base_Type (Ent))
                 or else Ekind (Ent) = E_Package
                 or else Is_Generic_Unit (Ent)
               then
                  Rewrite (N,
                    New_Occurrence_Of (RTE (RE_Null_Address), Sloc (N)));

               --  Anything else is illegal

               else
                  Error_Attr ("invalid prefix for % attribute", P);
               end if;
            end;

         --  Object is OK

         elsif Is_Object_Reference (P) then
            return;

         --  Subprogram called using dot notation

         elsif Nkind (P) = N_Selected_Component
           and then Is_Subprogram (Entity (Selector_Name (P)))
         then
            return;

         --  What exactly are we allowing here ??? and is this properly
         --  documented in the sinfo documentation for this node ???

         elsif Relaxed_RM_Semantics
           and then Nkind (P) = N_Attribute_Reference
         then
            return;

         --  All other non-entity name cases are illegal

         else
            Error_Attr ("invalid prefix for % attribute", P);
         end if;
      end Address_Checks;

      ------------------------------
      -- Analyze_Access_Attribute --
      ------------------------------

      procedure Analyze_Access_Attribute is
         Acc_Type : Entity_Id;

         Scop : Entity_Id;
         Typ  : Entity_Id;

         function Build_Access_Object_Type (DT : Entity_Id) return Entity_Id;
         --  Build an access-to-object type whose designated type is DT,
         --  and whose Ekind is appropriate to the attribute type. The
         --  type that is constructed is returned as the result.

         procedure Build_Access_Subprogram_Type (P : Node_Id);
         --  Build an access to subprogram whose designated type is the type of
         --  the prefix. If prefix is overloaded, so is the node itself. The
         --  result is stored in Acc_Type.

         function OK_Self_Reference return Boolean;
         --  An access reference whose prefix is a type can legally appear
         --  within an aggregate, where it is obtained by expansion of
         --  a defaulted aggregate. The enclosing aggregate that contains
         --  the self-referenced is flagged so that the self-reference can
         --  be expanded into a reference to the target object (see exp_aggr).

         ------------------------------
         -- Build_Access_Object_Type --
         ------------------------------

         function Build_Access_Object_Type (DT : Entity_Id) return Entity_Id is
            Typ : constant Entity_Id :=
                    New_Internal_Entity
                      (E_Access_Attribute_Type, Current_Scope, Loc, 'A');
         begin
            Set_Etype                     (Typ, Typ);
            Set_Is_Itype                  (Typ);
            Set_Associated_Node_For_Itype (Typ, N);
            Set_Directly_Designated_Type  (Typ, DT);
            return Typ;
         end Build_Access_Object_Type;

         ----------------------------------
         -- Build_Access_Subprogram_Type --
         ----------------------------------

         procedure Build_Access_Subprogram_Type (P : Node_Id) is
            Index : Interp_Index;
            It    : Interp;

            procedure Check_Local_Access (E : Entity_Id);
            --  Deal with possible access to local subprogram. If we have such
            --  an access, we set a flag to kill all tracked values on any call
            --  because this access value may be passed around, and any called
            --  code might use it to access a local procedure which clobbers a
            --  tracked value. If the scope is a loop or block, indicate that
            --  value tracking is disabled for the enclosing subprogram.

            function Get_Kind (E : Entity_Id) return Entity_Kind;
            --  Distinguish between access to regular/protected subprograms

            ------------------------
            -- Check_Local_Access --
            ------------------------

            procedure Check_Local_Access (E : Entity_Id) is
            begin
               if not Is_Library_Level_Entity (E) then
                  Set_Suppress_Value_Tracking_On_Call (Current_Scope);
                  Set_Suppress_Value_Tracking_On_Call
                    (Nearest_Dynamic_Scope (Current_Scope));
               end if;
            end Check_Local_Access;

            --------------
            -- Get_Kind --
            --------------

            function Get_Kind (E : Entity_Id) return Entity_Kind is
            begin
               if Convention (E) = Convention_Protected then
                  return E_Access_Protected_Subprogram_Type;
               else
                  return E_Access_Subprogram_Type;
               end if;
            end Get_Kind;

         --  Start of processing for Build_Access_Subprogram_Type

         begin
            --  In the case of an access to subprogram, use the name of the
            --  subprogram itself as the designated type. Type-checking in
            --  this case compares the signatures of the designated types.

            --  Note: This fragment of the tree is temporarily malformed
            --  because the correct tree requires an E_Subprogram_Type entity
            --  as the designated type. In most cases this designated type is
            --  later overridden by the semantics with the type imposed by the
            --  context during the resolution phase. In the specific case of
            --  the expression Address!(Prim'Unrestricted_Access), used to
            --  initialize slots of dispatch tables, this work will be done by
            --  the expander (see Exp_Aggr).

            --  The reason to temporarily add this kind of node to the tree
            --  instead of a proper E_Subprogram_Type itype, is the following:
            --  in case of errors found in the source file we report better
            --  error messages. For example, instead of generating the
            --  following error:

            --      "expected access to subprogram with profile
            --       defined at line X"

            --  we currently generate:

            --      "expected access to function Z defined at line X"

            Set_Etype (N, Any_Type);

            if not Is_Overloaded (P) then
               Check_Local_Access (Entity (P));

               if not Is_Intrinsic_Subprogram (Entity (P)) then
                  Acc_Type := Create_Itype (Get_Kind (Entity (P)), N);
                  Set_Is_Public (Acc_Type, False);
                  Set_Etype (Acc_Type, Acc_Type);
                  Set_Convention (Acc_Type, Convention (Entity (P)));
                  Set_Directly_Designated_Type (Acc_Type, Entity (P));
                  Set_Etype (N, Acc_Type);
                  Freeze_Before (N, Acc_Type);
               end if;

            else
               Get_First_Interp (P, Index, It);
               while Present (It.Nam) loop
                  Check_Local_Access (It.Nam);

                  if not Is_Intrinsic_Subprogram (It.Nam) then
                     Acc_Type := Create_Itype (Get_Kind (It.Nam), N);
                     Set_Is_Public (Acc_Type, False);
                     Set_Etype (Acc_Type, Acc_Type);
                     Set_Convention (Acc_Type, Convention (It.Nam));
                     Set_Directly_Designated_Type (Acc_Type, It.Nam);
                     Add_One_Interp (N, Acc_Type, Acc_Type);
                     Freeze_Before (N, Acc_Type);
                  end if;

                  Get_Next_Interp (Index, It);
               end loop;
            end if;

            --  Cannot be applied to intrinsic. Looking at the tests above,
            --  the only way Etype (N) can still be set to Any_Type is if
            --  Is_Intrinsic_Subprogram was True for some referenced entity.

            if Etype (N) = Any_Type then
               Error_Attr_P ("prefix of % attribute cannot be intrinsic");
            end if;
         end Build_Access_Subprogram_Type;

         ----------------------
         -- OK_Self_Reference --
         ----------------------

         function OK_Self_Reference return Boolean is
            Par : Node_Id;

         begin
            Par := Parent (N);
            while Present (Par)
              and then
               (Nkind (Par) = N_Component_Association
                 or else Nkind (Par) in N_Subexpr)
            loop
               if Nkind_In (Par, N_Aggregate, N_Extension_Aggregate) then
                  if Etype (Par) = Typ then
                     Set_Has_Self_Reference (Par);

                     --  Check the context: the aggregate must be part of the
                     --  initialization of a type or component, or it is the
                     --  resulting expansion in an initialization procedure.

                     if Is_Init_Proc (Current_Scope) then
                        return True;
                     else
                        Par := Parent (Par);
                        while Present (Par) loop
                           if Nkind (Par) = N_Full_Type_Declaration then
                              return True;
                           end if;

                           Par := Parent (Par);
                        end loop;
                     end if;

                     return False;
                  end if;
               end if;

               Par := Parent (Par);
            end loop;

            --  No enclosing aggregate, or not a self-reference

            return False;
         end OK_Self_Reference;

      --  Start of processing for Analyze_Access_Attribute

      begin
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E0;

         if Nkind (P) = N_Character_Literal then
            Error_Attr_P
              ("prefix of % attribute cannot be enumeration literal");
         end if;

         --  Case of access to subprogram

         if Is_Entity_Name (P) and then Is_Overloadable (Entity (P)) then
            if Has_Pragma_Inline_Always (Entity (P)) then
               Error_Attr_P
                 ("prefix of % attribute cannot be Inline_Always subprogram");

            elsif Aname = Name_Unchecked_Access then
               Error_Attr ("attribute% cannot be applied to a subprogram", P);
            end if;

            --  Issue an error if the prefix denotes an eliminated subprogram

            Check_For_Eliminated_Subprogram (P, Entity (P));

            --  Check for obsolescent subprogram reference

            Check_Obsolescent_2005_Entity (Entity (P), P);

            --  Build the appropriate subprogram type

            Build_Access_Subprogram_Type (P);

            --  For P'Access or P'Unrestricted_Access, where P is a nested
            --  subprogram, we might be passing P to another subprogram (but we
            --  don't check that here), which might call P. P could modify
            --  local variables, so we need to kill current values. It is
            --  important not to do this for library-level subprograms, because
            --  Kill_Current_Values is very inefficient in the case of library
            --  level packages with lots of tagged types.

            if Is_Library_Level_Entity (Entity (Prefix (N))) then
               null;

            --  Do not kill values on nodes initializing dispatch tables
            --  slots. The construct Prim_Ptr!(Prim'Unrestricted_Access)
            --  is currently generated by the expander only for this
            --  purpose. Done to keep the quality of warnings currently
            --  generated by the compiler (otherwise any declaration of
            --  a tagged type cleans constant indications from its scope).

            elsif Nkind (Parent (N)) = N_Unchecked_Type_Conversion
              and then (Etype (Parent (N)) = RTE (RE_Prim_Ptr)
                          or else
                        Etype (Parent (N)) = RTE (RE_Size_Ptr))
              and then Is_Dispatching_Operation
                         (Directly_Designated_Type (Etype (N)))
            then
               null;

            else
               Kill_Current_Values;
            end if;

            --  In the static elaboration model, treat the attribute reference
            --  as a call for elaboration purposes.  Suppress this treatment
            --  under debug flag. In any case, we are all done.

            if not Dynamic_Elaboration_Checks and not Debug_Flag_Dot_UU then
               Check_Elab_Call (N);
            end if;

            return;

         --  Component is an operation of a protected type

         elsif Nkind (P) = N_Selected_Component
           and then Is_Overloadable (Entity (Selector_Name (P)))
         then
            if Ekind (Entity (Selector_Name (P))) = E_Entry then
               Error_Attr_P ("prefix of % attribute must be subprogram");
            end if;

            Build_Access_Subprogram_Type (Selector_Name (P));
            return;
         end if;

         --  Deal with incorrect reference to a type, but note that some
         --  accesses are allowed: references to the current type instance,
         --  or in Ada 2005 self-referential pointer in a default-initialized
         --  aggregate.

         if Is_Entity_Name (P) then
            Typ := Entity (P);

            --  The reference may appear in an aggregate that has been expanded
            --  into a loop. Locate scope of type definition, if any.

            Scop := Current_Scope;
            while Ekind (Scop) = E_Loop loop
               Scop := Scope (Scop);
            end loop;

            if Is_Type (Typ) then

               --  OK if we are within the scope of a limited type
               --  let's mark the component as having per object constraint

               if Is_Anonymous_Tagged_Base (Scop, Typ) then
                  Typ := Scop;
                  Set_Entity (P, Typ);
                  Set_Etype  (P, Typ);
               end if;

               if Typ = Scop then
                  declare
                     Q : Node_Id := Parent (N);

                  begin
                     while Present (Q)
                       and then Nkind (Q) /= N_Component_Declaration
                     loop
                        Q := Parent (Q);
                     end loop;

                     if Present (Q) then
                        Set_Has_Per_Object_Constraint
                          (Defining_Identifier (Q), True);
                     end if;
                  end;

                  if Nkind (P) = N_Expanded_Name then
                     Error_Msg_F
                       ("current instance prefix must be a direct name", P);
                  end if;

                  --  If a current instance attribute appears in a component
                  --  constraint it must appear alone; other contexts (spec-
                  --  expressions, within a task body) are not subject to this
                  --  restriction.

                  if not In_Spec_Expression
                    and then not Has_Completion (Scop)
                    and then not
                      Nkind_In (Parent (N), N_Discriminant_Association,
                                            N_Index_Or_Discriminant_Constraint)
                  then
                     Error_Msg_N
                       ("current instance attribute must appear alone", N);
                  end if;

                  if Is_CPP_Class (Root_Type (Typ)) then
                     Error_Msg_N
                       ("??current instance unsupported for derivations of "
                        & "'C'P'P types", N);
                  end if;

               --  OK if we are in initialization procedure for the type
               --  in question, in which case the reference to the type
               --  is rewritten as a reference to the current object.

               elsif Ekind (Scop) = E_Procedure
                 and then Is_Init_Proc (Scop)
                 and then Etype (First_Formal (Scop)) = Typ
               then
                  Rewrite (N,
                    Make_Attribute_Reference (Loc,
                      Prefix         => Make_Identifier (Loc, Name_uInit),
                      Attribute_Name => Name_Unrestricted_Access));
                  Analyze (N);
                  return;

               --  OK if a task type, this test needs sharpening up ???

               elsif Is_Task_Type (Typ) then
                  null;

               --  OK if self-reference in an aggregate in Ada 2005, and
               --  the reference comes from a copied default expression.

               --  Note that we check legality of self-reference even if the
               --  expression comes from source, e.g. when a single component
               --  association in an aggregate has a box association.

               elsif Ada_Version >= Ada_2005
                 and then OK_Self_Reference
               then
                  null;

               --  OK if reference to current instance of a protected object

               elsif Is_Protected_Self_Reference (P) then
                  null;

               --  Otherwise we have an error case

               else
                  Error_Attr ("% attribute cannot be applied to type", P);
                  return;
               end if;
            end if;
         end if;

         --  If we fall through, we have a normal access to object case

         --  Unrestricted_Access is (for now) legal wherever an allocator would
         --  be legal, so its Etype is set to E_Allocator. The expected type
         --  of the other attributes is a general access type, and therefore
         --  we label them with E_Access_Attribute_Type.

         if not Is_Overloaded (P) then
            Acc_Type := Build_Access_Object_Type (P_Type);
            Set_Etype (N, Acc_Type);

         else
            declare
               Index : Interp_Index;
               It    : Interp;
            begin
               Set_Etype (N, Any_Type);
               Get_First_Interp (P, Index, It);
               while Present (It.Typ) loop
                  Acc_Type := Build_Access_Object_Type (It.Typ);
                  Add_One_Interp (N, Acc_Type, Acc_Type);
                  Get_Next_Interp (Index, It);
               end loop;
            end;
         end if;

         --  Special cases when we can find a prefix that is an entity name

         declare
            PP  : Node_Id;
            Ent : Entity_Id;

         begin
            PP := P;
            loop
               if Is_Entity_Name (PP) then
                  Ent := Entity (PP);

                  --  If we have an access to an object, and the attribute
                  --  comes from source, then set the object as potentially
                  --  source modified. We do this because the resulting access
                  --  pointer can be used to modify the variable, and we might
                  --  not detect this, leading to some junk warnings.

                  --  We only do this for source references, since otherwise
                  --  we can suppress warnings, e.g. from the unrestricted
                  --  access generated for validity checks in -gnatVa mode.

                  if Comes_From_Source (N) then
                     Set_Never_Set_In_Source (Ent, False);
                  end if;

                  --  Mark entity as address taken in the case of
                  --  'Unrestricted_Access or subprograms, and kill current
                  --  values.

                  if Aname = Name_Unrestricted_Access
                    or else Is_Subprogram (Ent)
                  then
                     Set_Address_Taken (Ent);
                  end if;

                  Kill_Current_Values (Ent);
                  exit;

               elsif Nkind_In (PP, N_Selected_Component,
                                   N_Indexed_Component)
               then
                  PP := Prefix (PP);

               else
                  exit;
               end if;
            end loop;
         end;

         --  Check for aliased view. We allow a nonaliased prefix when within
         --  an instance because the prefix may have been a tagged formal
         --  object, which is defined to be aliased even when the actual
         --  might not be (other instance cases will have been caught in the
         --  generic). Similarly, within an inlined body we know that the
         --  attribute is legal in the original subprogram, and therefore
         --  legal in the expansion.

         if not Is_Aliased_View (P)
           and then not In_Instance
           and then not In_Inlined_Body
           and then Comes_From_Source (N)
         then
            --  Here we have a non-aliased view. This is illegal unless we
            --  have the case of Unrestricted_Access, where for now we allow
            --  this (we will reject later if expected type is access to an
            --  unconstrained array with a thin pointer).

            --  No need for an error message on a generated access reference
            --  for the controlling argument in a dispatching call: error will
            --  be reported when resolving the call.

            if Aname /= Name_Unrestricted_Access then
               Error_Attr_P ("prefix of % attribute must be aliased");
               Check_No_Implicit_Aliasing (P);

            --  For Unrestricted_Access, record that prefix is not aliased
            --  to simplify legality check later on.

            else
               Set_Non_Aliased_Prefix (N);
            end if;

         --  If we have an aliased view, and we have Unrestricted_Access, then
         --  output a warning that Unchecked_Access would have been fine, and
         --  change the node to be Unchecked_Access.

         else
            --  For now, hold off on this change ???

            null;
         end if;
      end Analyze_Access_Attribute;

      ----------------------------------
      -- Analyze_Attribute_Old_Result --
      ----------------------------------

      procedure Analyze_Attribute_Old_Result
        (Legal   : out Boolean;
         Spec_Id : out Entity_Id)
      is
         procedure Check_Placement_In_Check (Prag : Node_Id);
         --  Verify that the attribute appears within pragma Check that mimics
         --  a postcondition.

         procedure Check_Placement_In_Contract_Cases (Prag : Node_Id);
         --  Verify that the attribute appears within a consequence of aspect
         --  or pragma Contract_Cases denoted by Prag.

         procedure Check_Placement_In_Test_Case (Prag : Node_Id);
         --  Verify that the attribute appears within the "Ensures" argument of
         --  aspect or pragma Test_Case denoted by Prag.

         function Is_Within
           (Nod      : Node_Id;
            Encl_Nod : Node_Id) return Boolean;
         --  Subsidiary to Check_Placemenet_In_XXX. Determine whether arbitrary
         --  node Nod is within enclosing node Encl_Nod.

         procedure Placement_Error;
         --  Emit a general error when the attributes does not appear in a
         --  postcondition-like aspect or pragma.

         ------------------------------
         -- Check_Placement_In_Check --
         ------------------------------

         procedure Check_Placement_In_Check (Prag : Node_Id) is
            Args : constant List_Id := Pragma_Argument_Associations (Prag);
            Nam  : constant Name_Id := Chars (Get_Pragma_Arg (First (Args)));

         begin
            --  The "Name" argument of pragma Check denotes a postcondition

            if Nam_In (Nam, Name_Post,
                            Name_Post_Class,
                            Name_Postcondition,
                            Name_Refined_Post)
            then
               null;

            --  Otherwise the placement of the attribute is illegal

            else
               Placement_Error;
            end if;
         end Check_Placement_In_Check;

         ---------------------------------------
         -- Check_Placement_In_Contract_Cases --
         ---------------------------------------

         procedure Check_Placement_In_Contract_Cases (Prag : Node_Id) is
            Arg   : Node_Id;
            Cases : Node_Id;
            CCase : Node_Id;

         begin
            --  Obtain the argument of the aspect or pragma

            if Nkind (Prag) = N_Aspect_Specification then
               Arg := Prag;
            else
               Arg := First (Pragma_Argument_Associations (Prag));
            end if;

            Cases := Expression (Arg);

            if Present (Component_Associations (Cases)) then
               CCase := First (Component_Associations (Cases));
               while Present (CCase) loop

                  --  Detect whether the attribute appears within the
                  --  consequence of the current contract case.

                  if Nkind (CCase) = N_Component_Association
                    and then Is_Within (N, Expression (CCase))
                  then
                     return;
                  end if;

                  Next (CCase);
               end loop;
            end if;

            --  Otherwise aspect or pragma Contract_Cases is either malformed
            --  or the attribute does not appear within a consequence.

            Error_Attr
              ("attribute % must appear in the consequence of a contract case",
               P);
         end Check_Placement_In_Contract_Cases;

         ----------------------------------
         -- Check_Placement_In_Test_Case --
         ----------------------------------

         procedure Check_Placement_In_Test_Case (Prag : Node_Id) is
            Arg : constant Node_Id :=
                    Test_Case_Arg
                      (Prag        => Prag,
                       Arg_Nam     => Name_Ensures,
                       From_Aspect => Nkind (Prag) = N_Aspect_Specification);

         begin
            --  Detect whether the attribute appears within the "Ensures"
            --  expression of aspect or pragma Test_Case.

            if Present (Arg) and then Is_Within (N, Arg) then
               null;

            else
               Error_Attr
                 ("attribute % must appear in the ensures expression of a "
                  & "test case", P);
            end if;
         end Check_Placement_In_Test_Case;

         ---------------
         -- Is_Within --
         ---------------

         function Is_Within
           (Nod      : Node_Id;
            Encl_Nod : Node_Id) return Boolean
         is
            Par : Node_Id;

         begin
            Par := Nod;
            while Present (Par) loop
               if Par = Encl_Nod then
                  return True;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Par := Parent (Par);
            end loop;

            return False;
         end Is_Within;

         ---------------------
         -- Placement_Error --
         ---------------------

         procedure Placement_Error is
         begin
            if Aname = Name_Old then
               Error_Attr ("attribute % can only appear in postcondition", P);

            --  Specialize the error message for attribute 'Result

            else
               Error_Attr
                 ("attribute % can only appear in postcondition of function",
                  P);
            end if;
         end Placement_Error;

         --  Local variables

         Prag      : Node_Id;
         Prag_Nam  : Name_Id;
         Subp_Decl : Node_Id;

      --  Start of processing for Analyze_Attribute_Old_Result

      begin
         --  Assume that the attribute is illegal

         Legal   := False;
         Spec_Id := Empty;

         --  Traverse the parent chain to find the aspect or pragma where the
         --  attribute resides.

         Prag := N;
         while Present (Prag) loop
            if Nkind_In (Prag, N_Aspect_Specification, N_Pragma) then
               exit;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Prag) then
               exit;
            end if;

            Prag := Parent (Prag);
         end loop;

         --  The attribute is allowed to appear only in postcondition-like
         --  aspects or pragmas.

         if Nkind_In (Prag, N_Aspect_Specification, N_Pragma) then
            if Nkind (Prag) = N_Aspect_Specification then
               Prag_Nam := Chars (Identifier (Prag));
            else
               Prag_Nam := Pragma_Name (Prag);
            end if;

            if Prag_Nam = Name_Check then
               Check_Placement_In_Check (Prag);

            elsif Prag_Nam = Name_Contract_Cases then
               Check_Placement_In_Contract_Cases (Prag);

            --  Attribute 'Result is allowed to appear in aspect or pragma
            --  [Refined_]Depends (SPARK RM 6.1.5(11)).

            elsif Nam_In (Prag_Nam, Name_Depends, Name_Refined_Depends)
              and then Aname = Name_Result
            then
               null;

            elsif Nam_In (Prag_Nam, Name_Post,
                                    Name_Post_Class,
                                    Name_Postcondition,
                                    Name_Refined_Post)
            then
               null;

            elsif Prag_Nam = Name_Test_Case then
               Check_Placement_In_Test_Case (Prag);

            else
               Placement_Error;
               return;
            end if;

         --  Otherwise the placement of the attribute is illegal

         else
            Placement_Error;
            return;
         end if;

         --  Find the related subprogram subject to the aspect or pragma

         if Nkind (Prag) = N_Aspect_Specification then
            Subp_Decl := Parent (Prag);
         else
            Subp_Decl := Find_Related_Declaration_Or_Body (Prag);
         end if;

         --  The aspect or pragma where the attribute resides should be
         --  associated with a subprogram declaration or a body. If this is not
         --  the case, then the aspect or pragma is illegal. Return as analysis
         --  cannot be carried out. Note that it is legal to have the aspect
         --  appear on a subprogram renaming, when the renamed entity is an
         --  attribute reference.

         --  Generating C code the internally built nested _postcondition
         --  subprograms are inlined; after expanded, inlined aspects are
         --  located in the internal block generated by the frontend.

         if Nkind (Subp_Decl) = N_Block_Statement
           and then Modify_Tree_For_C
           and then In_Inlined_Body
         then
            null;

         elsif not Nkind_In (Subp_Decl, N_Abstract_Subprogram_Declaration,
                                        N_Entry_Declaration,
                                        N_Generic_Subprogram_Declaration,
                                        N_Subprogram_Body,
                                        N_Subprogram_Body_Stub,
                                        N_Subprogram_Declaration,
                                        N_Subprogram_Renaming_Declaration)
         then
            return;
         end if;

         --  If we get here, then the attribute is legal

         Legal   := True;
         Spec_Id := Unique_Defining_Entity (Subp_Decl);

         --  When generating C code, nested _postcondition subprograms are
         --  inlined by the front end to avoid problems (when unnested) with
         --  referenced itypes. Handle that here, since as part of inlining the
         --  expander nests subprogram within a dummy procedure named _parent
         --  (see Build_Postconditions_Procedure and Build_Body_To_Inline).
         --  Hence, in this context, the spec_id of _postconditions is the
         --  enclosing scope.

         if Modify_Tree_For_C
           and then Chars (Spec_Id) = Name_uParent
           and then Chars (Scope (Spec_Id)) = Name_uPostconditions
         then
            --  This situation occurs only when preanalyzing the inlined body

            pragma Assert (not Full_Analysis);

            Spec_Id := Scope (Spec_Id);
            pragma Assert (Is_Inlined (Spec_Id));
         end if;
      end Analyze_Attribute_Old_Result;

      ---------------------------------
      -- Bad_Attribute_For_Predicate --
      ---------------------------------

      procedure Bad_Attribute_For_Predicate is
      begin
         if Is_Scalar_Type (P_Type)
           and then Comes_From_Source (N)
         then
            Error_Msg_Name_1 := Aname;
            Bad_Predicated_Subtype_Use
              ("type& has predicates, attribute % not allowed", N, P_Type);
         end if;
      end Bad_Attribute_For_Predicate;

      --------------------------------
      -- Check_Array_Or_Scalar_Type --
      --------------------------------

      procedure Check_Array_Or_Scalar_Type is
         function In_Aspect_Specification return Boolean;
         --  A current instance of a type in an aspect specification is an
         --  object and not a type, and therefore cannot be of a scalar type
         --  in the prefix of one of the array attributes if the attribute
         --  reference is part of an aspect expression.

         -----------------------------
         -- In_Aspect_Specification --
         -----------------------------

         function In_Aspect_Specification return Boolean is
            P : Node_Id;

         begin
            P := Parent (N);
            while Present (P) loop
               if Nkind (P) = N_Aspect_Specification then
                  return P_Type = Entity (P);

               elsif Nkind (P) in N_Declaration then
                  return False;
               end if;

               P := Parent (P);
            end loop;

            return False;
         end In_Aspect_Specification;

         --  Local variables

         Dims  : Int;
         Index : Entity_Id;

      --  Start of processing for Check_Array_Or_Scalar_Type

      begin
         --  Case of string literal or string literal subtype. These cases
         --  cannot arise from legal Ada code, but the expander is allowed
         --  to generate them. They require special handling because string
         --  literal subtypes do not have standard bounds (the whole idea
         --  of these subtypes is to avoid having to generate the bounds)

         if Ekind (P_Type) = E_String_Literal_Subtype then
            Set_Etype (N, Etype (First_Index (P_Base_Type)));
            return;

         --  Scalar types

         elsif Is_Scalar_Type (P_Type) then
            Check_Type;

            if Present (E1) then
               Error_Attr ("invalid argument in % attribute", E1);

            elsif In_Aspect_Specification then
               Error_Attr
                 ("prefix of % attribute cannot be the current instance of a "
                  & "scalar type", P);

            else
               Set_Etype (N, P_Base_Type);
               return;
            end if;

         --  The following is a special test to allow 'First to apply to
         --  private scalar types if the attribute comes from generated
         --  code. This occurs in the case of Normalize_Scalars code.

         elsif Is_Private_Type (P_Type)
           and then Present (Full_View (P_Type))
           and then Is_Scalar_Type (Full_View (P_Type))
           and then not Comes_From_Source (N)
         then
            Set_Etype (N, Implementation_Base_Type (P_Type));

         --  Array types other than string literal subtypes handled above

         else
            Check_Array_Type;

            --  We know prefix is an array type, or the name of an array
            --  object, and that the expression, if present, is static
            --  and within the range of the dimensions of the type.

            pragma Assert (Is_Array_Type (P_Type));
            Index := First_Index (P_Base_Type);

            if No (E1) then

               --  First dimension assumed

               Set_Etype (N, Base_Type (Etype (Index)));

            else
               Dims := UI_To_Int (Intval (E1));

               for J in 1 .. Dims - 1 loop
                  Next_Index (Index);
               end loop;

               Set_Etype (N, Base_Type (Etype (Index)));
               Set_Etype (E1, Standard_Integer);
            end if;
         end if;
      end Check_Array_Or_Scalar_Type;

      ----------------------
      -- Check_Array_Type --
      ----------------------

      procedure Check_Array_Type is
         D : Int;
         --  Dimension number for array attributes

      begin
         --  If the type is a string literal type, then this must be generated
         --  internally, and no further check is required on its legality.

         if Ekind (P_Type) = E_String_Literal_Subtype then
            return;

         --  If the type is a composite, it is an illegal aggregate, no point
         --  in going on.

         elsif P_Type = Any_Composite then
            raise Bad_Attribute;
         end if;

         --  Normal case of array type or subtype

         Check_Either_E0_Or_E1;
         Check_Dereference;

         if Is_Array_Type (P_Type) then
            if not Is_Constrained (P_Type)
              and then Is_Entity_Name (P)
              and then Is_Type (Entity (P))
            then
               --  Note: we do not call Error_Attr here, since we prefer to
               --  continue, using the relevant index type of the array,
               --  even though it is unconstrained. This gives better error
               --  recovery behavior.

               Error_Msg_Name_1 := Aname;
               Error_Msg_F
                 ("prefix for % attribute must be constrained array", P);
            end if;

            --  The attribute reference freezes the type, and thus the
            --  component type, even if the attribute may not depend on the
            --  component. Diagnose arrays with incomplete components now.
            --  If the prefix is an access to array, this does not freeze
            --  the designated type.

            if Nkind (P) /= N_Explicit_Dereference then
               Check_Fully_Declared (Component_Type (P_Type), P);
            end if;

            D := Number_Dimensions (P_Type);

         else
            if Is_Private_Type (P_Type) then
               Error_Attr_P ("prefix for % attribute may not be private type");

            elsif Is_Access_Type (P_Type)
              and then Is_Array_Type (Designated_Type (P_Type))
              and then Is_Entity_Name (P)
              and then Is_Type (Entity (P))
            then
               Error_Attr_P ("prefix of % attribute cannot be access type");

            elsif Attr_Id = Attribute_First
                    or else
                  Attr_Id = Attribute_Last
            then
               Error_Attr ("invalid prefix for % attribute", P);

            else
               Error_Attr_P ("prefix for % attribute must be array");
            end if;
         end if;

         if Present (E1) then
            Resolve (E1, Any_Integer);
            Set_Etype (E1, Standard_Integer);

            if not Is_OK_Static_Expression (E1)
              or else Raises_Constraint_Error (E1)
            then
               Flag_Non_Static_Expr
                 ("expression for dimension must be static!", E1);
               Error_Attr;

            elsif UI_To_Int (Expr_Value (E1)) > D
              or else UI_To_Int (Expr_Value (E1)) < 1
            then
               Error_Attr ("invalid dimension number for array type", E1);
            end if;
         end if;

         if (Style_Check and Style_Check_Array_Attribute_Index)
           and then Comes_From_Source (N)
         then
            Style.Check_Array_Attribute_Index (N, E1, D);
         end if;
      end Check_Array_Type;

      -------------------------
      -- Check_Asm_Attribute --
      -------------------------

      procedure Check_Asm_Attribute is
      begin
         Check_Type;
         Check_E2;

         --  Check first argument is static string expression

         Analyze_And_Resolve (E1, Standard_String);

         if Etype (E1) = Any_Type then
            return;

         elsif not Is_OK_Static_Expression (E1) then
            Flag_Non_Static_Expr
              ("constraint argument must be static string expression!", E1);
            Error_Attr;
         end if;

         --  Check second argument is right type

         Analyze_And_Resolve (E2, Entity (P));

         --  Note: that is all we need to do, we don't need to check
         --  that it appears in a correct context. The Ada type system
         --  will do that for us.

      end Check_Asm_Attribute;

      ---------------------
      -- Check_Component --
      ---------------------

      procedure Check_Component is
      begin
         Check_E0;

         if Nkind (P) /= N_Selected_Component
           or else
             (Ekind (Entity (Selector_Name (P))) /= E_Component
               and then
              Ekind (Entity (Selector_Name (P))) /= E_Discriminant)
         then
            Error_Attr_P ("prefix for % attribute must be selected component");
         end if;
      end Check_Component;

      ------------------------------------
      -- Check_Decimal_Fixed_Point_Type --
      ------------------------------------

      procedure Check_Decimal_Fixed_Point_Type is
      begin
         Check_Type;

         if not Is_Decimal_Fixed_Point_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be decimal type");
         end if;
      end Check_Decimal_Fixed_Point_Type;

      -----------------------
      -- Check_Dereference --
      -----------------------

      procedure Check_Dereference is
      begin

         --  Case of a subtype mark

         if Is_Entity_Name (P) and then Is_Type (Entity (P)) then
            return;
         end if;

         --  Case of an expression

         Resolve (P);

         if Is_Access_Type (P_Type) then

            --  If there is an implicit dereference, then we must freeze the
            --  designated type of the access type, since the type of the
            --  referenced array is this type (see AI95-00106).

            --  As done elsewhere, freezing must not happen when pre-analyzing
            --  a pre- or postcondition or a default value for an object or for
            --  a formal parameter.

            if not In_Spec_Expression then
               Freeze_Before (N, Designated_Type (P_Type));
            end if;

            Rewrite (P,
              Make_Explicit_Dereference (Sloc (P),
                Prefix => Relocate_Node (P)));

            Analyze_And_Resolve (P);
            P_Type := Etype (P);

            if P_Type = Any_Type then
               raise Bad_Attribute;
            end if;

            P_Base_Type := Base_Type (P_Type);
         end if;
      end Check_Dereference;

      -------------------------
      -- Check_Discrete_Type --
      -------------------------

      procedure Check_Discrete_Type is
      begin
         Check_Type;

         if not Is_Discrete_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be discrete type");
         end if;
      end Check_Discrete_Type;

      --------------
      -- Check_E0 --
      --------------

      procedure Check_E0 is
      begin
         if Present (E1) then
            Unexpected_Argument (E1);
         end if;
      end Check_E0;

      --------------
      -- Check_E1 --
      --------------

      procedure Check_E1 is
      begin
         Check_Either_E0_Or_E1;

         if No (E1) then

            --  Special-case attributes that are functions and that appear as
            --  the prefix of another attribute. Error is posted on parent.

            if Nkind (Parent (N)) = N_Attribute_Reference
              and then Nam_In (Attribute_Name (Parent (N)), Name_Address,
                                                            Name_Code_Address,
                                                            Name_Access)
            then
               Error_Msg_Name_1 := Attribute_Name (Parent (N));
               Error_Msg_N ("illegal prefix for % attribute", Parent (N));
               Set_Etype (Parent (N), Any_Type);
               Set_Entity (Parent (N), Any_Type);
               raise Bad_Attribute;

            else
               Error_Attr ("missing argument for % attribute", N);
            end if;
         end if;
      end Check_E1;

      --------------
      -- Check_E2 --
      --------------

      procedure Check_E2 is
      begin
         if No (E1) then
            Error_Attr ("missing arguments for % attribute (2 required)", N);
         elsif No (E2) then
            Error_Attr ("missing argument for % attribute (2 required)", N);
         end if;
      end Check_E2;

      ---------------------------
      -- Check_Either_E0_Or_E1 --
      ---------------------------

      procedure Check_Either_E0_Or_E1 is
      begin
         if Present (E2) then
            Unexpected_Argument (E2);
         end if;
      end Check_Either_E0_Or_E1;

      ----------------------
      -- Check_Enum_Image --
      ----------------------

      procedure Check_Enum_Image is
         Lit : Entity_Id;

      begin
         --  When an enumeration type appears in an attribute reference, all
         --  literals of the type are marked as referenced. This must only be
         --  done if the attribute reference appears in the current source.
         --  Otherwise the information on references may differ between a
         --  normal compilation and one that performs inlining.

         if Is_Enumeration_Type (P_Base_Type)
           and then In_Extended_Main_Code_Unit (N)
         then
            Lit := First_Literal (P_Base_Type);
            while Present (Lit) loop
               Set_Referenced (Lit);
               Next_Literal (Lit);
            end loop;
         end if;
      end Check_Enum_Image;

      ----------------------------
      -- Check_First_Last_Valid --
      ----------------------------

      procedure Check_First_Last_Valid is
      begin
         Check_Discrete_Type;

         --  Freeze the subtype now, so that the following test for predicates
         --  works (we set the predicates stuff up at freeze time)

         Insert_Actions (N, Freeze_Entity (P_Type, P));

         --  Now test for dynamic predicate

         if Has_Predicates (P_Type)
           and then not (Has_Static_Predicate (P_Type))
         then
            Error_Attr_P
              ("prefix of % attribute may not have dynamic predicate");
         end if;

         --  Check non-static subtype

         if not Is_OK_Static_Subtype (P_Type) then
            Error_Attr_P ("prefix of % attribute must be a static subtype");
         end if;

         --  Test case for no values

         if Expr_Value (Type_Low_Bound (P_Type)) >
            Expr_Value (Type_High_Bound (P_Type))
           or else (Has_Predicates (P_Type)
                     and then
                       Is_Empty_List (Static_Discrete_Predicate (P_Type)))
         then
            Error_Attr_P
              ("prefix of % attribute must be subtype with at least one "
               & "value");
         end if;
      end Check_First_Last_Valid;

      ----------------------------
      -- Check_Fixed_Point_Type --
      ----------------------------

      procedure Check_Fixed_Point_Type is
      begin
         Check_Type;

         if not Is_Fixed_Point_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be fixed point type");
         end if;
      end Check_Fixed_Point_Type;

      ------------------------------
      -- Check_Fixed_Point_Type_0 --
      ------------------------------

      procedure Check_Fixed_Point_Type_0 is
      begin
         Check_Fixed_Point_Type;
         Check_E0;
      end Check_Fixed_Point_Type_0;

      -------------------------------
      -- Check_Floating_Point_Type --
      -------------------------------

      procedure Check_Floating_Point_Type is
      begin
         Check_Type;

         if not Is_Floating_Point_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be float type");
         end if;
      end Check_Floating_Point_Type;

      ---------------------------------
      -- Check_Floating_Point_Type_0 --
      ---------------------------------

      procedure Check_Floating_Point_Type_0 is
      begin
         Check_Floating_Point_Type;
         Check_E0;
      end Check_Floating_Point_Type_0;

      ---------------------------------
      -- Check_Floating_Point_Type_1 --
      ---------------------------------

      procedure Check_Floating_Point_Type_1 is
      begin
         Check_Floating_Point_Type;
         Check_E1;
      end Check_Floating_Point_Type_1;

      ---------------------------------
      -- Check_Floating_Point_Type_2 --
      ---------------------------------

      procedure Check_Floating_Point_Type_2 is
      begin
         Check_Floating_Point_Type;
         Check_E2;
      end Check_Floating_Point_Type_2;

      ------------------------
      -- Check_Integer_Type --
      ------------------------

      procedure Check_Integer_Type is
      begin
         Check_Type;

         if not Is_Integer_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be integer type");
         end if;
      end Check_Integer_Type;

      --------------------------------
      -- Check_Modular_Integer_Type --
      --------------------------------

      procedure Check_Modular_Integer_Type is
      begin
         Check_Type;

         if not Is_Modular_Integer_Type (P_Type) then
            Error_Attr_P
              ("prefix of % attribute must be modular integer type");
         end if;
      end Check_Modular_Integer_Type;

      ------------------------
      -- Check_Not_CPP_Type --
      ------------------------

      procedure Check_Not_CPP_Type is
      begin
         if Is_Tagged_Type (Etype (P))
           and then Convention (Etype (P)) = Convention_CPP
           and then Is_CPP_Class (Root_Type (Etype (P)))
         then
            Error_Attr_P
              ("invalid use of % attribute with 'C'P'P tagged type");
         end if;
      end Check_Not_CPP_Type;

      -------------------------------
      -- Check_Not_Incomplete_Type --
      -------------------------------

      procedure Check_Not_Incomplete_Type is
         E   : Entity_Id;
         Typ : Entity_Id;

      begin
         --  Ada 2005 (AI-50217, AI-326): If the prefix is an explicit
         --  dereference we have to check wrong uses of incomplete types
         --  (other wrong uses are checked at their freezing point).

         --  In Ada 2012, incomplete types can appear in subprogram
         --  profiles, but formals with incomplete types cannot be the
         --  prefix of attributes.

         --  Example 1: Limited-with

         --    limited with Pkg;
         --    package P is
         --       type Acc is access Pkg.T;
         --       X : Acc;
         --       S : Integer := X.all'Size;                    -- ERROR
         --    end P;

         --  Example 2: Tagged incomplete

         --     type T is tagged;
         --     type Acc is access all T;
         --     X : Acc;
         --     S : constant Integer := X.all'Size;             -- ERROR
         --     procedure Q (Obj : Integer := X.all'Alignment); -- ERROR

         if Ada_Version >= Ada_2005
           and then Nkind (P) = N_Explicit_Dereference
         then
            E := P;
            while Nkind (E) = N_Explicit_Dereference loop
               E := Prefix (E);
            end loop;

            Typ := Etype (E);

            if From_Limited_With (Typ) then
               Error_Attr_P
                 ("prefix of % attribute cannot be an incomplete type");

            --  If the prefix is an access type check the designated type

            elsif Is_Access_Type (Typ)
              and then Nkind (P) = N_Explicit_Dereference
            then
               Typ := Directly_Designated_Type (Typ);
            end if;

            if Is_Class_Wide_Type (Typ) then
               Typ := Root_Type (Typ);
            end if;

            --  A legal use of a shadow entity occurs only when the unit where
            --  the non-limited view resides is imported via a regular with
            --  clause in the current body. Such references to shadow entities
            --  may occur in subprogram formals.

            if Is_Incomplete_Type (Typ)
              and then From_Limited_With (Typ)
              and then Present (Non_Limited_View (Typ))
              and then Is_Legal_Shadow_Entity_In_Body (Typ)
            then
               Typ := Non_Limited_View (Typ);
            end if;

            --  If still incomplete, it can be a local incomplete type, or a
            --  limited view whose scope is also a limited view.

            if Ekind (Typ) = E_Incomplete_Type then
               if not From_Limited_With (Typ)
                  and then No (Full_View (Typ))
               then
                  Error_Attr_P
                    ("prefix of % attribute cannot be an incomplete type");

               --  The limited view may be available indirectly through
               --  an intermediate unit. If the non-limited view is available
               --  the attribute reference is legal.

               elsif From_Limited_With (Typ)
                 and then
                   (No (Non_Limited_View (Typ))
                     or else Is_Incomplete_Type (Non_Limited_View (Typ)))
               then
                  Error_Attr_P
                    ("prefix of % attribute cannot be an incomplete type");
               end if;
            end if;

         --  Ada 2012 : formals in bodies may be incomplete, but no attribute
         --  legally applies.

         elsif Is_Entity_Name (P)
           and then Is_Formal (Entity (P))
           and then Is_Incomplete_Type (Etype (Etype (P)))
         then
            Error_Attr_P
              ("prefix of % attribute cannot be an incomplete type");
         end if;

         if not Is_Entity_Name (P)
           or else not Is_Type (Entity (P))
           or else In_Spec_Expression
         then
            return;
         else
            Check_Fully_Declared (P_Type, P);
         end if;
      end Check_Not_Incomplete_Type;

      ----------------------------
      -- Check_Object_Reference --
      ----------------------------

      procedure Check_Object_Reference (P : Node_Id) is
         Rtyp : Entity_Id;

      begin
         --  If we need an object, and we have a prefix that is the name of
         --  a function entity, convert it into a function call.

         if Is_Entity_Name (P)
           and then Ekind (Entity (P)) = E_Function
         then
            Rtyp := Etype (Entity (P));

            Rewrite (P,
              Make_Function_Call (Sloc (P),
                Name => Relocate_Node (P)));

            Analyze_And_Resolve (P, Rtyp);

         --  Otherwise we must have an object reference

         elsif not Is_Object_Reference (P) then
            Error_Attr_P ("prefix of % attribute must be object");
         end if;
      end Check_Object_Reference;

      ----------------------------------
      -- Check_Object_Reference_Image --
      ----------------------------------

      procedure Check_Object_Reference_Image (Str_Typ : Entity_Id) is
      begin
         Check_E0;
         Set_Etype (N, Str_Typ);

         if not Is_Scalar_Type (P_Type)
           or else (Is_Entity_Name (P) and then Is_Type (Entity (P)))
         then
            Error_Attr_P
              ("prefix of % attribute must be scalar object name");
         end if;

         Check_Enum_Image;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;
      end Check_Object_Reference_Image;

      ----------------------------
      -- Check_PolyORB_Attribute --
      ----------------------------

      procedure Check_PolyORB_Attribute is
      begin
         Validate_Non_Static_Attribute_Function_Call;

         Check_Type;
         Check_Not_CPP_Type;

         if Get_PCS_Name /= Name_PolyORB_DSA then
            Error_Attr
              ("attribute% requires the 'Poly'O'R'B 'P'C'S", N);
         end if;
      end Check_PolyORB_Attribute;

      ------------------------
      -- Check_Program_Unit --
      ------------------------

      procedure Check_Program_Unit is
      begin
         if Is_Entity_Name (P) then
            declare
               K : constant Entity_Kind := Ekind (Entity (P));
               T : constant Entity_Id   := Etype (Entity (P));

            begin
               if K in Subprogram_Kind
                 or else K in Task_Kind
                 or else K in Protected_Kind
                 or else K = E_Package
                 or else K in Generic_Unit_Kind
                 or else (K = E_Variable
                            and then
                              (Is_Task_Type (T)
                                 or else
                               Is_Protected_Type (T)))
               then
                  return;
               end if;
            end;
         end if;

         Error_Attr_P ("prefix of % attribute must be program unit");
      end Check_Program_Unit;

      ---------------------
      -- Check_Real_Type --
      ---------------------

      procedure Check_Real_Type is
      begin
         Check_Type;

         if not Is_Real_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be real type");
         end if;
      end Check_Real_Type;

      -----------------------
      -- Check_Scalar_Type --
      -----------------------

      procedure Check_Scalar_Type is
      begin
         Check_Type;

         if not Is_Scalar_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be scalar type");
         end if;
      end Check_Scalar_Type;

      ------------------------------------------
      -- Check_SPARK_05_Restriction_On_Attribute --
      ------------------------------------------

      procedure Check_SPARK_05_Restriction_On_Attribute is
      begin
         Error_Msg_Name_1 := Aname;
         Check_SPARK_05_Restriction ("attribute % is not allowed", P);
      end Check_SPARK_05_Restriction_On_Attribute;

      ---------------------------
      -- Check_Standard_Prefix --
      ---------------------------

      procedure Check_Standard_Prefix is
      begin
         Check_E0;

         if Nkind (P) /= N_Identifier or else Chars (P) /= Name_Standard then
            Error_Attr ("only allowed prefix for % attribute is Standard", P);
         end if;
      end Check_Standard_Prefix;

      ----------------------------
      -- Check_Stream_Attribute --
      ----------------------------

      procedure Check_Stream_Attribute (Nam : TSS_Name_Type) is
         Etyp : Entity_Id;
         Btyp : Entity_Id;

         In_Shared_Var_Procs : Boolean;
         --  True when compiling System.Shared_Storage.Shared_Var_Procs body.
         --  For this runtime package (always compiled in GNAT mode), we allow
         --  stream attributes references for limited types for the case where
         --  shared passive objects are implemented using stream attributes,
         --  which is the default in GNAT's persistent storage implementation.

      begin
         Validate_Non_Static_Attribute_Function_Call;

         --  With the exception of 'Input, Stream attributes are procedures,
         --  and can only appear at the position of procedure calls. We check
         --  for this here, before they are rewritten, to give a more precise
         --  diagnostic.

         if Nam = TSS_Stream_Input then
            null;

         elsif Is_List_Member (N)
           and then not Nkind_In (Parent (N), N_Procedure_Call_Statement,
                                              N_Aggregate)
         then
            null;

         else
            Error_Attr
              ("invalid context for attribute%, which is a procedure", N);
         end if;

         Check_Type;
         Btyp := Implementation_Base_Type (P_Type);

         --  Stream attributes not allowed on limited types unless the
         --  attribute reference was generated by the expander (in which
         --  case the underlying type will be used, as described in Sinfo),
         --  or the attribute was specified explicitly for the type itself
         --  or one of its ancestors (taking visibility rules into account if
         --  in Ada 2005 mode), or a pragma Stream_Convert applies to Btyp
         --  (with no visibility restriction).

         declare
            Gen_Body : constant Node_Id := Enclosing_Generic_Body (N);
         begin
            if Present (Gen_Body) then
               In_Shared_Var_Procs :=
                 Is_RTE (Corresponding_Spec (Gen_Body), RE_Shared_Var_Procs);
            else
               In_Shared_Var_Procs := False;
            end if;
         end;

         if (Comes_From_Source (N)
              and then not (In_Shared_Var_Procs or In_Instance))
           and then not Stream_Attribute_Available (P_Type, Nam)
           and then not Has_Rep_Pragma (Btyp, Name_Stream_Convert)
         then
            Error_Msg_Name_1 := Aname;

            if Is_Limited_Type (P_Type) then
               Error_Msg_NE
                 ("limited type& has no% attribute", P, P_Type);
               Explain_Limited_Type (P_Type, P);
            else
               Error_Msg_NE
                 ("attribute% for type& is not available", P, P_Type);
            end if;
         end if;

         --  Check for no stream operations allowed from No_Tagged_Streams

         if Is_Tagged_Type (P_Type)
           and then Present (No_Tagged_Streams_Pragma (P_Type))
         then
            Error_Msg_Sloc := Sloc (No_Tagged_Streams_Pragma (P_Type));
            Error_Msg_NE
              ("no stream operations for & (No_Tagged_Streams #)", N, P_Type);
            return;
         end if;

         --  Check restriction violations

         --  First check the No_Streams restriction, which prohibits the use
         --  of explicit stream attributes in the source program. We do not
         --  prevent the occurrence of stream attributes in generated code,
         --  for instance those generated implicitly for dispatching purposes.

         if Comes_From_Source (N) then
            Check_Restriction (No_Streams, P);
         end if;

         --  AI05-0057: if restriction No_Default_Stream_Attributes is active,
         --  it is illegal to use a predefined elementary type stream attribute
         --  either by itself, or more importantly as part of the attribute
         --  subprogram for a composite type. However, if the broader
         --  restriction No_Streams is active, stream operations are not
         --  generated, and there is no error.

         if Restriction_Active (No_Default_Stream_Attributes)
           and then not Restriction_Active (No_Streams)
         then
            declare
               T : Entity_Id;

            begin
               if Nam = TSS_Stream_Input
                    or else
                  Nam = TSS_Stream_Read
               then
                  T :=
                    Type_Without_Stream_Operation (P_Type, TSS_Stream_Read);
               else
                  T :=
                    Type_Without_Stream_Operation (P_Type, TSS_Stream_Write);
               end if;

               if Present (T) then
                  Check_Restriction (No_Default_Stream_Attributes, N);

                  Error_Msg_NE
                    ("missing user-defined Stream Read or Write for type&",
                      N, T);
                  if not Is_Elementary_Type (P_Type) then
                     Error_Msg_NE
                     ("\which is a component of type&", N, P_Type);
                  end if;
               end if;
            end;
         end if;

         --  Check special case of Exception_Id and Exception_Occurrence which
         --  are not allowed for restriction No_Exception_Registration.

         if Restriction_Check_Required (No_Exception_Registration)
           and then (Is_RTE (P_Type, RE_Exception_Id)
                       or else
                     Is_RTE (P_Type, RE_Exception_Occurrence))
         then
            Check_Restriction (No_Exception_Registration, P);
         end if;

         --  Here we must check that the first argument is an access type
         --  that is compatible with Ada.Streams.Root_Stream_Type'Class.

         Analyze_And_Resolve (E1);
         Etyp := Etype (E1);

         --  Note: the double call to Root_Type here is needed because the
         --  root type of a class-wide type is the corresponding type (e.g.
         --  X for X'Class, and we really want to go to the root.)

         if not Is_Access_Type (Etyp)
           or else Root_Type (Root_Type (Designated_Type (Etyp))) /=
                     RTE (RE_Root_Stream_Type)
         then
            Error_Attr
              ("expected access to Ada.Streams.Root_Stream_Type''Class", E1);
         end if;

         --  Check that the second argument is of the right type if there is
         --  one (the Input attribute has only one argument so this is skipped)

         if Present (E2) then
            Analyze (E2);

            if Nam = TSS_Stream_Read
              and then not Is_OK_Variable_For_Out_Formal (E2)
            then
               Error_Attr
                 ("second argument of % attribute must be a variable", E2);
            end if;

            Resolve (E2, P_Type);
         end if;

         Check_Not_CPP_Type;
      end Check_Stream_Attribute;

      -------------------------
      -- Check_System_Prefix --
      -------------------------

      procedure Check_System_Prefix is
      begin
         if Nkind (P) /= N_Identifier or else Chars (P) /= Name_System then
            Error_Attr ("only allowed prefix for % attribute is System", P);
         end if;
      end Check_System_Prefix;

      -----------------------
      -- Check_Task_Prefix --
      -----------------------

      procedure Check_Task_Prefix is
      begin
         Analyze (P);

         --  Ada 2005 (AI-345): Attribute 'Terminated can be applied to
         --  task interface class-wide types.

         if Is_Task_Type (Etype (P))
           or else (Is_Access_Type (Etype (P))
                      and then Is_Task_Type (Designated_Type (Etype (P))))
           or else (Ada_Version >= Ada_2005
                      and then Ekind (Etype (P)) = E_Class_Wide_Type
                      and then Is_Interface (Etype (P))
                      and then Is_Task_Interface (Etype (P)))
         then
            Resolve (P);

         else
            if Ada_Version >= Ada_2005 then
               Error_Attr_P
                 ("prefix of % attribute must be a task or a task " &
                  "interface class-wide object");

            else
               Error_Attr_P ("prefix of % attribute must be a task");
            end if;
         end if;
      end Check_Task_Prefix;

      ----------------
      -- Check_Type --
      ----------------

      --  The possibilities are an entity name denoting a type, or an
      --  attribute reference that denotes a type (Base or Class). If
      --  the type is incomplete, replace it with its full view.

      procedure Check_Type is
      begin
         if not Is_Entity_Name (P)
           or else not Is_Type (Entity (P))
         then
            Error_Attr_P ("prefix of % attribute must be a type");

         elsif Is_Protected_Self_Reference (P) then
            Error_Attr_P
              ("prefix of % attribute denotes current instance "
               & "(RM 9.4(21/2))");

         elsif Ekind (Entity (P)) = E_Incomplete_Type
            and then Present (Full_View (Entity (P)))
         then
            P_Type := Full_View (Entity (P));
            Set_Entity (P, P_Type);
         end if;
      end Check_Type;

      ---------------------
      -- Check_Unit_Name --
      ---------------------

      procedure Check_Unit_Name (Nod : Node_Id) is
      begin
         if Nkind (Nod) = N_Identifier then
            return;

         elsif Nkind_In (Nod, N_Selected_Component, N_Expanded_Name) then
            Check_Unit_Name (Prefix (Nod));

            if Nkind (Selector_Name (Nod)) = N_Identifier then
               return;
            end if;
         end if;

         Error_Attr ("argument for % attribute must be unit name", P);
      end Check_Unit_Name;

      ----------------
      -- Error_Attr --
      ----------------

      procedure Error_Attr is
      begin
         Set_Etype (N, Any_Type);
         Set_Entity (N, Any_Type);
         raise Bad_Attribute;
      end Error_Attr;

      procedure Error_Attr (Msg : String; Error_Node : Node_Id) is
      begin
         Error_Msg_Name_1 := Aname;
         Error_Msg_N (Msg, Error_Node);
         Error_Attr;
      end Error_Attr;

      ------------------
      -- Error_Attr_P --
      ------------------

      procedure Error_Attr_P (Msg : String) is
      begin
         Error_Msg_Name_1 := Aname;
         Error_Msg_F (Msg, P);
         Error_Attr;
      end Error_Attr_P;

      ----------------------------
      -- Legal_Formal_Attribute --
      ----------------------------

      procedure Legal_Formal_Attribute is
      begin
         Check_E0;

         if not Is_Entity_Name (P)
           or else not Is_Type (Entity (P))
         then
            Error_Attr_P ("prefix of % attribute must be generic type");

         elsif Is_Generic_Actual_Type (Entity (P))
           or else In_Instance
           or else In_Inlined_Body
         then
            null;

         elsif Is_Generic_Type (Entity (P)) then
            if Is_Definite_Subtype (Entity (P)) then
               Error_Attr_P
                 ("prefix of % attribute must be indefinite generic type");
            end if;

         else
            Error_Attr_P
              ("prefix of % attribute must be indefinite generic type");
         end if;

         Set_Etype (N, Standard_Boolean);
      end Legal_Formal_Attribute;

      ---------------------------------------------------------------
      -- Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements --
      ---------------------------------------------------------------

      procedure Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements is
      begin
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);
      end Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements;

      -------------
      -- Min_Max --
      -------------

      procedure Min_Max is
      begin
         Check_E2;
         Check_Scalar_Type;
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);
         Set_Etype (N, P_Base_Type);

         --  Check for comparison on unordered enumeration type

         if Bad_Unordered_Enumeration_Reference (N, P_Base_Type) then
            Error_Msg_Sloc := Sloc (P_Base_Type);
            Error_Msg_NE
              ("comparison on unordered enumeration type& declared#?U?",
               N, P_Base_Type);
         end if;
      end Min_Max;

      ------------------------
      -- Standard_Attribute --
      ------------------------

      procedure Standard_Attribute (Val : Int) is
      begin
         Check_Standard_Prefix;
         Rewrite (N, Make_Integer_Literal (Loc, Val));
         Analyze (N);
         Set_Is_Static_Expression (N, True);
      end Standard_Attribute;

      --------------------
      -- Uneval_Old_Msg --
      --------------------

      procedure Uneval_Old_Msg is
         Uneval_Old_Setting : Character;
         Prag               : Node_Id;

      begin
         --  If from aspect, then Uneval_Old_Setting comes from flags in the
         --  N_Aspect_Specification node that corresponds to the attribute.

         --  First find the pragma in which we appear (note that at this stage,
         --  even if we appeared originally within an aspect specification, we
         --  are now within the corresponding pragma).

         Prag := N;
         loop
            Prag := Parent (Prag);
            exit when No (Prag) or else Nkind (Prag) = N_Pragma;
         end loop;

         if Present (Prag) then
            if Uneval_Old_Accept (Prag) then
               Uneval_Old_Setting := 'A';
            elsif Uneval_Old_Warn (Prag) then
               Uneval_Old_Setting := 'W';
            else
               Uneval_Old_Setting := 'E';
            end if;

         --  If we did not find the pragma, that's odd, just use the setting
         --  from Opt.Uneval_Old. Perhaps this is due to a previous error?

         else
            Uneval_Old_Setting := Opt.Uneval_Old;
         end if;

         --  Processing depends on the setting of Uneval_Old

         case Uneval_Old_Setting is
            when 'E' =>
               Error_Attr_P
                 ("prefix of attribute % that is potentially "
                  & "unevaluated must denote an entity");

            when 'W' =>
               Error_Msg_Name_1 := Aname;
               Error_Msg_F
                 ("??prefix of attribute % appears in potentially "
                  & "unevaluated context, exception may be raised", P);

            when 'A' =>
               null;

            when others =>
               raise Program_Error;
         end case;
      end Uneval_Old_Msg;

      -------------------------
      -- Unexpected Argument --
      -------------------------

      procedure Unexpected_Argument (En : Node_Id) is
      begin
         Error_Attr ("unexpected argument for % attribute", En);
      end Unexpected_Argument;

      -------------------------------------------------
      -- Validate_Non_Static_Attribute_Function_Call --
      -------------------------------------------------

      --  This function should be moved to Sem_Dist ???

      procedure Validate_Non_Static_Attribute_Function_Call is
      begin
         if In_Preelaborated_Unit
           and then not In_Subprogram_Or_Concurrent_Unit
         then
            Flag_Non_Static_Expr
              ("non-static function call in preelaborated unit!", N);
         end if;
      end Validate_Non_Static_Attribute_Function_Call;

   --  Start of processing for Analyze_Attribute

   begin
      --  Immediate return if unrecognized attribute (already diagnosed by
      --  parser, so there is nothing more that we need to do).

      if not Is_Attribute_Name (Aname) then
         raise Bad_Attribute;
      end if;

      Check_Restriction_No_Use_Of_Attribute (N);

      --  Deal with Ada 83 issues

      if Comes_From_Source (N) then
         if not Attribute_83 (Attr_Id) then
            if Ada_Version = Ada_83 and then Comes_From_Source (N) then
               Error_Msg_Name_1 := Aname;
               Error_Msg_N ("(Ada 83) attribute% is not standard??", N);
            end if;

            if Attribute_Impl_Def (Attr_Id) then
               Check_Restriction (No_Implementation_Attributes, N);
            end if;
         end if;
      end if;

      --  Deal with Ada 2005 attributes that are implementation attributes
      --  because they appear in a version of Ada before Ada 2005, and
      --  similarly for Ada 2012 attributes appearing in an earlier version.

      if (Attribute_05 (Attr_Id) and then Ada_Version < Ada_2005)
            or else
         (Attribute_12 (Attr_Id) and then Ada_Version < Ada_2012)
      then
         Check_Restriction (No_Implementation_Attributes, N);
      end if;

      --   Remote access to subprogram type access attribute reference needs
      --   unanalyzed copy for tree transformation. The analyzed copy is used
      --   for its semantic information (whether prefix is a remote subprogram
      --   name), the unanalyzed copy is used to construct new subtree rooted
      --   with N_Aggregate which represents a fat pointer aggregate.

      if Aname = Name_Access then
         Discard_Node (Copy_Separate_Tree (N));
      end if;

      --  Analyze prefix and exit if error in analysis. If the prefix is an
      --  incomplete type, use full view if available. Note that there are
      --  some attributes for which we do not analyze the prefix, since the
      --  prefix is not a normal name, or else needs special handling.

      if Aname /= Name_Elab_Body       and then
         Aname /= Name_Elab_Spec       and then
         Aname /= Name_Elab_Subp_Body  and then
         Aname /= Name_Enabled         and then
         Aname /= Name_Old
      then
         Analyze (P);
         P_Type := Etype (P);

         if Is_Entity_Name (P)
           and then Present (Entity (P))
           and then Is_Type (Entity (P))
         then
            if Ekind (Entity (P)) = E_Incomplete_Type then
               P_Type := Get_Full_View (P_Type);
               Set_Entity (P, P_Type);
               Set_Etype  (P, P_Type);

            elsif Entity (P) = Current_Scope
              and then Is_Record_Type (Entity (P))
            then
               --  Use of current instance within the type. Verify that if the
               --  attribute appears within a constraint, it  yields an access
               --  type, other uses are illegal.

               declare
                  Par : Node_Id;

               begin
                  Par := Parent (N);
                  while Present (Par)
                    and then Nkind (Parent (Par)) /= N_Component_Definition
                  loop
                     Par := Parent (Par);
                  end loop;

                  if Present (Par)
                    and then Nkind (Par) = N_Subtype_Indication
                  then
                     if Attr_Id /= Attribute_Access
                       and then Attr_Id /= Attribute_Unchecked_Access
                       and then Attr_Id /= Attribute_Unrestricted_Access
                     then
                        Error_Msg_N
                          ("in a constraint the current instance can only "
                           & "be used with an access attribute", N);
                     end if;
                  end if;
               end;
            end if;
         end if;

         if P_Type = Any_Type then
            raise Bad_Attribute;
         end if;

         P_Base_Type := Base_Type (P_Type);
      end if;

      --  Analyze expressions that may be present, exiting if an error occurs

      if No (Exprs) then
         E1 := Empty;
         E2 := Empty;

      else
         E1 := First (Exprs);

         --  Skip analysis for case of Restriction_Set, we do not expect
         --  the argument to be analyzed in this case.

         if Aname /= Name_Restriction_Set then
            Analyze (E1);

            --  Check for missing/bad expression (result of previous error)

            if No (E1) or else Etype (E1) = Any_Type then
               raise Bad_Attribute;
            end if;
         end if;

         E2 := Next (E1);

         if Present (E2) then
            Analyze (E2);

            if Etype (E2) = Any_Type then
               raise Bad_Attribute;
            end if;

            if Present (Next (E2)) then
               Unexpected_Argument (Next (E2));
            end if;
         end if;
      end if;

      --  Cases where prefix must be resolvable by itself

      if Is_Overloaded (P)
        and then Aname /= Name_Access
        and then Aname /= Name_Address
        and then Aname /= Name_Code_Address
        and then Aname /= Name_Result
        and then Aname /= Name_Unchecked_Access
      then
         --  The prefix must be resolvable by itself, without reference to the
         --  attribute. One case that requires special handling is a prefix
         --  that is a function name, where one interpretation may be a
         --  parameterless call. Entry attributes are handled specially below.

         if Is_Entity_Name (P)
           and then not Nam_In (Aname, Name_Count, Name_Caller)
         then
            Check_Parameterless_Call (P);
         end if;

         if Is_Overloaded (P) then

            --  Ada 2005 (AI-345): Since protected and task types have
            --  primitive entry wrappers, the attributes Count, and Caller
            --  require a context check

            if Nam_In (Aname, Name_Count, Name_Caller) then
               declare
                  Count : Natural := 0;
                  I     : Interp_Index;
                  It    : Interp;

               begin
                  Get_First_Interp (P, I, It);
                  while Present (It.Nam) loop
                     if Comes_From_Source (It.Nam) then
                        Count := Count + 1;
                     else
                        Remove_Interp (I);
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;

                  if Count > 1 then
                     Error_Attr ("ambiguous prefix for % attribute", P);
                  else
                     Set_Is_Overloaded (P, False);
                  end if;
               end;

            else
               Error_Attr ("ambiguous prefix for % attribute", P);
            end if;
         end if;
      end if;

      --  In SPARK, attributes of private types are only allowed if the full
      --  type declaration is visible.

      --  Note: the check for Present (Entity (P)) defends against some error
      --  conditions where the Entity field is not set.

      if Is_Entity_Name (P) and then Present (Entity (P))
        and then Is_Type (Entity (P))
        and then Is_Private_Type (P_Type)
        and then not In_Open_Scopes (Scope (P_Type))
        and then not In_Spec_Expression
      then
         Check_SPARK_05_Restriction ("invisible attribute of type", N);
      end if;

      --  Remaining processing depends on attribute

      case Attr_Id is

      --  Attributes related to Ada 2012 iterators. Attribute specifications
      --  exist for these, but they cannot be queried.

      when Attribute_Constant_Indexing
         | Attribute_Default_Iterator
         | Attribute_Implicit_Dereference
         | Attribute_Iterator_Element
         | Attribute_Iterable
         | Attribute_Variable_Indexing
      =>
         Error_Msg_N ("illegal attribute", N);

      --  Internal attributes used to deal with Ada 2012 delayed aspects. These
      --  were already rejected by the parser. Thus they shouldn't appear here.

      when Internal_Attribute_Id =>
         raise Program_Error;

      ------------------
      -- Abort_Signal --
      ------------------

      when Attribute_Abort_Signal =>
         Check_Standard_Prefix;
         Rewrite (N, New_Occurrence_Of (Stand.Abort_Signal, Loc));
         Analyze (N);

      ------------
      -- Access --
      ------------

      when Attribute_Access =>
         Analyze_Access_Attribute;
         Check_Not_Incomplete_Type;

      -------------
      -- Address --
      -------------

      when Attribute_Address =>
         Check_E0;
         Address_Checks;
         Check_Not_Incomplete_Type;
         Set_Etype (N, RTE (RE_Address));

      ------------------
      -- Address_Size --
      ------------------

      when Attribute_Address_Size =>
         Standard_Attribute (System_Address_Size);

      --------------
      -- Adjacent --
      --------------

      when Attribute_Adjacent =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);

      ---------
      -- Aft --
      ---------

      when Attribute_Aft =>
         Check_Fixed_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment =>

         --  Don't we need more checking here, cf Size ???

         Check_E0;
         Check_Not_Incomplete_Type;
         Check_Not_CPP_Type;
         Set_Etype (N, Universal_Integer);

      ---------------
      -- Asm_Input --
      ---------------

      when Attribute_Asm_Input =>
         Check_Asm_Attribute;

         --  The back end may need to take the address of E2

         if Is_Entity_Name (E2) then
            Set_Address_Taken (Entity (E2));
         end if;

         Set_Etype (N, RTE (RE_Asm_Input_Operand));

      ----------------
      -- Asm_Output --
      ----------------

      when Attribute_Asm_Output =>
         Check_Asm_Attribute;

         if Etype (E2) = Any_Type then
            return;

         elsif Aname = Name_Asm_Output then
            if not Is_Variable (E2) then
               Error_Attr
                 ("second argument for Asm_Output is not variable", E2);
            end if;
         end if;

         Note_Possible_Modification (E2, Sure => True);

         --  The back end may need to take the address of E2

         if Is_Entity_Name (E2) then
            Set_Address_Taken (Entity (E2));
         end if;

         Set_Etype (N, RTE (RE_Asm_Output_Operand));

      -----------------------------
      -- Atomic_Always_Lock_Free --
      -----------------------------

      when Attribute_Atomic_Always_Lock_Free =>
         Check_E0;
         Check_Type;
         Set_Etype (N, Standard_Boolean);

      ----------
      -- Base --
      ----------

      --  Note: when the base attribute appears in the context of a subtype
      --  mark, the analysis is done by Sem_Ch8.Find_Type, rather than by
      --  the following circuit.

      when Attribute_Base => Base : declare
         Typ : Entity_Id;

      begin
         Check_E0;
         Find_Type (P);
         Typ := Entity (P);

         if Ada_Version >= Ada_95
           and then not Is_Scalar_Type (Typ)
           and then not Is_Generic_Type (Typ)
         then
            Error_Attr_P ("prefix of Base attribute must be scalar type");

         elsif Sloc (Typ) = Standard_Location
           and then Base_Type (Typ) = Typ
           and then Warn_On_Redundant_Constructs
         then
            Error_Msg_NE -- CODEFIX
              ("?r?redundant attribute, & is its own base type", N, Typ);
         end if;

         if Nkind (Parent (N)) /= N_Attribute_Reference then
            Error_Msg_Name_1 := Aname;
            Check_SPARK_05_Restriction
              ("attribute% is only allowed as prefix of another attribute", P);
         end if;

         Set_Etype (N, Base_Type (Entity (P)));
         Set_Entity (N, Base_Type (Entity (P)));
         Rewrite (N, New_Occurrence_Of (Entity (N), Loc));
         Analyze (N);
      end Base;

      ---------
      -- Bit --
      ---------

      when Attribute_Bit =>
         Check_E0;

         if not Is_Object_Reference (P) then
            Error_Attr_P ("prefix for % attribute must be object");

         --  What about the access object cases ???

         else
            null;
         end if;

         Set_Etype (N, Universal_Integer);

      ---------------
      -- Bit_Order --
      ---------------

      when Attribute_Bit_Order =>
         Check_E0;
         Check_Type;

         if not Is_Record_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be record type");
         end if;

         if Bytes_Big_Endian xor Reverse_Bit_Order (P_Type) then
            Rewrite (N,
              New_Occurrence_Of (RTE (RE_High_Order_First), Loc));
         else
            Rewrite (N,
              New_Occurrence_Of (RTE (RE_Low_Order_First), Loc));
         end if;

         Set_Etype (N, RTE (RE_Bit_Order));
         Resolve (N);

         --  Reset incorrect indication of staticness

         Set_Is_Static_Expression (N, False);

      ------------------
      -- Bit_Position --
      ------------------

      --  Note: in generated code, we can have a Bit_Position attribute
      --  applied to a (naked) record component (i.e. the prefix is an
      --  identifier that references an E_Component or E_Discriminant
      --  entity directly, and this is interpreted as expected by Gigi.
      --  The following code will not tolerate such usage, but when the
      --  expander creates this special case, it marks it as analyzed
      --  immediately and sets an appropriate type.

      when Attribute_Bit_Position =>
         if Comes_From_Source (N) then
            Check_Component;
         end if;

         Set_Etype (N, Universal_Integer);

      ------------------
      -- Body_Version --
      ------------------

      when Attribute_Body_Version =>
         Check_E0;
         Check_Program_Unit;
         Set_Etype (N, RTE (RE_Version_String));

      --------------
      -- Callable --
      --------------

      when Attribute_Callable =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);
         Check_Task_Prefix;

      ------------
      -- Caller --
      ------------

      when Attribute_Caller => Caller : declare
         Ent        : Entity_Id;
         S          : Entity_Id;

      begin
         Check_E0;

         if Nkind_In (P, N_Identifier, N_Expanded_Name) then
            Ent := Entity (P);

            if not Is_Entry (Ent) then
               Error_Attr ("invalid entry name", N);
            end if;

         else
            Error_Attr ("invalid entry name", N);
            return;
         end if;

         for J in reverse 0 .. Scope_Stack.Last loop
            S := Scope_Stack.Table (J).Entity;

            if S = Scope (Ent) then
               Error_Attr ("Caller must appear in matching accept or body", N);
            elsif S = Ent then
               exit;
            end if;
         end loop;

         Set_Etype (N, RTE (RO_AT_Task_Id));
      end Caller;

      -------------
      -- Ceiling --
      -------------

      when Attribute_Ceiling =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      -----------
      -- Class --
      -----------

      when Attribute_Class =>
         Check_Restriction (No_Dispatch, N);
         Check_E0;
         Find_Type (N);

         --  Applying Class to untagged incomplete type is obsolescent in Ada
         --  2005. Note that we can't test Is_Tagged_Type here on P_Type, since
         --  this flag gets set by Find_Type in this situation.

         if Restriction_Check_Required (No_Obsolescent_Features)
           and then Ada_Version >= Ada_2005
           and then Ekind (P_Type) = E_Incomplete_Type
         then
            declare
               DN : constant Node_Id := Declaration_Node (P_Type);
            begin
               if Nkind (DN) = N_Incomplete_Type_Declaration
                 and then not Tagged_Present (DN)
               then
                  Check_Restriction (No_Obsolescent_Features, P);
               end if;
            end;
         end if;

      ------------------
      -- Code_Address --
      ------------------

      when Attribute_Code_Address =>
         Check_E0;

         if Nkind (P) = N_Attribute_Reference
           and then Nam_In (Attribute_Name (P), Name_Elab_Body, Name_Elab_Spec)
         then
            null;

         elsif not Is_Entity_Name (P)
           or else (Ekind (Entity (P)) /= E_Function
                      and then
                    Ekind (Entity (P)) /= E_Procedure)
         then
            Error_Attr ("invalid prefix for % attribute", P);
            Set_Address_Taken (Entity (P));

         --  Issue an error if the prefix denotes an eliminated subprogram

         else
            Check_For_Eliminated_Subprogram (P, Entity (P));
         end if;

         Set_Etype (N, RTE (RE_Address));

      ----------------------
      -- Compiler_Version --
      ----------------------

      when Attribute_Compiler_Version =>
         Check_E0;
         Check_Standard_Prefix;
         Rewrite (N, Make_String_Literal (Loc, "GNAT " & Gnat_Version_String));
         Analyze_And_Resolve (N, Standard_String);
         Set_Is_Static_Expression (N, True);

      --------------------
      -- Component_Size --
      --------------------

      when Attribute_Component_Size =>
         Check_E0;
         Set_Etype (N, Universal_Integer);

         --  Note: unlike other array attributes, unconstrained arrays are OK

         if Is_Array_Type (P_Type) and then not Is_Constrained (P_Type) then
            null;
         else
            Check_Array_Type;
         end if;

      -------------
      -- Compose --
      -------------

      when Attribute_Compose =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, Any_Integer);

      -----------------
      -- Constrained --
      -----------------

      when Attribute_Constrained =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);

         --  Case from RM J.4(2) of constrained applied to private type

         if Is_Entity_Name (P) and then Is_Type (Entity (P)) then
            Check_Restriction (No_Obsolescent_Features, P);

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("constrained for private type is an obsolescent feature "
                  & "(RM J.4)?j?", N);
            end if;

            --  If we are within an instance, the attribute must be legal
            --  because it was valid in the generic unit. Ditto if this is
            --  an inlining of a function declared in an instance.

            if In_Instance or else In_Inlined_Body then
               return;

            --  For sure OK if we have a real private type itself, but must
            --  be completed, cannot apply Constrained to incomplete type.

            elsif Is_Private_Type (Entity (P)) then

               --  Note: this is one of the Annex J features that does not
               --  generate a warning from -gnatwj, since in fact it seems
               --  very useful, and is used in the GNAT runtime.

               Check_Not_Incomplete_Type;
               return;
            end if;

         --  Normal (non-obsolescent case) of application to object of
         --  a discriminated type.

         else
            Check_Object_Reference (P);

            --  If N does not come from source, then we allow the
            --  the attribute prefix to be of a private type whose
            --  full type has discriminants. This occurs in cases
            --  involving expanded calls to stream attributes.

            if not Comes_From_Source (N) then
               P_Type := Underlying_Type (P_Type);
            end if;

            --  Must have discriminants or be an access type designating a type
            --  with discriminants. If it is a class-wide type it has unknown
            --  discriminants.

            if Has_Discriminants (P_Type)
              or else Has_Unknown_Discriminants (P_Type)
              or else
                (Is_Access_Type (P_Type)
                  and then Has_Discriminants (Designated_Type (P_Type)))
            then
               return;

            --  The rule given in 3.7.2 is part of static semantics, but the
            --  intent is clearly that it be treated as a legality rule, and
            --  rechecked in the visible part of an instance. Nevertheless
            --  the intent also seems to be it should legally apply to the
            --  actual of a formal with unknown discriminants, regardless of
            --  whether the actual has discriminants, in which case the value
            --  of the attribute is determined using the J.4 rules. This choice
            --  seems the most useful, and is compatible with existing tests.

            elsif In_Instance then
               return;

            --  Also allow an object of a generic type if extensions allowed
            --  and allow this for any type at all. (this may be obsolete ???)

            elsif (Is_Generic_Type (P_Type)
                    or else Is_Generic_Actual_Type (P_Type))
              and then Extensions_Allowed
            then
               return;
            end if;
         end if;

         --  Fall through if bad prefix

         Error_Attr_P
           ("prefix of % attribute must be object of discriminated type");

      ---------------
      -- Copy_Sign --
      ---------------

      when Attribute_Copy_Sign =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);

      -----------
      -- Count --
      -----------

      when Attribute_Count => Count : declare
         Ent : Entity_Id;
         S   : Entity_Id;
         Tsk : Entity_Id;

      begin
         Check_E0;

         if Nkind_In (P, N_Identifier, N_Expanded_Name) then
            Ent := Entity (P);

            if Ekind (Ent) /= E_Entry then
               Error_Attr ("invalid entry name", N);
            end if;

         elsif Nkind (P) = N_Indexed_Component then
            if not Is_Entity_Name (Prefix (P))
              or else  No (Entity (Prefix (P)))
              or else Ekind (Entity (Prefix (P))) /= E_Entry_Family
            then
               if Nkind (Prefix (P)) = N_Selected_Component
                 and then Present (Entity (Selector_Name (Prefix (P))))
                 and then Ekind (Entity (Selector_Name (Prefix (P)))) =
                                                             E_Entry_Family
               then
                  Error_Attr
                    ("attribute % must apply to entry of current task", P);

               else
                  Error_Attr ("invalid entry family name", P);
               end if;
               return;

            else
               Ent := Entity (Prefix (P));
            end if;

         elsif Nkind (P) = N_Selected_Component
           and then Present (Entity (Selector_Name (P)))
           and then Ekind (Entity (Selector_Name (P))) = E_Entry
         then
            Error_Attr
              ("attribute % must apply to entry of current task", P);

         else
            Error_Attr ("invalid entry name", N);
            return;
         end if;

         for J in reverse 0 .. Scope_Stack.Last loop
            S := Scope_Stack.Table (J).Entity;

            if S = Scope (Ent) then
               if Nkind (P) = N_Expanded_Name then
                  Tsk := Entity (Prefix (P));

                  --  The prefix denotes either the task type, or else a
                  --  single task whose task type is being analyzed.

                  if (Is_Type (Tsk) and then Tsk = S)
                    or else (not Is_Type (Tsk)
                              and then Etype (Tsk) = S
                              and then not (Comes_From_Source (S)))
                  then
                     null;
                  else
                     Error_Attr
                       ("Attribute % must apply to entry of current task", N);
                  end if;
               end if;

               exit;

            elsif Ekind (Scope (Ent)) in Task_Kind
              and then not Ekind_In (S, E_Block,
                                        E_Entry,
                                        E_Entry_Family,
                                        E_Loop)
            then
               Error_Attr ("Attribute % cannot appear in inner unit", N);

            elsif Ekind (Scope (Ent)) = E_Protected_Type
              and then not Has_Completion (Scope (Ent))
            then
               Error_Attr ("attribute % can only be used inside body", N);
            end if;
         end loop;

         if Is_Overloaded (P) then
            declare
               Index : Interp_Index;
               It    : Interp;

            begin
               Get_First_Interp (P, Index, It);
               while Present (It.Nam) loop
                  if It.Nam = Ent then
                     null;

                  --  Ada 2005 (AI-345): Do not consider primitive entry
                  --  wrappers generated for task or protected types.

                  elsif Ada_Version >= Ada_2005
                    and then not Comes_From_Source (It.Nam)
                  then
                     null;

                  else
                     Error_Attr ("ambiguous entry name", N);
                  end if;

                  Get_Next_Interp (Index, It);
               end loop;
            end;
         end if;

         Set_Etype (N, Universal_Integer);
      end Count;

      -----------------------
      -- Default_Bit_Order --
      -----------------------

      when Attribute_Default_Bit_Order => Default_Bit_Order : declare
         Target_Default_Bit_Order : System.Bit_Order;

      begin
         Check_Standard_Prefix;

         if Bytes_Big_Endian then
            Target_Default_Bit_Order := System.High_Order_First;
         else
            Target_Default_Bit_Order := System.Low_Order_First;
         end if;

         Rewrite (N,
           Make_Integer_Literal (Loc,
             UI_From_Int (System.Bit_Order'Pos (Target_Default_Bit_Order))));

         Set_Etype (N, Universal_Integer);
         Set_Is_Static_Expression (N);
      end Default_Bit_Order;

      ----------------------------------
      -- Default_Scalar_Storage_Order --
      ----------------------------------

      when Attribute_Default_Scalar_Storage_Order => Default_SSO : declare
         RE_Default_SSO : RE_Id;

      begin
         Check_Standard_Prefix;

         case Opt.Default_SSO is
            when ' ' =>
               if Bytes_Big_Endian then
                  RE_Default_SSO := RE_High_Order_First;
               else
                  RE_Default_SSO := RE_Low_Order_First;
               end if;

            when 'H' =>
               RE_Default_SSO := RE_High_Order_First;

            when 'L' =>
               RE_Default_SSO := RE_Low_Order_First;

            when others =>
               raise Program_Error;
         end case;

         Rewrite (N, New_Occurrence_Of (RTE (RE_Default_SSO), Loc));
      end Default_SSO;

      --------------
      -- Definite --
      --------------

      when Attribute_Definite =>
         Legal_Formal_Attribute;

      -----------
      -- Delta --
      -----------

      when Attribute_Delta =>
         Check_Fixed_Point_Type_0;
         Set_Etype (N, Universal_Real);

      ------------
      -- Denorm --
      ------------

      when Attribute_Denorm =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Standard_Boolean);

      -----------
      -- Deref --
      -----------

      when Attribute_Deref =>
         Check_Type;
         Check_E1;
         Resolve (E1, RTE (RE_Address));
         Set_Etype (N, P_Type);

      ---------------------
      -- Descriptor_Size --
      ---------------------

      when Attribute_Descriptor_Size =>
         Check_E0;

         if not Is_Entity_Name (P) or else not Is_Type (Entity (P)) then
            Error_Attr_P ("prefix of attribute % must denote a type");
         end if;

         Set_Etype (N, Universal_Integer);

      ------------
      -- Digits --
      ------------

      when Attribute_Digits =>
         Check_E0;
         Check_Type;

         if not Is_Floating_Point_Type (P_Type)
           and then not Is_Decimal_Fixed_Point_Type (P_Type)
         then
            Error_Attr_P
              ("prefix of % attribute must be float or decimal type");
         end if;

         Set_Etype (N, Universal_Integer);

      ---------------
      -- Elab_Body --
      ---------------

      --  Also handles processing for Elab_Spec and Elab_Subp_Body

      when Attribute_Elab_Body
         | Attribute_Elab_Spec
         | Attribute_Elab_Subp_Body
      =>
         Check_E0;
         Check_Unit_Name (P);
         Set_Etype (N, Standard_Void_Type);

         --  We have to manually call the expander in this case to get
         --  the necessary expansion (normally attributes that return
         --  entities are not expanded).

         Expand (N);

      ---------------
      -- Elab_Spec --
      ---------------

      --  Shares processing with Elab_Body

      ----------------
      -- Elaborated --
      ----------------

      when Attribute_Elaborated =>
         Check_E0;
         Check_Unit_Name (P);
         Set_Etype (N, Standard_Boolean);

      ----------
      -- Emax --
      ----------

      when Attribute_Emax =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      -------------
      -- Enabled --
      -------------

      when Attribute_Enabled =>
         Check_Either_E0_Or_E1;

         if Present (E1) then
            if not Is_Entity_Name (E1) or else No (Entity (E1)) then
               Error_Msg_N ("entity name expected for Enabled attribute", E1);
               E1 := Empty;
            end if;
         end if;

         if Nkind (P) /= N_Identifier then
            Error_Msg_N ("identifier expected (check name)", P);
         elsif Get_Check_Id (Chars (P)) = No_Check_Id then
            Error_Msg_N ("& is not a recognized check name", P);
         end if;

         Set_Etype (N, Standard_Boolean);

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep =>

         --  T'Enum_Rep (X) case

         if Present (E1) then
            Check_E1;
            Check_Discrete_Type;
            Resolve (E1, P_Base_Type);

         --  X'Enum_Rep case. X must be an object or enumeration literal, and
         --  it must be of a discrete type.

         elsif not
           ((Is_Object_Reference (P)
               or else
                 (Is_Entity_Name (P)
                    and then Ekind (Entity (P)) = E_Enumeration_Literal))
             and then Is_Discrete_Type (Etype (P)))
         then
            Error_Attr_P ("prefix of % attribute must be discrete object");
         end if;

         Set_Etype (N, Universal_Integer);

      --------------
      -- Enum_Val --
      --------------

      when Attribute_Enum_Val =>
         Check_E1;
         Check_Type;

         if not Is_Enumeration_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be enumeration type");
         end if;

         --  If the enumeration type has a standard representation, the effect
         --  is the same as 'Val, so rewrite the attribute as a 'Val.

         if not Has_Non_Standard_Rep (P_Base_Type) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix         => Relocate_Node (Prefix (N)),
                Attribute_Name => Name_Val,
                Expressions    => New_List (Relocate_Node (E1))));
            Analyze_And_Resolve (N, P_Base_Type);

         --  Non-standard representation case (enumeration with holes)

         else
            Check_Enum_Image;
            Resolve (E1, Any_Integer);
            Set_Etype (N, P_Base_Type);
         end if;

      -------------
      -- Epsilon --
      -------------

      when Attribute_Epsilon =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Real);

      --------------
      -- Exponent --
      --------------

      when Attribute_Exponent =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, Universal_Integer);
         Resolve (E1, P_Base_Type);

      ------------------
      -- External_Tag --
      ------------------

      when Attribute_External_Tag =>
         Check_E0;
         Check_Type;

         Set_Etype (N, Standard_String);

         if not Is_Tagged_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be tagged");
         end if;

      ---------------
      -- Fast_Math --
      ---------------

      when Attribute_Fast_Math =>
         Check_Standard_Prefix;
         Rewrite (N, New_Occurrence_Of (Boolean_Literals (Fast_Math), Loc));

      -----------------------
      -- Finalization_Size --
      -----------------------

      when Attribute_Finalization_Size =>
         Check_E0;

         --  The prefix denotes an object

         if Is_Object_Reference (P) then
            Check_Object_Reference (P);

         --  The prefix denotes a type

         elsif Is_Entity_Name (P) and then Is_Type (Entity (P)) then
            Check_Type;
            Check_Not_Incomplete_Type;

            --  Attribute 'Finalization_Size is not defined for class-wide
            --  types because it is not possible to know statically whether
            --  a definite type will have controlled components or not.

            if Is_Class_Wide_Type (Etype (P)) then
               Error_Attr_P
                 ("prefix of % attribute cannot denote a class-wide type");
            end if;

         --  The prefix denotes an illegal construct

         else
            Error_Attr_P
              ("prefix of % attribute must be a definite type or an object");
         end if;

         Set_Etype (N, Universal_Integer);

      -----------
      -- First --
      -----------

      when Attribute_First =>
         Check_Array_Or_Scalar_Type;
         Bad_Attribute_For_Predicate;

      ---------------
      -- First_Bit --
      ---------------

      when Attribute_First_Bit =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      -----------------
      -- First_Valid --
      -----------------

      when Attribute_First_Valid =>
         Check_First_Last_Valid;
         Set_Etype (N, P_Type);

      -----------------
      -- Fixed_Value --
      -----------------

      when Attribute_Fixed_Value =>
         Check_E1;
         Check_Fixed_Point_Type;
         Resolve (E1, Any_Integer);
         Set_Etype (N, P_Base_Type);

      -----------
      -- Floor --
      -----------

      when Attribute_Floor =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------
      -- Fore --
      ----------

      when Attribute_Fore =>
         Check_Fixed_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      --------------
      -- Fraction --
      --------------

      when Attribute_Fraction =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      --------------
      -- From_Any --
      --------------

      when Attribute_From_Any =>
         Check_E1;
         Check_PolyORB_Attribute;
         Set_Etype (N, P_Base_Type);

      -----------------------
      -- Has_Access_Values --
      -----------------------

      when Attribute_Has_Access_Values =>
         Check_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      ----------------------
      -- Has_Same_Storage --
      ----------------------

      when Attribute_Has_Same_Storage =>
         Check_E1;

         --  The arguments must be objects of any type

         Analyze_And_Resolve (P);
         Analyze_And_Resolve (E1);
         Check_Object_Reference (P);
         Check_Object_Reference (E1);
         Set_Etype (N, Standard_Boolean);

      -----------------------
      -- Has_Tagged_Values --
      -----------------------

      when Attribute_Has_Tagged_Values =>
         Check_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      -----------------------
      -- Has_Discriminants --
      -----------------------

      when Attribute_Has_Discriminants =>
         Legal_Formal_Attribute;

      --------------
      -- Identity --
      --------------

      when Attribute_Identity =>
         Check_E0;
         Analyze (P);

         if Etype (P) = Standard_Exception_Type then
            Set_Etype (N, RTE (RE_Exception_Id));

         --  Ada 2005 (AI-345): Attribute 'Identity may be applied to task
         --  interface class-wide types.

         elsif Is_Task_Type (Etype (P))
           or else (Is_Access_Type (Etype (P))
                      and then Is_Task_Type (Designated_Type (Etype (P))))
           or else (Ada_Version >= Ada_2005
                      and then Ekind (Etype (P)) = E_Class_Wide_Type
                      and then Is_Interface (Etype (P))
                      and then Is_Task_Interface (Etype (P)))
         then
            Resolve (P);
            Set_Etype (N, RTE (RO_AT_Task_Id));

         else
            if Ada_Version >= Ada_2005 then
               Error_Attr_P
                 ("prefix of % attribute must be an exception, a task or a "
                  & "task interface class-wide object");
            else
               Error_Attr_P
                 ("prefix of % attribute must be a task or an exception");
            end if;
         end if;

      -----------
      -- Image --
      -----------

      when Attribute_Image =>
         Check_SPARK_05_Restriction_On_Attribute;

         if Is_Image_Applied_To_Object (P, P_Type) then
            Check_Object_Reference_Image (Standard_String);
            return;
         end if;

         Check_Scalar_Type;
         Set_Etype (N, Standard_String);

         if Is_Real_Type (P_Type) then
            if Ada_Version = Ada_83 and then Comes_From_Source (N) then
               Error_Msg_Name_1 := Aname;
               Error_Msg_N
                 ("(Ada 83) % attribute not allowed for real types", N);
            end if;
         end if;

         if Is_Enumeration_Type (P_Type) then
            Check_Restriction (No_Enumeration_Maps, N);
         end if;

         Check_E1;
         Resolve (E1, P_Base_Type);
         Check_Enum_Image;
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO. Note the check of Comes_From_Source
         --  to avoid giving a duplicate message for Img expanded into Image.

         if Restriction_Check_Required (No_Fixed_IO)
           and then Comes_From_Source (N)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;

      ---------
      -- Img --
      ---------

      when Attribute_Img =>
         Check_Object_Reference_Image (Standard_String);

      -----------
      -- Input --
      -----------

      when Attribute_Input =>
         Check_E1;
         Check_Stream_Attribute (TSS_Stream_Input);
         Set_Etype (N, P_Base_Type);

      -------------------
      -- Integer_Value --
      -------------------

      when Attribute_Integer_Value =>
         Check_E1;
         Check_Integer_Type;
         Resolve (E1, Any_Fixed);

         --  Signal an error if argument type is not a specific fixed-point
         --  subtype. An error has been signalled already if the argument
         --  was not of a fixed-point type.

         if Etype (E1) = Any_Fixed and then not Error_Posted (E1) then
            Error_Attr ("argument of % must be of a fixed-point type", E1);
         end if;

         Set_Etype (N, P_Base_Type);

      -------------------
      -- Invalid_Value --
      -------------------

      when Attribute_Invalid_Value =>
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, P_Base_Type);
         Invalid_Value_Used := True;

      -----------
      -- Large --
      -----------

      when Attribute_Large =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Real);

      ----------
      -- Last --
      ----------

      when Attribute_Last =>
         Check_Array_Or_Scalar_Type;
         Bad_Attribute_For_Predicate;

      --------------
      -- Last_Bit --
      --------------

      when Attribute_Last_Bit =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      ----------------
      -- Last_Valid --
      ----------------

      when Attribute_Last_Valid =>
         Check_First_Last_Valid;
         Set_Etype (N, P_Type);

      ------------------
      -- Leading_Part --
      ------------------

      when Attribute_Leading_Part =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, Any_Integer);

      ------------
      -- Length --
      ------------

      when Attribute_Length =>
         Check_Array_Type;
         Set_Etype (N, Universal_Integer);

      -------------------
      -- Library_Level --
      -------------------

      when Attribute_Library_Level =>
         Check_E0;

         if not Is_Entity_Name (P) then
            Error_Attr_P ("prefix of % attribute must be an entity name");
         end if;

         if not Inside_A_Generic then
            Set_Boolean_Result (N,
              Is_Library_Level_Entity (Entity (P)));
         end if;

         Set_Etype (N, Standard_Boolean);

      ---------------
      -- Lock_Free --
      ---------------

      when Attribute_Lock_Free =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);

         if not Is_Protected_Type (P_Type) then
            Error_Attr_P
              ("prefix of % attribute must be a protected object");
         end if;

      ----------------
      -- Loop_Entry --
      ----------------

      when Attribute_Loop_Entry => Loop_Entry : declare
         procedure Check_References_In_Prefix (Loop_Id : Entity_Id);
         --  Inspect the prefix for any uses of entities declared within the
         --  related loop. Loop_Id denotes the loop identifier.

         --------------------------------
         -- Check_References_In_Prefix --
         --------------------------------

         procedure Check_References_In_Prefix (Loop_Id : Entity_Id) is
            Loop_Decl : constant Node_Id := Label_Construct (Parent (Loop_Id));

            function Check_Reference (Nod : Node_Id) return Traverse_Result;
            --  Determine whether a reference mentions an entity declared
            --  within the related loop.

            function Declared_Within (Nod : Node_Id) return Boolean;
            --  Determine whether Nod appears in the subtree of Loop_Decl

            ---------------------
            -- Check_Reference --
            ---------------------

            function Check_Reference (Nod : Node_Id) return Traverse_Result is
            begin
               if Nkind (Nod) = N_Identifier
                 and then Present (Entity (Nod))
                 and then Declared_Within (Declaration_Node (Entity (Nod)))
               then
                  Error_Attr
                    ("prefix of attribute % cannot reference local entities",
                     Nod);
                  return Abandon;
               else
                  return OK;
               end if;
            end Check_Reference;

            procedure Check_References is new Traverse_Proc (Check_Reference);

            ---------------------
            -- Declared_Within --
            ---------------------

            function Declared_Within (Nod : Node_Id) return Boolean is
               Stmt : Node_Id;

            begin
               Stmt := Nod;
               while Present (Stmt) loop
                  if Stmt = Loop_Decl then
                     return True;

                  --  Prevent the search from going too far

                  elsif Is_Body_Or_Package_Declaration (Stmt) then
                     exit;
                  end if;

                  Stmt := Parent (Stmt);
               end loop;

               return False;
            end Declared_Within;

         --  Start of processing for Check_Prefix_For_Local_References

         begin
            Check_References (P);
         end Check_References_In_Prefix;

         --  Local variables

         Context   : constant Node_Id := Parent (N);
         Attr      : Node_Id;
         Encl_Loop : Node_Id   := Empty;
         Encl_Prag : Node_Id   := Empty;
         Loop_Id   : Entity_Id := Empty;
         Scop      : Entity_Id;
         Stmt      : Node_Id;

      --  Start of processing for Loop_Entry

      begin
         Attr := N;

         --  Set the type of the attribute now to ensure the successful
         --  continuation of analysis even if the attribute is misplaced.

         Set_Etype (Attr, P_Type);

         --  Attribute 'Loop_Entry may appear in several flavors:

         --    * Prefix'Loop_Entry - in this form, the attribute applies to the
         --        nearest enclosing loop.

         --    * Prefix'Loop_Entry (Expr) - depending on what Expr denotes, the
         --        attribute may be related to a loop denoted by label Expr or
         --        the prefix may denote an array object and Expr may act as an
         --        indexed component.

         --    * Prefix'Loop_Entry (Expr1, ..., ExprN) - the attribute applies
         --        to the nearest enclosing loop, all expressions are part of
         --        an indexed component.

         --    * Prefix'Loop_Entry (Expr) (...) (...) - depending on what Expr
         --        denotes, the attribute may be related to a loop denoted by
         --        label Expr or the prefix may denote a multidimensional array
         --        array object and Expr along with the rest of the expressions
         --        may act as indexed components.

         --  Regardless of variations, the attribute reference does not have an
         --  expression list. Instead, all available expressions are stored as
         --  indexed components.

         --  When the attribute is part of an indexed component, find the first
         --  expression as it will determine the semantics of 'Loop_Entry.

         --  If the attribute is itself an index in an indexed component, i.e.
         --  a member of a list, the context itself is not relevant (the code
         --  below would lead to an infinite loop) and the attribute applies
         --  to the enclosing loop.

         if Nkind (Context) = N_Indexed_Component
           and then not Is_List_Member (N)
         then
            E1 := First (Expressions (Context));
            E2 := Next (E1);

            --  The attribute reference appears in the following form:

            --    Prefix'Loop_Entry (Exp1, Expr2, ..., ExprN) [(...)]

            --  In this case, the loop name is omitted and no rewriting is
            --  required.

            if Present (E2) then
               null;

            --  The form of the attribute is:

            --    Prefix'Loop_Entry (Expr) [(...)]

            --  If Expr denotes a loop entry, the whole attribute and indexed
            --  component will have to be rewritten to reflect this relation.

            else
               pragma Assert (Present (E1));

               --  Do not expand the expression as it may have side effects.
               --  Simply preanalyze to determine whether it is a loop name or
               --  something else.

               Preanalyze_And_Resolve (E1);

               if Is_Entity_Name (E1)
                 and then Present (Entity (E1))
                 and then Ekind (Entity (E1)) = E_Loop
               then
                  Loop_Id := Entity (E1);

                  --  Transform the attribute and enclosing indexed component

                  Set_Expressions (N, Expressions (Context));
                  Rewrite   (Context, N);
                  Set_Etype (Context, P_Type);

                  Attr := Context;
               end if;
            end if;
         end if;

         --  The prefix must denote an object

         if not Is_Object_Reference (P) then
            Error_Attr_P ("prefix of attribute % must denote an object");
         end if;

         --  The prefix cannot be of a limited type because the expansion of
         --  Loop_Entry must create a constant initialized by the evaluated
         --  prefix.

         if Is_Limited_View (Etype (P)) then
            Error_Attr_P ("prefix of attribute % cannot be limited");
         end if;

         --  Climb the parent chain to verify the location of the attribute and
         --  find the enclosing loop.

         Stmt := Attr;
         while Present (Stmt) loop

            --  Locate the corresponding enclosing pragma. Note that in the
            --  case of Assert[And_Cut] and Assume, we have already checked
            --  that the pragma appears in an appropriate loop location.

            if Nkind (Original_Node (Stmt)) = N_Pragma
              and then Nam_In (Pragma_Name_Unmapped (Original_Node (Stmt)),
                               Name_Loop_Invariant,
                               Name_Loop_Variant,
                               Name_Assert,
                               Name_Assert_And_Cut,
                               Name_Assume)
            then
               Encl_Prag := Original_Node (Stmt);

            --  Locate the enclosing loop (if any). Note that Ada 2012 array
            --  iteration may be expanded into several nested loops, we are
            --  interested in the outermost one which has the loop identifier,
            --  and comes from source.

            elsif Nkind (Stmt) = N_Loop_Statement
              and then Present (Identifier (Stmt))
              and then Comes_From_Source (Original_Node (Stmt))
              and then Nkind (Original_Node (Stmt)) = N_Loop_Statement
            then
               Encl_Loop := Stmt;

               --  The original attribute reference may lack a loop name. Use
               --  the name of the enclosing loop because it is the related
               --  loop.

               if No (Loop_Id) then
                  Loop_Id := Entity (Identifier (Encl_Loop));
               end if;

               exit;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Stmt) then
               exit;
            end if;

            Stmt := Parent (Stmt);
         end loop;

         --  Loop_Entry must appear within a Loop_Assertion pragma (Assert,
         --  Assert_And_Cut, Assume count as loop assertion pragmas for this
         --  purpose if they appear in an appropriate location in a loop,
         --  which was already checked by the top level pragma circuit).

         --  Loop_Entry also denotes a value and as such can appear within an
         --  expression that is an argument for another loop aspect. In that
         --  case it will have been expanded into the corresponding assignment.

         if Expander_Active
           and then Nkind (Parent (N)) = N_Assignment_Statement
           and then not Comes_From_Source (Parent (N))
         then
            null;

         elsif No (Encl_Prag) then
            Error_Attr ("attribute% must appear within appropriate pragma", N);
         end if;

         --  A Loop_Entry that applies to a given loop statement must not
         --  appear within a body of accept statement, if this construct is
         --  itself enclosed by the given loop statement.

         for Index in reverse 0 .. Scope_Stack.Last loop
            Scop := Scope_Stack.Table (Index).Entity;

            if Ekind (Scop) = E_Loop and then Scop = Loop_Id then
               exit;
            elsif Ekind_In (Scop, E_Block, E_Loop, E_Return_Statement) then
               null;
            else
               Error_Attr
                 ("attribute % cannot appear in body or accept statement", N);
               exit;
            end if;
         end loop;

         --  The prefix cannot mention entities declared within the related
         --  loop because they will not be visible once the prefix is moved
         --  outside the loop.

         Check_References_In_Prefix (Loop_Id);

         --  The prefix must denote a static entity if the pragma does not
         --  apply to the innermost enclosing loop statement, or if it appears
         --  within a potentially unevaluated epxression.

         if Is_Entity_Name (P)
           or else Nkind (Parent (P)) = N_Object_Renaming_Declaration
           or else Statically_Denotes_Object (P)
         then
            null;

         elsif Present (Encl_Loop)
           and then Entity (Identifier (Encl_Loop)) /= Loop_Id
         then
            Error_Attr_P
              ("prefix of attribute % that applies to outer loop must denote "
               & "an entity");

         elsif Is_Potentially_Unevaluated (P) then
            Uneval_Old_Msg;
         end if;

         --  Replace the Loop_Entry attribute reference by its prefix if the
         --  related pragma is ignored. This transformation is OK with respect
         --  to typing because Loop_Entry's type is that of its prefix. This
         --  early transformation also avoids the generation of a useless loop
         --  entry constant.

         if Present (Encl_Prag) and then Is_Ignored (Encl_Prag) then
            Rewrite (N, Relocate_Node (P));
            Preanalyze_And_Resolve (N);

         else
            Preanalyze_And_Resolve (P);
         end if;
      end Loop_Entry;

      -------------
      -- Machine --
      -------------

      when Attribute_Machine =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ------------------
      -- Machine_Emax --
      ------------------

      when Attribute_Machine_Emax =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      ------------------
      -- Machine_Emin --
      ------------------

      when Attribute_Machine_Emin =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      ----------------------
      -- Machine_Mantissa --
      ----------------------

      when Attribute_Machine_Mantissa =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      -----------------------
      -- Machine_Overflows --
      -----------------------

      when Attribute_Machine_Overflows =>
         Check_Real_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      -------------------
      -- Machine_Radix --
      -------------------

      when Attribute_Machine_Radix =>
         Check_Real_Type;
         Check_E0;
         Set_Etype (N, Universal_Integer);

      ----------------------
      -- Machine_Rounding --
      ----------------------

      when Attribute_Machine_Rounding =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      --------------------
      -- Machine_Rounds --
      --------------------

      when Attribute_Machine_Rounds =>
         Check_Real_Type;
         Check_E0;
         Set_Etype (N, Standard_Boolean);

      ------------------
      -- Machine_Size --
      ------------------

      when Attribute_Machine_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      --------------
      -- Mantissa --
      --------------

      when Attribute_Mantissa =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Integer);

      ---------
      -- Max --
      ---------

      when Attribute_Max =>
         Min_Max;

      ----------------------------------
      -- Max_Alignment_For_Allocation --
      ----------------------------------

      when Attribute_Max_Size_In_Storage_Elements =>
         Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements;

      ----------------------------------
      -- Max_Size_In_Storage_Elements --
      ----------------------------------

      when Attribute_Max_Alignment_For_Allocation =>
         Max_Alignment_For_Allocation_Max_Size_In_Storage_Elements;

      -----------------------
      -- Maximum_Alignment --
      -----------------------

      when Attribute_Maximum_Alignment =>
         Standard_Attribute (Ttypes.Maximum_Alignment);

      --------------------
      -- Mechanism_Code --
      --------------------

      when Attribute_Mechanism_Code =>
         if not Is_Entity_Name (P)
           or else not Is_Subprogram (Entity (P))
         then
            Error_Attr_P ("prefix of % attribute must be subprogram");
         end if;

         Check_Either_E0_Or_E1;

         if Present (E1) then
            Resolve (E1, Any_Integer);
            Set_Etype (E1, Standard_Integer);

            if not Is_OK_Static_Expression (E1) then
               Flag_Non_Static_Expr
                 ("expression for parameter number must be static!", E1);
               Error_Attr;

            elsif UI_To_Int (Intval (E1)) > Number_Formals (Entity (P))
              or else UI_To_Int (Intval (E1)) < 0
            then
               Error_Attr ("invalid parameter number for % attribute", E1);
            end if;
         end if;

         Set_Etype (N, Universal_Integer);

      ---------
      -- Min --
      ---------

      when Attribute_Min =>
         Min_Max;

      ---------
      -- Mod --
      ---------

      when Attribute_Mod =>

         --  Note: this attribute is only allowed in Ada 2005 mode, but
         --  we do not need to test that here, since Mod is only recognized
         --  as an attribute name in Ada 2005 mode during the parse.

         Check_E1;
         Check_Modular_Integer_Type;
         Resolve (E1, Any_Integer);
         Set_Etype (N, P_Base_Type);

      -----------
      -- Model --
      -----------

      when Attribute_Model =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------------
      -- Model_Emin --
      ----------------

      when Attribute_Model_Emin =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      -------------------
      -- Model_Epsilon --
      -------------------

      when Attribute_Model_Epsilon =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Real);

      --------------------
      -- Model_Mantissa --
      --------------------

      when Attribute_Model_Mantissa =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      -----------------
      -- Model_Small --
      -----------------

      when Attribute_Model_Small =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Real);

      -------------
      -- Modulus --
      -------------

      when Attribute_Modulus =>
         Check_E0;
         Check_Modular_Integer_Type;
         Set_Etype (N, Universal_Integer);

      --------------------
      -- Null_Parameter --
      --------------------

      when Attribute_Null_Parameter => Null_Parameter : declare
         Parnt  : constant Node_Id := Parent (N);
         GParnt : constant Node_Id := Parent (Parnt);

         procedure Bad_Null_Parameter (Msg : String);
         --  Used if bad Null parameter attribute node is found. Issues
         --  given error message, and also sets the type to Any_Type to
         --  avoid blowups later on from dealing with a junk node.

         procedure Must_Be_Imported (Proc_Ent : Entity_Id);
         --  Called to check that Proc_Ent is imported subprogram

         ------------------------
         -- Bad_Null_Parameter --
         ------------------------

         procedure Bad_Null_Parameter (Msg : String) is
         begin
            Error_Msg_N (Msg, N);
            Set_Etype (N, Any_Type);
         end Bad_Null_Parameter;

         ----------------------
         -- Must_Be_Imported --
         ----------------------

         procedure Must_Be_Imported (Proc_Ent : Entity_Id) is
            Pent : constant Entity_Id := Ultimate_Alias (Proc_Ent);

         begin
            --  Ignore check if procedure not frozen yet (we will get
            --  another chance when the default parameter is reanalyzed)

            if not Is_Frozen (Pent) then
               return;

            elsif not Is_Imported (Pent) then
               Bad_Null_Parameter
                 ("Null_Parameter can only be used with imported subprogram");

            else
               return;
            end if;
         end Must_Be_Imported;

      --  Start of processing for Null_Parameter

      begin
         Check_Type;
         Check_E0;
         Set_Etype (N, P_Type);

         --  Case of attribute used as default expression

         if Nkind (Parnt) = N_Parameter_Specification then
            Must_Be_Imported (Defining_Entity (GParnt));

         --  Case of attribute used as actual for subprogram (positional)

         elsif Nkind (Parnt) in N_Subprogram_Call
            and then Is_Entity_Name (Name (Parnt))
         then
            Must_Be_Imported (Entity (Name (Parnt)));

         --  Case of attribute used as actual for subprogram (named)

         elsif Nkind (Parnt) = N_Parameter_Association
           and then Nkind (GParnt) in N_Subprogram_Call
           and then Is_Entity_Name (Name (GParnt))
         then
            Must_Be_Imported (Entity (Name (GParnt)));

         --  Not an allowed case

         else
            Bad_Null_Parameter
              ("Null_Parameter must be actual or default parameter");
         end if;
      end Null_Parameter;

      -----------------
      -- Object_Size --
      -----------------

      when Attribute_Object_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      ---------
      -- Old --
      ---------

      when Attribute_Old => Old : declare
         procedure Check_References_In_Prefix (Subp_Id : Entity_Id);
         --  Inspect the contents of the prefix and detect illegal uses of a
         --  nested 'Old, attribute 'Result or a use of an entity declared in
         --  the related postcondition expression. Subp_Id is the subprogram to
         --  which the related postcondition applies.

         --------------------------------
         -- Check_References_In_Prefix --
         --------------------------------

         procedure Check_References_In_Prefix (Subp_Id : Entity_Id) is
            function Check_Reference (Nod : Node_Id) return Traverse_Result;
            --  Detect attribute 'Old, attribute 'Result of a use of an entity
            --  and perform the appropriate semantic check.

            ---------------------
            -- Check_Reference --
            ---------------------

            function Check_Reference (Nod : Node_Id) return Traverse_Result is
            begin
               --  Attributes 'Old and 'Result cannot appear in the prefix of
               --  another attribute 'Old.

               if Nkind (Nod) = N_Attribute_Reference
                 and then Nam_In (Attribute_Name (Nod), Name_Old,
                                                        Name_Result)
               then
                  Error_Msg_Name_1 := Attribute_Name (Nod);
                  Error_Msg_Name_2 := Name_Old;
                  Error_Msg_N
                    ("attribute % cannot appear in the prefix of attribute %",
                     Nod);
                  return Abandon;

               --  Entities mentioned within the prefix of attribute 'Old must
               --  be global to the related postcondition. If this is not the
               --  case, then the scope of the local entity is nested within
               --  that of the subprogram.

               elsif Is_Entity_Name (Nod)
                 and then Present (Entity (Nod))
                 and then Scope_Within (Scope (Entity (Nod)), Subp_Id)
               then
                  Error_Attr
                    ("prefix of attribute % cannot reference local entities",
                     Nod);
                  return Abandon;

               --  Otherwise keep inspecting the prefix

               else
                  return OK;
               end if;
            end Check_Reference;

            procedure Check_References is new Traverse_Proc (Check_Reference);

         --  Start of processing for Check_References_In_Prefix

         begin
            Check_References (P);
         end Check_References_In_Prefix;

         --  Local variables

         Legal    : Boolean;
         Pref_Id  : Entity_Id;
         Pref_Typ : Entity_Id;
         Spec_Id  : Entity_Id;

      --  Start of processing for Old

      begin
         --  The attribute reference is a primary. If any expressions follow,
         --  then the attribute reference is an indexable object. Transform the
         --  attribute into an indexed component and analyze it.

         if Present (E1) then
            Rewrite (N,
              Make_Indexed_Component (Loc,
                Prefix      =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Relocate_Node (P),
                    Attribute_Name => Name_Old),
                Expressions => Expressions (N)));
            Analyze (N);
            return;
         end if;

         Analyze_Attribute_Old_Result (Legal, Spec_Id);

         --  The aspect or pragma where attribute 'Old resides should be
         --  associated with a subprogram declaration or a body. If this is not
         --  the case, then the aspect or pragma is illegal. Return as analysis
         --  cannot be carried out.

         --  The exception to this rule is when generating C since in this case
         --  postconditions are inlined.

         if No (Spec_Id)
           and then Modify_Tree_For_C
           and then In_Inlined_Body
         then
            Spec_Id := Entity (P);

         elsif not Legal then
            return;
         end if;

         --  The prefix must be preanalyzed as the full analysis will take
         --  place during expansion.

         Preanalyze_And_Resolve (P);

         --  Ensure that the prefix does not contain attributes 'Old or 'Result

         Check_References_In_Prefix (Spec_Id);

         --  Set the type of the attribute now to prevent cascaded errors

         Pref_Typ := Etype (P);
         Set_Etype (N, Pref_Typ);

         --  Legality checks

         if Is_Limited_Type (Pref_Typ) then
            Error_Attr ("attribute % cannot apply to limited objects", P);
         end if;

         --  The prefix is a simple name

         if Is_Entity_Name (P) and then Present (Entity (P)) then
            Pref_Id := Entity (P);

            --  Emit a warning when the prefix is a constant. Note that the use
            --  of Error_Attr would reset the type of N to Any_Type even though
            --  this is a warning. Use Error_Msg_XXX instead.

            if Is_Constant_Object (Pref_Id) then
               Error_Msg_Name_1 := Name_Old;
               Error_Msg_N
                 ("??attribute % applied to constant has no effect", P);
            end if;

         --  Otherwise the prefix is not a simple name

         else
            --  Ensure that the prefix of attribute 'Old is an entity when it
            --  is potentially unevaluated (6.1.1 (27/3)).

            if Is_Potentially_Unevaluated (N)
              and then not Statically_Denotes_Object (P)
            then
               Uneval_Old_Msg;

            --  Detect a possible infinite recursion when the prefix denotes
            --  the related function.

            --    function Func (...) return ...
            --      with Post => Func'Old ...;

            --  The function may be specified in qualified form X.Y where X is
            --  a protected object and Y is a protected function. In that case
            --  ensure that the qualified form has an entity.

            elsif Nkind (P) = N_Function_Call
              and then Nkind (Name (P)) in N_Has_Entity
            then
               Pref_Id := Entity (Name (P));

               if Ekind_In (Spec_Id, E_Function, E_Generic_Function)
                 and then Pref_Id = Spec_Id
               then
                  Error_Msg_Warn := SPARK_Mode /= On;
                  Error_Msg_N ("!possible infinite recursion<<", P);
                  Error_Msg_N ("\!??Storage_Error ]<<", P);
               end if;
            end if;

            --  The prefix of attribute 'Old may refer to a component of a
            --  formal parameter. In this case its expansion may generate
            --  actual subtypes that are referenced in an inner context and
            --  that must be elaborated within the subprogram itself. If the
            --  prefix includes a function call, it may involve finalization
            --  actions that should be inserted when the attribute has been
            --  rewritten as a declaration. Create a declaration for the prefix
            --  and insert it at the start of the enclosing subprogram. This is
            --  an expansion activity that has to be performed now to prevent
            --  out-of-order issues.

            --  This expansion is both harmful and not needed in SPARK mode,
            --  since the formal verification back end relies on the types of
            --  nodes (hence is not robust w.r.t. a change to base type here),
            --  and does not suffer from the out-of-order issue described
            --  above. Thus, this expansion is skipped in SPARK mode.

            --  The expansion is not relevant for discrete types, which will
            --  not generate extra declarations, and where use of the base type
            --  may lead to spurious errors if context is a case.

            if not GNATprove_Mode then
               if not Is_Discrete_Type (Pref_Typ) then
                  Pref_Typ := Base_Type (Pref_Typ);
               end if;

               Set_Etype (N, Pref_Typ);
               Set_Etype (P, Pref_Typ);

               Analyze_Dimension (N);
               Expand (N);
            end if;
         end if;
      end Old;

      ----------------------
      -- Overlaps_Storage --
      ----------------------

      when Attribute_Overlaps_Storage =>
         Check_E1;

         --  Both arguments must be objects of any type

         Analyze_And_Resolve (P);
         Analyze_And_Resolve (E1);
         Check_Object_Reference (P);
         Check_Object_Reference (E1);
         Set_Etype (N, Standard_Boolean);

      ------------
      -- Output --
      ------------

      when Attribute_Output =>
         Check_E2;
         Check_Stream_Attribute (TSS_Stream_Output);
         Set_Etype (N, Standard_Void_Type);
         Resolve (N, Standard_Void_Type);

      ------------------
      -- Partition_ID --
      ------------------

      when Attribute_Partition_ID =>
         Check_E0;

         if P_Type /= Any_Type then
            if not Is_Library_Level_Entity (Entity (P)) then
               Error_Attr_P
                 ("prefix of % attribute must be library-level entity");

            --  The defining entity of prefix should not be declared inside a
            --  Pure unit. RM E.1(8). Is_Pure was set during declaration.

            elsif Is_Entity_Name (P)
              and then Is_Pure (Entity (P))
            then
               Error_Attr_P ("prefix of% attribute must not be declared pure");
            end if;
         end if;

         Set_Etype (N, Universal_Integer);

      -------------------------
      -- Passed_By_Reference --
      -------------------------

      when Attribute_Passed_By_Reference =>
         Check_E0;
         Check_Type;
         Set_Etype (N, Standard_Boolean);

      ------------------
      -- Pool_Address --
      ------------------

      when Attribute_Pool_Address =>
         Check_E0;
         Set_Etype (N, RTE (RE_Address));

      ---------
      -- Pos --
      ---------

      when Attribute_Pos =>
         Check_Discrete_Type;
         Check_E1;

         if Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         Resolve (E1, P_Base_Type);
         Set_Etype (N, Universal_Integer);

      --------------
      -- Position --
      --------------

      when Attribute_Position =>
         Check_Component;
         Set_Etype (N, Universal_Integer);

      ----------
      -- Pred --
      ----------

      when Attribute_Pred =>
         Check_Scalar_Type;
         Check_E1;

         if Is_Real_Type (P_Type) or else Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

         --  Since Pred works on the base type, we normally do no check for the
         --  floating-point case, since the base type is unconstrained. But we
         --  make an exception in Check_Float_Overflow mode.

         if Is_Floating_Point_Type (P_Type) then
            if not Range_Checks_Suppressed (P_Base_Type) then
               Set_Do_Range_Check (E1);
            end if;

         --  If not modular type, test for overflow check required

         else
            if not Is_Modular_Integer_Type (P_Type)
              and then not Range_Checks_Suppressed (P_Base_Type)
            then
               Enable_Range_Check (E1);
            end if;
         end if;

      --------------
      -- Priority --
      --------------

      --  Ada 2005 (AI-327): Dynamic ceiling priorities

      when Attribute_Priority =>
         if Ada_Version < Ada_2005 then
            Error_Attr ("% attribute is allowed only in Ada 2005 mode", P);
         end if;

         Check_E0;

         Check_Restriction (No_Dynamic_Priorities, N);

         --  The prefix must be a protected object (AARM D.5.2 (2/2))

         Analyze (P);

         if Is_Protected_Type (Etype (P))
           or else (Is_Access_Type (Etype (P))
                      and then Is_Protected_Type (Designated_Type (Etype (P))))
         then
            Resolve (P, Etype (P));
         else
            Error_Attr_P ("prefix of % attribute must be a protected object");
         end if;

         Set_Etype (N, Standard_Integer);

         --  Must be called from within a protected procedure or entry of the
         --  protected object.

         declare
            S : Entity_Id;

         begin
            S := Current_Scope;
            while S /= Etype (P)
               and then S /= Standard_Standard
            loop
               S := Scope (S);
            end loop;

            if S = Standard_Standard then
               Error_Attr ("the attribute % is only allowed inside protected "
                           & "operations", P);
            end if;
         end;

         Validate_Non_Static_Attribute_Function_Call;

      -----------
      -- Range --
      -----------

      when Attribute_Range =>
         Check_Array_Or_Scalar_Type;
         Bad_Attribute_For_Predicate;

         if Ada_Version = Ada_83
           and then Is_Scalar_Type (P_Type)
           and then Comes_From_Source (N)
         then
            Error_Attr
              ("(Ada 83) % attribute not allowed for scalar type", P);
         end if;

      ------------
      -- Result --
      ------------

      when Attribute_Result => Result : declare
         function Denote_Same_Function
           (Pref_Id : Entity_Id;
            Spec_Id : Entity_Id) return Boolean;
         --  Determine whether the entity of the prefix Pref_Id denotes the
         --  same entity as that of the related subprogram Spec_Id.

         --------------------------
         -- Denote_Same_Function --
         --------------------------

         function Denote_Same_Function
           (Pref_Id : Entity_Id;
            Spec_Id : Entity_Id) return Boolean
         is
            Over_Id   : constant Entity_Id := Overridden_Operation (Spec_Id);
            Subp_Spec : constant Node_Id   := Parent (Spec_Id);

         begin
            --  The prefix denotes the related subprogram

            if Pref_Id = Spec_Id then
               return True;

            --  Account for a special case when attribute 'Result appears in
            --  the postcondition of a generic function.

            --    generic
            --    function Gen_Func return ...
            --      with Post => Gen_Func'Result ...;

            --  When the generic function is instantiated, the Chars field of
            --  the instantiated prefix still denotes the name of the generic
            --  function. Note that any preemptive transformation is impossible
            --  without a proper analysis. The structure of the wrapper package
            --  is as follows:

            --    package Anon_Gen_Pack is
            --       <subtypes and renamings>
            --       function Subp_Decl return ...;               --  (!)
            --       pragma Postcondition (Gen_Func'Result ...);  --  (!)
            --       function Gen_Func ... renames Subp_Decl;
            --    end Anon_Gen_Pack;

            elsif Nkind (Subp_Spec) = N_Function_Specification
              and then Present (Generic_Parent (Subp_Spec))
              and then Ekind_In (Pref_Id, E_Generic_Function, E_Function)
            then
               if Generic_Parent (Subp_Spec) = Pref_Id then
                  return True;

               elsif Present (Alias (Pref_Id))
                 and then Alias (Pref_Id) = Spec_Id
               then
                  return True;
               end if;

            --  Account for a special case where a primitive of a tagged type
            --  inherits a class-wide postcondition from a parent type. In this
            --  case the prefix of attribute 'Result denotes the overriding
            --  primitive.

            elsif Present (Over_Id) and then Pref_Id = Over_Id then
               return True;
            end if;

            --  Otherwise the prefix does not denote the related subprogram

            return False;
         end Denote_Same_Function;

         --  Local variables

         In_Inlined_C_Postcondition : constant Boolean :=
                                        Modify_Tree_For_C
                                          and then In_Inlined_Body;

         Legal   : Boolean;
         Pref_Id : Entity_Id;
         Spec_Id : Entity_Id;

      --  Start of processing for Result

      begin
         --  The attribute reference is a primary. If any expressions follow,
         --  then the attribute reference is an indexable object. Transform the
         --  attribute into an indexed component and analyze it.

         if Present (E1) then
            Rewrite (N,
              Make_Indexed_Component (Loc,
                Prefix      =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Relocate_Node (P),
                    Attribute_Name => Name_Result),
                Expressions => Expressions (N)));
            Analyze (N);
            return;
         end if;

         Analyze_Attribute_Old_Result (Legal, Spec_Id);

         --  The aspect or pragma where attribute 'Result resides should be
         --  associated with a subprogram declaration or a body. If this is not
         --  the case, then the aspect or pragma is illegal. Return as analysis
         --  cannot be carried out.

         --  The exception to this rule is when generating C since in this case
         --  postconditions are inlined.

         if No (Spec_Id) and then In_Inlined_C_Postcondition then
            Spec_Id := Entity (P);

         elsif not Legal then
            return;
         end if;

         --  Attribute 'Result is part of a _Postconditions procedure. There is
         --  no need to perform the semantic checks below as they were already
         --  verified when the attribute was analyzed in its original context.
         --  Instead, rewrite the attribute as a reference to formal parameter
         --  _Result of the _Postconditions procedure.

         if Chars (Spec_Id) = Name_uPostconditions
           or else
             (In_Inlined_C_Postcondition
               and then Nkind (Parent (Spec_Id)) = N_Block_Statement)
         then
            Rewrite (N, Make_Identifier (Loc, Name_uResult));

            --  The type of formal parameter _Result is that of the function
            --  encapsulating the _Postconditions procedure. Resolution must
            --  be carried out against the function return type.

            Analyze_And_Resolve (N, Etype (Scope (Spec_Id)));

         --  Otherwise attribute 'Result appears in its original context and
         --  all semantic checks should be carried out.

         else
            --  Verify the legality of the prefix. It must denotes the entity
            --  of the related [generic] function.

            if Is_Entity_Name (P) then
               Pref_Id := Entity (P);

               if Ekind_In (Pref_Id, E_Function, E_Generic_Function)
                 and then Ekind (Spec_Id) = Ekind (Pref_Id)
               then
                  if Denote_Same_Function (Pref_Id, Spec_Id) then

                     --  Correct the prefix of the attribute when the context
                     --  is a generic function.

                     if Pref_Id /= Spec_Id then
                        Rewrite (P, New_Occurrence_Of (Spec_Id, Loc));
                        Analyze (P);
                     end if;

                     Set_Etype (N, Etype (Spec_Id));

                  --  Otherwise the prefix denotes some unrelated function

                  else
                     Error_Msg_Name_2 := Chars (Spec_Id);
                     Error_Attr
                       ("incorrect prefix for attribute %, expected %", P);
                  end if;

               --  Otherwise the prefix denotes some other form of subprogram
               --  entity.

               else
                  Error_Attr
                    ("attribute % can only appear in postcondition of "
                     & "function", P);
               end if;

            --  Otherwise the prefix is illegal

            else
               Error_Msg_Name_2 := Chars (Spec_Id);
               Error_Attr ("incorrect prefix for attribute %, expected %", P);
            end if;
         end if;
      end Result;

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length =>
         Check_E0;
         Check_Discrete_Type;
         Set_Etype (N, Universal_Integer);

      ----------
      -- Read --
      ----------

      when Attribute_Read =>
         Check_E2;
         Check_Stream_Attribute (TSS_Stream_Read);
         Set_Etype (N, Standard_Void_Type);
         Resolve (N, Standard_Void_Type);
         Note_Possible_Modification (E2, Sure => True);

      ---------
      -- Ref --
      ---------

      when Attribute_Ref =>
         Check_E1;
         Analyze (P);

         if Nkind (P) /= N_Expanded_Name
           or else not Is_RTE (P_Type, RE_Address)
         then
            Error_Attr_P ("prefix of % attribute must be System.Address");
         end if;

         Analyze_And_Resolve (E1, Any_Integer);
         Set_Etype (N, RTE (RE_Address));

      ---------------
      -- Remainder --
      ---------------

      when Attribute_Remainder =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);
         Resolve (E2, P_Base_Type);

      ---------------------
      -- Restriction_Set --
      ---------------------

      when Attribute_Restriction_Set => Restriction_Set : declare
         R    : Restriction_Id;
         U    : Node_Id;
         Unam : Unit_Name_Type;

      begin
         Check_E1;
         Analyze (P);
         Check_System_Prefix;

         --  No_Dependence case

         if Nkind (E1) = N_Parameter_Association then
            pragma Assert (Chars (Selector_Name (E1)) = Name_No_Dependence);
            U := Explicit_Actual_Parameter (E1);

            if not OK_No_Dependence_Unit_Name (U) then
               Set_Boolean_Result (N, False);
               Error_Attr;
            end if;

            --  See if there is an entry already in the table. That's the
            --  case in which we can return True.

            for J in No_Dependences.First .. No_Dependences.Last loop
               if Designate_Same_Unit (U, No_Dependences.Table (J).Unit)
                 and then No_Dependences.Table (J).Warn = False
               then
                  Set_Boolean_Result (N, True);
                  return;
               end if;
            end loop;

            --  If not in the No_Dependence table, result is False

            Set_Boolean_Result (N, False);

            --  In this case, we must ensure that the binder will reject any
            --  other unit in the partition that sets No_Dependence for this
            --  unit. We do that by making an entry in the special table kept
            --  for this purpose (if the entry is not there already).

            Unam := Get_Spec_Name (Get_Unit_Name (U));

            for J in Restriction_Set_Dependences.First ..
                     Restriction_Set_Dependences.Last
            loop
               if Restriction_Set_Dependences.Table (J) = Unam then
                  return;
               end if;
            end loop;

            Restriction_Set_Dependences.Append (Unam);

         --  Normal restriction case

         else
            if Nkind (E1) /= N_Identifier then
               Set_Boolean_Result (N, False);
               Error_Attr ("attribute % requires restriction identifier", E1);

            else
               R := Get_Restriction_Id (Process_Restriction_Synonyms (E1));

               if R = Not_A_Restriction_Id then
                  Set_Boolean_Result (N, False);
                  Error_Msg_Node_1 := E1;
                  Error_Attr ("invalid restriction identifier &", E1);

               elsif R not in Partition_Boolean_Restrictions then
                  Set_Boolean_Result (N, False);
                  Error_Msg_Node_1 := E1;
                  Error_Attr
                    ("& is not a boolean partition-wide restriction", E1);
               end if;

               if Restriction_Active (R) then
                  Set_Boolean_Result (N, True);
               else
                  Check_Restriction (R, N);
                  Set_Boolean_Result (N, False);
               end if;
            end if;
         end if;
      end Restriction_Set;

      -----------
      -- Round --
      -----------

      when Attribute_Round =>
         Check_E1;
         Check_Decimal_Fixed_Point_Type;
         Set_Etype (N, P_Base_Type);

         --  Because the context is universal_real (3.5.10(12)) it is a
         --  legal context for a universal fixed expression. This is the
         --  only attribute whose functional description involves U_R.

         if Etype (E1) = Universal_Fixed then
            declare
               Conv : constant Node_Id := Make_Type_Conversion (Loc,
                  Subtype_Mark => New_Occurrence_Of (Universal_Real, Loc),
                  Expression   => Relocate_Node (E1));

            begin
               Rewrite (E1, Conv);
               Analyze (E1);
            end;
         end if;

         Resolve (E1, Any_Real);

      --------------
      -- Rounding --
      --------------

      when Attribute_Rounding =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ---------------
      -- Safe_Emax --
      ---------------

      when Attribute_Safe_Emax =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Integer);

      ----------------
      -- Safe_First --
      ----------------

      when Attribute_Safe_First =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Real);

      ----------------
      -- Safe_Large --
      ----------------

      when Attribute_Safe_Large =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Real);

      ---------------
      -- Safe_Last --
      ---------------

      when Attribute_Safe_Last =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Universal_Real);

      ----------------
      -- Safe_Small --
      ----------------

      when Attribute_Safe_Small =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Real);

      --------------------------
      -- Scalar_Storage_Order --
      --------------------------

      when Attribute_Scalar_Storage_Order => Scalar_Storage_Order : declare
         Ent : Entity_Id := Empty;

      begin
         Check_E0;
         Check_Type;

         if not (Is_Record_Type (P_Type) or else Is_Array_Type (P_Type)) then

            --  In GNAT mode, the attribute applies to generic types as well
            --  as composite types, and for non-composite types always returns
            --  the default bit order for the target.

            if not (GNAT_Mode and then Is_Generic_Type (P_Type))
              and then not In_Instance
            then
               Error_Attr_P
                 ("prefix of % attribute must be record or array type");

            elsif not Is_Generic_Type (P_Type) then
               if Bytes_Big_Endian then
                  Ent := RTE (RE_High_Order_First);
               else
                  Ent := RTE (RE_Low_Order_First);
               end if;
            end if;

         elsif Bytes_Big_Endian xor Reverse_Storage_Order (P_Type) then
            Ent := RTE (RE_High_Order_First);

         else
            Ent := RTE (RE_Low_Order_First);
         end if;

         if Present (Ent) then
            Rewrite (N, New_Occurrence_Of (Ent, Loc));
         end if;

         Set_Etype (N, RTE (RE_Bit_Order));
         Resolve (N);

         --  Reset incorrect indication of staticness

         Set_Is_Static_Expression (N, False);
      end Scalar_Storage_Order;

      -----------
      -- Scale --
      -----------

      when Attribute_Scale =>
         Check_E0;
         Check_Decimal_Fixed_Point_Type;
         Set_Etype (N, Universal_Integer);

      -------------
      -- Scaling --
      -------------

      when Attribute_Scaling =>
         Check_Floating_Point_Type_2;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ------------------
      -- Signed_Zeros --
      ------------------

      when Attribute_Signed_Zeros =>
         Check_Floating_Point_Type_0;
         Set_Etype (N, Standard_Boolean);

      ----------
      -- Size --
      ----------

      when Attribute_Size
         | Attribute_VADS_Size
      =>
         Check_E0;

         --  If prefix is parameterless function call, rewrite and resolve
         --  as such.

         if Is_Entity_Name (P)
           and then Ekind (Entity (P)) = E_Function
         then
            Resolve (P);

         --  Similar processing for a protected function call

         elsif Nkind (P) = N_Selected_Component
           and then Ekind (Entity (Selector_Name (P))) = E_Function
         then
            Resolve (P);
         end if;

         if Is_Object_Reference (P) then
            Check_Object_Reference (P);

         elsif Is_Entity_Name (P)
           and then (Is_Type (Entity (P))
                       or else Ekind (Entity (P)) = E_Enumeration_Literal)
         then
            null;

         elsif Nkind (P) = N_Type_Conversion
           and then not Comes_From_Source (P)
         then
            null;

         --  Some other compilers allow dubious use of X'???'Size

         elsif Relaxed_RM_Semantics
           and then Nkind (P) = N_Attribute_Reference
         then
            null;

         else
            Error_Attr_P ("invalid prefix for % attribute");
         end if;

         Check_Not_Incomplete_Type;
         Check_Not_CPP_Type;
         Set_Etype (N, Universal_Integer);

         --  If we are processing pragmas Compile_Time_Warning and Compile_
         --  Time_Errors after the back end has been called and this occurrence
         --  of 'Size is known at compile time then it is safe to perform this
         --  evaluation. Needed to perform the static evaluation of the full
         --  boolean expression of these pragmas.

         if In_Compile_Time_Warning_Or_Error
           and then Is_Entity_Name (P)
           and then (Is_Type (Entity (P))
                      or else Ekind (Entity (P)) = E_Enumeration_Literal)
           and then Size_Known_At_Compile_Time (Entity (P))
         then
            Rewrite (N, Make_Integer_Literal (Sloc (N), Esize (Entity (P))));
            Analyze (N);
         end if;

      -----------
      -- Small --
      -----------

      when Attribute_Small =>
         Check_E0;
         Check_Real_Type;
         Set_Etype (N, Universal_Real);

      ------------------
      -- Storage_Pool --
      ------------------

      when Attribute_Storage_Pool
         | Attribute_Simple_Storage_Pool
      =>
         Check_E0;

         if Is_Access_Type (P_Type) then
            if Ekind (P_Type) = E_Access_Subprogram_Type then
               Error_Attr_P
                 ("cannot use % attribute for access-to-subprogram type");
            end if;

            --  Set appropriate entity

            if Present (Associated_Storage_Pool (Root_Type (P_Type))) then
               Set_Entity (N, Associated_Storage_Pool (Root_Type (P_Type)));
            else
               Set_Entity (N, RTE (RE_Global_Pool_Object));
            end if;

            if Attr_Id = Attribute_Storage_Pool then
               if Present (Get_Rep_Pragma (Etype (Entity (N)),
                                           Name_Simple_Storage_Pool_Type))
               then
                  Error_Msg_Name_1 := Aname;
                     Error_Msg_Warn := SPARK_Mode /= On;
                  Error_Msg_N
                    ("cannot use % attribute for type with simple storage "
                     & "pool<<", N);
                  Error_Msg_N ("\Program_Error [<<", N);

                  Rewrite
                    (N, Make_Raise_Program_Error
                          (Sloc (N), Reason => PE_Explicit_Raise));
               end if;

               Set_Etype (N, Class_Wide_Type (RTE (RE_Root_Storage_Pool)));

            --  In the Simple_Storage_Pool case, verify that the pool entity is
            --  actually of a simple storage pool type, and set the attribute's
            --  type to the pool object's type.

            else
               if not Present (Get_Rep_Pragma (Etype (Entity (N)),
                                               Name_Simple_Storage_Pool_Type))
               then
                  Error_Attr_P
                    ("cannot use % attribute for type without simple " &
                     "storage pool");
               end if;

               Set_Etype (N, Etype (Entity (N)));
            end if;

            --  Validate_Remote_Access_To_Class_Wide_Type for attribute
            --  Storage_Pool since this attribute is not defined for such
            --  types (RM E.2.3(22)).

            Validate_Remote_Access_To_Class_Wide_Type (N);

         else
            Error_Attr_P ("prefix of % attribute must be access type");
         end if;

      ------------------
      -- Storage_Size --
      ------------------

      when Attribute_Storage_Size =>
         Check_E0;

         if Is_Task_Type (P_Type) then
            Set_Etype (N, Universal_Integer);

            --  Use with tasks is an obsolescent feature

            Check_Restriction (No_Obsolescent_Features, P);

         elsif Is_Access_Type (P_Type) then
            if Ekind (P_Type) = E_Access_Subprogram_Type then
               Error_Attr_P
                 ("cannot use % attribute for access-to-subprogram type");
            end if;

            if Is_Entity_Name (P)
              and then Is_Type (Entity (P))
            then
               Check_Type;
               Set_Etype (N, Universal_Integer);

               --   Validate_Remote_Access_To_Class_Wide_Type for attribute
               --   Storage_Size since this attribute is not defined for
               --   such types (RM E.2.3(22)).

               Validate_Remote_Access_To_Class_Wide_Type (N);

            --  The prefix is allowed to be an implicit dereference of an
            --  access value designating a task.

            else
               Check_Task_Prefix;
               Set_Etype (N, Universal_Integer);
            end if;

         else
            Error_Attr_P ("prefix of % attribute must be access or task type");
         end if;

      ------------------
      -- Storage_Unit --
      ------------------

      when Attribute_Storage_Unit =>
         Standard_Attribute (Ttypes.System_Storage_Unit);

      -----------------
      -- Stream_Size --
      -----------------

      when Attribute_Stream_Size =>
         Check_E0;
         Check_Type;

         if Is_Entity_Name (P)
           and then Is_Elementary_Type (Entity (P))
         then
            Set_Etype (N, Universal_Integer);
         else
            Error_Attr_P ("invalid prefix for % attribute");
         end if;

      ---------------
      -- Stub_Type --
      ---------------

      when Attribute_Stub_Type =>
         Check_Type;
         Check_E0;

         if Is_Remote_Access_To_Class_Wide_Type (Base_Type (P_Type)) then

            --  For a real RACW [sub]type, use corresponding stub type

            if not Is_Generic_Type (P_Type) then
               Rewrite (N,
                 New_Occurrence_Of
                   (Corresponding_Stub_Type (Base_Type (P_Type)), Loc));

            --  For a generic type (that has been marked as an RACW using the
            --  Remote_Access_Type aspect or pragma), use a generic RACW stub
            --  type. Note that if the actual is not a remote access type, the
            --  instantiation will fail.

            else
               --  Note: we go to the underlying type here because the view
               --  returned by RTE (RE_RACW_Stub_Type) might be incomplete.

               Rewrite (N,
                 New_Occurrence_Of
                   (Underlying_Type (RTE (RE_RACW_Stub_Type)), Loc));
            end if;

         else
            Error_Attr_P
              ("prefix of% attribute must be remote access-to-class-wide");
         end if;

      ----------
      -- Succ --
      ----------

      when Attribute_Succ =>
         Check_Scalar_Type;
         Check_E1;

         if Is_Real_Type (P_Type) or else Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

         --  Since Pred works on the base type, we normally do no check for the
         --  floating-point case, since the base type is unconstrained. But we
         --  make an exception in Check_Float_Overflow mode.

         if Is_Floating_Point_Type (P_Type) then
            if not Range_Checks_Suppressed (P_Base_Type) then
               Set_Do_Range_Check (E1);
            end if;

         --  If not modular type, test for overflow check required

         else
            if not Is_Modular_Integer_Type (P_Type)
              and then not Range_Checks_Suppressed (P_Base_Type)
            then
               Enable_Range_Check (E1);
            end if;
         end if;

      --------------------------------
      -- System_Allocator_Alignment --
      --------------------------------

      when Attribute_System_Allocator_Alignment =>
         Standard_Attribute (Ttypes.System_Allocator_Alignment);

      ---------
      -- Tag --
      ---------

      when Attribute_Tag =>
         Check_E0;
         Check_Dereference;

         if not Is_Tagged_Type (P_Type) then
            Error_Attr_P ("prefix of % attribute must be tagged");

         --  Next test does not apply to generated code why not, and what does
         --  the illegal reference mean???

         elsif Is_Object_Reference (P)
           and then not Is_Class_Wide_Type (P_Type)
           and then Comes_From_Source (N)
         then
            Error_Attr_P
              ("% attribute can only be applied to objects " &
               "of class - wide type");
         end if;

         --  The prefix cannot be an incomplete type. However, references to
         --  'Tag can be generated when expanding interface conversions, and
         --  this is legal.

         if Comes_From_Source (N) then
            Check_Not_Incomplete_Type;
         end if;

         --  Set appropriate type

         Set_Etype (N, RTE (RE_Tag));

      -----------------
      -- Target_Name --
      -----------------

      when Attribute_Target_Name => Target_Name : declare
         TN : constant String := Sdefault.Target_Name.all;
         TL : Natural;

      begin
         Check_Standard_Prefix;

         TL := TN'Last;

         if TN (TL) = '/' or else TN (TL) = '\' then
            TL := TL - 1;
         end if;

         Rewrite (N,
           Make_String_Literal (Loc,
             Strval => TN (TN'First .. TL)));
         Analyze_And_Resolve (N, Standard_String);
         Set_Is_Static_Expression (N, True);
      end Target_Name;

      ----------------
      -- Terminated --
      ----------------

      when Attribute_Terminated =>
         Check_E0;
         Set_Etype (N, Standard_Boolean);
         Check_Task_Prefix;

      ----------------
      -- To_Address --
      ----------------

      when Attribute_To_Address => To_Address : declare
         Val : Uint;

      begin
         Check_E1;
         Analyze (P);
         Check_System_Prefix;

         Generate_Reference (RTE (RE_Address), P);
         Analyze_And_Resolve (E1, Any_Integer);
         Set_Etype (N, RTE (RE_Address));

         if Is_Static_Expression (E1) then
            Set_Is_Static_Expression (N, True);
         end if;

         --  OK static expression case, check range and set appropriate type

         if Is_OK_Static_Expression (E1) then
            Val := Expr_Value (E1);

            if Val < -(2 ** UI_From_Int (Standard'Address_Size - 1))
                 or else
               Val > 2 ** UI_From_Int (Standard'Address_Size) - 1
            then
               Error_Attr ("address value out of range for % attribute", E1);
            end if;

            --  In most cases the expression is a numeric literal or some other
            --  address expression, but if it is a declared constant it may be
            --  of a compatible type that must be left on the node.

            if Is_Entity_Name (E1) then
               null;

            --  Set type to universal integer if negative

            elsif Val < 0 then
               Set_Etype (E1, Universal_Integer);

            --  Otherwise set type to Unsigned_64 to accommodate max values

            else
               Set_Etype (E1, Standard_Unsigned_64);
            end if;
         end if;

         Set_Is_Static_Expression (N, True);
      end To_Address;

      ------------
      -- To_Any --
      ------------

      when Attribute_To_Any =>
         Check_E1;
         Check_PolyORB_Attribute;
         Set_Etype (N, RTE (RE_Any));

      ----------------
      -- Truncation --
      ----------------

      when Attribute_Truncation =>
         Check_Floating_Point_Type_1;
         Resolve (E1, P_Base_Type);
         Set_Etype (N, P_Base_Type);

      ----------------
      -- Type_Class --
      ----------------

      when Attribute_Type_Class =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, RTE (RE_Type_Class));

      --------------
      -- TypeCode --
      --------------

      when Attribute_TypeCode =>
         Check_E0;
         Check_PolyORB_Attribute;
         Set_Etype (N, RTE (RE_TypeCode));

      --------------
      -- Type_Key --
      --------------

      when Attribute_Type_Key => Type_Key : declare
         Full_Name  : constant String_Id :=
                        Fully_Qualified_Name_String (Entity (P));

         CRC : CRC32;
         --  The computed signature for the type

         Deref : Boolean;
         --  To simplify the handling of mutually recursive types, follow a
         --  single dereference link in a composite type.

         procedure Compute_Type_Key (T : Entity_Id);
         --  Create a CRC integer from the declaration of the type. For a
         --  composite type, fold in the representation of its components in
         --  recursive fashion. We use directly the source representation of
         --  the types involved.

         ----------------------
         -- Compute_Type_Key --
         ----------------------

         procedure Compute_Type_Key (T : Entity_Id) is
            Buffer : Source_Buffer_Ptr;
            P_Max  : Source_Ptr;
            P_Min  : Source_Ptr;
            Rep    : Node_Id;
            SFI    : Source_File_Index;

            procedure Process_One_Declaration;
            --  Update CRC with the characters of one type declaration, or a
            --  representation pragma that applies to the type.

            -----------------------------
            -- Process_One_Declaration --
            -----------------------------

            procedure Process_One_Declaration is
            begin
               --  Scan type declaration, skipping blanks

               for Ptr in P_Min .. P_Max loop
                  if Buffer (Ptr) /= ' ' then
                     System.CRC32.Update (CRC, Buffer (Ptr));
                  end if;
               end loop;
            end Process_One_Declaration;

         --  Start of processing for Compute_Type_Key

         begin
            if Is_Itype (T) then
               return;
            end if;

            --  If the type is declared in Standard, there is no source, so
            --  just use its name.

            if Scope (T) = Standard_Standard then
               declare
                  Name : constant String := Get_Name_String (Chars (T));
               begin
                  for J in Name'Range loop
                     System.CRC32.Update (CRC, Name (J));
                  end loop;
               end;

               return;
            end if;

            Sloc_Range (Enclosing_Declaration (T), P_Min, P_Max);
            SFI := Get_Source_File_Index (P_Min);
            pragma Assert (SFI = Get_Source_File_Index (P_Max));
            Buffer := Source_Text (SFI);

            Process_One_Declaration;

            --  Recurse on relevant component types

            if Is_Array_Type (T) then
               Compute_Type_Key (Component_Type (T));

            elsif Is_Access_Type (T) then
               if not Deref then
                  Deref := True;
                  Compute_Type_Key (Designated_Type (T));
               end if;

            elsif Is_Derived_Type (T) then
               Compute_Type_Key (Etype (T));

            elsif Is_Record_Type (T) then
               declare
                  Comp : Entity_Id;
               begin
                  Comp := First_Component (T);
                  while Present (Comp) loop
                     Compute_Type_Key (Etype (Comp));
                     Next_Component (Comp);
                  end loop;
               end;
            end if;

            if Is_First_Subtype (T) then

               --  Fold in representation aspects for the type, which appear in
               --  the same source buffer. If the representation aspects are in
               --  a different source file, then skip them; they apply to some
               --  other type, perhaps one we're derived from.

               Rep := First_Rep_Item (T);

               while Present (Rep) loop
                  if Comes_From_Source (Rep) then
                     Sloc_Range (Rep, P_Min, P_Max);

                     if SFI = Get_Source_File_Index (P_Min) then
                        pragma Assert (SFI = Get_Source_File_Index (P_Max));
                        Process_One_Declaration;
                     end if;
                  end if;

                  Rep := Next_Rep_Item (Rep);
               end loop;
            end if;
         end Compute_Type_Key;

      --  Start of processing for Type_Key

      begin
         Check_E0;
         Check_Type;

         Start_String;
         Deref := False;

         --  Copy all characters in Full_Name but the trailing NUL

         for J in 1 .. String_Length (Full_Name) - 1 loop
            Store_String_Char (Get_String_Char (Full_Name, Pos (J)));
         end loop;

         --  Compute CRC and convert it to string one character at a time, so
         --  as not to use Image within the compiler.

         Initialize (CRC);
         Compute_Type_Key (Entity (P));

         if not Is_Frozen (Entity (P)) then
            Error_Msg_N ("premature usage of Type_Key?", N);
         end if;

         while CRC > 0 loop
            Store_String_Char (Character'Val (48 + (CRC rem 10)));
            CRC := CRC / 10;
         end loop;

         Rewrite (N, Make_String_Literal (Loc, End_String));
         Analyze_And_Resolve (N, Standard_String);
      end Type_Key;

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      when Attribute_Unbiased_Rounding =>
         Check_Floating_Point_Type_1;
         Set_Etype (N, P_Base_Type);
         Resolve (E1, P_Base_Type);

      ----------------------
      -- Unchecked_Access --
      ----------------------

      when Attribute_Unchecked_Access =>
         if Comes_From_Source (N) then
            Check_Restriction (No_Unchecked_Access, N);
         end if;

         Analyze_Access_Attribute;
         Check_Not_Incomplete_Type;

      -------------------------
      -- Unconstrained_Array --
      -------------------------

      when Attribute_Unconstrained_Array =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Standard_Boolean);
         Set_Is_Static_Expression (N, True);

      ------------------------------
      -- Universal_Literal_String --
      ------------------------------

      --  This is a GNAT specific attribute whose prefix must be a named
      --  number where the expression is either a single numeric literal,
      --  or a numeric literal immediately preceded by a minus sign. The
      --  result is equivalent to a string literal containing the text of
      --  the literal as it appeared in the source program with a possible
      --  leading minus sign.

      when Attribute_Universal_Literal_String =>
         Check_E0;

         if not Is_Entity_Name (P)
           or else Ekind (Entity (P)) not in Named_Kind
         then
            Error_Attr_P ("prefix for % attribute must be named number");

         else
            declare
               Expr     : Node_Id;
               Negative : Boolean;
               S        : Source_Ptr;
               Src      : Source_Buffer_Ptr;

            begin
               Expr := Original_Node (Expression (Parent (Entity (P))));

               if Nkind (Expr) = N_Op_Minus then
                  Negative := True;
                  Expr := Original_Node (Right_Opnd (Expr));
               else
                  Negative := False;
               end if;

               if not Nkind_In (Expr, N_Integer_Literal, N_Real_Literal) then
                  Error_Attr
                    ("named number for % attribute must be simple literal", N);
               end if;

               --  Build string literal corresponding to source literal text

               Start_String;

               if Negative then
                  Store_String_Char (Get_Char_Code ('-'));
               end if;

               S := Sloc (Expr);
               Src := Source_Text (Get_Source_File_Index (S));

               while Src (S) /= ';' and then Src (S) /= ' ' loop
                  Store_String_Char (Get_Char_Code (Src (S)));
                  S := S + 1;
               end loop;

               --  Now we rewrite the attribute with the string literal

               Rewrite (N,
                 Make_String_Literal (Loc, End_String));
               Analyze (N);
               Set_Is_Static_Expression (N, True);
            end;
         end if;

      -------------------------
      -- Unrestricted_Access --
      -------------------------

      --  This is a GNAT specific attribute which is like Access except that
      --  all scope checks and checks for aliased views are omitted. It is
      --  documented as being equivalent to the use of the Address attribute
      --  followed by an unchecked conversion to the target access type.

      when Attribute_Unrestricted_Access =>

         --  If from source, deal with relevant restrictions

         if Comes_From_Source (N) then
            Check_Restriction (No_Unchecked_Access, N);

            if Nkind (P) in N_Has_Entity
              and then Present (Entity (P))
              and then Is_Object (Entity (P))
            then
               Check_Restriction (No_Implicit_Aliasing, N);
            end if;
         end if;

         if Is_Entity_Name (P) then
            Set_Address_Taken (Entity (P));
         end if;

         --  It might seem reasonable to call Address_Checks here to apply the
         --  same set of semantic checks that we enforce for 'Address (after
         --  all we document Unrestricted_Access as being equivalent to the
         --  use of Address followed by an Unchecked_Conversion). However, if
         --  we do enable these checks, we get multiple failures in both the
         --  compiler run-time and in our regression test suite, so we leave
         --  out these checks for now. To be investigated further some time???

         --  Address_Checks;

         --  Now complete analysis using common access processing

         Analyze_Access_Attribute;

      ------------
      -- Update --
      ------------

      when Attribute_Update => Update : declare
         Common_Typ : Entity_Id;
         --  The common type of a multiple component update for a record

         Comps : Elist_Id := No_Elist;
         --  A list used in the resolution of a record update. It contains the
         --  entities of all record components processed so far.

         procedure Analyze_Array_Component_Update (Assoc : Node_Id);
         --  Analyze and resolve array_component_association Assoc against the
         --  index of array type P_Type.

         procedure Analyze_Record_Component_Update (Comp : Node_Id);
         --  Analyze and resolve record_component_association Comp against
         --  record type P_Type.

         ------------------------------------
         -- Analyze_Array_Component_Update --
         ------------------------------------

         procedure Analyze_Array_Component_Update (Assoc : Node_Id) is
            Expr      : Node_Id;
            High      : Node_Id;
            Index     : Node_Id;
            Index_Typ : Entity_Id;
            Low       : Node_Id;

         begin
            --  The current association contains a sequence of indexes denoting
            --  an element of a multidimensional array:

            --    (Index_1, ..., Index_N)

            --  Examine each individual index and resolve it against the proper
            --  index type of the array.

            if Nkind (First (Choices (Assoc))) = N_Aggregate then
               Expr := First (Choices (Assoc));
               while Present (Expr) loop

                  --  The use of others is illegal (SPARK RM 4.4.1(12))

                  if Nkind (Expr) = N_Others_Choice then
                     Error_Attr
                       ("others choice not allowed in attribute %", Expr);

                  --  Otherwise analyze and resolve all indexes

                  else
                     Index     := First (Expressions (Expr));
                     Index_Typ := First_Index (P_Type);
                     while Present (Index) and then Present (Index_Typ) loop
                        Analyze_And_Resolve (Index, Etype (Index_Typ));
                        Next (Index);
                        Next_Index (Index_Typ);
                     end loop;

                     --  Detect a case where the association either lacks an
                     --  index or contains an extra index.

                     if Present (Index) or else Present (Index_Typ) then
                        Error_Msg_N
                          ("dimension mismatch in index list", Assoc);
                     end if;
                  end if;

                  Next (Expr);
               end loop;

            --  The current association denotes either a single component or a
            --  range of components of a one dimensional array:

            --    1, 2 .. 5

            --  Resolve the index or its high and low bounds (if range) against
            --  the proper index type of the array.

            else
               Index     := First (Choices (Assoc));
               Index_Typ := First_Index (P_Type);

               if Present (Next_Index (Index_Typ)) then
                  Error_Msg_N ("too few subscripts in array reference", Assoc);
               end if;

               while Present (Index) loop

                  --  The use of others is illegal (SPARK RM 4.4.1(12))

                  if Nkind (Index) = N_Others_Choice then
                     Error_Attr
                       ("others choice not allowed in attribute %", Index);

                  --  The index denotes a range of elements

                  elsif Nkind (Index) = N_Range then
                     Low  := Low_Bound  (Index);
                     High := High_Bound (Index);

                     Analyze_And_Resolve (Low,  Etype (Index_Typ));
                     Analyze_And_Resolve (High, Etype (Index_Typ));

                     --  Add a range check to ensure that the bounds of the
                     --  range are within the index type when this cannot be
                     --  determined statically.

                     if not Is_OK_Static_Expression (Low) then
                        Set_Do_Range_Check (Low);
                     end if;

                     if not Is_OK_Static_Expression (High) then
                        Set_Do_Range_Check (High);
                     end if;

                  --  Otherwise the index denotes a single element

                  else
                     Analyze_And_Resolve (Index, Etype (Index_Typ));

                     --  Add a range check to ensure that the index is within
                     --  the index type when it is not possible to determine
                     --  this statically.

                     if not Is_OK_Static_Expression (Index) then
                        Set_Do_Range_Check (Index);
                     end if;
                  end if;

                  Next (Index);
               end loop;
            end if;
         end Analyze_Array_Component_Update;

         -------------------------------------
         -- Analyze_Record_Component_Update --
         -------------------------------------

         procedure Analyze_Record_Component_Update (Comp : Node_Id) is
            Comp_Name     : constant Name_Id := Chars (Comp);
            Base_Typ      : Entity_Id;
            Comp_Or_Discr : Entity_Id;

         begin
            --  Find the discriminant or component whose name corresponds to
            --  Comp. A simple character comparison is sufficient because all
            --  visible names within a record type are unique.

            Comp_Or_Discr := First_Entity (P_Type);
            while Present (Comp_Or_Discr) loop
               if Chars (Comp_Or_Discr) = Comp_Name then

                  --  Decorate the component reference by setting its entity
                  --  and type for resolution purposes.

                  Set_Entity (Comp, Comp_Or_Discr);
                  Set_Etype  (Comp, Etype (Comp_Or_Discr));
                  exit;
               end if;

               Comp_Or_Discr := Next_Entity (Comp_Or_Discr);
            end loop;

            --  Diagnose an illegal reference

            if Present (Comp_Or_Discr) then
               if Ekind (Comp_Or_Discr) = E_Discriminant then
                  Error_Attr
                    ("attribute % may not modify record discriminants", Comp);

               else pragma Assert (Ekind (Comp_Or_Discr) = E_Component);
                  if Contains (Comps, Comp_Or_Discr) then
                     Error_Msg_N ("component & already updated", Comp);

                  --  Mark this component as processed

                  else
                     Append_New_Elmt (Comp_Or_Discr, Comps);
                  end if;
               end if;

            --  The update aggregate mentions an entity that does not belong to
            --  the record type.

            else
               Error_Msg_N ("& is not a component of aggregate subtype", Comp);
            end if;

            --  Verify the consistency of types when the current component is
            --  part of a miltiple component update.

            --    Comp_1, ..., Comp_N => <value>

            if Present (Etype (Comp)) then
               Base_Typ := Base_Type (Etype (Comp));

               --  Save the type of the first component reference as the
               --  remaning references (if any) must resolve to this type.

               if No (Common_Typ) then
                  Common_Typ := Base_Typ;

               elsif Base_Typ /= Common_Typ then
                  Error_Msg_N
                    ("components in choice list must have same type", Comp);
               end if;
            end if;
         end Analyze_Record_Component_Update;

         --  Local variables

         Assoc : Node_Id;
         Comp  : Node_Id;

      --  Start of processing for Update

      begin
         Check_E1;

         if not Is_Object_Reference (P) then
            Error_Attr_P ("prefix of attribute % must denote an object");

         elsif not Is_Array_Type (P_Type)
           and then not Is_Record_Type (P_Type)
         then
            Error_Attr_P ("prefix of attribute % must be a record or array");

         elsif Is_Limited_View (P_Type) then
            Error_Attr ("prefix of attribute % cannot be limited", N);

         elsif Nkind (E1) /= N_Aggregate then
            Error_Attr ("attribute % requires component association list", N);
         end if;

         --  Inspect the update aggregate, looking at all the associations and
         --  choices. Perform the following checks:

         --    1) Legality of "others" in all cases
         --    2) Legality of <>
         --    3) Component legality for arrays
         --    4) Component legality for records

         --  The remaining checks are performed on the expanded attribute

         Assoc := First (Component_Associations (E1));
         while Present (Assoc) loop

            --  The use of <> is illegal (SPARK RM 4.4.1(1))

            if Box_Present (Assoc) then
               Error_Attr
                 ("default initialization not allowed in attribute %", Assoc);

            --  Otherwise process the association

            else
               Analyze (Expression (Assoc));

               if Is_Array_Type (P_Type) then
                  Analyze_Array_Component_Update (Assoc);

               elsif Is_Record_Type (P_Type) then

                  --  Reset the common type used in a multiple component update
                  --  as we are processing the contents of a new association.

                  Common_Typ := Empty;

                  Comp := First (Choices (Assoc));
                  while Present (Comp) loop
                     if Nkind (Comp) = N_Identifier then
                        Analyze_Record_Component_Update (Comp);

                     --  The use of others is illegal (SPARK RM 4.4.1(5))

                     elsif Nkind (Comp) = N_Others_Choice then
                        Error_Attr
                          ("others choice not allowed in attribute %", Comp);

                     --  The name of a record component cannot appear in any
                     --  other form.

                     else
                        Error_Msg_N
                          ("name should be identifier or OTHERS", Comp);
                     end if;

                     Next (Comp);
                  end loop;
               end if;
            end if;

            Next (Assoc);
         end loop;

         --  The type of attribute 'Update is that of the prefix

         Set_Etype (N, P_Type);

         Sem_Warn.Warn_On_Suspicious_Update (N);
      end Update;

      ---------
      -- Val --
      ---------

      when Attribute_Val =>
         Check_E1;
         Check_Discrete_Type;

         if Is_Boolean_Type (P_Type) then
            Error_Msg_Name_1 := Aname;
            Error_Msg_Name_2 := Chars (P_Type);
            Check_SPARK_05_Restriction
              ("attribute% is not allowed for type%", P);
         end if;

         --  Note, we need a range check in general, but we wait for the
         --  Resolve call to do this, since we want to let Eval_Attribute
         --  have a chance to find an static illegality first.

         Resolve (E1, Any_Integer);
         Set_Etype (N, P_Base_Type);

      -----------
      -- Valid --
      -----------

      when Attribute_Valid =>
         Check_E0;

         --  Ignore check for object if we have a 'Valid reference generated
         --  by the expanded code, since in some cases valid checks can occur
         --  on items that are names, but are not objects (e.g. attributes).

         if Comes_From_Source (N) then
            Check_Object_Reference (P);
         end if;

         if not Is_Scalar_Type (P_Type) then
            Error_Attr_P ("object for % attribute must be of scalar type");
         end if;

         --  If the attribute appears within the subtype's own predicate
         --  function, then issue a warning that this will cause infinite
         --  recursion.

         declare
            Pred_Func : constant Entity_Id := Predicate_Function (P_Type);

         begin
            if Present (Pred_Func) and then Current_Scope = Pred_Func then
               Error_Msg_N
                 ("attribute Valid requires a predicate check??", N);
               Error_Msg_N ("\and will result in infinite recursion??", N);
            end if;
         end;

         Set_Etype (N, Standard_Boolean);

      -------------------
      -- Valid_Scalars --
      -------------------

      when Attribute_Valid_Scalars =>
         Check_E0;
         Check_Object_Reference (P);
         Set_Etype (N, Standard_Boolean);

         --  Following checks are only for source types

         if Comes_From_Source (N) then
            if not Scalar_Part_Present (P_Type) then
               Error_Attr_P
                 ("??attribute % always True, no scalars to check");
            end if;

            --  Not allowed for unchecked union type

            if Has_Unchecked_Union (P_Type) then
               Error_Attr_P
                 ("attribute % not allowed for Unchecked_Union type");
            end if;
         end if;

      -----------
      -- Value --
      -----------

      when Attribute_Value =>
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E1;
         Check_Scalar_Type;

         --  Case of enumeration type

         --  When an enumeration type appears in an attribute reference, all
         --  literals of the type are marked as referenced. This must only be
         --  done if the attribute reference appears in the current source.
         --  Otherwise the information on references may differ between a
         --  normal compilation and one that performs inlining.

         if Is_Enumeration_Type (P_Type)
           and then In_Extended_Main_Code_Unit (N)
         then
            Check_Restriction (No_Enumeration_Maps, N);

            --  Mark all enumeration literals as referenced, since the use of
            --  the Value attribute can implicitly reference any of the
            --  literals of the enumeration base type.

            declare
               Ent : Entity_Id := First_Literal (P_Base_Type);
            begin
               while Present (Ent) loop
                  Set_Referenced (Ent);
                  Next_Literal (Ent);
               end loop;
            end;
         end if;

         --  Set Etype before resolving expression because expansion of
         --  expression may require enclosing type. Note that the type
         --  returned by 'Value is the base type of the prefix type.

         Set_Etype (N, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;

      ----------------
      -- Value_Size --
      ----------------

      when Attribute_Value_Size =>
         Check_E0;
         Check_Type;
         Check_Not_Incomplete_Type;
         Set_Etype (N, Universal_Integer);

      -------------
      -- Version --
      -------------

      when Attribute_Version =>
         Check_E0;
         Check_Program_Unit;
         Set_Etype (N, RTE (RE_Version_String));

      ------------------
      -- Wchar_T_Size --
      ------------------

      when Attribute_Wchar_T_Size =>
         Standard_Attribute (Interfaces_Wchar_T_Size);

      ----------------
      -- Wide_Image --
      ----------------

      when Attribute_Wide_Image =>
         Check_SPARK_05_Restriction_On_Attribute;

         if Is_Image_Applied_To_Object (P, P_Type) then
            Check_Object_Reference_Image (Standard_Wide_String);
            return;
         end if;

         Check_Scalar_Type;
         Set_Etype (N, Standard_Wide_String);
         Check_E1;
         Resolve (E1, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;

      ---------------------
      -- Wide_Wide_Image --
      ---------------------

      when Attribute_Wide_Wide_Image =>
         if Is_Image_Applied_To_Object (P, P_Type) then
            Check_Object_Reference_Image (Standard_Wide_Wide_String);
            return;
         end if;

         Check_Scalar_Type;
         Set_Etype (N, Standard_Wide_Wide_String);
         Check_E1;
         Resolve (E1, P_Base_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;

      ----------------
      -- Wide_Value --
      ----------------

      when Attribute_Wide_Value =>
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E1;
         Check_Scalar_Type;

         --  Set Etype before resolving expression because expansion
         --  of expression may require enclosing type.

         Set_Etype (N, P_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;

      ---------------------
      -- Wide_Wide_Value --
      ---------------------

      when Attribute_Wide_Wide_Value =>
         Check_E1;
         Check_Scalar_Type;

         --  Set Etype before resolving expression because expansion
         --  of expression may require enclosing type.

         Set_Etype (N, P_Type);
         Validate_Non_Static_Attribute_Function_Call;

         --  Check restriction No_Fixed_IO

         if Restriction_Check_Required (No_Fixed_IO)
           and then Is_Fixed_Point_Type (P_Type)
         then
            Check_Restriction (No_Fixed_IO, P);
         end if;

      ---------------------
      -- Wide_Wide_Width --
      ---------------------

      when Attribute_Wide_Wide_Width =>
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, Universal_Integer);

      ----------------
      -- Wide_Width --
      ----------------

      when Attribute_Wide_Width =>
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, Universal_Integer);

      -----------
      -- Width --
      -----------

      when Attribute_Width =>
         Check_SPARK_05_Restriction_On_Attribute;
         Check_E0;
         Check_Scalar_Type;
         Set_Etype (N, Universal_Integer);

      ---------------
      -- Word_Size --
      ---------------

      when Attribute_Word_Size =>
         Standard_Attribute (System_Word_Size);

      -----------
      -- Write --
      -----------

      when Attribute_Write =>
         Check_E2;
         Check_Stream_Attribute (TSS_Stream_Write);
         Set_Etype (N, Standard_Void_Type);
         Resolve (N, Standard_Void_Type);

      end case;

      --  In SPARK certain attributes (see below) depend on Tasking_State.
      --  Ensure that the entity is available for gnat2why by loading it.
      --  See SPARK RM 9(18) for the relevant rule.

      if GNATprove_Mode then
         declare
            Unused : Entity_Id;

         begin
            case Attr_Id is
               when Attribute_Callable
                  | Attribute_Caller
                  | Attribute_Count
                  | Attribute_Terminated
               =>
                  Unused := RTE (RE_Tasking_State);

               when others =>
                  null;
            end case;
         end;
      end if;

   --  All errors raise Bad_Attribute, so that we get out before any further
   --  damage occurs when an error is detected (for example, if we check for
   --  one attribute expression, and the check succeeds, we want to be able
   --  to proceed securely assuming that an expression is in fact present.

   --  Note: we set the attribute analyzed in this case to prevent any
   --  attempt at reanalysis which could generate spurious error msgs.

   exception
      when Bad_Attribute =>
         Set_Analyzed (N);
         Set_Etype (N, Any_Type);
         return;
   end Analyze_Attribute;

   --------------------
   -- Eval_Attribute --
   --------------------

   procedure Eval_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Aname : constant Name_Id      := Attribute_Name (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Aname);
      P     : constant Node_Id      := Prefix (N);

      C_Type : constant Entity_Id := Etype (N);
      --  The type imposed by the context

      E1 : Node_Id;
      --  First expression, or Empty if none

      E2 : Node_Id;
      --  Second expression, or Empty if none

      P_Entity : Entity_Id;
      --  Entity denoted by prefix

      P_Type : Entity_Id;
      --  The type of the prefix

      P_Base_Type : Entity_Id;
      --  The base type of the prefix type

      P_Root_Type : Entity_Id;
      --  The root type of the prefix type

      Static : Boolean;
      --  True if the result is Static. This is set by the general processing
      --  to true if the prefix is static, and all expressions are static. It
      --  can be reset as processing continues for particular attributes. This
      --  flag can still be True if the reference raises a constraint error.
      --  Is_Static_Expression (N) is set to follow this value as it is set
      --  and we could always reference this, but it is convenient to have a
      --  simple short name to use, since it is frequently referenced.

      Lo_Bound, Hi_Bound : Node_Id;
      --  Expressions for low and high bounds of type or array index referenced
      --  by First, Last, or Length attribute for array, set by Set_Bounds.

      CE_Node : Node_Id;
      --  Constraint error node used if we have an attribute reference has
      --  an argument that raises a constraint error. In this case we replace
      --  the attribute with a raise constraint_error node. This is important
      --  processing, since otherwise gigi might see an attribute which it is
      --  unprepared to deal with.

      procedure Check_Concurrent_Discriminant (Bound : Node_Id);
      --  If Bound is a reference to a discriminant of a task or protected type
      --  occurring within the object's body, rewrite attribute reference into
      --  a reference to the corresponding discriminal. Use for the expansion
      --  of checks against bounds of entry family index subtypes.

      procedure Check_Expressions;
      --  In case where the attribute is not foldable, the expressions, if
      --  any, of the attribute, are in a non-static context. This procedure
      --  performs the required additional checks.

      function Compile_Time_Known_Bounds (Typ : Entity_Id) return Boolean;
      --  Determines if the given type has compile time known bounds. Note
      --  that we enter the case statement even in cases where the prefix
      --  type does NOT have known bounds, so it is important to guard any
      --  attempt to evaluate both bounds with a call to this function.

      procedure Compile_Time_Known_Attribute (N : Node_Id; Val : Uint);
      --  This procedure is called when the attribute N has a non-static
      --  but compile time known value given by Val. It includes the
      --  necessary checks for out of range values.

      function Fore_Value return Nat;
      --  Computes the Fore value for the current attribute prefix, which is
      --  known to be a static fixed-point type. Used by Fore and Width.

      function Mantissa return Uint;
      --  Returns the Mantissa value for the prefix type

      procedure Set_Bounds;
      --  Used for First, Last and Length attributes applied to an array or
      --  array subtype. Sets the variables Lo_Bound and Hi_Bound to the low
      --  and high bound expressions for the index referenced by the attribute
      --  designator (i.e. the first index if no expression is present, and the
      --  N'th index if the value N is present as an expression). Also used for
      --  First and Last of scalar types and for First_Valid and Last_Valid.
      --  Static is reset to False if the type or index type is not statically
      --  constrained.

      function Statically_Denotes_Entity (N : Node_Id) return Boolean;
      --  Verify that the prefix of a potentially static array attribute
      --  satisfies the conditions of 4.9 (14).

      -----------------------------------
      -- Check_Concurrent_Discriminant --
      -----------------------------------

      procedure Check_Concurrent_Discriminant (Bound : Node_Id) is
         Tsk : Entity_Id;
         --  The concurrent (task or protected) type

      begin
         if Nkind (Bound) = N_Identifier
           and then Ekind (Entity (Bound)) = E_Discriminant
           and then Is_Concurrent_Record_Type (Scope (Entity (Bound)))
         then
            Tsk := Corresponding_Concurrent_Type (Scope (Entity (Bound)));

            if In_Open_Scopes (Tsk) and then Has_Completion (Tsk) then

               --  Find discriminant of original concurrent type, and use
               --  its current discriminal, which is the renaming within
               --  the task/protected body.

               Rewrite (N,
                 New_Occurrence_Of
                   (Find_Body_Discriminal (Entity (Bound)), Loc));
            end if;
         end if;
      end Check_Concurrent_Discriminant;

      -----------------------
      -- Check_Expressions --
      -----------------------

      procedure Check_Expressions is
         E : Node_Id;
      begin
         E := E1;
         while Present (E) loop
            Check_Non_Static_Context (E);
            Next (E);
         end loop;
      end Check_Expressions;

      ----------------------------------
      -- Compile_Time_Known_Attribute --
      ----------------------------------

      procedure Compile_Time_Known_Attribute (N : Node_Id; Val : Uint) is
         T : constant Entity_Id := Etype (N);

      begin
         Fold_Uint (N, Val, False);

         --  Check that result is in bounds of the type if it is static

         if Is_In_Range (N, T, Assume_Valid => False) then
            null;

         elsif Is_Out_Of_Range (N, T) then
            Apply_Compile_Time_Constraint_Error
              (N, "value not in range of}??", CE_Range_Check_Failed);

         elsif not Range_Checks_Suppressed (T) then
            Enable_Range_Check (N);

         else
            Set_Do_Range_Check (N, False);
         end if;
      end Compile_Time_Known_Attribute;

      -------------------------------
      -- Compile_Time_Known_Bounds --
      -------------------------------

      function Compile_Time_Known_Bounds (Typ : Entity_Id) return Boolean is
      begin
         return
           Compile_Time_Known_Value (Type_Low_Bound (Typ))
             and then
           Compile_Time_Known_Value (Type_High_Bound (Typ));
      end Compile_Time_Known_Bounds;

      ----------------
      -- Fore_Value --
      ----------------

      --  Note that the Fore calculation is based on the actual values
      --  of the bounds, and does not take into account possible rounding.

      function Fore_Value return Nat is
         Lo      : constant Uint  := Expr_Value (Type_Low_Bound (P_Type));
         Hi      : constant Uint  := Expr_Value (Type_High_Bound (P_Type));
         Small   : constant Ureal := Small_Value (P_Type);
         Lo_Real : constant Ureal := Lo * Small;
         Hi_Real : constant Ureal := Hi * Small;
         T       : Ureal;
         R       : Nat;

      begin
         --  Bounds are given in terms of small units, so first compute
         --  proper values as reals.

         T := UR_Max (abs Lo_Real, abs Hi_Real);
         R := 2;

         --  Loop to compute proper value if more than one digit required

         while T >= Ureal_10 loop
            R := R + 1;
            T := T / Ureal_10;
         end loop;

         return R;
      end Fore_Value;

      --------------
      -- Mantissa --
      --------------

      --  Table of mantissa values accessed by function  Computed using
      --  the relation:

      --    T'Mantissa = integer next above (D * log(10)/log(2)) + 1)

      --  where D is T'Digits (RM83 3.5.7)

      Mantissa_Value : constant array (Nat range 1 .. 40) of Nat := (
          1 =>   5,
          2 =>   8,
          3 =>  11,
          4 =>  15,
          5 =>  18,
          6 =>  21,
          7 =>  25,
          8 =>  28,
          9 =>  31,
         10 =>  35,
         11 =>  38,
         12 =>  41,
         13 =>  45,
         14 =>  48,
         15 =>  51,
         16 =>  55,
         17 =>  58,
         18 =>  61,
         19 =>  65,
         20 =>  68,
         21 =>  71,
         22 =>  75,
         23 =>  78,
         24 =>  81,
         25 =>  85,
         26 =>  88,
         27 =>  91,
         28 =>  95,
         29 =>  98,
         30 => 101,
         31 => 104,
         32 => 108,
         33 => 111,
         34 => 114,
         35 => 118,
         36 => 121,
         37 => 124,
         38 => 128,
         39 => 131,
         40 => 134);

      function Mantissa return Uint is
      begin
         return
           UI_From_Int (Mantissa_Value (UI_To_Int (Digits_Value (P_Type))));
      end Mantissa;

      ----------------
      -- Set_Bounds --
      ----------------

      procedure Set_Bounds is
         Ndim : Nat;
         Indx : Node_Id;
         Ityp : Entity_Id;

      begin
         --  For a string literal subtype, we have to construct the bounds.
         --  Valid Ada code never applies attributes to string literals, but
         --  it is convenient to allow the expander to generate attribute
         --  references of this type (e.g. First and Last applied to a string
         --  literal).

         --  Note that the whole point of the E_String_Literal_Subtype is to
         --  avoid this construction of bounds, but the cases in which we
         --  have to materialize them are rare enough that we don't worry.

         --  The low bound is simply the low bound of the base type. The
         --  high bound is computed from the length of the string and this
         --  low bound.

         if Ekind (P_Type) = E_String_Literal_Subtype then
            Ityp := Etype (First_Index (Base_Type (P_Type)));
            Lo_Bound := Type_Low_Bound (Ityp);

            Hi_Bound :=
              Make_Integer_Literal (Sloc (P),
                Intval =>
                  Expr_Value (Lo_Bound) + String_Literal_Length (P_Type) - 1);

            Set_Parent (Hi_Bound, P);
            Analyze_And_Resolve (Hi_Bound, Etype (Lo_Bound));
            return;

         --  For non-array case, just get bounds of scalar type

         elsif Is_Scalar_Type (P_Type) then
            Ityp := P_Type;

            --  For a fixed-point type, we must freeze to get the attributes
            --  of the fixed-point type set now so we can reference them.

            if Is_Fixed_Point_Type (P_Type)
              and then not Is_Frozen (Base_Type (P_Type))
              and then Compile_Time_Known_Value (Type_Low_Bound (P_Type))
              and then Compile_Time_Known_Value (Type_High_Bound (P_Type))
            then
               Freeze_Fixed_Point_Type (Base_Type (P_Type));
            end if;

         --  For array case, get type of proper index

         else
            if No (E1) then
               Ndim := 1;
            else
               Ndim := UI_To_Int (Expr_Value (E1));
            end if;

            Indx := First_Index (P_Type);
            for J in 1 .. Ndim - 1 loop
               Next_Index (Indx);
            end loop;

            --  If no index type, get out (some other error occurred, and
            --  we don't have enough information to complete the job).

            if No (Indx) then
               Lo_Bound := Error;
               Hi_Bound := Error;
               return;
            end if;

            Ityp := Etype (Indx);
         end if;

         --  A discrete range in an index constraint is allowed to be a
         --  subtype indication. This is syntactically a pain, but should
         --  not propagate to the entity for the corresponding index subtype.
         --  After checking that the subtype indication is legal, the range
         --  of the subtype indication should be transfered to the entity.
         --  The attributes for the bounds should remain the simple retrievals
         --  that they are now.

         Lo_Bound := Type_Low_Bound (Ityp);
         Hi_Bound := Type_High_Bound (Ityp);

         --  If subtype is non-static, result is definitely non-static

         if not Is_Static_Subtype (Ityp) then
            Static := False;
            Set_Is_Static_Expression (N, False);

         --  Subtype is static, does it raise CE?

         elsif not Is_OK_Static_Subtype (Ityp) then
            Set_Raises_Constraint_Error (N);
         end if;
      end Set_Bounds;

      -------------------------------
      -- Statically_Denotes_Entity --
      -------------------------------

      function Statically_Denotes_Entity (N : Node_Id) return Boolean is
         E : Entity_Id;

      begin
         if not Is_Entity_Name (N) then
            return False;
         else
            E := Entity (N);
         end if;

         return
           Nkind (Parent (E)) /= N_Object_Renaming_Declaration
             or else Statically_Denotes_Entity (Renamed_Object (E));
      end Statically_Denotes_Entity;

   --  Start of processing for Eval_Attribute

   begin
      --  Initialize result as non-static, will be reset if appropriate

      Set_Is_Static_Expression (N, False);
      Static := False;

      --  Acquire first two expressions (at the moment, no attributes take more
      --  than two expressions in any case).

      if Present (Expressions (N)) then
         E1 := First (Expressions (N));
         E2 := Next (E1);
      else
         E1 := Empty;
         E2 := Empty;
      end if;

      --  Special processing for Enabled attribute. This attribute has a very
      --  special prefix, and the easiest way to avoid lots of special checks
      --  to protect this special prefix from causing trouble is to deal with
      --  this attribute immediately and be done with it.

      if Id = Attribute_Enabled then

         --  We skip evaluation if the expander is not active. This is not just
         --  an optimization. It is of key importance that we not rewrite the
         --  attribute in a generic template, since we want to pick up the
         --  setting of the check in the instance, Testing Expander_Active
         --  might seem an easy way of doing this, but we need to account for
         --  ASIS needs, so check explicitly for a generic context.

         if not Inside_A_Generic then
            declare
               C : constant Check_Id := Get_Check_Id (Chars (P));
               R : Boolean;

            begin
               if No (E1) then
                  if C in Predefined_Check_Id then
                     R := Scope_Suppress.Suppress (C);
                  else
                     R := Is_Check_Suppressed (Empty, C);
                  end if;

               else
                  R := Is_Check_Suppressed (Entity (E1), C);
               end if;

               Rewrite (N, New_Occurrence_Of (Boolean_Literals (not R), Loc));
            end;
         end if;

         return;
      end if;

      --  Attribute 'Img applied to a static enumeration value is static, and
      --  we will do the folding right here (things get confused if we let this
      --  case go through the normal circuitry).

      if Attribute_Name (N) = Name_Img
        and then Is_Entity_Name (P)
        and then Is_Enumeration_Type (Etype (Entity (P)))
        and then Is_OK_Static_Expression (P)
      then
         declare
            Lit : constant Entity_Id := Expr_Value_E (P);
            Str : String_Id;

         begin
            Start_String;
            Get_Unqualified_Decoded_Name_String (Chars (Lit));
            Set_Casing (All_Upper_Case);
            Store_String_Chars (Name_Buffer (1 .. Name_Len));
            Str := End_String;

            Rewrite (N, Make_String_Literal (Loc, Strval => Str));
            Analyze_And_Resolve (N, Standard_String);
            Set_Is_Static_Expression (N, True);
         end;

         return;
      end if;

      --  Special processing for cases where the prefix is an object. For this
      --  purpose, a string literal counts as an object (attributes of string
      --  literals can only appear in generated code).

      if Is_Object_Reference (P) or else Nkind (P) = N_String_Literal then

         --  For Component_Size, the prefix is an array object, and we apply
         --  the attribute to the type of the object. This is allowed for both
         --  unconstrained and constrained arrays, since the bounds have no
         --  influence on the value of this attribute.

         if Id = Attribute_Component_Size then
            P_Entity := Etype (P);

         --  For Enum_Rep, evaluation depends on the nature of the prefix and
         --  the optional argument.

         elsif Id = Attribute_Enum_Rep then
            if Is_Entity_Name (P) then

               declare
                  Enum_Expr : Node_Id;
                  --  The enumeration-type expression of interest

               begin
                  --  P'Enum_Rep case

                  if Ekind_In (Entity (P), E_Constant,
                                           E_Enumeration_Literal)
                  then
                     Enum_Expr := P;

                  --  Enum_Type'Enum_Rep (E1) case

                  elsif Is_Enumeration_Type (Entity (P)) then
                     Enum_Expr := E1;

                  --  Otherwise the attribute must be expanded into a
                  --  conversion and evaluated at run time.

                  else
                     Check_Expressions;
                     return;
                  end if;

                  --  We can fold if the expression is an enumeration
                  --  literal, or if it denotes a constant whose value
                  --  is known at compile time.

                  if Nkind (Enum_Expr) in N_Has_Entity
                    and then (Ekind (Entity (Enum_Expr)) =
                                E_Enumeration_Literal
                      or else
                       (Ekind (Entity (Enum_Expr)) = E_Constant
                          and then Nkind (Parent (Entity (Enum_Expr))) =
                                     N_Object_Declaration
                          and then Compile_Time_Known_Value
                                     (Expression (Parent (Entity (P))))))
                  then
                     P_Entity := Etype (P);
                  else
                     Check_Expressions;
                     return;
                  end if;
               end;

            --  Otherwise the attribute is illegal, do not attempt to perform
            --  any kind of folding.

            else
               return;
            end if;

         --  For First and Last, the prefix is an array object, and we apply
         --  the attribute to the type of the array, but we need a constrained
         --  type for this, so we use the actual subtype if available.

         elsif Id = Attribute_First or else
               Id = Attribute_Last  or else
               Id = Attribute_Length
         then
            declare
               AS : constant Entity_Id := Get_Actual_Subtype_If_Available (P);

            begin
               if Present (AS) and then Is_Constrained (AS) then
                  P_Entity := AS;

               --  If we have an unconstrained type we cannot fold

               else
                  Check_Expressions;
                  return;
               end if;
            end;

         --  For Size, give size of object if available, otherwise we
         --  cannot fold Size.

         elsif Id = Attribute_Size then
            if Is_Entity_Name (P)
              and then Known_Esize (Entity (P))
            then
               Compile_Time_Known_Attribute (N, Esize (Entity (P)));
               return;

            else
               Check_Expressions;
               return;
            end if;

         --  For Alignment, give size of object if available, otherwise we
         --  cannot fold Alignment.

         elsif Id = Attribute_Alignment then
            if Is_Entity_Name (P)
              and then Known_Alignment (Entity (P))
            then
               Fold_Uint (N, Alignment (Entity (P)), Static);
               return;

            else
               Check_Expressions;
               return;
            end if;

         --  For Lock_Free, we apply the attribute to the type of the object.
         --  This is allowed since we have already verified that the type is a
         --  protected type.

         elsif Id = Attribute_Lock_Free then
            P_Entity := Etype (P);

         --  No other attributes for objects are folded

         else
            Check_Expressions;
            return;
         end if;

      --  Cases where P is not an object. Cannot do anything if P is not the
      --  name of an entity.

      elsif not Is_Entity_Name (P) then
         Check_Expressions;
         return;

      --  Otherwise get prefix entity

      else
         P_Entity := Entity (P);
      end if;

      --  If we are asked to evaluate an attribute where the prefix is a
      --  non-frozen generic actual type whose RM_Size is still set to zero,
      --  then abandon the effort.

      if Is_Type (P_Entity)
        and then (not Is_Frozen (P_Entity)
                   and then Is_Generic_Actual_Type (P_Entity)
                   and then RM_Size (P_Entity) = 0)

        --  However, the attribute Unconstrained_Array must be evaluated,
        --  since it is documented to be a static attribute (and can for
        --  example appear in a Compile_Time_Warning pragma). The frozen
        --  status of the type does not affect its evaluation.

        and then Id /= Attribute_Unconstrained_Array
      then
         return;
      end if;

      --  At this stage P_Entity is the entity to which the attribute
      --  is to be applied. This is usually simply the entity of the
      --  prefix, except in some cases of attributes for objects, where
      --  as described above, we apply the attribute to the object type.

      --  Here is where we make sure that static attributes are properly
      --  marked as such. These are attributes whose prefix is a static
      --  scalar subtype, whose result is scalar, and whose arguments, if
      --  present, are static scalar expressions. Note that such references
      --  are static expressions even if they raise Constraint_Error.

      --  For example, Boolean'Pos (1/0 = 0) is a static expression, even
      --  though evaluating it raises constraint error. This means that a
      --  declaration like:

      --    X : constant := (if True then 1 else Boolean'Pos (1/0 = 0));

      --  is legal, since here this expression appears in a statically
      --  unevaluated position, so it does not actually raise an exception.

      if Is_Scalar_Type (P_Entity)
        and then (not Is_Generic_Type (P_Entity))
        and then Is_Static_Subtype (P_Entity)
        and then Is_Scalar_Type (Etype (N))
        and then
          (No (E1)
            or else (Is_Static_Expression (E1)
                      and then Is_Scalar_Type (Etype (E1))))
        and then
          (No (E2)
            or else (Is_Static_Expression (E2)
                      and then Is_Scalar_Type (Etype (E1))))
      then
         Static := True;
         Set_Is_Static_Expression (N, True);
      end if;

      --  First foldable possibility is a scalar or array type (RM 4.9(7))
      --  that is not generic (generic types are eliminated by RM 4.9(25)).
      --  Note we allow non-static non-generic types at this stage as further
      --  described below.

      if Is_Type (P_Entity)
        and then (Is_Scalar_Type (P_Entity) or Is_Array_Type (P_Entity))
        and then (not Is_Generic_Type (P_Entity))
      then
         P_Type := P_Entity;

      --  Second foldable possibility is an array object (RM 4.9(8))

      elsif Ekind_In (P_Entity, E_Variable, E_Constant)
        and then Is_Array_Type (Etype (P_Entity))
        and then (not Is_Generic_Type (Etype (P_Entity)))
      then
         P_Type := Etype (P_Entity);

         --  If the entity is an array constant with an unconstrained nominal
         --  subtype then get the type from the initial value. If the value has
         --  been expanded into assignments, there is no expression and the
         --  attribute reference remains dynamic.

         --  We could do better here and retrieve the type ???

         if Ekind (P_Entity) = E_Constant
           and then not Is_Constrained (P_Type)
         then
            if No (Constant_Value (P_Entity)) then
               return;
            else
               P_Type := Etype (Constant_Value (P_Entity));
            end if;
         end if;

      --  Definite must be folded if the prefix is not a generic type, that
      --  is to say if we are within an instantiation. Same processing applies
      --  to the GNAT attributes Atomic_Always_Lock_Free, Has_Discriminants,
      --  Lock_Free, Type_Class, Has_Tagged_Value, and Unconstrained_Array.

      elsif (Id = Attribute_Atomic_Always_Lock_Free or else
             Id = Attribute_Definite                or else
             Id = Attribute_Has_Access_Values       or else
             Id = Attribute_Has_Discriminants       or else
             Id = Attribute_Has_Tagged_Values       or else
             Id = Attribute_Lock_Free               or else
             Id = Attribute_Type_Class              or else
             Id = Attribute_Unconstrained_Array     or else
             Id = Attribute_Max_Alignment_For_Allocation)
        and then not Is_Generic_Type (P_Entity)
      then
         P_Type := P_Entity;

      --  We can fold 'Size applied to a type if the size is known (as happens
      --  for a size from an attribute definition clause). At this stage, this
      --  can happen only for types (e.g. record types) for which the size is
      --  always non-static. We exclude generic types from consideration (since
      --  they have bogus sizes set within templates).

      elsif Id = Attribute_Size
        and then Is_Type (P_Entity)
        and then (not Is_Generic_Type (P_Entity))
        and then Known_Static_RM_Size (P_Entity)
      then
         Compile_Time_Known_Attribute (N, RM_Size (P_Entity));
         return;

      --  We can fold 'Alignment applied to a type if the alignment is known
      --  (as happens for an alignment from an attribute definition clause).
      --  At this stage, this can happen only for types (e.g. record types) for
      --  which the size is always non-static. We exclude generic types from
      --  consideration (since they have bogus sizes set within templates).

      elsif Id = Attribute_Alignment
        and then Is_Type (P_Entity)
        and then (not Is_Generic_Type (P_Entity))
        and then Known_Alignment (P_Entity)
      then
         Compile_Time_Known_Attribute (N, Alignment (P_Entity));
         return;

      --  If this is an access attribute that is known to fail accessibility
      --  check, rewrite accordingly.

      elsif Attribute_Name (N) = Name_Access
        and then Raises_Constraint_Error (N)
      then
         Rewrite (N,
           Make_Raise_Program_Error (Loc,
             Reason => PE_Accessibility_Check_Failed));
         Set_Etype (N, C_Type);
         return;

      --  No other cases are foldable (they certainly aren't static, and at
      --  the moment we don't try to fold any cases other than the ones above).

      else
         Check_Expressions;
         return;
      end if;

      --  If either attribute or the prefix is Any_Type, then propagate
      --  Any_Type to the result and don't do anything else at all.

      if P_Type = Any_Type
        or else (Present (E1) and then Etype (E1) = Any_Type)
        or else (Present (E2) and then Etype (E2) = Any_Type)
      then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Scalar subtype case. We have not yet enforced the static requirement
      --  of (RM 4.9(7)) and we don't intend to just yet, since there are cases
      --  of non-static attribute references (e.g. S'Digits for a non-static
      --  floating-point type, which we can compute at compile time).

      --  Note: this folding of non-static attributes is not simply a case of
      --  optimization. For many of the attributes affected, Gigi cannot handle
      --  the attribute and depends on the front end having folded them away.

      --  Note: although we don't require staticness at this stage, we do set
      --  the Static variable to record the staticness, for easy reference by
      --  those attributes where it matters (e.g. Succ and Pred), and also to
      --  be used to ensure that non-static folded things are not marked as
      --  being static (a check that is done right at the end).

      P_Root_Type := Root_Type (P_Type);
      P_Base_Type := Base_Type (P_Type);

      --  If the root type or base type is generic, then we cannot fold. This
      --  test is needed because subtypes of generic types are not always
      --  marked as being generic themselves (which seems odd???)

      if Is_Generic_Type (P_Root_Type)
        or else Is_Generic_Type (P_Base_Type)
      then
         return;
      end if;

      if Is_Scalar_Type (P_Type) then
         if not Is_Static_Subtype (P_Type) then
            Static := False;
            Set_Is_Static_Expression (N, False);
         elsif not Is_OK_Static_Subtype (P_Type) then
            Set_Raises_Constraint_Error (N);
         end if;

      --  Array case. We enforce the constrained requirement of (RM 4.9(7-8))
      --  since we can't do anything with unconstrained arrays. In addition,
      --  only the First, Last and Length attributes are possibly static.

      --  Atomic_Always_Lock_Free, Definite, Has_Access_Values,
      --  Has_Discriminants, Has_Tagged_Values, Lock_Free, Type_Class, and
      --  Unconstrained_Array are again exceptions, because they apply as well
      --  to unconstrained types.

      --  In addition Component_Size is an exception since it is possibly
      --  foldable, even though it is never static, and it does apply to
      --  unconstrained arrays. Furthermore, it is essential to fold this
      --  in the packed case, since otherwise the value will be incorrect.

      elsif Id = Attribute_Atomic_Always_Lock_Free or else
            Id = Attribute_Definite                or else
            Id = Attribute_Has_Access_Values       or else
            Id = Attribute_Has_Discriminants       or else
            Id = Attribute_Has_Tagged_Values       or else
            Id = Attribute_Lock_Free               or else
            Id = Attribute_Type_Class              or else
            Id = Attribute_Unconstrained_Array     or else
            Id = Attribute_Component_Size
      then
         Static := False;
         Set_Is_Static_Expression (N, False);

      elsif Id /= Attribute_Max_Alignment_For_Allocation then
         if not Is_Constrained (P_Type)
           or else (Id /= Attribute_First and then
                    Id /= Attribute_Last  and then
                    Id /= Attribute_Length)
         then
            Check_Expressions;
            return;
         end if;

         --  The rules in (RM 4.9(7,8)) require a static array, but as in the
         --  scalar case, we hold off on enforcing staticness, since there are
         --  cases which we can fold at compile time even though they are not
         --  static (e.g. 'Length applied to a static index, even though other
         --  non-static indexes make the array type non-static). This is only
         --  an optimization, but it falls out essentially free, so why not.
         --  Again we compute the variable Static for easy reference later
         --  (note that no array attributes are static in Ada 83).

         --  We also need to set Static properly for subsequent legality checks
         --  which might otherwise accept non-static constants in contexts
         --  where they are not legal.

         Static :=
           Ada_Version >= Ada_95 and then Statically_Denotes_Entity (P);
         Set_Is_Static_Expression (N, Static);

         declare
            Nod : Node_Id;

         begin
            Nod := First_Index (P_Type);

            --  The expression is static if the array type is constrained
            --  by given bounds, and not by an initial expression. Constant
            --  strings are static in any case.

            if Root_Type (P_Type) /= Standard_String then
               Static :=
                 Static and then not Is_Constr_Subt_For_U_Nominal (P_Type);
               Set_Is_Static_Expression (N, Static);
            end if;

            while Present (Nod) loop
               if not Is_Static_Subtype (Etype (Nod)) then
                  Static := False;
                  Set_Is_Static_Expression (N, False);

               elsif not Is_OK_Static_Subtype (Etype (Nod)) then
                  Set_Raises_Constraint_Error (N);
                  Static := False;
                  Set_Is_Static_Expression (N, False);
               end if;

               --  If however the index type is generic, or derived from
               --  one, attributes cannot be folded.

               if Is_Generic_Type (Root_Type (Etype (Nod)))
                 and then Id /= Attribute_Component_Size
               then
                  return;
               end if;

               Next_Index (Nod);
            end loop;
         end;
      end if;

      --  Check any expressions that are present. Note that these expressions,
      --  depending on the particular attribute type, are either part of the
      --  attribute designator, or they are arguments in a case where the
      --  attribute reference returns a function. In the latter case, the
      --  rule in (RM 4.9(22)) applies and in particular requires the type
      --  of the expressions to be scalar in order for the attribute to be
      --  considered to be static.

      declare
         E : Node_Id;

      begin
         E := E1;

         while Present (E) loop

            --  If expression is not static, then the attribute reference
            --  result certainly cannot be static.

            if not Is_Static_Expression (E) then
               Static := False;
               Set_Is_Static_Expression (N, False);
            end if;

            if Raises_Constraint_Error (E) then
               Set_Raises_Constraint_Error (N);
            end if;

            --  If the result is not known at compile time, or is not of
            --  a scalar type, then the result is definitely not static,
            --  so we can quit now.

            if not Compile_Time_Known_Value (E)
              or else not Is_Scalar_Type (Etype (E))
            then
               --  An odd special case, if this is a Pos attribute, this
               --  is where we need to apply a range check since it does
               --  not get done anywhere else.

               if Id = Attribute_Pos then
                  if Is_Integer_Type (Etype (E)) then
                     Apply_Range_Check (E, Etype (N));
                  end if;
               end if;

               Check_Expressions;
               return;

            --  If the expression raises a constraint error, then so does
            --  the attribute reference. We keep going in this case because
            --  we are still interested in whether the attribute reference
            --  is static even if it is not static.

            elsif Raises_Constraint_Error (E) then
               Set_Raises_Constraint_Error (N);
            end if;

            Next (E);
         end loop;

         if Raises_Constraint_Error (Prefix (N)) then
            Set_Is_Static_Expression (N, False);
            return;
         end if;
      end;

      --  Deal with the case of a static attribute reference that raises
      --  constraint error. The Raises_Constraint_Error flag will already
      --  have been set, and the Static flag shows whether the attribute
      --  reference is static. In any case we certainly can't fold such an
      --  attribute reference.

      --  Note that the rewriting of the attribute node with the constraint
      --  error node is essential in this case, because otherwise Gigi might
      --  blow up on one of the attributes it never expects to see.

      --  The constraint_error node must have the type imposed by the context,
      --  to avoid spurious errors in the enclosing expression.

      if Raises_Constraint_Error (N) then
         CE_Node :=
           Make_Raise_Constraint_Error (Sloc (N),
             Reason => CE_Range_Check_Failed);
         Set_Etype (CE_Node, Etype (N));
         Set_Raises_Constraint_Error (CE_Node);
         Check_Expressions;
         Rewrite (N, Relocate_Node (CE_Node));
         Set_Raises_Constraint_Error (N, True);
         return;
      end if;

      --  At this point we have a potentially foldable attribute reference.
      --  If Static is set, then the attribute reference definitely obeys
      --  the requirements in (RM 4.9(7,8,22)), and it definitely can be
      --  folded. If Static is not set, then the attribute may or may not
      --  be foldable, and the individual attribute processing routines
      --  test Static as required in cases where it makes a difference.

      --  In the case where Static is not set, we do know that all the
      --  expressions present are at least known at compile time (we assumed
      --  above that if this was not the case, then there was no hope of static
      --  evaluation). However, we did not require that the bounds of the
      --  prefix type be compile time known, let alone static). That's because
      --  there are many attributes that can be computed at compile time on
      --  non-static subtypes, even though such references are not static
      --  expressions.

      --  For VAX float, the root type is an IEEE type. So make sure to use the
      --  base type instead of the root-type for floating point attributes.

      case Id is

      --  Attributes related to Ada 2012 iterators (placeholder ???)

      when Attribute_Constant_Indexing
         | Attribute_Default_Iterator
         | Attribute_Implicit_Dereference
         | Attribute_Iterator_Element
         | Attribute_Iterable
         | Attribute_Variable_Indexing
      =>
         null;

      --  Internal attributes used to deal with Ada 2012 delayed aspects.
      --  These were already rejected by the parser. Thus they shouldn't
      --  appear here.

      when Internal_Attribute_Id =>
         raise Program_Error;

      --------------
      -- Adjacent --
      --------------

      when Attribute_Adjacent =>
         Fold_Ureal
           (N,
            Eval_Fat.Adjacent
              (P_Base_Type, Expr_Value_R (E1), Expr_Value_R (E2)),
            Static);

      ---------
      -- Aft --
      ---------

      when Attribute_Aft =>
         Fold_Uint (N, Aft_Value (P_Type), Static);

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment => Alignment_Block : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         --  Fold if alignment is set and not otherwise

         if Known_Alignment (P_TypeA) then
            Fold_Uint (N, Alignment (P_TypeA), Static);
         end if;
      end Alignment_Block;

      -----------------------------
      -- Atomic_Always_Lock_Free --
      -----------------------------

      --  Atomic_Always_Lock_Free attribute is a Boolean, thus no need to fold
      --  here.

      when Attribute_Atomic_Always_Lock_Free => Atomic_Always_Lock_Free :
      declare
         V : constant Entity_Id :=
               Boolean_Literals
                 (Support_Atomic_Primitives_On_Target
                   and then Support_Atomic_Primitives (P_Type));

      begin
         Rewrite (N, New_Occurrence_Of (V, Loc));

         --  Analyze and resolve as boolean. Note that this attribute is a
         --  static attribute in GNAT.

         Analyze_And_Resolve (N, Standard_Boolean);
            Static := True;
            Set_Is_Static_Expression (N, True);
      end Atomic_Always_Lock_Free;

      ---------
      -- Bit --
      ---------

      --  Bit can never be folded

      when Attribute_Bit =>
         null;

      ------------------
      -- Body_Version --
      ------------------

      --  Body_version can never be static

      when Attribute_Body_Version =>
         null;

      -------------
      -- Ceiling --
      -------------

      when Attribute_Ceiling =>
         Fold_Ureal
           (N, Eval_Fat.Ceiling (P_Base_Type, Expr_Value_R (E1)), Static);

      --------------------
      -- Component_Size --
      --------------------

      when Attribute_Component_Size =>
         if Known_Static_Component_Size (P_Type) then
            Fold_Uint (N, Component_Size (P_Type), Static);
         end if;

      -------------
      -- Compose --
      -------------

      when Attribute_Compose =>
         Fold_Ureal
           (N,
            Eval_Fat.Compose (P_Base_Type, Expr_Value_R (E1), Expr_Value (E2)),
            Static);

      -----------------
      -- Constrained --
      -----------------

      --  Constrained is never folded for now, there may be cases that
      --  could be handled at compile time. To be looked at later.

      when Attribute_Constrained =>

         --  The expander might fold it and set the static flag accordingly,
         --  but with expansion disabled (as in ASIS), it remains as an
         --  attribute reference, and this reference is not static.

         Set_Is_Static_Expression (N, False);
         null;

      ---------------
      -- Copy_Sign --
      ---------------

      when Attribute_Copy_Sign =>
         Fold_Ureal
           (N,
            Eval_Fat.Copy_Sign
              (P_Base_Type, Expr_Value_R (E1), Expr_Value_R (E2)),
            Static);

      --------------
      -- Definite --
      --------------

      when Attribute_Definite =>
         Rewrite (N, New_Occurrence_Of (
           Boolean_Literals (Is_Definite_Subtype (P_Entity)), Loc));
         Analyze_And_Resolve (N, Standard_Boolean);

      -----------
      -- Delta --
      -----------

      when Attribute_Delta =>
         Fold_Ureal (N, Delta_Value (P_Type), True);

      ------------
      -- Denorm --
      ------------

      when Attribute_Denorm =>
         Fold_Uint
           (N, UI_From_Int (Boolean'Pos (Has_Denormals (P_Type))), Static);

      ---------------------
      -- Descriptor_Size --
      ---------------------

      when Attribute_Descriptor_Size =>
         null;

      ------------
      -- Digits --
      ------------

      when Attribute_Digits =>
         Fold_Uint (N, Digits_Value (P_Type), Static);

      ----------
      -- Emax --
      ----------

      when Attribute_Emax =>

         --  Ada 83 attribute is defined as (RM83 3.5.8)

         --    T'Emax = 4 * T'Mantissa

         Fold_Uint (N, 4 * Mantissa, Static);

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep => Enum_Rep : declare
         Val : Node_Id;

      begin
         --  The attribute appears in the form:

         --    Enum_Typ'Enum_Rep (Const)
         --    Enum_Typ'Enum_Rep (Enum_Lit)

         if Present (E1) then
            Val := E1;

         --  Otherwise the prefix denotes a constant or enumeration literal:

         --    Const'Enum_Rep
         --    Enum_Lit'Enum_Rep

         else
            Val := P;
         end if;

         --  For an enumeration type with a non-standard representation use
         --  the Enumeration_Rep field of the proper constant. Note that this
         --  will not work for types Character/Wide_[Wide-]Character, since no
         --  real entities are created for the enumeration literals, but that
         --  does not matter since these two types do not have non-standard
         --  representations anyway.

         if Is_Enumeration_Type (P_Type)
           and then Has_Non_Standard_Rep (P_Type)
         then
            Fold_Uint (N, Enumeration_Rep (Expr_Value_E (Val)), Static);

         --  For enumeration types with standard representations and all other
         --  cases (i.e. all integer and modular types), Enum_Rep is equivalent
         --  to Pos.

         else
            Fold_Uint (N, Expr_Value (Val), Static);
         end if;
      end Enum_Rep;

      --------------
      -- Enum_Val --
      --------------

      when Attribute_Enum_Val => Enum_Val : declare
         Lit : Node_Id;

      begin
         --  We have something like Enum_Type'Enum_Val (23), so search for a
         --  corresponding value in the list of Enum_Rep values for the type.

         Lit := First_Literal (P_Base_Type);
         loop
            if Enumeration_Rep (Lit) = Expr_Value (E1) then
               Fold_Uint (N, Enumeration_Pos (Lit), Static);
               exit;
            end if;

            Next_Literal (Lit);

            if No (Lit) then
               Apply_Compile_Time_Constraint_Error
                 (N, "no representation value matches",
                  CE_Range_Check_Failed,
                  Warn => not Static);
               exit;
            end if;
         end loop;
      end Enum_Val;

      -------------
      -- Epsilon --
      -------------

      when Attribute_Epsilon =>

         --  Ada 83 attribute is defined as (RM83 3.5.8)

         --    T'Epsilon = 2.0**(1 - T'Mantissa)

         Fold_Ureal (N, Ureal_2 ** (1 - Mantissa), True);

      --------------
      -- Exponent --
      --------------

      when Attribute_Exponent =>
         Fold_Uint (N,
           Eval_Fat.Exponent (P_Base_Type, Expr_Value_R (E1)), Static);

      -----------------------
      -- Finalization_Size --
      -----------------------

      when Attribute_Finalization_Size =>
         null;

      -----------
      -- First --
      -----------

      when Attribute_First =>
         Set_Bounds;

         if Compile_Time_Known_Value (Lo_Bound) then
            if Is_Real_Type (P_Type) then
               Fold_Ureal (N, Expr_Value_R (Lo_Bound), Static);
            else
               Fold_Uint  (N, Expr_Value (Lo_Bound), Static);
            end if;

         else
            Check_Concurrent_Discriminant (Lo_Bound);
         end if;

      -----------------
      -- First_Valid --
      -----------------

      when Attribute_First_Valid =>
         if Has_Predicates (P_Type)
           and then Has_Static_Predicate (P_Type)
         then
            declare
               FirstN : constant Node_Id :=
                          First (Static_Discrete_Predicate (P_Type));
            begin
               if Nkind (FirstN) = N_Range then
                  Fold_Uint (N, Expr_Value (Low_Bound (FirstN)), Static);
               else
                  Fold_Uint (N, Expr_Value (FirstN), Static);
               end if;
            end;

         else
            Set_Bounds;
            Fold_Uint (N, Expr_Value (Lo_Bound), Static);
         end if;

      -----------------
      -- Fixed_Value --
      -----------------

      when Attribute_Fixed_Value =>
         null;

      -----------
      -- Floor --
      -----------

      when Attribute_Floor =>
         Fold_Ureal
           (N, Eval_Fat.Floor (P_Base_Type, Expr_Value_R (E1)), Static);

      ----------
      -- Fore --
      ----------

      when Attribute_Fore =>
         if Compile_Time_Known_Bounds (P_Type) then
            Fold_Uint (N, UI_From_Int (Fore_Value), Static);
         end if;

      --------------
      -- Fraction --
      --------------

      when Attribute_Fraction =>
         Fold_Ureal
           (N, Eval_Fat.Fraction (P_Base_Type, Expr_Value_R (E1)), Static);

      -----------------------
      -- Has_Access_Values --
      -----------------------

      when Attribute_Has_Access_Values =>
         Rewrite (N, New_Occurrence_Of
           (Boolean_Literals (Has_Access_Values (P_Root_Type)), Loc));
         Analyze_And_Resolve (N, Standard_Boolean);

      -----------------------
      -- Has_Discriminants --
      -----------------------

      when Attribute_Has_Discriminants =>
         Rewrite (N, New_Occurrence_Of (
           Boolean_Literals (Has_Discriminants (P_Entity)), Loc));
         Analyze_And_Resolve (N, Standard_Boolean);

      ----------------------
      -- Has_Same_Storage --
      ----------------------

      when Attribute_Has_Same_Storage =>
         null;

      -----------------------
      -- Has_Tagged_Values --
      -----------------------

      when Attribute_Has_Tagged_Values =>
         Rewrite (N, New_Occurrence_Of
           (Boolean_Literals (Has_Tagged_Component (P_Root_Type)), Loc));
         Analyze_And_Resolve (N, Standard_Boolean);

      --------------
      -- Identity --
      --------------

      when Attribute_Identity =>
         null;

      -----------
      -- Image --
      -----------

      --  Image is a scalar attribute, but is never static, because it is
      --  not a static function (having a non-scalar argument (RM 4.9(22))
      --  However, we can constant-fold the image of an enumeration literal
      --  if names are available.

      when Attribute_Image =>
         if Is_Entity_Name (E1)
           and then Ekind (Entity (E1)) = E_Enumeration_Literal
           and then not Discard_Names (First_Subtype (Etype (E1)))
           and then not Global_Discard_Names
         then
            declare
               Lit : constant Entity_Id := Entity (E1);
               Str : String_Id;
            begin
               Start_String;
               Get_Unqualified_Decoded_Name_String (Chars (Lit));
               Set_Casing (All_Upper_Case);
               Store_String_Chars (Name_Buffer (1 .. Name_Len));
               Str := End_String;
               Rewrite (N, Make_String_Literal (Loc, Strval => Str));
               Analyze_And_Resolve (N, Standard_String);
               Set_Is_Static_Expression (N, False);
            end;
         end if;

      -------------------
      -- Integer_Value --
      -------------------

      --  We never try to fold Integer_Value (though perhaps we could???)

      when Attribute_Integer_Value =>
         null;

      -------------------
      -- Invalid_Value --
      -------------------

      --  Invalid_Value is a scalar attribute that is never static, because
      --  the value is by design out of range.

      when Attribute_Invalid_Value =>
         null;

      -----------
      -- Large --
      -----------

      when Attribute_Large =>

         --  For fixed-point, we use the identity:

         --    T'Large = (2.0**T'Mantissa - 1.0) * T'Small

         if Is_Fixed_Point_Type (P_Type) then
            Rewrite (N,
              Make_Op_Multiply (Loc,
                Left_Opnd =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Op_Expon (Loc,
                        Left_Opnd =>
                          Make_Real_Literal (Loc, Ureal_2),
                        Right_Opnd =>
                          Make_Attribute_Reference (Loc,
                            Prefix => P,
                            Attribute_Name => Name_Mantissa)),
                    Right_Opnd => Make_Real_Literal (Loc, Ureal_1)),

                Right_Opnd =>
                  Make_Real_Literal (Loc, Small_Value (Entity (P)))));

            Analyze_And_Resolve (N, C_Type);

         --  Floating-point (Ada 83 compatibility)

         else
            --  Ada 83 attribute is defined as (RM83 3.5.8)

            --    T'Large = 2.0**T'Emax * (1.0 - 2.0**(-T'Mantissa))

            --  where

            --    T'Emax = 4 * T'Mantissa

            Fold_Ureal
              (N,
               Ureal_2 ** (4 * Mantissa) * (Ureal_1 - Ureal_2 ** (-Mantissa)),
               True);
         end if;

      ---------------
      -- Lock_Free --
      ---------------

      when Attribute_Lock_Free => Lock_Free : declare
         V : constant Entity_Id := Boolean_Literals (Uses_Lock_Free (P_Type));

      begin
         Rewrite (N, New_Occurrence_Of (V, Loc));

         --  Analyze and resolve as boolean. Note that this attribute is a
         --  static attribute in GNAT.

         Analyze_And_Resolve (N, Standard_Boolean);
            Static := True;
            Set_Is_Static_Expression (N, True);
      end Lock_Free;

      ----------
      -- Last --
      ----------

      when Attribute_Last =>
         Set_Bounds;

         if Compile_Time_Known_Value (Hi_Bound) then
            if Is_Real_Type (P_Type) then
               Fold_Ureal (N, Expr_Value_R (Hi_Bound), Static);
            else
               Fold_Uint  (N, Expr_Value (Hi_Bound), Static);
            end if;

         else
            Check_Concurrent_Discriminant (Hi_Bound);
         end if;

      ----------------
      -- Last_Valid --
      ----------------

      when Attribute_Last_Valid =>
         if Has_Predicates (P_Type)
           and then Has_Static_Predicate (P_Type)
         then
            declare
               LastN : constant Node_Id :=
                         Last (Static_Discrete_Predicate (P_Type));
            begin
               if Nkind (LastN) = N_Range then
                  Fold_Uint (N, Expr_Value (High_Bound (LastN)), Static);
               else
                  Fold_Uint (N, Expr_Value (LastN), Static);
               end if;
            end;

         else
            Set_Bounds;
            Fold_Uint (N, Expr_Value (Hi_Bound), Static);
         end if;

      ------------------
      -- Leading_Part --
      ------------------

      when Attribute_Leading_Part =>
         Fold_Ureal
           (N,
            Eval_Fat.Leading_Part
              (P_Base_Type, Expr_Value_R (E1), Expr_Value (E2)),
            Static);

      ------------
      -- Length --
      ------------

      when Attribute_Length => Length : declare
         Ind : Node_Id;

      begin
         --  If any index type is a formal type, or derived from one, the
         --  bounds are not static. Treating them as static can produce
         --  spurious warnings or improper constant folding.

         Ind := First_Index (P_Type);
         while Present (Ind) loop
            if Is_Generic_Type (Root_Type (Etype (Ind))) then
               return;
            end if;

            Next_Index (Ind);
         end loop;

         Set_Bounds;

         --  For two compile time values, we can compute length

         if Compile_Time_Known_Value (Lo_Bound)
           and then Compile_Time_Known_Value (Hi_Bound)
         then
            Fold_Uint (N,
              UI_Max (0, 1 + (Expr_Value (Hi_Bound) - Expr_Value (Lo_Bound))),
              Static);
         end if;

         --  One more case is where Hi_Bound and Lo_Bound are compile-time
         --  comparable, and we can figure out the difference between them.

         declare
            Diff : aliased Uint;

         begin
            case
              Compile_Time_Compare
                (Lo_Bound, Hi_Bound, Diff'Access, Assume_Valid => False)
            is
               when EQ =>
                  Fold_Uint (N, Uint_1, Static);

               when GT =>
                  Fold_Uint (N, Uint_0, Static);

               when LT =>
                  if Diff /= No_Uint then
                     Fold_Uint (N, Diff + 1, Static);
                  end if;

               when others =>
                  null;
            end case;
         end;
      end Length;

      ----------------
      -- Loop_Entry --
      ----------------

      --  Loop_Entry acts as an alias of a constant initialized to the prefix
      --  of the said attribute at the point of entry into the related loop. As
      --  such, the attribute reference does not need to be evaluated because
      --  the prefix is the one that is evaluted.

      when Attribute_Loop_Entry =>
         null;

      -------------
      -- Machine --
      -------------

      when Attribute_Machine =>
         Fold_Ureal
           (N,
            Eval_Fat.Machine
              (P_Base_Type, Expr_Value_R (E1), Eval_Fat.Round, N),
            Static);

      ------------------
      -- Machine_Emax --
      ------------------

      when Attribute_Machine_Emax =>
         Fold_Uint (N, Machine_Emax_Value (P_Type), Static);

      ------------------
      -- Machine_Emin --
      ------------------

      when Attribute_Machine_Emin =>
         Fold_Uint (N, Machine_Emin_Value (P_Type), Static);

      ----------------------
      -- Machine_Mantissa --
      ----------------------

      when Attribute_Machine_Mantissa =>
         Fold_Uint (N, Machine_Mantissa_Value (P_Type), Static);

      -----------------------
      -- Machine_Overflows --
      -----------------------

      when Attribute_Machine_Overflows =>

         --  Always true for fixed-point

         if Is_Fixed_Point_Type (P_Type) then
            Fold_Uint (N, True_Value, Static);

         --  Floating point case

         else
            Fold_Uint (N,
              UI_From_Int (Boolean'Pos (Machine_Overflows_On_Target)),
              Static);
         end if;

      -------------------
      -- Machine_Radix --
      -------------------

      when Attribute_Machine_Radix =>
         if Is_Fixed_Point_Type (P_Type) then
            if Is_Decimal_Fixed_Point_Type (P_Type)
              and then Machine_Radix_10 (P_Type)
            then
               Fold_Uint (N, Uint_10, Static);
            else
               Fold_Uint (N, Uint_2, Static);
            end if;

         --  All floating-point type always have radix 2

         else
            Fold_Uint (N, Uint_2, Static);
         end if;

      ----------------------
      -- Machine_Rounding --
      ----------------------

      --  Note: for the folding case, it is fine to treat Machine_Rounding
      --  exactly the same way as Rounding, since this is one of the allowed
      --  behaviors, and performance is not an issue here. It might be a bit
      --  better to give the same result as it would give at run time, even
      --  though the non-determinism is certainly permitted.

      when Attribute_Machine_Rounding =>
         Fold_Ureal
           (N, Eval_Fat.Rounding (P_Base_Type, Expr_Value_R (E1)), Static);

      --------------------
      -- Machine_Rounds --
      --------------------

      when Attribute_Machine_Rounds =>

         --  Always False for fixed-point

         if Is_Fixed_Point_Type (P_Type) then
            Fold_Uint (N, False_Value, Static);

         --  Else yield proper floating-point result

         else
            Fold_Uint
              (N, UI_From_Int (Boolean'Pos (Machine_Rounds_On_Target)),
               Static);
         end if;

      ------------------
      -- Machine_Size --
      ------------------

      --  Note: Machine_Size is identical to Object_Size

      when Attribute_Machine_Size => Machine_Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if Known_Esize (P_TypeA) then
            Fold_Uint (N, Esize (P_TypeA), Static);
         end if;
      end Machine_Size;

      --------------
      -- Mantissa --
      --------------

      when Attribute_Mantissa =>

         --  Fixed-point mantissa

         if Is_Fixed_Point_Type (P_Type) then

            --  Compile time foldable case

            if Compile_Time_Known_Value (Type_Low_Bound  (P_Type))
                 and then
               Compile_Time_Known_Value (Type_High_Bound (P_Type))
            then
               --  The calculation of the obsolete Ada 83 attribute Mantissa
               --  is annoying, because of AI00143, quoted here:

               --  !question 84-01-10

               --  Consider the model numbers for F:

               --         type F is delta 1.0 range -7.0 .. 8.0;

               --  The wording requires that F'MANTISSA be the SMALLEST
               --  integer number for which each  bound  of the specified
               --  range is either a model number or lies at most small
               --  distant from a model number. This means F'MANTISSA
               --  is required to be 3 since the range  -7.0 .. 7.0 fits
               --  in 3 signed bits, and 8 is "at most" 1.0 from a model
               --  number, namely, 7. Is this analysis correct? Note that
               --  this implies the upper bound of the range is not
               --  represented as a model number.

               --  !response 84-03-17

               --  The analysis is correct. The upper and lower bounds for
               --  a fixed  point type can lie outside the range of model
               --  numbers.

               declare
                  Siz     : Uint;
                  LBound  : Ureal;
                  UBound  : Ureal;
                  Bound   : Ureal;
                  Max_Man : Uint;

               begin
                  LBound  := Expr_Value_R (Type_Low_Bound  (P_Type));
                  UBound  := Expr_Value_R (Type_High_Bound (P_Type));
                  Bound   := UR_Max (UR_Abs (LBound), UR_Abs (UBound));
                  Max_Man := UR_Trunc (Bound / Small_Value (P_Type));

                  --  If the Bound is exactly a model number, i.e. a multiple
                  --  of Small, then we back it off by one to get the integer
                  --  value that must be representable.

                  if Small_Value (P_Type) * Max_Man = Bound then
                     Max_Man := Max_Man - 1;
                  end if;

                  --  Now find corresponding size = Mantissa value

                  Siz := Uint_0;
                  while 2 ** Siz < Max_Man loop
                     Siz := Siz + 1;
                  end loop;

                  Fold_Uint (N, Siz, Static);
               end;

            else
               --  The case of dynamic bounds cannot be evaluated at compile
               --  time. Instead we use a runtime routine (see Exp_Attr).

               null;
            end if;

         --  Floating-point Mantissa

         else
            Fold_Uint (N, Mantissa, Static);
         end if;

      ---------
      -- Max --
      ---------

      when Attribute_Max =>
         if Is_Real_Type (P_Type) then
            Fold_Ureal
              (N, UR_Max (Expr_Value_R (E1), Expr_Value_R (E2)), Static);
         else
            Fold_Uint (N, UI_Max (Expr_Value (E1), Expr_Value (E2)), Static);
         end if;

      ----------------------------------
      -- Max_Alignment_For_Allocation --
      ----------------------------------

      --  Max_Alignment_For_Allocation is usually the Alignment. However,
      --  arrays are allocated with dope, so we need to take into account both
      --  the alignment of the array, which comes from the component alignment,
      --  and the alignment of the dope. Also, if the alignment is unknown, we
      --  use the max (it's OK to be pessimistic).

      when Attribute_Max_Alignment_For_Allocation => Max_Align : declare
         A : Uint := UI_From_Int (Ttypes.Maximum_Alignment);
      begin
         if Known_Alignment (P_Type)
           and then (not Is_Array_Type (P_Type) or else Alignment (P_Type) > A)
         then
            A := Alignment (P_Type);
         end if;

            Fold_Uint (N, A, Static);
      end Max_Align;

      ----------------------------------
      -- Max_Size_In_Storage_Elements --
      ----------------------------------

      --  Max_Size_In_Storage_Elements is simply the Size rounded up to a
      --  Storage_Unit boundary. We can fold any cases for which the size
      --  is known by the front end.

      when Attribute_Max_Size_In_Storage_Elements =>
         if Known_Esize (P_Type) then
            Fold_Uint (N,
              (Esize (P_Type) + System_Storage_Unit - 1) /
                                          System_Storage_Unit,
               Static);
         end if;

      --------------------
      -- Mechanism_Code --
      --------------------

      when Attribute_Mechanism_Code => Mechanism_Code : declare
         Formal : Entity_Id;
         Mech   : Mechanism_Type;
         Val    : Int;

      begin
         if No (E1) then
            Mech := Mechanism (P_Entity);

         else
            Val := UI_To_Int (Expr_Value (E1));

            Formal := First_Formal (P_Entity);
            for J in 1 .. Val - 1 loop
               Next_Formal (Formal);
            end loop;

            Mech := Mechanism (Formal);
         end if;

         if Mech < 0 then
            Fold_Uint (N, UI_From_Int (Int (-Mech)), Static);
         end if;
      end Mechanism_Code;

      ---------
      -- Min --
      ---------

      when Attribute_Min =>
         if Is_Real_Type (P_Type) then
            Fold_Ureal
              (N, UR_Min (Expr_Value_R (E1), Expr_Value_R (E2)), Static);
         else
            Fold_Uint
              (N, UI_Min (Expr_Value (E1), Expr_Value (E2)), Static);
         end if;

      ---------
      -- Mod --
      ---------

      when Attribute_Mod =>
         Fold_Uint
           (N, UI_Mod (Expr_Value (E1), Modulus (P_Base_Type)), Static);

      -----------
      -- Model --
      -----------

      when Attribute_Model =>
         Fold_Ureal
           (N, Eval_Fat.Model (P_Base_Type, Expr_Value_R (E1)), Static);

      ----------------
      -- Model_Emin --
      ----------------

      when Attribute_Model_Emin =>
         Fold_Uint (N, Model_Emin_Value (P_Base_Type), Static);

      -------------------
      -- Model_Epsilon --
      -------------------

      when Attribute_Model_Epsilon =>
         Fold_Ureal (N, Model_Epsilon_Value (P_Base_Type), Static);

      --------------------
      -- Model_Mantissa --
      --------------------

      when Attribute_Model_Mantissa =>
         Fold_Uint (N, Model_Mantissa_Value (P_Base_Type), Static);

      -----------------
      -- Model_Small --
      -----------------

      when Attribute_Model_Small =>
         Fold_Ureal (N, Model_Small_Value (P_Base_Type), Static);

      -------------
      -- Modulus --
      -------------

      when Attribute_Modulus =>
         Fold_Uint (N, Modulus (P_Type), Static);

      --------------------
      -- Null_Parameter --
      --------------------

      --  Cannot fold, we know the value sort of, but the whole point is
      --  that there is no way to talk about this imaginary value except
      --  by using the attribute, so we leave it the way it is.

      when Attribute_Null_Parameter =>
         null;

      -----------------
      -- Object_Size --
      -----------------

      --  The Object_Size attribute for a type returns the Esize of the
      --  type and can be folded if this value is known.

      when Attribute_Object_Size => Object_Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if Known_Esize (P_TypeA) then
            Fold_Uint (N, Esize (P_TypeA), Static);
         end if;
      end Object_Size;

      ----------------------
      -- Overlaps_Storage --
      ----------------------

      when Attribute_Overlaps_Storage =>
         null;

      -------------------------
      -- Passed_By_Reference --
      -------------------------

      --  Scalar types are never passed by reference

      when Attribute_Passed_By_Reference =>
         Fold_Uint (N, False_Value, Static);

      ---------
      -- Pos --
      ---------

      when Attribute_Pos =>
         Fold_Uint (N, Expr_Value (E1), Static);

      ----------
      -- Pred --
      ----------

      when Attribute_Pred =>

         --  Floating-point case

         if Is_Floating_Point_Type (P_Type) then
            Fold_Ureal
              (N, Eval_Fat.Pred (P_Base_Type, Expr_Value_R (E1)), Static);

         --  Fixed-point case

         elsif Is_Fixed_Point_Type (P_Type) then
            Fold_Ureal
              (N, Expr_Value_R (E1) - Small_Value (P_Type), True);

         --  Modular integer case (wraps)

         elsif Is_Modular_Integer_Type (P_Type) then
            Fold_Uint (N, (Expr_Value (E1) - 1) mod Modulus (P_Type), Static);

         --  Other scalar cases

         else
            pragma Assert (Is_Scalar_Type (P_Type));

            if Is_Enumeration_Type (P_Type)
              and then Expr_Value (E1) =
                         Expr_Value (Type_Low_Bound (P_Base_Type))
            then
               Apply_Compile_Time_Constraint_Error
                 (N, "Pred of `&''First`",
                  CE_Overflow_Check_Failed,
                  Ent  => P_Base_Type,
                  Warn => not Static);

               Check_Expressions;
               return;
            end if;

            Fold_Uint (N, Expr_Value (E1) - 1, Static);
         end if;

      -----------
      -- Range --
      -----------

      --  No processing required, because by this stage, Range has been
      --  replaced by First .. Last, so this branch can never be taken.

      when Attribute_Range =>
         raise Program_Error;

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length => Range_Length : declare
         Diff : aliased Uint;

      begin
         Set_Bounds;

         --  Can fold if both bounds are compile time known

         if Compile_Time_Known_Value (Hi_Bound)
           and then Compile_Time_Known_Value (Lo_Bound)
         then
            Fold_Uint (N,
              UI_Max
                (0, Expr_Value (Hi_Bound) - Expr_Value (Lo_Bound) + 1),
                 Static);
         end if;

         --  One more case is where Hi_Bound and Lo_Bound are compile-time
         --  comparable, and we can figure out the difference between them.

         case Compile_Time_Compare
                (Lo_Bound, Hi_Bound, Diff'Access, Assume_Valid => False)
         is
            when EQ =>
               Fold_Uint (N, Uint_1, Static);

            when GT =>
               Fold_Uint (N, Uint_0, Static);

            when LT =>
               if Diff /= No_Uint then
                  Fold_Uint (N, Diff + 1, Static);
               end if;

            when others =>
               null;
         end case;
      end Range_Length;

      ---------
      -- Ref --
      ---------

      when Attribute_Ref =>
         Fold_Uint (N, Expr_Value (E1), Static);

      ---------------
      -- Remainder --
      ---------------

      when Attribute_Remainder => Remainder : declare
         X : constant Ureal := Expr_Value_R (E1);
         Y : constant Ureal := Expr_Value_R (E2);

      begin
         if UR_Is_Zero (Y) then
            Apply_Compile_Time_Constraint_Error
              (N, "division by zero in Remainder",
               CE_Overflow_Check_Failed,
               Warn => not Static);

            Check_Expressions;
            return;
         end if;

         Fold_Ureal (N, Eval_Fat.Remainder (P_Base_Type, X, Y), Static);
      end Remainder;

      -----------------
      -- Restriction --
      -----------------

      when Attribute_Restriction_Set =>
         Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
         Set_Is_Static_Expression (N);

      -----------
      -- Round --
      -----------

      when Attribute_Round => Round : declare
         Sr : Ureal;
         Si : Uint;

      begin
         --  First we get the (exact result) in units of small

         Sr := Expr_Value_R (E1) / Small_Value (C_Type);

         --  Now round that exactly to an integer

         Si := UR_To_Uint (Sr);

         --  Finally the result is obtained by converting back to real

         Fold_Ureal (N, Si * Small_Value (C_Type), Static);
      end Round;

      --------------
      -- Rounding --
      --------------

      when Attribute_Rounding =>
         Fold_Ureal
           (N, Eval_Fat.Rounding (P_Base_Type, Expr_Value_R (E1)), Static);

      ---------------
      -- Safe_Emax --
      ---------------

      when Attribute_Safe_Emax =>
         Fold_Uint (N, Safe_Emax_Value (P_Type), Static);

      ----------------
      -- Safe_First --
      ----------------

      when Attribute_Safe_First =>
         Fold_Ureal (N, Safe_First_Value (P_Type), Static);

      ----------------
      -- Safe_Large --
      ----------------

      when Attribute_Safe_Large =>
         if Is_Fixed_Point_Type (P_Type) then
            Fold_Ureal
              (N, Expr_Value_R (Type_High_Bound (P_Base_Type)), Static);
         else
            Fold_Ureal (N, Safe_Last_Value (P_Type), Static);
         end if;

      ---------------
      -- Safe_Last --
      ---------------

      when Attribute_Safe_Last =>
         Fold_Ureal (N, Safe_Last_Value (P_Type), Static);

      ----------------
      -- Safe_Small --
      ----------------

      when Attribute_Safe_Small =>

         --  In Ada 95, the old Ada 83 attribute Safe_Small is redundant
         --  for fixed-point, since is the same as Small, but we implement
         --  it for backwards compatibility.

         if Is_Fixed_Point_Type (P_Type) then
            Fold_Ureal (N, Small_Value (P_Type), Static);

         --  Ada 83 Safe_Small for floating-point cases

         else
            Fold_Ureal (N, Model_Small_Value (P_Type), Static);
         end if;

      -----------
      -- Scale --
      -----------

      when Attribute_Scale =>
         Fold_Uint (N, Scale_Value (P_Type), Static);

      -------------
      -- Scaling --
      -------------

      when Attribute_Scaling =>
         Fold_Ureal
           (N,
            Eval_Fat.Scaling
              (P_Base_Type, Expr_Value_R (E1), Expr_Value (E2)),
            Static);

      ------------------
      -- Signed_Zeros --
      ------------------

      when Attribute_Signed_Zeros =>
         Fold_Uint
           (N, UI_From_Int (Boolean'Pos (Has_Signed_Zeros (P_Type))), Static);

      ----------
      -- Size --
      ----------

      --  Size attribute returns the RM size. All scalar types can be folded,
      --  as well as any types for which the size is known by the front end,
      --  including any type for which a size attribute is specified. This is
      --  one of the places where it is annoying that a size of zero means two
      --  things (zero size for scalars, unspecified size for non-scalars).

      when Attribute_Size
         | Attribute_VADS_Size
      =>
         Size : declare
            P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

         begin
            if Is_Scalar_Type (P_TypeA)
              or else RM_Size (P_TypeA) /= Uint_0
            then
               --  VADS_Size case

               if Id = Attribute_VADS_Size or else Use_VADS_Size then
                  declare
                     S : constant Node_Id := Size_Clause (P_TypeA);

                  begin
                     --  If a size clause applies, then use the size from it.
                     --  This is one of the rare cases where we can use the
                     --  Size_Clause field for a subtype when Has_Size_Clause
                     --  is False. Consider:

                     --    type x is range 1 .. 64;
                     --    for x'size use 12;
                     --    subtype y is x range 0 .. 3;

                     --  Here y has a size clause inherited from x, but
                     --  normally it does not apply, and y'size is 2. However,
                     --  y'VADS_Size is indeed 12 and not 2.

                     if Present (S)
                       and then Is_OK_Static_Expression (Expression (S))
                     then
                        Fold_Uint (N, Expr_Value (Expression (S)), Static);

                     --  If no size is specified, then we simply use the object
                     --  size in the VADS_Size case (e.g. Natural'Size is equal
                     --  to Integer'Size, not one less).

                     else
                        Fold_Uint (N, Esize (P_TypeA), Static);
                     end if;
                  end;

               --  Normal case (Size) in which case we want the RM_Size

               else
                  Fold_Uint (N, RM_Size (P_TypeA), Static);
               end if;
            end if;
         end Size;

      -----------
      -- Small --
      -----------

      when Attribute_Small =>

         --  The floating-point case is present only for Ada 83 compatibility.
         --  Note that strictly this is an illegal addition, since we are
         --  extending an Ada 95 defined attribute, but we anticipate an
         --  ARG ruling that will permit this.

         if Is_Floating_Point_Type (P_Type) then

            --  Ada 83 attribute is defined as (RM83 3.5.8)

            --    T'Small = 2.0**(-T'Emax - 1)

            --  where

            --    T'Emax = 4 * T'Mantissa

            Fold_Ureal (N, Ureal_2 ** ((-(4 * Mantissa)) - 1), Static);

         --  Normal Ada 95 fixed-point case

         else
            Fold_Ureal (N, Small_Value (P_Type), True);
         end if;

      -----------------
      -- Stream_Size --
      -----------------

      when Attribute_Stream_Size =>
         null;

      ----------
      -- Succ --
      ----------

      when Attribute_Succ =>
         --  Floating-point case

         if Is_Floating_Point_Type (P_Type) then
            Fold_Ureal
              (N, Eval_Fat.Succ (P_Base_Type, Expr_Value_R (E1)), Static);

         --  Fixed-point case

         elsif Is_Fixed_Point_Type (P_Type) then
            Fold_Ureal (N, Expr_Value_R (E1) + Small_Value (P_Type), Static);

         --  Modular integer case (wraps)

         elsif Is_Modular_Integer_Type (P_Type) then
            Fold_Uint (N, (Expr_Value (E1) + 1) mod Modulus (P_Type), Static);

         --  Other scalar cases

         else
            pragma Assert (Is_Scalar_Type (P_Type));

            if Is_Enumeration_Type (P_Type)
              and then Expr_Value (E1) =
                         Expr_Value (Type_High_Bound (P_Base_Type))
            then
               Apply_Compile_Time_Constraint_Error
                 (N, "Succ of `&''Last`",
                  CE_Overflow_Check_Failed,
                  Ent  => P_Base_Type,
                  Warn => not Static);

               Check_Expressions;
               return;
            else
               Fold_Uint (N, Expr_Value (E1) + 1, Static);
            end if;
         end if;

      ----------------
      -- Truncation --
      ----------------

      when Attribute_Truncation =>
         Fold_Ureal
           (N,
            Eval_Fat.Truncation (P_Base_Type, Expr_Value_R (E1)),
            Static);

      ----------------
      -- Type_Class --
      ----------------

      when Attribute_Type_Class => Type_Class : declare
         Typ : constant Entity_Id := Underlying_Type (P_Base_Type);
         Id  : RE_Id;

      begin
         if Is_Descendant_Of_Address (Typ) then
            Id := RE_Type_Class_Address;

         elsif Is_Enumeration_Type (Typ) then
            Id := RE_Type_Class_Enumeration;

         elsif Is_Integer_Type (Typ) then
            Id := RE_Type_Class_Integer;

         elsif Is_Fixed_Point_Type (Typ) then
            Id := RE_Type_Class_Fixed_Point;

         elsif Is_Floating_Point_Type (Typ) then
            Id := RE_Type_Class_Floating_Point;

         elsif Is_Array_Type (Typ) then
            Id := RE_Type_Class_Array;

         elsif Is_Record_Type (Typ) then
            Id := RE_Type_Class_Record;

         elsif Is_Access_Type (Typ) then
            Id := RE_Type_Class_Access;

         elsif Is_Task_Type (Typ) then
            Id := RE_Type_Class_Task;

         --  We treat protected types like task types. It would make more
         --  sense to have another enumeration value, but after all the
         --  whole point of this feature is to be exactly DEC compatible,
         --  and changing the type Type_Class would not meet this requirement.

         elsif Is_Protected_Type (Typ) then
            Id := RE_Type_Class_Task;

         --  Not clear if there are any other possibilities, but if there
         --  are, then we will treat them as the address case.

         else
            Id := RE_Type_Class_Address;
         end if;

         Rewrite (N, New_Occurrence_Of (RTE (Id), Loc));
      end Type_Class;

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      when Attribute_Unbiased_Rounding =>
         Fold_Ureal
           (N,
            Eval_Fat.Unbiased_Rounding (P_Base_Type, Expr_Value_R (E1)),
            Static);

      -------------------------
      -- Unconstrained_Array --
      -------------------------

      when Attribute_Unconstrained_Array => Unconstrained_Array : declare
         Typ : constant Entity_Id := Underlying_Type (P_Type);

      begin
         Rewrite (N, New_Occurrence_Of (
           Boolean_Literals (
             Is_Array_Type (P_Type)
              and then not Is_Constrained (Typ)), Loc));

         --  Analyze and resolve as boolean, note that this attribute is
         --  a static attribute in GNAT.

         Analyze_And_Resolve (N, Standard_Boolean);
         Static := True;
         Set_Is_Static_Expression (N, True);
      end Unconstrained_Array;

      --  Attribute Update is never static

      when Attribute_Update =>
         return;

      ---------------
      -- VADS_Size --
      ---------------

      --  Processing is shared with Size

      ---------
      -- Val --
      ---------

      when Attribute_Val =>
         if  Expr_Value (E1) < Expr_Value (Type_Low_Bound (P_Base_Type))
           or else
             Expr_Value (E1) > Expr_Value (Type_High_Bound (P_Base_Type))
         then
            Apply_Compile_Time_Constraint_Error
              (N, "Val expression out of range",
               CE_Range_Check_Failed,
               Warn => not Static);

            Check_Expressions;
            return;

         else
            Fold_Uint (N, Expr_Value (E1), Static);
         end if;

      ----------------
      -- Value_Size --
      ----------------

      --  The Value_Size attribute for a type returns the RM size of the type.
      --  This an always be folded for scalar types, and can also be folded for
      --  non-scalar types if the size is set. This is one of the places where
      --  it is annoying that a size of zero means two things!

      when Attribute_Value_Size => Value_Size : declare
         P_TypeA : constant Entity_Id := Underlying_Type (P_Type);

      begin
         if Is_Scalar_Type (P_TypeA) or else RM_Size (P_TypeA) /= Uint_0 then
            Fold_Uint (N, RM_Size (P_TypeA), Static);
         end if;
      end Value_Size;

      -------------
      -- Version --
      -------------

      --  Version can never be static

      when Attribute_Version =>
         null;

      ----------------
      -- Wide_Image --
      ----------------

      --  Wide_Image is a scalar attribute, but is never static, because it
      --  is not a static function (having a non-scalar argument (RM 4.9(22))

      when Attribute_Wide_Image =>
         null;

      ---------------------
      -- Wide_Wide_Image --
      ---------------------

      --  Wide_Wide_Image is a scalar attribute but is never static, because it
      --  is not a static function (having a non-scalar argument (RM 4.9(22)).

      when Attribute_Wide_Wide_Image =>
         null;

      ---------------------
      -- Wide_Wide_Width --
      ---------------------

      --  Processing for Wide_Wide_Width is combined with Width

      ----------------
      -- Wide_Width --
      ----------------

      --  Processing for Wide_Width is combined with Width

      -----------
      -- Width --
      -----------

      --  This processing also handles the case of Wide_[Wide_]Width

      when Attribute_Width
         | Attribute_Wide_Width
         | Attribute_Wide_Wide_Width
      =>
         if Compile_Time_Known_Bounds (P_Type) then

            --  Floating-point types

            if Is_Floating_Point_Type (P_Type) then

               --  Width is zero for a null range (RM 3.5 (38))

               if Expr_Value_R (Type_High_Bound (P_Type)) <
                  Expr_Value_R (Type_Low_Bound (P_Type))
               then
                  Fold_Uint (N, Uint_0, Static);

               else
                  --  For floating-point, we have +N.dddE+nnn where length
                  --  of ddd is determined by type'Digits - 1, but is one
                  --  if Digits is one (RM 3.5 (33)).

                  --  nnn is set to 2 for Short_Float and Float (32 bit
                  --  floats), and 3 for Long_Float and Long_Long_Float.
                  --  For machines where Long_Long_Float is the IEEE
                  --  extended precision type, the exponent takes 4 digits.

                  declare
                     Len : Int :=
                             Int'Max (2, UI_To_Int (Digits_Value (P_Type)));

                  begin
                     if Esize (P_Type) <= 32 then
                        Len := Len + 6;
                     elsif Esize (P_Type) = 64 then
                        Len := Len + 7;
                     else
                        Len := Len + 8;
                     end if;

                     Fold_Uint (N, UI_From_Int (Len), Static);
                  end;
               end if;

            --  Fixed-point types

            elsif Is_Fixed_Point_Type (P_Type) then

               --  Width is zero for a null range (RM 3.5 (38))

               if Expr_Value (Type_High_Bound (P_Type)) <
                  Expr_Value (Type_Low_Bound  (P_Type))
               then
                  Fold_Uint (N, Uint_0, Static);

               --  The non-null case depends on the specific real type

               else
                  --  For fixed-point type width is Fore + 1 + Aft (RM 3.5(34))

                  Fold_Uint
                    (N, UI_From_Int (Fore_Value + 1) + Aft_Value (P_Type),
                     Static);
               end if;

            --  Discrete types

            else
               declare
                  R  : constant Entity_Id := Root_Type (P_Type);
                  Lo : constant Uint := Expr_Value (Type_Low_Bound (P_Type));
                  Hi : constant Uint := Expr_Value (Type_High_Bound (P_Type));
                  W  : Nat;
                  Wt : Nat;
                  T  : Uint;
                  L  : Node_Id;
                  C  : Character;

               begin
                  --  Empty ranges

                  if Lo > Hi then
                     W := 0;

                  --  Width for types derived from Standard.Character
                  --  and Standard.Wide_[Wide_]Character.

                  elsif Is_Standard_Character_Type (P_Type) then
                     W := 0;

                     --  Set W larger if needed

                     for J in UI_To_Int (Lo) .. UI_To_Int (Hi) loop

                        --  All wide characters look like Hex_hhhhhhhh

                        if J > 255 then

                           --  No need to compute this more than once

                           exit;

                        else
                           C := Character'Val (J);

                           --  Test for all cases where Character'Image
                           --  yields an image that is longer than three
                           --  characters. First the cases of Reserved_xxx
                           --  names (length = 12).

                           case C is
                              when Reserved_128
                                 | Reserved_129
                                 | Reserved_132
                                 | Reserved_153
                              =>
                                 Wt := 12;

                              when BS
                                 | CR
                                 | EM
                                 | FF
                                 | FS
                                 | GS
                                 | HT
                                 | LF
                                 | MW
                                 | PM
                                 | RI
                                 | RS
                                 | SI
                                 | SO
                                 | ST
                                 | US
                                 | VT
                              =>
                                 Wt := 2;

                              when ACK
                                 | APC
                                 | BEL
                                 | BPH
                                 | CAN
                                 | CCH
                                 | CSI
                                 | DC1
                                 | DC2
                                 | DC3
                                 | DC4
                                 | DCS
                                 | DEL
                                 | DLE
                                 | ENQ
                                 | EOT
                                 | EPA
                                 | ESA
                                 | ESC
                                 | ETB
                                 | ETX
                                 | HTJ
                                 | HTS
                                 | NAK
                                 | NBH
                                 | NEL
                                 | NUL
                                 | OSC
                                 | PLD
                                 | PLU
                                 | PU1
                                 | PU2
                                 | SCI
                                 | SOH
                                 | SOS
                                 | SPA
                                 | SS2
                                 | SS3
                                 | SSA
                                 | STS
                                 | STX
                                 | SUB
                                 | SYN
                                 | VTS
                              =>
                                 Wt := 3;

                              when Space .. Tilde
                                 | No_Break_Space .. LC_Y_Diaeresis
                              =>
                                 --  Special case of soft hyphen in Ada 2005

                                 if C = Character'Val (16#AD#)
                                   and then Ada_Version >= Ada_2005
                                 then
                                    Wt := 11;
                                 else
                                    Wt := 3;
                                 end if;
                           end case;

                           W := Int'Max (W, Wt);
                        end if;
                     end loop;

                  --  Width for types derived from Standard.Boolean

                  elsif R = Standard_Boolean then
                     if Lo = 0 then
                        W := 5; -- FALSE
                     else
                        W := 4; -- TRUE
                     end if;

                  --  Width for integer types

                  elsif Is_Integer_Type (P_Type) then
                     T := UI_Max (abs Lo, abs Hi);

                     W := 2;
                     while T >= 10 loop
                        W := W + 1;
                        T := T / 10;
                     end loop;

                  --  User declared enum type with discard names

                  elsif Discard_Names (R) then

                     --  If range is null, result is zero, that has already
                     --  been dealt with, so what we need is the power of ten
                     --  that accommodates the Pos of the largest value, which
                     --  is the high bound of the range + one for the space.

                     W := 1;
                     T := Hi;
                     while T /= 0 loop
                        T := T / 10;
                        W := W + 1;
                     end loop;

                  --  Only remaining possibility is user declared enum type
                  --  with normal case of Discard_Names not active.

                  else
                     pragma Assert (Is_Enumeration_Type (P_Type));

                     W := 0;
                     L := First_Literal (P_Type);
                     while Present (L) loop

                        --  Only pay attention to in range characters

                        if Lo <= Enumeration_Pos (L)
                          and then Enumeration_Pos (L) <= Hi
                        then
                           --  For Width case, use decoded name

                           if Id = Attribute_Width then
                              Get_Decoded_Name_String (Chars (L));
                              Wt := Nat (Name_Len);

                           --  For Wide_[Wide_]Width, use encoded name, and
                           --  then adjust for the encoding.

                           else
                              Get_Name_String (Chars (L));

                              --  Character literals are always of length 3

                              if Name_Buffer (1) = 'Q' then
                                 Wt := 3;

                              --  Otherwise loop to adjust for upper/wide chars

                              else
                                 Wt := Nat (Name_Len);

                                 for J in 1 .. Name_Len loop
                                    if Name_Buffer (J) = 'U' then
                                       Wt := Wt - 2;
                                    elsif Name_Buffer (J) = 'W' then
                                       Wt := Wt - 4;
                                    end if;
                                 end loop;
                              end if;
                           end if;

                           W := Int'Max (W, Wt);
                        end if;

                        Next_Literal (L);
                     end loop;
                  end if;

                  Fold_Uint (N, UI_From_Int (W), Static);
               end;
            end if;
         end if;

      --  The following attributes denote functions that cannot be folded

      when Attribute_From_Any
         | Attribute_To_Any
         | Attribute_TypeCode
      =>
         null;

      --  The following attributes can never be folded, and furthermore we
      --  should not even have entered the case statement for any of these.
      --  Note that in some cases, the values have already been folded as
      --  a result of the processing in Analyze_Attribute or earlier in
      --  this procedure.

      when Attribute_Abort_Signal
         | Attribute_Access
         | Attribute_Address
         | Attribute_Address_Size
         | Attribute_Asm_Input
         | Attribute_Asm_Output
         | Attribute_Base
         | Attribute_Bit_Order
         | Attribute_Bit_Position
         | Attribute_Callable
         | Attribute_Caller
         | Attribute_Class
         | Attribute_Code_Address
         | Attribute_Compiler_Version
         | Attribute_Count
         | Attribute_Default_Bit_Order
         | Attribute_Default_Scalar_Storage_Order
         | Attribute_Deref
         | Attribute_Elaborated
         | Attribute_Elab_Body
         | Attribute_Elab_Spec
         | Attribute_Elab_Subp_Body
         | Attribute_Enabled
         | Attribute_External_Tag
         | Attribute_Fast_Math
         | Attribute_First_Bit
         | Attribute_Img
         | Attribute_Input
         | Attribute_Last_Bit
         | Attribute_Library_Level
         | Attribute_Maximum_Alignment
         | Attribute_Old
         | Attribute_Output
         | Attribute_Partition_ID
         | Attribute_Pool_Address
         | Attribute_Position
         | Attribute_Priority
         | Attribute_Read
         | Attribute_Result
         | Attribute_Scalar_Storage_Order
         | Attribute_Simple_Storage_Pool
         | Attribute_Storage_Pool
         | Attribute_Storage_Size
         | Attribute_Storage_Unit
         | Attribute_Stub_Type
         | Attribute_System_Allocator_Alignment
         | Attribute_Tag
         | Attribute_Target_Name
         | Attribute_Terminated
         | Attribute_To_Address
         | Attribute_Type_Key
         | Attribute_Unchecked_Access
         | Attribute_Universal_Literal_String
         | Attribute_Unrestricted_Access
         | Attribute_Valid
         | Attribute_Valid_Scalars
         | Attribute_Value
         | Attribute_Wchar_T_Size
         | Attribute_Wide_Value
         | Attribute_Wide_Wide_Value
         | Attribute_Word_Size
         | Attribute_Write
      =>
         raise Program_Error;
      end case;

      --  At the end of the case, one more check. If we did a static evaluation
      --  so that the result is now a literal, then set Is_Static_Expression
      --  in the constant only if the prefix type is a static subtype. For
      --  non-static subtypes, the folding is still OK, but not static.

      --  An exception is the GNAT attribute Constrained_Array which is
      --  defined to be a static attribute in all cases.

      if Nkind_In (N, N_Integer_Literal,
                      N_Real_Literal,
                      N_Character_Literal,
                      N_String_Literal)
        or else (Is_Entity_Name (N)
                  and then Ekind (Entity (N)) = E_Enumeration_Literal)
      then
         Set_Is_Static_Expression (N, Static);

      --  If this is still an attribute reference, then it has not been folded
      --  and that means that its expressions are in a non-static context.

      elsif Nkind (N) = N_Attribute_Reference then
         Check_Expressions;

      --  Note: the else case not covered here are odd cases where the
      --  processing has transformed the attribute into something other
      --  than a constant. Nothing more to do in such cases.

      else
         null;
      end if;
   end Eval_Attribute;

   ------------------------------
   -- Is_Anonymous_Tagged_Base --
   ------------------------------

   function Is_Anonymous_Tagged_Base
     (Anon : Entity_Id;
      Typ  : Entity_Id) return Boolean
   is
   begin
      return
        Anon = Current_Scope
          and then Is_Itype (Anon)
          and then Associated_Node_For_Itype (Anon) = Parent (Typ);
   end Is_Anonymous_Tagged_Base;

   --------------------------------
   -- Name_Implies_Lvalue_Prefix --
   --------------------------------

   function Name_Implies_Lvalue_Prefix (Nam : Name_Id) return Boolean is
      pragma Assert (Is_Attribute_Name (Nam));
   begin
      return Attribute_Name_Implies_Lvalue_Prefix (Get_Attribute_Id (Nam));
   end Name_Implies_Lvalue_Prefix;

   -----------------------
   -- Resolve_Attribute --
   -----------------------

   procedure Resolve_Attribute (N : Node_Id; Typ : Entity_Id) is
      Loc      : constant Source_Ptr   := Sloc (N);
      P        : constant Node_Id      := Prefix (N);
      Aname    : constant Name_Id      := Attribute_Name (N);
      Attr_Id  : constant Attribute_Id := Get_Attribute_Id (Aname);
      Btyp     : constant Entity_Id    := Base_Type (Typ);
      Des_Btyp : Entity_Id;
      Index    : Interp_Index;
      It       : Interp;
      Nom_Subt : Entity_Id;

      procedure Accessibility_Message;
      --  Error, or warning within an instance, if the static accessibility
      --  rules of 3.10.2 are violated.

      function Declared_Within_Generic_Unit
        (Entity       : Entity_Id;
         Generic_Unit : Node_Id) return Boolean;
      --  Returns True if Declared_Entity is declared within the declarative
      --  region of Generic_Unit; otherwise returns False.

      ---------------------------
      -- Accessibility_Message --
      ---------------------------

      procedure Accessibility_Message is
         Indic : Node_Id := Parent (Parent (N));

      begin
         --  In an instance, this is a runtime check, but one we
         --  know will fail, so generate an appropriate warning.

         if In_Instance_Body then
            Error_Msg_Warn := SPARK_Mode /= On;
            Error_Msg_F
              ("non-local pointer cannot point to local object<<", P);
            Error_Msg_F ("\Program_Error [<<", P);
            Rewrite (N,
              Make_Raise_Program_Error (Loc,
                Reason => PE_Accessibility_Check_Failed));
            Set_Etype (N, Typ);
            return;

         else
            Error_Msg_F ("non-local pointer cannot point to local object", P);

            --  Check for case where we have a missing access definition

            if Is_Record_Type (Current_Scope)
              and then
                Nkind_In (Parent (N), N_Discriminant_Association,
                                      N_Index_Or_Discriminant_Constraint)
            then
               Indic := Parent (Parent (N));
               while Present (Indic)
                 and then Nkind (Indic) /= N_Subtype_Indication
               loop
                  Indic := Parent (Indic);
               end loop;

               if Present (Indic) then
                  Error_Msg_NE
                    ("\use an access definition for" &
                     " the access discriminant of&",
                     N, Entity (Subtype_Mark (Indic)));
               end if;
            end if;
         end if;
      end Accessibility_Message;

      ----------------------------------
      -- Declared_Within_Generic_Unit --
      ----------------------------------

      function Declared_Within_Generic_Unit
        (Entity       : Entity_Id;
         Generic_Unit : Node_Id) return Boolean
      is
         Generic_Encloser : Node_Id := Enclosing_Generic_Unit (Entity);

      begin
         while Present (Generic_Encloser) loop
            if Generic_Encloser = Generic_Unit then
               return True;
            end if;

            --  We have to step to the scope of the generic's entity, because
            --  otherwise we'll just get back the same generic.

            Generic_Encloser :=
              Enclosing_Generic_Unit
                (Scope (Defining_Entity (Generic_Encloser)));
         end loop;

         return False;
      end Declared_Within_Generic_Unit;

   --  Start of processing for Resolve_Attribute

   begin
      --  If error during analysis, no point in continuing, except for array
      --  types, where we get better recovery by using unconstrained indexes
      --  than nothing at all (see Check_Array_Type).

      if Error_Posted (N)
        and then Attr_Id /= Attribute_First
        and then Attr_Id /= Attribute_Last
        and then Attr_Id /= Attribute_Length
        and then Attr_Id /= Attribute_Range
      then
         return;
      end if;

      --  If attribute was universal type, reset to actual type

      if Etype (N) = Universal_Integer
        or else Etype (N) = Universal_Real
      then
         Set_Etype (N, Typ);
      end if;

      --  Remaining processing depends on attribute

      case Attr_Id is

         ------------
         -- Access --
         ------------

         --  For access attributes, if the prefix denotes an entity, it is
         --  interpreted as a name, never as a call. It may be overloaded,
         --  in which case resolution uses the profile of the context type.
         --  Otherwise prefix must be resolved.

         when Attribute_Access
            | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access
         =>
            --  Note possible modification if we have a variable

            if Is_Variable (P) then
               declare
                  PN : constant Node_Id := Parent (N);
                  Nm : Node_Id;

                  Note : Boolean := True;
                  --  Skip this for the case of Unrestricted_Access occuring in
                  --  the context of a Valid check, since this otherwise leads
                  --  to a missed warning (the Valid check does not really
                  --  modify!) If this case, Note will be reset to False.

                  --  Skip it as well if the type is an Acccess_To_Constant,
                  --  given that no use of the value can modify the prefix.

               begin
                  if Attr_Id = Attribute_Unrestricted_Access
                    and then Nkind (PN) = N_Function_Call
                  then
                     Nm := Name (PN);

                     if Nkind (Nm) = N_Expanded_Name
                       and then Chars (Nm) = Name_Valid
                       and then Nkind (Prefix (Nm)) = N_Identifier
                       and then Chars (Prefix (Nm)) = Name_Attr_Long_Float
                     then
                        Note := False;
                     end if;

                  elsif Is_Access_Constant (Typ) then
                     Note := False;
                  end if;

                  if Note then
                     Note_Possible_Modification (P, Sure => False);
                  end if;
               end;
            end if;

            --  The following comes from a query concerning improper use of
            --  universal_access in equality tests involving anonymous access
            --  types. Another good reason for 'Ref, but for now disable the
            --  test, which breaks several filed tests???

            if Ekind (Typ) = E_Anonymous_Access_Type
              and then Nkind_In (Parent (N), N_Op_Eq, N_Op_Ne)
              and then False
            then
               Error_Msg_N ("need unique type to resolve 'Access", N);
               Error_Msg_N ("\qualify attribute with some access type", N);
            end if;

            --  Case where prefix is an entity name

            if Is_Entity_Name (P) then

               --  Deal with case where prefix itself is overloaded

               if Is_Overloaded (P) then
                  Get_First_Interp (P, Index, It);
                  while Present (It.Nam) loop
                     if Type_Conformant (Designated_Type (Typ), It.Nam) then
                        Set_Entity (P, It.Nam);

                        --  The prefix is definitely NOT overloaded anymore at
                        --  this point, so we reset the Is_Overloaded flag to
                        --  avoid any confusion when reanalyzing the node.

                        Set_Is_Overloaded (P, False);
                        Set_Is_Overloaded (N, False);
                        Generate_Reference (Entity (P), P);
                        exit;
                     end if;

                     Get_Next_Interp (Index, It);
                  end loop;

                  --  If Prefix is a subprogram name, this reference freezes,
                  --  but not if within spec expression mode. The profile of
                  --  the subprogram is not frozen at this point.

                  if not In_Spec_Expression then
                     Freeze_Before (N, Entity (P), Do_Freeze_Profile => False);
                  end if;

               --  If it is a type, there is nothing to resolve.
               --  If it is a subprogram, do not freeze its profile.
               --  If it is an object, complete its resolution.

               elsif Is_Overloadable (Entity (P)) then
                  if not In_Spec_Expression then
                     Freeze_Before (N, Entity (P), Do_Freeze_Profile => False);
                  end if;

               --  Nothing to do if prefix is a type name

               elsif Is_Type (Entity (P)) then
                  null;

               --  Otherwise non-overloaded other case, resolve the prefix

               else
                  Resolve (P);
               end if;

               --  Some further error checks

               Error_Msg_Name_1 := Aname;

               if not Is_Entity_Name (P) then
                  null;

               elsif Is_Overloadable (Entity (P))
                 and then Is_Abstract_Subprogram (Entity (P))
               then
                  Error_Msg_F ("prefix of % attribute cannot be abstract", P);
                  Set_Etype (N, Any_Type);

               elsif Ekind (Entity (P)) = E_Enumeration_Literal then
                  Error_Msg_F
                    ("prefix of % attribute cannot be enumeration literal", P);
                  Set_Etype (N, Any_Type);

               --  An attempt to take 'Access of a function that renames an
               --  enumeration literal. Issue a specialized error message.

               elsif Ekind (Entity (P)) = E_Function
                 and then Present (Alias (Entity (P)))
                 and then Ekind (Alias (Entity (P))) = E_Enumeration_Literal
               then
                  Error_Msg_F
                    ("prefix of % attribute cannot be function renaming "
                     & "an enumeration literal", P);
                  Set_Etype (N, Any_Type);

               elsif Convention (Entity (P)) = Convention_Intrinsic then
                  Error_Msg_F ("prefix of % attribute cannot be intrinsic", P);
                  Set_Etype (N, Any_Type);
               end if;

               --  Assignments, return statements, components of aggregates,
               --  generic instantiations will require convention checks if
               --  the type is an access to subprogram. Given that there will
               --  also be accessibility checks on those, this is where the
               --  checks can eventually be centralized ???

               if Ekind_In (Btyp, E_Access_Protected_Subprogram_Type,
                                  E_Access_Subprogram_Type,
                                  E_Anonymous_Access_Protected_Subprogram_Type,
                                  E_Anonymous_Access_Subprogram_Type)
               then
                  --  Deal with convention mismatch

                  if Convention (Designated_Type (Btyp)) /=
                     Convention (Entity (P))
                  then
                     --  The rule in 6.3.1 (8) deserves a special error
                     --  message.

                     if Convention (Btyp) = Convention_Intrinsic
                       and then Nkind (Parent (N)) = N_Procedure_Call_Statement
                       and then Is_Entity_Name (Name (Parent (N)))
                       and then Inside_A_Generic
                     then
                        declare
                           Subp : constant Entity_Id :=
                                    Entity (Name (Parent (N)));
                        begin
                           if Convention (Subp) = Convention_Intrinsic then
                              Error_Msg_FE
                                ("?subprogram and its formal access "
                                 & "parameters have convention Intrinsic",
                                 Parent (N), Subp);
                              Error_Msg_N
                                ("actual cannot be access attribute", N);
                           end if;
                        end;

                     else
                        Error_Msg_FE
                          ("subprogram & has wrong convention", P, Entity (P));
                        Error_Msg_Sloc := Sloc (Btyp);
                        Error_Msg_FE ("\does not match & declared#", P, Btyp);
                     end if;

                     if not Is_Itype (Btyp)
                       and then not Has_Convention_Pragma (Btyp)
                     then
                        Error_Msg_FE
                          ("\probable missing pragma Convention for &",
                           P, Btyp);
                     end if;

                  else
                     Check_Subtype_Conformant
                       (New_Id  => Entity (P),
                        Old_Id  => Designated_Type (Btyp),
                        Err_Loc => P);
                  end if;

                  if Attr_Id = Attribute_Unchecked_Access then
                     Error_Msg_Name_1 := Aname;
                     Error_Msg_F
                       ("attribute% cannot be applied to a subprogram", P);

                  elsif Aname = Name_Unrestricted_Access then
                     null;  --  Nothing to check

                  --  Check the static accessibility rule of 3.10.2(32).
                  --  This rule also applies within the private part of an
                  --  instantiation. This rule does not apply to anonymous
                  --  access-to-subprogram types in access parameters.

                  elsif Attr_Id = Attribute_Access
                    and then not In_Instance_Body
                    and then
                      (Ekind (Btyp) = E_Access_Subprogram_Type
                        or else Is_Local_Anonymous_Access (Btyp))
                    and then Subprogram_Access_Level (Entity (P)) >
                               Type_Access_Level (Btyp)
                  then
                     Error_Msg_F
                       ("subprogram must not be deeper than access type", P);

                  --  Check the restriction of 3.10.2(32) that disallows the
                  --  access attribute within a generic body when the ultimate
                  --  ancestor of the type of the attribute is declared outside
                  --  of the generic unit and the subprogram is declared within
                  --  that generic unit. This includes any such attribute that
                  --  occurs within the body of a generic unit that is a child
                  --  of the generic unit where the subprogram is declared.

                  --  The rule also prohibits applying the attribute when the
                  --  access type is a generic formal access type (since the
                  --  level of the actual type is not known). This restriction
                  --  does not apply when the attribute type is an anonymous
                  --  access-to-subprogram type. Note that this check was
                  --  revised by AI-229, because the original Ada 95 rule
                  --  was too lax. The original rule only applied when the
                  --  subprogram was declared within the body of the generic,
                  --  which allowed the possibility of dangling references).
                  --  The rule was also too strict in some cases, in that it
                  --  didn't permit the access to be declared in the generic
                  --  spec, whereas the revised rule does (as long as it's not
                  --  a formal type).

                  --  There are a couple of subtleties of the test for applying
                  --  the check that are worth noting. First, we only apply it
                  --  when the levels of the subprogram and access type are the
                  --  same (the case where the subprogram is statically deeper
                  --  was applied above, and the case where the type is deeper
                  --  is always safe). Second, we want the check to apply
                  --  within nested generic bodies and generic child unit
                  --  bodies, but not to apply to an attribute that appears in
                  --  the generic unit's specification. This is done by testing
                  --  that the attribute's innermost enclosing generic body is
                  --  not the same as the innermost generic body enclosing the
                  --  generic unit where the subprogram is declared (we don't
                  --  want the check to apply when the access attribute is in
                  --  the spec and there's some other generic body enclosing
                  --  generic). Finally, there's no point applying the check
                  --  when within an instance, because any violations will have
                  --  been caught by the compilation of the generic unit.

                  --  We relax this check in Relaxed_RM_Semantics mode for
                  --  compatibility with legacy code for use by Ada source
                  --  code analyzers (e.g. CodePeer).

                  elsif Attr_Id = Attribute_Access
                    and then not Relaxed_RM_Semantics
                    and then not In_Instance
                    and then Present (Enclosing_Generic_Unit (Entity (P)))
                    and then Present (Enclosing_Generic_Body (N))
                    and then Enclosing_Generic_Body (N) /=
                               Enclosing_Generic_Body
                                 (Enclosing_Generic_Unit (Entity (P)))
                    and then Subprogram_Access_Level (Entity (P)) =
                               Type_Access_Level (Btyp)
                    and then Ekind (Btyp) /=
                               E_Anonymous_Access_Subprogram_Type
                    and then Ekind (Btyp) /=
                               E_Anonymous_Access_Protected_Subprogram_Type
                  then
                     --  The attribute type's ultimate ancestor must be
                     --  declared within the same generic unit as the
                     --  subprogram is declared (including within another
                     --  nested generic unit). The error message is
                     --  specialized to say "ancestor" for the case where the
                     --  access type is not its own ancestor, since saying
                     --  simply "access type" would be very confusing.

                     if not Declared_Within_Generic_Unit
                              (Root_Type (Btyp),
                               Enclosing_Generic_Unit (Entity (P)))
                     then
                        Error_Msg_N
                          ("''Access attribute not allowed in generic body",
                           N);

                        if Root_Type (Btyp) = Btyp then
                           Error_Msg_NE
                             ("\because " &
                              "access type & is declared outside " &
                              "generic unit (RM 3.10.2(32))", N, Btyp);
                        else
                           Error_Msg_NE
                             ("\because ancestor of " &
                              "access type & is declared outside " &
                              "generic unit (RM 3.10.2(32))", N, Btyp);
                        end if;

                        Error_Msg_NE
                          ("\move ''Access to private part, or " &
                           "(Ada 2005) use anonymous access type instead of &",
                           N, Btyp);

                     --  If the ultimate ancestor of the attribute's type is
                     --  a formal type, then the attribute is illegal because
                     --  the actual type might be declared at a higher level.
                     --  The error message is specialized to say "ancestor"
                     --  for the case where the access type is not its own
                     --  ancestor, since saying simply "access type" would be
                     --  very confusing.

                     elsif Is_Generic_Type (Root_Type (Btyp)) then
                        if Root_Type (Btyp) = Btyp then
                           Error_Msg_N
                             ("access type must not be a generic formal type",
                              N);
                        else
                           Error_Msg_N
                             ("ancestor access type must not be a generic " &
                              "formal type", N);
                        end if;
                     end if;
                  end if;
               end if;

               --  If this is a renaming, an inherited operation, or a
               --  subprogram instance, use the original entity. This may make
               --  the node type-inconsistent, so this transformation can only
               --  be done if the node will not be reanalyzed. In particular,
               --  if it is within a default expression, the transformation
               --  must be delayed until the default subprogram is created for
               --  it, when the enclosing subprogram is frozen.

               if Is_Entity_Name (P)
                 and then Is_Overloadable (Entity (P))
                 and then Present (Alias (Entity (P)))
                 and then Expander_Active
               then
                  Rewrite (P,
                    New_Occurrence_Of (Alias (Entity (P)), Sloc (P)));
               end if;

            elsif Nkind (P) = N_Selected_Component
              and then Is_Overloadable (Entity (Selector_Name (P)))
            then
               --  Protected operation. If operation is overloaded, must
               --  disambiguate. Prefix that denotes protected object itself
               --  is resolved with its own type.

               if Attr_Id = Attribute_Unchecked_Access then
                  Error_Msg_Name_1 := Aname;
                  Error_Msg_F
                    ("attribute% cannot be applied to protected operation", P);
               end if;

               Resolve (Prefix (P));
               Generate_Reference (Entity (Selector_Name (P)), P);

            --  Implement check implied by 3.10.2 (18.1/2) : F.all'access is
            --  statically illegal if F is an anonymous access to subprogram.

            elsif Nkind (P) = N_Explicit_Dereference
              and then Is_Entity_Name (Prefix (P))
              and then Ekind (Etype (Entity (Prefix  (P)))) =
                 E_Anonymous_Access_Subprogram_Type
            then
               Error_Msg_N ("anonymous access to subprogram "
                 &  "has deeper accessibility than any master", P);

            elsif Is_Overloaded (P) then

               --  Use the designated type of the context to disambiguate
               --  Note that this was not strictly conformant to Ada 95,
               --  but was the implementation adopted by most Ada 95 compilers.
               --  The use of the context type to resolve an Access attribute
               --  reference is now mandated in AI-235 for Ada 2005.

               declare
                  Index : Interp_Index;
                  It    : Interp;

               begin
                  Get_First_Interp (P, Index, It);
                  while Present (It.Typ) loop
                     if Covers (Designated_Type (Typ), It.Typ) then
                        Resolve (P, It.Typ);
                        exit;
                     end if;

                     Get_Next_Interp (Index, It);
                  end loop;
               end;
            else
               Resolve (P);
            end if;

            --  X'Access is illegal if X denotes a constant and the access type
            --  is access-to-variable. Same for 'Unchecked_Access. The rule
            --  does not apply to 'Unrestricted_Access. If the reference is a
            --  default-initialized aggregate component for a self-referential
            --  type the reference is legal.

            if not (Ekind (Btyp) = E_Access_Subprogram_Type
                     or else Ekind (Btyp) = E_Anonymous_Access_Subprogram_Type
                     or else (Is_Record_Type (Btyp)
                               and then
                                 Present (Corresponding_Remote_Type (Btyp)))
                     or else Ekind (Btyp) = E_Access_Protected_Subprogram_Type
                     or else Ekind (Btyp)
                               = E_Anonymous_Access_Protected_Subprogram_Type
                     or else Is_Access_Constant (Btyp)
                     or else Is_Variable (P)
                     or else Attr_Id = Attribute_Unrestricted_Access)
            then
               if Is_Entity_Name (P)
                 and then Is_Type (Entity (P))
               then
                  --  Legality of a self-reference through an access
                  --  attribute has been verified in Analyze_Access_Attribute.

                  null;

               elsif Comes_From_Source (N) then
                  Error_Msg_F ("access-to-variable designates constant", P);
               end if;
            end if;

            Des_Btyp := Designated_Type (Btyp);

            if Ada_Version >= Ada_2005
              and then Is_Incomplete_Type (Des_Btyp)
            then
               --  Ada 2005 (AI-412): If the (sub)type is a limited view of an
               --  imported entity, and the non-limited view is visible, make
               --  use of it. If it is an incomplete subtype, use the base type
               --  in any case.

               if From_Limited_With (Des_Btyp)
                 and then Present (Non_Limited_View (Des_Btyp))
               then
                  Des_Btyp := Non_Limited_View (Des_Btyp);

               elsif Ekind (Des_Btyp) = E_Incomplete_Subtype then
                  Des_Btyp := Etype (Des_Btyp);
               end if;
            end if;

            if (Attr_Id = Attribute_Access
                  or else
                Attr_Id = Attribute_Unchecked_Access)
              and then (Ekind (Btyp) = E_General_Access_Type
                         or else Ekind (Btyp) = E_Anonymous_Access_Type)
            then
               --  Ada 2005 (AI-230): Check the accessibility of anonymous
               --  access types for stand-alone objects, record and array
               --  components, and return objects. For a component definition
               --  the level is the same of the enclosing composite type.

               if Ada_Version >= Ada_2005
                 and then (Is_Local_Anonymous_Access (Btyp)

                            --  Handle cases where Btyp is the anonymous access
                            --  type of an Ada 2012 stand-alone object.

                            or else Nkind (Associated_Node_For_Itype (Btyp)) =
                                                        N_Object_Declaration)
                 and then
                   Object_Access_Level (P) > Deepest_Type_Access_Level (Btyp)
                 and then Attr_Id = Attribute_Access
               then
                  --  In an instance, this is a runtime check, but one we know
                  --  will fail, so generate an appropriate warning. As usual,
                  --  this kind of warning is an error in SPARK mode.

                  if In_Instance_Body then
                     Error_Msg_Warn := SPARK_Mode /= On;
                     Error_Msg_F
                       ("non-local pointer cannot point to local object<<", P);
                     Error_Msg_F ("\Program_Error [<<", P);

                     Rewrite (N,
                       Make_Raise_Program_Error (Loc,
                         Reason => PE_Accessibility_Check_Failed));
                     Set_Etype (N, Typ);

                  else
                     Error_Msg_F
                       ("non-local pointer cannot point to local object", P);
                  end if;
               end if;

               if Is_Dependent_Component_Of_Mutable_Object (P) then
                  Error_Msg_F
                    ("illegal attribute for discriminant-dependent component",
                     P);
               end if;

               --  Check static matching rule of 3.10.2(27). Nominal subtype
               --  of the prefix must statically match the designated type.

               Nom_Subt := Etype (P);

               if Is_Constr_Subt_For_U_Nominal (Nom_Subt) then
                  Nom_Subt := Base_Type (Nom_Subt);
               end if;

               if Is_Tagged_Type (Designated_Type (Typ)) then

                  --  If the attribute is in the context of an access
                  --  parameter, then the prefix is allowed to be of
                  --  the class-wide type (by AI-127).

                  if Ekind (Typ) = E_Anonymous_Access_Type then
                     if not Covers (Designated_Type (Typ), Nom_Subt)
                       and then not Covers (Nom_Subt, Designated_Type (Typ))
                     then
                        declare
                           Desig : Entity_Id;

                        begin
                           Desig := Designated_Type (Typ);

                           if Is_Class_Wide_Type (Desig) then
                              Desig := Etype (Desig);
                           end if;

                           if Is_Anonymous_Tagged_Base (Nom_Subt, Desig) then
                              null;

                           else
                              Error_Msg_FE
                                ("type of prefix: & not compatible",
                                  P, Nom_Subt);
                              Error_Msg_FE
                                ("\with &, the expected designated type",
                                  P, Designated_Type (Typ));
                           end if;
                        end;
                     end if;

                  elsif not Covers (Designated_Type (Typ), Nom_Subt)
                    or else
                      (not Is_Class_Wide_Type (Designated_Type (Typ))
                        and then Is_Class_Wide_Type (Nom_Subt))
                  then
                     Error_Msg_FE
                       ("type of prefix: & is not covered", P, Nom_Subt);
                     Error_Msg_FE
                       ("\by &, the expected designated type" &
                           " (RM 3.10.2 (27))", P, Designated_Type (Typ));
                  end if;

                  if Is_Class_Wide_Type (Designated_Type (Typ))
                    and then Has_Discriminants (Etype (Designated_Type (Typ)))
                    and then Is_Constrained (Etype (Designated_Type (Typ)))
                    and then Designated_Type (Typ) /= Nom_Subt
                  then
                     Apply_Discriminant_Check
                       (N, Etype (Designated_Type (Typ)));
                  end if;

               --  Ada 2005 (AI-363): Require static matching when designated
               --  type has discriminants and a constrained partial view, since
               --  in general objects of such types are mutable, so we can't
               --  allow the access value to designate a constrained object
               --  (because access values must be assumed to designate mutable
               --  objects when designated type does not impose a constraint).

               elsif Subtypes_Statically_Match (Des_Btyp, Nom_Subt) then
                  null;

               elsif Has_Discriminants (Designated_Type (Typ))
                 and then not Is_Constrained (Des_Btyp)
                 and then
                   (Ada_Version < Ada_2005
                     or else
                       not Object_Type_Has_Constrained_Partial_View
                             (Typ => Designated_Type (Base_Type (Typ)),
                              Scop => Current_Scope))
               then
                  null;

               else
                  Error_Msg_F
                    ("object subtype must statically match "
                     & "designated subtype", P);

                  if Is_Entity_Name (P)
                    and then Is_Array_Type (Designated_Type (Typ))
                  then
                     declare
                        D : constant Node_Id := Declaration_Node (Entity (P));
                     begin
                        Error_Msg_N
                          ("aliased object has explicit bounds??", D);
                        Error_Msg_N
                          ("\declare without bounds (and with explicit "
                           & "initialization)??", D);
                        Error_Msg_N
                          ("\for use with unconstrained access??", D);
                     end;
                  end if;
               end if;

               --  Check the static accessibility rule of 3.10.2(28). Note that
               --  this check is not performed for the case of an anonymous
               --  access type, since the access attribute is always legal
               --  in such a context.

               if Attr_Id /= Attribute_Unchecked_Access
                 and then Ekind (Btyp) = E_General_Access_Type
                 and then
                   Object_Access_Level (P) > Deepest_Type_Access_Level (Btyp)
               then
                  Accessibility_Message;
                  return;
               end if;
            end if;

            if Ekind_In (Btyp, E_Access_Protected_Subprogram_Type,
                               E_Anonymous_Access_Protected_Subprogram_Type)
            then
               if Is_Entity_Name (P)
                 and then not Is_Protected_Type (Scope (Entity (P)))
               then
                  Error_Msg_F ("context requires a protected subprogram", P);

               --  Check accessibility of protected object against that of the
               --  access type, but only on user code, because the expander
               --  creates access references for handlers. If the context is an
               --  anonymous_access_to_protected, there are no accessibility
               --  checks either. Omit check entirely for Unrestricted_Access.

               elsif Object_Access_Level (P) > Deepest_Type_Access_Level (Btyp)
                 and then Comes_From_Source (N)
                 and then Ekind (Btyp) = E_Access_Protected_Subprogram_Type
                 and then Attr_Id /= Attribute_Unrestricted_Access
               then
                  Accessibility_Message;
                  return;

               --  AI05-0225: If the context is not an access to protected
               --  function, the prefix must be a variable, given that it may
               --  be used subsequently in a protected call.

               elsif Nkind (P) = N_Selected_Component
                 and then not Is_Variable (Prefix (P))
                 and then Ekind (Entity (Selector_Name (P))) /= E_Function
               then
                  Error_Msg_N
                    ("target object of access to protected procedure "
                      & "must be variable", N);

               elsif Is_Entity_Name (P) then
                  Check_Internal_Protected_Use (N, Entity (P));
               end if;

            elsif Ekind_In (Btyp, E_Access_Subprogram_Type,
                                  E_Anonymous_Access_Subprogram_Type)
              and then Ekind (Etype (N)) = E_Access_Protected_Subprogram_Type
            then
               Error_Msg_F ("context requires a non-protected subprogram", P);
            end if;

            --  The context cannot be a pool-specific type, but this is a
            --  legality rule, not a resolution rule, so it must be checked
            --  separately, after possibly disambiguation (see AI-245).

            if Ekind (Btyp) = E_Access_Type
              and then Attr_Id /= Attribute_Unrestricted_Access
            then
               Wrong_Type (N, Typ);
            end if;

            --  The context may be a constrained access type (however ill-
            --  advised such subtypes might be) so in order to generate a
            --  constraint check when needed set the type of the attribute
            --  reference to the base type of the context.

            Set_Etype (N, Btyp);

            --  Check for incorrect atomic/volatile reference (RM C.6(12))

            if Attr_Id /= Attribute_Unrestricted_Access then
               if Is_Atomic_Object (P)
                 and then not Is_Atomic (Designated_Type (Typ))
               then
                  Error_Msg_F
                    ("access to atomic object cannot yield access-to-" &
                     "non-atomic type", P);

               elsif Is_Volatile_Object (P)
                 and then not Is_Volatile (Designated_Type (Typ))
               then
                  Error_Msg_F
                    ("access to volatile object cannot yield access-to-" &
                     "non-volatile type", P);
               end if;
            end if;

            --  Check for unrestricted access where expected type is a thin
            --  pointer to an unconstrained array.

            if Non_Aliased_Prefix (N)
              and then Has_Size_Clause (Typ)
              and then RM_Size (Typ) = System_Address_Size
            then
               declare
                  DT : constant Entity_Id := Designated_Type (Typ);
               begin
                  if Is_Array_Type (DT) and then not Is_Constrained (DT) then
                     Error_Msg_N
                       ("illegal use of Unrestricted_Access attribute", P);
                     Error_Msg_N
                       ("\attempt to generate thin pointer to unaliased "
                        & "object", P);
                  end if;
               end;
            end if;

            --  Mark that address of entity is taken in case of
            --  'Unrestricted_Access or in case of a subprogram.

            if Is_Entity_Name (P)
             and then (Attr_Id = Attribute_Unrestricted_Access
                        or else Is_Subprogram (Entity (P)))
            then
               Set_Address_Taken (Entity (P));
            end if;

            --  Deal with possible elaboration check

            if Is_Entity_Name (P) and then Is_Subprogram (Entity (P)) then
               declare
                  Subp_Id   : constant Entity_Id := Entity (P);
                  Scop      : constant Entity_Id := Scope (Subp_Id);
                  Subp_Decl : constant Node_Id   :=
                                Unit_Declaration_Node (Subp_Id);
                  Flag_Id   : Entity_Id;
                  Subp_Body : Node_Id;

               --  If the access has been taken and the body of the subprogram
               --  has not been see yet, indirect calls must be protected with
               --  elaboration checks. We have the proper elaboration machinery
               --  for subprograms declared in packages, but within a block or
               --  a subprogram the body will appear in the same declarative
               --  part, and we must insert a check in the eventual body itself
               --  using the elaboration flag that we generate now. The check
               --  is then inserted when the body is expanded. This processing
               --  is not needed for a stand alone expression function because
               --  the internally generated spec and body are always inserted
               --  as a pair in the same declarative list.

               begin
                  if Expander_Active
                    and then Comes_From_Source (Subp_Id)
                    and then Comes_From_Source (N)
                    and then In_Open_Scopes (Scop)
                    and then Ekind_In (Scop, E_Block, E_Procedure, E_Function)
                    and then not Has_Completion (Subp_Id)
                    and then No (Elaboration_Entity (Subp_Id))
                    and then Nkind (Subp_Decl) = N_Subprogram_Declaration
                    and then Nkind (Original_Node (Subp_Decl)) /=
                                                       N_Expression_Function
                  then
                     --  Create elaboration variable for it

                     Flag_Id := Make_Temporary (Loc, 'E');
                     Set_Elaboration_Entity (Subp_Id, Flag_Id);
                     Set_Is_Frozen (Flag_Id);

                     --  Insert declaration for flag after subprogram
                     --  declaration. Note that attribute reference may
                     --  appear within a nested scope.

                     Insert_After_And_Analyze (Subp_Decl,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Flag_Id,
                         Object_Definition   =>
                           New_Occurrence_Of (Standard_Short_Integer, Loc),
                         Expression          =>
                           Make_Integer_Literal (Loc, Uint_0)));
                  end if;

                  --  Taking the 'Access of an expression function freezes its
                  --  expression (RM 13.14 10.3/3). This does not apply to an
                  --  expression function that acts as a completion because the
                  --  generated body is immediately analyzed and the expression
                  --  is automatically frozen.

                  if Is_Expression_Function (Subp_Id)
                    and then Present (Corresponding_Body (Subp_Decl))
                  then
                     Subp_Body :=
                       Unit_Declaration_Node (Corresponding_Body (Subp_Decl));

                     --  The body has already been analyzed when the expression
                     --  function acts as a completion.

                     if Analyzed (Subp_Body) then
                        null;

                     --  Attribute 'Access may appear within the generated body
                     --  of the expression function subject to the attribute:

                     --    function F is (... F'Access ...);

                     --  If the expression function is on the scope stack, then
                     --  the body is currently being analyzed. Do not reanalyze
                     --  it because this will lead to infinite recursion.

                     elsif In_Open_Scopes (Subp_Id) then
                        null;

                     --  If reference to the expression function appears in an
                     --  inner scope, for example as an actual in an instance,
                     --  this is not a freeze point either.

                     elsif Scope (Subp_Id) /= Current_Scope then
                        null;

                      --  Analyze the body of the expression function to freeze
                      --  the expression. This takes care of the case where the
                      --  'Access is part of dispatch table initialization and
                      --  the generated body of the expression function has not
                      --  been analyzed yet.

                     else
                        Analyze (Subp_Body);
                     end if;
                  end if;
               end;
            end if;

         -------------
         -- Address --
         -------------

         --  Deal with resolving the type for Address attribute, overloading
         --  is not permitted here, since there is no context to resolve it.

         when Attribute_Address
            | Attribute_Code_Address
         =>
            --  To be safe, assume that if the address of a variable is taken,
            --  it may be modified via this address, so note modification.

            if Is_Variable (P) then
               Note_Possible_Modification (P, Sure => False);
            end if;

            if Nkind (P) in N_Subexpr
              and then Is_Overloaded (P)
            then
               Get_First_Interp (P, Index, It);
               Get_Next_Interp (Index, It);

               if Present (It.Nam) then
                  Error_Msg_Name_1 := Aname;
                  Error_Msg_F
                    ("prefix of % attribute cannot be overloaded", P);
               end if;
            end if;

            if not Is_Entity_Name (P)
              or else not Is_Overloadable (Entity (P))
            then
               if not Is_Task_Type (Etype (P))
                 or else Nkind (P) = N_Explicit_Dereference
               then
                  Resolve (P);
               end if;
            end if;

            --  If this is the name of a derived subprogram, or that of a
            --  generic actual, the address is that of the original entity.

            if Is_Entity_Name (P)
              and then Is_Overloadable (Entity (P))
              and then Present (Alias (Entity (P)))
            then
               Rewrite (P,
                 New_Occurrence_Of (Alias (Entity (P)), Sloc (P)));
            end if;

            if Is_Entity_Name (P) then
               Set_Address_Taken (Entity (P));
            end if;

            if Nkind (P) = N_Slice then

               --  Arr (X .. Y)'address is identical to Arr (X)'address,
               --  even if the array is packed and the slice itself is not
               --  addressable. Transform the prefix into an indexed component.

               --  Note that the transformation is safe only if we know that
               --  the slice is non-null. That is because a null slice can have
               --  an out of bounds index value.

               --  Right now, gigi blows up if given 'Address on a slice as a
               --  result of some incorrect freeze nodes generated by the front
               --  end, and this covers up that bug in one case, but the bug is
               --  likely still there in the cases not handled by this code ???

               --  It's not clear what 'Address *should* return for a null
               --  slice with out of bounds indexes, this might be worth an ARG
               --  discussion ???

               --  One approach would be to do a length check unconditionally,
               --  and then do the transformation below unconditionally, but
               --  analyze with checks off, avoiding the problem of the out of
               --  bounds index. This approach would interpret the address of
               --  an out of bounds null slice as being the address where the
               --  array element would be if there was one, which is probably
               --  as reasonable an interpretation as any ???

               declare
                  Loc : constant Source_Ptr := Sloc (P);
                  D   : constant Node_Id := Discrete_Range (P);
                  Lo  : Node_Id;

               begin
                  if Is_Entity_Name (D)
                    and then
                      Not_Null_Range
                        (Type_Low_Bound (Entity (D)),
                         Type_High_Bound (Entity (D)))
                  then
                     Lo :=
                       Make_Attribute_Reference (Loc,
                          Prefix => (New_Occurrence_Of (Entity (D), Loc)),
                          Attribute_Name => Name_First);

                  elsif Nkind (D) = N_Range
                    and then Not_Null_Range (Low_Bound (D), High_Bound (D))
                  then
                     Lo := Low_Bound (D);

                  else
                     Lo := Empty;
                  end if;

                  if Present (Lo) then
                     Rewrite (P,
                        Make_Indexed_Component (Loc,
                           Prefix =>  Relocate_Node (Prefix (P)),
                           Expressions => New_List (Lo)));

                     Analyze_And_Resolve (P);
                  end if;
               end;
            end if;

         ------------------
         -- Body_Version --
         ------------------

         --  Prefix of Body_Version attribute can be a subprogram name which
         --  must not be resolved, since this is not a call.

         when Attribute_Body_Version =>
            null;

         ------------
         -- Caller --
         ------------

         --  Prefix of Caller attribute is an entry name which must not
         --  be resolved, since this is definitely not an entry call.

         when Attribute_Caller =>
            null;

         ------------------
         -- Code_Address --
         ------------------

         --  Shares processing with Address attribute

         -----------
         -- Count --
         -----------

         --  If the prefix of the Count attribute is an entry name it must not
         --  be resolved, since this is definitely not an entry call. However,
         --  if it is an element of an entry family, the index itself may
         --  have to be resolved because it can be a general expression.

         when Attribute_Count =>
            if Nkind (P) = N_Indexed_Component
              and then Is_Entity_Name (Prefix (P))
            then
               declare
                  Indx : constant Node_Id   := First (Expressions (P));
                  Fam  : constant Entity_Id := Entity (Prefix (P));
               begin
                  Resolve (Indx, Entry_Index_Type (Fam));
                  Apply_Range_Check (Indx, Entry_Index_Type (Fam));
               end;
            end if;

         ----------------
         -- Elaborated --
         ----------------

         --  Prefix of the Elaborated attribute is a subprogram name which
         --  must not be resolved, since this is definitely not a call. Note
         --  that it is a library unit, so it cannot be overloaded here.

         when Attribute_Elaborated =>
            null;

         -------------
         -- Enabled --
         -------------

         --  Prefix of Enabled attribute is a check name, which must be treated
         --  specially and not touched by Resolve.

         when Attribute_Enabled =>
            null;

         ----------------
         -- Loop_Entry --
         ----------------

         --  Do not resolve the prefix of Loop_Entry, instead wait until the
         --  attribute has been expanded (see Expand_Loop_Entry_Attributes).
         --  The delay ensures that any generated checks or temporaries are
         --  inserted before the relocated prefix.

         when Attribute_Loop_Entry =>
            null;

         --------------------
         -- Mechanism_Code --
         --------------------

         --  Prefix of the Mechanism_Code attribute is a function name
         --  which must not be resolved. Should we check for overloaded ???

         when Attribute_Mechanism_Code =>
            null;

         ------------------
         -- Partition_ID --
         ------------------

         --  Most processing is done in sem_dist, after determining the
         --  context type. Node is rewritten as a conversion to a runtime call.

         when Attribute_Partition_ID =>
            Process_Partition_Id (N);
            return;

         ------------------
         -- Pool_Address --
         ------------------

         when Attribute_Pool_Address =>
            Resolve (P);

         -----------
         -- Range --
         -----------

         --  We replace the Range attribute node with a range expression whose
         --  bounds are the 'First and 'Last attributes applied to the same
         --  prefix. The reason that we do this transformation here instead of
         --  in the expander is that it simplifies other parts of the semantic
         --  analysis which assume that the Range has been replaced; thus it
         --  must be done even when in semantic-only mode (note that the RM
         --  specifically mentions this equivalence, we take care that the
         --  prefix is only evaluated once).

         when Attribute_Range => Range_Attribute : declare
            Dims : List_Id;
            HB   : Node_Id;
            LB   : Node_Id;

         begin
            if not Is_Entity_Name (P) or else not Is_Type (Entity (P)) then
               Resolve (P);
            end if;

            Dims := Expressions (N);

            HB :=
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (P, Name_Req => True),
                Attribute_Name => Name_Last,
                Expressions    => Dims);

            LB :=
              Make_Attribute_Reference (Loc,
                Prefix          => P,
                Attribute_Name  => Name_First,
                Expressions     => (Dims));

            --  Do not share the dimension indicator, if present. Even though
            --  it is a static constant, its source location may be modified
            --  when printing expanded code and node sharing will lead to chaos
            --  in Sprint.

            if Present (Dims) then
               Set_Expressions (LB, New_List (New_Copy_Tree (First (Dims))));
            end if;

            --  If the original was marked as Must_Not_Freeze (see code in
            --  Sem_Ch3.Make_Index), then make sure the rewriting does not
            --  freeze either.

            if Must_Not_Freeze (N) then
               Set_Must_Not_Freeze (HB);
               Set_Must_Not_Freeze (LB);
               Set_Must_Not_Freeze (Prefix (HB));
               Set_Must_Not_Freeze (Prefix (LB));
            end if;

            if Raises_Constraint_Error (Prefix (N)) then

               --  Preserve Sloc of prefix in the new bounds, so that the
               --  posted warning can be removed if we are within unreachable
               --  code.

               Set_Sloc (LB, Sloc (Prefix (N)));
               Set_Sloc (HB, Sloc (Prefix (N)));
            end if;

            Rewrite (N, Make_Range (Loc, LB, HB));
            Analyze_And_Resolve (N, Typ);

            --  Ensure that the expanded range does not have side effects

            Force_Evaluation (LB);
            Force_Evaluation (HB);

            --  Normally after resolving attribute nodes, Eval_Attribute
            --  is called to do any possible static evaluation of the node.
            --  However, here since the Range attribute has just been
            --  transformed into a range expression it is no longer an
            --  attribute node and therefore the call needs to be avoided
            --  and is accomplished by simply returning from the procedure.

            return;
         end Range_Attribute;

         ------------
         -- Result --
         ------------

         --  We will only come here during the prescan of a spec expression
         --  containing a Result attribute. In that case the proper Etype has
         --  already been set, and nothing more needs to be done here.

         when Attribute_Result =>
            null;

         ----------------------
         -- Unchecked_Access --
         ----------------------

         --  Processing is shared with Access

         -------------------------
         -- Unrestricted_Access --
         -------------------------

         --  Processing is shared with Access

         ------------
         -- Update --
         ------------

         --  Resolve aggregate components in component associations

         when Attribute_Update => Update : declare
            Aggr  : constant Node_Id   := First (Expressions (N));
            Typ   : constant Entity_Id := Etype (Prefix (N));
            Assoc : Node_Id;
            Comp  : Node_Id;
            Expr  : Node_Id;

         begin
            --  Set the Etype of the aggregate to that of the prefix, even
            --  though the aggregate may not be a proper representation of a
            --  value of the type (missing or duplicated associations, etc.)
            --  Complete resolution of the prefix. Note that in Ada 2012 it
            --  can be a qualified expression that is e.g. an aggregate.

            Set_Etype (Aggr, Typ);
            Resolve (Prefix (N), Typ);

            --  For an array type, resolve expressions with the component type
            --  of the array, and apply constraint checks when needed.

            if Is_Array_Type (Typ) then
               Assoc := First (Component_Associations (Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Resolve (Expr, Component_Type (Typ));

                  --  For scalar array components set Do_Range_Check when
                  --  needed. Constraint checking on non-scalar components
                  --  is done in Aggregate_Constraint_Checks, but only if
                  --  full analysis is enabled. These flags are not set in
                  --  the front-end in GnatProve mode.

                  if Is_Scalar_Type (Component_Type (Typ))
                    and then not Is_OK_Static_Expression (Expr)
                    and then not Range_Checks_Suppressed (Component_Type (Typ))
                  then
                     if Is_Entity_Name (Expr)
                       and then Etype (Expr) = Component_Type (Typ)
                     then
                        null;

                     else
                        Set_Do_Range_Check (Expr);
                     end if;
                  end if;

                  --  The choices in the association are static constants,
                  --  or static aggregates each of whose components belongs
                  --  to the proper index type. However, they must also
                  --  belong to the index subtype (s) of the prefix, which
                  --  may be a subtype (e.g. given by a slice).

                  --  Choices may also be identifiers with no staticness
                  --  requirements, in which case they must resolve to the
                  --  index type.

                  declare
                     C    : Node_Id;
                     C_E  : Node_Id;
                     Indx : Node_Id;

                  begin
                     C := First (Choices (Assoc));
                     while Present (C) loop
                        Indx := First_Index (Etype (Prefix (N)));

                        if Nkind (C) /= N_Aggregate then
                           Analyze_And_Resolve (C, Etype (Indx));
                           Apply_Constraint_Check (C, Etype (Indx));
                           Check_Non_Static_Context (C);

                        else
                           C_E := First (Expressions (C));
                           while Present (C_E) loop
                              Analyze_And_Resolve (C_E, Etype (Indx));
                              Apply_Constraint_Check (C_E, Etype (Indx));
                              Check_Non_Static_Context (C_E);

                              Next (C_E);
                              Next_Index (Indx);
                           end loop;
                        end if;

                        Next (C);
                     end loop;
                  end;

                  Next (Assoc);
               end loop;

            --  For a record type, use type of each component, which is
            --  recorded during analysis.

            else
               Assoc := First (Component_Associations (Aggr));
               while Present (Assoc) loop
                  Comp := First (Choices (Assoc));
                  Expr := Expression (Assoc);

                  if Nkind (Comp) /= N_Others_Choice
                    and then not Error_Posted (Comp)
                  then
                     Resolve (Expr, Etype (Entity (Comp)));

                     if Is_Scalar_Type (Etype (Entity (Comp)))
                       and then not Is_OK_Static_Expression (Expr)
                       and then not Range_Checks_Suppressed
                                      (Etype (Entity (Comp)))
                     then
                        Set_Do_Range_Check (Expr);
                     end if;
                  end if;

                  Next (Assoc);
               end loop;
            end if;
         end Update;

         ---------
         -- Val --
         ---------

         --  Apply range check. Note that we did not do this during the
         --  analysis phase, since we wanted Eval_Attribute to have a
         --  chance at finding an illegal out of range value.

         when Attribute_Val =>

            --  Note that we do our own Eval_Attribute call here rather than
            --  use the common one, because we need to do processing after
            --  the call, as per above comment.

            Eval_Attribute (N);

            --  Eval_Attribute may replace the node with a raise CE, or
            --  fold it to a constant. Obviously we only apply a scalar
            --  range check if this did not happen.

            if Nkind (N) = N_Attribute_Reference
              and then Attribute_Name (N) = Name_Val
            then
               Apply_Scalar_Range_Check (First (Expressions (N)), Btyp);
            end if;

            return;

         -------------
         -- Version --
         -------------

         --  Prefix of Version attribute can be a subprogram name which
         --  must not be resolved, since this is not a call.

         when Attribute_Version =>
            null;

         ----------------------
         -- Other Attributes --
         ----------------------

         --  For other attributes, resolve prefix unless it is a type. If
         --  the attribute reference itself is a type name ('Base and 'Class)
         --  then this is only legal within a task or protected record.

         when others =>
            if not Is_Entity_Name (P) or else not Is_Type (Entity (P)) then
               Resolve (P);
            end if;

            --  If the attribute reference itself is a type name ('Base,
            --  'Class) then this is only legal within a task or protected
            --  record. What is this all about ???

            if Is_Entity_Name (N) and then Is_Type (Entity (N)) then
               if Is_Concurrent_Type (Entity (N))
                 and then In_Open_Scopes (Entity (P))
               then
                  null;
               else
                  Error_Msg_N
                    ("invalid use of subtype name in expression or call", N);
               end if;
            end if;

            --  For attributes whose argument may be a string, complete
            --  resolution of argument now. This avoids premature expansion
            --  (and the creation of transient scopes) before the attribute
            --  reference is resolved.

            case Attr_Id is
               when Attribute_Value =>
                  Resolve (First (Expressions (N)), Standard_String);

               when Attribute_Wide_Value =>
                  Resolve (First (Expressions (N)), Standard_Wide_String);

               when Attribute_Wide_Wide_Value =>
                  Resolve (First (Expressions (N)), Standard_Wide_Wide_String);

               when others => null;
            end case;

            --  If the prefix of the attribute is a class-wide type then it
            --  will be expanded into a dispatching call to a predefined
            --  primitive. Therefore we must check for potential violation
            --  of such restriction.

            if Is_Class_Wide_Type (Etype (P)) then
               Check_Restriction (No_Dispatching_Calls, N);
            end if;
      end case;

      --  Normally the Freezing is done by Resolve but sometimes the Prefix
      --  is not resolved, in which case the freezing must be done now.

      --  For an elaboration check on a subprogram, we do not freeze its type.
      --  It may be declared in an unrelated scope, in particular in the case
      --  of a generic function whose type may remain unelaborated.

      if Attr_Id = Attribute_Elaborated then
         null;

      else
         Freeze_Expression (P);
      end if;

      --  Finally perform static evaluation on the attribute reference

      Analyze_Dimension (N);
      Eval_Attribute (N);
   end Resolve_Attribute;

   ------------------------
   -- Set_Boolean_Result --
   ------------------------

   procedure Set_Boolean_Result (N : Node_Id; B : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
   begin
      if B then
         Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
      else
         Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
      end if;
   end Set_Boolean_Result;

   -------------------------------
   -- Statically_Denotes_Object --
   -------------------------------

   function Statically_Denotes_Object (N : Node_Id) return Boolean is
      Indx : Node_Id;

   begin
      if Is_Entity_Name (N) then
         return True;

      elsif Nkind (N) = N_Selected_Component
        and then Statically_Denotes_Object (Prefix (N))
        and then Present (Entity (Selector_Name (N)))
      then
         declare
            Sel_Id    : constant Entity_Id := Entity (Selector_Name (N));
            Comp_Decl : constant Node_Id   := Parent (Sel_Id);

         begin
            if Depends_On_Discriminant (Sel_Id) then
               return False;

            elsif Nkind (Parent (Parent (Comp_Decl))) = N_Variant then
               return False;

            else
               return True;
            end if;
         end;

      elsif Nkind (N) = N_Indexed_Component
        and then Statically_Denotes_Object (Prefix (N))
        and then Is_Constrained (Etype (Prefix (N)))
      then
         Indx := First (Expressions (N));
         while Present (Indx) loop
            if not Compile_Time_Known_Value (Indx)
              or else Do_Range_Check (Indx)
            then
               return False;
            end if;

            Next (Indx);
         end loop;

         return True;

      else
         return False;
      end if;
   end Statically_Denotes_Object;

   --------------------------------
   -- Stream_Attribute_Available --
   --------------------------------

   function Stream_Attribute_Available
     (Typ          : Entity_Id;
      Nam          : TSS_Name_Type;
      Partial_View : Node_Id := Empty) return Boolean
   is
      Etyp : Entity_Id := Typ;

   --  Start of processing for Stream_Attribute_Available

   begin
      --  We need some comments in this body ???

      if Has_Stream_Attribute_Definition (Typ, Nam) then
         return True;
      end if;

      if Is_Class_Wide_Type (Typ) then
         return not Is_Limited_Type (Typ)
           or else Stream_Attribute_Available (Etype (Typ), Nam);
      end if;

      if Nam = TSS_Stream_Input
        and then Is_Abstract_Type (Typ)
        and then not Is_Class_Wide_Type (Typ)
      then
         return False;
      end if;

      if not (Is_Limited_Type (Typ)
        or else (Present (Partial_View)
                   and then Is_Limited_Type (Partial_View)))
      then
         return True;
      end if;

      --  In Ada 2005, Input can invoke Read, and Output can invoke Write

      if Nam = TSS_Stream_Input
        and then Ada_Version >= Ada_2005
        and then Stream_Attribute_Available (Etyp, TSS_Stream_Read)
      then
         return True;

      elsif Nam = TSS_Stream_Output
        and then Ada_Version >= Ada_2005
        and then Stream_Attribute_Available (Etyp, TSS_Stream_Write)
      then
         return True;
      end if;

      --  Case of Read and Write: check for attribute definition clause that
      --  applies to an ancestor type.

      while Etype (Etyp) /= Etyp loop
         Etyp := Etype (Etyp);

         if Has_Stream_Attribute_Definition (Etyp, Nam) then
            return True;
         end if;
      end loop;

      if Ada_Version < Ada_2005 then

         --  In Ada 95 mode, also consider a non-visible definition

         declare
            Btyp : constant Entity_Id := Implementation_Base_Type (Typ);
         begin
            return Btyp /= Typ
              and then Stream_Attribute_Available
                         (Btyp, Nam, Partial_View => Typ);
         end;
      end if;

      return False;
   end Stream_Attribute_Available;

end Sem_Attr;
