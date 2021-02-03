------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           E I N F O . U T I L S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2020-2021, Free Software Foundation, Inc.        --
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

with Einfo.Entities; use Einfo.Entities;

package Einfo.Utils is

   -----------------------------------
   -- Renamings of Renamed_Or_Alias --
   -----------------------------------

   --  See the comment in einfo.ads, "Renaming and Aliasing", which is somewhat
   --  incorrect. In fact, the compiler uses Alias, Renamed_Entity, and
   --  Renamed_Object more-or-less interchangeably, so we rename them here.
   --  ????Should add preconditions.

   function Alias
     (N : Entity_Id) return Node_Id renames Renamed_Or_Alias;
   procedure Set_Alias
     (N : Entity_Id; Val : Node_Id) renames Set_Renamed_Or_Alias;
   function Renamed_Entity
     (N : Entity_Id) return Node_Id renames Renamed_Or_Alias;
   procedure Set_Renamed_Entity
     (N : Entity_Id; Val : Node_Id) renames Set_Renamed_Or_Alias;
   function Renamed_Object
     (N : Entity_Id) return Node_Id renames Renamed_Or_Alias;
   procedure Set_Renamed_Object
     (N : Entity_Id; Val : Node_Id) renames Set_Renamed_Or_Alias;

   --------------------------
   -- Subtype Declarations --
   --------------------------

   --  ????
   --  The above entities are arranged so that they can be conveniently grouped
   --  into subtype ranges. Note that for each of the xxx_Kind ranges defined
   --  below, there is a corresponding Is_xxx (or for types, Is_xxx_Type)
   --  predicate which is to be used in preference to direct range tests using
   --  the subtype name. However, the subtype names are available for direct
   --  use, e.g. as choices in case statements.

   -------------------
   -- Type Synonyms --
   -------------------

   --  The following type synonyms are used to tidy up the function and
   --  procedure declarations that follow, and also to make it possible to meet
   --  the requirement for the XEINFO utility that all function specs must fit
   --  on a single source line.????

   subtype B is Boolean;
   subtype C is Component_Alignment_Kind;
   subtype E is Entity_Id;
   subtype F is Float_Rep_Kind;
   subtype M is Mechanism_Type;
   subtype N is Node_Id;
   subtype U is Uint;
   subtype R is Ureal;
   subtype L is Elist_Id;
   subtype S is List_Id;

   -------------------------------
   -- Classification Attributes --
   -------------------------------

   --  These functions provide a convenient functional notation for testing
   --  whether an Ekind value belongs to a specified kind, for example the
   --  function Is_Elementary_Type tests if its argument is in Elementary_Kind.
   --  In some cases, the test is of an entity attribute (e.g. in the case of
   --  Is_Generic_Type where the Ekind does not provide the needed
   --  information).
   --  ????Could automatically generate some of these?

   function Is_Access_Object_Type               (Id : E) return B;
   function Is_Access_Type                      (Id : E) return B;
   function Is_Access_Protected_Subprogram_Type (Id : E) return B;
   function Is_Access_Subprogram_Type           (Id : E) return B;
   function Is_Aggregate_Type                   (Id : E) return B;
   function Is_Anonymous_Access_Type            (Id : E) return B;
   function Is_Array_Type                       (Id : E) return B;
   function Is_Assignable                       (Id : E) return B;
   function Is_Class_Wide_Type                  (Id : E) return B;
   function Is_Composite_Type                   (Id : E) return B;
   function Is_Concurrent_Body                  (Id : E) return B;
   function Is_Concurrent_Type                  (Id : E) return B;
   function Is_Decimal_Fixed_Point_Type         (Id : E) return B;
   function Is_Digits_Type                      (Id : E) return B;
   function Is_Discrete_Or_Fixed_Point_Type     (Id : E) return B;
   function Is_Discrete_Type                    (Id : E) return B;
   function Is_Elementary_Type                  (Id : E) return B;
   function Is_Entry                            (Id : E) return B;
   function Is_Enumeration_Type                 (Id : E) return B;
   function Is_Fixed_Point_Type                 (Id : E) return B;
   function Is_Floating_Point_Type              (Id : E) return B;
   function Is_Formal                           (Id : E) return B;
   function Is_Formal_Object                    (Id : E) return B;
   function Is_Generic_Subprogram               (Id : E) return B;
   function Is_Generic_Unit                     (Id : E) return B;
   function Is_Ghost_Entity                     (Id : E) return B;
   function Is_Incomplete_Or_Private_Type       (Id : E) return B;
   function Is_Incomplete_Type                  (Id : E) return B;
   function Is_Integer_Type                     (Id : E) return B;
   function Is_Modular_Integer_Type             (Id : E) return B;
   function Is_Named_Access_Type                (Id : E) return B;
   function Is_Named_Number                     (Id : E) return B;
   function Is_Numeric_Type                     (Id : E) return B;
   function Is_Object                           (Id : E) return B;
   function Is_Ordinary_Fixed_Point_Type        (Id : E) return B;
   function Is_Overloadable                     (Id : E) return B;
   function Is_Private_Type                     (Id : E) return B;
   function Is_Protected_Type                   (Id : E) return B;
   function Is_Real_Type                        (Id : E) return B;
   function Is_Record_Type                      (Id : E) return B;
   function Is_Scalar_Type                      (Id : E) return B;
   function Is_Signed_Integer_Type              (Id : E) return B;
   function Is_Subprogram                       (Id : E) return B;
   function Is_Subprogram_Or_Entry              (Id : E) return B;
   function Is_Subprogram_Or_Generic_Subprogram (Id : E) return B;
   function Is_Task_Type                        (Id : E) return B;
   function Is_Type                             (Id : E) return B;

   -------------------------------------
   -- Synthesized Attribute Functions --
   -------------------------------------

   --  The functions in this section synthesize attributes from the tree,
   --  so they do not correspond to defined fields in the entity itself.

   function Address_Clause                      (Id : E) return N;
   function Aft_Value                           (Id : E) return U;
   function Alignment_Clause                    (Id : E) return N;
   function Base_Type                           (Id : E) return E;
   function Declaration_Node                    (Id : E) return N;
   function Designated_Type                     (Id : E) return E;
   function Entry_Index_Type                    (Id : E) return E;
   function First_Component                     (Id : E) return E;
   function First_Component_Or_Discriminant     (Id : E) return E;
   function First_Formal                        (Id : E) return E;
   function First_Formal_With_Extras            (Id : E) return E;
   function Has_Attach_Handler                  (Id : E) return B;
   function Has_DIC                             (Id : E) return B;
   function Has_Entries                         (Id : E) return B;
   function Has_Foreign_Convention              (Id : E) return B;
   function Has_Interrupt_Handler               (Id : E) return B;
   function Has_Invariants                      (Id : E) return B;
   function Has_Limited_View                    (Id : E) return B;
   function Has_Non_Limited_View                (Id : E) return B;
   function Has_Non_Null_Abstract_State         (Id : E) return B;
   function Has_Non_Null_Visible_Refinement     (Id : E) return B;
   function Has_Null_Abstract_State             (Id : E) return B;
   function Has_Null_Visible_Refinement         (Id : E) return B;
   function Implementation_Base_Type            (Id : E) return E;
   function Is_Base_Type                        (Id : E) return B;
   function Is_Boolean_Type                     (Id : E) return B;
   function Is_Constant_Object                  (Id : E) return B;
   function Is_Controlled                       (Id : E) return B;
   function Is_Discriminal                      (Id : E) return B;
   function Is_Dynamic_Scope                    (Id : E) return B;
   function Is_Elaboration_Target               (Id : E) return B;
   function Is_External_State                   (Id : E) return B;
   function Is_Finalizer                        (Id : E) return B;
   function Is_Full_Access                      (Id : E) return B;
   function Is_Null_State                       (Id : E) return B;
   function Is_Package_Or_Generic_Package       (Id : E) return B;
   function Is_Packed_Array                     (Id : E) return B;
   function Is_Prival                           (Id : E) return B;
   function Is_Protected_Component              (Id : E) return B;
   function Is_Protected_Interface              (Id : E) return B;
   function Is_Protected_Record_Type            (Id : E) return B;
   function Is_Relaxed_Initialization_State     (Id : E) return B;
   function Is_Standard_Character_Type          (Id : E) return B;
   function Is_Standard_String_Type             (Id : E) return B;
   function Is_String_Type                      (Id : E) return B;
   function Is_Synchronized_Interface           (Id : E) return B;
   function Is_Synchronized_State               (Id : E) return B;
   function Is_Task_Interface                   (Id : E) return B;
   function Is_Task_Record_Type                 (Id : E) return B;
   function Is_Wrapper_Package                  (Id : E) return B;
   function Last_Formal                         (Id : E) return E;
   function Machine_Emax_Value                  (Id : E) return U;
   function Machine_Emin_Value                  (Id : E) return U;
   function Machine_Mantissa_Value              (Id : E) return U;
   function Machine_Radix_Value                 (Id : E) return U;
   function Model_Emin_Value                    (Id : E) return U;
   function Model_Epsilon_Value                 (Id : E) return R;
   function Model_Mantissa_Value                (Id : E) return U;
   function Model_Small_Value                   (Id : E) return R;
   function Next_Component                      (Id : E) return E;
   function Next_Component_Or_Discriminant      (Id : E) return E;
   function Next_Discriminant                   (Id : E) return E;
   function Next_Formal                         (Id : E) return E;
   function Next_Formal_With_Extras             (Id : E) return E;
   function Next_Index                          (Id : N) return N;
   function Next_Literal                        (Id : E) return E;
   function Next_Stored_Discriminant            (Id : E) return E;
   function Number_Dimensions                   (Id : E) return Pos;
   function Number_Entries                      (Id : E) return Nat;
   function Number_Formals                      (Id : E) return Pos;
   function Object_Size_Clause                  (Id : E) return N;
   function Parameter_Mode                      (Id : E) return Formal_Kind;
   function Partial_Refinement_Constituents     (Id : E) return L;
   function Primitive_Operations                (Id : E) return L;
   function Root_Type                           (Id : E) return E;
   function Safe_Emax_Value                     (Id : E) return U;
   function Safe_First_Value                    (Id : E) return R;
   function Safe_Last_Value                     (Id : E) return R;
   function Scope_Depth                         (Id : E) return U;
   function Scope_Depth_Set                     (Id : E) return B;
   function Size_Clause                         (Id : E) return N;
   function Stream_Size_Clause                  (Id : E) return N;
   function Type_High_Bound                     (Id : E) return N;
   function Type_Low_Bound                      (Id : E) return N;
   function Underlying_Type                     (Id : E) return E;

   ----------------------------------------------
   -- Type Representation Attribute Predicates --
   ----------------------------------------------

   --  These predicates test the setting of the indicated attribute. If the
   --  value has been set, then Known is True, and Unknown is False. If no
   --  value is set, then Known is False and Unknown is True. The Known_Static
   --  predicate is true only if the value is set (Known) and is set to a
   --  compile time known value. Note that in the case of Alignment and
   --  Normalized_First_Bit, dynamic values are not possible, so we do not
   --  need a separate Known_Static calls in these cases. The not set (unknown)
   --  values are as follows:

   --    Alignment               Uint_0 or No_Uint
   --    Component_Size          Uint_0 or No_Uint
   --    Component_Bit_Offset    No_Uint
   --    Digits_Value            Uint_0 or No_Uint
   --    Esize                   Uint_0 or No_Uint
   --    Normalized_First_Bit    No_Uint
   --    Normalized_Position     No_Uint
   --    Normalized_Position_Max No_Uint
   --    RM_Size                 Uint_0 or No_Uint

   --  It would be cleaner to use No_Uint in all these cases, but historically
   --  we chose to use Uint_0 at first, and the change over will take time ???
   --  This is particularly true for the RM_Size field, where a value of zero
   --  is legitimate. We deal with this by a considering that the value is
   --  always known static for discrete types (and no other types can have
   --  an RM_Size value of zero).

   --  In two cases, Known_Static_Esize and Known_Static_RM_Size, there is one
   --  more consideration, which is that we always return False for generic
   --  types. Within a template, the size can look known, because of the fake
   --  size values we put in template types, but they are not really known and
   --  anyone testing if they are known within the template should get False as
   --  a result to prevent incorrect assumptions.

   function Known_Alignment                       (E : Entity_Id) return B;
   function Known_Component_Bit_Offset            (E : Entity_Id) return B;
   function Known_Component_Size                  (E : Entity_Id) return B;
   function Known_Esize                           (E : Entity_Id) return B;
   function Known_Normalized_First_Bit            (E : Entity_Id) return B;
   function Known_Normalized_Position             (E : Entity_Id) return B;
   function Known_Normalized_Position_Max         (E : Entity_Id) return B;
   function Known_RM_Size                         (E : Entity_Id) return B;

   function Known_Static_Component_Bit_Offset     (E : Entity_Id) return B;
   function Known_Static_Component_Size           (E : Entity_Id) return B;
   function Known_Static_Esize                    (E : Entity_Id) return B;
   function Known_Static_Normalized_First_Bit     (E : Entity_Id) return B;
   function Known_Static_Normalized_Position      (E : Entity_Id) return B;
   function Known_Static_Normalized_Position_Max  (E : Entity_Id) return B;
   function Known_Static_RM_Size                  (E : Entity_Id) return B;

   function Unknown_Alignment                     (E : Entity_Id) return B;
   function Unknown_Component_Bit_Offset          (E : Entity_Id) return B;
   function Unknown_Component_Size                (E : Entity_Id) return B;
   function Unknown_Esize                         (E : Entity_Id) return B;
   function Unknown_Normalized_First_Bit          (E : Entity_Id) return B;
   function Unknown_Normalized_Position           (E : Entity_Id) return B;
   function Unknown_Normalized_Position_Max       (E : Entity_Id) return B;
   function Unknown_RM_Size                       (E : Entity_Id) return B;

   ---------------------------------------------------
   -- Access to Subprograms in Subprograms_For_Type --
   ---------------------------------------------------

   function Is_Partial_DIC_Procedure            (Id : E) return B;

   function DIC_Procedure                        (Id : E) return E;
   function Partial_DIC_Procedure                (Id : E) return E;
   function Invariant_Procedure                  (Id : E) return E;
   function Partial_Invariant_Procedure          (Id : E) return E;
   function Predicate_Function                   (Id : E) return E;
   function Predicate_Function_M                 (Id : E) return E;

   procedure Set_DIC_Procedure                   (Id : E; V : E);
   procedure Set_Partial_DIC_Procedure           (Id : E; V : E);
   procedure Set_Invariant_Procedure             (Id : E; V : E);
   procedure Set_Partial_Invariant_Procedure     (Id : E; V : E);
   procedure Set_Predicate_Function              (Id : E; V : E);
   procedure Set_Predicate_Function_M            (Id : E; V : E);

   -----------------------------------
   -- Field Initialization Routines --
   -----------------------------------

   --  These routines are overloadings of some of the above Set procedures
   --  where the argument is normally a Uint. The overloadings take an Int
   --  parameter instead, and appropriately convert it. There are also
   --  versions that implicitly initialize to the appropriate "not set"
   --  value. The not set (unknown) values are as follows:

   --    Alignment                 Uint_0
   --    Component_Size            Uint_0
   --    Component_Bit_Offset      No_Uint
   --    Digits_Value              Uint_0
   --    Esize                     Uint_0
   --    Normalized_First_Bit      No_Uint
   --    Normalized_Position       No_Uint
   --    Normalized_Position_Max   No_Uint
   --    RM_Size                   Uint_0

   --  It would be cleaner to use No_Uint in all these cases, but historically
   --  we chose to use Uint_0 at first, and the change over will take time ???
   --  This is particularly true for the RM_Size field, where a value of zero
   --  is legitimate and causes some special tests around the code.

   --  Contrary to the corresponding Set procedures above, these routines
   --  do NOT check the entity kind of their argument, instead they set the
   --  underlying Uint fields directly (this allows them to be used for
   --  entities whose Ekind has not been set yet).

   procedure Init_Alignment                (Id : E; V : Int);
   procedure Init_Component_Bit_Offset     (Id : E; V : Int);
   procedure Init_Component_Size           (Id : E; V : Int);
   procedure Init_Digits_Value             (Id : E; V : Int);
   procedure Init_Esize                    (Id : E; V : Int);
   procedure Init_Normalized_First_Bit     (Id : E; V : Int);
   procedure Init_Normalized_Position      (Id : E; V : Int);
   procedure Init_Normalized_Position_Max  (Id : E; V : Int);
   procedure Init_RM_Size                  (Id : E; V : Int);

   procedure Init_Alignment                (Id : E);
   procedure Init_Component_Bit_Offset     (Id : E);
   procedure Init_Component_Size           (Id : E);
   procedure Init_Digits_Value             (Id : E);
   procedure Init_Esize                    (Id : E);
   procedure Init_Normalized_First_Bit     (Id : E);
   procedure Init_Normalized_Position      (Id : E);
   procedure Init_Normalized_Position_Max  (Id : E);
   procedure Init_RM_Size                  (Id : E);

   procedure Init_Component_Location (Id : E);
   --  Initializes all fields describing the location of a component
   --  (Normalized_Position, Component_Bit_Offset, Normalized_First_Bit,
   --  Normalized_Position_Max, Esize) to all be Unknown.

   procedure Init_Size (Id : E; V : Int);
   --  Initialize both the Esize and RM_Size fields of E to V

   procedure Init_Size_Align (Id : E);
   --  This procedure initializes both size fields and the alignment
   --  field to all be Unknown.

   procedure Init_Object_Size_Align (Id : E);
   --  Same as Init_Size_Align except RM_Size field (which is only for types)
   --  is unaffected.

   ---------------
   -- Iterators --
   ---------------

   --  The call to Next_xxx (obj) is equivalent to obj := Next_xxx (obj)
   --  We define the set of Proc_Next_xxx routines simply for the purposes
   --  of inlining them without necessarily inlining the function.

   procedure Proc_Next_Component                 (N : in out Node_Id);
   procedure Proc_Next_Component_Or_Discriminant (N : in out Node_Id);
   procedure Proc_Next_Discriminant              (N : in out Node_Id);
   procedure Proc_Next_Formal                    (N : in out Node_Id);
   procedure Proc_Next_Formal_With_Extras        (N : in out Node_Id);
   procedure Proc_Next_Index                     (N : in out Node_Id);
   procedure Proc_Next_Inlined_Subprogram        (N : in out Node_Id);
   procedure Proc_Next_Literal                   (N : in out Node_Id);
   procedure Proc_Next_Stored_Discriminant       (N : in out Node_Id);

   pragma Inline (Proc_Next_Component);
   pragma Inline (Proc_Next_Component_Or_Discriminant);
   pragma Inline (Proc_Next_Discriminant);
   pragma Inline (Proc_Next_Formal);
   pragma Inline (Proc_Next_Formal_With_Extras);
   pragma Inline (Proc_Next_Index);
   pragma Inline (Proc_Next_Inlined_Subprogram);
   pragma Inline (Proc_Next_Literal);
   pragma Inline (Proc_Next_Stored_Discriminant);

   procedure Next_Component                 (N : in out Node_Id)
     renames Proc_Next_Component;

   procedure Next_Component_Or_Discriminant (N : in out Node_Id)
     renames Proc_Next_Component_Or_Discriminant;

   procedure Next_Discriminant              (N : in out Node_Id)
     renames Proc_Next_Discriminant;

   procedure Next_Formal                    (N : in out Node_Id)
     renames Proc_Next_Formal;

   procedure Next_Formal_With_Extras        (N : in out Node_Id)
     renames Proc_Next_Formal_With_Extras;

   procedure Next_Index                     (N : in out Node_Id)
     renames Proc_Next_Index;

   procedure Next_Inlined_Subprogram        (N : in out Node_Id)
     renames Proc_Next_Inlined_Subprogram;

   procedure Next_Literal                   (N : in out Node_Id)
     renames Proc_Next_Literal;

   procedure Next_Stored_Discriminant       (N : in out Node_Id)
     renames Proc_Next_Stored_Discriminant;

   ---------------------------
   -- Testing Warning Flags --
   ---------------------------

   --  These routines are to be used rather than testing flags Warnings_Off,
   --  Has_Pragma_Unmodified, Has_Pragma_Unreferenced. They deal with setting
   --  the flags Warnings_Off_Used[_Unmodified|Unreferenced] for later access.

   function Has_Warnings_Off (E : Entity_Id) return Boolean;
   --  If Warnings_Off is set on E, then returns True and also sets the flag
   --  Warnings_Off_Used on E. If Warnings_Off is not set on E, returns False
   --  and has no side effect.

   function Has_Unmodified (E : Entity_Id) return Boolean;
   --  If flag Has_Pragma_Unmodified is set on E, returns True with no side
   --  effects. Otherwise if Warnings_Off is set on E, returns True and also
   --  sets the flag Warnings_Off_Used_Unmodified on E. If neither of the flags
   --  Warnings_Off nor Has_Pragma_Unmodified is set, returns False with no
   --  side effects.

   function Has_Unreferenced (E : Entity_Id) return Boolean;
   --  If flag Has_Pragma_Unreferenced is set on E, returns True with no side
   --  effects. Otherwise if Warnings_Off is set on E, returns True and also
   --  sets the flag Warnings_Off_Used_Unreferenced on E. If neither of the
   --  flags Warnings_Off nor Has_Pragma_Unreferenced is set, returns False
   --  with no side effects.

   ----------------------------------------------
   -- Subprograms for Accessing Rep Item Chain --
   ----------------------------------------------

   --  The First_Rep_Item field of every entity points to a linked list (linked
   --  through Next_Rep_Item) of representation pragmas, attribute definition
   --  clauses, representation clauses, and aspect specifications that apply to
   --  the item. Note that in the case of types, it is assumed that any such
   --  rep items for a base type also apply to all subtypes. This is achieved
   --  by having the chain for subtypes link onto the chain for the base type,
   --  so that new entries for the subtype are added at the start of the chain.
   --
   --  Note: aspect specification nodes are linked only when evaluation of the
   --  expression is deferred to the freeze point. For further details see
   --  Sem_Ch13.Analyze_Aspect_Specifications.

   function Get_Attribute_Definition_Clause
     (E  : Entity_Id;
      Id : Attribute_Id) return Node_Id;
   --  Searches the Rep_Item chain for a given entity E, for an instance of an
   --  attribute definition clause with the given attribute Id. If found, the
   --  value returned is the N_Attribute_Definition_Clause node, otherwise
   --  Empty is returned.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Get_Pragma (E : Entity_Id; Id : Pragma_Id) return Node_Id;
   --  Searches the Rep_Item chain of entity E, for an instance of a pragma
   --  with the given pragma Id. If found, the value returned is the N_Pragma
   --  node, otherwise Empty is returned. The following contract pragmas that
   --  appear in N_Contract nodes are also handled by this routine:
   --    Abstract_State
   --    Async_Readers
   --    Async_Writers
   --    Attach_Handler
   --    Constant_After_Elaboration
   --    Contract_Cases
   --    Depends
   --    Effective_Reads
   --    Effective_Writes
   --    Global
   --    Initial_Condition
   --    Initializes
   --    Interrupt_Handler
   --    No_Caching
   --    Part_Of
   --    Precondition
   --    Postcondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Refined_State
   --    Subprogram_Variant
   --    Test_Case
   --    Volatile_Function

   function Get_Class_Wide_Pragma
     (E  : Entity_Id;
      Id : Pragma_Id) return Node_Id;
   --  Examine Rep_Item chain to locate a classwide pre- or postcondition of a
   --  primitive operation. Returns Empty if not present.

   function Get_Record_Representation_Clause (E : Entity_Id) return Node_Id;
   --  Searches the Rep_Item chain for a given entity E, for a record
   --  representation clause, and if found, returns it. Returns Empty
   --  if no such clause is found.

   function Present_In_Rep_Item (E : Entity_Id; N : Node_Id) return Boolean;
   --  Return True if N is present in the Rep_Item chain for a given entity E

   procedure Record_Rep_Item (E : Entity_Id; N : Node_Id);
   --  N is the node for a representation pragma, representation clause, an
   --  attribute definition clause, or an aspect specification that applies to
   --  entity E. This procedure links the node N onto the Rep_Item chain for
   --  entity E. Note that it is an error to call this procedure with E being
   --  overloadable, and N being a pragma that applies to multiple overloadable
   --  entities (Convention, Interface, Inline, Inline_Always, Import, Export,
   --  External). This is not allowed even in the case where the entity is not
   --  overloaded, since we can't rely on it being present in the overloaded
   --  case, it is not useful to have it present in the non-overloaded case.

   -------------------------------
   -- Miscellaneous Subprograms --
   -------------------------------

   procedure Append_Entity (Id : Entity_Id; Scop : Entity_Id);
   --  Add an entity to the list of entities declared in the scope Scop

   function Get_Full_View (T : Entity_Id) return Entity_Id;
   --  If T is an incomplete type and the full declaration has been seen, or
   --  is the name of a class_wide type whose root is incomplete, return the
   --  corresponding full declaration, else return T itself.

   function Is_Entity_Name (N : Node_Id) return Boolean;
   --  Test if the node N is the name of an entity (i.e. is an identifier,
   --  expanded name, or an attribute reference that returns an entity).

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   procedure Link_Entities (First : Entity_Id; Second : Entity_Id);
   --  Link entities First and Second in one entity chain.
   --
   --  NOTE: No updates are done to the First_Entity and Last_Entity fields
   --  of the scope.

   procedure Remove_Entity (Id : Entity_Id);
   --  Remove entity Id from the entity chain of its scope

   function Subtype_Kind (K : Entity_Kind) return Entity_Kind;
   --  Given an entity_kind K this function returns the entity_kind
   --  corresponding to subtype kind of the type represented by K. For
   --  example if K is E_Signed_Integer_Type then E_Signed_Integer_Subtype
   --  is returned. If K is already a subtype kind it itself is returned. An
   --  internal error is generated if no such correspondence exists for K.

   procedure Unlink_Next_Entity (Id : Entity_Id);
   --  Unchain entity Id's forward link within the entity chain of its scope

   function Is_Volatile (Id : E) return B;
   procedure Set_Is_Volatile (Id : E; V : B := True);
   --  Call [Set_]Is_Volatile_Type/Is_Volatile_Object as appropriate for the
   --  Ekind of Id.

   function Convention
     (N : Entity_Id) return Convention_Id renames Basic_Convention;
   procedure Set_Convention (E : Entity_Id; Val : Convention_Id);
   --  Same as Set_Basic_Convention, but with an extra check for access types.
   --  In particular, if E is an access-to-subprogram type, and Val is a
   --  foreign convention, then we set Can_Use_Internal_Rep to False on E.
   --  Also, if the Etype of E is set and is an anonymous access type with
   --  no convention set, this anonymous type inherits the convention of E.

   ----------------------------------
   -- Debugging Output Subprograms --
   ----------------------------------

   procedure Write_Entity_Info (Id : Entity_Id; Prefix : String);
   --  A debugging procedure to write out information about an entity

   --  ????Make sure the Inlines from Einfo were fully copied here.
   --  ????
   --  The following Inline pragmas are *not* read by XEINFO when building the
   --  C version of this interface automatically (so the C version will end up
   --  making out of line calls). The pragma scan in XEINFO will be terminated
   --  on encountering the END XEINFO INLINES line. We inline things here which
   --  are small, but not of the canonical attribute access/set format that can
   --  be handled by XEINFO.

   pragma Inline (Address_Clause);
   pragma Inline (Alignment_Clause);
   pragma Inline (Base_Type);

   pragma Inline (Has_Foreign_Convention);
   pragma Inline (Has_Non_Limited_View);
   pragma Inline (Is_Base_Type);
   pragma Inline (Is_Boolean_Type);
   pragma Inline (Is_Constant_Object);
   pragma Inline (Is_Controlled);
   pragma Inline (Is_Discriminal);
   pragma Inline (Is_Entity_Name);
   pragma Inline (Is_Finalizer);
   pragma Inline (Is_Full_Access);
   pragma Inline (Is_Null_State);
   pragma Inline (Is_Package_Or_Generic_Package);
   pragma Inline (Is_Packed_Array);
   pragma Inline (Is_Prival);
   pragma Inline (Is_Protected_Component);
   pragma Inline (Is_Protected_Record_Type);
   pragma Inline (Is_String_Type);
   pragma Inline (Is_Task_Record_Type);
   pragma Inline (Is_Wrapper_Package);
   pragma Inline (Scope_Depth);
   pragma Inline (Scope_Depth_Set);
   pragma Inline (Size_Clause);
   pragma Inline (Stream_Size_Clause);
   pragma Inline (Type_High_Bound);
   pragma Inline (Type_Low_Bound);

   pragma Inline (Known_Alignment);
   pragma Inline (Known_Component_Bit_Offset);
   pragma Inline (Known_Component_Size);
   pragma Inline (Known_Esize);
   pragma Inline (Known_Normalized_First_Bit);
   pragma Inline (Known_Normalized_Position);
   pragma Inline (Known_Normalized_Position_Max);
   pragma Inline (Known_RM_Size);

   pragma Inline (Known_Static_Component_Bit_Offset);
   pragma Inline (Known_Static_Component_Size);
   pragma Inline (Known_Static_Esize);
   pragma Inline (Known_Static_Normalized_First_Bit);
   pragma Inline (Known_Static_Normalized_Position);
   pragma Inline (Known_Static_Normalized_Position_Max);
   pragma Inline (Known_Static_RM_Size);

   pragma Inline (Unknown_Alignment);
   pragma Inline (Unknown_Component_Bit_Offset);
   pragma Inline (Unknown_Component_Size);
   pragma Inline (Unknown_Esize);
   pragma Inline (Unknown_Normalized_First_Bit);
   pragma Inline (Unknown_Normalized_Position);
   pragma Inline (Unknown_Normalized_Position_Max);
   pragma Inline (Unknown_RM_Size);

   pragma Inline (Init_Alignment);
   pragma Inline (Init_Component_Bit_Offset);
   pragma Inline (Init_Component_Size);
   pragma Inline (Init_Digits_Value);
   pragma Inline (Init_Esize);
   pragma Inline (Init_Normalized_First_Bit);
   pragma Inline (Init_Normalized_Position);
   pragma Inline (Init_Normalized_Position_Max);
   pragma Inline (Init_RM_Size);

end Einfo.Utils;
