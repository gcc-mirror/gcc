------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           E I N F O . U T I L S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2020-2025, Free Software Foundation, Inc.        --
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
with Sinfo.Nodes;    use Sinfo.Nodes;

package Einfo.Utils is

   -------------------------------------------
   -- Aliases/Renamings of Renamed_Or_Alias --
   -------------------------------------------

   --  See the comment in einfo.ads, "Renaming and Aliasing", which is somewhat
   --  incorrect. Each of the following calls [Set_]Renamed_Or_Alias. Alias and
   --  Renamed_Entity are fields of nonobject Entity_Ids, and the value of the
   --  field is Entity_Id. Alias is only for callable entities and subprogram
   --  types. We sometimes call Set_Renamed_Entity and then expect Alias to
   --  return the value set. Renamed_Object is a field of Entity_Ids that are
   --  objects, and it returns an expression, because you can rename things
   --  like "X.all(J).Y". Renamings of entries and subprograms can also be
   --  expressions, but those use different mechanisms; the fields here are not
   --  used.

   function Alias (N : Entity_Id) return Entity_Id with Inline;
   procedure Set_Alias (N : Entity_Id; Val : Entity_Id) with Inline;
   function Renamed_Entity (N : Entity_Id) return Entity_Id with Inline;
   procedure Set_Renamed_Entity (N : Entity_Id; Val : Entity_Id) with Inline;
   function Renamed_Object (N : Entity_Id) return Node_Id with Inline;
   procedure Set_Renamed_Object (N : Entity_Id; Val : Node_Id) with Inline;

   function Renamed_Entity_Or_Object (N : Entity_Id) return Node_Id
     with Inline;
   --  This getter is used when we don't know statically whether we want to
   --  call Renamed_Entity or Renamed_Object.

   procedure Set_Renamed_Object_Of_Possibly_Void
     (N : Entity_Id; Val : Node_Id) with Inline;
   --  Set_Renamed_Object doesn't allow Void; this is used in the rare cases
   --  where we set the field of an entity that might be Void. It might be a
   --  good idea to get rid of calls to this.

   -------------------
   -- Type Synonyms --
   -------------------

   --  The following type synonyms are used to tidy up the function and
   --  procedure declarations that follow. Note that E and N have predicates
   --  ensuring the correct kind; we use Entity_Id or Node_Id when the
   --  predicates can't be satisfied.

   subtype B is Boolean;
   subtype C is Component_Alignment_Kind;
   subtype E is N_Entity_Id;
   subtype F is Float_Rep_Kind;
   subtype M is Mechanism_Type;
   subtype N is Node_Id with Predicate => N /= Empty and then N not in E;
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

   function Is_Access_Object_Type               (Id : E) return B with Inline;
   function Is_Access_Type                      (Id : E) return B with Inline;
   function Is_Access_Protected_Subprogram_Type (Id : E) return B with Inline;
   function Is_Access_Subprogram_Type           (Id : E) return B with Inline;
   function Is_Address_Compatible_Type          (Id : E) return B with Inline;
   --  Check whether the type represents an address
   function Is_Aggregate_Type                   (Id : E) return B with Inline;
   function Is_Anonymous_Access_Type            (Id : E) return B with Inline;
   function Is_Array_Type                       (Id : E) return B with Inline;
   function Is_Assignable                       (Id : E) return B with Inline;
   function Is_Class_Wide_Type                  (Id : E) return B with Inline;
   function Is_Composite_Type                   (Id : E) return B with Inline;
   function Is_Concurrent_Body                  (Id : E) return B with Inline;
   function Is_Concurrent_Type                  (Id : E) return B with Inline;
   function Is_Decimal_Fixed_Point_Type         (Id : E) return B with Inline;
   function Is_Digits_Type                      (Id : E) return B with Inline;
   function Is_Discrete_Or_Fixed_Point_Type     (Id : E) return B with Inline;
   function Is_Discrete_Type                    (Id : E) return B with Inline;
   function Is_Elementary_Type                  (Id : E) return B with Inline;
   function Is_Entry                            (Id : E) return B with Inline;
   function Is_Enumeration_Type                 (Id : E) return B with Inline;
   function Is_Fixed_Point_Type                 (Id : E) return B with Inline;
   function Is_Floating_Point_Type              (Id : E) return B with Inline;
   function Is_Formal                           (Id : E) return B with Inline;
   function Is_Formal_Object                    (Id : E) return B with Inline;
   function Is_Generic_Subprogram               (Id : E) return B with Inline;
   function Is_Generic_Unit                     (Id : E) return B with Inline;
   function Is_Ghost_Entity                     (Id : E) return B with Inline;
   function Is_Incomplete_Or_Private_Type       (Id : E) return B with Inline;
   function Is_Incomplete_Type                  (Id : E) return B with Inline;
   function Is_Integer_Type                     (Id : E) return B with Inline;
   function Is_Modular_Integer_Type             (Id : E) return B with Inline;
   function Is_Named_Access_Type                (Id : E) return B with Inline;
   function Is_Named_Number                     (Id : E) return B with Inline;
   function Is_Numeric_Type                     (Id : E) return B with Inline;
   function Is_Object                           (Id : E) return B with Inline;
   function Is_Ordinary_Fixed_Point_Type        (Id : E) return B with Inline;
   function Is_Overloadable                     (Id : E) return B with Inline;
   function Is_Private_Type                     (Id : E) return B with Inline;
   function Is_Protected_Type                   (Id : E) return B with Inline;
   function Is_Real_Type                        (Id : E) return B with Inline;
   function Is_Record_Type                      (Id : E) return B with Inline;
   function Is_Scalar_Type                      (Id : E) return B with Inline;
   function Is_Signed_Integer_Type              (Id : E) return B with Inline;
   function Is_Subprogram                       (Id : E) return B with Inline;
   function Is_Subprogram_Or_Entry              (Id : E) return B with Inline;
   function Is_Subprogram_Or_Generic_Subprogram (Id : E) return B with Inline;
   function Is_Task_Type                        (Id : E) return B with Inline;
   function Is_Type                             (Id : E) return B with Inline;

   -------------------------------------
   -- Synthesized Attribute Functions --
   -------------------------------------

   --  The functions in this section synthesize attributes from the tree,
   --  so they do not correspond to defined fields in the entity itself.

   function Address_Clause (Id : E) return Node_Id with Inline;
   function Aft_Value (Id : E) return U;
   function Alignment_Clause (Id : E) return Node_Id with Inline;
   function Base_Type (Id : E) return E with Inline;
   function Declaration_Node (Id : E) return Node_Id;
   function Designated_Type (Id : E) return E;
   function Entry_Index_Type (Id : E) return E;
   function First_Component (Id : E) return Entity_Id;
   function First_Component_Or_Discriminant (Id : E) return Entity_Id;
   function First_Formal (Id : E) return Entity_Id;
   function First_Formal_With_Extras (Id : E) return Entity_Id;

   function Float_Rep
     (N : Entity_Id) return F with Inline, Pre =>
      N in E_Void_Id
         | Float_Kind_Id;
   procedure Set_Float_Rep
     (Ignore_N : Entity_Id; Ignore_Val : F) with Inline, Pre =>
      Ignore_N in E_Void_Id
         | Float_Kind_Id;

   function Has_Attach_Handler (Id : E) return B;
   function Has_DIC (Id : E) return B;
   function Has_Entries (Id : E) return B;
   function Has_Foreign_Convention (Id : E) return B with Inline;
   function Has_Interrupt_Handler (Id : E) return B;
   function Has_Invariants (Id : E) return B;
   function Has_Limited_View (Id : E) return B;
   function Has_Non_Limited_View (Id : E) return B with Inline;
   function Has_Non_Null_Abstract_State (Id : E) return B;
   function Has_Non_Null_Visible_Refinement (Id : E) return B;
   function Has_Null_Abstract_State (Id : E) return B;
   function Has_Null_Visible_Refinement (Id : E) return B;
   function Implementation_Base_Type (Id : E) return E;
   function Is_Boolean_Type (Id : E) return B with Inline;
   function Is_Constant_Object (Id : E) return B with Inline;
   function Is_Controlled (Id : E) return B with Inline;
   function Is_Discriminal (Id : E) return B with Inline;
   function Is_Dynamic_Scope (Id : E) return B;
   function Is_Elaboration_Target (Id : E) return B;
   function Is_External_State (Id : E) return B;
   function Is_Full_Access (Id : E) return B with Inline;
   function Is_Null_State (Id : E) return B;
   function Is_Package_Or_Generic_Package (Id : E) return B with Inline;
   function Is_Packed_Array (Id : E) return B with Inline;
   function Is_Prival (Id : E) return B with Inline;
   function Is_Protected_Component (Id : E) return B with Inline;
   function Is_Protected_Interface (Id : E) return B;
   function Is_Protected_Record_Type (Id : E) return B with Inline;
   function Is_Standard_Character_Type (Id : E) return B;
   function Is_Standard_String_Type (Id : E) return B;
   function Is_String_Type (Id : E) return B with Inline;
   function Is_Synchronized_Interface (Id : E) return B;
   function Is_Synchronized_State (Id : E) return B;
   function Is_Task_Interface (Id : E) return B;
   function Is_Task_Record_Type (Id : E) return B with Inline;
   function Is_Wrapper_Package (Id : E) return B with Inline;
   function Last_Formal (Id : E) return Entity_Id;
   function Machine_Emax_Value (Id : E) return U;
   function Machine_Emin_Value (Id : E) return U;
   function Machine_Mantissa_Value (Id : E) return U;
   function Machine_Radix_Value (Id : E) return U;
   function Model_Emin_Value (Id : E) return U;
   function Model_Epsilon_Value (Id : E) return R;
   function Model_Mantissa_Value (Id : E) return U;
   function Model_Small_Value (Id : E) return R;
   function Next_Component (Id : E) return Entity_Id;
   function Next_Component_Or_Discriminant (Id : E) return Entity_Id;
   function Next_Discriminant (Id : E) return Entity_Id;
   function Next_Formal (Id : E) return Entity_Id;
   function Next_Formal_With_Extras (Id : E) return Entity_Id;
   function Next_Index (Id : N) return Node_Id;
   function Next_Literal (Id : E) return Entity_Id;
   function Next_Stored_Discriminant (Id : E) return Entity_Id;
   function Number_Dimensions (Id : E) return Pos;
   function Number_Entries (Id : E) return Nat;
   function Number_Formals (Id : E) return Nat;
   function Object_Size_Clause (Id : E) return Node_Id;
   function Parameter_Mode (Id : E) return Formal_Kind;
   function Partial_Refinement_Constituents (Id : E) return L;
   function Primitive_Operations (Id : E) return L;
   function Root_Type (Id : E) return E;
   function Safe_Emax_Value (Id : E) return U;
   function Safe_First_Value (Id : E) return R;
   function Safe_Last_Value (Id : E) return R;
   function Size_Clause (Id : E) return Node_Id with Inline;
   function Stream_Size_Clause (Id : E) return N with Inline;
   function Type_High_Bound (Id : E) return N with Inline;
   function Type_Low_Bound (Id : E) return N with Inline;
   function Underlying_Type (Id : E) return Entity_Id;

   function Scope_Depth (Id : Scope_Kind_Id) return U with Inline;
   function Scope_Depth_Set (Id : Scope_Kind_Id) return B with Inline;

   function Scope_Depth_Default_0 (Id : Scope_Kind_Id) return U;
   --  In rare cases, the Scope_Depth_Value (queried by Scope_Depth) is
   --  not correctly set before querying it; this may be used instead of
   --  Scope_Depth in such cases. It returns Uint_0 if the Scope_Depth_Value
   --  has not been set. See documentation in Einfo.

   ------------------------------------------
   -- Type Representation Attribute Fields --
   ------------------------------------------

   function Known_Alignment (E : Entity_Id) return B with Inline;
   procedure Reinit_Alignment (Id : E) with Inline;
   procedure Copy_Alignment (To, From : E);

   function Known_Component_Bit_Offset (E : Entity_Id) return B with Inline;
   function Known_Static_Component_Bit_Offset (E : Entity_Id) return B
     with Inline;

   function Known_Component_Size (E : Entity_Id) return B with Inline;
   function Known_Static_Component_Size (E : Entity_Id) return B with Inline;

   function Known_Esize (E : Entity_Id) return B with Inline;
   function Known_Static_Esize (E : Entity_Id) return B with Inline;
   procedure Reinit_Esize (Id : E) with Inline;
   procedure Copy_Esize (To, From : E);

   function Known_Normalized_First_Bit (E : Entity_Id) return B with Inline;
   function Known_Static_Normalized_First_Bit (E : Entity_Id) return B
     with Inline;

   function Known_Normalized_Position (E : Entity_Id) return B with Inline;
   function Known_Static_Normalized_Position (E : Entity_Id) return B
     with Inline;

   function Known_RM_Size (E : Entity_Id) return B with Inline;
   function Known_Static_RM_Size (E : Entity_Id) return B with Inline;
   procedure Reinit_RM_Size (Id : E) with Inline;
   procedure Copy_RM_Size (To, From : E);

   --  NOTE: "known" here does not mean "known at compile time". It means that
   --  the compiler has computed the value of the field (either by default, or
   --  by noting some representation clauses), and the field has not been
   --  reinitialized.
   --
   --  We document the Esize functions here; the others above are analogous:
   --
   --     Known_Esize: True if Set_Esize has been called without a subsequent
   --     Reinit_Esize.
   --
   --     Known_Static_Esize: True if Known_Esize and the Esize is known at
   --     compile time. (We're not using "static" in the Ada RM sense here. We
   --     are using it to mean "known at compile time".)
   --
   --     Reinit_Esize: Set the Esize field to its initial unknown state.
   --
   --     Copy_Esize: Copies the Esize from From to To; Known_Esize (From) may
   --     be False, in which case Known_Esize (To) becomes False.
   --
   --     Esize: This is the normal automatically-generated getter for Esize,
   --     declared elsewhere. Returns No_Uint if not Known_Esize.
   --
   --     Set_Esize: This is the normal automatically-generated setter for
   --     Esize. After a call to this, Known_Esize is True. It is an error
   --     to call this with a No_Uint value.
   --
   --  Normally, we call Set_Esize first, and then query Esize (and similarly
   --  for other fields). However in some cases, we need to check Known_Esize
   --  before calling Esize, because the code is written in such a way that we
   --  don't know whether Set_Esize has already been called.
   --
   --  In two cases, Known_Static_Esize and Known_Static_RM_Size, there is one
   --  more consideration, which is that we always return False for generic
   --  types. Within a template, the size can look Known_Static, because of the
   --  fake size values we put in template types, but they are not really
   --  Known_Static and anyone testing if they are Known_Static within the
   --  template should get False as a result to prevent incorrect assumptions.

   ---------------------------------------------------------
   -- Procedures for setting multiple of the above fields --
   ---------------------------------------------------------

   procedure Reinit_Component_Location (Id : E);
   --  Initializes all fields describing the location of a component
   --  (Normalized_Position, Component_Bit_Offset, Normalized_First_Bit,
   --  Esize) to all be Unknown.

   procedure Init_Size (Id : E; V : Int);
   --  Initialize both the Esize and RM_Size fields of E to V

   procedure Reinit_Size_Align (Id : E);
   --  This procedure initializes both size fields and the alignment
   --  field to all be Unknown.

   procedure Reinit_Object_Size_Align (Id : E);
   --  Same as Reinit_Size_Align except RM_Size field (which is only for types)
   --  is unaffected.

   ---------------------------------------------------
   -- Access to Subprograms in Subprograms_For_Type --
   ---------------------------------------------------

   --  Now that we have variable-sized nodes, it might be possible to replace
   --  the following with regular fields, and get rid of the flags used to mark
   --  these kinds of subprograms.

   function Is_Partial_DIC_Procedure             (Id : E) return B;

   function DIC_Procedure                        (Id : E) return Entity_Id;
   function Partial_DIC_Procedure                (Id : E) return Entity_Id;
   function Invariant_Procedure                  (Id : E) return Entity_Id;
   function Partial_Invariant_Procedure          (Id : E) return Entity_Id;
   function Predicate_Function                   (Id : E) return Entity_Id;

   procedure Set_DIC_Procedure                   (Id : E; V : E);
   procedure Set_Partial_DIC_Procedure           (Id : E; V : E);
   procedure Set_Invariant_Procedure             (Id : E; V : E);
   procedure Set_Partial_Invariant_Procedure     (Id : E; V : E);
   procedure Set_Predicate_Function              (Id : E; V : E);

   ---------------
   -- Iterators --
   ---------------

   --  Next_xxx (obj) is equivalent to obj := Next_xxx (obj)

   procedure Next_Component                 (N : in out Node_Id) with Inline;
   procedure Next_Component_Or_Discriminant (N : in out Node_Id) with Inline;
   procedure Next_Discriminant              (N : in out Node_Id) with Inline;
   procedure Next_Formal                    (N : in out Node_Id) with Inline;
   procedure Next_Formal_With_Extras        (N : in out Node_Id) with Inline;
   procedure Next_Index                     (N : in out Node_Id) with Inline;
   procedure Next_Inlined_Subprogram        (N : in out Node_Id) with Inline;
   procedure Next_Literal                   (N : in out Node_Id) with Inline;
   procedure Next_Stored_Discriminant       (N : in out Node_Id) with Inline;

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
   --    Always_Terminates
   --    Async_Readers
   --    Async_Writers
   --    Attach_Handler
   --    Constant_After_Elaboration
   --    Contract_Cases
   --    Depends
   --    Effective_Reads
   --    Effective_Writes
   --    Exceptional_Cases
   --    Exit_Cases
   --    Extensions_Visible
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
   --    Side_Effects
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

   function Is_Base_Type (Id : E) return B with Inline;
   --  Return True for a type entity and False for a subtype entity. Note that
   --  this returns True for nontypes.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Is_Entity_Name (N : Node_Id) return Boolean with Inline;
   --  Test if the node N is the name of an entity (i.e. is an identifier,
   --  expanded name, or an attribute reference that returns an entity).

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   procedure Link_Entities (First, Second : Entity_Id);
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

end Einfo.Utils;
