------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 3                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

with Nlists; use Nlists;
with Types;  use Types;

package Sem_Ch3  is
   procedure Analyze_Component_Declaration              (N : Node_Id);
   procedure Analyze_Incomplete_Type_Decl               (N : Node_Id);
   procedure Analyze_Itype_Reference                    (N : Node_Id);
   procedure Analyze_Number_Declaration                 (N : Node_Id);
   procedure Analyze_Object_Declaration                 (N : Node_Id);
   procedure Analyze_Others_Choice                      (N : Node_Id);
   procedure Analyze_Private_Extension_Declaration      (N : Node_Id);
   procedure Analyze_Subtype_Declaration                (N : Node_Id);
   procedure Analyze_Subtype_Indication                 (N : Node_Id);
   procedure Analyze_Type_Declaration                   (N : Node_Id);
   procedure Analyze_Variant_Part                       (N : Node_Id);

   function Access_Definition
     (Related_Nod : Node_Id;
      N           : Node_Id)
      return        Entity_Id;
   --  An access definition defines a general access type for a formal
   --  parameter.  The procedure is called when processing formals, when
   --  the current scope is the subprogram. The Implicit type is attached
   --  to the Related_Nod put into the enclosing scope, so that the only
   --  entities defined in the spec are the formals themselves.

   procedure Access_Subprogram_Declaration
     (T_Name : Entity_Id;
      T_Def  : Node_Id);
   --  The subprogram specification yields the signature of an implicit
   --  type, whose Ekind is Access_Subprogram_Type. This implicit type is
   --  the designated type of the declared access type. In subprogram calls,
   --  the signature of the implicit type works like the profile of a regular
   --  subprogram.

   procedure Analyze_Declarations (L : List_Id);
   --  Called to analyze a list of declarations (in what context ???). Also
   --  performs necessary freezing actions (more description needed ???)

   procedure Analyze_Default_Expression (N : Node_Id; T : Entity_Id);
   --  Default expressions do not freeze their components, and must be
   --  analyzed and resolved accordingly, by calling the
   --  Pre_Analyze_And_Resolve routine and setting the global
   --  In_Default_Expression flag.

   procedure Array_Type_Declaration (T : in out Entity_Id; Def : Node_Id);
   --  Process an array type declaration. If the array is constrained, we
   --  create an implicit parent array type, with the same index types and
   --  component type.

   procedure Access_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Process an access type declaration

   procedure Check_Abstract_Overriding (T : Entity_Id);
   --  Check that all abstract subprograms inherited from T's parent type
   --  have been overridden as required, and that nonabstract subprograms
   --  have not been incorrectly overridden with an abstract subprogram.

   procedure Check_Aliased_Component_Types (T : Entity_Id);
   --  Given an array type or record type T, check that if the type is
   --  nonlimited, then the nominal subtype of any components of T
   --  that have discriminants must be constrained.

   procedure Check_Completion (Body_Id : Node_Id := Empty);
   --  At the end of a declarative part, verify that all entities that
   --  require completion have received one. If Body_Id is absent, the
   --  error indicating a missing completion is placed on the declaration
   --  that needs completion. If Body_Id is present, it is the defining
   --  identifier of a package body, and errors are posted on that node,
   --  rather than on the declarations that require completion in the package
   --  declaration.

   procedure Derive_Subprogram
     (New_Subp       : in out Entity_Id;
      Parent_Subp    : Entity_Id;
      Derived_Type   : Entity_Id;
      Parent_Type    : Entity_Id;
      Actual_Subp    : Entity_Id := Empty);
   --  Derive the subprogram Parent_Subp from Parent_Type, and replace the
   --  subsidiary subtypes with the derived type to build the specification
   --  of the inherited subprogram (returned in New_Subp). For tagged types,
   --  the derived subprogram is aliased to that of the actual (in the
   --  case where Actual_Subp is nonempty) rather than to the corresponding
   --  subprogram of the parent type.

   procedure Derive_Subprograms
     (Parent_Type    : Entity_Id;
      Derived_Type   : Entity_Id;
      Generic_Actual : Entity_Id := Empty);
   --  To complete type derivation, collect or retrieve the primitive
   --  operations of the parent type, and replace the subsidiary subtypes
   --  with the derived type, to build the specs of the inherited ops.
   --  For generic actuals, the mapping of the primitive operations to those
   --  of the parent type is also done by rederiving the operations within
   --  the instance. For tagged types, the derived subprograms are aliased to
   --  those of the actual, not those of the ancestor.

   function Expand_To_Girder_Constraint
     (Typ        : Entity_Id;
      Constraint : Elist_Id)
      return       Elist_Id;
   --  Given a Constraint (ie a list of expressions) on the discriminants of
   --  Typ, expand it into a constraint on the girder discriminants and
   --  return the new list of expressions constraining the girder
   --  discriminants.

   function Find_Type_Name (N : Node_Id) return Entity_Id;
   --  Enter the identifier in a type definition, or find the entity already
   --  declared, in the case of the full declaration of an incomplete or
   --  private type.

   function Get_Discriminant_Value
     (Discriminant         : Entity_Id;
      Typ_For_Constraint   : Entity_Id;
      Constraint           : Elist_Id)
      return                 Node_Id;
   --  ??? MORE DOCUMENTATION
   --  Given a discriminant somewhere in the Typ_For_Constraint tree
   --  and a Constraint, return the value of that discriminant.

   function Is_Visible_Component (C : Entity_Id) return Boolean;
   --  Determines if a record component C is visible in the present context.
   --  Note that even though component C could appear in the entity chain
   --  of a record type, C may not be visible in the current context. For
   --  instance, C may be a component inherited in the full view of a private
   --  extension which is not visible in the current context.

   procedure Make_Index
     (I            : Node_Id;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id := Empty;
      Suffix_Index : Nat := 1);
   --  Process an index that is given in an array declaration, an entry
   --  family declaration or a loop iteration. The index is given by an
   --  index declaration (a 'box'), or by a discrete range. The later can
   --  be the name of a discrete type, or a subtype indication.
   --  Related_Nod is the node where the potential generated implicit types
   --  will be inserted. The 2 last parameters are used for creating the name.

   procedure Make_Class_Wide_Type (T : Entity_Id);
   --  A Class_Wide_Type is created for each tagged type definition. The
   --  attributes of a class wide type are inherited from those of the type
   --  T. If T is introduced by a private declaration, the corresponding
   --  class wide type is created at the same time, and therefore there is
   --  a private and a full declaration for the class wide type type as well.

   procedure Process_Full_View (N : Node_Id; Full_T, Priv_T : Entity_Id);
   --  Process some semantic actions when the full view of a private type is
   --  encountered and analyzed. The first action is to create the full views
   --  of the dependent private subtypes. The second action is to recopy the
   --  primitive operations of the private view (in the tagged case).
   --  N is the N_Full_Type_Declaration node.

   --    Full_T is the full view of the type whose full declaration is in N.
   --
   --    Priv_T is the private view of the type whose full declaration is in N.

   procedure Process_Range_Expr_In_Decl
     (R           : Node_Id;
      T           : Entity_Id;
      Check_List  : List_Id := Empty_List;
      R_Check_Off : Boolean := False);
   --  Process a range expression that appears in a declaration context. The
   --  range is analyzed and resolved with the base type of the given type,
   --  and an appropriate check for expressions in non-static contexts made
   --  on the bounds. R is analyzed and resolved using T, so the caller should
   --  if necessary link R into the tree before the call, and in particular in
   --  the case of a subtype declaration, it is appropriate to set the parent
   --  pointer of R so that the types get properly frozen. The Check_List
   --  parameter is used when the subprogram is called from
   --  Build_Record_Init_Proc and is used to return a set of constraint
   --  checking statements generated by the Checks package. R_Check_Off is
   --  set to True when the call to Range_Check is to be skipped.

   function Process_Subtype
     (S           : Node_Id;
      Related_Nod : Node_Id;
      Related_Id  : Entity_Id := Empty;
      Suffix      : Character := ' ')
      return        Entity_Id;
   --  Process a subtype indication S and return corresponding entity.
   --  Related_Nod is the node where the potential generated implicit types
   --  will be inserted. The Related_Id and Suffix parameters are used to
   --  build the associated Implicit type name.

   procedure Process_Discriminants (N : Node_Id);
   --  Process the discriminants contained in an N_Full_Type_Declaration or
   --  N_Incomplete_Type_Decl node N.

   procedure Set_Completion_Referenced (E : Entity_Id);
   --  If E is the completion of a private or incomplete  type declaration,
   --  or the completion of a deferred constant declaration, mark the entity
   --  as referenced. Warnings on unused entities, if needed, go on the
   --  partial view.

   procedure Set_Girder_Constraint_From_Discriminant_Constraint
     (E : Entity_Id);
   --  E is some record type. This routine computes E's Girder_Constraint
   --  from its Discriminant_Constraint.

end Sem_Ch3;
