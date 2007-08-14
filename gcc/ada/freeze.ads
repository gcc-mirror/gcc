------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               F R E E Z E                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Freeze is

   --------------------------
   -- Handling of Freezing --
   --------------------------

   --  In the formal Ada semantics, freezing of entities occurs at a well
   --  defined point, described in (RM 13.14). The model in GNAT of freezing
   --  is that a Freeze_Entity node is generated at the point where an entity
   --  is frozen, and the entity contains a pointer (Freeze_Node) to this
   --  generated freeze node.

   --  The freeze node is processed in the expander to generate associated
   --  data and subprograms (e.g. an initialization procedure) which must
   --  be delayed until the type is frozen and its representation can be
   --  fully determined. Subsequently the freeze node is used by Gigi to
   --  determine the point at which it should elaborate the corresponding
   --  entity (this elaboration also requires the representation of the
   --  entity to be fully determinable). The freeze node is also used to
   --  provide additional diagnostic information (pinpointing the freeze
   --  point), when order of freezing errors are detected.

   --  If we were fully faithful to the Ada model, we would generate freeze
   --  nodes for all entities, but that is a bit heavy so we optimize (that
   --  is the nice word) or cut corners (which is a bit more honest). For
   --  many entities, we do not need to delay the freeze and instead can
   --  freeze them at the point of declaration. The conditions for this
   --  early freezing being permissible are as follows:

   --    There is no associated expander activity that needs to be delayed

   --    Gigi can fully elaborate the entity at the point of occurrence (or,
   --    equivalently, no real elaboration is required for the entity).

   --  In order for these conditions to be met (especially the second), it
   --  must be the case that all representation characteristics of the entity
   --  can be determined at declaration time.

   --  The following indicates how freezing is handled for all entity kinds:

   --    Types

   --      All declared types have freeze nodes, as well as anonymous base
   --      types created for type declarations where the defining identifier
   --      is a first subtype of the anonymous type.

   --    Subtypes

   --      All first subtypes have freeze nodes. Other subtypes need freeze
   --      nodes if the corresponding base type has not yet been frozen. If
   --      the base type has been frozen, then there is no need for a freeze
   --      node, since no rep clauses can appear for the subtype in any case.

   --    Implicit types and subtypes

   --      As noted above, implicit base types always have freeze nodes. Other
   --      implicit types and subtypes typically do not require freeze nodes,
   --      because there is no possibility of delaying any information about
   --      their representation.

   --    Subprograms
   --
   --      Are frozen at the point of declaration unless one or more of the
   --      formal types or return type themselves have delayed freezing and
   --      are not yet frozen. This includes the case of a formal access type
   --      where the designated type is not frozen. Note that we are talking
   --      about subprogram specs here (subprogram body entities have no
   --      relevance), and in any case, subprogram bodies freeze everything.

   --    Objects with dynamic address clauses
   --
   --      These have a delayed freeze. Gigi will generate code to evaluate
   --      the initialization expression if present and store it in a temp.
   --      The actual object is created at the point of the freeze, and if
   --      necessary initialized by copying the value of this temporary.

   --    Formal Parameters
   --
   --      Are frozen when the associated subprogram is frozen, so there is
   --      never any need for them to have delayed freezing.

   --    Other Objects
   --
   --      Are always frozen at the point of declaration

   --    All Other Entities

   --      Are always frozen at the point of declaration

   --  The flag Has_Delayed_Freeze is used for to indicate that delayed
   --  freezing is required. Usually the associated freeze node is allocated
   --  at the freezing point. One special exception occurs with anonymous
   --  base types, where the freeze node is preallocated at the point of
   --  declaration, so that the First_Subtype_Link field can be set.

   Freezing_Library_Level_Tagged_Type : Boolean := False;
   --  Flag used to indicate that we are freezing the primitives of a library
   --  level tagged types. Used to disable checks on premature freezing.
   --  More documentation needed??? why is this flag needed? what are these
   --  checks? why do they need disabling in some cases?

   -----------------
   -- Subprograms --
   -----------------

   function Build_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id) return Node_Id;
   --  Rewrite renaming declaration as a subprogram body, whose single
   --  statement is a call to the renamed entity. New_S is the entity that
   --  appears in the renaming declaration. If this is a Renaming_As_Body,
   --  then Decl is the original subprogram declaration that is completed
   --  by the renaming, otherwise it is the renaming declaration itself.
   --  The caller inserts the body where required. If this call comes
   --  from a freezing action, the resulting body is analyzed at once.

   procedure Check_Compile_Time_Size (T : Entity_Id);
   --  Check to see whether the size of the type T is known at compile time.
   --  There are three possible cases:
   --
   --    Size is not known at compile time. In this case, the call has no
   --    effect. Note that the processing is conservative here, in the sense
   --    that this routine may decide that the size is not known even if in
   --    fact Gigi decides it is known, but the opposite situation can never
   --    occur.
   --
   --    Size is known at compile time, but the actual value of the size is
   --    not known to the front end or is definitely 32 or more. In this case
   --    Size_Known_At_Compile_Time is set, but the Esize field is left set
   --    to zero (to be set by Gigi).
   --
   --    Size is known at compile time, and the actual value of the size is
   --    known to the front end and is less than 32. In this case, the flag
   --    Size_Known_At_Compile_Time is set, and in addition Esize is set to
   --    the required size, allowing for possible front end packing of an
   --    array using this type as a component type.
   --
   --  Note: the flag Size_Known_At_Compile_Time is used to determine if the
   --  secondary stack must be used to return a value of the type, and also
   --  to determine whether a component clause is allowed for a component
   --  of the given type.
   --
   --  Note: this is public because of one dubious use in Sem_Res???
   --
   --  Note: Check_Compile_Time_Size does not test the case of the size being
   --  known because a size clause is specifically given. That is because we
   --  do not allow a size clause if the size would not otherwise be known at
   --  compile time in any case.

   procedure Expand_Atomic_Aggregate (E : Entity_Id; Typ : Entity_Id);
   --  If an atomic object is initialized with an aggregate or is assigned
   --  an aggregate, we have to prevent a piecemeal access or assignment
   --  to the object, even if the aggregate is to be expanded. we create
   --  a temporary for the aggregate, and assign the temporary instead,
   --  so that the back end can generate an atomic move for it.

   function Freeze_Entity (E : Entity_Id; Loc : Source_Ptr) return List_Id;
   --  Freeze an entity, and return Freeze nodes, to be inserted at the
   --  point of call. Loc is a source location which corresponds to the
   --  freeze point. This is used in placing warning messages in the
   --  situation where it appears that a type has been frozen too early,
   --  e.g. when a primitive operation is declared after the freezing
   --  point of its tagged type. Returns No_List if no freeze nodes needed.

   procedure Freeze_All (From : Entity_Id; After : in out Node_Id);
   --  Before a non-instance body, or at the end of a declarative part
   --  freeze all entities therein that are not yet frozen. Calls itself
   --  recursively to catch types in inner packages that were not frozen
   --  at the inner level because they were not yet completely defined.
   --  This routine also analyzes and freezes default parameter expressions
   --  in subprogram specifications (this has to be delayed until all the
   --  types are frozen). The resulting freeze nodes are inserted just
   --  after node After (which is a list node) and analyzed. On return,
   --  'After' is updated to point to the last node inserted (or is returned
   --  unchanged if no nodes were inserted). 'From' is the last entity frozen
   --  in the scope. It is used to prevent a quadratic traversal over already
   --  frozen entities.

   procedure Freeze_Before (N : Node_Id; T : Entity_Id);
   --  Freeze T then Insert the generated Freeze nodes before the node N

   procedure Freeze_Expression (N : Node_Id);
   --  Freezes the required entities when the Expression N causes freezing.
   --  The node N here is either a subexpression node (a "real" expression)
   --  or a subtype mark, or a subtype indication. The latter two cases are
   --  not really expressions, but they can appear within expressions and
   --  so need to be similarly treated. Freeze_Expression takes care of
   --  determining the proper insertion point for generated freeze actions.

   procedure Freeze_Fixed_Point_Type (Typ : Entity_Id);
   --  Freeze fixed point type. For fixed-point types, we have to defer
   --  setting the size and bounds till the freeze point, since they are
   --  potentially affected by the presence of size and small clauses.

   procedure Freeze_Itype (T : Entity_Id; N : Node_Id);
   --  This routine is called when an Itype is created and must be frozen
   --  immediately at the point of creation (for the sake of the expansion
   --  activities in Exp_Ch3 (for example, the creation of packed array
   --  types). We can't just let Freeze_Expression do this job since it
   --  goes out of its way to make sure that the freeze node occurs at a
   --  point outside the current construct, e.g. outside the expression or
   --  outside the initialization procedure. That's normally right, but
   --  not in this case, since if we create an Itype in an expression it
   --  may be the case that it is not always elaborated (for example it
   --  may result from the right operand of a short circuit). In this case
   --  we want the freeze node to be inserted at the same point as the Itype.
   --  The node N provides both the location for the freezing and also the
   --  insertion point for the resulting freeze nodes.

end Freeze;
