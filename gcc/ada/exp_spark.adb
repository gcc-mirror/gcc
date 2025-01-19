------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            E X P _ S P A R K                             --
--                                                                          --
--                                 B o d y                                  --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Checks;         use Checks;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Exp_Attr;
with Exp_Ch3;
with Exp_Ch4;
with Exp_Ch5;        use Exp_Ch5;
with Exp_Dbug;       use Exp_Dbug;
with Exp_Util;       use Exp_Util;
with Ghost;          use Ghost;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aggr;       use Sem_Aggr;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch7;        use Sem_Ch7;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;

package body Exp_SPARK is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_SPARK_N_Aggregate (N : Node_Id);
   --  Perform specific expansion of container aggregates, to ensure suitable
   --  checking of expressions.

   procedure Expand_SPARK_N_Attribute_Reference (N : Node_Id);
   --  Perform attribute-reference-specific expansion

   procedure Expand_SPARK_N_Delta_Aggregate (N : Node_Id);
   --  Perform delta-aggregate-specific expansion

   procedure Expand_SPARK_N_Freeze_Entity (N : Node_Id);
   --  Do a minimal expansion of freeze entities required by GNATprove. It is
   --  a subset of what is done for GNAT in Exp_Ch13.Expand_N_Freeze_Entity.
   --  Those two routines should be kept in sync.

   procedure Expand_SPARK_N_Loop_Statement (N : Node_Id);
   --  Perform loop-statement-specific expansion

   procedure Expand_SPARK_N_Object_Declaration (N : Node_Id);
   --  Perform object-declaration-specific expansion

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id);
   --  Perform name evaluation for a renamed object

   procedure Expand_SPARK_N_Op_Ne (N : Node_Id);
   --  Rewrite operator /= based on operator = when defined explicitly

   procedure Expand_SPARK_Delta_Or_Update (Typ : Entity_Id; Aggr : Node_Id);
   --  Common expansion for attribute Update and delta aggregates

   procedure SPARK_Freeze_Type (N : Node_Id);
   --  Do a minimal type freezing required by GNATprove. It is a subset of what
   --  is done for GNAT in Exp_Ch3.Freeze_Type. Those two routines should be
   --  kept in sync.
   --
   --  Currently in freezing we build the spec of dispatching equality. This
   --  spec is needed to properly resolve references to the equality operator.
   --  The body is not needed, because proof knows how to directly synthesize a
   --  logical meaning for it. Also, for tagged types with extension the
   --  expanded body would compare the _parent component, which is
   --  intentionally not generated in the GNATprove mode.
   --
   --  We build the DIC and Type_Invariant procedure bodies here as well.

   ------------------
   -- Expand_SPARK --
   ------------------

   procedure Expand_SPARK (N : Node_Id) is
   begin
      case Nkind (N) is

         --  Qualification of entity names in formal verification mode
         --  is limited to the addition of a suffix for homonyms (see
         --  Exp_Dbug.Qualify_Entity_Name). We used to qualify entity names
         --  as full expansion does, but this was removed as this prevents the
         --  verification back-end from using a short name for debugging and
         --  user interaction. The verification back-end already takes care
         --  of qualifying names when needed.

         when N_Block_Statement
            | N_Entry_Declaration
            | N_Package_Body
            | N_Package_Declaration
            | N_Protected_Type_Declaration
            | N_Subprogram_Body
            | N_Task_Type_Declaration
         =>
            Qualify_Entity_Names (N);

         --  Replace occurrences of System'To_Address by calls to
         --  System.Storage_Elements.To_Address.

         when N_Attribute_Reference =>
            Expand_SPARK_N_Attribute_Reference (N);

         when N_Delta_Aggregate =>
            Expand_SPARK_N_Delta_Aggregate (N);

         when N_Aggregate =>
            Expand_SPARK_N_Aggregate (N);

         when N_Expanded_Name
            | N_Identifier
         =>
            Expand_SPARK_Potential_Renaming (N);

         --  Loop iterations over arrays need to be expanded, to avoid getting
         --  two names referring to the same object in memory (the array and
         --  the iterator) in GNATprove, especially since both can be written
         --  (thus possibly leading to interferences due to aliasing). No such
         --  problem arises with quantified expressions over arrays, which are
         --  dealt with specially in GNATprove.

         when N_Loop_Statement =>
            Expand_SPARK_N_Loop_Statement (N);

         when N_Object_Declaration =>
            Expand_SPARK_N_Object_Declaration (N);

         when N_Object_Renaming_Declaration =>
            Expand_SPARK_N_Object_Renaming_Declaration (N);

         when N_Op_Ne =>
            Expand_SPARK_N_Op_Ne (N);

         when N_Freeze_Entity =>
            --  Currently we only expand type freeze entities, so ignore other
            --  freeze entites, because it is expensive to create a suitable
            --  freezing environment.

            if Is_Type (Entity (N)) then
               Expand_SPARK_N_Freeze_Entity (N);
            end if;

         --  In SPARK mode, no other constructs require expansion

         when others =>
            null;
      end case;
   end Expand_SPARK;

   ----------------------------------
   -- Expand_SPARK_Delta_Or_Update --
   ----------------------------------

   procedure Expand_SPARK_Delta_Or_Update
     (Typ  : Entity_Id;
      Aggr : Node_Id)
   is
      procedure Apply_Range_Checks (Choice : Node_Id);
      --  Apply range checks on indexes from a deep choice

      ------------------------
      -- Apply_Range_Checks --
      ------------------------

      procedure Apply_Range_Checks (Choice : Node_Id) is
         Pref  : Node_Id := Choice;
         Index : N_Subexpr_Id;
      begin
         loop
            if Nkind (Pref) = N_Indexed_Component then
               Index := First (Expressions (Pref));
               Apply_Scalar_Range_Check (Index, Etype (Index));

            elsif Is_Array_Type (Typ)
              and then Is_Root_Prefix_Of_Deep_Choice (Pref)
            then
               Index := Pref;
               Apply_Scalar_Range_Check (Index, Etype (Index));
            end if;

            exit when Is_Root_Prefix_Of_Deep_Choice (Pref);

            Pref := Prefix (Pref);
         end loop;
      end Apply_Range_Checks;

      --  Local variables

      Assoc     : Node_Id;
      Comp      : Node_Id;
      Comp_Type : Entity_Id;
      Expr      : Node_Id;
      Index     : Node_Id;
      Index_Typ : Entity_Id;
      New_Assoc : Node_Id;

   --  Start of processing for Expand_SPARK_Delta_Or_Update

   begin
      --  Apply scalar range checks on the updated components, if needed

      if Is_Array_Type (Typ) then

         --  Multidimensional arrays

         if Present (Next_Index (First_Index (Typ))) then
            Assoc := First (Component_Associations (Aggr));

            while Present (Assoc) loop
               Expr      := Expression (Assoc);
               Comp_Type := Component_Type (Typ);

               if Is_Scalar_Type (Comp_Type) then
                  Apply_Scalar_Range_Check (Expr, Comp_Type);
               end if;

               --  The current association contains a sequence of indexes
               --  denoting an element of a multidimensional array:
               --
               --    (Index_1, ..., Index_N)

               Expr := First (Choices (Assoc));

               pragma Assert (Nkind (Aggr) = N_Aggregate);

               while Present (Expr) loop
                  Index     := First (Expressions (Expr));
                  Index_Typ := First_Index (Typ);

                  while Present (Index_Typ) loop
                     Apply_Scalar_Range_Check (Index, Etype (Index_Typ));
                     Next (Index);
                     Next_Index (Index_Typ);
                  end loop;

                  Next (Expr);
               end loop;

               Next (Assoc);
            end loop;

         --  One-dimensional arrays

         else
            Assoc := First (Component_Associations (Aggr));

            while Present (Assoc) loop
               Expr      := Expression (Assoc);
               Comp_Type := Component_Type (Typ);

               --  Analyze expression of the iterated_component_association
               --  with its index parameter in scope.

               if Nkind (Assoc) = N_Iterated_Component_Association then
                  Push_Scope (Scope (Defining_Identifier (Assoc)));
                  Enter_Name (Defining_Identifier (Assoc));
                  Analyze_And_Resolve (Expr, Comp_Type);
               end if;

               if Is_Scalar_Type (Comp_Type) then
                  Apply_Scalar_Range_Check (Expr, Comp_Type);
               end if;

               --  Restore scope of the iterated_component_association

               if Nkind (Assoc) = N_Iterated_Component_Association then
                  End_Scope;
               end if;

               Index     := First (Choice_List (Assoc));
               Index_Typ := First_Index (Typ);

               while Present (Index) loop
                  --  If the index denotes a range of elements or a constrained
                  --  subtype indication, then their low and high bounds
                  --  already have range checks applied.

                  if Nkind (Index) in N_Range | N_Subtype_Indication then
                     null;

                  elsif Is_Deep_Choice (Index, Typ) then
                     Apply_Range_Checks (Index);

                  --  Otherwise the index denotes a single expression where
                  --  range checks need to be applied or a subtype name
                  --  (without range constraints) where applying checks is
                  --  harmless.
                  --
                  --  In delta_aggregate and Update attribute on array the
                  --  others_choice is not allowed.

                  else pragma Assert (Nkind (Index) in N_Subexpr);
                     Apply_Scalar_Range_Check (Index, Etype (Index_Typ));
                  end if;

                  Next (Index);
               end loop;

               Next (Assoc);
            end loop;
         end if;

      else pragma Assert (Is_Record_Type (Typ));

         --  If the aggregate has multiple component choices, e.g.:
         --
         --    X'Update (A | B | C => 123)
         --
         --  then each component might be of a different type and might or
         --  might not require a range check. We first rewrite associations
         --  into single-component choices, e.g.:
         --
         --    X'Update (A => 123, B => 123, C => 123)
         --
         --  and then apply range checks to individual copies of the
         --  expressions. We do the same for delta aggregates, accordingly.

         --  Iterate over associations of the original aggregate

         Assoc := First (Component_Associations (Aggr));

         --  Rewrite into a new aggregate and decorate

         case Nkind (Aggr) is
            when N_Aggregate =>
               Rewrite
                 (Aggr,
                  Make_Aggregate
                    (Sloc                   => Sloc (Aggr),
                     Component_Associations => New_List));

            when N_Delta_Aggregate =>
               Rewrite
                 (Aggr,
                  Make_Delta_Aggregate
                    (Sloc                   => Sloc (Aggr),
                     Expression             => Expression (Aggr),
                     Component_Associations => New_List));

            when others =>
               raise Program_Error;
         end case;

         Set_Etype (Aggr, Typ);

         --  Populate the new aggregate with component associations

         while Present (Assoc) loop
            Expr := Expression (Assoc);
            Comp := First (Choices (Assoc));

            while Present (Comp) loop
               if Is_Deep_Choice (Comp, Typ) then
                  Comp_Type := Etype (Comp);
               else
                  Comp_Type := Etype (Entity (Comp));
               end if;

               New_Assoc :=
                 Make_Component_Association
                   (Sloc       => Sloc (Assoc),
                    Choices    => New_List (New_Copy_Tree (Comp)),
                    Expression => New_Copy_Tree (Expr));

               --  New association must be attached to the aggregate before we
               --  analyze it.

               Append (New_Assoc, Component_Associations (Aggr));

               Analyze_And_Resolve (Expression (New_Assoc), Comp_Type);

               if Is_Deep_Choice (Comp, Typ) then
                  Apply_Range_Checks (First (Choices (New_Assoc)));
               end if;

               if Is_Scalar_Type (Comp_Type) then
                  Apply_Scalar_Range_Check
                    (Expression (New_Assoc), Comp_Type);
               end if;

               Next (Comp);
            end loop;

            Next (Assoc);
         end loop;
      end if;
   end Expand_SPARK_Delta_Or_Update;

   ------------------------------
   -- Expand_SPARK_N_Aggregate --
   ------------------------------

   procedure Expand_SPARK_N_Aggregate (N : Node_Id) is

      --  Local subprograms

      procedure Parse_Named_Subp
        (Subp         : Subprogram_Kind_Id;
         Key_Type     : out Type_Kind_Id;
         Element_Type : out Type_Kind_Id);
      --  Retrieve key and element types from subprogram for named addition

      procedure Parse_Unnamed_Subp
        (Subp         : Subprogram_Kind_Id;
         Element_Type : out Type_Kind_Id);
      --  Retrieve element types from subprogram for unnamed addition

      procedure Wrap_For_Checks (Expr : N_Subexpr_Id; Typ : Type_Kind_Id);
      --  If Expr might require a range check for conversion to type Typ, set
      --  Do_Range_Check on Expr. In all cases, wrap Expr in a type conversion
      --  if Typ is not the type of Expr already, for GNATprove to correctly
      --  identity the target type for the range check and insert any other
      --  checks.

      ----------------------
      -- Parse_Named_Subp --
      ----------------------

      procedure Parse_Named_Subp
        (Subp         : Subprogram_Kind_Id;
         Key_Type     : out Type_Kind_Id;
         Element_Type : out Type_Kind_Id)
      is
         Formal : Entity_Id := First_Formal (Subp);
      begin
         Next_Formal (Formal);
         Key_Type := Etype (Formal);
         Next_Formal (Formal);
         Element_Type := Etype (Formal);
      end Parse_Named_Subp;

      ------------------------
      -- Parse_Unnamed_Subp --
      ------------------------

      procedure Parse_Unnamed_Subp
        (Subp         : Subprogram_Kind_Id;
         Element_Type : out Type_Kind_Id)
      is
         Formal : Entity_Id := First_Formal (Subp);
      begin
         Next_Formal (Formal);
         Element_Type := Etype (Formal);
      end Parse_Unnamed_Subp;

      ---------------------
      -- Wrap_For_Checks --
      ---------------------

      procedure Wrap_For_Checks (Expr : N_Subexpr_Id; Typ : Type_Kind_Id) is
      begin
         if Is_Scalar_Type (Typ) then
            Apply_Scalar_Range_Check (Expr, Typ);
         end if;

         Convert_To_And_Rewrite (Typ, Expr);
      end Wrap_For_Checks;

      --  Local variables

      Typ : constant Entity_Id := Etype (N);
      Asp : constant Node_Id := Find_Value_Of_Aspect (Typ, Aspect_Aggregate);

      Empty_Subp          : Node_Id := Empty;
      Add_Named_Subp      : Node_Id := Empty;
      Add_Unnamed_Subp    : Node_Id := Empty;
      New_Indexed_Subp    : Node_Id := Empty;
      Assign_Indexed_Subp : Node_Id := Empty;
      Key_Type            : Entity_Id;
      Element_Type        : Entity_Id;

      Assocs : constant List_Id := Component_Associations (N);
      Exprs  : constant List_Id := Expressions (N);
      Choice : Node_Id;
      Assoc  : Node_Id;
      Expr   : Node_Id;

   --  Start of processing for Expand_SPARK_N_Aggregate

   begin
      if Is_Container_Aggregate (N) then

         Parse_Aspect_Aggregate (Asp,
           Empty_Subp, Add_Named_Subp, Add_Unnamed_Subp,
           New_Indexed_Subp, Assign_Indexed_Subp);

         Assoc := First (Assocs);
         Expr := First (Exprs);

         --  Both lists could be empty as in [] but they can't be both
         --  non-empty.
         pragma Assert (not (Present (Assoc) and then Present (Expr)));

         --  Deal with cases supported in GNATprove:
         --  - named container aggregate which is not an indexed aggregate
         --  - positional container aggregate

         if Present (Assoc)
           and then Present (Add_Named_Subp)
         then
            Parse_Named_Subp (Entity (Add_Named_Subp), Key_Type, Element_Type);

            while Present (Assoc) loop
               Choice := First (Choice_List (Assoc));

               while Present (Choice) loop
                  Wrap_For_Checks (Choice, Key_Type);
                  Next (Choice);
               end loop;

               Wrap_For_Checks (Expression (Assoc), Element_Type);
               Next (Assoc);
            end loop;

         elsif Present (Expr) then
            Parse_Unnamed_Subp (Entity (Add_Unnamed_Subp), Element_Type);

            while Present (Expr) loop
               Wrap_For_Checks (Expr, Element_Type);
               Next (Expr);
            end loop;
         end if;
      end if;
   end Expand_SPARK_N_Aggregate;

   ----------------------------------
   -- Expand_SPARK_N_Freeze_Entity --
   ----------------------------------

   procedure Expand_SPARK_N_Freeze_Entity (N : Entity_Id) is
      E : constant Entity_Id := Entity (N);

      Action         : Node_Id;
      E_Scope        : Entity_Id;
      In_Other_Scope : Boolean;
      In_Outer_Scope : Boolean;

   begin
      --  Here E is a type or a subprogram

      E_Scope := Scope (E);

      --  This is an error protection against previous errors

      if No (E_Scope) then
         Check_Error_Detected;
         return;
      end if;

      --  The entity may be a subtype declared for a constrained record
      --  component, in which case the relevant scope is the scope of
      --  the record. This happens for class-wide subtypes created for
      --  a constrained type extension with inherited discriminants.

      if Is_Type (E_Scope)
        and then not Is_Concurrent_Type (E_Scope)
      then
         E_Scope := Scope (E_Scope);

      --  The entity may be a subtype declared for an iterator

      elsif Ekind (E_Scope) = E_Loop then
         E_Scope := Scope (E_Scope);
      end if;

      --  If we are freezing entities defined in protected types, they belong
      --  in the enclosing scope, given that the original type has been
      --  expanded away. The same is true for entities in task types, in
      --  particular the parameter records of entries (Entities in bodies are
      --  all frozen within the body). If we are in the task body, this is a
      --  proper scope. If we are within a subprogram body, the proper scope
      --  is the corresponding spec. This may happen for itypes generated in
      --  the bodies of protected operations.

      if Ekind (E_Scope) = E_Protected_Type
        or else (Ekind (E_Scope) = E_Task_Type
                  and then not Has_Completion (E_Scope))
      then
         E_Scope := Scope (E_Scope);

      elsif Ekind (E_Scope) = E_Subprogram_Body then
         E_Scope := Corresponding_Spec (Unit_Declaration_Node (E_Scope));
      end if;

      --  If the scope of the entity is in open scopes, it is the current one
      --  or an enclosing one, including a loop, a block, or a subprogram.

      if In_Open_Scopes (E_Scope) then
         In_Other_Scope := False;
         In_Outer_Scope := E_Scope /= Current_Scope;

      --  Otherwise it is a local package or a different compilation unit

      else
         In_Other_Scope := True;
         In_Outer_Scope := False;
      end if;

      --  If the entity being frozen is defined in a scope that is not
      --  currently on the scope stack, we must establish the proper
      --  visibility before freezing the entity and related subprograms.

      if In_Other_Scope then
         Push_Scope (E_Scope);

         --  Finalizers are little odd in terms of freezing. The spec of the
         --  procedure appears in the declarations while the body appears in
         --  the statement part of a single construct. Since the finalizer must
         --  be called by the At_End handler of the construct, the spec is
         --  manually frozen right after its declaration. The only side effect
         --  of this action appears in contexts where the construct is not in
         --  its final resting place. These contexts are:

         --    * Entry bodies - The declarations and statements are moved to
         --      the procedure equivalen of the entry.
         --    * Protected subprograms - The declarations and statements are
         --      moved to the non-protected version of the subprogram.
         --    * Task bodies - The declarations and statements are moved to the
         --      task body procedure.
         --    * Blocks that will be rewritten as subprograms when unnesting
         --      is in effect.

         --  Visible declarations do not need to be installed in these three
         --  cases since it does not make semantic sense to do so. All entities
         --  referenced by a finalizer are visible and already resolved, plus
         --  the enclosing scope may not have visible declarations at all.

         if Ekind (E) = E_Procedure
           and then Is_Finalizer (E)
           and then
             (Is_Entry (E_Scope)
                or else (Is_Subprogram (E_Scope)
                          and then Is_Protected_Type (Scope (E_Scope)))
                or else Is_Task_Type (E_Scope)
                or else Ekind (E_Scope) = E_Block)
         then
            null;
         else
            Install_Visible_Declarations (E_Scope);
         end if;

         if Is_Concurrent_Type (E_Scope)
           or else Is_Package_Or_Generic_Package (E_Scope)
         then
            Install_Private_Declarations (E_Scope);
         end if;

      --  If the entity is in an outer scope, then that scope needs to
      --  temporarily become the current scope so that operations created
      --  during type freezing will be declared in the right scope and
      --  can properly override any corresponding inherited operations.

      elsif In_Outer_Scope then
         Push_Scope (E_Scope);
      end if;

      --  Remember that we are processing a freezing entity and its freezing
      --  nodes. This flag (non-zero = set) is used to avoid the need of
      --  climbing through the tree while processing the freezing actions (ie.
      --  to avoid generating spurious warnings or to avoid killing constant
      --  indications while processing the code associated with freezing
      --  actions). We use a counter to deal with nesting.

      Inside_Freezing_Actions := Inside_Freezing_Actions + 1;

      --  Currently only types require freezing in SPARK

      SPARK_Freeze_Type (N);

      --  Analyze actions in freeze node, if any

      Action := First (Actions (N));
      while Present (Action) loop
         Analyze (Action);
         Next (Action);
      end loop;

      --  Pop scope if we installed one for the analysis

      if In_Other_Scope then
         if Ekind (Current_Scope) = E_Package then
            End_Package_Scope (E_Scope);
         else
            End_Scope;
         end if;

      elsif In_Outer_Scope then
         Pop_Scope;
      end if;

      --  Restore previous value of the nesting-level counter that records
      --  whether we are inside a (possibly nested) call to this procedure.

      Inside_Freezing_Actions := Inside_Freezing_Actions - 1;
   end Expand_SPARK_N_Freeze_Entity;

   ----------------------------------------
   -- Expand_SPARK_N_Attribute_Reference --
   ----------------------------------------

   procedure Expand_SPARK_N_Attribute_Reference (N : Node_Id) is
      Aname   : constant Name_Id      := Attribute_Name (N);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
      Loc     : constant Source_Ptr   := Sloc (N);
      Pref    : constant Node_Id      := Prefix (N);
      Typ     : constant Entity_Id    := Etype (N);
      Expr    : Node_Id;

   begin
      case Attr_Id is
         when Attribute_To_Address =>

            --  Extract and convert argument to expected type for call

            Expr :=
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (RTE (RE_Integer_Address), Loc),
                Expression   => Relocate_Node (First (Expressions (N))));

            --  Replace attribute reference with call

            Rewrite
              (N,
               Make_Function_Call (Loc,
                 Name                   =>
                   New_Occurrence_Of (RTE (RE_To_Address), Loc),
                 Parameter_Associations => New_List (Expr)));
            Analyze_And_Resolve (N, Typ);

         when Attribute_Object_Size
            | Attribute_Size
            | Attribute_Value_Size
            | Attribute_VADS_Size
         =>
            Exp_Attr.Expand_Size_Attribute (N);

         --  For attributes which return Universal_Integer, introduce a
         --  conversion to the expected type with the appropriate check flags
         --  set.

         when Attribute_Aft
            | Attribute_Alignment
            | Attribute_Bit
            | Attribute_Bit_Position
            | Attribute_Descriptor_Size
            | Attribute_First_Bit
            | Attribute_Last_Bit
            | Attribute_Length
            | Attribute_Max_Alignment_For_Allocation
            | Attribute_Max_Size_In_Storage_Elements
            | Attribute_Pos
            | Attribute_Position
            | Attribute_Range_Length
         =>
            --  If the expected type is Long_Long_Integer, there will be no
            --  check flag as the compiler assumes attributes always fit in
            --  this type. Since in SPARK_Mode we do not take Storage_Error
            --  into account, we cannot make this assumption and need to
            --  produce a check. ??? It should be enough to add this check for
            --  attributes 'Length, 'Range_Length and 'Pos when the type is as
            --  big as Long_Long_Integer.

            declare
               Typ : Entity_Id;
            begin
               if Attr_Id in Attribute_Pos | Attribute_Range_Length then
                  Typ := Etype (Prefix (N));

               elsif Attr_Id = Attribute_Length then
                  Typ := Get_Index_Subtype (N);

               else
                  Typ := Empty;
               end if;

               Apply_Universal_Integer_Attribute_Checks (N);

               if Present (Typ)
                 and then Known_RM_Size (Typ)
                 and then RM_Size (Typ) = RM_Size (Standard_Long_Long_Integer)
               then
                  --  ??? This should rather be a range check, but this would
                  --  crash GNATprove which somehow recovers the proper kind
                  --  of check anyway.
                  Set_Do_Overflow_Check (N);
               end if;
            end;

         when Attribute_Constrained =>

            --  If the prefix is an access to object, the attribute applies to
            --  the designated object, so rewrite with an explicit dereference.

            if Is_Access_Type (Etype (Pref))
              and then
              (not Is_Entity_Name (Pref) or else Is_Object (Entity (Pref)))
            then
               Rewrite (Pref,
                        Make_Explicit_Dereference (Loc, Relocate_Node (Pref)));
               Analyze_And_Resolve (N, Standard_Boolean);
            end if;

         when Attribute_Update =>
            Expand_SPARK_Delta_Or_Update (Typ, First (Expressions (N)));

         when others =>
            null;
      end case;
   end Expand_SPARK_N_Attribute_Reference;

   ------------------------------------
   -- Expand_SPARK_N_Delta_Aggregate --
   ------------------------------------

   procedure Expand_SPARK_N_Delta_Aggregate (N : Node_Id) is
   begin
      Expand_SPARK_Delta_Or_Update (Etype (N), N);
   end Expand_SPARK_N_Delta_Aggregate;

   -----------------------------------
   -- Expand_SPARK_N_Loop_Statement --
   -----------------------------------

   procedure Expand_SPARK_N_Loop_Statement (N : Node_Id) is
      Scheme : constant Node_Id := Iteration_Scheme (N);

   begin
      --  Loop iterations over arrays need to be expanded, to avoid getting
      --  two names referring to the same object in memory (the array and the
      --  iterator) in GNATprove, especially since both can be written (thus
      --  possibly leading to interferences due to aliasing). No such problem
      --  arises with quantified expressions over arrays, which are dealt with
      --  specially in GNATprove.

      if Present (Scheme)
        and then Present (Iterator_Specification (Scheme))
        and then Is_Iterator_Over_Array (Iterator_Specification (Scheme))
      then
         Expand_Iterator_Loop_Over_Array (N);
      end if;
   end Expand_SPARK_N_Loop_Statement;

   ---------------------------------------
   -- Expand_SPARK_N_Object_Declaration --
   ---------------------------------------

   procedure Expand_SPARK_N_Object_Declaration (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Obj_Id : constant Entity_Id  := Defining_Identifier (N);
      Typ    : constant Entity_Id  := Etype (Obj_Id);

      Call : Node_Id;

   begin
      --  If the object declaration denotes a variable without initialization
      --  whose type is subject to pragma Default_Initial_Condition, create
      --  and analyze a dummy call to the DIC procedure of the type in order
      --  to detect potential elaboration issues.

      if Comes_From_Source (Obj_Id)
        and then Ekind (Obj_Id) = E_Variable
        and then Has_DIC (Typ)
        and then Present (DIC_Procedure (Typ))
        and then not Has_Init_Expression (N)
      then
         Call := Build_DIC_Call (Loc, New_Occurrence_Of (Obj_Id, Loc), Typ);

         --  Partially insert the call into the tree by setting its parent
         --  pointer.

         Set_Parent (Call, N);
         Analyze (Call);
      end if;
   end Expand_SPARK_N_Object_Declaration;

   ------------------------------------------------
   -- Expand_SPARK_N_Object_Renaming_Declaration --
   ------------------------------------------------

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id) is
      CFS    : constant Boolean    := Comes_From_Source (N);
      Loc    : constant Source_Ptr := Sloc (N);
      Obj_Id : constant Entity_Id  := Defining_Entity (N);
      Nam    : constant Node_Id    := Name (N);
      Typ    : constant Entity_Id  := Etype (Obj_Id);

   begin
      --  Transform a renaming of the form

      --    Obj_Id : <subtype mark> renames <function call>;

      --  into

      --    Obj_Id : constant <subtype mark> := <function call>;

      --  Invoking Evaluate_Name and ultimately Remove_Side_Effects introduces
      --  a temporary to capture the function result. Once potential renamings
      --  are rewritten for SPARK, the temporary may be leaked out into source
      --  constructs and lead to confusing error diagnostics. Using an object
      --  declaration prevents this unwanted side effect.

      if Nkind (Nam) = N_Function_Call then
         Rewrite (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Obj_Id,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Typ, Loc),
             Expression          => Nam));

         --  Inherit the original Comes_From_Source status of the renaming

         Set_Comes_From_Source (N, CFS);

         --  Sever the link to the renamed function result because the entity
         --  will no longer alias anything.

         Set_Renamed_Object (Obj_Id, Empty);

         --  Remove the entity of the renaming declaration from visibility as
         --  the analysis of the object declaration will reintroduce it again.

         Remove_Entity_And_Homonym (Obj_Id);
         Analyze (N);

      --  Otherwise unconditionally remove all side effects from the name

      else
         Evaluate_Name (Nam);
      end if;
   end Expand_SPARK_N_Object_Renaming_Declaration;

   --------------------------
   -- Expand_SPARK_N_Op_Ne --
   --------------------------

   procedure Expand_SPARK_N_Op_Ne (N : Node_Id) is
      Typ : constant Entity_Id := Etype (Left_Opnd (N));

   begin
      --  Case of elementary type with standard operator

      if Is_Elementary_Type (Typ)
        and then Sloc (Entity (N)) = Standard_Location
      then
         null;

      else
         Exp_Ch4.Expand_N_Op_Ne (N);
      end if;
   end Expand_SPARK_N_Op_Ne;

   -------------------------------------
   -- Expand_SPARK_Potential_Renaming --
   -------------------------------------

   procedure Expand_SPARK_Potential_Renaming (N : Node_Id) is
      function In_Insignificant_Pragma (Nod : Node_Id) return Boolean;
      --  Determine whether arbitrary node Nod appears within a significant
      --  pragma for SPARK.

      -----------------------------
      -- In_Insignificant_Pragma --
      -----------------------------

      function In_Insignificant_Pragma (Nod : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         --  Climb the parent chain looking for an enclosing pragma

         Par := Nod;
         while Present (Par) loop
            if Nkind (Par) = N_Pragma then
               return not Pragma_Significant_In_SPARK (Get_Pragma_Id (Par));

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Insignificant_Pragma;

      --  Local variables

      Loc    : constant Source_Ptr := Sloc (N);
      Obj_Id : constant Entity_Id  := Entity (N);
      Typ    : constant Entity_Id  := Etype (N);
      Ren    : Node_Id;

   --  Start of processing for Expand_SPARK_Potential_Renaming

   begin
      --  Replace a reference to a renaming with the actual renamed object.
      --  Protect against previous errors leaving no entity in N.

      if Present (Obj_Id)
        and then Is_Object (Obj_Id)
      then
         Ren := Renamed_Object (Obj_Id);

         if Present (Ren) then

            --  Do not process a reference when it appears within a pragma of
            --  no significance to SPARK. It is assumed that the replacement
            --  will violate the semantics of the pragma and cause a spurious
            --  error.

            if In_Insignificant_Pragma (N) then
               return;

            --  Instantiations and inlining of subprograms employ "prologues"
            --  which map actual to formal parameters by means of renamings.
            --  Replace a reference to a formal by the corresponding actual
            --  parameter.

            elsif Nkind (Ren) in N_Entity then
               Rewrite (N, New_Occurrence_Of (Ren, Loc));

            --  Otherwise the renamed object denotes a name

            else
               Rewrite (N, New_Copy_Tree (Ren, New_Sloc => Loc));
               Reset_Analyzed_Flags (N);
            end if;

            Analyze_And_Resolve (N, Typ);
         end if;
      end if;
   end Expand_SPARK_Potential_Renaming;

   -----------------------
   -- SPARK_Freeze_Type --
   -----------------------

   procedure SPARK_Freeze_Type (N : Entity_Id) is
      Typ : constant Entity_Id := Entity (N);

      Renamed_Eq : Entity_Id;
      --  Defining unit name for the predefined equality function in the case
      --  where the type has a primitive operation that is a renaming of
      --  predefined equality (but only if there is also an overriding
      --  user-defined equality function). Used to pass this entity from
      --  Make_Predefined_Primitive_Specs to Predefined_Primitive_Bodies.

      Decl        : Node_Id;
      Eq_Spec     : Node_Id := Empty;
      Predef_List : List_Id;

      Wrapper_Decl_List : List_Id;
      Wrapper_Body_List : List_Id := No_List;

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

   begin
      --  The type being frozen may be subject to pragma Ghost. Set the mode
      --  now to ensure that any nodes generated during freezing are properly
      --  marked as Ghost.

      Set_Ghost_Mode (Typ);

      --  Generate the [spec and] body of the invariant procedure tasked with
      --  the runtime verification of all invariants that pertain to the type.
      --  This includes invariants on the partial and full view, inherited
      --  class-wide invariants from parent types or interfaces, and invariants
      --  on array elements or record components. But skip internal types.

      if Is_Itype (Typ) then
         null;

      elsif Is_Interface (Typ) then

         --  Interfaces are treated as the partial view of a private type in
         --  order to achieve uniformity with the general case. As a result, an
         --  interface receives only a "partial" invariant procedure which is
         --  never called.

         if Has_Own_Invariants (Typ) then
            Build_Invariant_Procedure_Body
              (Typ               => Typ,
               Partial_Invariant => Is_Interface (Typ));
         end if;

      --  Non-interface types

      --  Do not generate invariant procedure within other assertion
      --  subprograms, which may involve local declarations of local
      --  subtypes to which these checks do not apply.

      else
         if Has_Invariants (Typ) then
            if not Predicate_Check_In_Scope (Typ)
              or else (Ekind (Current_Scope) = E_Function
                        and then Is_Predicate_Function (Current_Scope))
            then
               null;
            else
               Build_Invariant_Procedure_Body (Typ);
            end if;
         end if;

         --  Generate the [spec and] body of the procedure tasked with the
         --  run-time verification of pragma Default_Initial_Condition's
         --  expression.

         if Has_DIC (Typ) then
            Build_DIC_Procedure_Body (Typ);
         end if;
      end if;

      if Ekind (Typ) = E_Record_Type
        and then Is_Tagged_Type (Typ)
        and then not Is_Interface (Typ)
        and then not Is_Limited_Type (Typ)
      then
         if Is_CPP_Class (Root_Type (Typ))
           and then Convention (Typ) = Convention_CPP
         then
            null;

         --  Do not add the spec of the predefined primitives if we are
         --  compiling under restriction No_Dispatching_Calls.

         elsif not Restriction_Active (No_Dispatching_Calls) then
            Set_Is_Frozen (Typ, False);

            Predef_List := New_List;
            Exp_Ch3.Make_Predefined_Primitive_Eq_Spec
              (Typ, Predef_List, Renamed_Eq);
            Eq_Spec := First (Predef_List);
            Insert_List_Before_And_Analyze (N, Predef_List);

            Set_Is_Frozen (Typ);

            --  Remove link from the parent list to the spec and body of
            --  the dispatching equality, but keep the link in the opposite
            --  direction, to allow up-traversal of the AST.

            if Present (Eq_Spec) then
               Decl := Parent (Eq_Spec);
               Remove (Eq_Spec);
               Set_Parent (Eq_Spec, Decl);
            end if;
         end if;
      end if;

      if Ekind (Typ) = E_Record_Type
        and then Is_Tagged_Type (Typ)
        and then not Is_CPP_Class (Typ)
      then
         --  Ada 2005 (AI-391): For a nonabstract null extension, create
         --  wrapper functions for each nonoverridden inherited function
         --  with a controlling result of the type. The wrapper for such
         --  a function returns an extension aggregate that invokes the
         --  parent function.

         if Ada_Version >= Ada_2005
           and then not Is_Abstract_Type (Typ)
           and then Is_Null_Extension (Typ)
         then
            Exp_Ch3.Make_Controlling_Function_Wrappers
              (Typ, Wrapper_Decl_List, Wrapper_Body_List);
            Insert_List_Before_And_Analyze (N, Wrapper_Decl_List);
         end if;

         --  Ada 2005 (AI-391): If any wrappers were created for nonoverridden
         --  inherited functions, then add their bodies to the AST, so they
         --  will be processed like ordinary subprogram bodies (even though the
         --  compiler adds them into the freezing action).

         if not Is_Interface (Typ) then
            Insert_List_Before_And_Analyze (N, Wrapper_Body_List);
         end if;
      end if;

      Restore_Ghost_Region (Saved_GM, Saved_IGR);
   end SPARK_Freeze_Type;

end Exp_SPARK;
