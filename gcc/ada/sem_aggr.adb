------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A G G R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

package body Sem_Aggr is

   type Case_Bounds is record
     Choice_Lo   : Node_Id;
     Choice_Hi   : Node_Id;
     Choice_Node : Node_Id;
   end record;

   type Case_Table_Type is array (Nat range <>) of Case_Bounds;
   --  Table type used by Check_Case_Choices procedure

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type);
   --  Sort the Case Table using the Lower Bound of each Choice as the key.
   --  A simple insertion sort is used since the number of choices in a case
   --  statement of variant part will usually be small and probably in near
   --  sorted order.

   ------------------------------------------------------
   -- Subprograms used for RECORD AGGREGATE Processing --
   ------------------------------------------------------

   procedure Resolve_Record_Aggregate (N : Node_Id; Typ : Entity_Id);
   --  This procedure performs all the semantic checks required for record
   --  aggregates. Note that for aggregates analysis and resolution go
   --  hand in hand. Aggregate analysis has been delayed up to here and
   --  it is done while resolving the aggregate.
   --
   --    N is the N_Aggregate node.
   --    Typ is the record type for the aggregate resolution
   --
   --  While performing the semantic checks, this procedure
   --  builds a new Component_Association_List where each record field
   --  appears alone in a Component_Choice_List along with its corresponding
   --  expression. The record fields in the Component_Association_List
   --  appear in the same order in which they appear in the record type Typ.
   --
   --  Once this new Component_Association_List is built and all the
   --  semantic checks performed, the original aggregate subtree is replaced
   --  with the new named record aggregate just built. Note that the subtree
   --  substitution is performed with Rewrite so as to be
   --  able to retrieve the original aggregate.
   --
   --  The aggregate subtree manipulation performed by Resolve_Record_Aggregate
   --  yields the aggregate format expected by Gigi. Typically, this kind of
   --  tree manipulations are done in the expander. However, because the
   --  semantic checks that need to be performed on record aggregates really
   --  go hand in hand with the record aggreagate normalization, the aggregate
   --  subtree transformation is performed during resolution rather than
   --  expansion. Had we decided otherwise we would have had to duplicate
   --  most of the code in the expansion procedure Expand_Record_Aggregate.
   --  Note, however, that all the expansion concerning aggegates for tagged
   --  records is done in Expand_Record_Aggregate.
   --
   --  The algorithm of Resolve_Record_Aggregate proceeds as follows:
   --
   --  1. Make sure that the record type against which the record aggregate
   --     has to be resolved is not abstract. Furthermore if the type is
   --     a null aggregate make sure the input aggregate N is also null.
   --
   --  2. Verify that the structure of the aggregate is that of a record
   --     aggregate. Specifically, look for component associations and ensure
   --     that each choice list only has identifiers or the N_Others_Choice
   --     node. Also make sure that if present, the N_Others_Choice occurs
   --     last and by itself.
   --
   --  3. If Typ contains discriminants, the values for each discriminant
   --     is looked for. If the record type Typ has variants, we check
   --     that the expressions corresponding to each discriminant ruling
   --     the (possibly nested) variant parts of Typ, are static. This
   --     allows us to determine the variant parts to which the rest of
   --     the aggregate must conform. The names of discriminants with their
   --     values are saved in a new association list, New_Assoc_List which
   --     is later augmented with the names and values of the remaining
   --     components in the record type.
   --
   --     During this phase we also make sure that every discriminant is
   --     assigned exactly one value. Note that when several values
   --     for a given discriminant are found, semantic processing continues
   --     looking for further errors. In this case it's the first
   --     discriminant value found which we will be recorded.
   --
   --     IMPORTANT NOTE: For derived tagged types this procedure expects
   --     First_Discriminant and Next_Discriminant to give the correct list
   --     of discriminants, in the correct order.
   --
   --  4. After all the discriminant values have been gathered, we can
   --     set the Etype of the record aggregate. If Typ contains no
   --     discriminants this is straightforward: the Etype of N is just
   --     Typ, otherwise a new implicit constrained subtype of Typ is
   --     built to be the Etype of N.
   --
   --  5. Gather the remaining record components according to the discriminant
   --     values. This involves recursively traversing the record type
   --     structure to see what variants are selected by the given discriminant
   --     values. This processing is a little more convoluted if Typ is a
   --     derived tagged types since we need to retrieve the record structure
   --     of all the ancestors of Typ.
   --
   --  6. After gathering the record components we look for their values
   --     in the record aggregate and emit appropriate error messages
   --     should we not find such values or should they be duplicated.
   --
   --  7. We then make sure no illegal component names appear in the
   --     record aggegate and make sure that the type of the record
   --     components appearing in a same choice list is the same.
   --     Finally we ensure that the others choice, if present, is
   --     used to provide the value of at least a record component.
   --
   --  8. The original aggregate node is replaced with the new named
   --     aggregate built in steps 3 through 6, as explained earlier.
   --
   --  Given the complexity of record aggregate resolution, the primary
   --  goal of this routine is clarity and simplicity rather than execution
   --  and storage efficiency. If there are only positional components in the
   --  aggregate the running time is linear. If there are associations
   --  the running time is still linear as long as the order of the
   --  associations is not too far off the order of the components in the
   --  record type. If this is not the case the running time is at worst
   --  quadratic in the size of the association list.

   procedure Check_Misspelled_Component
     (Elements      : Elist_Id;
      Component     : Node_Id);
   --  Give possible misspelling diagnostic if Component is likely to be
   --  a misspelling of one of the components of the Assoc_List.
   --  This is called by Resolv_Aggr_Expr after producing
   --  an invalid component error message.

   procedure Check_Static_Discriminated_Subtype (T : Entity_Id; V : Node_Id);
   --  An optimization: determine whether a discriminated subtype has a
   --  static constraint, and contains array components whose length is also
   --  static, either because they are constrained by the discriminant, or
   --  because the original component bounds are static.

   -----------------------------------------------------
   -- Subprograms used for ARRAY AGGREGATE Processing --
   -----------------------------------------------------

   function Resolve_Array_Aggregate
     (N              : Node_Id;
      Index          : Node_Id;
      Index_Constr   : Node_Id;
      Component_Typ  : Entity_Id;
      Others_Allowed : Boolean)
      return           Boolean;
   --  This procedure performs the semantic checks for an array aggregate.
   --  True is returned if the aggregate resolution succeeds.
   --  The procedure works by recursively checking each nested aggregate.
   --  Specifically, after checking a sub-aggreate nested at the i-th level
   --  we recursively check all the subaggregates at the i+1-st level (if any).
   --  Note that for aggregates analysis and resolution go hand in hand.
   --  Aggregate analysis has been delayed up to here and it is done while
   --  resolving the aggregate.
   --
   --    N is the current N_Aggregate node to be checked.
   --
   --    Index is the index node corresponding to the array sub-aggregate that
   --    we are currently checking (RM 4.3.3 (8)). Its Etype is the
   --    corresponding index type (or subtype).
   --
   --    Index_Constr is the node giving the applicable index constraint if
   --    any (RM 4.3.3 (10)). It "is a constraint provided by certain
   --    contexts [...] that can be used to determine the bounds of the array
   --    value specified by the aggregate". If Others_Allowed below is False
   --    there is no applicable index constraint and this node is set to Index.
   --
   --    Component_Typ is the array component type.
   --
   --    Others_Allowed indicates whether an others choice is allowed
   --    in the context where the top-level aggregate appeared.
   --
   --  The algorithm of Resolve_Array_Aggregate proceeds as follows:
   --
   --  1. Make sure that the others choice, if present, is by itself and
   --     appears last in the sub-aggregate. Check that we do not have
   --     positional and named components in the array sub-aggregate (unless
   --     the named association is an others choice). Finally if an others
   --     choice is present, make sure it is allowed in the aggregate contex.
   --
   --  2. If the array sub-aggregate contains discrete_choices:
   --
   --     (A) Verify their validity. Specifically verify that:
   --
   --        (a) If a null range is present it must be the only possible
   --            choice in the array aggregate.
   --
   --        (b) Ditto for a non static range.
   --
   --        (c) Ditto for a non static expression.
   --
   --        In addition this step analyzes and resolves each discrete_choice,
   --        making sure that its type is the type of the corresponding Index.
   --        If we are not at the lowest array aggregate level (in the case of
   --        multi-dimensional aggregates) then invoke Resolve_Array_Aggregate
   --        recursively on each component expression. Otherwise, resolve the
   --        bottom level component expressions against the expected component
   --        type ONLY IF the component corresponds to a single discrete choice
   --        which is not an others choice (to see why read the DELAYED
   --        COMPONENT RESOLUTION below).
   --
   --     (B) Determine the bounds of the sub-aggregate and lowest and
   --         highest choice values.
   --
   --  3. For positional aggregates:
   --
   --     (A) Loop over the component expressions either recursively invoking
   --         Resolve_Array_Aggregate on each of these for multi-dimensional
   --         array aggregates or resolving the bottom level component
   --         expressions against the expected component type.
   --
   --     (B) Determine the bounds of the positional sub-aggregates.
   --
   --  4. Try to determine statically whether the evaluation of the array
   --     sub-aggregate raises Constraint_Error. If yes emit proper
   --     warnings. The precise checks are the following:
   --
   --     (A) Check that the index range defined by aggregate bounds is
   --         compatible with corresponding index subtype.
   --         We also check against the base type. In fact it could be that
   --         Low/High bounds of the base type are static whereas those of
   --         the index subtype are not. Thus if we can statically catch
   --         a problem with respect to the base type we are guaranteed
   --         that the same problem will arise with the index subtype
   --
   --     (B) If we are dealing with a named aggregate containing an others
   --         choice and at least one discrete choice then make sure the range
   --         specified by the discrete choices does not overflow the
   --         aggregate bounds. We also check against the index type and base
   --         type bounds for the same reasons given in (A).
   --
   --     (C) If we are dealing with a positional aggregate with an others
   --         choice make sure the number of positional elements specified
   --         does not overflow the aggregate bounds. We also check against
   --         the index type and base type bounds as mentioned in (A).
   --
   --     Finally construct an N_Range node giving the sub-aggregate bounds.
   --     Set the Aggregate_Bounds field of the sub-aggregate to be this
   --     N_Range. The routine Array_Aggr_Subtype below uses such N_Ranges
   --     to build the appropriate aggregate subtype. Aggregate_Bounds
   --     information is needed during expansion.
   --
   --  DELAYED COMPONENT RESOLUTION: The resolution of bottom level component
   --  expressions in an array aggregate may call Duplicate_Subexpr or some
   --  other routine that inserts code just outside the outermost aggregate.
   --  If the array aggregate contains discrete choices or an others choice,
   --  this may be wrong. Consider for instance the following example.
   --
   --    type Rec is record
   --       V : Integer := 0;
   --    end record;
   --
   --    type Acc_Rec is access Rec;
   --    Arr : array (1..3) of Acc_Rec := (1 .. 3 => new Rec);
   --
   --  Then the transformation of "new Rec" that occurs during resolution
   --  entails the following code modifications
   --
   --    P7b : constant Acc_Rec := new Rec;
   --    Rec_init_proc (P7b.all);
   --    Arr : array (1..3) of Acc_Rec := (1 .. 3 => P7b);
   --
   --  This code transformation is clearly wrong, since we need to call
   --  "new Rec" for each of the 3 array elements. To avoid this problem we
   --  delay resolution of the components of non positional array aggregates
   --  to the expansion phase. As an optimization, if the discrete choice
   --  specifies a single value we do not delay resolution.

   function Array_Aggr_Subtype (N : Node_Id; Typ : Node_Id) return Entity_Id;
   --  This routine returns the type or subtype of an array aggregate.
   --
   --    N is the array aggregate node whose type we return.
   --
   --    Typ is the context type in which N occurs.
   --
   --  This routine creates an implicit array subtype whose bouds are
   --  those defined by the aggregate. When this routine is invoked
   --  Resolve_Array_Aggregate has already processed aggregate N. Thus the
   --  Aggregate_Bounds of each sub-aggregate, is an N_Range node giving the
   --  sub-aggregate bounds. When building the aggegate itype, this function
   --  traverses the array aggregate N collecting such Aggregate_Bounds and
   --  constructs the proper array aggregate itype.
   --
   --  Note that in the case of multidimensional aggregates each inner
   --  sub-aggregate corresponding to a given array dimension, may provide a
   --  different bounds. If it is possible to determine statically that
   --  some sub-aggregates corresponding to the same index do not have the
   --  same bounds, then a warning is emitted. If such check is not possible
   --  statically (because some sub-aggregate bounds are dynamic expressions)
   --  then this job is left to the expander. In all cases the particular
   --  bounds that this function will chose for a given dimension is the first
   --  N_Range node for a sub-aggregate corresponding to that dimension.
   --
   --  Note that the Raises_Constraint_Error flag of an array aggregate
   --  whose evaluation is determined to raise CE by Resolve_Array_Aggregate,
   --  is set in Resolve_Array_Aggregate but the aggregate is not
   --  immediately replaced with a raise CE. In fact, Array_Aggr_Subtype must
   --  first construct the proper itype for the aggregate (Gigi needs
   --  this). After constructing the proper itype we will eventually  replace
   --  the top-level aggregate with a raise CE (done in Resolve_Aggregate).
   --  Of course in cases such as:
   --
   --     type Arr is array (integer range <>) of Integer;
   --     A : Arr := (positive range -1 .. 2 => 0);
   --
   --  The bounds of the aggregate itype are cooked up to look reasonable
   --  (in this particular case the bounds will be 1 .. 2).

   procedure Aggregate_Constraint_Checks
     (Exp       : Node_Id;
      Check_Typ : Entity_Id);
   --  Checks expression Exp against subtype Check_Typ. If Exp is an
   --  aggregate and Check_Typ a constrained record type with discriminants,
   --  we generate the appropriate discriminant checks. If Exp is an array
   --  aggregate then emit the appropriate length checks. If Exp is a scalar
   --  type, or a string literal, Exp is changed into Check_Typ'(Exp) to
   --  ensure that range checks are performed at run time.

   procedure Make_String_Into_Aggregate (N : Node_Id);
   --  A string literal can appear in  a context in  which a one dimensional
   --  array of characters is expected. This procedure simply rewrites the
   --  string as an aggregate, prior to resolution.

   ---------------------------------
   -- Aggregate_Constraint_Checks --
   ---------------------------------

   procedure Aggregate_Constraint_Checks
     (Exp       : Node_Id;
      Check_Typ : Entity_Id)
   is
      Exp_Typ : constant Entity_Id  := Etype (Exp);

   begin
      if Raises_Constraint_Error (Exp) then
         return;
      end if;

      --  This is really expansion activity, so make sure that expansion
      --  is on and is allowed.

      if not Expander_Active or else In_Default_Expression then
         return;
      end if;

      --  First check if we have to insert discriminant checks

      if Has_Discriminants (Exp_Typ) then
         Apply_Discriminant_Check (Exp, Check_Typ);

      --  Next emit length checks for array aggregates

      elsif Is_Array_Type (Exp_Typ) then
         Apply_Length_Check (Exp, Check_Typ);

      --  Finally emit scalar and string checks. If we are dealing with a
      --  scalar literal we need to check by hand because the Etype of
      --  literals is not necessarily correct.

      elsif Is_Scalar_Type (Exp_Typ)
        and then Compile_Time_Known_Value (Exp)
      then
         if Is_Out_Of_Range (Exp, Base_Type (Check_Typ)) then
            Apply_Compile_Time_Constraint_Error
              (Exp, "value not in range of}?",
               Ent => Base_Type (Check_Typ),
               Typ => Base_Type (Check_Typ));

         elsif Is_Out_Of_Range (Exp, Check_Typ) then
            Apply_Compile_Time_Constraint_Error
              (Exp, "value not in range of}?",
               Ent => Check_Typ,
               Typ => Check_Typ);

         elsif not Range_Checks_Suppressed (Check_Typ) then
            Apply_Scalar_Range_Check (Exp, Check_Typ);
         end if;

      elsif (Is_Scalar_Type (Exp_Typ)
             or else Nkind (Exp) = N_String_Literal)
        and then Exp_Typ /= Check_Typ
      then
         if Is_Entity_Name (Exp)
           and then Ekind (Entity (Exp)) = E_Constant
         then
            --  If expression is a constant, it is worthwhile checking whether
            --  it is a bound of the type.

            if (Is_Entity_Name (Type_Low_Bound (Check_Typ))
                 and then Entity (Exp) = Entity (Type_Low_Bound (Check_Typ)))
              or else (Is_Entity_Name (Type_High_Bound (Check_Typ))
                and then Entity (Exp) = Entity (Type_High_Bound (Check_Typ)))
            then
               return;

            else
               Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
               Analyze_And_Resolve (Exp, Check_Typ);
            end if;
         else
            Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
            Analyze_And_Resolve (Exp, Check_Typ);
         end if;

      end if;
   end Aggregate_Constraint_Checks;

   ------------------------
   -- Array_Aggr_Subtype --
   ------------------------

   function Array_Aggr_Subtype
     (N    : Node_Id;
      Typ  : Entity_Id)
      return Entity_Id
   is
      Aggr_Dimension : constant Pos := Number_Dimensions (Typ);
      --  Number of aggregate index dimensions.

      Aggr_Range : array (1 .. Aggr_Dimension) of Node_Id := (others => Empty);
      --  Constrained N_Range of each index dimension in our aggregate itype.

      Aggr_Low   : array (1 .. Aggr_Dimension) of Node_Id := (others => Empty);
      Aggr_High  : array (1 .. Aggr_Dimension) of Node_Id := (others => Empty);
      --  Low and High bounds for each index dimension in our aggregate itype.

      Is_Fully_Positional : Boolean := True;

      procedure Collect_Aggr_Bounds (N : Node_Id; Dim : Pos);
      --  N is an array (sub-)aggregate. Dim is the dimension corresponding to
      --  (sub-)aggregate N. This procedure collects the constrained N_Range
      --  nodes corresponding to each index dimension of our aggregate itype.
      --  These N_Range nodes are collected in Aggr_Range above.
      --  Likewise collect in Aggr_Low & Aggr_High above the low and high
      --  bounds of each index dimension. If, when collecting, two bounds
      --  corresponding to the same dimension are static and found to differ,
      --  then emit a warning, and mark N as raising Constraint_Error.

      -------------------------
      -- Collect_Aggr_Bounds --
      -------------------------

      procedure Collect_Aggr_Bounds (N : Node_Id; Dim : Pos) is
         This_Range : constant Node_Id := Aggregate_Bounds (N);
         --  The aggregate range node of this specific sub-aggregate.

         This_Low  : constant Node_Id := Low_Bound (Aggregate_Bounds (N));
         This_High : constant Node_Id := High_Bound (Aggregate_Bounds (N));
         --  The aggregate bounds of this specific sub-aggregate.

         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         --  Collect the first N_Range for a given dimension that you find.
         --  For a given dimension they must be all equal anyway.

         if No (Aggr_Range (Dim)) then
            Aggr_Low (Dim)   := This_Low;
            Aggr_High (Dim)  := This_High;
            Aggr_Range (Dim) := This_Range;

         else
            if Compile_Time_Known_Value (This_Low) then
               if not Compile_Time_Known_Value (Aggr_Low (Dim)) then
                  Aggr_Low (Dim)  := This_Low;

               elsif Expr_Value (This_Low) /= Expr_Value (Aggr_Low (Dim)) then
                  Set_Raises_Constraint_Error (N);
                  Error_Msg_N ("Sub-aggregate low bound mismatch?", N);
                  Error_Msg_N ("Constraint_Error will be raised at run-time?",
                               N);
               end if;
            end if;

            if Compile_Time_Known_Value (This_High) then
               if not Compile_Time_Known_Value (Aggr_High (Dim)) then
                  Aggr_High (Dim)  := This_High;

               elsif
                 Expr_Value (This_High) /= Expr_Value (Aggr_High (Dim))
               then
                  Set_Raises_Constraint_Error (N);
                  Error_Msg_N ("Sub-aggregate high bound mismatch?", N);
                  Error_Msg_N ("Constraint_Error will be raised at run-time?",
                               N);
               end if;
            end if;
         end if;

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (N)) then
               Expr := First (Expressions (N));
               while Present (Expr) loop
                  Collect_Aggr_Bounds (Expr, Dim + 1);
                  Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (N)) then
               Is_Fully_Positional := False;

               Assoc := First (Component_Associations (N));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Collect_Aggr_Bounds (Expr, Dim + 1);
                  Next (Assoc);
               end loop;
            end if;
         end if;
      end Collect_Aggr_Bounds;

      --  Array_Aggr_Subtype variables

      Itype : Entity_Id;
      --  the final itype of the overall aggregate

      Index_Constraints : List_Id := New_List;
      --  The list of index constraints of the aggregate itype.

   --  Start of processing for Array_Aggr_Subtype

   begin
      --  Make sure that the list of index constraints is properly attached
      --  to the tree, and then collect the aggregate bounds.

      Set_Parent (Index_Constraints, N);
      Collect_Aggr_Bounds (N, 1);

      --  Build the list of constrained indices of our aggregate itype.

      for J in 1 .. Aggr_Dimension loop
         Create_Index : declare
            Index_Base : Entity_Id := Base_Type (Etype (Aggr_Range (J)));
            Index_Typ  : Entity_Id;

         begin
            --  Construct the Index subtype

            Index_Typ := Create_Itype (Subtype_Kind (Ekind (Index_Base)), N);

            Set_Etype (Index_Typ, Index_Base);

            if Is_Character_Type (Index_Base) then
               Set_Is_Character_Type (Index_Typ);
            end if;

            Set_Size_Info      (Index_Typ,                (Index_Base));
            Set_RM_Size        (Index_Typ, RM_Size        (Index_Base));
            Set_First_Rep_Item (Index_Typ, First_Rep_Item (Index_Base));
            Set_Scalar_Range   (Index_Typ, Aggr_Range (J));

            if Is_Discrete_Or_Fixed_Point_Type (Index_Typ) then
               Set_RM_Size (Index_Typ, UI_From_Int (Minimum_Size (Index_Typ)));
            end if;

            Set_Etype (Aggr_Range (J), Index_Typ);

            Append (Aggr_Range (J), To => Index_Constraints);
         end Create_Index;
      end loop;

      --  Now build the Itype

      Itype := Create_Itype (E_Array_Subtype, N);

      Set_First_Rep_Item         (Itype, First_Rep_Item         (Typ));
      Set_Component_Type         (Itype, Component_Type         (Typ));
      Set_Convention             (Itype, Convention             (Typ));
      Set_Depends_On_Private     (Itype, Has_Private_Component  (Typ));
      Set_Etype                  (Itype, Base_Type              (Typ));
      Set_Has_Alignment_Clause   (Itype, Has_Alignment_Clause   (Typ));
      Set_Is_Aliased             (Itype, Is_Aliased             (Typ));
      Set_Suppress_Index_Checks  (Itype, Suppress_Index_Checks  (Typ));
      Set_Suppress_Length_Checks (Itype, Suppress_Length_Checks (Typ));
      Set_Depends_On_Private     (Itype, Depends_On_Private     (Typ));

      Set_First_Index    (Itype, First (Index_Constraints));
      Set_Is_Constrained (Itype, True);
      Set_Is_Internal    (Itype, True);
      Init_Size_Align    (Itype);

      --  A simple optimization: purely positional aggregates of static
      --  components should be passed to gigi unexpanded whenever possible,
      --  and regardless of the staticness of the bounds themselves. Subse-
      --  quent checks in exp_aggr verify that type is not packed, etc.

      Set_Size_Known_At_Compile_Time (Itype,
         Is_Fully_Positional
           and then Comes_From_Source (N)
           and then Size_Known_At_Compile_Time (Component_Type (Typ)));

      --  We always need a freeze node for a packed array subtype, so that
      --  we can build the Packed_Array_Type corresponding to the subtype.
      --  If expansion is disabled, the packed array subtype is not built,
      --  and we must not generate a freeze node for the type, or else it
      --  will appear incomplete to gigi.

      if Is_Packed (Itype) and then not In_Default_Expression
        and then Expander_Active
      then
         Freeze_Itype (Itype, N);
      end if;

      return Itype;
   end Array_Aggr_Subtype;

   --------------------------------
   -- Check_Misspelled_Component --
   --------------------------------

   procedure Check_Misspelled_Component
     (Elements      : Elist_Id;
      Component     : Node_Id)
   is
      Max_Suggestions   : constant := 2;

      Nr_Of_Suggestions : Natural := 0;
      Suggestion_1      : Entity_Id := Empty;
      Suggestion_2      : Entity_Id := Empty;
      Component_Elmt    : Elmt_Id;

   begin
      --  All the components of List are matched against Component and
      --  a count is maintained of possible misspellings. When at the
      --  end of the analysis there are one or two (not more!) possible
      --  misspellings, these misspellings will be suggested as
      --  possible correction.

      Get_Name_String (Chars (Component));

      declare
         S  : constant String (1 .. Name_Len) :=
                Name_Buffer (1 .. Name_Len);

      begin

         Component_Elmt := First_Elmt (Elements);

         while Nr_Of_Suggestions <= Max_Suggestions
            and then Present (Component_Elmt)
         loop

            Get_Name_String (Chars (Node (Component_Elmt)));

            if Is_Bad_Spelling_Of (Name_Buffer (1 .. Name_Len), S) then
               Nr_Of_Suggestions := Nr_Of_Suggestions + 1;

               case Nr_Of_Suggestions is
                  when 1      => Suggestion_1 := Node (Component_Elmt);
                  when 2      => Suggestion_2 := Node (Component_Elmt);
                  when others => exit;
               end case;
            end if;

            Next_Elmt (Component_Elmt);
         end loop;

         --  Report at most two suggestions

         if Nr_Of_Suggestions = 1 then
            Error_Msg_NE ("\possible misspelling of&",
               Component, Suggestion_1);

         elsif Nr_Of_Suggestions = 2 then
            Error_Msg_Node_2 := Suggestion_2;
            Error_Msg_NE ("\possible misspelling of& or&",
              Component, Suggestion_1);
         end if;
      end;
   end Check_Misspelled_Component;

   ----------------------------------------
   -- Check_Static_Discriminated_Subtype --
   ----------------------------------------

   procedure Check_Static_Discriminated_Subtype (T : Entity_Id; V : Node_Id) is
      Disc : constant Entity_Id := First_Discriminant (T);
      Comp : Entity_Id;
      Ind  : Entity_Id;

   begin
      if Has_Record_Rep_Clause (Base_Type (T)) then
         return;

      elsif Present (Next_Discriminant (Disc)) then
         return;

      elsif Nkind (V) /= N_Integer_Literal then
         return;
      end if;

      Comp := First_Component (T);

      while Present (Comp) loop

         if Is_Scalar_Type (Etype (Comp)) then
            null;

         elsif Is_Private_Type (Etype (Comp))
           and then Present (Full_View (Etype (Comp)))
           and then Is_Scalar_Type (Full_View (Etype (Comp)))
         then
            null;

         elsif Is_Array_Type (Etype (Comp)) then

            if Is_Bit_Packed_Array (Etype (Comp)) then
               return;
            end if;

            Ind := First_Index (Etype (Comp));

            while Present (Ind) loop

               if Nkind (Ind) /= N_Range
                 or else Nkind (Low_Bound (Ind)) /= N_Integer_Literal
                 or else Nkind (High_Bound (Ind)) /= N_Integer_Literal
               then
                  return;
               end if;

               Next_Index (Ind);
            end loop;

         else
            return;
         end if;

         Next_Component (Comp);
      end loop;

      --  On exit, all components have statically known sizes.

      Set_Size_Known_At_Compile_Time (T);
   end Check_Static_Discriminated_Subtype;

   --------------------------------
   -- Make_String_Into_Aggregate --
   --------------------------------

   procedure Make_String_Into_Aggregate (N : Node_Id) is
      C      : Char_Code;
      C_Node : Node_Id;
      Exprs  : List_Id := New_List;
      Loc    : constant Source_Ptr := Sloc (N);
      New_N  : Node_Id;
      P      : Source_Ptr := Loc + 1;
      Str    : constant String_Id  := Strval (N);
      Strlen : constant Nat        := String_Length (Str);

   begin
      for J in  1 .. Strlen loop
         C := Get_String_Char (Str, J);
         Set_Character_Literal_Name (C);

         C_Node :=  Make_Character_Literal (P, Name_Find, C);
         Set_Etype (C_Node, Any_Character);
         Set_Analyzed (C_Node);
         Append_To (Exprs, C_Node);

         P := P + 1;
         --  something special for wide strings ?
      end loop;

      New_N := Make_Aggregate (Loc, Expressions => Exprs);
      Set_Analyzed (New_N);
      Set_Etype (New_N, Any_Composite);

      Rewrite (N, New_N);
   end Make_String_Into_Aggregate;

   -----------------------
   -- Resolve_Aggregate --
   -----------------------

   procedure Resolve_Aggregate (N : Node_Id; Typ : Entity_Id) is
      Pkind : constant Node_Kind := Nkind (Parent (N));

      Aggr_Subtyp : Entity_Id;
      --  The actual aggregate subtype. This is not necessarily the same as Typ
      --  which is the subtype of the context in which the aggregate was found.

   begin
      if Is_Limited_Type (Typ) then
         Error_Msg_N ("aggregate type cannot be limited", N);

      elsif Is_Limited_Composite (Typ) then
         Error_Msg_N ("aggregate type cannot have limited component", N);

      elsif Is_Class_Wide_Type (Typ) then
         Error_Msg_N ("type of aggregate cannot be class-wide", N);

      elsif Typ = Any_String
        or else Typ = Any_Composite
      then
         Error_Msg_N ("no unique type for aggregate", N);
         Set_Etype (N, Any_Composite);

      elsif Is_Array_Type (Typ) and then Null_Record_Present (N) then
         Error_Msg_N ("null record forbidden in array aggregate", N);

      elsif Is_Record_Type (Typ) then
         Resolve_Record_Aggregate (N, Typ);

      elsif Is_Array_Type (Typ) then

         --  First a special test, for the case of a positional aggregate
         --  of characters which can be replaced by a string literal.
         --  Do not perform this transformation if this was a string literal
         --  to start with, whose components needed constraint checks, or if
         --  the component type is non-static, because it will require those
         --  checks and be transformed back into an aggregate.

         if Number_Dimensions (Typ) = 1
           and then
             (Root_Type (Component_Type (Typ)) = Standard_Character
               or else
              Root_Type (Component_Type (Typ)) = Standard_Wide_Character)
           and then No (Component_Associations (N))
           and then not Is_Limited_Composite (Typ)
           and then not Is_Private_Composite (Typ)
           and then not Is_Bit_Packed_Array (Typ)
           and then Nkind (Original_Node (Parent (N))) /= N_String_Literal
           and then Is_Static_Subtype (Component_Type (Typ))
         then
            declare
               Expr : Node_Id;

            begin
               Expr := First (Expressions (N));
               while Present (Expr) loop
                  exit when Nkind (Expr) /= N_Character_Literal;
                  Next (Expr);
               end loop;

               if No (Expr) then
                  Start_String;

                  Expr := First (Expressions (N));
                  while Present (Expr) loop
                     Store_String_Char (Char_Literal_Value (Expr));
                     Next (Expr);
                  end loop;

                  Rewrite (N,
                    Make_String_Literal (Sloc (N), End_String));

                  Analyze_And_Resolve (N, Typ);
                  return;
               end if;
            end;
         end if;

         --  Here if we have a real aggregate to deal with

         Array_Aggregate : declare
            Aggr_Resolved : Boolean;
            Aggr_Typ      : Entity_Id := Etype (Typ);
            --  This is the unconstrained array type, which is the type
            --  against which the aggregate is to be resoved. Typ itself
            --  is the array type of the context which may not be the same
            --  subtype as the subtype for the final aggregate.

         begin
            --  In the following we determine whether an others choice is
            --  allowed inside the array aggregate. The test checks the context
            --  in which the array aggregate occurs. If the context does not
            --  permit it, or the aggregate type is unconstrained, an others
            --  choice is not allowed.
            --
            --  Note that there is no node for Explicit_Actual_Parameter.
            --  To test for this context we therefore have to test for node
            --  N_Parameter_Association which itself appears only if there is a
            --  formal parameter. Consequently we also need to test for
            --  N_Procedure_Call_Statement or N_Function_Call.

            if Is_Constrained (Typ) and then
              (Pkind = N_Assignment_Statement      or else
               Pkind = N_Parameter_Association     or else
               Pkind = N_Function_Call             or else
               Pkind = N_Procedure_Call_Statement  or else
               Pkind = N_Generic_Association       or else
               Pkind = N_Formal_Object_Declaration or else
               Pkind = N_Return_Statement          or else
               Pkind = N_Object_Declaration        or else
               Pkind = N_Component_Declaration     or else
               Pkind = N_Parameter_Specification   or else
               Pkind = N_Qualified_Expression      or else
               Pkind = N_Aggregate                 or else
               Pkind = N_Extension_Aggregate       or else
               Pkind = N_Component_Association)
            then
               Aggr_Resolved :=
                 Resolve_Array_Aggregate
                   (N,
                    Index          => First_Index (Aggr_Typ),
                    Index_Constr   => First_Index (Typ),
                    Component_Typ  => Component_Type (Typ),
                    Others_Allowed => True);

            else
               Aggr_Resolved :=
                 Resolve_Array_Aggregate
                   (N,
                    Index          => First_Index (Aggr_Typ),
                    Index_Constr   => First_Index (Aggr_Typ),
                    Component_Typ  => Component_Type (Typ),
                    Others_Allowed => False);
            end if;

            if not Aggr_Resolved then
               Aggr_Subtyp := Any_Composite;
            else
               Aggr_Subtyp := Array_Aggr_Subtype (N, Typ);
            end if;

            Set_Etype (N, Aggr_Subtyp);
         end Array_Aggregate;

      else
         Error_Msg_N ("illegal context for aggregate", N);

      end if;

      --  If we can determine statically that the evaluation of the
      --  aggregate raises Constraint_Error, then replace the
      --  aggregate with an N_Raise_Constraint_Error node, but set the
      --  Etype to the right aggregate subtype. Gigi needs this.

      if Raises_Constraint_Error (N) then
         Aggr_Subtyp := Etype (N);
         Rewrite (N, Make_Raise_Constraint_Error (Sloc (N)));
         Set_Raises_Constraint_Error (N);
         Set_Etype (N, Aggr_Subtyp);
         Set_Analyzed (N);
      end if;

   end Resolve_Aggregate;

   -----------------------------
   -- Resolve_Array_Aggregate --
   -----------------------------

   function Resolve_Array_Aggregate
     (N              : Node_Id;
      Index          : Node_Id;
      Index_Constr   : Node_Id;
      Component_Typ  : Entity_Id;
      Others_Allowed : Boolean)
      return           Boolean
   is
      Loc : constant Source_Ptr := Sloc (N);

      Failure : constant Boolean := False;
      Success : constant Boolean := True;

      Index_Typ      : constant Entity_Id := Etype (Index);
      Index_Typ_Low  : constant Node_Id   := Type_Low_Bound  (Index_Typ);
      Index_Typ_High : constant Node_Id   := Type_High_Bound (Index_Typ);
      --  The type of the index corresponding to the array sub-aggregate
      --  along with its low and upper bounds

      Index_Base      : constant Entity_Id := Base_Type (Index_Typ);
      Index_Base_Low  : constant Node_Id   := Type_Low_Bound (Index_Base);
      Index_Base_High : constant Node_Id   := Type_High_Bound (Index_Base);
      --  ditto for the base type

      function Add (Val : Uint; To : Node_Id) return Node_Id;
      --  Creates a new expression node where Val is added to expression To.
      --  Tries to constant fold whenever possible. To must be an already
      --  analyzed expression.

      procedure Check_Bound (BH : Node_Id; AH : in out Node_Id);
      --  Checks that AH (the upper bound of an array aggregate) is <= BH
      --  (the upper bound of the index base type). If the check fails a
      --  warning is emitted, the Raises_Constraint_Error Flag of N is set,
      --  and AH is replaced with a duplicate of BH.

      procedure Check_Bounds (L, H : Node_Id; AL, AH : Node_Id);
      --  Checks that range AL .. AH is compatible with range L .. H. Emits a
      --  warning if not and sets the Raises_Constraint_Error Flag in N.

      procedure Check_Length (L, H : Node_Id; Len : Uint);
      --  Checks that range L .. H contains at least Len elements. Emits a
      --  warning if not and sets the Raises_Constraint_Error Flag in N.

      function Dynamic_Or_Null_Range (L, H : Node_Id) return Boolean;
      --  Returns True if range L .. H is dynamic or null.

      procedure Get (Value : out Uint; From : Node_Id; OK : out Boolean);
      --  Given expression node From, this routine sets OK to False if it
      --  cannot statically evaluate From. Otherwise it stores this static
      --  value into Value.

      function Resolve_Aggr_Expr
        (Expr        : Node_Id;
         Single_Elmt : Boolean)
         return        Boolean;
      --  Resolves aggregate expression Expr. Returs False if resolution
      --  fails. If Single_Elmt is set to False, the expression Expr may be
      --  used to initialize several array aggregate elements (this can
      --  happen for discrete choices such as "L .. H => Expr" or the others
      --  choice). In this event we do not resolve Expr unless expansion is
      --  disabled. To know why, see the DELAYED COMPONENT RESOLUTION
      --  note above.

      ---------
      -- Add --
      ---------

      function Add (Val : Uint; To : Node_Id) return Node_Id is
         Expr_Pos : Node_Id;
         Expr     : Node_Id;
         To_Pos   : Node_Id;

      begin
         if Raises_Constraint_Error (To) then
            return To;
         end if;

         --  First test if we can do constant folding

         if Compile_Time_Known_Value (To)
           or else Nkind (To) = N_Integer_Literal
         then
            Expr_Pos := Make_Integer_Literal (Loc, Expr_Value (To) + Val);
            Set_Is_Static_Expression (Expr_Pos);
            Set_Etype (Expr_Pos, Etype (To));
            Set_Analyzed (Expr_Pos, Analyzed (To));

            if not Is_Enumeration_Type (Index_Typ) then
               Expr := Expr_Pos;

            --  If we are dealing with enumeration return
            --     Index_Typ'Val (Expr_Pos)

            else
               Expr :=
                 Make_Attribute_Reference
                   (Loc,
                    Prefix         => New_Reference_To (Index_Typ, Loc),
                    Attribute_Name => Name_Val,
                    Expressions    => New_List (Expr_Pos));
            end if;

            return Expr;
         end if;

         --  If we are here no constant folding possible

         if not Is_Enumeration_Type (Index_Base) then
            Expr :=
              Make_Op_Add (Loc,
                           Left_Opnd  => Duplicate_Subexpr (To),
                           Right_Opnd => Make_Integer_Literal (Loc, Val));

         --  If we are dealing with enumeration return
         --    Index_Typ'Val (Index_Typ'Pos (To) + Val)

         else
            To_Pos :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => New_Reference_To (Index_Typ, Loc),
                 Attribute_Name => Name_Pos,
                 Expressions    => New_List (Duplicate_Subexpr (To)));

            Expr_Pos :=
              Make_Op_Add (Loc,
                           Left_Opnd  => To_Pos,
                           Right_Opnd => Make_Integer_Literal (Loc, Val));

            Expr :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => New_Reference_To (Index_Typ, Loc),
                 Attribute_Name => Name_Val,
                 Expressions    => New_List (Expr_Pos));
         end if;

         return Expr;
      end Add;

      -----------------
      -- Check_Bound --
      -----------------

      procedure Check_Bound (BH : Node_Id; AH : in out Node_Id) is
         Val_BH : Uint;
         Val_AH : Uint;

         OK_BH : Boolean;
         OK_AH : Boolean;

      begin
         Get (Value => Val_BH, From => BH, OK => OK_BH);
         Get (Value => Val_AH, From => AH, OK => OK_AH);

         if OK_BH and then OK_AH and then Val_BH < Val_AH then
            Set_Raises_Constraint_Error (N);
            Error_Msg_N ("upper bound out of range?", AH);
            Error_Msg_N ("Constraint_Error will be raised at run-time?", AH);

            --  You need to set AH to BH or else in the case of enumerations
            --  indices we will not be able to resolve the aggregate bounds.

            AH := Duplicate_Subexpr (BH);
         end if;
      end Check_Bound;

      ------------------
      -- Check_Bounds --
      ------------------

      procedure Check_Bounds (L, H : Node_Id; AL, AH : Node_Id) is
         Val_L  : Uint;
         Val_H  : Uint;
         Val_AL : Uint;
         Val_AH : Uint;

         OK_L  : Boolean;
         OK_H  : Boolean;
         OK_AL : Boolean;
         OK_AH : Boolean;

      begin
         if Raises_Constraint_Error (N)
           or else Dynamic_Or_Null_Range (AL, AH)
         then
            return;
         end if;

         Get (Value => Val_L, From => L, OK => OK_L);
         Get (Value => Val_H, From => H, OK => OK_H);

         Get (Value => Val_AL, From => AL, OK => OK_AL);
         Get (Value => Val_AH, From => AH, OK => OK_AH);

         if OK_L and then Val_L > Val_AL then
            Set_Raises_Constraint_Error (N);
            Error_Msg_N ("lower bound of aggregate out of range?", N);
            Error_Msg_N ("Constraint_Error will be raised at run-time?", N);
         end if;

         if OK_H and then Val_H < Val_AH then
            Set_Raises_Constraint_Error (N);
            Error_Msg_N ("upper bound of aggregate out of range?", N);
            Error_Msg_N ("Constraint_Error will be raised at run-time?", N);
         end if;
      end Check_Bounds;

      ------------------
      -- Check_Length --
      ------------------

      procedure Check_Length (L, H : Node_Id; Len : Uint) is
         Val_L  : Uint;
         Val_H  : Uint;

         OK_L  : Boolean;
         OK_H  : Boolean;

         Range_Len : Uint;

      begin
         if Raises_Constraint_Error (N) then
            return;
         end if;

         Get (Value => Val_L, From => L, OK => OK_L);
         Get (Value => Val_H, From => H, OK => OK_H);

         if not OK_L or else not OK_H then
            return;
         end if;

         --  If null range length is zero

         if Val_L > Val_H then
            Range_Len := Uint_0;
         else
            Range_Len := Val_H - Val_L + 1;
         end if;

         if Range_Len < Len then
            Set_Raises_Constraint_Error (N);
            Error_Msg_N ("Too many elements?", N);
            Error_Msg_N ("Constraint_Error will be raised at run-time?", N);
         end if;
      end Check_Length;

      ---------------------------
      -- Dynamic_Or_Null_Range --
      ---------------------------

      function Dynamic_Or_Null_Range (L, H : Node_Id) return Boolean is
         Val_L : Uint;
         Val_H : Uint;

         OK_L  : Boolean;
         OK_H  : Boolean;

      begin
         Get (Value => Val_L, From => L, OK => OK_L);
         Get (Value => Val_H, From => H, OK => OK_H);

         return not OK_L or else not OK_H
           or else not Is_OK_Static_Expression (L)
           or else not Is_OK_Static_Expression (H)
           or else Val_L > Val_H;
      end Dynamic_Or_Null_Range;

      ---------
      -- Get --
      ---------

      procedure Get (Value : out Uint; From : Node_Id; OK : out Boolean) is
      begin
         OK := True;

         if Compile_Time_Known_Value (From) then
            Value := Expr_Value (From);

         --  If expression From is something like Some_Type'Val (10) then
         --  Value = 10

         elsif Nkind (From) = N_Attribute_Reference
           and then Attribute_Name (From) = Name_Val
           and then Compile_Time_Known_Value (First (Expressions (From)))
         then
            Value := Expr_Value (First (Expressions (From)));

         else
            Value := Uint_0;
            OK := False;
         end if;
      end Get;

      -----------------------
      -- Resolve_Aggr_Expr --
      -----------------------

      function Resolve_Aggr_Expr
        (Expr        : Node_Id;
         Single_Elmt : Boolean)
         return        Boolean
      is
         Nxt_Ind        : Node_Id := Next_Index (Index);
         Nxt_Ind_Constr : Node_Id := Next_Index (Index_Constr);
         --  Index is the current index corresponding to the expression.

         Resolution_OK : Boolean := True;
         --  Set to False if resolution of the expression failed.

      begin
         --  If the array type against which we are resolving the aggregate
         --  has several dimensions, the expressions nested inside the
         --  aggregate must be further aggregates (or strings).

         if Present (Nxt_Ind) then
            if Nkind (Expr) /= N_Aggregate then

               --  A string literal can appear where a one-dimensional array
               --  of characters is expected. If the literal looks like an
               --  operator, it is still an operator symbol, which will be
               --  transformed into a string when analyzed.

               if Is_Character_Type (Component_Typ)
                 and then No (Next_Index (Nxt_Ind))
                 and then (Nkind (Expr) = N_String_Literal
                            or else Nkind (Expr) = N_Operator_Symbol)
               then
                  --  A string literal used in a multidimensional array
                  --  aggregate in place of the final one-dimensional
                  --  aggregate must not be enclosed in parentheses.

                  if Paren_Count (Expr) /= 0 then
                     Error_Msg_N ("No parenthesis allowed here", Expr);
                  end if;

                  Make_String_Into_Aggregate (Expr);

               else
                  Error_Msg_N ("nested array aggregate expected", Expr);
                  return Failure;
               end if;
            end if;

            Resolution_OK := Resolve_Array_Aggregate
              (Expr, Nxt_Ind, Nxt_Ind_Constr, Component_Typ, Others_Allowed);

         --  Do not resolve the expressions of discrete or others choices
         --  unless the expression covers a single component, or the expander
         --  is inactive.

         elsif Single_Elmt
           or else not Expander_Active
           or else In_Default_Expression
         then
            Analyze_And_Resolve (Expr, Component_Typ);
            Check_Non_Static_Context (Expr);
            Aggregate_Constraint_Checks (Expr, Component_Typ);
         end if;

         if Raises_Constraint_Error (Expr)
           and then Nkind (Parent (Expr)) /= N_Component_Association
         then
            Set_Raises_Constraint_Error (N);
         end if;

         return Resolution_OK;
      end Resolve_Aggr_Expr;

      --  Variables local to Resolve_Array_Aggregate

      Assoc   : Node_Id;
      Choice  : Node_Id;
      Expr    : Node_Id;

      Who_Cares : Node_Id;

      Aggr_Low  : Node_Id := Empty;
      Aggr_High : Node_Id := Empty;
      --  The actual low and high bounds of this sub-aggegate

      Choices_Low  : Node_Id := Empty;
      Choices_High : Node_Id := Empty;
      --  The lowest and highest discrete choices values for a named aggregate

      Nb_Elements : Uint := Uint_0;
      --  The number of elements in a positional aggegate

      Others_Present : Boolean := False;

      Nb_Choices : Nat := 0;
      --  Contains the overall number of named choices in this sub-aggregate

      Nb_Discrete_Choices : Nat := 0;
      --  The overall number of discrete choices (not counting others choice)

      Case_Table_Size : Nat;
      --  Contains the size of the case table needed to sort aggregate choices

   --  Start of processing for Resolve_Array_Aggregate

   begin
      --  STEP 1: make sure the aggregate is correctly formatted

      if Present (Component_Associations (N)) then
         Assoc := First (Component_Associations (N));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));
            while Present (Choice) loop
               if Nkind (Choice) = N_Others_Choice then
                  Others_Present := True;

                  if Choice /= First (Choices (Assoc))
                    or else Present (Next (Choice))
                  then
                     Error_Msg_N
                       ("OTHERS must appear alone in a choice list", Choice);
                     return Failure;
                  end if;

                  if Present (Next (Assoc)) then
                     Error_Msg_N
                       ("OTHERS must appear last in an aggregate", Choice);
                     return Failure;
                  end if;

                  if Ada_83
                    and then Assoc /= First (Component_Associations (N))
                    and then (Nkind (Parent (N)) = N_Assignment_Statement
                               or else
                                 Nkind (Parent (N)) = N_Object_Declaration)
                  then
                     Error_Msg_N
                       ("(Ada 83) illegal context for OTHERS choice", N);
                  end if;
               end if;

               Nb_Choices := Nb_Choices + 1;
               Next (Choice);
            end loop;

            Next (Assoc);
         end loop;
      end if;

      --  At this point we know that the others choice, if present, is by
      --  itself and appears last in the aggregate. Check if we have mixed
      --  positional and discrete associations (other than the others choice).

      if Present (Expressions (N))
        and then (Nb_Choices > 1
                   or else (Nb_Choices = 1 and then not Others_Present))
      then
         Error_Msg_N
           ("named association cannot follow positional association",
            First (Choices (First (Component_Associations (N)))));
         return Failure;
      end if;

      --  Test for the validity of an others choice if present

      if Others_Present and then not Others_Allowed then
         Error_Msg_N
           ("OTHERS choice not allowed here",
            First (Choices (First (Component_Associations (N)))));
         return Failure;
      end if;

      --  STEP 2: Process named components

      if No (Expressions (N)) then

         if Others_Present then
            Case_Table_Size := Nb_Choices - 1;
         else
            Case_Table_Size := Nb_Choices;
         end if;

         Step_2 : declare
            Low  : Node_Id;
            High : Node_Id;
            --  Denote the lowest and highest values in an aggregate choice

            Hi_Val : Uint;
            Lo_Val : Uint;
            --  High end of one range and Low end of the next. Should be
            --  contiguous if there is no hole in the list of values.

            Missing_Values : Boolean;
            --  Set True if missing index values

            S_Low  : Node_Id := Empty;
            S_High : Node_Id := Empty;
            --  if a choice in an aggregate is a subtype indication these
            --  denote the lowest and highest values of the subtype

            Table : Case_Table_Type (1 .. Case_Table_Size);
            --  Used to sort all the different choice values

            Single_Choice : Boolean;
            --  Set to true every time there is a single discrete choice in a
            --  discrete association

            Prev_Nb_Discrete_Choices : Nat;
            --  Used to keep track of the number of discrete choices
            --  in the current association.

         begin
            --  STEP 2 (A): Check discrete choices validity.

            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop

               Prev_Nb_Discrete_Choices := Nb_Discrete_Choices;
               Choice := First (Choices (Assoc));
               loop
                  Analyze (Choice);

                  if Nkind (Choice) = N_Others_Choice then
                     Single_Choice := False;
                     exit;

                  --  Test for subtype mark without constraint

                  elsif Is_Entity_Name (Choice) and then
                    Is_Type (Entity (Choice))
                  then
                     if Base_Type (Entity (Choice)) /= Index_Base then
                        Error_Msg_N
                          ("invalid subtype mark in aggregate choice",
                           Choice);
                        return Failure;
                     end if;

                  elsif Nkind (Choice) = N_Subtype_Indication then
                     Resolve_Discrete_Subtype_Indication (Choice, Index_Base);

                     --  Does the subtype indication evaluation raise CE ?

                     Get_Index_Bounds (Subtype_Mark (Choice), S_Low, S_High);
                     Get_Index_Bounds (Choice, Low, High);
                     Check_Bounds (S_Low, S_High, Low, High);

                  else  --  Choice is a range or an expression
                     Resolve (Choice, Index_Base);
                     Check_Non_Static_Context (Choice);

                     --  Do not range check a choice. This check is redundant
                     --  since this test is already performed when we check
                     --  that the bounds of the array aggregate are within
                     --  range.

                     Set_Do_Range_Check (Choice, False);
                  end if;

                  --  If we could not resolve the discrete choice stop here

                  if Etype (Choice) = Any_Type then
                     return Failure;

                  --  If the discrete choice raises CE get its original bounds.

                  elsif Nkind (Choice) = N_Raise_Constraint_Error then
                     Set_Raises_Constraint_Error (N);
                     Get_Index_Bounds (Original_Node (Choice), Low, High);

                  --  Otherwise get its bounds as usual

                  else
                     Get_Index_Bounds (Choice, Low, High);
                  end if;

                  if (Dynamic_Or_Null_Range (Low, High)
                       or else (Nkind (Choice) = N_Subtype_Indication
                                 and then
                                   Dynamic_Or_Null_Range (S_Low, S_High)))
                    and then Nb_Choices /= 1
                  then
                     Error_Msg_N
                       ("dynamic or empty choice in aggregate " &
                        "must be the only choice", Choice);
                     return Failure;
                  end if;

                  Nb_Discrete_Choices := Nb_Discrete_Choices + 1;
                  Table (Nb_Discrete_Choices).Choice_Lo := Low;
                  Table (Nb_Discrete_Choices).Choice_Hi := High;

                  Next (Choice);

                  if No (Choice) then
                     --  Check if we have a single discrete choice and whether
                     --  this discrete choice specifies a single value.

                     Single_Choice :=
                       (Nb_Discrete_Choices = Prev_Nb_Discrete_Choices + 1)
                         and then (Low = High);

                     exit;
                  end if;
               end loop;

               if not
                 Resolve_Aggr_Expr
                   (Expression (Assoc), Single_Elmt => Single_Choice)
               then
                  return Failure;
               end if;

               Next (Assoc);
            end loop;

            --  If aggregate contains more than one choice then these must be
            --  static. Sort them and check that they are contiguous

            if Nb_Discrete_Choices > 1 then
               Sort_Case_Table (Table);
               Missing_Values := False;

               Outer : for J in 1 .. Nb_Discrete_Choices - 1 loop
                  if Expr_Value (Table (J).Choice_Hi) >=
                       Expr_Value (Table (J + 1).Choice_Lo)
                  then
                     Error_Msg_N
                       ("duplicate choice values in array aggregate",
                        Table (J).Choice_Hi);
                     return Failure;

                  elsif not Others_Present then

                     Hi_Val := Expr_Value (Table (J).Choice_Hi);
                     Lo_Val := Expr_Value (Table (J + 1).Choice_Lo);

                     --  If missing values, output error messages

                     if Lo_Val - Hi_Val > 1 then

                        --  Header message if not first missing value

                        if not Missing_Values then
                           Error_Msg_N
                             ("missing index value(s) in array aggregate", N);
                           Missing_Values := True;
                        end if;

                        --  Output values of missing indexes

                        Lo_Val := Lo_Val - 1;
                        Hi_Val := Hi_Val + 1;

                        --  Enumeration type case

                        if Is_Enumeration_Type (Index_Typ) then
                           Error_Msg_Name_1 :=
                             Chars
                               (Get_Enum_Lit_From_Pos
                                 (Index_Typ, Hi_Val, Loc));

                           if Lo_Val = Hi_Val then
                              Error_Msg_N ("\  %", N);
                           else
                              Error_Msg_Name_2 :=
                                Chars
                                  (Get_Enum_Lit_From_Pos
                                    (Index_Typ, Lo_Val, Loc));
                              Error_Msg_N ("\  % .. %", N);
                           end if;

                        --  Integer types case

                        else
                           Error_Msg_Uint_1 := Hi_Val;

                           if Lo_Val = Hi_Val then
                              Error_Msg_N ("\  ^", N);
                           else
                              Error_Msg_Uint_2 := Lo_Val;
                              Error_Msg_N ("\  ^ .. ^", N);
                           end if;
                        end if;
                     end if;
                  end if;
               end loop Outer;

               if Missing_Values then
                  Set_Etype (N, Any_Composite);
                  return Failure;
               end if;
            end if;

            --  STEP 2 (B): Compute aggregate bounds and min/max choices values

            if Nb_Discrete_Choices > 0 then
               Choices_Low  := Table (1).Choice_Lo;
               Choices_High := Table (Nb_Discrete_Choices).Choice_Hi;
            end if;

            if Others_Present then
               Get_Index_Bounds (Index_Constr, Aggr_Low, Aggr_High);

            else
               Aggr_Low  := Choices_Low;
               Aggr_High := Choices_High;
            end if;
         end Step_2;

      --  STEP 3: Process positional components

      else
         --  STEP 3 (A): Process positional elements

         Expr := First (Expressions (N));
         Nb_Elements := Uint_0;
         while Present (Expr) loop
            Nb_Elements := Nb_Elements + 1;

            if not Resolve_Aggr_Expr (Expr, Single_Elmt => True) then
               return Failure;
            end if;

            Next (Expr);
         end loop;

         if Others_Present then
            Assoc := Last (Component_Associations (N));
            if not Resolve_Aggr_Expr (Expression (Assoc),
                                      Single_Elmt => False)
            then
               return Failure;
            end if;
         end if;

         --  STEP 3 (B): Compute the aggregate bounds

         if Others_Present then
            Get_Index_Bounds (Index_Constr, Aggr_Low, Aggr_High);

         else
            if Others_Allowed then
               Get_Index_Bounds (Index_Constr, Aggr_Low, Who_Cares);
            else
               Aggr_Low := Index_Typ_Low;
            end if;

            Aggr_High := Add (Nb_Elements - 1, To => Aggr_Low);
            Check_Bound (Index_Base_High, Aggr_High);
         end if;
      end if;

      --  STEP 4: Perform static aggregate checks and save the bounds

      --  Check (A)

      Check_Bounds (Index_Typ_Low, Index_Typ_High, Aggr_Low, Aggr_High);
      Check_Bounds (Index_Base_Low, Index_Base_High, Aggr_Low, Aggr_High);

      --  Check (B)

      if Others_Present and then Nb_Discrete_Choices > 0 then
         Check_Bounds (Aggr_Low, Aggr_High, Choices_Low, Choices_High);
         Check_Bounds (Index_Typ_Low, Index_Typ_High,
                       Choices_Low, Choices_High);
         Check_Bounds (Index_Base_Low, Index_Base_High,
                       Choices_Low, Choices_High);

      --  Check (C)

      elsif Others_Present and then Nb_Elements > 0 then
         Check_Length (Aggr_Low, Aggr_High, Nb_Elements);
         Check_Length (Index_Typ_Low, Index_Typ_High, Nb_Elements);
         Check_Length (Index_Base_Low, Index_Base_High, Nb_Elements);

      end if;

      if Raises_Constraint_Error (Aggr_Low)
        or else Raises_Constraint_Error (Aggr_High)
      then
         Set_Raises_Constraint_Error (N);
      end if;

      Aggr_Low := Duplicate_Subexpr (Aggr_Low);

      --  Do not duplicate Aggr_High if Aggr_High = Aggr_Low + Nb_Elements
      --  since the addition node returned by Add is not yet analyzed. Attach
      --  to tree and analyze first. Reset analyzed flag to insure it will get
      --  analyzed when it is a literal bound whose type must be properly
      --  set.

      if Others_Present or else Nb_Discrete_Choices > 0 then
         Aggr_High := Duplicate_Subexpr (Aggr_High);

         if Etype (Aggr_High) = Universal_Integer then
            Set_Analyzed (Aggr_High, False);
         end if;
      end if;

      Set_Aggregate_Bounds
        (N, Make_Range (Loc, Low_Bound => Aggr_Low, High_Bound => Aggr_High));

      --  The bounds may contain expressions that must be inserted upwards.
      --  Attach them fully to the tree. After analysis, remove side effects
      --  from upper bound, if still needed.

      Set_Parent (Aggregate_Bounds (N), N);
      Analyze_And_Resolve (Aggregate_Bounds (N), Index_Typ);

      if not Others_Present and then Nb_Discrete_Choices = 0 then
         Set_High_Bound (Aggregate_Bounds (N),
             Duplicate_Subexpr (High_Bound (Aggregate_Bounds (N))));
      end if;

      return Success;
   end Resolve_Array_Aggregate;

   ---------------------------------
   -- Resolve_Extension_Aggregate --
   ---------------------------------

   --  There are two cases to consider:

   --  a) If the ancestor part is a type mark, the components needed are
   --  the difference between the components of the expected type and the
   --  components of the given type mark.

   --  b) If the ancestor part is an expression, it must be unambiguous,
   --  and once we have its type we can also compute the needed  components
   --  as in the previous case. In both cases, if the ancestor type is not
   --  the immediate ancestor, we have to build this ancestor recursively.

   --  In both cases discriminants of the ancestor type do not play a
   --  role in the resolution of the needed components, because inherited
   --  discriminants cannot be used in a type extension. As a result we can
   --  compute independently the list of components of the ancestor type and
   --  of the expected type.

   procedure Resolve_Extension_Aggregate (N : Node_Id; Typ : Entity_Id) is
      A        : constant Node_Id := Ancestor_Part (N);
      A_Type   : Entity_Id;
      I        : Interp_Index;
      It       : Interp;
      Imm_Type : Entity_Id;

      function Valid_Ancestor_Type return Boolean;
      --  Verify that the type of the ancestor part is a non-private ancestor
      --  of the expected type.

      function Valid_Ancestor_Type return Boolean is
         Imm_Type : Entity_Id;

      begin
         Imm_Type := Base_Type (Typ);
         while Is_Derived_Type (Imm_Type)
           and then Etype (Imm_Type) /= Base_Type (A_Type)
         loop
            Imm_Type := Etype (Base_Type (Imm_Type));
         end loop;

         if Etype (Imm_Type) /= Base_Type (A_Type) then
            Error_Msg_NE ("expect ancestor type of &", A, Typ);
            return False;
         else
            return True;
         end if;
      end Valid_Ancestor_Type;

   --  Start of processing for Resolve_Extension_Aggregate

   begin
      Analyze (A);

      if not Is_Tagged_Type (Typ) then
         Error_Msg_N ("type of extension aggregate must be tagged", N);
         return;

      elsif Is_Limited_Type (Typ) then
         Error_Msg_N ("aggregate type cannot be limited", N);
         return;

      elsif Is_Class_Wide_Type (Typ) then
         Error_Msg_N ("aggregate cannot be of a class-wide type", N);
         return;
      end if;

      if Is_Entity_Name (A)
        and then Is_Type (Entity (A))
      then
         A_Type   := Get_Full_View (Entity (A));
         Imm_Type := Base_Type (Typ);

         if Valid_Ancestor_Type then
            Set_Entity (A, A_Type);
            Set_Etype  (A, A_Type);

            Validate_Ancestor_Part (N);
            Resolve_Record_Aggregate (N, Typ);
         end if;

      elsif Nkind (A) /= N_Aggregate then
         if Is_Overloaded (A) then
            A_Type := Any_Type;
            Get_First_Interp (A, I, It);

            while Present (It.Typ) loop

               if Is_Tagged_Type (It.Typ)
                  and then not Is_Limited_Type (It.Typ)
               then
                  if A_Type /= Any_Type then
                     Error_Msg_N ("cannot resolve expression", A);
                     return;
                  else
                     A_Type := It.Typ;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;

            if A_Type = Any_Type then
               Error_Msg_N
                 ("ancestor part must be non-limited tagged type", A);
               return;
            end if;

         else
            A_Type := Etype (A);
         end if;

         if Valid_Ancestor_Type then
            Resolve (A, A_Type);
            Check_Non_Static_Context (A);
            Resolve_Record_Aggregate (N, Typ);
         end if;

      else
         Error_Msg_N (" No unique type for this aggregate",  A);
      end if;

   end Resolve_Extension_Aggregate;

   ------------------------------
   -- Resolve_Record_Aggregate --
   ------------------------------

   procedure Resolve_Record_Aggregate (N : Node_Id; Typ : Entity_Id) is
      Regular_Aggr    : constant Boolean := Nkind (N) /= N_Extension_Aggregate;

      New_Assoc_List  : List_Id := New_List;
      New_Assoc       : Node_Id;
      --  New_Assoc_List is the newly built list of N_Component_Association
      --  nodes. New_Assoc is one such N_Component_Association node in it.
      --  Please note that while Assoc and New_Assoc contain the same
      --  kind of nodes, they are used to iterate over two different
      --  N_Component_Association lists.

      Others_Etype : Entity_Id := Empty;
      --  This variable is used to save the Etype of the last record component
      --  that takes its value from the others choice. Its purpose is:
      --
      --    (a) make sure the others choice is useful
      --
      --    (b) make sure the type of all the components whose value is
      --        subsumed by the others choice are the same.
      --
      --  This variable is updated as a side effect of function Get_Value

      procedure Add_Association (Component : Entity_Id; Expr : Node_Id);
      --  Builds a new N_Component_Association node which associates
      --  Component to expression Expr and adds it to the new association
      --  list New_Assoc_List being built.

      function Discr_Present (Discr : Entity_Id) return Boolean;
      --  If aggregate N is a regular aggregate this routine will return True.
      --  Otherwise, if N is an extension aggreagte, Discr is a discriminant
      --  whose value may already have been specified by N's ancestor part,
      --  this routine checks whether this is indeed the case and if so
      --  returns False, signaling that no value for Discr should appear in the
      --  N's aggregate part. Also, in this case, the routine appends to
      --  New_Assoc_List Discr the discriminant value specified in the ancestor
      --  part.

      function Get_Value
        (Compon                 : Node_Id;
         From                   : List_Id;
         Consider_Others_Choice : Boolean := False)
         return                   Node_Id;
      --  Given a record component stored in parameter Compon, the
      --  following function returns its value as it appears in the list
      --  From, which is a list of N_Component_Association nodes. If no
      --  component association has a choice for the searched component,
      --  the value provided by the others choice is returned, if there
      --  is  one and Consider_Others_Choice is set to true. Otherwise
      --  Empty is returned. If there is more than one component association
      --  giving a value for the searched record component, an error message
      --  is emitted and the first found value is returned.
      --
      --  If Consider_Others_Choice is set and the returned expression comes
      --  from the others choice, then Others_Etype is set as a side effect.
      --  An error message is emitted if the components taking their value
      --  from the others choice do not have same type.

      procedure Resolve_Aggr_Expr (Expr : Node_Id; Component : Node_Id);
      --  Analyzes and resolves expression Expr against the Etype of the
      --  Component. This routine also applies all appropriate checks to Expr.
      --  It finally saves a Expr in the newly created association list that
      --  will be attached to the final record aggregate. Note that if the
      --  Parent pointer of Expr is not set then Expr was produced with a
      --  New_copy_Tree or some such.

      ---------------------
      -- Add_Association --
      ---------------------

      procedure Add_Association (Component : Entity_Id; Expr : Node_Id) is
         New_Assoc   : Node_Id;
         Choice_List : List_Id := New_List;

      begin
         Append (New_Occurrence_Of (Component, Sloc (Expr)), Choice_List);
         New_Assoc :=
           Make_Component_Association (Sloc (Expr),
             Choices    => Choice_List,
             Expression => Expr);
         Append (New_Assoc, New_Assoc_List);
      end Add_Association;

      -------------------
      -- Discr_Present --
      -------------------

      function Discr_Present (Discr : Entity_Id) return Boolean is
         Loc : Source_Ptr;

         Ancestor     : Node_Id;
         Discr_Expr   : Node_Id;

         Ancestor_Typ : Entity_Id;
         Orig_Discr   : Entity_Id;
         D            : Entity_Id;
         D_Val        : Elmt_Id := No_Elmt; -- stop junk warning

         Ancestor_Is_Subtyp : Boolean;

      begin
         if Regular_Aggr then
            return True;
         end if;

         Ancestor     := Ancestor_Part (N);
         Ancestor_Typ := Etype (Ancestor);
         Loc          := Sloc (Ancestor);

         Ancestor_Is_Subtyp :=
           Is_Entity_Name (Ancestor) and then Is_Type (Entity (Ancestor));

         --  If the ancestor part has no discriminants clearly N's aggregate
         --  part must provide a value for Discr.

         if not Has_Discriminants (Ancestor_Typ) then
            return True;

         --  If the ancestor part is an unconstrained subtype mark then the
         --  Discr must be present in N's aggregate part.

         elsif Ancestor_Is_Subtyp
           and then not Is_Constrained (Entity (Ancestor))
         then
            return True;
         end if;

         --  Now look to see if Discr was specified in the ancestor part.

         Orig_Discr := Original_Record_Component (Discr);
         D          := First_Discriminant (Ancestor_Typ);

         if Ancestor_Is_Subtyp then
            D_Val := First_Elmt (Discriminant_Constraint (Entity (Ancestor)));
         end if;

         while Present (D) loop
            --  If Ancestor has already specified Disc value than
            --  insert its value in the final aggregate.

            if Original_Record_Component (D) = Orig_Discr then
               if Ancestor_Is_Subtyp then
                  Discr_Expr := New_Copy_Tree (Node (D_Val));
               else
                  Discr_Expr :=
                    Make_Selected_Component (Loc,
                      Prefix        => Duplicate_Subexpr (Ancestor),
                      Selector_Name => New_Occurrence_Of (Discr, Loc));
               end if;

               Resolve_Aggr_Expr (Discr_Expr, Discr);
               return False;
            end if;

            Next_Discriminant (D);

            if Ancestor_Is_Subtyp then
               Next_Elmt (D_Val);
            end if;
         end loop;

         return True;
      end Discr_Present;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value
        (Compon                 : Node_Id;
         From                   : List_Id;
         Consider_Others_Choice : Boolean := False)
         return                   Node_Id
      is
         Assoc         : Node_Id;
         Expr          : Node_Id := Empty;
         Selector_Name : Node_Id;

      begin
         if Present (From) then
            Assoc := First (From);
         else
            return Empty;
         end if;

         while Present (Assoc) loop
            Selector_Name := First (Choices (Assoc));
            while Present (Selector_Name) loop
               if Nkind (Selector_Name) = N_Others_Choice then
                  if Consider_Others_Choice and then No (Expr) then
                     if Present (Others_Etype) and then
                        Base_Type (Others_Etype) /= Base_Type (Etype (Compon))
                     then
                        Error_Msg_N ("components in OTHERS choice must " &
                                     "have same type", Selector_Name);
                     end if;

                     Others_Etype := Etype (Compon);

                     --  We need to duplicate the expression for each
                     --  successive component covered by the others choice.
                     --  If the expression is itself an array aggregate with
                     --  "others", its subtype must be obtained from the
                     --  current component, and therefore it must be (at least
                     --  partly) reanalyzed.

                     if Analyzed (Expression (Assoc)) then
                        Expr := New_Copy_Tree (Expression (Assoc));

                        if Nkind (Expr) = N_Aggregate
                          and then Is_Array_Type (Etype (Expr))
                          and then No (Expressions (Expr))
                          and then
                            Nkind (First (Choices
                              (First (Component_Associations (Expr)))))
                                = N_Others_Choice
                        then
                           Set_Analyzed (Expr, False);
                        end if;

                        return Expr;

                     else
                        return Expression (Assoc);
                     end if;
                  end if;

               elsif Chars (Compon) = Chars (Selector_Name) then
                  if No (Expr) then
                     --  We need to duplicate the expression when several
                     --  components are grouped together with a "|" choice.
                     --  For instance "filed1 | filed2 => Expr"

                     if Present (Next (Selector_Name)) then
                        Expr := New_Copy_Tree (Expression (Assoc));
                     else
                        Expr := Expression (Assoc);
                     end if;

                  else
                     Error_Msg_NE
                       ("more than one value supplied for &",
                        Selector_Name, Compon);

                  end if;
               end if;

               Next (Selector_Name);
            end loop;

            Next (Assoc);
         end loop;

         return Expr;
      end Get_Value;

      -----------------------
      -- Resolve_Aggr_Expr --
      -----------------------

      procedure Resolve_Aggr_Expr (Expr : Node_Id; Component : Node_Id) is
         New_C     : Entity_Id := Component;
         Expr_Type : Entity_Id := Empty;

         function Has_Expansion_Delayed (Expr : Node_Id) return Boolean;
         --  If the expression is an aggregate (possibly qualified) then its
         --  expansion is delayed until the enclosing aggregate is expanded
         --  into assignments. In that case, do not generate checks on the
         --  expression, because they will be generated later, and will other-
         --  wise force a copy (to remove side-effects) that would leave a
         --  dynamic-sized aggregate in the code, something that gigi cannot
         --  handle.

         Relocate  : Boolean;
         --  Set to True if the resolved Expr node needs to be relocated
         --  when attached to the newly created association list. This node
         --  need not be relocated if its parent pointer is not set.
         --  In fact in this case Expr is the output of a New_Copy_Tree call.
         --  if Relocate is True then we have analyzed the expression node
         --  in the original aggregate and hence it needs to be relocated
         --  when moved over the new association list.

         function Has_Expansion_Delayed (Expr : Node_Id) return Boolean is
            Kind : constant Node_Kind := Nkind (Expr);

         begin
            return ((Kind = N_Aggregate
                       or else Kind = N_Extension_Aggregate)
                     and then Present (Etype (Expr))
                     and then Is_Record_Type (Etype (Expr))
                     and then Expansion_Delayed (Expr))

              or else (Kind = N_Qualified_Expression
                        and then Has_Expansion_Delayed (Expression (Expr)));
         end Has_Expansion_Delayed;

      --  Start of processing for  Resolve_Aggr_Expr

      begin
         --  If the type of the component is elementary or the type of the
         --  aggregate does not contain discriminants, use the type of the
         --  component to resolve Expr.

         if Is_Elementary_Type (Etype (Component))
           or else not Has_Discriminants (Etype (N))
         then
            Expr_Type := Etype (Component);

         --  Otherwise we have to pick up the new type of the component from
         --  the new costrained subtype of the aggregate. In fact components
         --  which are of a composite type might be constrained by a
         --  discriminant, and we want to resolve Expr against the subtype were
         --  all discriminant occurrences are replaced with their actual value.

         else
            New_C := First_Component (Etype (N));
            while Present (New_C) loop
               if Chars (New_C) = Chars (Component) then
                  Expr_Type := Etype (New_C);
                  exit;
               end if;

               Next_Component (New_C);
            end loop;

            pragma Assert (Present (Expr_Type));

            --  For each range in an array type where a discriminant has been
            --  replaced with the constraint, check that this range is within
            --  the range of the base type. This checks is done in the
            --  _init_proc for regular objects, but has to be done here for
            --  aggregates since no _init_proc is called for them.

            if Is_Array_Type (Expr_Type) then
               declare
                  Index          : Node_Id := First_Index (Expr_Type);
                  --  Range of the current constrained index in the array.

                  Orig_Index     : Node_Id := First_Index (Etype (Component));
                  --  Range corresponding to the range Index above in the
                  --  original unconstrained record type. The bounds of this
                  --  range may be governed by discriminants.

                  Unconstr_Index : Node_Id := First_Index (Etype (Expr_Type));
                  --  Range corresponding to the range Index above for the
                  --  unconstrained array type. This range is needed to apply
                  --  range checks.

               begin
                  while Present (Index) loop
                     if Depends_On_Discriminant (Orig_Index) then
                        Apply_Range_Check (Index, Etype (Unconstr_Index));
                     end if;

                     Next_Index (Index);
                     Next_Index (Orig_Index);
                     Next_Index (Unconstr_Index);
                  end loop;
               end;
            end if;
         end if;

         --  If the Parent pointer of Expr is not set, Expr is an expression
         --  duplicated by New_Tree_Copy (this happens for record aggregates
         --  that look like (Field1 | Filed2 => Expr) or (others => Expr)).
         --  Such a duplicated expression must be attached to the tree
         --  before analysis and resolution to enforce the rule that a tree
         --  fragment should never be analyzed or resolved unless it is
         --  attached to the current compilation unit.

         if No (Parent (Expr)) then
            Set_Parent (Expr, N);
            Relocate := False;
         else
            Relocate := True;
         end if;

         Analyze_And_Resolve (Expr, Expr_Type);
         Check_Non_Static_Context (Expr);

         if not Has_Expansion_Delayed (Expr) then
            Aggregate_Constraint_Checks (Expr, Expr_Type);
         end if;

         if Raises_Constraint_Error (Expr) then
            Set_Raises_Constraint_Error (N);
         end if;

         if Relocate then
            Add_Association (New_C, Relocate_Node (Expr));
         else
            Add_Association (New_C, Expr);
         end if;

      end Resolve_Aggr_Expr;

      --  Resolve_Record_Aggregate local variables

      Assoc : Node_Id;
      --  N_Component_Association node belonging to the input aggregate N

      Expr            : Node_Id;
      Positional_Expr : Node_Id;

      Component      : Entity_Id;
      Component_Elmt : Elmt_Id;
      Components     : Elist_Id := New_Elmt_List;
      --  Components is the list of the record components whose value must
      --  be provided in the aggregate. This list does include discriminants.

   --  Start of processing for Resolve_Record_Aggregate

   begin
      --  We may end up calling Duplicate_Subexpr on expressions that are
      --  attached to New_Assoc_List. For this reason we need to attach it
      --  to the tree by setting its parent pointer to N. This parent point
      --  will change in STEP 8 below.

      Set_Parent (New_Assoc_List, N);

      --  STEP 1: abstract type and null record verification

      if Is_Abstract (Typ) then
         Error_Msg_N ("type of aggregate cannot be abstract",  N);
      end if;

      if No (First_Entity (Typ)) and then Null_Record_Present (N) then
         Set_Etype (N, Typ);
         return;

      elsif Present (First_Entity (Typ))
        and then Null_Record_Present (N)
        and then not Is_Tagged_Type (Typ)
      then
         Error_Msg_N ("record aggregate cannot be null", N);
         return;

      elsif No (First_Entity (Typ)) then
         Error_Msg_N ("record aggregate must be null", N);
         return;
      end if;

      --  STEP 2: Verify aggregate structure

      Step_2 : declare
         Selector_Name : Node_Id;
         Bad_Aggregate : Boolean := False;

      begin
         if Present (Component_Associations (N)) then
            Assoc := First (Component_Associations (N));
         else
            Assoc := Empty;
         end if;

         while Present (Assoc) loop
            Selector_Name := First (Choices (Assoc));
            while Present (Selector_Name) loop
               if Nkind (Selector_Name) = N_Identifier then
                  null;

               elsif Nkind (Selector_Name) = N_Others_Choice then
                  if Selector_Name /= First (Choices (Assoc))
                    or else Present (Next (Selector_Name))
                  then
                     Error_Msg_N ("OTHERS must appear alone in a choice list",
                                  Selector_Name);
                     return;

                  elsif Present (Next (Assoc)) then
                     Error_Msg_N ("OTHERS must appear last in an aggregate",
                                  Selector_Name);
                     return;
                  end if;

               else
                  Error_Msg_N
                    ("selector name should be identifier or OTHERS",
                     Selector_Name);
                  Bad_Aggregate := True;
               end if;

               Next (Selector_Name);
            end loop;

            Next (Assoc);
         end loop;

         if Bad_Aggregate then
            return;
         end if;
      end Step_2;

      --  STEP 3: Find discriminant Values

      Step_3 : declare
         Discrim               : Entity_Id;
         Missing_Discriminants : Boolean := False;

      begin
         if Present (Expressions (N)) then
            Positional_Expr := First (Expressions (N));
         else
            Positional_Expr := Empty;
         end if;

         if Has_Discriminants (Typ) then
            Discrim := First_Discriminant (Typ);
         else
            Discrim := Empty;
         end if;

         --  First find the discriminant values in the positional components

         while Present (Discrim) and then Present (Positional_Expr) loop
            if Discr_Present (Discrim) then
               Resolve_Aggr_Expr (Positional_Expr, Discrim);
               Next (Positional_Expr);
            end if;

            if Present (Get_Value (Discrim, Component_Associations (N))) then
               Error_Msg_NE
                 ("more than one value supplied for discriminant&",
                  N, Discrim);
            end if;

            Next_Discriminant (Discrim);
         end loop;

         --  Find remaining discriminant values, if any, among named components

         while Present (Discrim) loop
            Expr := Get_Value (Discrim, Component_Associations (N), True);

            if not Discr_Present (Discrim) then
               if Present (Expr) then
                  Error_Msg_NE
                    ("more than one value supplied for discriminant&",
                     N, Discrim);
               end if;

            elsif No (Expr) then
               Error_Msg_NE
                 ("no value supplied for discriminant &", N, Discrim);
               Missing_Discriminants := True;

            else
               Resolve_Aggr_Expr (Expr, Discrim);
            end if;

            Next_Discriminant (Discrim);
         end loop;

         if Missing_Discriminants then
            return;
         end if;

         --  At this point and until the beginning of STEP 6, New_Assoc_List
         --  contains only the discriminants and their values.

      end Step_3;

      --  STEP 4: Set the Etype of the record aggregate

      --  ??? This code is pretty much a copy of Sem_Ch3.Build_Subtype. That
      --  routine should really be exported in sem_util or some such and used
      --  in sem_ch3 and here rather than have a copy of the code which is a
      --  maintenance nightmare.

      --  ??? Performace WARNING. The current implementation creates a new
      --  itype for all aggregates whose base type is discriminated.
      --  This means that for record aggregates nested inside an array
      --  aggregate we will create a new itype for each record aggregate
      --  if the array cmponent type has discriminants. For large aggregates
      --  this may be a problem. What should be done in this case is
      --  to reuse itypes as much as possible.

      if Has_Discriminants (Typ) then
         Build_Constrained_Itype : declare
            Loc         : constant Source_Ptr := Sloc (N);
            Indic       : Node_Id;
            Subtyp_Decl : Node_Id;
            Def_Id      : Entity_Id;

            C : List_Id := New_List;

         begin
            New_Assoc := First (New_Assoc_List);
            while Present (New_Assoc) loop
               Append (Duplicate_Subexpr (Expression (New_Assoc)), To => C);
               Next (New_Assoc);
            end loop;

            Indic :=
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Occurrence_Of (Base_Type (Typ), Loc),
                Constraint  => Make_Index_Or_Discriminant_Constraint (Loc, C));

            Def_Id := Create_Itype (Ekind (Typ), N);

            Subtyp_Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Indication  => Indic);
            Set_Parent (Subtyp_Decl, Parent (N));

            --  Itypes must be analyzed with checks off (see itypes.ads).

            Analyze (Subtyp_Decl, Suppress => All_Checks);

            Set_Etype (N, Def_Id);
            Check_Static_Discriminated_Subtype
              (Def_Id, Expression (First (New_Assoc_List)));
         end Build_Constrained_Itype;

      else
         Set_Etype (N, Typ);
      end if;

      --  STEP 5: Get remaining components according to discriminant values

      Step_5 : declare
         Record_Def      : Node_Id;
         Parent_Typ      : Entity_Id;
         Root_Typ        : Entity_Id;
         Parent_Typ_List : Elist_Id;
         Parent_Elmt     : Elmt_Id;
         Errors_Found    : Boolean := False;
         Dnode           : Node_Id;

      begin
         if Is_Derived_Type (Typ) and then Is_Tagged_Type (Typ) then
            Parent_Typ_List := New_Elmt_List;

            --  If this is an extension aggregate, the component list must
            --  include all components that are not in the given ancestor
            --  type. Otherwise, the component list must include components
            --  of all ancestors.

            if Nkind (N) = N_Extension_Aggregate then
               Root_Typ := Base_Type (Etype (Ancestor_Part (N)));
            else
               Root_Typ := Root_Type (Typ);

               if Nkind (Parent (Base_Type (Root_Typ)))
                    = N_Private_Type_Declaration
               then
                  Error_Msg_NE
                    ("type of aggregate has private ancestor&!",
                     N, Root_Typ);
                  Error_Msg_N  ("must use extension aggregate!", N);
                  return;
               end if;

               Dnode := Declaration_Node (Base_Type (Root_Typ));

               --  If we don't get a full declaration, then we have some
               --  error which will get signalled later so skip this part.

               if Nkind (Dnode) = N_Full_Type_Declaration then
                  Record_Def := Type_Definition (Dnode);
                  Gather_Components (Typ,
                    Component_List (Record_Def),
                    Governed_By   => New_Assoc_List,
                    Into          => Components,
                    Report_Errors => Errors_Found);
               end if;
            end if;

            Parent_Typ  := Base_Type (Typ);
            while Parent_Typ /= Root_Typ loop

               Prepend_Elmt (Parent_Typ, To => Parent_Typ_List);
               Parent_Typ := Etype (Parent_Typ);

               if (Nkind (Parent (Base_Type (Parent_Typ))) =
                                        N_Private_Type_Declaration
                    or else Nkind (Parent (Base_Type (Parent_Typ))) =
                                        N_Private_Extension_Declaration)
               then
                  if Nkind (N) /= N_Extension_Aggregate then
                     Error_Msg_NE
                       ("type of aggregate has private ancestor&!",
                        N, Parent_Typ);
                     Error_Msg_N  ("must use extension aggregate!", N);
                     return;

                  elsif Parent_Typ /= Root_Typ then
                     Error_Msg_NE
                       ("ancestor part of aggregate must be private type&",
                         Ancestor_Part (N), Parent_Typ);
                     return;
                  end if;
               end if;
            end loop;

            --  Now collect components from all other ancestors.

            Parent_Elmt := First_Elmt (Parent_Typ_List);
            while Present (Parent_Elmt) loop
               Parent_Typ := Node (Parent_Elmt);
               Record_Def := Type_Definition (Parent (Base_Type (Parent_Typ)));
               Gather_Components (Empty,
                 Component_List (Record_Extension_Part (Record_Def)),
                 Governed_By   => New_Assoc_List,
                 Into          => Components,
                 Report_Errors => Errors_Found);

               Next_Elmt (Parent_Elmt);
            end loop;

         else
            Record_Def := Type_Definition (Parent (Base_Type (Typ)));

            if Null_Present (Record_Def) then
               null;
            else
               Gather_Components (Typ,
                 Component_List (Record_Def),
                 Governed_By   => New_Assoc_List,
                 Into          => Components,
                 Report_Errors => Errors_Found);
            end if;
         end if;

         if Errors_Found then
            return;
         end if;
      end Step_5;

      --  STEP 6: Find component Values

      Component := Empty;
      Component_Elmt := First_Elmt (Components);

      --  First scan the remaining positional associations in the aggregate.
      --  Remember that at this point Positional_Expr contains the current
      --  positional association if any is left after looking for discriminant
      --  values in step 3.

      while Present (Positional_Expr) and then Present (Component_Elmt) loop
         Component := Node (Component_Elmt);
         Resolve_Aggr_Expr (Positional_Expr, Component);

         if Present (Get_Value (Component, Component_Associations (N))) then
            Error_Msg_NE
              ("more than one value supplied for Component &", N, Component);
         end if;

         Next (Positional_Expr);
         Next_Elmt (Component_Elmt);
      end loop;

      if Present (Positional_Expr) then
         Error_Msg_N
           ("too many components for record aggregate", Positional_Expr);
      end if;

      --  Now scan for the named arguments of the aggregate

      while Present (Component_Elmt) loop
         Component := Node (Component_Elmt);
         Expr := Get_Value (Component, Component_Associations (N), True);

         if No (Expr) then
            Error_Msg_NE ("no value supplied for component &!", N, Component);
         else
            Resolve_Aggr_Expr (Expr, Component);
         end if;

         Next_Elmt (Component_Elmt);
      end loop;

      --  STEP 7: check for invalid components + check type in choice list

      Step_7 : declare
         Selectr : Node_Id;
         --  Selector name

         Typech  : Entity_Id;
         --  Type of first component in choice list

      begin
         if Present (Component_Associations (N)) then
            Assoc := First (Component_Associations (N));
         else
            Assoc := Empty;
         end if;

         Verification : while Present (Assoc) loop
            Selectr := First (Choices (Assoc));
            Typech := Empty;

            if Nkind (Selectr) = N_Others_Choice then
               if No (Others_Etype) then
                  Error_Msg_N
                    ("OTHERS must represent at least one component", Selectr);
               end if;

               exit Verification;
            end if;

            while Present (Selectr) loop
               New_Assoc := First (New_Assoc_List);
               while Present (New_Assoc) loop
                  Component := First (Choices (New_Assoc));
                  exit when Chars (Selectr) = Chars (Component);
                  Next (New_Assoc);
               end loop;

               --  If no association, this is not a legal component of
               --  of the type in question,  except if this is an internal
               --  component supplied by a previous expansion.

               if No (New_Assoc) then

                  if Chars (Selectr) /= Name_uTag
                    and then Chars (Selectr) /= Name_uParent
                    and then Chars (Selectr) /= Name_uController
                  then
                     if not Has_Discriminants (Typ) then
                        Error_Msg_Node_2 := Typ;
                        Error_Msg_N
                          ("& is not a component of}",
                            Selectr);
                     else
                        Error_Msg_N
                          ("& is not a component of the aggregate subtype",
                            Selectr);
                     end if;

                     Check_Misspelled_Component (Components, Selectr);
                  end if;

               elsif No (Typech) then
                  Typech := Base_Type (Etype (Component));

               elsif Typech /= Base_Type (Etype (Component)) then
                  Error_Msg_N
                    ("components in choice list must have same type", Selectr);
               end if;

               Next (Selectr);
            end loop;

            Next (Assoc);
         end loop Verification;
      end Step_7;

      --  STEP 8: replace the original aggregate

      Step_8 : declare
         New_Aggregate : Node_Id := New_Copy (N);

      begin
         Set_Expressions            (New_Aggregate, No_List);
         Set_Etype                  (New_Aggregate, Etype (N));
         Set_Component_Associations (New_Aggregate, New_Assoc_List);

         Rewrite (N, New_Aggregate);
      end Step_8;
   end Resolve_Record_Aggregate;

   ---------------------
   -- Sort_Case_Table --
   ---------------------

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type) is
      L : Int := Case_Table'First;
      U : Int := Case_Table'Last;
      K : Int;
      J : Int;
      T : Case_Bounds;

   begin
      K := L;

      while K /= U loop
         T := Case_Table (K + 1);
         J := K + 1;

         while J /= L
           and then Expr_Value (Case_Table (J - 1).Choice_Lo) >
                    Expr_Value (T.Choice_Lo)
         loop
            Case_Table (J) := Case_Table (J - 1);
            J := J - 1;
         end loop;

         Case_Table (J) := T;
         K := K + 1;
      end loop;
   end Sort_Case_Table;

end Sem_Aggr;
