------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A G G R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Namet.Sp; use Namet.Sp;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stand;    use Stand;
with Style;    use Style;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

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

   procedure Check_Can_Never_Be_Null (Typ : Entity_Id; Expr : Node_Id);
   --  Ada 2005 (AI-231): Check bad usage of null for a component for which
   --  null exclusion (NOT NULL) is specified. Typ can be an E_Array_Type for
   --  the array case (the component type of the array will be used) or an
   --  E_Component/E_Discriminant entity in the record case, in which case the
   --  type of the component will be used for the test. If Typ is any other
   --  kind of entity, the call is ignored. Expr is the component node in the
   --  aggregate which is known to have a null value. A warning message will be
   --  issued if the component is null excluding.
   --
   --  It would be better to pass the proper type for Typ ???

   procedure Check_Expr_OK_In_Limited_Aggregate (Expr : Node_Id);
   --  Check that Expr is either not limited or else is one of the cases of
   --  expressions allowed for a limited component association (namely, an
   --  aggregate, function call, or <> notation). Report error for violations.

   procedure Check_Qualified_Aggregate (Level : Nat; Expr : Node_Id);
   --  Given aggregate Expr, check that sub-aggregates of Expr that are nested
   --  at Level are qualified. If Level = 0, this applies to Expr directly.
   --  Only issue errors in formal verification mode.

   function Is_Top_Level_Aggregate (Expr : Node_Id) return Boolean;
   --  Return True of Expr is an aggregate not contained directly in another
   --  aggregate.

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
   --  While performing the semantic checks, this procedure builds a new
   --  Component_Association_List where each record field appears alone in a
   --  Component_Choice_List along with its corresponding expression. The
   --  record fields in the Component_Association_List appear in the same order
   --  in which they appear in the record type Typ.
   --
   --  Once this new Component_Association_List is built and all the semantic
   --  checks performed, the original aggregate subtree is replaced with the
   --  new named record aggregate just built. Note that subtree substitution is
   --  performed with Rewrite so as to be able to retrieve the original
   --  aggregate.
   --
   --  The aggregate subtree manipulation performed by Resolve_Record_Aggregate
   --  yields the aggregate format expected by Gigi. Typically, this kind of
   --  tree manipulations are done in the expander. However, because the
   --  semantic checks that need to be performed on record aggregates really go
   --  hand in hand with the record aggregate normalization, the aggregate
   --  subtree transformation is performed during resolution rather than
   --  expansion. Had we decided otherwise we would have had to duplicate most
   --  of the code in the expansion procedure Expand_Record_Aggregate. Note,
   --  however, that all the expansion concerning aggregates for tagged records
   --  is done in Expand_Record_Aggregate.
   --
   --  The algorithm of Resolve_Record_Aggregate proceeds as follows:
   --
   --  1. Make sure that the record type against which the record aggregate
   --     has to be resolved is not abstract. Furthermore if the type is a
   --     null aggregate make sure the input aggregate N is also null.
   --
   --  2. Verify that the structure of the aggregate is that of a record
   --     aggregate. Specifically, look for component associations and ensure
   --     that each choice list only has identifiers or the N_Others_Choice
   --     node. Also make sure that if present, the N_Others_Choice occurs
   --     last and by itself.
   --
   --  3. If Typ contains discriminants, the values for each discriminant is
   --     looked for. If the record type Typ has variants, we check that the
   --     expressions corresponding to each discriminant ruling the (possibly
   --     nested) variant parts of Typ, are static. This allows us to determine
   --     the variant parts to which the rest of the aggregate must conform.
   --     The names of discriminants with their values are saved in a new
   --     association list, New_Assoc_List which is later augmented with the
   --     names and values of the remaining components in the record type.
   --
   --     During this phase we also make sure that every discriminant is
   --     assigned exactly one value. Note that when several values for a given
   --     discriminant are found, semantic processing continues looking for
   --     further errors. In this case it's the first discriminant value found
   --     which we will be recorded.
   --
   --     IMPORTANT NOTE: For derived tagged types this procedure expects
   --     First_Discriminant and Next_Discriminant to give the correct list
   --     of discriminants, in the correct order.
   --
   --  4. After all the discriminant values have been gathered, we can set the
   --     Etype of the record aggregate. If Typ contains no discriminants this
   --     is straightforward: the Etype of N is just Typ, otherwise a new
   --     implicit constrained subtype of Typ is built to be the Etype of N.
   --
   --  5. Gather the remaining record components according to the discriminant
   --     values. This involves recursively traversing the record type
   --     structure to see what variants are selected by the given discriminant
   --     values. This processing is a little more convoluted if Typ is a
   --     derived tagged types since we need to retrieve the record structure
   --     of all the ancestors of Typ.
   --
   --  6. After gathering the record components we look for their values in the
   --     record aggregate and emit appropriate error messages should we not
   --     find such values or should they be duplicated.
   --
   --  7. We then make sure no illegal component names appear in the record
   --     aggregate and make sure that the type of the record components
   --     appearing in a same choice list is the same. Finally we ensure that
   --     the others choice, if present, is used to provide the value of at
   --     least a record component.
   --
   --  8. The original aggregate node is replaced with the new named aggregate
   --     built in steps 3 through 6, as explained earlier.
   --
   --  Given the complexity of record aggregate resolution, the primary goal of
   --  this routine is clarity and simplicity rather than execution and storage
   --  efficiency. If there are only positional components in the aggregate the
   --  running time is linear. If there are associations the running time is
   --  still linear as long as the order of the associations is not too far off
   --  the order of the components in the record type. If this is not the case
   --  the running time is at worst quadratic in the size of the association
   --  list.

   procedure Check_Misspelled_Component
     (Elements  : Elist_Id;
      Component : Node_Id);
   --  Give possible misspelling diagnostic if Component is likely to be a
   --  misspelling of one of the components of the Assoc_List. This is called
   --  by Resolve_Aggr_Expr after producing an invalid component error message.

   procedure Check_Static_Discriminated_Subtype (T : Entity_Id; V : Node_Id);
   --  An optimization: determine whether a discriminated subtype has a static
   --  constraint, and contains array components whose length is also static,
   --  either because they are constrained by the discriminant, or because the
   --  original component bounds are static.

   -----------------------------------------------------
   -- Subprograms used for ARRAY AGGREGATE Processing --
   -----------------------------------------------------

   function Resolve_Array_Aggregate
     (N              : Node_Id;
      Index          : Node_Id;
      Index_Constr   : Node_Id;
      Component_Typ  : Entity_Id;
      Others_Allowed : Boolean) return Boolean;
   --  This procedure performs the semantic checks for an array aggregate.
   --  True is returned if the aggregate resolution succeeds.
   --
   --  The procedure works by recursively checking each nested aggregate.
   --  Specifically, after checking a sub-aggregate nested at the i-th level
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
   --     choice is present, make sure it is allowed in the aggregate context.
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
   --    RecIP (P7b.all);
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
   --  This routine creates an implicit array subtype whose bounds are
   --  those defined by the aggregate. When this routine is invoked
   --  Resolve_Array_Aggregate has already processed aggregate N. Thus the
   --  Aggregate_Bounds of each sub-aggregate, is an N_Range node giving the
   --  sub-aggregate bounds. When building the aggregate itype, this function
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

      --  Ada 2005 (AI-230): Generate a conversion to an anonymous access
      --  component's type to force the appropriate accessibility checks.

      --  Ada 2005 (AI-231): Generate conversion to the null-excluding
      --  type to force the corresponding run-time check

      if Is_Access_Type (Check_Typ)
        and then ((Is_Local_Anonymous_Access (Check_Typ))
                    or else (Can_Never_Be_Null (Check_Typ)
                               and then not Can_Never_Be_Null (Exp_Typ)))
      then
         Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp, Check_Typ);
         Check_Unset_Reference (Exp);
      end if;

      --  This is really expansion activity, so make sure that expansion
      --  is on and is allowed.

      if not Expander_Active or else In_Spec_Expression then
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
              (Exp, "value not in range of}?", CE_Range_Check_Failed,
               Ent => Base_Type (Check_Typ),
               Typ => Base_Type (Check_Typ));

         elsif Is_Out_Of_Range (Exp, Check_Typ) then
            Apply_Compile_Time_Constraint_Error
              (Exp, "value not in range of}?", CE_Range_Check_Failed,
               Ent => Check_Typ,
               Typ => Check_Typ);

         elsif not Range_Checks_Suppressed (Check_Typ) then
            Apply_Scalar_Range_Check (Exp, Check_Typ);
         end if;

      --  Verify that target type is also scalar, to prevent view anomalies
      --  in instantiations.

      elsif (Is_Scalar_Type (Exp_Typ)
              or else Nkind (Exp) = N_String_Literal)
        and then Is_Scalar_Type (Check_Typ)
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
               Check_Unset_Reference (Exp);
            end if;
         else
            Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
            Analyze_And_Resolve (Exp, Check_Typ);
            Check_Unset_Reference (Exp);
         end if;

      end if;
   end Aggregate_Constraint_Checks;

   ------------------------
   -- Array_Aggr_Subtype --
   ------------------------

   function Array_Aggr_Subtype
     (N   : Node_Id;
      Typ : Entity_Id) return Entity_Id
   is
      Aggr_Dimension : constant Pos := Number_Dimensions (Typ);
      --  Number of aggregate index dimensions

      Aggr_Range : array (1 .. Aggr_Dimension) of Node_Id := (others => Empty);
      --  Constrained N_Range of each index dimension in our aggregate itype

      Aggr_Low   : array (1 .. Aggr_Dimension) of Node_Id := (others => Empty);
      Aggr_High  : array (1 .. Aggr_Dimension) of Node_Id := (others => Empty);
      --  Low and High bounds for each index dimension in our aggregate itype

      Is_Fully_Positional : Boolean := True;

      procedure Collect_Aggr_Bounds (N : Node_Id; Dim : Pos);
      --  N is an array (sub-)aggregate. Dim is the dimension corresponding
      --  to (sub-)aggregate N. This procedure collects and removes the side
      --  effects of the constrained N_Range nodes corresponding to each index
      --  dimension of our aggregate itype. These N_Range nodes are collected
      --  in Aggr_Range above.
      --
      --  Likewise collect in Aggr_Low & Aggr_High above the low and high
      --  bounds of each index dimension. If, when collecting, two bounds
      --  corresponding to the same dimension are static and found to differ,
      --  then emit a warning, and mark N as raising Constraint_Error.

      -------------------------
      -- Collect_Aggr_Bounds --
      -------------------------

      procedure Collect_Aggr_Bounds (N : Node_Id; Dim : Pos) is
         This_Range : constant Node_Id := Aggregate_Bounds (N);
         --  The aggregate range node of this specific sub-aggregate

         This_Low  : constant Node_Id := Low_Bound (Aggregate_Bounds (N));
         This_High : constant Node_Id := High_Bound (Aggregate_Bounds (N));
         --  The aggregate bounds of this specific sub-aggregate

         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         Remove_Side_Effects (This_Low,  Variable_Ref => True);
         Remove_Side_Effects (This_High, Variable_Ref => True);

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
                  Error_Msg_N ("sub-aggregate low bound mismatch?", N);
                  Error_Msg_N
                     ("\Constraint_Error will be raised at run time?", N);
               end if;
            end if;

            if Compile_Time_Known_Value (This_High) then
               if not Compile_Time_Known_Value (Aggr_High (Dim)) then
                  Aggr_High (Dim)  := This_High;

               elsif
                 Expr_Value (This_High) /= Expr_Value (Aggr_High (Dim))
               then
                  Set_Raises_Constraint_Error (N);
                  Error_Msg_N ("sub-aggregate high bound mismatch?", N);
                  Error_Msg_N
                     ("\Constraint_Error will be raised at run time?", N);
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
      --  The final itype of the overall aggregate

      Index_Constraints : constant List_Id := New_List;
      --  The list of index constraints of the aggregate itype

   --  Start of processing for Array_Aggr_Subtype

   begin
      --  Make sure that the list of index constraints is properly attached to
      --  the tree, and then collect the aggregate bounds.

      Set_Parent (Index_Constraints, N);
      Collect_Aggr_Bounds (N, 1);

      --  Build the list of constrained indexes of our aggregate itype

      for J in 1 .. Aggr_Dimension loop
         Create_Index : declare
            Index_Base : constant Entity_Id :=
                           Base_Type (Etype (Aggr_Range (J)));
            Index_Typ  : Entity_Id;

         begin
            --  Construct the Index subtype, and associate it with the range
            --  construct that generates it.

            Index_Typ :=
              Create_Itype (Subtype_Kind (Ekind (Index_Base)), Aggr_Range (J));

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

      Set_First_Rep_Item         (Itype, First_Rep_Item        (Typ));
      Set_Convention             (Itype, Convention            (Typ));
      Set_Depends_On_Private     (Itype, Has_Private_Component (Typ));
      Set_Etype                  (Itype, Base_Type             (Typ));
      Set_Has_Alignment_Clause   (Itype, Has_Alignment_Clause  (Typ));
      Set_Is_Aliased             (Itype, Is_Aliased            (Typ));
      Set_Depends_On_Private     (Itype, Depends_On_Private    (Typ));

      Copy_Suppress_Status (Index_Check,  Typ, Itype);
      Copy_Suppress_Status (Length_Check, Typ, Itype);

      Set_First_Index    (Itype, First (Index_Constraints));
      Set_Is_Constrained (Itype, True);
      Set_Is_Internal    (Itype, True);

      --  A simple optimization: purely positional aggregates of static
      --  components should be passed to gigi unexpanded whenever possible, and
      --  regardless of the staticness of the bounds themselves. Subsequent
      --  checks in exp_aggr verify that type is not packed, etc.

      Set_Size_Known_At_Compile_Time (Itype,
         Is_Fully_Positional
           and then Comes_From_Source (N)
           and then Size_Known_At_Compile_Time (Component_Type (Typ)));

      --  We always need a freeze node for a packed array subtype, so that we
      --  can build the Packed_Array_Type corresponding to the subtype. If
      --  expansion is disabled, the packed array subtype is not built, and we
      --  must not generate a freeze node for the type, or else it will appear
      --  incomplete to gigi.

      if Is_Packed (Itype)
        and then not In_Spec_Expression
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
     (Elements  : Elist_Id;
      Component : Node_Id)
   is
      Max_Suggestions   : constant := 2;

      Nr_Of_Suggestions : Natural := 0;
      Suggestion_1      : Entity_Id := Empty;
      Suggestion_2      : Entity_Id := Empty;
      Component_Elmt    : Elmt_Id;

   begin
      --  All the components of List are matched against Component and a count
      --  is maintained of possible misspellings. When at the end of the the
      --  analysis there are one or two (not more!) possible misspellings,
      --  these misspellings will be suggested as possible correction.

      Component_Elmt := First_Elmt (Elements);
      while Nr_Of_Suggestions <= Max_Suggestions
        and then Present (Component_Elmt)
      loop
         if Is_Bad_Spelling_Of
              (Chars (Node (Component_Elmt)),
               Chars (Component))
         then
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
         Error_Msg_NE -- CODEFIX
           ("\possible misspelling of&", Component, Suggestion_1);

      elsif Nr_Of_Suggestions = 2 then
         Error_Msg_Node_2 := Suggestion_2;
         Error_Msg_NE -- CODEFIX
           ("\possible misspelling of& or&", Component, Suggestion_1);
      end if;
   end Check_Misspelled_Component;

   ----------------------------------------
   -- Check_Expr_OK_In_Limited_Aggregate --
   ----------------------------------------

   procedure Check_Expr_OK_In_Limited_Aggregate (Expr : Node_Id) is
   begin
      if Is_Limited_Type (Etype (Expr))
         and then Comes_From_Source (Expr)
         and then not In_Instance_Body
      then
         if not OK_For_Limited_Init (Etype (Expr), Expr) then
            Error_Msg_N ("initialization not allowed for limited types", Expr);
            Explain_Limited_Type (Etype (Expr), Expr);
         end if;
      end if;
   end Check_Expr_OK_In_Limited_Aggregate;

   -------------------------------
   -- Check_Qualified_Aggregate --
   -------------------------------

   procedure Check_Qualified_Aggregate (Level : Nat; Expr : Node_Id) is
      Comp_Expr : Node_Id;
      Comp_Assn : Node_Id;

   begin
      if Level = 0 then
         if Nkind (Parent (Expr)) /= N_Qualified_Expression then
            Check_SPARK_Restriction ("aggregate should be qualified", Expr);
         end if;

      else
         Comp_Expr := First (Expressions (Expr));
         while Present (Comp_Expr) loop
            if Nkind (Comp_Expr) = N_Aggregate then
               Check_Qualified_Aggregate (Level - 1, Comp_Expr);
            end if;

            Comp_Expr := Next (Comp_Expr);
         end loop;

         Comp_Assn := First (Component_Associations (Expr));
         while Present (Comp_Assn) loop
            Comp_Expr := Expression (Comp_Assn);

            if Nkind (Comp_Expr) = N_Aggregate then
               Check_Qualified_Aggregate (Level - 1, Comp_Expr);
            end if;

            Comp_Assn := Next (Comp_Assn);
         end loop;
      end if;
   end Check_Qualified_Aggregate;

   ----------------------------------------
   -- Check_Static_Discriminated_Subtype --
   ----------------------------------------

   procedure Check_Static_Discriminated_Subtype (T : Entity_Id; V : Node_Id) is
      Disc : constant Entity_Id := First_Discriminant (T);
      Comp : Entity_Id;
      Ind  : Entity_Id;

   begin
      if Has_Record_Rep_Clause (T) then
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

      --  On exit, all components have statically known sizes

      Set_Size_Known_At_Compile_Time (T);
   end Check_Static_Discriminated_Subtype;

   -------------------------
   -- Is_Others_Aggregate --
   -------------------------

   function Is_Others_Aggregate (Aggr : Node_Id) return Boolean is
   begin
      return No (Expressions (Aggr))
        and then
          Nkind (First (Choices (First (Component_Associations (Aggr)))))
            = N_Others_Choice;
   end Is_Others_Aggregate;

   ----------------------------
   -- Is_Top_Level_Aggregate --
   ----------------------------

   function Is_Top_Level_Aggregate (Expr : Node_Id) return Boolean is
   begin
      return Nkind (Parent (Expr)) /= N_Aggregate
        and then (Nkind (Parent (Expr)) /= N_Component_Association
                   or else Nkind (Parent (Parent (Expr))) /= N_Aggregate);
   end Is_Top_Level_Aggregate;

   --------------------------------
   -- Make_String_Into_Aggregate --
   --------------------------------

   procedure Make_String_Into_Aggregate (N : Node_Id) is
      Exprs  : constant List_Id    := New_List;
      Loc    : constant Source_Ptr := Sloc (N);
      Str    : constant String_Id  := Strval (N);
      Strlen : constant Nat        := String_Length (Str);
      C      : Char_Code;
      C_Node : Node_Id;
      New_N  : Node_Id;
      P      : Source_Ptr;

   begin
      P := Loc + 1;
      for J in  1 .. Strlen loop
         C := Get_String_Char (Str, J);
         Set_Character_Literal_Name (C);

         C_Node :=
           Make_Character_Literal (P,
             Chars              => Name_Find,
             Char_Literal_Value => UI_From_CC (C));
         Set_Etype (C_Node, Any_Character);
         Append_To (Exprs, C_Node);

         P := P + 1;
         --  Something special for wide strings???
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
      Loc   : constant Source_Ptr := Sloc (N);
      Pkind : constant Node_Kind  := Nkind (Parent (N));

      Aggr_Subtyp : Entity_Id;
      --  The actual aggregate subtype. This is not necessarily the same as Typ
      --  which is the subtype of the context in which the aggregate was found.

   begin
      --  Ignore junk empty aggregate resulting from parser error

      if No (Expressions (N))
        and then No (Component_Associations (N))
        and then not Null_Record_Present (N)
      then
         return;
      end if;

      --  If the aggregate has box-initialized components, its type must be
      --  frozen so that initialization procedures can properly be called
      --  in the resolution that follows.  The replacement of boxes with
      --  initialization calls is properly an expansion activity but it must
      --  be done during revolution.

      if Expander_Active
        and then  Present (Component_Associations (N))
      then
         declare
            Comp : Node_Id;

         begin
            Comp := First (Component_Associations (N));
            while Present (Comp) loop
               if Box_Present (Comp) then
                  Insert_Actions (N, Freeze_Entity (Typ, N));
                  exit;
               end if;

               Next (Comp);
            end loop;
         end;
      end if;

      --  An unqualified aggregate is restricted in SPARK to:

      --    An aggregate item inside an aggregate for a multi-dimensional array

      --    An expression being assigned to an unconstrained array, but only if
      --    the aggregate specifies a value for OTHERS only.

      if Nkind (Parent (N)) = N_Qualified_Expression then
         if Is_Array_Type (Typ) then
            Check_Qualified_Aggregate (Number_Dimensions (Typ), N);
         else
            Check_Qualified_Aggregate (1, N);
         end if;
      else
         if Is_Array_Type (Typ)
           and then Nkind (Parent (N)) = N_Assignment_Statement
           and then not Is_Constrained (Etype (Name (Parent (N))))
         then
            if not Is_Others_Aggregate (N) then
               Check_SPARK_Restriction
                 ("array aggregate should have only OTHERS", N);
            end if;

         elsif Is_Top_Level_Aggregate (N) then
            Check_SPARK_Restriction ("aggregate should be qualified", N);

         --  The legality of this unqualified aggregate is checked by calling
         --  Check_Qualified_Aggregate from one of its enclosing aggregate,
         --  unless one of these already causes an error to be issued.

         else
            null;
         end if;
      end if;

      --  Check for aggregates not allowed in configurable run-time mode.
      --  We allow all cases of aggregates that do not come from source, since
      --  these are all assumed to be small (e.g. bounds of a string literal).
      --  We also allow aggregates of types we know to be small.

      if not Support_Aggregates_On_Target
        and then Comes_From_Source (N)
        and then (not Known_Static_Esize (Typ) or else Esize (Typ) > 64)
      then
         Error_Msg_CRT ("aggregate", N);
      end if;

      --  Ada 2005 (AI-287): Limited aggregates allowed

      --  In an instance, ignore aggregate subcomponents tnat may be limited,
      --  because they originate in view conflicts. If the original aggregate
      --  is legal and the actuals are legal, the aggregate itself is legal.

      if Is_Limited_Type (Typ)
        and then Ada_Version < Ada_2005
        and then not In_Instance
      then
         Error_Msg_N ("aggregate type cannot be limited", N);
         Explain_Limited_Type (Typ, N);

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

         --  Do not perform this transformation if this was a string literal to
         --  start with, whose components needed constraint checks, or if the
         --  component type is non-static, because it will require those checks
         --  and be transformed back into an aggregate.

         if Number_Dimensions (Typ) = 1
           and then Is_Standard_Character_Type (Component_Type (Typ))
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
                     Store_String_Char (UI_To_CC (Char_Literal_Value (Expr)));
                     Next (Expr);
                  end loop;

                  Rewrite (N, Make_String_Literal (Loc, End_String));

                  Analyze_And_Resolve (N, Typ);
                  return;
               end if;
            end;
         end if;

         --  Here if we have a real aggregate to deal with

         Array_Aggregate : declare
            Aggr_Resolved : Boolean;

            Aggr_Typ : constant Entity_Id := Etype (Typ);
            --  This is the unconstrained array type, which is the type against
            --  which the aggregate is to be resolved. Typ itself is the array
            --  type of the context which may not be the same subtype as the
            --  subtype for the final aggregate.

         begin
            --  In the following we determine whether an OTHERS choice is
            --  allowed inside the array aggregate. The test checks the context
            --  in which the array aggregate occurs. If the context does not
            --  permit it, or the aggregate type is unconstrained, an OTHERS
            --  choice is not allowed (except that it is always allowed on the
            --  right-hand side of an assignment statement; in this case the
            --  constrainedness of the type doesn't matter).

            --  If expansion is disabled (generic context, or semantics-only
            --  mode) actual subtypes cannot be constructed, and the type of an
            --  object may be its unconstrained nominal type. However, if the
            --  context is an assignment, we assume that OTHERS is allowed,
            --  because the target of the assignment will have a constrained
            --  subtype when fully compiled.

            --  Note that there is no node for Explicit_Actual_Parameter.
            --  To test for this context we therefore have to test for node
            --  N_Parameter_Association which itself appears only if there is a
            --  formal parameter. Consequently we also need to test for
            --  N_Procedure_Call_Statement or N_Function_Call.

            Set_Etype (N, Aggr_Typ);  --  May be overridden later on

            if Pkind = N_Assignment_Statement
              or else (Is_Constrained (Typ)
                        and then
                          (Pkind = N_Parameter_Association     or else
                           Pkind = N_Function_Call             or else
                           Pkind = N_Procedure_Call_Statement  or else
                           Pkind = N_Generic_Association       or else
                           Pkind = N_Formal_Object_Declaration or else
                           Pkind = N_Simple_Return_Statement   or else
                           Pkind = N_Object_Declaration        or else
                           Pkind = N_Component_Declaration     or else
                           Pkind = N_Parameter_Specification   or else
                           Pkind = N_Qualified_Expression      or else
                           Pkind = N_Aggregate                 or else
                           Pkind = N_Extension_Aggregate       or else
                           Pkind = N_Component_Association))
            then
               Aggr_Resolved :=
                 Resolve_Array_Aggregate
                   (N,
                    Index          => First_Index (Aggr_Typ),
                    Index_Constr   => First_Index (Typ),
                    Component_Typ  => Component_Type (Typ),
                    Others_Allowed => True);

            elsif not Expander_Active
              and then Pkind = N_Assignment_Statement
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

               --  A parenthesized expression may have been intended as an
               --  aggregate, leading to a type error when analyzing the
               --  component. This can also happen for a nested component
               --  (see Analyze_Aggr_Expr).

               if Paren_Count (N) > 0 then
                  Error_Msg_N
                    ("positional aggregate cannot have one component", N);
               end if;

               Aggr_Subtyp := Any_Composite;

            else
               Aggr_Subtyp := Array_Aggr_Subtype (N, Typ);
            end if;

            Set_Etype (N, Aggr_Subtyp);
         end Array_Aggregate;

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then (In_Inlined_Body or In_Instance_Body)
        and then Is_Composite_Type (Full_View (Typ))
      then
         Resolve (N, Full_View (Typ));

      else
         Error_Msg_N ("illegal context for aggregate", N);
      end if;

      --  If we can determine statically that the evaluation of the aggregate
      --  raises Constraint_Error, then replace the aggregate with an
      --  N_Raise_Constraint_Error node, but set the Etype to the right
      --  aggregate subtype. Gigi needs this.

      if Raises_Constraint_Error (N) then
         Aggr_Subtyp := Etype (N);
         Rewrite (N,
           Make_Raise_Constraint_Error (Loc, Reason => CE_Range_Check_Failed));
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
      Others_Allowed : Boolean) return Boolean
   is
      Loc : constant Source_Ptr := Sloc (N);

      Failure : constant Boolean := False;
      Success : constant Boolean := True;

      Index_Typ      : constant Entity_Id := Etype (Index);
      Index_Typ_Low  : constant Node_Id   := Type_Low_Bound  (Index_Typ);
      Index_Typ_High : constant Node_Id   := Type_High_Bound (Index_Typ);
      --  The type of the index corresponding to the array sub-aggregate along
      --  with its low and upper bounds.

      Index_Base      : constant Entity_Id := Base_Type (Index_Typ);
      Index_Base_Low  : constant Node_Id   := Type_Low_Bound (Index_Base);
      Index_Base_High : constant Node_Id   := Type_High_Bound (Index_Base);
      --  Ditto for the base type

      function Add (Val : Uint; To : Node_Id) return Node_Id;
      --  Creates a new expression node where Val is added to expression To.
      --  Tries to constant fold whenever possible. To must be an already
      --  analyzed expression.

      procedure Check_Bound (BH : Node_Id; AH : in out Node_Id);
      --  Checks that AH (the upper bound of an array aggregate) is less than
      --  or equal to BH (the upper bound of the index base type). If the check
      --  fails, a warning is emitted, the Raises_Constraint_Error flag of N is
      --  set, and AH is replaced with a duplicate of BH.

      procedure Check_Bounds (L, H : Node_Id; AL, AH : Node_Id);
      --  Checks that range AL .. AH is compatible with range L .. H. Emits a
      --  warning if not and sets the Raises_Constraint_Error flag in N.

      procedure Check_Length (L, H : Node_Id; Len : Uint);
      --  Checks that range L .. H contains at least Len elements. Emits a
      --  warning if not and sets the Raises_Constraint_Error flag in N.

      function Dynamic_Or_Null_Range (L, H : Node_Id) return Boolean;
      --  Returns True if range L .. H is dynamic or null

      procedure Get (Value : out Uint; From : Node_Id; OK : out Boolean);
      --  Given expression node From, this routine sets OK to False if it
      --  cannot statically evaluate From. Otherwise it stores this static
      --  value into Value.

      function Resolve_Aggr_Expr
        (Expr        : Node_Id;
         Single_Elmt : Boolean) return Boolean;
      --  Resolves aggregate expression Expr. Returns False if resolution
      --  fails. If Single_Elmt is set to False, the expression Expr may be
      --  used to initialize several array aggregate elements (this can happen
      --  for discrete choices such as "L .. H => Expr" or the OTHERS choice).
      --  In this event we do not resolve Expr unless expansion is disabled.
      --  To know why, see the DELAYED COMPONENT RESOLUTION note above.
      --
      --  NOTE: In the case of "... => <>", we pass the in the
      --  N_Component_Association node as Expr, since there is no Expression in
      --  that case, and we need a Sloc for the error message.

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

            --  If the index type has a non standard representation, the
            --  attributes 'Val and 'Pos expand into function calls and the
            --  resulting expression is considered non-safe for reevaluation
            --  by the backend. Relocate it into a constant temporary in order
            --  to make it safe for reevaluation.

            if Has_Non_Standard_Rep (Etype (N)) then
               declare
                  Def_Id : Entity_Id;

               begin
                  Def_Id := Make_Temporary (Loc, 'R', Expr);
                  Set_Etype (Def_Id, Index_Typ);
                  Insert_Action (N,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Def_Id,
                      Object_Definition   => New_Reference_To (Index_Typ, Loc),
                      Constant_Present    => True,
                      Expression          => Relocate_Node (Expr)));

                  Expr := New_Reference_To (Def_Id, Loc);
               end;
            end if;
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
            Error_Msg_N ("\Constraint_Error will be raised at run time?", AH);

            --  You need to set AH to BH or else in the case of enumerations
            --  indexes we will not be able to resolve the aggregate bounds.

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

         OK_L : Boolean;
         OK_H : Boolean;

         OK_AL : Boolean;
         OK_AH  : Boolean;
         pragma Warnings (Off, OK_AL);
         pragma Warnings (Off, OK_AH);

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
            Error_Msg_N ("\Constraint_Error will be raised at run time?", N);
         end if;

         if OK_H and then Val_H < Val_AH then
            Set_Raises_Constraint_Error (N);
            Error_Msg_N ("upper bound of aggregate out of range?", N);
            Error_Msg_N ("\Constraint_Error will be raised at run time?", N);
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
            Error_Msg_N ("too many elements?", N);
            Error_Msg_N ("\Constraint_Error will be raised at run time?", N);
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
         Single_Elmt : Boolean) return Boolean
      is
         Nxt_Ind        : constant Node_Id := Next_Index (Index);
         Nxt_Ind_Constr : constant Node_Id := Next_Index (Index_Constr);
         --  Index is the current index corresponding to the expression

         Resolution_OK : Boolean := True;
         --  Set to False if resolution of the expression failed

      begin
         --  Defend against previous errors

         if Nkind (Expr) = N_Error
           or else Error_Posted (Expr)
         then
            return True;
         end if;

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
                 and then Nkind_In (Expr, N_String_Literal, N_Operator_Symbol)
               then
                  --  A string literal used in a multidimensional array
                  --  aggregate in place of the final one-dimensional
                  --  aggregate must not be enclosed in parentheses.

                  if Paren_Count (Expr) /= 0 then
                     Error_Msg_N ("no parenthesis allowed here", Expr);
                  end if;

                  Make_String_Into_Aggregate (Expr);

               else
                  Error_Msg_N ("nested array aggregate expected", Expr);

                  --  If the expression is parenthesized, this may be
                  --  a missing component association for a 1-aggregate.

                  if Paren_Count (Expr) > 0 then
                     Error_Msg_N
                       ("\if single-component aggregate is intended,"
                        & " write e.g. (1 ='> ...)", Expr);
                  end if;

                  return Failure;
               end if;
            end if;

            --  If it's "... => <>", nothing to resolve

            if Nkind (Expr) = N_Component_Association then
               pragma Assert (Box_Present (Expr));
               return Success;
            end if;

            --  Ada 2005 (AI-231): Propagate the type to the nested aggregate.
            --  Required to check the null-exclusion attribute (if present).
            --  This value may be overridden later on.

            Set_Etype (Expr, Etype (N));

            Resolution_OK := Resolve_Array_Aggregate
              (Expr, Nxt_Ind, Nxt_Ind_Constr, Component_Typ, Others_Allowed);

         else

            --  If it's "... => <>", nothing to resolve

            if Nkind (Expr) = N_Component_Association then
               pragma Assert (Box_Present (Expr));
               return Success;
            end if;

            --  Do not resolve the expressions of discrete or others choices
            --  unless the expression covers a single component, or the
            --  expander is inactive.

            --  In Alfa mode, expressions that can perform side-effects will be
            --  recognized by the gnat2why back-end, and the whole subprogram
            --  will be ignored. So semantic analysis can be performed safely.

            if Single_Elmt
              or else not Full_Expander_Active
              or else In_Spec_Expression
            then
               Analyze_And_Resolve (Expr, Component_Typ);
               Check_Expr_OK_In_Limited_Aggregate (Expr);
               Check_Non_Static_Context (Expr);
               Aggregate_Constraint_Checks (Expr, Component_Typ);
               Check_Unset_Reference (Expr);
            end if;
         end if;

         --  If an aggregate component has a type with predicates, an explicit
         --  predicate check must be applied, as for an assignment statement,
         --  because the aggegate might not be expanded into individual
         --  component assignments.

         if Present (Predicate_Function (Component_Typ)) then
            Apply_Predicate_Check (Expr, Component_Typ);
         end if;

         if Raises_Constraint_Error (Expr)
           and then Nkind (Parent (Expr)) /= N_Component_Association
         then
            Set_Raises_Constraint_Error (N);
         end if;

         --  If the expression has been marked as requiring a range check,
         --  then generate it here.

         if Do_Range_Check (Expr) then
            Set_Do_Range_Check (Expr, False);
            Generate_Range_Check (Expr, Component_Typ, CE_Range_Check_Failed);
         end if;

         return Resolution_OK;
      end Resolve_Aggr_Expr;

      --  Variables local to Resolve_Array_Aggregate

      Assoc   : Node_Id;
      Choice  : Node_Id;
      Expr    : Node_Id;

      Discard : Node_Id;
      pragma Warnings (Off, Discard);

      Aggr_Low  : Node_Id := Empty;
      Aggr_High : Node_Id := Empty;
      --  The actual low and high bounds of this sub-aggregate

      Choices_Low  : Node_Id := Empty;
      Choices_High : Node_Id := Empty;
      --  The lowest and highest discrete choices values for a named aggregate

      Nb_Elements : Uint := Uint_0;
      --  The number of elements in a positional aggregate

      Others_Present : Boolean := False;

      Nb_Choices : Nat := 0;
      --  Contains the overall number of named choices in this sub-aggregate

      Nb_Discrete_Choices : Nat := 0;
      --  The overall number of discrete choices (not counting others choice)

      Case_Table_Size : Nat;
      --  Contains the size of the case table needed to sort aggregate choices

   --  Start of processing for Resolve_Array_Aggregate

   begin
      --  Ignore junk empty aggregate resulting from parser error

      if No (Expressions (N))
        and then No (Component_Associations (N))
        and then not Null_Record_Present (N)
      then
         return False;
      end if;

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

                  if Ada_Version = Ada_83
                    and then Assoc /= First (Component_Associations (N))
                    and then Nkind_In (Parent (N), N_Assignment_Statement,
                                                   N_Object_Declaration)
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

      if Others_Present
        and then Nkind (Parent (N)) /= N_Component_Association
        and then No (Expressions (N))
        and then
          Nkind (First (Choices (First (Component_Associations (N)))))
            = N_Others_Choice
        and then Is_Elementary_Type (Component_Typ)
        and then False
      then
         declare
            Assoc : constant Node_Id := First (Component_Associations (N));
         begin
            Rewrite (Assoc,
              Make_Component_Association (Loc,
                 Choices =>
                   New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Occurrence_Of (Index_Typ, Loc),
                       Attribute_Name => Name_Range)),
                 Expression => Relocate_Node (Expression (Assoc))));
            return Resolve_Array_Aggregate
              (N, Index, Index_Constr, Component_Typ, Others_Allowed);
         end;
      end if;

      --  Protect against cascaded errors

      if Etype (Index_Typ) = Any_Type then
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
            --  Used to keep track of the number of discrete choices in the
            --  current association.

            Errors_Posted_On_Choices : Boolean := False;
            --  Keeps track of whether any choices have semantic errors

         begin
            --  STEP 2 (A): Check discrete choices validity

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

                  --  Case of subtype indication

                  elsif Nkind (Choice) = N_Subtype_Indication then
                     Resolve_Discrete_Subtype_Indication (Choice, Index_Base);

                     --  Does the subtype indication evaluation raise CE ?

                     Get_Index_Bounds (Subtype_Mark (Choice), S_Low, S_High);
                     Get_Index_Bounds (Choice, Low, High);
                     Check_Bounds (S_Low, S_High, Low, High);

                  --  Case of range or expression

                  else
                     Resolve (Choice, Index_Base);
                     Check_Unset_Reference (Choice);
                     Check_Non_Static_Context (Choice);

                     --  If semantic errors were posted on the choice, then
                     --  record that for possible early return from later
                     --  processing (see handling of enumeration choices).

                     if Error_Posted (Choice) then
                        Errors_Posted_On_Choices := True;
                     end if;

                     --  Do not range check a choice. This check is redundant
                     --  since this test is already done when we check that the
                     --  bounds of the array aggregate are within range.

                     Set_Do_Range_Check (Choice, False);

                     --  In SPARK, the choice must be static

                     if not (Is_Static_Expression (Choice)
                              or else (Nkind (Choice) = N_Range
                                        and then Is_Static_Range (Choice)))
                     then
                        Check_SPARK_Restriction
                          ("choice should be static", Choice);
                     end if;
                  end if;

                  --  If we could not resolve the discrete choice stop here

                  if Etype (Choice) = Any_Type then
                     return Failure;

                  --  If the discrete choice raises CE get its original bounds

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

               --  Ada 2005 (AI-231)

               if Ada_Version >= Ada_2005
                 and then Known_Null (Expression (Assoc))
               then
                  Check_Can_Never_Be_Null (Etype (N), Expression (Assoc));
               end if;

               --  Ada 2005 (AI-287): In case of default initialized component
               --  we delay the resolution to the expansion phase.

               if Box_Present (Assoc) then

                  --  Ada 2005 (AI-287): In case of default initialization of a
                  --  component the expander will generate calls to the
                  --  corresponding initialization subprogram. We need to call
                  --  Resolve_Aggr_Expr to check the rules about
                  --  dimensionality.

                  if not Resolve_Aggr_Expr (Assoc,
                                            Single_Elmt => Single_Choice)
                  then
                     return Failure;
                  end if;

               elsif not Resolve_Aggr_Expr (Expression (Assoc),
                                            Single_Elmt => Single_Choice)
               then
                  return Failure;

               --  Check incorrect use of dynamically tagged expression

               --  We differentiate here two cases because the expression may
               --  not be decorated. For example, the analysis and resolution
               --  of the expression associated with the others choice will be
               --  done later with the full aggregate. In such case we
               --  duplicate the expression tree to analyze the copy and
               --  perform the required check.

               elsif not Present (Etype (Expression (Assoc))) then
                  declare
                     Save_Analysis : constant Boolean := Full_Analysis;
                     Expr          : constant Node_Id :=
                                       New_Copy_Tree (Expression (Assoc));

                  begin
                     Expander_Mode_Save_And_Set (False);
                     Full_Analysis := False;

                     --  Analyze the expression, making sure it is properly
                     --  attached to the tree before we do the analysis.

                     Set_Parent (Expr, Parent (Expression (Assoc)));
                     Analyze (Expr);

                     --  If the expression is a literal, propagate this info
                     --  to the expression in the association, to enable some
                     --  optimizations downstream.

                     if Is_Entity_Name (Expr)
                       and then Present (Entity (Expr))
                       and then Ekind (Entity (Expr)) = E_Enumeration_Literal
                     then
                        Analyze_And_Resolve
                          (Expression (Assoc), Component_Typ);
                     end if;

                     Full_Analysis := Save_Analysis;
                     Expander_Mode_Restore;

                     if Is_Tagged_Type (Etype (Expr)) then
                        Check_Dynamically_Tagged_Expression
                          (Expr => Expr,
                           Typ  => Component_Type (Etype (N)),
                           Related_Nod => N);
                     end if;
                  end;

               elsif Is_Tagged_Type (Etype (Expression (Assoc))) then
                  Check_Dynamically_Tagged_Expression
                    (Expr        => Expression (Assoc),
                     Typ         => Component_Type (Etype (N)),
                     Related_Nod => N);
               end if;

               Next (Assoc);
            end loop;

            --  If aggregate contains more than one choice then these must be
            --  static. Sort them and check that they are contiguous.

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

            --  If Others is present, then bounds of aggregate come from the
            --  index constraint (not the choices in the aggregate itself).

            if Others_Present then
               Get_Index_Bounds (Index_Constr, Aggr_Low, Aggr_High);

            --  No others clause present

            else
               --  Special processing if others allowed and not present. This
               --  means that the bounds of the aggregate come from the index
               --  constraint (and the length must match).

               if Others_Allowed then
                  Get_Index_Bounds (Index_Constr, Aggr_Low, Aggr_High);

                  --  If others allowed, and no others present, then the array
                  --  should cover all index values. If it does not, we will
                  --  get a length check warning, but there is two cases where
                  --  an additional warning is useful:

                  --  If we have no positional components, and the length is
                  --  wrong (which we can tell by others being allowed with
                  --  missing components), and the index type is an enumeration
                  --  type, then issue appropriate warnings about these missing
                  --  components. They are only warnings, since the aggregate
                  --  is fine, it's just the wrong length. We skip this check
                  --  for standard character types (since there are no literals
                  --  and it is too much trouble to concoct them), and also if
                  --  any of the bounds have not-known-at-compile-time values.

                  --  Another case warranting a warning is when the length is
                  --  right, but as above we have an index type that is an
                  --  enumeration, and the bounds do not match. This is a
                  --  case where dubious sliding is allowed and we generate
                  --  a warning that the bounds do not match.

                  if No (Expressions (N))
                    and then Nkind (Index) = N_Range
                    and then Is_Enumeration_Type (Etype (Index))
                    and then not Is_Standard_Character_Type (Etype (Index))
                    and then Compile_Time_Known_Value (Aggr_Low)
                    and then Compile_Time_Known_Value (Aggr_High)
                    and then Compile_Time_Known_Value (Choices_Low)
                    and then Compile_Time_Known_Value (Choices_High)
                  then
                     --  If any of the expressions or range bounds in choices
                     --  have semantic errors, then do not attempt further
                     --  resolution, to prevent cascaded errors.

                     if Errors_Posted_On_Choices then
                        return Failure;
                     end if;

                     declare
                        ALo : constant Node_Id := Expr_Value_E (Aggr_Low);
                        AHi : constant Node_Id := Expr_Value_E (Aggr_High);
                        CLo : constant Node_Id := Expr_Value_E (Choices_Low);
                        CHi : constant Node_Id := Expr_Value_E (Choices_High);

                        Ent : Entity_Id;

                     begin
                        --  Warning case 1, missing values at start/end. Only
                        --  do the check if the number of entries is too small.

                        if (Enumeration_Pos (CHi) - Enumeration_Pos (CLo))
                              <
                           (Enumeration_Pos (AHi) - Enumeration_Pos (ALo))
                        then
                           Error_Msg_N
                             ("missing index value(s) in array aggregate?", N);

                           --  Output missing value(s) at start

                           if Chars (ALo) /= Chars (CLo) then
                              Ent := Prev (CLo);

                              if Chars (ALo) = Chars (Ent) then
                                 Error_Msg_Name_1 := Chars (ALo);
                                 Error_Msg_N ("\  %?", N);
                              else
                                 Error_Msg_Name_1 := Chars (ALo);
                                 Error_Msg_Name_2 := Chars (Ent);
                                 Error_Msg_N ("\  % .. %?", N);
                              end if;
                           end if;

                           --  Output missing value(s) at end

                           if Chars (AHi) /= Chars (CHi) then
                              Ent := Next (CHi);

                              if Chars (AHi) = Chars (Ent) then
                                 Error_Msg_Name_1 := Chars (Ent);
                                 Error_Msg_N ("\  %?", N);
                              else
                                 Error_Msg_Name_1 := Chars (Ent);
                                 Error_Msg_Name_2 := Chars (AHi);
                                 Error_Msg_N ("\  % .. %?", N);
                              end if;
                           end if;

                        --  Warning case 2, dubious sliding. The First_Subtype
                        --  test distinguishes between a constrained type where
                        --  sliding is not allowed (so we will get a warning
                        --  later that Constraint_Error will be raised), and
                        --  the unconstrained case where sliding is permitted.

                        elsif (Enumeration_Pos (CHi) - Enumeration_Pos (CLo))
                                 =
                              (Enumeration_Pos (AHi) - Enumeration_Pos (ALo))
                          and then Chars (ALo) /= Chars (CLo)
                          and then
                            not Is_Constrained (First_Subtype (Etype (N)))
                        then
                           Error_Msg_N
                             ("bounds of aggregate do not match target?", N);
                        end if;
                     end;
                  end if;
               end if;

               --  If no others, aggregate bounds come from aggregate

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

            --  Ada 2005 (AI-231)

            if Ada_Version >= Ada_2005
              and then Known_Null (Expr)
            then
               Check_Can_Never_Be_Null (Etype (N), Expr);
            end if;

            if not Resolve_Aggr_Expr (Expr, Single_Elmt => True) then
               return Failure;
            end if;

            --  Check incorrect use of dynamically tagged expression

            if Is_Tagged_Type (Etype (Expr)) then
               Check_Dynamically_Tagged_Expression
                 (Expr => Expr,
                  Typ  => Component_Type (Etype (N)),
                  Related_Nod => N);
            end if;

            Next (Expr);
         end loop;

         if Others_Present then
            Assoc := Last (Component_Associations (N));

            --  Ada 2005 (AI-231)

            if Ada_Version >= Ada_2005
              and then Known_Null (Assoc)
            then
               Check_Can_Never_Be_Null (Etype (N), Expression (Assoc));
            end if;

            --  Ada 2005 (AI-287): In case of default initialized component,
            --  we delay the resolution to the expansion phase.

            if Box_Present (Assoc) then

               --  Ada 2005 (AI-287): In case of default initialization of a
               --  component the expander will generate calls to the
               --  corresponding initialization subprogram. We need to call
               --  Resolve_Aggr_Expr to check the rules about
               --  dimensionality.

               if not Resolve_Aggr_Expr (Assoc, Single_Elmt => False) then
                  return Failure;
               end if;

            elsif not Resolve_Aggr_Expr (Expression (Assoc),
                                         Single_Elmt => False)
            then
               return Failure;

            --  Check incorrect use of dynamically tagged expression. The
            --  expression of the others choice has not been resolved yet.
            --  In order to diagnose the semantic error we create a duplicate
            --  tree to analyze it and perform the check.

            else
               declare
                  Save_Analysis : constant Boolean := Full_Analysis;
                  Expr          : constant Node_Id :=
                                    New_Copy_Tree (Expression (Assoc));

               begin
                  Expander_Mode_Save_And_Set (False);
                  Full_Analysis := False;
                  Analyze (Expr);
                  Full_Analysis := Save_Analysis;
                  Expander_Mode_Restore;

                  if Is_Tagged_Type (Etype (Expr)) then
                     Check_Dynamically_Tagged_Expression
                       (Expr => Expr,
                        Typ  => Component_Type (Etype (N)),
                        Related_Nod => N);
                  end if;
               end;
            end if;
         end if;

         --  STEP 3 (B): Compute the aggregate bounds

         if Others_Present then
            Get_Index_Bounds (Index_Constr, Aggr_Low, Aggr_High);

         else
            if Others_Allowed then
               Get_Index_Bounds (Index_Constr, Aggr_Low, Discard);
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
      --  to tree and analyze first. Reset analyzed flag to ensure it will get
      --  analyzed when it is a literal bound whose type must be properly set.

      if Others_Present or else Nb_Discrete_Choices > 0 then
         Aggr_High := Duplicate_Subexpr (Aggr_High);

         if Etype (Aggr_High) = Universal_Integer then
            Set_Analyzed (Aggr_High, False);
         end if;
      end if;

      --  If the aggregate already has bounds attached to it, it means this is
      --  a positional aggregate created as an optimization by
      --  Exp_Aggr.Convert_To_Positional, so we don't want to change those
      --  bounds.

      if Present (Aggregate_Bounds (N)) and then not Others_Allowed then
         Aggr_Low  := Low_Bound  (Aggregate_Bounds (N));
         Aggr_High := High_Bound (Aggregate_Bounds (N));
      end if;

      Set_Aggregate_Bounds
        (N, Make_Range (Loc, Low_Bound => Aggr_Low, High_Bound => Aggr_High));

      --  The bounds may contain expressions that must be inserted upwards.
      --  Attach them fully to the tree. After analysis, remove side effects
      --  from upper bound, if still needed.

      Set_Parent (Aggregate_Bounds (N), N);
      Analyze_And_Resolve (Aggregate_Bounds (N), Index_Typ);
      Check_Unset_Reference (Aggregate_Bounds (N));

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

   --  a) If the ancestor part is a type mark, the components needed are the
   --  difference between the components of the expected type and the
   --  components of the given type mark.

   --  b) If the ancestor part is an expression, it must be unambiguous, and
   --  once we have its type we can also compute the needed components as in
   --  the previous case. In both cases, if the ancestor type is not the
   --  immediate ancestor, we have to build this ancestor recursively.

   --  In both cases, discriminants of the ancestor type do not play a role in
   --  the resolution of the needed components, because inherited discriminants
   --  cannot be used in a type extension. As a result we can compute
   --  independently the list of components of the ancestor type and of the
   --  expected type.

   procedure Resolve_Extension_Aggregate (N : Node_Id; Typ : Entity_Id) is
      A      : constant Node_Id := Ancestor_Part (N);
      A_Type : Entity_Id;
      I      : Interp_Index;
      It     : Interp;

      function Valid_Limited_Ancestor (Anc : Node_Id) return Boolean;
      --  If the type is limited, verify that the ancestor part is a legal
      --  expression (aggregate or function call, including 'Input)) that does
      --  not require a copy, as specified in 7.5(2).

      function Valid_Ancestor_Type return Boolean;
      --  Verify that the type of the ancestor part is a non-private ancestor
      --  of the expected type, which must be a type extension.

      ----------------------------
      -- Valid_Limited_Ancestor --
      ----------------------------

      function Valid_Limited_Ancestor (Anc : Node_Id) return Boolean is
      begin
         if Is_Entity_Name (Anc)
           and then Is_Type (Entity (Anc))
         then
            return True;

         elsif Nkind_In (Anc, N_Aggregate, N_Function_Call) then
            return True;

         elsif Nkind (Anc) = N_Attribute_Reference
           and then Attribute_Name (Anc) = Name_Input
         then
            return True;

         elsif Nkind (Anc) = N_Qualified_Expression then
            return Valid_Limited_Ancestor (Expression (Anc));

         else
            return False;
         end if;
      end Valid_Limited_Ancestor;

      -------------------------
      -- Valid_Ancestor_Type --
      -------------------------

      function Valid_Ancestor_Type return Boolean is
         Imm_Type : Entity_Id;

      begin
         Imm_Type := Base_Type (Typ);
         while Is_Derived_Type (Imm_Type) loop
            if Etype (Imm_Type) = Base_Type (A_Type) then
               return True;

            --  The base type of the parent type may appear as  a private
            --  extension if it is declared as such in a parent unit of the
            --  current one. For consistency of the subsequent analysis use
            --  the partial view for the ancestor part.

            elsif Is_Private_Type (Etype (Imm_Type))
              and then Present (Full_View (Etype (Imm_Type)))
              and then Base_Type (A_Type) = Full_View (Etype (Imm_Type))
            then
               A_Type := Etype (Imm_Type);
               return True;

            --  The parent type may be a private extension. The aggregate is
            --  legal if the type of the aggregate is an extension of it that
            --  is not a private extension.

            elsif Is_Private_Type (A_Type)
              and then not Is_Private_Type (Imm_Type)
              and then Present (Full_View (A_Type))
              and then Base_Type (Full_View (A_Type)) = Etype (Imm_Type)
            then
               return True;

            else
               Imm_Type := Etype (Base_Type (Imm_Type));
            end if;
         end loop;

         --  If previous loop did not find a proper ancestor, report error

         Error_Msg_NE ("expect ancestor type of &", A, Typ);
         return False;
      end Valid_Ancestor_Type;

   --  Start of processing for Resolve_Extension_Aggregate

   begin
      --  Analyze the ancestor part and account for the case where it is a
      --  parameterless function call.

      Analyze (A);
      Check_Parameterless_Call (A);

      --  In SPARK, the ancestor part cannot be a type mark

      if Is_Entity_Name (A)
        and then Is_Type (Entity (A))
      then
         Check_SPARK_Restriction ("ancestor part cannot be a type mark", A);

         --  AI05-0115: if the ancestor part is a subtype mark, the ancestor
         --  must not have unknown discriminants.

         if Has_Unknown_Discriminants (Root_Type (Typ)) then
            Error_Msg_NE
              ("aggregate not available for type& whose ancestor "
                 & "has unknown discriminants", N, Typ);
         end if;
      end if;

      if not Is_Tagged_Type (Typ) then
         Error_Msg_N ("type of extension aggregate must be tagged", N);
         return;

      elsif Is_Limited_Type (Typ) then

         --  Ada 2005 (AI-287): Limited aggregates are allowed

         if Ada_Version < Ada_2005 then
            Error_Msg_N ("aggregate type cannot be limited", N);
            Explain_Limited_Type (Typ, N);
            return;

         elsif Valid_Limited_Ancestor (A) then
            null;

         else
            Error_Msg_N
              ("limited ancestor part must be aggregate or function call", A);
         end if;

      elsif Is_Class_Wide_Type (Typ) then
         Error_Msg_N ("aggregate cannot be of a class-wide type", N);
         return;
      end if;

      if Is_Entity_Name (A)
        and then Is_Type (Entity (A))
      then
         A_Type := Get_Full_View (Entity (A));

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
               --  Only consider limited interpretations in the Ada 2005 case

               if Is_Tagged_Type (It.Typ)
                 and then (Ada_Version >= Ada_2005
                            or else not Is_Limited_Type (It.Typ))
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
               if Ada_Version >= Ada_2005 then
                  Error_Msg_N ("ancestor part must be of a tagged type", A);
               else
                  Error_Msg_N
                    ("ancestor part must be of a nonlimited tagged type", A);
               end if;

               return;
            end if;

         else
            A_Type := Etype (A);
         end if;

         if Valid_Ancestor_Type then
            Resolve (A, A_Type);
            Check_Unset_Reference (A);
            Check_Non_Static_Context (A);

            --  The aggregate is illegal if the ancestor expression is a call
            --  to a function with a limited unconstrained result, unless the
            --  type of the aggregate is a null extension. This restriction
            --  was added in AI05-67 to simplify implementation.

            if Nkind (A) = N_Function_Call
              and then Is_Limited_Type (A_Type)
              and then not Is_Null_Extension (Typ)
              and then not Is_Constrained (A_Type)
            then
               Error_Msg_N
                 ("type of limited ancestor part must be constrained", A);

            --  Reject the use of CPP constructors that leave objects partially
            --  initialized. For example:

            --    type CPP_Root is tagged limited record ...
            --    pragma Import (CPP, CPP_Root);

            --    type CPP_DT is new CPP_Root and Iface ...
            --    pragma Import (CPP, CPP_DT);

            --    type Ada_DT is new CPP_DT with ...

            --    Obj : Ada_DT := Ada_DT'(New_CPP_Root with others => <>);

            --  Using the constructor of CPP_Root the slots of the dispatch
            --  table of CPP_DT cannot be set, and the secondary tag of
            --  CPP_DT is unknown.

            elsif Nkind (A) = N_Function_Call
              and then Is_CPP_Constructor_Call (A)
              and then Enclosing_CPP_Parent (Typ) /= A_Type
            then
               Error_Msg_NE
                 ("?must use 'C'P'P constructor for type &", A,
                  Enclosing_CPP_Parent (Typ));

               --  The following call is not needed if the previous warning
               --  is promoted to an error.

               Resolve_Record_Aggregate (N, Typ);

            elsif Is_Class_Wide_Type (Etype (A))
              and then Nkind (Original_Node (A)) = N_Function_Call
            then
               --  If the ancestor part is a dispatching call, it appears
               --  statically to be a legal ancestor, but it yields any member
               --  of the class, and it is not possible to determine whether
               --  it is an ancestor of the extension aggregate (much less
               --  which ancestor). It is not possible to determine the
               --  components of the extension part.

               --  This check implements AI-306, which in fact was motivated by
               --  an AdaCore query to the ARG after this test was added.

               Error_Msg_N ("ancestor part must be statically tagged", A);
            else
               Resolve_Record_Aggregate (N, Typ);
            end if;
         end if;

      else
         Error_Msg_N ("no unique type for this aggregate",  A);
      end if;
   end Resolve_Extension_Aggregate;

   ------------------------------
   -- Resolve_Record_Aggregate --
   ------------------------------

   procedure Resolve_Record_Aggregate (N : Node_Id; Typ : Entity_Id) is
      Assoc : Node_Id;
      --  N_Component_Association node belonging to the input aggregate N

      Expr            : Node_Id;
      Positional_Expr : Node_Id;
      Component       : Entity_Id;
      Component_Elmt  : Elmt_Id;

      Components : constant Elist_Id := New_Elmt_List;
      --  Components is the list of the record components whose value must be
      --  provided in the aggregate. This list does include discriminants.

      New_Assoc_List : constant List_Id := New_List;
      New_Assoc      : Node_Id;
      --  New_Assoc_List is the newly built list of N_Component_Association
      --  nodes. New_Assoc is one such N_Component_Association node in it.
      --  Note that while Assoc and New_Assoc contain the same kind of nodes,
      --  they are used to iterate over two different N_Component_Association
      --  lists.

      Others_Etype : Entity_Id := Empty;
      --  This variable is used to save the Etype of the last record component
      --  that takes its value from the others choice. Its purpose is:
      --
      --    (a) make sure the others choice is useful
      --
      --    (b) make sure the type of all the components whose value is
      --        subsumed by the others choice are the same.
      --
      --  This variable is updated as a side effect of function Get_Value.

      Is_Box_Present : Boolean := False;
      Others_Box     : Boolean := False;
      --  Ada 2005 (AI-287): Variables used in case of default initialization
      --  to provide a functionality similar to Others_Etype. Box_Present
      --  indicates that the component takes its default initialization;
      --  Others_Box indicates that at least one component takes its default
      --  initialization. Similar to Others_Etype, they are also updated as a
      --  side effect of function Get_Value.

      procedure Add_Association
        (Component      : Entity_Id;
         Expr           : Node_Id;
         Assoc_List     : List_Id;
         Is_Box_Present : Boolean := False);
      --  Builds a new N_Component_Association node which associates Component
      --  to expression Expr and adds it to the association list being built,
      --  either New_Assoc_List, or the association being built for an inner
      --  aggregate.

      function Discr_Present (Discr : Entity_Id) return Boolean;
      --  If aggregate N is a regular aggregate this routine will return True.
      --  Otherwise, if N is an extension aggregate, Discr is a discriminant
      --  whose value may already have been specified by N's ancestor part.
      --  This routine checks whether this is indeed the case and if so returns
      --  False, signaling that no value for Discr should appear in N's
      --  aggregate part. Also, in this case, the routine appends to
      --  New_Assoc_List the discriminant value specified in the ancestor part.
      --
      --  If the aggregate is in a context with expansion delayed, it will be
      --  reanalyzed. The inherited discriminant values must not be reinserted
      --  in the component list to prevent spurious errors, but they must be
      --  present on first analysis to build the proper subtype indications.
      --  The flag Inherited_Discriminant is used to prevent the re-insertion.

      function Get_Value
        (Compon                 : Node_Id;
         From                   : List_Id;
         Consider_Others_Choice : Boolean := False)
         return                   Node_Id;
      --  Given a record component stored in parameter Compon, this function
      --  returns its value as it appears in the list From, which is a list
      --  of N_Component_Association nodes.
      --
      --  If no component association has a choice for the searched component,
      --  the value provided by the others choice is returned, if there is one,
      --  and Consider_Others_Choice is set to true. Otherwise Empty is
      --  returned. If there is more than one component association giving a
      --  value for the searched record component, an error message is emitted
      --  and the first found value is returned.
      --
      --  If Consider_Others_Choice is set and the returned expression comes
      --  from the others choice, then Others_Etype is set as a side effect.
      --  An error message is emitted if the components taking their value from
      --  the others choice do not have same type.

      procedure Resolve_Aggr_Expr (Expr : Node_Id; Component : Node_Id);
      --  Analyzes and resolves expression Expr against the Etype of the
      --  Component. This routine also applies all appropriate checks to Expr.
      --  It finally saves a Expr in the newly created association list that
      --  will be attached to the final record aggregate. Note that if the
      --  Parent pointer of Expr is not set then Expr was produced with a
      --  New_Copy_Tree or some such.

      ---------------------
      -- Add_Association --
      ---------------------

      procedure Add_Association
        (Component      : Entity_Id;
         Expr           : Node_Id;
         Assoc_List     : List_Id;
         Is_Box_Present : Boolean := False)
      is
         Loc : Source_Ptr;
         Choice_List : constant List_Id := New_List;
         New_Assoc   : Node_Id;

      begin
         --  If this is a box association the expression is missing, so
         --  use the Sloc of the aggregate itself for the new association.

         if Present (Expr) then
            Loc := Sloc (Expr);
         else
            Loc := Sloc (N);
         end if;

         Append (New_Occurrence_Of (Component, Loc), Choice_List);
         New_Assoc :=
           Make_Component_Association (Loc,
             Choices     => Choice_List,
             Expression  => Expr,
             Box_Present => Is_Box_Present);
         Append (New_Assoc, Assoc_List);
      end Add_Association;

      -------------------
      -- Discr_Present --
      -------------------

      function Discr_Present (Discr : Entity_Id) return Boolean is
         Regular_Aggr : constant Boolean := Nkind (N) /= N_Extension_Aggregate;

         Loc : Source_Ptr;

         Ancestor     : Node_Id;
         Comp_Assoc   : Node_Id;
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

         --  Check whether inherited discriminant values have already been
         --  inserted in the aggregate. This will be the case if we are
         --  re-analyzing an aggregate whose expansion was delayed.

         if Present (Component_Associations (N)) then
            Comp_Assoc := First (Component_Associations (N));
            while Present (Comp_Assoc) loop
               if Inherited_Discriminant (Comp_Assoc) then
                  return True;
               end if;

               Next (Comp_Assoc);
            end loop;
         end if;

         Ancestor     := Ancestor_Part (N);
         Ancestor_Typ := Etype (Ancestor);
         Loc          := Sloc (Ancestor);

         --  For a private type with unknown discriminants, use the underlying
         --  record view if it is available.

         if Has_Unknown_Discriminants (Ancestor_Typ)
           and then Present (Full_View (Ancestor_Typ))
           and then Present (Underlying_Record_View (Full_View (Ancestor_Typ)))
         then
            Ancestor_Typ := Underlying_Record_View (Full_View (Ancestor_Typ));
         end if;

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

         --  Now look to see if Discr was specified in the ancestor part

         if Ancestor_Is_Subtyp then
            D_Val := First_Elmt (Discriminant_Constraint (Entity (Ancestor)));
         end if;

         Orig_Discr := Original_Record_Component (Discr);

         D := First_Discriminant (Ancestor_Typ);
         while Present (D) loop

            --  If Ancestor has already specified Disc value then insert its
            --  value in the final aggregate.

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
               Set_Inherited_Discriminant (Last (New_Assoc_List));
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
         Is_Box_Present := False;

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

                     --  We need to duplicate the expression for each
                     --  successive component covered by the others choice.
                     --  This is redundant if the others_choice covers only
                     --  one component (small optimization possible???), but
                     --  indispensable otherwise, because each one must be
                     --  expanded individually to preserve side-effects.

                     --  Ada 2005 (AI-287): In case of default initialization
                     --  of components, we duplicate the corresponding default
                     --  expression (from the record type declaration). The
                     --  copy must carry the sloc of the association (not the
                     --  original expression) to prevent spurious elaboration
                     --  checks when the default includes function calls.

                     if Box_Present (Assoc) then
                        Others_Box     := True;
                        Is_Box_Present := True;

                        if Expander_Active then
                           return
                             New_Copy_Tree
                               (Expression (Parent (Compon)),
                                New_Sloc => Sloc (Assoc));
                        else
                           return Expression (Parent (Compon));
                        end if;

                     else
                        if Present (Others_Etype) and then
                           Base_Type (Others_Etype) /= Base_Type (Etype
                                                                   (Compon))
                        then
                           Error_Msg_N ("components in OTHERS choice must " &
                                        "have same type", Selector_Name);
                        end if;

                        Others_Etype := Etype (Compon);

                        if Expander_Active then
                           return New_Copy_Tree (Expression (Assoc));
                        else
                           return Expression (Assoc);
                        end if;
                     end if;
                  end if;

               elsif Chars (Compon) = Chars (Selector_Name) then
                  if No (Expr) then

                     --  Ada 2005 (AI-231)

                     if Ada_Version >= Ada_2005
                       and then Known_Null (Expression (Assoc))
                     then
                        Check_Can_Never_Be_Null (Compon, Expression (Assoc));
                     end if;

                     --  We need to duplicate the expression when several
                     --  components are grouped together with a "|" choice.
                     --  For instance "filed1 | filed2 => Expr"

                     --  Ada 2005 (AI-287)

                     if Box_Present (Assoc) then
                        Is_Box_Present := True;

                        --  Duplicate the default expression of the component
                        --  from the record type declaration, so a new copy
                        --  can be attached to the association.

                        --  Note that we always copy the default expression,
                        --  even when the association has a single choice, in
                        --  order to create a proper association for the
                        --  expanded aggregate.

                        Expr := New_Copy_Tree (Expression (Parent (Compon)));

                        --  Component may have no default, in which case the
                        --  expression is empty and the component is default-
                        --  initialized, but an association for the component
                        --  exists, and it is not covered by an others clause.

                        return Expr;

                     else
                        if Present (Next (Selector_Name)) then
                           Expr := New_Copy_Tree (Expression (Assoc));
                        else
                           Expr := Expression (Assoc);
                        end if;
                     end if;

                     Generate_Reference (Compon, Selector_Name, 'm');

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

         Relocate : Boolean;
         --  Set to True if the resolved Expr node needs to be relocated when
         --  attached to the newly created association list. This node need not
         --  be relocated if its parent pointer is not set. In fact in this
         --  case Expr is the output of a New_Copy_Tree call. If Relocate is
         --  True then we have analyzed the expression node in the original
         --  aggregate and hence it needs to be relocated when moved over to
         --  the new association list.

         ---------------------------
         -- Has_Expansion_Delayed --
         ---------------------------

         function Has_Expansion_Delayed (Expr : Node_Id) return Boolean is
            Kind : constant Node_Kind := Nkind (Expr);
         begin
            return (Nkind_In (Kind, N_Aggregate, N_Extension_Aggregate)
                     and then Present (Etype (Expr))
                     and then Is_Record_Type (Etype (Expr))
                     and then Expansion_Delayed (Expr))
              or else (Kind = N_Qualified_Expression
                        and then Has_Expansion_Delayed (Expression (Expr)));
         end Has_Expansion_Delayed;

      --  Start of processing for Resolve_Aggr_Expr

      begin
         --  If the type of the component is elementary or the type of the
         --  aggregate does not contain discriminants, use the type of the
         --  component to resolve Expr.

         if Is_Elementary_Type (Etype (Component))
           or else not Has_Discriminants (Etype (N))
         then
            Expr_Type := Etype (Component);

         --  Otherwise we have to pick up the new type of the component from
         --  the new constrained subtype of the aggregate. In fact components
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
            --  the range of the base type. This checks is done in the init
            --  proc for regular objects, but has to be done here for
            --  aggregates since no init proc is called for them.

            if Is_Array_Type (Expr_Type) then
               declare
                  Index : Node_Id;
                  --  Range of the current constrained index in the array

                  Orig_Index : Node_Id := First_Index (Etype (Component));
                  --  Range corresponding to the range Index above in the
                  --  original unconstrained record type. The bounds of this
                  --  range may be governed by discriminants.

                  Unconstr_Index : Node_Id := First_Index (Etype (Expr_Type));
                  --  Range corresponding to the range Index above for the
                  --  unconstrained array type. This range is needed to apply
                  --  range checks.

               begin
                  Index := First_Index (Expr_Type);
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
         Check_Expr_OK_In_Limited_Aggregate (Expr);
         Check_Non_Static_Context (Expr);
         Check_Unset_Reference (Expr);

         --  Check wrong use of class-wide types

         if Is_Class_Wide_Type (Etype (Expr)) then
            Error_Msg_N ("dynamically tagged expression not allowed", Expr);
         end if;

         if not Has_Expansion_Delayed (Expr) then
            Aggregate_Constraint_Checks (Expr, Expr_Type);
         end if;

         --  If an aggregate component has a type with predicates, an explicit
         --  predicate check must be applied, as for an assignment statement,
         --  because the aggegate might not be expanded into individual
         --  component assignments.

         if Present (Predicate_Function (Expr_Type)) then
            Apply_Predicate_Check (Expr, Expr_Type);
         end if;

         if Raises_Constraint_Error (Expr) then
            Set_Raises_Constraint_Error (N);
         end if;

         --  If the expression has been marked as requiring a range check, then
         --  generate it here.

         if Do_Range_Check (Expr) then
            Set_Do_Range_Check (Expr, False);
            Generate_Range_Check (Expr, Expr_Type, CE_Range_Check_Failed);
         end if;

         if Relocate then
            Add_Association (New_C, Relocate_Node (Expr), New_Assoc_List);
         else
            Add_Association (New_C, Expr, New_Assoc_List);
         end if;
      end Resolve_Aggr_Expr;

   --  Start of processing for Resolve_Record_Aggregate

   begin
      --  A record aggregate is restricted in SPARK:
      --    Each named association can have only a single choice.
      --    OTHERS cannot be used.
      --    Positional and named associations cannot be mixed.

      if Present (Component_Associations (N))
        and then Present (First (Component_Associations (N)))
      then

         if Present (Expressions (N)) then
            Check_SPARK_Restriction
              ("named association cannot follow positional one",
               First (Choices (First (Component_Associations (N)))));
         end if;

         declare
            Assoc : Node_Id;

         begin
            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop
               if List_Length (Choices (Assoc)) > 1 then
                  Check_SPARK_Restriction
                    ("component association in record aggregate must "
                     & "contain a single choice", Assoc);
               end if;

               if Nkind (First (Choices (Assoc))) = N_Others_Choice then
                  Check_SPARK_Restriction
                    ("record aggregate cannot contain OTHERS", Assoc);
               end if;

               Assoc := Next (Assoc);
            end loop;
         end;
      end if;

      --  We may end up calling Duplicate_Subexpr on expressions that are
      --  attached to New_Assoc_List. For this reason we need to attach it
      --  to the tree by setting its parent pointer to N. This parent point
      --  will change in STEP 8 below.

      Set_Parent (New_Assoc_List, N);

      --  STEP 1: abstract type and null record verification

      if Is_Abstract_Type (Typ) then
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

      --  If the type has no components, then the aggregate should either
      --  have "null record", or in Ada 2005 it could instead have a single
      --  component association given by "others => <>". For Ada 95 we flag an
      --  error at this point, but for Ada 2005 we proceed with checking the
      --  associations below, which will catch the case where it's not an
      --  aggregate with "others => <>". Note that the legality of a <>
      --  aggregate for a null record type was established by AI05-016.

      elsif No (First_Entity (Typ))
         and then Ada_Version < Ada_2005
      then
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
                     Error_Msg_N
                       ("OTHERS must appear alone in a choice list",
                        Selector_Name);
                     return;

                  elsif Present (Next (Assoc)) then
                     Error_Msg_N
                       ("OTHERS must appear last in an aggregate",
                        Selector_Name);
                     return;

                  --  (Ada 2005): If this is an association with a box,
                  --  indicate that the association need not represent
                  --  any component.

                  elsif Box_Present (Assoc) then
                     Others_Box := True;
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

         --  AI05-0115: if the ancestor part is a subtype mark, the ancestor
         --  must npt have unknown discriminants.

         if Is_Derived_Type (Typ)
           and then Has_Unknown_Discriminants (Root_Type (Typ))
           and then Nkind (N) /= N_Extension_Aggregate
         then
            Error_Msg_NE
              ("aggregate not available for type& whose ancestor "
                 & "has unknown discriminants ", N, Typ);
         end if;

         if Has_Unknown_Discriminants (Typ)
           and then Present (Underlying_Record_View (Typ))
         then
            Discrim := First_Discriminant (Underlying_Record_View (Typ));
         elsif Has_Discriminants (Typ) then
            Discrim := First_Discriminant (Typ);
         else
            Discrim := Empty;
         end if;

         --  First find the discriminant values in the positional components

         while Present (Discrim) and then Present (Positional_Expr) loop
            if Discr_Present (Discrim) then
               Resolve_Aggr_Expr (Positional_Expr, Discrim);

               --  Ada 2005 (AI-231)

               if Ada_Version >= Ada_2005
                 and then Known_Null (Positional_Expr)
               then
                  Check_Can_Never_Be_Null (Discrim, Positional_Expr);
               end if;

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

      --  ??? Performance WARNING. The current implementation creates a new
      --  itype for all aggregates whose base type is discriminated.
      --  This means that for record aggregates nested inside an array
      --  aggregate we will create a new itype for each record aggregate
      --  if the array component type has discriminants. For large aggregates
      --  this may be a problem. What should be done in this case is
      --  to reuse itypes as much as possible.

      if Has_Discriminants (Typ)
        or else (Has_Unknown_Discriminants (Typ)
                   and then Present (Underlying_Record_View (Typ)))
      then
         Build_Constrained_Itype : declare
            Loc         : constant Source_Ptr := Sloc (N);
            Indic       : Node_Id;
            Subtyp_Decl : Node_Id;
            Def_Id      : Entity_Id;

            C : constant List_Id := New_List;

         begin
            New_Assoc := First (New_Assoc_List);
            while Present (New_Assoc) loop
               Append (Duplicate_Subexpr (Expression (New_Assoc)), To => C);
               Next (New_Assoc);
            end loop;

            if Has_Unknown_Discriminants (Typ)
              and then Present (Underlying_Record_View (Typ))
            then
               Indic :=
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (Underlying_Record_View (Typ), Loc),
                   Constraint  =>
                     Make_Index_Or_Discriminant_Constraint (Loc, C));
            else
               Indic :=
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (Base_Type (Typ), Loc),
                   Constraint  =>
                     Make_Index_Or_Discriminant_Constraint (Loc, C));
            end if;

            Def_Id := Create_Itype (Ekind (Typ), N);

            Subtyp_Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Indication  => Indic);
            Set_Parent (Subtyp_Decl, Parent (N));

            --  Itypes must be analyzed with checks off (see itypes.ads)

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

         function Find_Private_Ancestor return Entity_Id;
         --  AI05-0115: Find earlier ancestor in the derivation chain that is
         --  derived from a private view. Whether the aggregate is legal
         --  depends on the current visibility of the type as well as that
         --  of the parent of the ancestor.

         ---------------------------
         -- Find_Private_Ancestor --
         ---------------------------

         function Find_Private_Ancestor return Entity_Id is
            Par : Entity_Id;
         begin
            Par := Typ;
            loop
               if Has_Private_Ancestor (Par)
                 and then not Has_Private_Ancestor (Etype (Base_Type (Par)))
               then
                  return Par;

               elsif not Is_Derived_Type (Par) then
                  return Empty;

               else
                  Par := Etype (Base_Type (Par));
               end if;
            end loop;
         end Find_Private_Ancestor;

      begin
         if Is_Derived_Type (Typ) and then Is_Tagged_Type (Typ) then
            Parent_Typ_List := New_Elmt_List;

            --  If this is an extension aggregate, the component list must
            --  include all components that are not in the given ancestor type.
            --  Otherwise, the component list must include components of all
            --  ancestors, starting with the root.

            if Nkind (N) = N_Extension_Aggregate then
               Root_Typ := Base_Type (Etype (Ancestor_Part (N)));

            else
               --  AI05-0115:  check legality of aggregate for type with
               --  aa private ancestor.

               Root_Typ := Root_Type (Typ);
               if Has_Private_Ancestor (Typ) then
                  declare
                     Ancestor      : constant Entity_Id :=
                       Find_Private_Ancestor;
                     Ancestor_Unit : constant Entity_Id :=
                       Cunit_Entity (Get_Source_Unit (Ancestor));
                     Parent_Unit   : constant Entity_Id :=
                       Cunit_Entity
                         (Get_Source_Unit (Base_Type (Etype (Ancestor))));
                  begin

                     --  check whether we are in a scope that has full view
                     --  over the private ancestor and its parent. This can
                     --  only happen if the derivation takes place in a child
                     --  unit of the unit that declares the parent, and we are
                     --  in the private part or body of that child unit, else
                     --  the aggregate is illegal.

                     if Is_Child_Unit (Ancestor_Unit)
                       and then Scope (Ancestor_Unit) = Parent_Unit
                       and then In_Open_Scopes (Scope (Ancestor))
                       and then
                        (In_Private_Part (Scope (Ancestor))
                           or else In_Package_Body (Scope (Ancestor)))
                     then
                        null;

                     else
                        Error_Msg_NE
                          ("type of aggregate has private ancestor&!",
                              N, Root_Typ);
                        Error_Msg_N ("must use extension aggregate!", N);
                        return;
                     end if;
                  end;
               end if;

               Dnode := Declaration_Node (Base_Type (Root_Typ));

               --  If we don't get a full declaration, then we have some error
               --  which will get signalled later so skip this part. Otherwise
               --  gather components of root that apply to the aggregate type.
               --  We use the base type in case there is an applicable stored
               --  constraint that renames the discriminants of the root.

               if Nkind (Dnode) = N_Full_Type_Declaration then
                  Record_Def := Type_Definition (Dnode);
                  Gather_Components (Base_Type (Typ),
                    Component_List (Record_Def),
                    Governed_By   => New_Assoc_List,
                    Into          => Components,
                    Report_Errors => Errors_Found);
               end if;
            end if;

            Parent_Typ := Base_Type (Typ);
            while Parent_Typ /= Root_Typ loop
               Prepend_Elmt (Parent_Typ, To => Parent_Typ_List);
               Parent_Typ := Etype (Parent_Typ);

               if Nkind (Parent (Base_Type (Parent_Typ))) =
                                        N_Private_Type_Declaration
                 or else Nkind (Parent (Base_Type (Parent_Typ))) =
                                        N_Private_Extension_Declaration
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

               --  The current view of ancestor part may be a private type,
               --  while the context type is always non-private.

               elsif Is_Private_Type (Root_Typ)
                 and then Present (Full_View (Root_Typ))
                 and then Nkind (N) = N_Extension_Aggregate
               then
                  exit when Base_Type (Full_View (Root_Typ)) = Parent_Typ;
               end if;
            end loop;

            --  Now collect components from all other ancestors, beginning
            --  with the current type. If the type has unknown discriminants
            --  use the component list of the Underlying_Record_View, which
            --  needs to be used for the subsequent expansion of the aggregate
            --  into assignments.

            Parent_Elmt := First_Elmt (Parent_Typ_List);
            while Present (Parent_Elmt) loop
               Parent_Typ := Node (Parent_Elmt);

               if Has_Unknown_Discriminants (Parent_Typ)
                 and then Present (Underlying_Record_View (Typ))
               then
                  Parent_Typ := Underlying_Record_View (Parent_Typ);
               end if;

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

            elsif not Has_Unknown_Discriminants (Typ) then
               Gather_Components (Base_Type (Typ),
                 Component_List (Record_Def),
                 Governed_By   => New_Assoc_List,
                 Into          => Components,
                 Report_Errors => Errors_Found);

            else
               Gather_Components
                 (Base_Type (Underlying_Record_View (Typ)),
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

         --  Ada 2005 (AI-231)

         if Ada_Version >= Ada_2005
           and then Known_Null (Positional_Expr)
         then
            Check_Can_Never_Be_Null (Component, Positional_Expr);
         end if;

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

         --  Note: The previous call to Get_Value sets the value of the
         --  variable Is_Box_Present.

         --  Ada 2005 (AI-287): Handle components with default initialization.
         --  Note: This feature was originally added to Ada 2005 for limited
         --  but it was finally allowed with any type.

         if Is_Box_Present then
            Check_Box_Component : declare
               Ctyp : constant Entity_Id := Etype (Component);

            begin
               --  If there is a default expression for the aggregate, copy
               --  it into a new association.

               --  If the component has an initialization procedure (IP) we
               --  pass the component to the expander, which will generate
               --  the call to such IP.

               --  If the component has discriminants, their values must
               --  be taken from their subtype. This is indispensable for
               --  constraints that are given by the current instance of an
               --  enclosing type, to allow the expansion of the aggregate
               --  to replace the reference to the current instance by the
               --  target object of the aggregate.

               if Present (Parent (Component))
                 and then
                   Nkind (Parent (Component)) = N_Component_Declaration
                 and then Present (Expression (Parent (Component)))
               then
                  Expr :=
                    New_Copy_Tree (Expression (Parent (Component)),
                      New_Sloc => Sloc (N));

                  Add_Association
                    (Component  => Component,
                     Expr       => Expr,
                     Assoc_List => New_Assoc_List);
                  Set_Has_Self_Reference (N);

               --  A box-defaulted access component gets the value null. Also
               --  included are components of private types whose underlying
               --  type is an access type. In either case set the type of the
               --  literal, for subsequent use in semantic checks.

               elsif Present (Underlying_Type (Ctyp))
                 and then Is_Access_Type (Underlying_Type (Ctyp))
               then
                  if not Is_Private_Type (Ctyp) then
                     Expr := Make_Null (Sloc (N));
                     Set_Etype (Expr, Ctyp);
                     Add_Association
                       (Component  => Component,
                        Expr       => Expr,
                        Assoc_List => New_Assoc_List);

                  --  If the component's type is private with an access type as
                  --  its underlying type then we have to create an unchecked
                  --  conversion to satisfy type checking.

                  else
                     declare
                        Qual_Null : constant Node_Id :=
                                      Make_Qualified_Expression (Sloc (N),
                                        Subtype_Mark =>
                                          New_Occurrence_Of
                                            (Underlying_Type (Ctyp), Sloc (N)),
                                        Expression => Make_Null (Sloc (N)));

                        Convert_Null : constant Node_Id :=
                                         Unchecked_Convert_To
                                           (Ctyp, Qual_Null);

                     begin
                        Analyze_And_Resolve (Convert_Null, Ctyp);
                        Add_Association
                          (Component  => Component,
                           Expr       => Convert_Null,
                           Assoc_List => New_Assoc_List);
                     end;
                  end if;

               elsif Has_Non_Null_Base_Init_Proc (Ctyp)
                 or else not Expander_Active
               then
                  if Is_Record_Type (Ctyp)
                    and then Has_Discriminants (Ctyp)
                    and then not Is_Private_Type (Ctyp)
                  then
                     --  We build a partially initialized aggregate with the
                     --  values of the discriminants and box initialization
                     --  for the rest, if other components are present.
                     --  The type of the aggregate is the known subtype of
                     --  the component. The capture of discriminants must
                     --  be recursive because subcomponents may be constrained
                     --  (transitively) by discriminants of enclosing types.
                     --  For a private type with discriminants, a call to the
                     --  initialization procedure will be generated, and no
                     --  subaggregate is needed.

                     Capture_Discriminants : declare
                        Loc  : constant Source_Ptr := Sloc (N);
                        Expr : Node_Id;

                        procedure Add_Discriminant_Values
                          (New_Aggr   : Node_Id;
                           Assoc_List : List_Id);
                        --  The constraint to a component may be given by a
                        --  discriminant of the enclosing type, in which case
                        --  we have to retrieve its value, which is part of the
                        --  enclosing aggregate. Assoc_List provides the
                        --  discriminant associations of the current type or
                        --  of some enclosing record.

                        procedure Propagate_Discriminants
                          (Aggr       : Node_Id;
                           Assoc_List : List_Id);
                        --  Nested components may themselves be discriminated
                        --  types constrained by outer discriminants, whose
                        --  values must be captured before the aggregate is
                        --  expanded into assignments.

                        -----------------------------
                        -- Add_Discriminant_Values --
                        -----------------------------

                        procedure Add_Discriminant_Values
                          (New_Aggr   : Node_Id;
                           Assoc_List : List_Id)
                        is
                           Assoc      : Node_Id;
                           Discr      : Entity_Id;
                           Discr_Elmt : Elmt_Id;
                           Discr_Val  : Node_Id;
                           Val        : Entity_Id;

                        begin
                           Discr := First_Discriminant (Etype (New_Aggr));
                           Discr_Elmt :=
                             First_Elmt
                               (Discriminant_Constraint (Etype (New_Aggr)));
                           while Present (Discr_Elmt) loop
                              Discr_Val := Node (Discr_Elmt);

                              --  If the constraint is given by a discriminant
                              --  it is a discriminant of an enclosing record,
                              --  and its value has already been placed in the
                              --  association list.

                              if Is_Entity_Name (Discr_Val)
                                and then
                                  Ekind (Entity (Discr_Val)) = E_Discriminant
                              then
                                 Val := Entity (Discr_Val);

                                 Assoc := First (Assoc_List);
                                 while Present (Assoc) loop
                                    if Present
                                      (Entity (First (Choices (Assoc))))
                                      and then
                                        Entity (First (Choices (Assoc)))
                                          = Val
                                    then
                                       Discr_Val := Expression (Assoc);
                                       exit;
                                    end if;
                                    Next (Assoc);
                                 end loop;
                              end if;

                              Add_Association
                                (Discr, New_Copy_Tree (Discr_Val),
                                  Component_Associations (New_Aggr));

                              --  If the discriminant constraint is a current
                              --  instance, mark the current aggregate so that
                              --  the self-reference can be expanded later.

                              if Nkind (Discr_Val) = N_Attribute_Reference
                                and then Is_Entity_Name (Prefix (Discr_Val))
                                and then Is_Type (Entity (Prefix (Discr_Val)))
                                and then Etype (N) =
                                  Entity (Prefix (Discr_Val))
                              then
                                 Set_Has_Self_Reference (N);
                              end if;

                              Next_Elmt (Discr_Elmt);
                              Next_Discriminant (Discr);
                           end loop;
                        end Add_Discriminant_Values;

                        ------------------------------
                        --  Propagate_Discriminants --
                        ------------------------------

                        procedure Propagate_Discriminants
                          (Aggr       : Node_Id;
                           Assoc_List : List_Id)
                        is
                           Aggr_Type : constant Entity_Id :=
                                         Base_Type (Etype (Aggr));
                           Def_Node  : constant Node_Id :=
                                         Type_Definition
                                           (Declaration_Node (Aggr_Type));

                           Comp       : Node_Id;
                           Comp_Elmt  : Elmt_Id;
                           Components : constant Elist_Id := New_Elmt_List;
                           Needs_Box  : Boolean := False;
                           Errors     : Boolean;

                           procedure Process_Component (Comp : Entity_Id);
                           --  Add one component with a box association to the
                           --  inner aggregate, and recurse if component is
                           --  itself composite.

                           ------------------------
                           --  Process_Component --
                           ------------------------

                           procedure Process_Component (Comp : Entity_Id) is
                              T : constant Entity_Id := Etype (Comp);
                              New_Aggr   : Node_Id;

                           begin
                              if Is_Record_Type (T)
                                and then Has_Discriminants (T)
                              then
                                 New_Aggr :=
                                   Make_Aggregate (Loc, New_List, New_List);
                                 Set_Etype (New_Aggr, T);
                                 Add_Association
                                   (Comp, New_Aggr,
                                     Component_Associations (Aggr));

                                 --  Collect discriminant values and recurse

                                 Add_Discriminant_Values
                                   (New_Aggr, Assoc_List);
                                 Propagate_Discriminants
                                   (New_Aggr, Assoc_List);

                              else
                                 Needs_Box := True;
                              end if;
                           end Process_Component;

                        --  Start of processing for Propagate_Discriminants

                        begin
                           --  The component type may be a variant type, so
                           --  collect the components that are ruled by the
                           --  known values of the discriminants. Their values
                           --  have already been inserted into the component
                           --  list of the current aggregate.

                           if Nkind (Def_Node) =  N_Record_Definition
                             and then
                               Present (Component_List (Def_Node))
                             and then
                               Present
                                 (Variant_Part (Component_List (Def_Node)))
                           then
                              Gather_Components (Aggr_Type,
                                Component_List (Def_Node),
                                Governed_By   => Component_Associations (Aggr),
                                Into          => Components,
                                Report_Errors => Errors);

                              Comp_Elmt := First_Elmt (Components);
                              while Present (Comp_Elmt) loop
                                 if
                                   Ekind (Node (Comp_Elmt)) /= E_Discriminant
                                 then
                                    Process_Component (Node (Comp_Elmt));
                                 end if;

                                 Next_Elmt (Comp_Elmt);
                              end loop;

                           --  No variant part, iterate over all components

                           else
                              Comp := First_Component (Etype (Aggr));
                              while Present (Comp) loop
                                 Process_Component (Comp);
                                 Next_Component (Comp);
                              end loop;
                           end if;

                           if Needs_Box then
                              Append
                                (Make_Component_Association (Loc,
                                   Choices     =>
                                     New_List (Make_Others_Choice (Loc)),
                                   Expression  => Empty,
                                      Box_Present => True),
                                 Component_Associations (Aggr));
                           end if;
                        end Propagate_Discriminants;

                     --  Start of processing for Capture_Discriminants

                     begin
                        Expr := Make_Aggregate (Loc, New_List, New_List);
                        Set_Etype (Expr, Ctyp);

                        --  If the enclosing type has discriminants, they have
                        --  been collected in the aggregate earlier, and they
                        --  may appear as constraints of subcomponents.

                        --  Similarly if this component has discriminants, they
                        --  might in turn be propagated to their components.

                        if Has_Discriminants (Typ) then
                           Add_Discriminant_Values (Expr, New_Assoc_List);
                           Propagate_Discriminants (Expr, New_Assoc_List);

                        elsif Has_Discriminants (Ctyp) then
                           Add_Discriminant_Values
                              (Expr, Component_Associations (Expr));
                           Propagate_Discriminants
                              (Expr, Component_Associations (Expr));

                        else
                           declare
                              Comp : Entity_Id;

                           begin
                              --  If the type has additional components, create
                              --  an OTHERS box association for them.

                              Comp := First_Component (Ctyp);
                              while Present (Comp) loop
                                 if Ekind (Comp) = E_Component then
                                    if not Is_Record_Type (Etype (Comp)) then
                                       Append
                                         (Make_Component_Association (Loc,
                                            Choices     =>
                                              New_List
                                               (Make_Others_Choice (Loc)),
                                            Expression  => Empty,
                                               Box_Present => True),
                                          Component_Associations (Expr));
                                    end if;
                                    exit;
                                 end if;

                                 Next_Component (Comp);
                              end loop;
                           end;
                        end if;

                        Add_Association
                          (Component  => Component,
                           Expr       => Expr,
                           Assoc_List => New_Assoc_List);
                     end Capture_Discriminants;

                  else
                     Add_Association
                       (Component      => Component,
                        Expr           => Empty,
                        Assoc_List     => New_Assoc_List,
                        Is_Box_Present => True);
                  end if;

               --  Otherwise we only need to resolve the expression if the
               --  component has partially initialized values (required to
               --  expand the corresponding assignments and run-time checks).

               elsif Present (Expr)
                 and then Is_Partially_Initialized_Type (Ctyp)
               then
                  Resolve_Aggr_Expr (Expr, Component);
               end if;
            end Check_Box_Component;

         elsif No (Expr) then

            --  Ignore hidden components associated with the position of the
            --  interface tags: these are initialized dynamically.

            if not Present (Related_Type (Component)) then
               Error_Msg_NE
                 ("no value supplied for component &!", N, Component);
            end if;

         else
            Resolve_Aggr_Expr (Expr, Component);
         end if;

         Next_Elmt (Component_Elmt);
      end loop;

      --  STEP 7: check for invalid components + check type in choice list

      Step_7 : declare
         Selectr : Node_Id;
         --  Selector name

         Typech : Entity_Id;
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

               --  Ada 2005 (AI-287): others choice may have expression or box

               if No (Others_Etype)
                  and then not Others_Box
               then
                  Error_Msg_N
                    ("OTHERS must represent at least one component", Selectr);
               end if;

               exit Verification;
            end if;

            while Present (Selectr) loop
               New_Assoc := First (New_Assoc_List);
               while Present (New_Assoc) loop
                  Component := First (Choices (New_Assoc));

                  if Chars (Selectr) = Chars (Component) then
                     if Style_Check then
                        Check_Identifier (Selectr, Entity (Component));
                     end if;

                     exit;
                  end if;

                  Next (New_Assoc);
               end loop;

               --  If no association, this is not a legal component of
               --  of the type in question, except if its association
               --  is provided with a box.

               if No (New_Assoc) then
                  if Box_Present (Parent (Selectr)) then

                     --  This may still be a bogus component with a box. Scan
                     --  list of components to verify that a component with
                     --  that name exists.

                     declare
                        C : Entity_Id;

                     begin
                        C := First_Component (Typ);
                        while Present (C) loop
                           if Chars (C) = Chars (Selectr) then

                              --  If the context is an extension aggregate,
                              --  the component must not be inherited from
                              --  the ancestor part of the aggregate.

                              if Nkind (N) /= N_Extension_Aggregate
                                or else
                                  Scope (Original_Record_Component (C)) /=
                                                     Etype (Ancestor_Part (N))
                              then
                                 exit;
                              end if;
                           end if;

                           Next_Component (C);
                        end loop;

                        if No (C) then
                           Error_Msg_Node_2 := Typ;
                           Error_Msg_N ("& is not a component of}", Selectr);
                        end if;
                     end;

                  elsif Chars (Selectr) /= Name_uTag
                    and then Chars (Selectr) /= Name_uParent
                  then
                     if not Has_Discriminants (Typ) then
                        Error_Msg_Node_2 := Typ;
                        Error_Msg_N ("& is not a component of}", Selectr);
                     else
                        Error_Msg_N
                          ("& is not a component of the aggregate subtype",
                            Selectr);
                     end if;

                     Check_Misspelled_Component (Components, Selectr);
                  end if;

               elsif No (Typech) then
                  Typech := Base_Type (Etype (Component));

               --  AI05-0199: In Ada 2012, several components of anonymous
               --  access types can appear in a choice list, as long as the
               --  designated types match.

               elsif Typech /= Base_Type (Etype (Component)) then
                  if Ada_Version >= Ada_2012
                    and then Ekind (Typech) = E_Anonymous_Access_Type
                    and then
                       Ekind (Etype (Component)) = E_Anonymous_Access_Type
                    and then Base_Type (Designated_Type (Typech)) =
                             Base_Type (Designated_Type (Etype (Component)))
                    and then
                      Subtypes_Statically_Match (Typech, (Etype (Component)))
                  then
                     null;

                  elsif not Box_Present (Parent (Selectr)) then
                     Error_Msg_N
                       ("components in choice list must have same type",
                        Selectr);
                  end if;
               end if;

               Next (Selectr);
            end loop;

            Next (Assoc);
         end loop Verification;
      end Step_7;

      --  STEP 8: replace the original aggregate

      Step_8 : declare
         New_Aggregate : constant Node_Id := New_Copy (N);

      begin
         Set_Expressions            (New_Aggregate, No_List);
         Set_Etype                  (New_Aggregate, Etype (N));
         Set_Component_Associations (New_Aggregate, New_Assoc_List);

         Rewrite (N, New_Aggregate);
      end Step_8;
   end Resolve_Record_Aggregate;

   -----------------------------
   -- Check_Can_Never_Be_Null --
   -----------------------------

   procedure Check_Can_Never_Be_Null (Typ : Entity_Id; Expr : Node_Id) is
      Comp_Typ : Entity_Id;

   begin
      pragma Assert
        (Ada_Version >= Ada_2005
          and then Present (Expr)
          and then Known_Null (Expr));

      case Ekind (Typ) is
         when E_Array_Type  =>
            Comp_Typ := Component_Type (Typ);

         when E_Component    |
              E_Discriminant =>
            Comp_Typ := Etype (Typ);

         when others =>
            return;
      end case;

      if Can_Never_Be_Null (Comp_Typ) then

         --  Here we know we have a constraint error. Note that we do not use
         --  Apply_Compile_Time_Constraint_Error here to the Expr, which might
         --  seem the more natural approach. That's because in some cases the
         --  components are rewritten, and the replacement would be missed.

         Insert_Action
           (Compile_Time_Constraint_Error
              (Expr,
               "(Ada 2005) null not allowed in null-excluding component?"),
            Make_Raise_Constraint_Error (Sloc (Expr),
              Reason => CE_Access_Check_Failed));

         --  Set proper type for bogus component (why is this needed???)

         Set_Etype    (Expr, Comp_Typ);
         Set_Analyzed (Expr);
      end if;
   end Check_Can_Never_Be_Null;

   ---------------------
   -- Sort_Case_Table --
   ---------------------

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type) is
      L : constant Int := Case_Table'First;
      U : constant Int := Case_Table'Last;
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
