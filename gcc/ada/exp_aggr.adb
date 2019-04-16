------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A G G R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Util; use Exp_Util;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Disp; use Exp_Disp;
with Exp_Tss;  use Exp_Tss;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Ttypes;   use Ttypes;
with Sem;      use Sem;
with Sem_Aggr; use Sem_Aggr;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Aggr is

   type Case_Bounds is record
     Choice_Lo   : Node_Id;
     Choice_Hi   : Node_Id;
     Choice_Node : Node_Id;
   end record;

   type Case_Table_Type is array (Nat range <>) of Case_Bounds;
   --  Table type used by Check_Case_Choices procedure

   procedure Collect_Initialization_Statements
     (Obj        : Entity_Id;
      N          : Node_Id;
      Node_After : Node_Id);
   --  If Obj is not frozen, collect actions inserted after N until, but not
   --  including, Node_After, for initialization of Obj, and move them to an
   --  expression with actions, which becomes the Initialization_Statements for
   --  Obj.

   procedure Expand_Delta_Array_Aggregate  (N : Node_Id; Deltas : List_Id);
   procedure Expand_Delta_Record_Aggregate (N : Node_Id; Deltas : List_Id);

   function Has_Default_Init_Comps (N : Node_Id) return Boolean;
   --  N is an aggregate (record or array). Checks the presence of default
   --  initialization (<>) in any component (Ada 2005: AI-287).

   function Is_CCG_Supported_Aggregate (N : Node_Id) return Boolean;
   --  Return True if aggregate N is located in a context supported by the
   --  CCG backend; False otherwise.

   function Is_Static_Dispatch_Table_Aggregate (N : Node_Id) return Boolean;
   --  Returns true if N is an aggregate used to initialize the components
   --  of a statically allocated dispatch table.

   function Late_Expansion
     (N      : Node_Id;
      Typ    : Entity_Id;
      Target : Node_Id) return List_Id;
   --  This routine implements top-down expansion of nested aggregates. In
   --  doing so, it avoids the generation of temporaries at each level. N is
   --  a nested record or array aggregate with the Expansion_Delayed flag.
   --  Typ is the expected type of the aggregate. Target is a (duplicatable)
   --  expression that will hold the result of the aggregate expansion.

   function Make_OK_Assignment_Statement
     (Sloc       : Source_Ptr;
      Name       : Node_Id;
      Expression : Node_Id) return Node_Id;
   --  This is like Make_Assignment_Statement, except that Assignment_OK
   --  is set in the left operand. All assignments built by this unit use
   --  this routine. This is needed to deal with assignments to initialized
   --  constants that are done in place.

   function Must_Slide
     (Obj_Type : Entity_Id;
      Typ      : Entity_Id) return Boolean;
   --  A static array aggregate in an object declaration can in most cases be
   --  expanded in place. The one exception is when the aggregate is given
   --  with component associations that specify different bounds from those of
   --  the type definition in the object declaration. In this pathological
   --  case the aggregate must slide, and we must introduce an intermediate
   --  temporary to hold it.
   --
   --  The same holds in an assignment to one-dimensional array of arrays,
   --  when a component may be given with bounds that differ from those of the
   --  component type.

   function Number_Of_Choices (N : Node_Id) return Nat;
   --  Returns the number of discrete choices (not including the others choice
   --  if present) contained in (sub-)aggregate N.

   procedure Process_Transient_Component
     (Loc        : Source_Ptr;
      Comp_Typ   : Entity_Id;
      Init_Expr  : Node_Id;
      Fin_Call   : out Node_Id;
      Hook_Clear : out Node_Id;
      Aggr       : Node_Id := Empty;
      Stmts      : List_Id := No_List);
   --  Subsidiary to the expansion of array and record aggregates. Generate
   --  part of the necessary code to finalize a transient component. Comp_Typ
   --  is the component type. Init_Expr is the initialization expression of the
   --  component which is always a function call. Fin_Call is the finalization
   --  call used to clean up the transient function result. Hook_Clear is the
   --  hook reset statement. Aggr and Stmts both control the placement of the
   --  generated code. Aggr is the related aggregate. If present, all code is
   --  inserted prior to Aggr using Insert_Action. Stmts is the initialization
   --  statements of the component. If present, all code is added to Stmts.

   procedure Process_Transient_Component_Completion
     (Loc        : Source_Ptr;
      Aggr       : Node_Id;
      Fin_Call   : Node_Id;
      Hook_Clear : Node_Id;
      Stmts      : List_Id);
   --  Subsidiary to the expansion of array and record aggregates. Generate
   --  part of the necessary code to finalize a transient component. Aggr is
   --  the related aggregate. Fin_Clear is the finalization call used to clean
   --  up the transient component. Hook_Clear is the hook reset statment. Stmts
   --  is the initialization statement list for the component. All generated
   --  code is added to Stmts.

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type);
   --  Sort the Case Table using the Lower Bound of each Choice as the key.
   --  A simple insertion sort is used since the number of choices in a case
   --  statement of variant part will usually be small and probably in near
   --  sorted order.

   ------------------------------------------------------
   -- Local subprograms for Record Aggregate Expansion --
   ------------------------------------------------------

   function Is_Build_In_Place_Aggregate_Return (N : Node_Id) return Boolean;
   --  True if N is an aggregate (possibly qualified or converted) that is
   --  being returned from a build-in-place function.

   function Build_Record_Aggr_Code
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id) return List_Id;
   --  N is an N_Aggregate or an N_Extension_Aggregate. Typ is the type of the
   --  aggregate. Target is an expression containing the location on which the
   --  component by component assignments will take place. Returns the list of
   --  assignments plus all other adjustments needed for tagged and controlled
   --  types.

   procedure Convert_To_Assignments (N : Node_Id; Typ : Entity_Id);
   --  Transform a record aggregate into a sequence of assignments performed
   --  component by component.  N is an N_Aggregate or N_Extension_Aggregate.
   --  Typ is the type of the record aggregate.

   procedure Expand_Record_Aggregate
     (N           : Node_Id;
      Orig_Tag    : Node_Id := Empty;
      Parent_Expr : Node_Id := Empty);
   --  This is the top level procedure for record aggregate expansion.
   --  Expansion for record aggregates needs expand aggregates for tagged
   --  record types. Specifically Expand_Record_Aggregate adds the Tag
   --  field in front of the Component_Association list that was created
   --  during resolution by Resolve_Record_Aggregate.
   --
   --    N is the record aggregate node.
   --    Orig_Tag is the value of the Tag that has to be provided for this
   --      specific aggregate. It carries the tag corresponding to the type
   --      of the outermost aggregate during the recursive expansion
   --    Parent_Expr is the ancestor part of the original extension
   --      aggregate

   function Has_Mutable_Components (Typ : Entity_Id) return Boolean;
   --  Return true if one of the components is of a discriminated type with
   --  defaults. An aggregate for a type with mutable components must be
   --  expanded into individual assignments.

   procedure Initialize_Discriminants (N : Node_Id; Typ : Entity_Id);
   --  If the type of the aggregate is a type extension with renamed discrimi-
   --  nants, we must initialize the hidden discriminants of the parent.
   --  Otherwise, the target object must not be initialized. The discriminants
   --  are initialized by calling the initialization procedure for the type.
   --  This is incorrect if the initialization of other components has any
   --  side effects. We restrict this call to the case where the parent type
   --  has a variant part, because this is the only case where the hidden
   --  discriminants are accessed, namely when calling discriminant checking
   --  functions of the parent type, and when applying a stream attribute to
   --  an object of the derived type.

   -----------------------------------------------------
   -- Local Subprograms for Array Aggregate Expansion --
   -----------------------------------------------------

   function Aggr_Size_OK (N : Node_Id; Typ : Entity_Id) return Boolean;
   --  Very large static aggregates present problems to the back-end, and are
   --  transformed into assignments and loops. This function verifies that the
   --  total number of components of an aggregate is acceptable for rewriting
   --  into a purely positional static form. Aggr_Size_OK must be called before
   --  calling Flatten.
   --
   --  This function also detects and warns about one-component aggregates that
   --  appear in a nonstatic context. Even if the component value is static,
   --  such an aggregate must be expanded into an assignment.

   function Backend_Processing_Possible (N : Node_Id) return Boolean;
   --  This function checks if array aggregate N can be processed directly
   --  by the backend. If this is the case, True is returned.

   function Build_Array_Aggr_Code
     (N           : Node_Id;
      Ctype       : Entity_Id;
      Index       : Node_Id;
      Into        : Node_Id;
      Scalar_Comp : Boolean;
      Indexes     : List_Id := No_List) return List_Id;
   --  This recursive routine returns a list of statements containing the
   --  loops and assignments that are needed for the expansion of the array
   --  aggregate N.
   --
   --    N is the (sub-)aggregate node to be expanded into code. This node has
   --    been fully analyzed, and its Etype is properly set.
   --
   --    Index is the index node corresponding to the array subaggregate N
   --
   --    Into is the target expression into which we are copying the aggregate.
   --    Note that this node may not have been analyzed yet, and so the Etype
   --    field may not be set.
   --
   --    Scalar_Comp is True if the component type of the aggregate is scalar
   --
   --    Indexes is the current list of expressions used to index the object we
   --    are writing into.

   procedure Convert_Array_Aggr_In_Allocator
     (Decl   : Node_Id;
      Aggr   : Node_Id;
      Target : Node_Id);
   --  If the aggregate appears within an allocator and can be expanded in
   --  place, this routine generates the individual assignments to components
   --  of the designated object. This is an optimization over the general
   --  case, where a temporary is first created on the stack and then used to
   --  construct the allocated object on the heap.

   procedure Convert_To_Positional
     (N                    : Node_Id;
      Max_Others_Replicate : Nat     := 32;
      Handle_Bit_Packed    : Boolean := False);
   --  If possible, convert named notation to positional notation. This
   --  conversion is possible only in some static cases. If the conversion is
   --  possible, then N is rewritten with the analyzed converted aggregate.
   --  The parameter Max_Others_Replicate controls the maximum number of
   --  values corresponding to an others choice that will be converted to
   --  positional notation (the default of 32 is the normal limit, and reflects
   --  the fact that normally the loop is better than a lot of separate
   --  assignments). Note that this limit gets overridden in any case if
   --  either of the restrictions No_Elaboration_Code or No_Implicit_Loops is
   --  set. The parameter Handle_Bit_Packed is usually set False (since we do
   --  not expect the back end to handle bit packed arrays, so the normal case
   --  of conversion is pointless), but in the special case of a call from
   --  Packed_Array_Aggregate_Handled, we set this parameter to True, since
   --  these are cases we handle in there.

   procedure Expand_Array_Aggregate (N : Node_Id);
   --  This is the top-level routine to perform array aggregate expansion.
   --  N is the N_Aggregate node to be expanded.

   function Is_Two_Dim_Packed_Array (Typ : Entity_Id) return Boolean;
   --  For two-dimensional packed aggregates with constant bounds and constant
   --  components, it is preferable to pack the inner aggregates because the
   --  whole matrix can then be presented to the back-end as a one-dimensional
   --  list of literals. This is much more efficient than expanding into single
   --  component assignments. This function determines if the type Typ is for
   --  an array that is suitable for this optimization: it returns True if Typ
   --  is a two dimensional bit packed array with component size 1, 2, or 4.

   function Packed_Array_Aggregate_Handled (N : Node_Id) return Boolean;
   --  Given an array aggregate, this function handles the case of a packed
   --  array aggregate with all constant values, where the aggregate can be
   --  evaluated at compile time. If this is possible, then N is rewritten
   --  to be its proper compile time value with all the components properly
   --  assembled. The expression is analyzed and resolved and True is returned.
   --  If this transformation is not possible, N is unchanged and False is
   --  returned.

   function Two_Dim_Packed_Array_Handled (N : Node_Id) return Boolean;
   --  If the type of the aggregate is a two-dimensional bit_packed array
   --  it may be transformed into an array of bytes with constant values,
   --  and presented to the back-end as a static value. The function returns
   --  false if this transformation cannot be performed. THis is similar to,
   --  and reuses part of the machinery in Packed_Array_Aggregate_Handled.

   ------------------
   -- Aggr_Size_OK --
   ------------------

   function Aggr_Size_OK (N : Node_Id; Typ : Entity_Id) return Boolean is
      Lo   : Node_Id;
      Hi   : Node_Id;
      Indx : Node_Id;
      Siz  : Int;
      Lov  : Uint;
      Hiv  : Uint;

      Max_Aggr_Size : Nat;
      --  Determines the maximum size of an array aggregate produced by
      --  converting named to positional notation (e.g. from others clauses).
      --  This avoids running away with attempts to convert huge aggregates,
      --  which hit memory limits in the backend.

      function Component_Count (T : Entity_Id) return Nat;
      --  The limit is applied to the total number of subcomponents that the
      --  aggregate will have, which is the number of static expressions
      --  that will appear in the flattened array. This requires a recursive
      --  computation of the number of scalar components of the structure.

      ---------------------
      -- Component_Count --
      ---------------------

      function Component_Count (T : Entity_Id) return Nat is
         Res  : Nat := 0;
         Comp : Entity_Id;

      begin
         if Is_Scalar_Type (T) then
            return 1;

         elsif Is_Record_Type (T) then
            Comp := First_Component (T);
            while Present (Comp) loop
               Res := Res + Component_Count (Etype (Comp));
               Next_Component (Comp);
            end loop;

            return Res;

         elsif Is_Array_Type (T) then
            declare
               Lo : constant Node_Id :=
                 Type_Low_Bound (Etype (First_Index (T)));
               Hi : constant Node_Id :=
                 Type_High_Bound (Etype (First_Index (T)));

               Siz : constant Nat := Component_Count (Component_Type (T));

            begin
               --  Check for superflat arrays, i.e. arrays with such bounds
               --  as 4 .. 2, to insure that this function never returns a
               --  meaningless negative value.

               if not Compile_Time_Known_Value (Lo)
                 or else not Compile_Time_Known_Value (Hi)
                 or else Expr_Value (Hi) < Expr_Value (Lo)
               then
                  return 0;

               else
                  --  If the number of components is greater than Int'Last,
                  --  then return Int'Last, so caller will return False (Aggr
                  --  size is not OK). Otherwise, UI_To_Int will crash.

                  declare
                     UI : constant Uint :=
                            Expr_Value (Hi) - Expr_Value (Lo) + 1;
                  begin
                     if UI_Is_In_Int_Range (UI) then
                        return Siz * UI_To_Int (UI);
                     else
                        return Int'Last;
                     end if;
                  end;
               end if;
            end;

         else
            --  Can only be a null for an access type

            return 1;
         end if;
      end Component_Count;

   --  Start of processing for Aggr_Size_OK

   begin
      --  The normal aggregate limit is 500000, but we increase this limit to
      --  2**24 (about 16 million) if Restrictions (No_Elaboration_Code) or
      --  Restrictions (No_Implicit_Loops) is specified, since in either case
      --  we are at risk of declaring the program illegal because of this
      --  limit. We also increase the limit when Static_Elaboration_Desired,
      --  given that this means that objects are intended to be placed in data
      --  memory.

      --  We also increase the limit if the aggregate is for a packed two-
      --  dimensional array, because if components are static it is much more
      --  efficient to construct a one-dimensional equivalent array with static
      --  components.

      --  Conversely, we decrease the maximum size if none of the above
      --  requirements apply, and if the aggregate has a single component
      --  association, which will be more efficient if implemented with a loop.

      --  Finally, we use a small limit in CodePeer mode where we favor loops
      --  instead of thousands of single assignments (from large aggregates).

      Max_Aggr_Size := 500000;

      if CodePeer_Mode then
         Max_Aggr_Size := 100;

      elsif Restriction_Active (No_Elaboration_Code)
        or else Restriction_Active (No_Implicit_Loops)
        or else Is_Two_Dim_Packed_Array (Typ)
        or else (Ekind (Current_Scope) = E_Package
                   and then Static_Elaboration_Desired (Current_Scope))
      then
         Max_Aggr_Size := 2 ** 24;

      elsif No (Expressions (N))
        and then No (Next (First (Component_Associations (N))))
      then
         Max_Aggr_Size := 5000;
      end if;

      Siz  := Component_Count (Component_Type (Typ));

      Indx := First_Index (Typ);
      while Present (Indx) loop
         Lo  := Type_Low_Bound (Etype (Indx));
         Hi  := Type_High_Bound (Etype (Indx));

         --  Bounds need to be known at compile time

         if not Compile_Time_Known_Value (Lo)
           or else not Compile_Time_Known_Value (Hi)
         then
            return False;
         end if;

         Lov := Expr_Value (Lo);
         Hiv := Expr_Value (Hi);

         --  A flat array is always safe

         if Hiv < Lov then
            return True;
         end if;

         --  One-component aggregates are suspicious, and if the context type
         --  is an object declaration with nonstatic bounds it will trip gcc;
         --  such an aggregate must be expanded into a single assignment.

         if Hiv = Lov and then Nkind (Parent (N)) = N_Object_Declaration then
            declare
               Index_Type : constant Entity_Id :=
                 Etype
                   (First_Index (Etype (Defining_Identifier (Parent (N)))));
               Indx       : Node_Id;

            begin
               if not Compile_Time_Known_Value (Type_Low_Bound (Index_Type))
                 or else not Compile_Time_Known_Value
                               (Type_High_Bound (Index_Type))
               then
                  if Present (Component_Associations (N)) then
                     Indx :=
                       First
                         (Choice_List (First (Component_Associations (N))));

                     if Is_Entity_Name (Indx)
                       and then not Is_Type (Entity (Indx))
                     then
                        Error_Msg_N
                          ("single component aggregate in "
                           &  "non-static context??", Indx);
                        Error_Msg_N ("\maybe subtype name was meant??", Indx);
                     end if;
                  end if;

                  return False;
               end if;
            end;
         end if;

         declare
            Rng : constant Uint := Hiv - Lov + 1;

         begin
            --  Check if size is too large

            if not UI_Is_In_Int_Range (Rng) then
               return False;
            end if;

            Siz := Siz * UI_To_Int (Rng);
         end;

         if Siz <= 0
           or else Siz > Max_Aggr_Size
         then
            return False;
         end if;

         --  Bounds must be in integer range, for later array construction

         if not UI_Is_In_Int_Range (Lov)
             or else
            not UI_Is_In_Int_Range (Hiv)
         then
            return False;
         end if;

         Next_Index (Indx);
      end loop;

      return True;
   end Aggr_Size_OK;

   ---------------------------------
   -- Backend_Processing_Possible --
   ---------------------------------

   --  Backend processing by Gigi/gcc is possible only if all the following
   --  conditions are met:

   --    1. N is fully positional

   --    2. N is not a bit-packed array aggregate;

   --    3. The size of N's array type must be known at compile time. Note
   --       that this implies that the component size is also known

   --    4. The array type of N does not follow the Fortran layout convention
   --       or if it does it must be 1 dimensional.

   --    5. The array component type may not be tagged (which could necessitate
   --       reassignment of proper tags).

   --    6. The array component type must not have unaligned bit components

   --    7. None of the components of the aggregate may be bit unaligned
   --       components.

   --    8. There cannot be delayed components, since we do not know enough
   --       at this stage to know if back end processing is possible.

   --    9. There cannot be any discriminated record components, since the
   --       back end cannot handle this complex case.

   --   10. No controlled actions need to be generated for components

   --   11. When generating C code, N must be part of a N_Object_Declaration

   --   12. When generating C code, N must not include function calls

   function Backend_Processing_Possible (N : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (N);
      --  Typ is the correct constrained array subtype of the aggregate

      function Component_Check (N : Node_Id; Index : Node_Id) return Boolean;
      --  This routine checks components of aggregate N, enforcing checks
      --  1, 7, 8, 9, 11, and 12. In the multidimensional case, these checks
      --  are performed on subaggregates. The Index value is the current index
      --  being checked in the multidimensional case.

      ---------------------
      -- Component_Check --
      ---------------------

      function Component_Check (N : Node_Id; Index : Node_Id) return Boolean is
         function Ultimate_Original_Expression (N : Node_Id) return Node_Id;
         --  Given a type conversion or an unchecked type conversion N, return
         --  its innermost original expression.

         ----------------------------------
         -- Ultimate_Original_Expression --
         ----------------------------------

         function Ultimate_Original_Expression (N : Node_Id) return Node_Id is
            Expr : Node_Id := Original_Node (N);

         begin
            while Nkind_In (Expr, N_Type_Conversion,
                                  N_Unchecked_Type_Conversion)
            loop
               Expr := Original_Node (Expression (Expr));
            end loop;

            return Expr;
         end Ultimate_Original_Expression;

         --  Local variables

         Expr : Node_Id;

      --  Start of processing for Component_Check

      begin
         --  Checks 1: (no component associations)

         if Present (Component_Associations (N)) then
            return False;
         end if;

         --  Checks 11: The C code generator cannot handle aggregates that are
         --  not part of an object declaration.

         if Modify_Tree_For_C then
            declare
               Par : Node_Id := Parent (N);

            begin
               --  Skip enclosing nested aggregates and their qualified
               --  expressions.

               while Nkind (Par) = N_Aggregate
                 or else Nkind (Par) = N_Qualified_Expression
               loop
                  Par := Parent (Par);
               end loop;

               if Nkind (Par) /= N_Object_Declaration then
                  return False;
               end if;
            end;
         end if;

         --  Checks on components

         --  Recurse to check subaggregates, which may appear in qualified
         --  expressions. If delayed, the front-end will have to expand.
         --  If the component is a discriminated record, treat as nonstatic,
         --  as the back-end cannot handle this properly.

         Expr := First (Expressions (N));
         while Present (Expr) loop

            --  Checks 8: (no delayed components)

            if Is_Delayed_Aggregate (Expr) then
               return False;
            end if;

            --  Checks 9: (no discriminated records)

            if Present (Etype (Expr))
              and then Is_Record_Type (Etype (Expr))
              and then Has_Discriminants (Etype (Expr))
            then
               return False;
            end if;

            --  Checks 7. Component must not be bit aligned component

            if Possible_Bit_Aligned_Component (Expr) then
               return False;
            end if;

            --  Checks 12: (no function call)

            if Modify_Tree_For_C
              and then
                Nkind (Ultimate_Original_Expression (Expr)) = N_Function_Call
            then
               return False;
            end if;

            --  Recursion to following indexes for multiple dimension case

            if Present (Next_Index (Index))
              and then not Component_Check (Expr, Next_Index (Index))
            then
               return False;
            end if;

            --  All checks for that component finished, on to next

            Next (Expr);
         end loop;

         return True;
      end Component_Check;

   --  Start of processing for Backend_Processing_Possible

   begin
      --  Checks 2 (array not bit packed) and 10 (no controlled actions)

      if Is_Bit_Packed_Array (Typ) or else Needs_Finalization (Typ) then
         return False;
      end if;

      --  If component is limited, aggregate must be expanded because each
      --  component assignment must be built in place.

      if Is_Limited_View (Component_Type (Typ)) then
         return False;
      end if;

      --  Checks 4 (array must not be multidimensional Fortran case)

      if Convention (Typ) = Convention_Fortran
        and then Number_Dimensions (Typ) > 1
      then
         return False;
      end if;

      --  Checks 3 (size of array must be known at compile time)

      if not Size_Known_At_Compile_Time (Typ) then
         return False;
      end if;

      --  Checks on components

      if not Component_Check (N, First_Index (Typ)) then
         return False;
      end if;

      --  Checks 5 (if the component type is tagged, then we may need to do
      --  tag adjustments. Perhaps this should be refined to check for any
      --  component associations that actually need tag adjustment, similar
      --  to the test in Component_OK_For_Backend for record aggregates with
      --  tagged components, but not clear whether it's worthwhile ???; in the
      --  case of virtual machines (no Tagged_Type_Expansion), object tags are
      --  handled implicitly).

      if Is_Tagged_Type (Component_Type (Typ))
        and then Tagged_Type_Expansion
      then
         return False;
      end if;

      --  Checks 6 (component type must not have bit aligned components)

      if Type_May_Have_Bit_Aligned_Components (Component_Type (Typ)) then
         return False;
      end if;

      --  Backend processing is possible

      Set_Size_Known_At_Compile_Time (Etype (N), True);
      return True;
   end Backend_Processing_Possible;

   ---------------------------
   -- Build_Array_Aggr_Code --
   ---------------------------

   --  The code that we generate from a one dimensional aggregate is

   --  1. If the subaggregate contains discrete choices we

   --     (a) Sort the discrete choices

   --     (b) Otherwise for each discrete choice that specifies a range we
   --         emit a loop. If a range specifies a maximum of three values, or
   --         we are dealing with an expression we emit a sequence of
   --         assignments instead of a loop.

   --     (c) Generate the remaining loops to cover the others choice if any

   --  2. If the aggregate contains positional elements we

   --     (a) translate the positional elements in a series of assignments

   --     (b) Generate a final loop to cover the others choice if any.
   --         Note that this final loop has to be a while loop since the case

   --             L : Integer := Integer'Last;
   --             H : Integer := Integer'Last;
   --             A : array (L .. H) := (1, others =>0);

   --         cannot be handled by a for loop. Thus for the following

   --             array (L .. H) := (.. positional elements.., others =>E);

   --         we always generate something like:

   --             J : Index_Type := Index_Of_Last_Positional_Element;
   --             while J < H loop
   --                J := Index_Base'Succ (J)
   --                Tmp (J) := E;
   --             end loop;

   function Build_Array_Aggr_Code
     (N           : Node_Id;
      Ctype       : Entity_Id;
      Index       : Node_Id;
      Into        : Node_Id;
      Scalar_Comp : Boolean;
      Indexes     : List_Id := No_List) return List_Id
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Index_Base   : constant Entity_Id  := Base_Type (Etype (Index));
      Index_Base_L : constant Node_Id := Type_Low_Bound (Index_Base);
      Index_Base_H : constant Node_Id := Type_High_Bound (Index_Base);

      function Add (Val : Int; To : Node_Id) return Node_Id;
      --  Returns an expression where Val is added to expression To, unless
      --  To+Val is provably out of To's base type range. To must be an
      --  already analyzed expression.

      function Empty_Range (L, H : Node_Id) return Boolean;
      --  Returns True if the range defined by L .. H is certainly empty

      function Equal (L, H : Node_Id) return Boolean;
      --  Returns True if L = H for sure

      function Index_Base_Name return Node_Id;
      --  Returns a new reference to the index type name

      function Gen_Assign
        (Ind     : Node_Id;
         Expr    : Node_Id;
         In_Loop : Boolean := False) return List_Id;
      --  Ind must be a side-effect-free expression. If the input aggregate N
      --  to Build_Loop contains no subaggregates, then this function returns
      --  the assignment statement:
      --
      --     Into (Indexes, Ind) := Expr;
      --
      --  Otherwise we call Build_Code recursively. Flag In_Loop should be set
      --  when the assignment appears within a generated loop.
      --
      --  Ada 2005 (AI-287): In case of default initialized component, Expr
      --  is empty and we generate a call to the corresponding IP subprogram.

      function Gen_Loop (L, H : Node_Id; Expr : Node_Id) return List_Id;
      --  Nodes L and H must be side-effect-free expressions. If the input
      --  aggregate N to Build_Loop contains no subaggregates, this routine
      --  returns the for loop statement:
      --
      --     for J in Index_Base'(L) .. Index_Base'(H) loop
      --        Into (Indexes, J) := Expr;
      --     end loop;
      --
      --  Otherwise we call Build_Code recursively. As an optimization if the
      --  loop covers 3 or fewer scalar elements we generate a sequence of
      --  assignments.
      --  If the component association that generates the loop comes from an
      --  Iterated_Component_Association, the loop parameter has the name of
      --  the corresponding parameter in the original construct.

      function Gen_While (L, H : Node_Id; Expr : Node_Id) return List_Id;
      --  Nodes L and H must be side-effect-free expressions. If the input
      --  aggregate N to Build_Loop contains no subaggregates, this routine
      --  returns the while loop statement:
      --
      --     J : Index_Base := L;
      --     while J < H loop
      --        J := Index_Base'Succ (J);
      --        Into (Indexes, J) := Expr;
      --     end loop;
      --
      --  Otherwise we call Build_Code recursively

      function Get_Assoc_Expr (Assoc : Node_Id) return Node_Id;
      --  For an association with a box, use value given by aspect
     --   Default_Component_Value of array type if specified, else use
     --   value given by aspect Default_Value for component type itself
     --   if specified, else return Empty.

      function Local_Compile_Time_Known_Value (E : Node_Id) return Boolean;
      function Local_Expr_Value               (E : Node_Id) return Uint;
      --  These two Local routines are used to replace the corresponding ones
      --  in sem_eval because while processing the bounds of an aggregate with
      --  discrete choices whose index type is an enumeration, we build static
      --  expressions not recognized by Compile_Time_Known_Value as such since
      --  they have not yet been analyzed and resolved. All the expressions in
      --  question are things like Index_Base_Name'Val (Const) which we can
      --  easily recognize as being constant.

      ---------
      -- Add --
      ---------

      function Add (Val : Int; To : Node_Id) return Node_Id is
         Expr_Pos : Node_Id;
         Expr     : Node_Id;
         To_Pos   : Node_Id;
         U_To     : Uint;
         U_Val    : constant Uint := UI_From_Int (Val);

      begin
         --  Note: do not try to optimize the case of Val = 0, because
         --  we need to build a new node with the proper Sloc value anyway.

         --  First test if we can do constant folding

         if Local_Compile_Time_Known_Value (To) then
            U_To := Local_Expr_Value (To) + Val;

            --  Determine if our constant is outside the range of the index.
            --  If so return an Empty node. This empty node will be caught
            --  by Empty_Range below.

            if Compile_Time_Known_Value (Index_Base_L)
              and then U_To < Expr_Value (Index_Base_L)
            then
               return Empty;

            elsif Compile_Time_Known_Value (Index_Base_H)
              and then U_To > Expr_Value (Index_Base_H)
            then
               return Empty;
            end if;

            Expr_Pos := Make_Integer_Literal (Loc, U_To);
            Set_Is_Static_Expression (Expr_Pos);

            if not Is_Enumeration_Type (Index_Base) then
               Expr := Expr_Pos;

            --  If we are dealing with enumeration return
            --     Index_Base'Val (Expr_Pos)

            else
               Expr :=
                 Make_Attribute_Reference
                   (Loc,
                    Prefix         => Index_Base_Name,
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
                Right_Opnd => Make_Integer_Literal (Loc, U_Val));

         --  If we are dealing with enumeration return
         --    Index_Base'Val (Index_Base'Pos (To) + Val)

         else
            To_Pos :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => Index_Base_Name,
                 Attribute_Name => Name_Pos,
                 Expressions    => New_List (Duplicate_Subexpr (To)));

            Expr_Pos :=
              Make_Op_Add (Loc,
                Left_Opnd  => To_Pos,
                Right_Opnd => Make_Integer_Literal (Loc, U_Val));

            Expr :=
              Make_Attribute_Reference
                (Loc,
                 Prefix         => Index_Base_Name,
                 Attribute_Name => Name_Val,
                 Expressions    => New_List (Expr_Pos));
         end if;

         return Expr;
      end Add;

      -----------------
      -- Empty_Range --
      -----------------

      function Empty_Range (L, H : Node_Id) return Boolean is
         Is_Empty : Boolean := False;
         Low      : Node_Id;
         High     : Node_Id;

      begin
         --  First check if L or H were already detected as overflowing the
         --  index base range type by function Add above. If this is so Add
         --  returns the empty node.

         if No (L) or else No (H) then
            return True;
         end if;

         for J in 1 .. 3 loop
            case J is

               --  L > H    range is empty

               when 1 =>
                  Low  := L;
                  High := H;

               --  B_L > H  range must be empty

               when 2 =>
                  Low  := Index_Base_L;
                  High := H;

               --  L > B_H  range must be empty

               when 3 =>
                  Low  := L;
                  High := Index_Base_H;
            end case;

            if Local_Compile_Time_Known_Value (Low)
                 and then
               Local_Compile_Time_Known_Value (High)
            then
               Is_Empty :=
                 UI_Gt (Local_Expr_Value (Low), Local_Expr_Value (High));
            end if;

            exit when Is_Empty;
         end loop;

         return Is_Empty;
      end Empty_Range;

      -----------
      -- Equal --
      -----------

      function Equal (L, H : Node_Id) return Boolean is
      begin
         if L = H then
            return True;

         elsif Local_Compile_Time_Known_Value (L)
                 and then
               Local_Compile_Time_Known_Value (H)
         then
            return UI_Eq (Local_Expr_Value (L), Local_Expr_Value (H));
         end if;

         return False;
      end Equal;

      ----------------
      -- Gen_Assign --
      ----------------

      function Gen_Assign
        (Ind     : Node_Id;
         Expr    : Node_Id;
         In_Loop : Boolean := False) return List_Id
       is
         function Add_Loop_Actions (Lis : List_Id) return List_Id;
         --  Collect insert_actions generated in the construction of a loop,
         --  and prepend them to the sequence of assignments to complete the
         --  eventual body of the loop.

         procedure Initialize_Array_Component
           (Arr_Comp  : Node_Id;
            Comp_Typ  : Node_Id;
            Init_Expr : Node_Id;
            Stmts     : List_Id);
         --  Perform the initialization of array component Arr_Comp with
         --  expected type Comp_Typ. Init_Expr denotes the initialization
         --  expression of the array component. All generated code is added
         --  to list Stmts.

         procedure Initialize_Ctrl_Array_Component
           (Arr_Comp  : Node_Id;
            Comp_Typ  : Entity_Id;
            Init_Expr : Node_Id;
            Stmts     : List_Id);
         --  Perform the initialization of array component Arr_Comp when its
         --  expected type Comp_Typ needs finalization actions. Init_Expr is
         --  the initialization expression of the array component. All hook-
         --  related declarations are inserted prior to aggregate N. Remaining
         --  code is added to list Stmts.

         ----------------------
         -- Add_Loop_Actions --
         ----------------------

         function Add_Loop_Actions (Lis : List_Id) return List_Id is
            Res : List_Id;

         begin
            --  Ada 2005 (AI-287): Do nothing else in case of default
            --  initialized component.

            if No (Expr) then
               return Lis;

            elsif Nkind (Parent (Expr)) = N_Component_Association
              and then Present (Loop_Actions (Parent (Expr)))
            then
               Append_List (Lis, Loop_Actions (Parent (Expr)));
               Res := Loop_Actions (Parent (Expr));
               Set_Loop_Actions (Parent (Expr), No_List);
               return Res;

            else
               return Lis;
            end if;
         end Add_Loop_Actions;

         --------------------------------
         -- Initialize_Array_Component --
         --------------------------------

         procedure Initialize_Array_Component
           (Arr_Comp  : Node_Id;
            Comp_Typ  : Node_Id;
            Init_Expr : Node_Id;
            Stmts     : List_Id)
         is
            Exceptions_OK : constant Boolean :=
                              not Restriction_Active
                                    (No_Exception_Propagation);

            Finalization_OK : constant Boolean :=
                                Present (Comp_Typ)
                                  and then Needs_Finalization (Comp_Typ);

            Full_Typ  : constant Entity_Id := Underlying_Type (Comp_Typ);
            Adj_Call  : Node_Id;
            Blk_Stmts : List_Id;
            Init_Stmt : Node_Id;

         begin
            --  Protect the initialization statements from aborts. Generate:

            --    Abort_Defer;

            if Finalization_OK and Abort_Allowed then
               if Exceptions_OK then
                  Blk_Stmts := New_List;
               else
                  Blk_Stmts := Stmts;
               end if;

               Append_To (Blk_Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));

            --  Otherwise aborts are not allowed. All generated code is added
            --  directly to the input list.

            else
               Blk_Stmts := Stmts;
            end if;

            --  Initialize the array element. Generate:

            --    Arr_Comp := Init_Expr;

            --  Note that the initialization expression is replicated because
            --  it has to be reevaluated within a generated loop.

            Init_Stmt :=
              Make_OK_Assignment_Statement (Loc,
                Name       => New_Copy_Tree (Arr_Comp),
                Expression => New_Copy_Tree (Init_Expr));
            Set_No_Ctrl_Actions (Init_Stmt);

            --  If this is an aggregate for an array of arrays, each
            --  subaggregate will be expanded as well, and even with
            --  No_Ctrl_Actions the assignments of inner components will
            --  require attachment in their assignments to temporaries. These
            --  temporaries must be finalized for each subaggregate. Generate:

            --    begin
            --       Arr_Comp := Init_Expr;
            --    end;

            if Finalization_OK and then Is_Array_Type (Comp_Typ) then
               Init_Stmt :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (Init_Stmt)));
            end if;

            Append_To (Blk_Stmts, Init_Stmt);

            --  Adjust the tag due to a possible view conversion. Generate:

            --    Arr_Comp._tag := Full_TypP;

            if Tagged_Type_Expansion
              and then Present (Comp_Typ)
              and then Is_Tagged_Type (Comp_Typ)
            then
               Append_To (Blk_Stmts,
                 Make_OK_Assignment_Statement (Loc,
                   Name       =>
                     Make_Selected_Component (Loc,
                       Prefix        => New_Copy_Tree (Arr_Comp),
                       Selector_Name =>
                         New_Occurrence_Of
                           (First_Tag_Component (Full_Typ), Loc)),

                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Occurrence_Of
                         (Node (First_Elmt (Access_Disp_Table (Full_Typ))),
                          Loc))));
            end if;

            --  Adjust the array component. Controlled subaggregates are not
            --  considered because each of their individual elements will
            --  receive an adjustment of its own. Generate:

            --    [Deep_]Adjust (Arr_Comp);

            if Finalization_OK
              and then not Is_Limited_Type (Comp_Typ)
              and then not Is_Build_In_Place_Function_Call (Init_Expr)
              and then not
                (Is_Array_Type (Comp_Typ)
                  and then Is_Controlled (Component_Type (Comp_Typ))
                  and then Nkind (Expr) = N_Aggregate)
            then
               Adj_Call :=
                 Make_Adjust_Call
                   (Obj_Ref => New_Copy_Tree (Arr_Comp),
                    Typ     => Comp_Typ);

               --  Guard against a missing [Deep_]Adjust when the component
               --  type was not frozen properly.

               if Present (Adj_Call) then
                  Append_To (Blk_Stmts, Adj_Call);
               end if;
            end if;

            --  Complete the protection of the initialization statements

            if Finalization_OK and Abort_Allowed then

               --  Wrap the initialization statements in a block to catch a
               --  potential exception. Generate:

               --    begin
               --       Abort_Defer;
               --       Arr_Comp := Init_Expr;
               --       Arr_Comp._tag := Full_TypP;
               --       [Deep_]Adjust (Arr_Comp);
               --    at end
               --       Abort_Undefer_Direct;
               --    end;

               if Exceptions_OK then
                  Append_To (Stmts,
                    Build_Abort_Undefer_Block (Loc,
                      Stmts   => Blk_Stmts,
                      Context => N));

               --  Otherwise exceptions are not propagated. Generate:

               --    Abort_Defer;
               --    Arr_Comp := Init_Expr;
               --    Arr_Comp._tag := Full_TypP;
               --    [Deep_]Adjust (Arr_Comp);
               --    Abort_Undefer;

               else
                  Append_To (Blk_Stmts,
                    Build_Runtime_Call (Loc, RE_Abort_Undefer));
               end if;
            end if;
         end Initialize_Array_Component;

         -------------------------------------
         -- Initialize_Ctrl_Array_Component --
         -------------------------------------

         procedure Initialize_Ctrl_Array_Component
           (Arr_Comp  : Node_Id;
            Comp_Typ  : Entity_Id;
            Init_Expr : Node_Id;
            Stmts     : List_Id)
         is
            Act_Aggr   : Node_Id;
            Act_Stmts  : List_Id;
            Expr       : Node_Id;
            Fin_Call   : Node_Id;
            Hook_Clear : Node_Id;

            In_Place_Expansion : Boolean;
            --  Flag set when a nonlimited controlled function call requires
            --  in-place expansion.

         begin
            --  Duplicate the initialization expression in case the context is
            --  a multi choice list or an "others" choice which plugs various
            --  holes in the aggregate. As a result the expression is no longer
            --  shared between the various components and is reevaluated for
            --  each such component.

            Expr := New_Copy_Tree (Init_Expr);
            Set_Parent (Expr, Parent (Init_Expr));

            --  Perform a preliminary analysis and resolution to determine what
            --  the initialization expression denotes. An unanalyzed function
            --  call may appear as an identifier or an indexed component.

            if Nkind_In (Expr, N_Function_Call,
                               N_Identifier,
                               N_Indexed_Component)
              and then not Analyzed (Expr)
            then
               Preanalyze_And_Resolve (Expr, Comp_Typ);
            end if;

            In_Place_Expansion :=
              Nkind (Expr) = N_Function_Call
                and then not Is_Build_In_Place_Result_Type (Comp_Typ);

            --  The initialization expression is a controlled function call.
            --  Perform in-place removal of side effects to avoid creating a
            --  transient scope, which leads to premature finalization.

            --  This in-place expansion is not performed for limited transient
            --  objects because the initialization is already done in-place.

            if In_Place_Expansion then

               --  Suppress the removal of side effects by general analysis
               --  because this behavior is emulated here. This avoids the
               --  generation of a transient scope, which leads to out-of-order
               --  adjustment and finalization.

               Set_No_Side_Effect_Removal (Expr);

               --  When the transient component initialization is related to a
               --  range or an "others", keep all generated statements within
               --  the enclosing loop. This way the controlled function call
               --  will be evaluated at each iteration, and its result will be
               --  finalized at the end of each iteration.

               if In_Loop then
                  Act_Aggr  := Empty;
                  Act_Stmts := Stmts;

               --  Otherwise this is a single component initialization. Hook-
               --  related statements are inserted prior to the aggregate.

               else
                  Act_Aggr  := N;
                  Act_Stmts := No_List;
               end if;

               --  Install all hook-related declarations and prepare the clean
               --  up statements.

               Process_Transient_Component
                 (Loc        => Loc,
                  Comp_Typ   => Comp_Typ,
                  Init_Expr  => Expr,
                  Fin_Call   => Fin_Call,
                  Hook_Clear => Hook_Clear,
                  Aggr       => Act_Aggr,
                  Stmts      => Act_Stmts);
            end if;

            --  Use the noncontrolled component initialization circuitry to
            --  assign the result of the function call to the array element.
            --  This also performs subaggregate wrapping, tag adjustment, and
            --  [deep] adjustment of the array element.

            Initialize_Array_Component
              (Arr_Comp  => Arr_Comp,
               Comp_Typ  => Comp_Typ,
               Init_Expr => Expr,
               Stmts     => Stmts);

            --  At this point the array element is fully initialized. Complete
            --  the processing of the controlled array component by finalizing
            --  the transient function result.

            if In_Place_Expansion then
               Process_Transient_Component_Completion
                 (Loc        => Loc,
                  Aggr       => N,
                  Fin_Call   => Fin_Call,
                  Hook_Clear => Hook_Clear,
                  Stmts      => Stmts);
            end if;
         end Initialize_Ctrl_Array_Component;

         --  Local variables

         Stmts : constant List_Id := New_List;

         Comp_Typ     : Entity_Id := Empty;
         Expr_Q       : Node_Id;
         Indexed_Comp : Node_Id;
         Init_Call    : Node_Id;
         New_Indexes  : List_Id;

      --  Start of processing for Gen_Assign

      begin
         if No (Indexes) then
            New_Indexes := New_List;
         else
            New_Indexes := New_Copy_List_Tree (Indexes);
         end if;

         Append_To (New_Indexes, Ind);

         if Present (Next_Index (Index)) then
            return
              Add_Loop_Actions (
                Build_Array_Aggr_Code
                  (N           => Expr,
                   Ctype       => Ctype,
                   Index       => Next_Index (Index),
                   Into        => Into,
                   Scalar_Comp => Scalar_Comp,
                   Indexes     => New_Indexes));
         end if;

         --  If we get here then we are at a bottom-level (sub-)aggregate

         Indexed_Comp :=
           Checks_Off
             (Make_Indexed_Component (Loc,
                Prefix      => New_Copy_Tree (Into),
                Expressions => New_Indexes));

         Set_Assignment_OK (Indexed_Comp);

         --  Ada 2005 (AI-287): In case of default initialized component, Expr
         --  is not present (and therefore we also initialize Expr_Q to empty).

         if No (Expr) then
            Expr_Q := Empty;
         elsif Nkind (Expr) = N_Qualified_Expression then
            Expr_Q := Expression (Expr);
         else
            Expr_Q := Expr;
         end if;

         if Present (Etype (N)) and then Etype (N) /= Any_Composite then
            Comp_Typ := Component_Type (Etype (N));
            pragma Assert (Comp_Typ = Ctype); --  AI-287

         elsif Present (Next (First (New_Indexes))) then

            --  Ada 2005 (AI-287): Do nothing in case of default initialized
            --  component because we have received the component type in
            --  the formal parameter Ctype.

            --  ??? Some assert pragmas have been added to check if this new
            --  formal can be used to replace this code in all cases.

            if Present (Expr) then

               --  This is a multidimensional array. Recover the component type
               --  from the outermost aggregate, because subaggregates do not
               --  have an assigned type.

               declare
                  P : Node_Id;

               begin
                  P := Parent (Expr);
                  while Present (P) loop
                     if Nkind (P) = N_Aggregate
                       and then Present (Etype (P))
                     then
                        Comp_Typ := Component_Type (Etype (P));
                        exit;

                     else
                        P := Parent (P);
                     end if;
                  end loop;

                  pragma Assert (Comp_Typ = Ctype); --  AI-287
               end;
            end if;
         end if;

         --  Ada 2005 (AI-287): We only analyze the expression in case of non-
         --  default initialized components (otherwise Expr_Q is not present).

         if Present (Expr_Q)
           and then Nkind_In (Expr_Q, N_Aggregate, N_Extension_Aggregate)
         then
            --  At this stage the Expression may not have been analyzed yet
            --  because the array aggregate code has not been updated to use
            --  the Expansion_Delayed flag and avoid analysis altogether to
            --  solve the same problem (see Resolve_Aggr_Expr). So let us do
            --  the analysis of non-array aggregates now in order to get the
            --  value of Expansion_Delayed flag for the inner aggregate ???

            --  In the case of an iterated component association, the analysis
            --  of the generated loop will analyze the expression in the
            --  proper context, in which the loop parameter is visible.

            if Present (Comp_Typ) and then not Is_Array_Type (Comp_Typ) then
               if Nkind (Parent (Expr_Q)) = N_Iterated_Component_Association
                 or else Nkind (Parent (Parent ((Expr_Q)))) =
                           N_Iterated_Component_Association
               then
                  null;
               else
                  Analyze_And_Resolve (Expr_Q, Comp_Typ);
               end if;
            end if;

            if Is_Delayed_Aggregate (Expr_Q) then

               --  This is either a subaggregate of a multidimensional array,
               --  or a component of an array type whose component type is
               --  also an array. In the latter case, the expression may have
               --  component associations that provide different bounds from
               --  those of the component type, and sliding must occur. Instead
               --  of decomposing the current aggregate assignment, force the
               --  reanalysis of the assignment, so that a temporary will be
               --  generated in the usual fashion, and sliding will take place.

               if Nkind (Parent (N)) = N_Assignment_Statement
                 and then Is_Array_Type (Comp_Typ)
                 and then Present (Component_Associations (Expr_Q))
                 and then Must_Slide (Comp_Typ, Etype (Expr_Q))
               then
                  Set_Expansion_Delayed (Expr_Q, False);
                  Set_Analyzed (Expr_Q, False);

               else
                  return
                    Add_Loop_Actions (
                      Late_Expansion (Expr_Q, Etype (Expr_Q), Indexed_Comp));
               end if;
            end if;
         end if;

         if Present (Expr) then

            --  Handle an initialization expression of a controlled type in
            --  case it denotes a function call. In general such a scenario
            --  will produce a transient scope, but this will lead to wrong
            --  order of initialization, adjustment, and finalization in the
            --  context of aggregates.

            --    Target (1) := Ctrl_Func_Call;

            --    begin                                  --  scope
            --       Trans_Obj : ... := Ctrl_Func_Call;  --  object
            --       Target (1) := Trans_Obj;
            --       Finalize (Trans_Obj);
            --    end;
            --    Target (1)._tag := ...;
            --    Adjust (Target (1));

            --  In the example above, the call to Finalize occurs too early
            --  and as a result it may leave the array component in a bad
            --  state. Finalization of the transient object should really
            --  happen after adjustment.

            --  To avoid this scenario, perform in-place side-effect removal
            --  of the function call. This eliminates the transient property
            --  of the function result and ensures correct order of actions.

            --    Res : ... := Ctrl_Func_Call;
            --    Target (1) := Res;
            --    Target (1)._tag := ...;
            --    Adjust (Target (1));
            --    Finalize (Res);

            if Present (Comp_Typ)
              and then Needs_Finalization (Comp_Typ)
              and then Nkind (Expr) /= N_Aggregate
            then
               Initialize_Ctrl_Array_Component
                 (Arr_Comp  => Indexed_Comp,
                  Comp_Typ  => Comp_Typ,
                  Init_Expr => Expr,
                  Stmts     => Stmts);

            --  Otherwise perform simple component initialization

            else
               Initialize_Array_Component
                 (Arr_Comp  => Indexed_Comp,
                  Comp_Typ  => Comp_Typ,
                  Init_Expr => Expr,
                  Stmts     => Stmts);
            end if;

         --  Ada 2005 (AI-287): In case of default initialized component, call
         --  the initialization subprogram associated with the component type.
         --  If the component type is an access type, add an explicit null
         --  assignment, because for the back-end there is an initialization
         --  present for the whole aggregate, and no default initialization
         --  will take place.

         --  In addition, if the component type is controlled, we must call
         --  its Initialize procedure explicitly, because there is no explicit
         --  object creation that will invoke it otherwise.

         else
            if Present (Base_Init_Proc (Base_Type (Ctype)))
              or else Has_Task (Base_Type (Ctype))
            then
               Append_List_To (Stmts,
                 Build_Initialization_Call (Loc,
                   Id_Ref            => Indexed_Comp,
                   Typ               => Ctype,
                   With_Default_Init => True));

               --  If the component type has invariants, add an invariant
               --  check after the component is default-initialized. It will
               --  be analyzed and resolved before the code for initialization
               --  of other components.

               if Has_Invariants (Ctype) then
                  Set_Etype (Indexed_Comp, Ctype);
                  Append_To (Stmts, Make_Invariant_Call (Indexed_Comp));
               end if;

            elsif Is_Access_Type (Ctype) then
               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Copy_Tree (Indexed_Comp),
                   Expression => Make_Null (Loc)));
            end if;

            if Needs_Finalization (Ctype) then
               Init_Call :=
                 Make_Init_Call
                   (Obj_Ref => New_Copy_Tree (Indexed_Comp),
                    Typ     => Ctype);

               --  Guard against a missing [Deep_]Initialize when the component
               --  type was not properly frozen.

               if Present (Init_Call) then
                  Append_To (Stmts, Init_Call);
               end if;
            end if;
         end if;

         return Add_Loop_Actions (Stmts);
      end Gen_Assign;

      --------------
      -- Gen_Loop --
      --------------

      function Gen_Loop (L, H : Node_Id; Expr : Node_Id) return List_Id is
         Is_Iterated_Component : constant Boolean :=
           Nkind (Parent (Expr)) = N_Iterated_Component_Association;

         L_J : Node_Id;

         L_L : Node_Id;
         --  Index_Base'(L)

         L_H : Node_Id;
         --  Index_Base'(H)

         L_Range : Node_Id;
         --  Index_Base'(L) .. Index_Base'(H)

         L_Iteration_Scheme : Node_Id;
         --  L_J in Index_Base'(L) .. Index_Base'(H)

         L_Body : List_Id;
         --  The statements to execute in the loop

         S : constant List_Id := New_List;
         --  List of statements

         Tcopy : Node_Id;
         --  Copy of expression tree, used for checking purposes

      begin
         --  If loop bounds define an empty range return the null statement

         if Empty_Range (L, H) then
            Append_To (S, Make_Null_Statement (Loc));

            --  Ada 2005 (AI-287): Nothing else need to be done in case of
            --  default initialized component.

            if No (Expr) then
               null;

            else
               --  The expression must be type-checked even though no component
               --  of the aggregate will have this value. This is done only for
               --  actual components of the array, not for subaggregates. Do
               --  the check on a copy, because the expression may be shared
               --  among several choices, some of which might be non-null.

               if Present (Etype (N))
                 and then Is_Array_Type (Etype (N))
                 and then No (Next_Index (Index))
               then
                  Expander_Mode_Save_And_Set (False);
                  Tcopy := New_Copy_Tree (Expr);
                  Set_Parent (Tcopy, N);
                  Analyze_And_Resolve (Tcopy, Component_Type (Etype (N)));
                  Expander_Mode_Restore;
               end if;
            end if;

            return S;

         --  If loop bounds are the same then generate an assignment, unless
         --  the parent construct is an Iterated_Component_Association.

         elsif Equal (L, H) and then not Is_Iterated_Component then
            return Gen_Assign (New_Copy_Tree (L), Expr);

         --  If H - L <= 2 then generate a sequence of assignments when we are
         --  processing the bottom most aggregate and it contains scalar
         --  components.

         elsif No (Next_Index (Index))
           and then Scalar_Comp
           and then Local_Compile_Time_Known_Value (L)
           and then Local_Compile_Time_Known_Value (H)
           and then Local_Expr_Value (H) - Local_Expr_Value (L) <= 2
           and then not Is_Iterated_Component
         then
            Append_List_To (S, Gen_Assign (New_Copy_Tree (L), Expr));
            Append_List_To (S, Gen_Assign (Add (1, To => L), Expr));

            if Local_Expr_Value (H) - Local_Expr_Value (L) = 2 then
               Append_List_To (S, Gen_Assign (Add (2, To => L), Expr));
            end if;

            return S;
         end if;

         --  Otherwise construct the loop, starting with the loop index L_J

         if Is_Iterated_Component then
            L_J :=
              Make_Defining_Identifier (Loc,
                Chars => (Chars (Defining_Identifier (Parent (Expr)))));

         else
            L_J := Make_Temporary (Loc, 'J', L);
         end if;

         --  Construct "L .. H" in Index_Base. We use a qualified expression
         --  for the bound to convert to the index base, but we don't need
         --  to do that if we already have the base type at hand.

         if Etype (L) = Index_Base then
            L_L := L;
         else
            L_L :=
              Make_Qualified_Expression (Loc,
                Subtype_Mark => Index_Base_Name,
                Expression   => New_Copy_Tree (L));
         end if;

         if Etype (H) = Index_Base then
            L_H := H;
         else
            L_H :=
              Make_Qualified_Expression (Loc,
                Subtype_Mark => Index_Base_Name,
                Expression   => New_Copy_Tree (H));
         end if;

         L_Range :=
           Make_Range (Loc,
             Low_Bound  => L_L,
             High_Bound => L_H);

         --  Construct "for L_J in Index_Base range L .. H"

         L_Iteration_Scheme :=
           Make_Iteration_Scheme
             (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification
                  (Loc,
                   Defining_Identifier         => L_J,
                   Discrete_Subtype_Definition => L_Range));

         --  Construct the statements to execute in the loop body

         L_Body :=
           Gen_Assign (New_Occurrence_Of (L_J, Loc), Expr, In_Loop => True);

         --  Construct the final loop

         Append_To (S,
           Make_Implicit_Loop_Statement
             (Node             => N,
              Identifier       => Empty,
              Iteration_Scheme => L_Iteration_Scheme,
              Statements       => L_Body));

         --  A small optimization: if the aggregate is initialized with a box
         --  and the component type has no initialization procedure, remove the
         --  useless empty loop.

         if Nkind (First (S)) = N_Loop_Statement
           and then Is_Empty_List (Statements (First (S)))
         then
            return New_List (Make_Null_Statement (Loc));
         else
            return S;
         end if;
      end Gen_Loop;

      ---------------
      -- Gen_While --
      ---------------

      --  The code built is

      --     W_J : Index_Base := L;
      --     while W_J < H loop
      --        W_J := Index_Base'Succ (W);
      --        L_Body;
      --     end loop;

      function Gen_While (L, H : Node_Id; Expr : Node_Id) return List_Id is
         W_J : Node_Id;

         W_Decl : Node_Id;
         --  W_J : Base_Type := L;

         W_Iteration_Scheme : Node_Id;
         --  while W_J < H

         W_Index_Succ : Node_Id;
         --  Index_Base'Succ (J)

         W_Increment : Node_Id;
         --  W_J := Index_Base'Succ (W)

         W_Body : constant List_Id := New_List;
         --  The statements to execute in the loop

         S : constant List_Id := New_List;
         --  list of statement

      begin
         --  If loop bounds define an empty range or are equal return null

         if Empty_Range (L, H) or else Equal (L, H) then
            Append_To (S, Make_Null_Statement (Loc));
            return S;
         end if;

         --  Build the decl of W_J

         W_J    := Make_Temporary (Loc, 'J', L);
         W_Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => W_J,
              Object_Definition   => Index_Base_Name,
              Expression          => L);

         --  Theoretically we should do a New_Copy_Tree (L) here, but we know
         --  that in this particular case L is a fresh Expr generated by
         --  Add which we are the only ones to use.

         Append_To (S, W_Decl);

         --  Construct " while W_J < H"

         W_Iteration_Scheme :=
           Make_Iteration_Scheme
             (Loc,
              Condition => Make_Op_Lt
                             (Loc,
                              Left_Opnd  => New_Occurrence_Of (W_J, Loc),
                              Right_Opnd => New_Copy_Tree (H)));

         --  Construct the statements to execute in the loop body

         W_Index_Succ :=
           Make_Attribute_Reference
             (Loc,
              Prefix         => Index_Base_Name,
              Attribute_Name => Name_Succ,
              Expressions    => New_List (New_Occurrence_Of (W_J, Loc)));

         W_Increment  :=
           Make_OK_Assignment_Statement
             (Loc,
              Name       => New_Occurrence_Of (W_J, Loc),
              Expression => W_Index_Succ);

         Append_To (W_Body, W_Increment);

         Append_List_To (W_Body,
           Gen_Assign (New_Occurrence_Of (W_J, Loc), Expr, In_Loop => True));

         --  Construct the final loop

         Append_To (S,
           Make_Implicit_Loop_Statement
             (Node             => N,
              Identifier       => Empty,
              Iteration_Scheme => W_Iteration_Scheme,
              Statements       => W_Body));

         return S;
      end Gen_While;

      --------------------
      -- Get_Assoc_Expr --
      --------------------

      function Get_Assoc_Expr (Assoc : Node_Id) return Node_Id is
         Typ : constant Entity_Id := Base_Type (Etype (N));

      begin
         if Box_Present (Assoc) then
            if Is_Scalar_Type (Ctype) then
               if Present (Default_Aspect_Component_Value (Typ)) then
                  return Default_Aspect_Component_Value (Typ);
               elsif Present (Default_Aspect_Value (Ctype)) then
                  return Default_Aspect_Value (Ctype);
               else
                  return Empty;
               end if;

            else
               return Empty;
            end if;

         else
            return Expression (Assoc);
         end if;
      end Get_Assoc_Expr;

      ---------------------
      -- Index_Base_Name --
      ---------------------

      function Index_Base_Name return Node_Id is
      begin
         return New_Occurrence_Of (Index_Base, Sloc (N));
      end Index_Base_Name;

      ------------------------------------
      -- Local_Compile_Time_Known_Value --
      ------------------------------------

      function Local_Compile_Time_Known_Value (E : Node_Id) return Boolean is
      begin
         return Compile_Time_Known_Value (E)
           or else
             (Nkind (E) = N_Attribute_Reference
               and then Attribute_Name (E) = Name_Val
               and then Compile_Time_Known_Value (First (Expressions (E))));
      end Local_Compile_Time_Known_Value;

      ----------------------
      -- Local_Expr_Value --
      ----------------------

      function Local_Expr_Value (E : Node_Id) return Uint is
      begin
         if Compile_Time_Known_Value (E) then
            return Expr_Value (E);
         else
            return Expr_Value (First (Expressions (E)));
         end if;
      end Local_Expr_Value;

      --  Local variables

      New_Code : constant List_Id := New_List;

      Aggr_L : constant Node_Id := Low_Bound (Aggregate_Bounds (N));
      Aggr_H : constant Node_Id := High_Bound (Aggregate_Bounds (N));
      --  The aggregate bounds of this specific subaggregate. Note that if the
      --  code generated by Build_Array_Aggr_Code is executed then these bounds
      --  are OK. Otherwise a Constraint_Error would have been raised.

      Aggr_Low  : constant Node_Id := Duplicate_Subexpr_No_Checks (Aggr_L);
      Aggr_High : constant Node_Id := Duplicate_Subexpr_No_Checks (Aggr_H);
      --  After Duplicate_Subexpr these are side-effect free

      Assoc  : Node_Id;
      Choice : Node_Id;
      Expr   : Node_Id;
      High   : Node_Id;
      Low    : Node_Id;
      Typ    : Entity_Id;

      Nb_Choices : Nat := 0;
      Table      : Case_Table_Type (1 .. Number_Of_Choices (N));
      --  Used to sort all the different choice values

      Nb_Elements : Int;
      --  Number of elements in the positional aggregate

      Others_Assoc : Node_Id := Empty;

   --  Start of processing for Build_Array_Aggr_Code

   begin
      --  First before we start, a special case. if we have a bit packed
      --  array represented as a modular type, then clear the value to
      --  zero first, to ensure that unused bits are properly cleared.

      Typ := Etype (N);

      if Present (Typ)
        and then Is_Bit_Packed_Array (Typ)
        and then Is_Modular_Integer_Type (Packed_Array_Impl_Type (Typ))
      then
         Append_To (New_Code,
           Make_Assignment_Statement (Loc,
             Name       => New_Copy_Tree (Into),
             Expression =>
               Unchecked_Convert_To (Typ,
                 Make_Integer_Literal (Loc, Uint_0))));
      end if;

      --  If the component type contains tasks, we need to build a Master
      --  entity in the current scope, because it will be needed if build-
      --  in-place functions are called in the expanded code.

      if Nkind (Parent (N)) = N_Object_Declaration and then Has_Task (Typ) then
         Build_Master_Entity (Defining_Identifier (Parent (N)));
      end if;

      --  STEP 1: Process component associations

      --  For those associations that may generate a loop, initialize
      --  Loop_Actions to collect inserted actions that may be crated.

      --  Skip this if no component associations

      if No (Expressions (N)) then

         --  STEP 1 (a): Sort the discrete choices

         Assoc := First (Component_Associations (N));
         while Present (Assoc) loop
            Choice := First (Choice_List (Assoc));
            while Present (Choice) loop
               if Nkind (Choice) = N_Others_Choice then
                  Set_Loop_Actions (Assoc, New_List);
                  Others_Assoc := Assoc;
                  exit;
               end if;

               Get_Index_Bounds (Choice, Low, High);

               if Low /= High then
                  Set_Loop_Actions (Assoc, New_List);
               end if;

               Nb_Choices := Nb_Choices + 1;

               Table (Nb_Choices) :=
                  (Choice_Lo   => Low,
                   Choice_Hi   => High,
                   Choice_Node => Get_Assoc_Expr (Assoc));

               Next (Choice);
            end loop;

            Next (Assoc);
         end loop;

         --  If there is more than one set of choices these must be static
         --  and we can therefore sort them. Remember that Nb_Choices does not
         --  account for an others choice.

         if Nb_Choices > 1 then
            Sort_Case_Table (Table);
         end if;

         --  STEP 1 (b):  take care of the whole set of discrete choices

         for J in 1 .. Nb_Choices loop
            Low  := Table (J).Choice_Lo;
            High := Table (J).Choice_Hi;
            Expr := Table (J).Choice_Node;
            Append_List (Gen_Loop (Low, High, Expr), To => New_Code);
         end loop;

         --  STEP 1 (c): generate the remaining loops to cover others choice
         --  We don't need to generate loops over empty gaps, but if there is
         --  a single empty range we must analyze the expression for semantics

         if Present (Others_Assoc) then
            declare
               First : Boolean := True;

            begin
               for J in 0 .. Nb_Choices loop
                  if J = 0 then
                     Low := Aggr_Low;
                  else
                     Low := Add (1, To => Table (J).Choice_Hi);
                  end if;

                  if J = Nb_Choices then
                     High := Aggr_High;
                  else
                     High := Add (-1, To => Table (J + 1).Choice_Lo);
                  end if;

                  --  If this is an expansion within an init proc, make
                  --  sure that discriminant references are replaced by
                  --  the corresponding discriminal.

                  if Inside_Init_Proc then
                     if Is_Entity_Name (Low)
                       and then Ekind (Entity (Low)) = E_Discriminant
                     then
                        Set_Entity (Low, Discriminal (Entity (Low)));
                     end if;

                     if Is_Entity_Name (High)
                       and then Ekind (Entity (High)) = E_Discriminant
                     then
                        Set_Entity (High, Discriminal (Entity (High)));
                     end if;
                  end if;

                  if First
                    or else not Empty_Range (Low, High)
                  then
                     First := False;
                     Append_List
                       (Gen_Loop (Low, High,
                          Get_Assoc_Expr (Others_Assoc)), To => New_Code);
                  end if;
               end loop;
            end;
         end if;

      --  STEP 2: Process positional components

      else
         --  STEP 2 (a): Generate the assignments for each positional element
         --  Note that here we have to use Aggr_L rather than Aggr_Low because
         --  Aggr_L is analyzed and Add wants an analyzed expression.

         Expr        := First (Expressions (N));
         Nb_Elements := -1;
         while Present (Expr) loop
            Nb_Elements := Nb_Elements + 1;
            Append_List (Gen_Assign (Add (Nb_Elements, To => Aggr_L), Expr),
                         To => New_Code);
            Next (Expr);
         end loop;

         --  STEP 2 (b): Generate final loop if an others choice is present
         --  Here Nb_Elements gives the offset of the last positional element.

         if Present (Component_Associations (N)) then
            Assoc := Last (Component_Associations (N));

            --  Ada 2005 (AI-287)

            Append_List (Gen_While (Add (Nb_Elements, To => Aggr_L),
                                    Aggr_High,
                                    Get_Assoc_Expr (Assoc)), --  AI-287
                         To => New_Code);
         end if;
      end if;

      return New_Code;
   end Build_Array_Aggr_Code;

   ----------------------------
   -- Build_Record_Aggr_Code --
   ----------------------------

   function Build_Record_Aggr_Code
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id) return List_Id
   is
      Loc     : constant Source_Ptr := Sloc (N);
      L       : constant List_Id    := New_List;
      N_Typ   : constant Entity_Id  := Etype (N);

      Comp      : Node_Id;
      Instr     : Node_Id;
      Ref       : Node_Id;
      Target    : Entity_Id;
      Comp_Type : Entity_Id;
      Selector  : Entity_Id;
      Comp_Expr : Node_Id;
      Expr_Q    : Node_Id;

      --  If this is an internal aggregate, the External_Final_List is an
      --  expression for the controller record of the enclosing type.

      --  If the current aggregate has several controlled components, this
      --  expression will appear in several calls to attach to the finali-
      --  zation list, and it must not be shared.

      Ancestor_Is_Expression   : Boolean := False;
      Ancestor_Is_Subtype_Mark : Boolean := False;

      Init_Typ : Entity_Id := Empty;

      Finalization_Done : Boolean := False;
      --  True if Generate_Finalization_Actions has already been called; calls
      --  after the first do nothing.

      function Ancestor_Discriminant_Value (Disc : Entity_Id) return Node_Id;
      --  Returns the value that the given discriminant of an ancestor type
      --  should receive (in the absence of a conflict with the value provided
      --  by an ancestor part of an extension aggregate).

      procedure Check_Ancestor_Discriminants (Anc_Typ : Entity_Id);
      --  Check that each of the discriminant values defined by the ancestor
      --  part of an extension aggregate match the corresponding values
      --  provided by either an association of the aggregate or by the
      --  constraint imposed by a parent type (RM95-4.3.2(8)).

      function Compatible_Int_Bounds
        (Agg_Bounds : Node_Id;
         Typ_Bounds : Node_Id) return Boolean;
      --  Return true if Agg_Bounds are equal or within Typ_Bounds. It is
      --  assumed that both bounds are integer ranges.

      procedure Generate_Finalization_Actions;
      --  Deal with the various controlled type data structure initializations
      --  (but only if it hasn't been done already).

      function Get_Constraint_Association (T : Entity_Id) return Node_Id;
      --  Returns the first discriminant association in the constraint
      --  associated with T, if any, otherwise returns Empty.

      function Get_Explicit_Discriminant_Value (D : Entity_Id) return Node_Id;
      --  If the ancestor part is an unconstrained type and further ancestors
      --  do not provide discriminants for it, check aggregate components for
      --  values of the discriminants.

      procedure Init_Hidden_Discriminants (Typ : Entity_Id; List : List_Id);
      --  If Typ is derived, and constrains discriminants of the parent type,
      --  these discriminants are not components of the aggregate, and must be
      --  initialized. The assignments are appended to List. The same is done
      --  if Typ derives fron an already constrained subtype of a discriminated
      --  parent type.

      procedure Init_Stored_Discriminants;
      --  If the type is derived and has inherited discriminants, generate
      --  explicit assignments for each, using the store constraint of the
      --  type. Note that both visible and stored discriminants must be
      --  initialized in case the derived type has some renamed and some
      --  constrained discriminants.

      procedure Init_Visible_Discriminants;
      --  If type has discriminants, retrieve their values from aggregate,
      --  and generate explicit assignments for each. This does not include
      --  discriminants inherited from ancestor, which are handled above.
      --  The type of the aggregate is a subtype created ealier using the
      --  given values of the discriminant components of the aggregate.

      procedure Initialize_Ctrl_Record_Component
        (Rec_Comp  : Node_Id;
         Comp_Typ  : Entity_Id;
         Init_Expr : Node_Id;
         Stmts     : List_Id);
      --  Perform the initialization of controlled record component Rec_Comp.
      --  Comp_Typ is the component type. Init_Expr is the initialization
      --  expression for the record component. Hook-related declarations are
      --  inserted prior to aggregate N using Insert_Action. All remaining
      --  generated code is added to list Stmts.

      procedure Initialize_Record_Component
        (Rec_Comp  : Node_Id;
         Comp_Typ  : Entity_Id;
         Init_Expr : Node_Id;
         Stmts     : List_Id);
      --  Perform the initialization of record component Rec_Comp. Comp_Typ
      --  is the component type. Init_Expr is the initialization expression
      --  of the record component. All generated code is added to list Stmts.

      function Is_Int_Range_Bounds (Bounds : Node_Id) return Boolean;
      --  Check whether Bounds is a range node and its lower and higher bounds
      --  are integers literals.

      function Replace_Type (Expr : Node_Id) return Traverse_Result;
      --  If the aggregate contains a self-reference, traverse each expression
      --  to replace a possible self-reference with a reference to the proper
      --  component of the target of the assignment.

      function Rewrite_Discriminant (Expr : Node_Id) return Traverse_Result;
      --  If default expression of a component mentions a discriminant of the
      --  type, it must be rewritten as the discriminant of the target object.

      ---------------------------------
      -- Ancestor_Discriminant_Value --
      ---------------------------------

      function Ancestor_Discriminant_Value (Disc : Entity_Id) return Node_Id is
         Assoc        : Node_Id;
         Assoc_Elmt   : Elmt_Id;
         Aggr_Comp    : Entity_Id;
         Corresp_Disc : Entity_Id;
         Current_Typ  : Entity_Id := Base_Type (Typ);
         Parent_Typ   : Entity_Id;
         Parent_Disc  : Entity_Id;
         Save_Assoc   : Node_Id := Empty;

      begin
         --  First check any discriminant associations to see if any of them
         --  provide a value for the discriminant.

         if Present (Discriminant_Specifications (Parent (Current_Typ))) then
            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop
               Aggr_Comp := Entity (First (Choices (Assoc)));

               if Ekind (Aggr_Comp) = E_Discriminant then
                  Save_Assoc := Expression (Assoc);

                  Corresp_Disc := Corresponding_Discriminant (Aggr_Comp);
                  while Present (Corresp_Disc) loop

                     --  If found a corresponding discriminant then return the
                     --  value given in the aggregate. (Note: this is not
                     --  correct in the presence of side effects. ???)

                     if Disc = Corresp_Disc then
                        return Duplicate_Subexpr (Expression (Assoc));
                     end if;

                     Corresp_Disc := Corresponding_Discriminant (Corresp_Disc);
                  end loop;
               end if;

               Next (Assoc);
            end loop;
         end if;

         --  No match found in aggregate, so chain up parent types to find
         --  a constraint that defines the value of the discriminant.

         Parent_Typ := Etype (Current_Typ);
         while Current_Typ /= Parent_Typ loop
            if Has_Discriminants (Parent_Typ)
              and then not Has_Unknown_Discriminants (Parent_Typ)
            then
               Parent_Disc := First_Discriminant (Parent_Typ);

               --  We either get the association from the subtype indication
               --  of the type definition itself, or from the discriminant
               --  constraint associated with the type entity (which is
               --  preferable, but it's not always present ???)

               if Is_Empty_Elmt_List (Discriminant_Constraint (Current_Typ))
               then
                  Assoc := Get_Constraint_Association (Current_Typ);
                  Assoc_Elmt := No_Elmt;
               else
                  Assoc_Elmt :=
                    First_Elmt (Discriminant_Constraint (Current_Typ));
                  Assoc := Node (Assoc_Elmt);
               end if;

               --  Traverse the discriminants of the parent type looking
               --  for one that corresponds.

               while Present (Parent_Disc) and then Present (Assoc) loop
                  Corresp_Disc := Parent_Disc;
                  while Present (Corresp_Disc)
                    and then Disc /= Corresp_Disc
                  loop
                     Corresp_Disc := Corresponding_Discriminant (Corresp_Disc);
                  end loop;

                  if Disc = Corresp_Disc then
                     if Nkind (Assoc) = N_Discriminant_Association then
                        Assoc := Expression (Assoc);
                     end if;

                     --  If the located association directly denotes
                     --  a discriminant, then use the value of a saved
                     --  association of the aggregate. This is an approach
                     --  used to handle certain cases involving multiple
                     --  discriminants mapped to a single discriminant of
                     --  a descendant. It's not clear how to locate the
                     --  appropriate discriminant value for such cases. ???

                     if Is_Entity_Name (Assoc)
                       and then Ekind (Entity (Assoc)) = E_Discriminant
                     then
                        Assoc := Save_Assoc;
                     end if;

                     return Duplicate_Subexpr (Assoc);
                  end if;

                  Next_Discriminant (Parent_Disc);

                  if No (Assoc_Elmt) then
                     Next (Assoc);

                  else
                     Next_Elmt (Assoc_Elmt);

                     if Present (Assoc_Elmt) then
                        Assoc := Node (Assoc_Elmt);
                     else
                        Assoc := Empty;
                     end if;
                  end if;
               end loop;
            end if;

            Current_Typ := Parent_Typ;
            Parent_Typ := Etype (Current_Typ);
         end loop;

         --  In some cases there's no ancestor value to locate (such as
         --  when an ancestor part given by an expression defines the
         --  discriminant value).

         return Empty;
      end Ancestor_Discriminant_Value;

      ----------------------------------
      -- Check_Ancestor_Discriminants --
      ----------------------------------

      procedure Check_Ancestor_Discriminants (Anc_Typ : Entity_Id) is
         Discr      : Entity_Id;
         Disc_Value : Node_Id;
         Cond       : Node_Id;

      begin
         Discr := First_Discriminant (Base_Type (Anc_Typ));
         while Present (Discr) loop
            Disc_Value := Ancestor_Discriminant_Value (Discr);

            if Present (Disc_Value) then
               Cond := Make_Op_Ne (Loc,
                 Left_Opnd  =>
                   Make_Selected_Component (Loc,
                     Prefix        => New_Copy_Tree (Target),
                     Selector_Name => New_Occurrence_Of (Discr, Loc)),
                 Right_Opnd => Disc_Value);

               Append_To (L,
                 Make_Raise_Constraint_Error (Loc,
                   Condition => Cond,
                   Reason    => CE_Discriminant_Check_Failed));
            end if;

            Next_Discriminant (Discr);
         end loop;
      end Check_Ancestor_Discriminants;

      ---------------------------
      -- Compatible_Int_Bounds --
      ---------------------------

      function Compatible_Int_Bounds
        (Agg_Bounds : Node_Id;
         Typ_Bounds : Node_Id) return Boolean
      is
         Agg_Lo : constant Uint := Intval (Low_Bound  (Agg_Bounds));
         Agg_Hi : constant Uint := Intval (High_Bound (Agg_Bounds));
         Typ_Lo : constant Uint := Intval (Low_Bound  (Typ_Bounds));
         Typ_Hi : constant Uint := Intval (High_Bound (Typ_Bounds));
      begin
         return Typ_Lo <= Agg_Lo and then Agg_Hi <= Typ_Hi;
      end Compatible_Int_Bounds;

      -----------------------------------
      -- Generate_Finalization_Actions --
      -----------------------------------

      procedure Generate_Finalization_Actions is
      begin
         --  Do the work only the first time this is called

         if Finalization_Done then
            return;
         end if;

         Finalization_Done := True;

         --  Determine the external finalization list. It is either the
         --  finalization list of the outer scope or the one coming from an
         --  outer aggregate. When the target is not a temporary, the proper
         --  scope is the scope of the target rather than the potentially
         --  transient current scope.

         if Is_Controlled (Typ) and then Ancestor_Is_Subtype_Mark then
            Ref := Convert_To (Init_Typ, New_Copy_Tree (Target));
            Set_Assignment_OK (Ref);

            Append_To (L,
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of
                    (Find_Prim_Op (Init_Typ, Name_Initialize), Loc),
                Parameter_Associations => New_List (New_Copy_Tree (Ref))));
         end if;
      end Generate_Finalization_Actions;

      --------------------------------
      -- Get_Constraint_Association --
      --------------------------------

      function Get_Constraint_Association (T : Entity_Id) return Node_Id is
         Indic : Node_Id;
         Typ   : Entity_Id;

      begin
         Typ := T;

         --  If type is private, get constraint from full view. This was
         --  previously done in an instance context, but is needed whenever
         --  the ancestor part has a discriminant, possibly inherited through
         --  multiple derivations.

         if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
            Typ := Full_View (Typ);
         end if;

         Indic := Subtype_Indication (Type_Definition (Parent (Typ)));

         --  Verify that the subtype indication carries a constraint

         if Nkind (Indic) = N_Subtype_Indication
           and then Present (Constraint (Indic))
         then
            return First (Constraints (Constraint (Indic)));
         end if;

         return Empty;
      end Get_Constraint_Association;

      -------------------------------------
      -- Get_Explicit_Discriminant_Value --
      -------------------------------------

      function Get_Explicit_Discriminant_Value
        (D : Entity_Id) return Node_Id
      is
         Assoc  : Node_Id;
         Choice : Node_Id;
         Val    : Node_Id;

      begin
         --  The aggregate has been normalized and all associations have a
         --  single choice.

         Assoc := First (Component_Associations (N));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));

            if Chars (Choice) = Chars (D) then
               Val := Expression (Assoc);
               Remove (Assoc);
               return Val;
            end if;

            Next (Assoc);
         end loop;

         return Empty;
      end Get_Explicit_Discriminant_Value;

      -------------------------------
      -- Init_Hidden_Discriminants --
      -------------------------------

      procedure Init_Hidden_Discriminants (Typ : Entity_Id; List : List_Id) is
         function Is_Completely_Hidden_Discriminant
           (Discr : Entity_Id) return Boolean;
         --  Determine whether Discr is a completely hidden discriminant of
         --  type Typ.

         ---------------------------------------
         -- Is_Completely_Hidden_Discriminant --
         ---------------------------------------

         function Is_Completely_Hidden_Discriminant
           (Discr : Entity_Id) return Boolean
         is
            Item : Entity_Id;

         begin
            --  Use First/Next_Entity as First/Next_Discriminant do not yield
            --  completely hidden discriminants.

            Item := First_Entity (Typ);
            while Present (Item) loop
               if Ekind (Item) = E_Discriminant
                 and then Is_Completely_Hidden (Item)
                 and then Chars (Original_Record_Component (Item)) =
                          Chars (Discr)
               then
                  return True;
               end if;

               Next_Entity (Item);
            end loop;

            return False;
         end Is_Completely_Hidden_Discriminant;

         --  Local variables

         Base_Typ     : Entity_Id;
         Discr        : Entity_Id;
         Discr_Constr : Elmt_Id;
         Discr_Init   : Node_Id;
         Discr_Val    : Node_Id;
         In_Aggr_Type : Boolean;
         Par_Typ      : Entity_Id;

      --  Start of processing for Init_Hidden_Discriminants

      begin
         --  The constraints on the hidden discriminants, if present, are kept
         --  in the Stored_Constraint list of the type itself, or in that of
         --  the base type. If not in the constraints of the aggregate itself,
         --  we examine ancestors to find discriminants that are not renamed
         --  by other discriminants but constrained explicitly.

         In_Aggr_Type := True;

         Base_Typ := Base_Type (Typ);
         while Is_Derived_Type (Base_Typ)
           and then
             (Present (Stored_Constraint (Base_Typ))
               or else
                 (In_Aggr_Type and then Present (Stored_Constraint (Typ))))
         loop
            Par_Typ := Etype (Base_Typ);

            if not Has_Discriminants (Par_Typ) then
               return;
            end if;

            Discr := First_Discriminant (Par_Typ);

            --  We know that one of the stored-constraint lists is present

            if Present (Stored_Constraint (Base_Typ)) then
               Discr_Constr := First_Elmt (Stored_Constraint (Base_Typ));

            --  For private extension, stored constraint may be on full view

            elsif Is_Private_Type (Base_Typ)
              and then Present (Full_View (Base_Typ))
              and then Present (Stored_Constraint (Full_View (Base_Typ)))
            then
               Discr_Constr :=
                 First_Elmt (Stored_Constraint (Full_View (Base_Typ)));

            else
               Discr_Constr := First_Elmt (Stored_Constraint (Typ));
            end if;

            while Present (Discr) and then Present (Discr_Constr) loop
               Discr_Val := Node (Discr_Constr);

               --  The parent discriminant is renamed in the derived type,
               --  nothing to initialize.

               --    type Deriv_Typ (Discr : ...)
               --      is new Parent_Typ (Discr => Discr);

               if Is_Entity_Name (Discr_Val)
                 and then Ekind (Entity (Discr_Val)) = E_Discriminant
               then
                  null;

               --  When the parent discriminant is constrained at the type
               --  extension level, it does not appear in the derived type.

               --    type Deriv_Typ (Discr : ...)
               --      is new Parent_Typ (Discr        => Discr,
               --                         Hidden_Discr => Expression);

               elsif Is_Completely_Hidden_Discriminant (Discr) then
                  null;

               --  Otherwise initialize the discriminant

               else
                  Discr_Init :=
                    Make_OK_Assignment_Statement (Loc,
                      Name       =>
                        Make_Selected_Component (Loc,
                          Prefix        => New_Copy_Tree (Target),
                          Selector_Name => New_Occurrence_Of (Discr, Loc)),
                      Expression => New_Copy_Tree (Discr_Val));

                  Append_To (List, Discr_Init);
               end if;

               Next_Elmt (Discr_Constr);
               Next_Discriminant (Discr);
            end loop;

            In_Aggr_Type := False;
            Base_Typ := Base_Type (Par_Typ);
         end loop;
      end Init_Hidden_Discriminants;

      --------------------------------
      -- Init_Visible_Discriminants --
      --------------------------------

      procedure Init_Visible_Discriminants is
         Discriminant       : Entity_Id;
         Discriminant_Value : Node_Id;

      begin
         Discriminant := First_Discriminant (Typ);
         while Present (Discriminant) loop
            Comp_Expr :=
              Make_Selected_Component (Loc,
                Prefix        => New_Copy_Tree (Target),
                Selector_Name => New_Occurrence_Of (Discriminant, Loc));

            Discriminant_Value :=
              Get_Discriminant_Value
                (Discriminant, Typ, Discriminant_Constraint (N_Typ));

            Instr :=
              Make_OK_Assignment_Statement (Loc,
                Name       => Comp_Expr,
                Expression => New_Copy_Tree (Discriminant_Value));

            Append_To (L, Instr);

            Next_Discriminant (Discriminant);
         end loop;
      end Init_Visible_Discriminants;

      -------------------------------
      -- Init_Stored_Discriminants --
      -------------------------------

      procedure Init_Stored_Discriminants is
         Discriminant       : Entity_Id;
         Discriminant_Value : Node_Id;

      begin
         Discriminant := First_Stored_Discriminant (Typ);
         while Present (Discriminant) loop
            Comp_Expr :=
              Make_Selected_Component (Loc,
                Prefix        => New_Copy_Tree (Target),
                Selector_Name => New_Occurrence_Of (Discriminant, Loc));

            Discriminant_Value :=
              Get_Discriminant_Value
                (Discriminant, N_Typ, Discriminant_Constraint (N_Typ));

            Instr :=
              Make_OK_Assignment_Statement (Loc,
                Name       => Comp_Expr,
                Expression => New_Copy_Tree (Discriminant_Value));

            Append_To (L, Instr);

            Next_Stored_Discriminant (Discriminant);
         end loop;
      end Init_Stored_Discriminants;

      --------------------------------------
      -- Initialize_Ctrl_Record_Component --
      --------------------------------------

      procedure Initialize_Ctrl_Record_Component
        (Rec_Comp  : Node_Id;
         Comp_Typ  : Entity_Id;
         Init_Expr : Node_Id;
         Stmts     : List_Id)
      is
         Fin_Call   : Node_Id;
         Hook_Clear : Node_Id;

         In_Place_Expansion : Boolean;
         --  Flag set when a nonlimited controlled function call requires
         --  in-place expansion.

      begin
         --  Perform a preliminary analysis and resolution to determine what
         --  the initialization expression denotes. Unanalyzed function calls
         --  may appear as identifiers or indexed components.

         if Nkind_In (Init_Expr, N_Function_Call,
                                 N_Identifier,
                                 N_Indexed_Component)
           and then not Analyzed (Init_Expr)
         then
            Preanalyze_And_Resolve (Init_Expr, Comp_Typ);
         end if;

         In_Place_Expansion :=
           Nkind (Init_Expr) = N_Function_Call
             and then not Is_Build_In_Place_Result_Type (Comp_Typ);

         --  The initialization expression is a controlled function call.
         --  Perform in-place removal of side effects to avoid creating a
         --  transient scope.

         --  This in-place expansion is not performed for limited transient
         --  objects because the initialization is already done in place.

         if In_Place_Expansion then

            --  Suppress the removal of side effects by general analysis
            --  because this behavior is emulated here. This avoids the
            --  generation of a transient scope, which leads to out-of-order
            --  adjustment and finalization.

            Set_No_Side_Effect_Removal (Init_Expr);

            --  Install all hook-related declarations and prepare the clean up
            --  statements. The generated code follows the initialization order
            --  of individual components and discriminants, rather than being
            --  inserted prior to the aggregate. This ensures that a transient
            --  component which mentions a discriminant has proper visibility
            --  of the discriminant.

            Process_Transient_Component
              (Loc        => Loc,
               Comp_Typ   => Comp_Typ,
               Init_Expr  => Init_Expr,
               Fin_Call   => Fin_Call,
               Hook_Clear => Hook_Clear,
               Stmts      => Stmts);
         end if;

         --  Use the noncontrolled component initialization circuitry to
         --  assign the result of the function call to the record component.
         --  This also performs tag adjustment and [deep] adjustment of the
         --  record component.

         Initialize_Record_Component
           (Rec_Comp  => Rec_Comp,
            Comp_Typ  => Comp_Typ,
            Init_Expr => Init_Expr,
            Stmts     => Stmts);

         --  At this point the record component is fully initialized. Complete
         --  the processing of the controlled record component by finalizing
         --  the transient function result.

         if In_Place_Expansion then
            Process_Transient_Component_Completion
              (Loc        => Loc,
               Aggr       => N,
               Fin_Call   => Fin_Call,
               Hook_Clear => Hook_Clear,
               Stmts      => Stmts);
         end if;
      end Initialize_Ctrl_Record_Component;

      ---------------------------------
      -- Initialize_Record_Component --
      ---------------------------------

      procedure Initialize_Record_Component
        (Rec_Comp  : Node_Id;
         Comp_Typ  : Entity_Id;
         Init_Expr : Node_Id;
         Stmts     : List_Id)
      is
         Exceptions_OK : constant Boolean :=
                           not Restriction_Active (No_Exception_Propagation);

         Finalization_OK : constant Boolean := Needs_Finalization (Comp_Typ);

         Full_Typ  : constant Entity_Id := Underlying_Type (Comp_Typ);
         Adj_Call  : Node_Id;
         Blk_Stmts : List_Id;
         Init_Stmt : Node_Id;

      begin
         --  Protect the initialization statements from aborts. Generate:

         --    Abort_Defer;

         if Finalization_OK and Abort_Allowed then
            if Exceptions_OK then
               Blk_Stmts := New_List;
            else
               Blk_Stmts := Stmts;
            end if;

            Append_To (Blk_Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));

         --  Otherwise aborts are not allowed. All generated code is added
         --  directly to the input list.

         else
            Blk_Stmts := Stmts;
         end if;

         --  Initialize the record component. Generate:

         --    Rec_Comp := Init_Expr;

         --  Note that the initialization expression is NOT replicated because
         --  only a single component may be initialized by it.

         Init_Stmt :=
           Make_OK_Assignment_Statement (Loc,
             Name       => New_Copy_Tree (Rec_Comp),
             Expression => Init_Expr);
         Set_No_Ctrl_Actions (Init_Stmt);

         Append_To (Blk_Stmts, Init_Stmt);

         --  Adjust the tag due to a possible view conversion. Generate:

         --    Rec_Comp._tag := Full_TypeP;

         if Tagged_Type_Expansion and then Is_Tagged_Type (Comp_Typ) then
            Append_To (Blk_Stmts,
              Make_OK_Assignment_Statement (Loc,
                Name       =>
                  Make_Selected_Component (Loc,
                    Prefix        => New_Copy_Tree (Rec_Comp),
                    Selector_Name =>
                      New_Occurrence_Of
                        (First_Tag_Component (Full_Typ), Loc)),

                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Occurrence_Of
                      (Node (First_Elmt (Access_Disp_Table (Full_Typ))),
                       Loc))));
         end if;

         --  Adjust the component. Generate:

         --    [Deep_]Adjust (Rec_Comp);

         if Finalization_OK
           and then not Is_Limited_Type (Comp_Typ)
           and then not Is_Build_In_Place_Function_Call (Init_Expr)
         then
            Adj_Call :=
              Make_Adjust_Call
                (Obj_Ref => New_Copy_Tree (Rec_Comp),
                 Typ     => Comp_Typ);

            --  Guard against a missing [Deep_]Adjust when the component type
            --  was not properly frozen.

            if Present (Adj_Call) then
               Append_To (Blk_Stmts, Adj_Call);
            end if;
         end if;

         --  Complete the protection of the initialization statements

         if Finalization_OK and Abort_Allowed then

            --  Wrap the initialization statements in a block to catch a
            --  potential exception. Generate:

            --    begin
            --       Abort_Defer;
            --       Rec_Comp := Init_Expr;
            --       Rec_Comp._tag := Full_TypP;
            --       [Deep_]Adjust (Rec_Comp);
            --    at end
            --       Abort_Undefer_Direct;
            --    end;

            if Exceptions_OK then
               Append_To (Stmts,
                 Build_Abort_Undefer_Block (Loc,
                   Stmts   => Blk_Stmts,
                   Context => N));

            --  Otherwise exceptions are not propagated. Generate:

            --    Abort_Defer;
            --    Rec_Comp := Init_Expr;
            --    Rec_Comp._tag := Full_TypP;
            --    [Deep_]Adjust (Rec_Comp);
            --    Abort_Undefer;

            else
               Append_To (Blk_Stmts,
                 Build_Runtime_Call (Loc, RE_Abort_Undefer));
            end if;
         end if;
      end Initialize_Record_Component;

      -------------------------
      -- Is_Int_Range_Bounds --
      -------------------------

      function Is_Int_Range_Bounds (Bounds : Node_Id) return Boolean is
      begin
         return Nkind (Bounds) = N_Range
           and then Nkind (Low_Bound  (Bounds)) = N_Integer_Literal
           and then Nkind (High_Bound (Bounds)) = N_Integer_Literal;
      end Is_Int_Range_Bounds;

      ------------------
      -- Replace_Type --
      ------------------

      function Replace_Type (Expr : Node_Id) return Traverse_Result is
      begin
         --  Note regarding the Root_Type test below: Aggregate components for
         --  self-referential types include attribute references to the current
         --  instance, of the form: Typ'access, etc.. These references are
         --  rewritten as references to the target of the aggregate: the
         --  left-hand side of an assignment, the entity in a declaration,
         --  or a temporary. Without this test, we would improperly extended
         --  this rewriting to attribute references whose prefix was not the
         --  type of the aggregate.

         if Nkind (Expr) = N_Attribute_Reference
           and then Is_Entity_Name (Prefix (Expr))
           and then Is_Type (Entity (Prefix (Expr)))
           and then Root_Type (Etype (N)) = Root_Type (Entity (Prefix (Expr)))
         then
            if Is_Entity_Name (Lhs) then
               Rewrite (Prefix (Expr), New_Occurrence_Of (Entity (Lhs), Loc));

            else
               Rewrite (Expr,
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Unrestricted_Access,
                   Prefix         => New_Copy_Tree (Lhs)));
               Set_Analyzed (Parent (Expr), False);
            end if;
         end if;

         return OK;
      end Replace_Type;

      --------------------------
      -- Rewrite_Discriminant --
      --------------------------

      function Rewrite_Discriminant (Expr : Node_Id) return Traverse_Result is
      begin
         if Is_Entity_Name (Expr)
           and then Present (Entity (Expr))
           and then Ekind (Entity (Expr)) = E_In_Parameter
           and then Present (Discriminal_Link (Entity (Expr)))
           and then Scope (Discriminal_Link (Entity (Expr))) =
                                                       Base_Type (Etype (N))
         then
            Rewrite (Expr,
              Make_Selected_Component (Loc,
                Prefix        => New_Copy_Tree (Lhs),
                Selector_Name => Make_Identifier (Loc, Chars (Expr))));
         end if;

         return OK;
      end Rewrite_Discriminant;

      procedure Replace_Discriminants is
        new Traverse_Proc (Rewrite_Discriminant);

      procedure Replace_Self_Reference is
        new Traverse_Proc (Replace_Type);

   --  Start of processing for Build_Record_Aggr_Code

   begin
      if Has_Self_Reference (N) then
         Replace_Self_Reference (N);
      end if;

      --  If the target of the aggregate is class-wide, we must convert it
      --  to the actual type of the aggregate, so that the proper components
      --  are visible. We know already that the types are compatible.

      if Present (Etype (Lhs))
        and then Is_Class_Wide_Type (Etype (Lhs))
      then
         Target := Unchecked_Convert_To (Typ, Lhs);
      else
         Target := Lhs;
      end if;

      --  Deal with the ancestor part of extension aggregates or with the
      --  discriminants of the root type.

      if Nkind (N) = N_Extension_Aggregate then
         declare
            Ancestor : constant Node_Id := Ancestor_Part (N);
            Adj_Call : Node_Id;
            Assign   : List_Id;

         begin
            --  If the ancestor part is a subtype mark "T", we generate

            --     init-proc (T (tmp));  if T is constrained and
            --     init-proc (S (tmp));  where S applies an appropriate
            --                           constraint if T is unconstrained

            if Is_Entity_Name (Ancestor)
              and then Is_Type (Entity (Ancestor))
            then
               Ancestor_Is_Subtype_Mark := True;

               if Is_Constrained (Entity (Ancestor)) then
                  Init_Typ := Entity (Ancestor);

               --  For an ancestor part given by an unconstrained type mark,
               --  create a subtype constrained by appropriate corresponding
               --  discriminant values coming from either associations of the
               --  aggregate or a constraint on a parent type. The subtype will
               --  be used to generate the correct default value for the
               --  ancestor part.

               elsif Has_Discriminants (Entity (Ancestor)) then
                  declare
                     Anc_Typ    : constant Entity_Id := Entity (Ancestor);
                     Anc_Constr : constant List_Id   := New_List;
                     Discrim    : Entity_Id;
                     Disc_Value : Node_Id;
                     New_Indic  : Node_Id;
                     Subt_Decl  : Node_Id;

                  begin
                     Discrim := First_Discriminant (Anc_Typ);
                     while Present (Discrim) loop
                        Disc_Value := Ancestor_Discriminant_Value (Discrim);

                        --  If no usable discriminant in ancestors, check
                        --  whether aggregate has an explicit value for it.

                        if No (Disc_Value) then
                           Disc_Value :=
                             Get_Explicit_Discriminant_Value (Discrim);
                        end if;

                        Append_To (Anc_Constr, Disc_Value);
                        Next_Discriminant (Discrim);
                     end loop;

                     New_Indic :=
                       Make_Subtype_Indication (Loc,
                         Subtype_Mark => New_Occurrence_Of (Anc_Typ, Loc),
                         Constraint   =>
                           Make_Index_Or_Discriminant_Constraint (Loc,
                             Constraints => Anc_Constr));

                     Init_Typ := Create_Itype (Ekind (Anc_Typ), N);

                     Subt_Decl :=
                       Make_Subtype_Declaration (Loc,
                         Defining_Identifier => Init_Typ,
                         Subtype_Indication  => New_Indic);

                     --  Itypes must be analyzed with checks off Declaration
                     --  must have a parent for proper handling of subsidiary
                     --  actions.

                     Set_Parent (Subt_Decl, N);
                     Analyze (Subt_Decl, Suppress => All_Checks);
                  end;
               end if;

               Ref := Convert_To (Init_Typ, New_Copy_Tree (Target));
               Set_Assignment_OK (Ref);

               if not Is_Interface (Init_Typ) then
                  Append_List_To (L,
                    Build_Initialization_Call (Loc,
                      Id_Ref            => Ref,
                      Typ               => Init_Typ,
                      In_Init_Proc      => Within_Init_Proc,
                      With_Default_Init => Has_Default_Init_Comps (N)
                                             or else
                                           Has_Task (Base_Type (Init_Typ))));

                  if Is_Constrained (Entity (Ancestor))
                    and then Has_Discriminants (Entity (Ancestor))
                  then
                     Check_Ancestor_Discriminants (Entity (Ancestor));
                  end if;
               end if;

            --  Handle calls to C++ constructors

            elsif Is_CPP_Constructor_Call (Ancestor) then
               Init_Typ := Etype (Ancestor);
               Ref := Convert_To (Init_Typ, New_Copy_Tree (Target));
               Set_Assignment_OK (Ref);

               Append_List_To (L,
                 Build_Initialization_Call (Loc,
                   Id_Ref            => Ref,
                   Typ               => Init_Typ,
                   In_Init_Proc      => Within_Init_Proc,
                   With_Default_Init => Has_Default_Init_Comps (N),
                   Constructor_Ref   => Ancestor));

            --  Ada 2005 (AI-287): If the ancestor part is an aggregate of
            --  limited type, a recursive call expands the ancestor. Note that
            --  in the limited case, the ancestor part must be either a
            --  function call (possibly qualified) or aggregate (definitely
            --  qualified).

            elsif Is_Limited_Type (Etype (Ancestor))
              and then Nkind_In (Unqualify (Ancestor), N_Aggregate,
                                                       N_Extension_Aggregate)
            then
               Ancestor_Is_Expression := True;

               --  Set up finalization data for enclosing record, because
               --  controlled subcomponents of the ancestor part will be
               --  attached to it.

               Generate_Finalization_Actions;

               Append_List_To (L,
                  Build_Record_Aggr_Code
                    (N   => Unqualify (Ancestor),
                     Typ => Etype (Unqualify (Ancestor)),
                     Lhs => Target));

            --  If the ancestor part is an expression "E", we generate

            --     T (tmp) := E;

            --  In Ada 2005, this includes the case of a (possibly qualified)
            --  limited function call. The assignment will turn into a
            --  build-in-place function call (for further details, see
            --  Make_Build_In_Place_Call_In_Assignment).

            else
               Ancestor_Is_Expression := True;
               Init_Typ := Etype (Ancestor);

               --  If the ancestor part is an aggregate, force its full
               --  expansion, which was delayed.

               if Nkind_In (Unqualify (Ancestor), N_Aggregate,
                                                  N_Extension_Aggregate)
               then
                  Set_Analyzed (Ancestor, False);
                  Set_Analyzed (Expression (Ancestor), False);
               end if;

               Ref := Convert_To (Init_Typ, New_Copy_Tree (Target));
               Set_Assignment_OK (Ref);

               --  Make the assignment without usual controlled actions, since
               --  we only want to Adjust afterwards, but not to Finalize
               --  beforehand. Add manual Adjust when necessary.

               Assign := New_List (
                 Make_OK_Assignment_Statement (Loc,
                   Name       => Ref,
                   Expression => Ancestor));
               Set_No_Ctrl_Actions (First (Assign));

               --  Assign the tag now to make sure that the dispatching call in
               --  the subsequent deep_adjust works properly (unless
               --  Tagged_Type_Expansion where tags are implicit).

               if Tagged_Type_Expansion then
                  Instr :=
                    Make_OK_Assignment_Statement (Loc,
                      Name       =>
                        Make_Selected_Component (Loc,
                          Prefix        => New_Copy_Tree (Target),
                          Selector_Name =>
                            New_Occurrence_Of
                              (First_Tag_Component (Base_Type (Typ)), Loc)),

                      Expression =>
                        Unchecked_Convert_To (RTE (RE_Tag),
                          New_Occurrence_Of
                            (Node (First_Elmt
                               (Access_Disp_Table (Base_Type (Typ)))),
                             Loc)));

                  Set_Assignment_OK (Name (Instr));
                  Append_To (Assign, Instr);

                  --  Ada 2005 (AI-251): If tagged type has progenitors we must
                  --  also initialize tags of the secondary dispatch tables.

                  if Has_Interfaces (Base_Type (Typ)) then
                     Init_Secondary_Tags
                       (Typ            => Base_Type (Typ),
                        Target         => Target,
                        Stmts_List     => Assign,
                        Init_Tags_List => Assign);
                  end if;
               end if;

               --  Call Adjust manually

               if Needs_Finalization (Etype (Ancestor))
                 and then not Is_Limited_Type (Etype (Ancestor))
                 and then not Is_Build_In_Place_Function_Call (Ancestor)
               then
                  Adj_Call :=
                    Make_Adjust_Call
                      (Obj_Ref => New_Copy_Tree (Ref),
                       Typ     => Etype (Ancestor));

                  --  Guard against a missing [Deep_]Adjust when the ancestor
                  --  type was not properly frozen.

                  if Present (Adj_Call) then
                     Append_To (Assign, Adj_Call);
                  end if;
               end if;

               Append_To (L,
                 Make_Unsuppress_Block (Loc, Name_Discriminant_Check, Assign));

               if Has_Discriminants (Init_Typ) then
                  Check_Ancestor_Discriminants (Init_Typ);
               end if;
            end if;

            pragma Assert (Nkind (N) = N_Extension_Aggregate);
            pragma Assert
              (not (Ancestor_Is_Expression and Ancestor_Is_Subtype_Mark));
         end;

         --  Generate assignments of hidden discriminants. If the base type is
         --  an unchecked union, the discriminants are unknown to the back-end
         --  and absent from a value of the type, so assignments for them are
         --  not emitted.

         if Has_Discriminants (Typ)
           and then not Is_Unchecked_Union (Base_Type (Typ))
         then
            Init_Hidden_Discriminants (Typ, L);
         end if;

      --  Normal case (not an extension aggregate)

      else
         --  Generate the discriminant expressions, component by component.
         --  If the base type is an unchecked union, the discriminants are
         --  unknown to the back-end and absent from a value of the type, so
         --  assignments for them are not emitted.

         if Has_Discriminants (Typ)
           and then not Is_Unchecked_Union (Base_Type (Typ))
         then
            Init_Hidden_Discriminants (Typ, L);

            --  Generate discriminant init values for the visible discriminants

            Init_Visible_Discriminants;

            if Is_Derived_Type (N_Typ) then
               Init_Stored_Discriminants;
            end if;
         end if;
      end if;

      --  For CPP types we generate an implicit call to the C++ default
      --  constructor to ensure the proper initialization of the _Tag
      --  component.

      if Is_CPP_Class (Root_Type (Typ)) and then CPP_Num_Prims (Typ) > 0 then
         Invoke_Constructor : declare
            CPP_Parent : constant Entity_Id := Enclosing_CPP_Parent (Typ);

            procedure Invoke_IC_Proc (T : Entity_Id);
            --  Recursive routine used to climb to parents. Required because
            --  parents must be initialized before descendants to ensure
            --  propagation of inherited C++ slots.

            --------------------
            -- Invoke_IC_Proc --
            --------------------

            procedure Invoke_IC_Proc (T : Entity_Id) is
            begin
               --  Avoid generating extra calls. Initialization required
               --  only for types defined from the level of derivation of
               --  type of the constructor and the type of the aggregate.

               if T = CPP_Parent then
                  return;
               end if;

               Invoke_IC_Proc (Etype (T));

               --  Generate call to the IC routine

               if Present (CPP_Init_Proc (T)) then
                  Append_To (L,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Occurrence_Of (CPP_Init_Proc (T), Loc)));
               end if;
            end Invoke_IC_Proc;

         --  Start of processing for Invoke_Constructor

         begin
            --  Implicit invocation of the C++ constructor

            if Nkind (N) = N_Aggregate then
               Append_To (L,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (Base_Init_Proc (CPP_Parent), Loc),
                   Parameter_Associations => New_List (
                     Unchecked_Convert_To (CPP_Parent,
                       New_Copy_Tree (Lhs)))));
            end if;

            Invoke_IC_Proc (Typ);
         end Invoke_Constructor;
      end if;

      --  Generate the assignments, component by component

      --    tmp.comp1 := Expr1_From_Aggr;
      --    tmp.comp2 := Expr2_From_Aggr;
      --    ....

      Comp := First (Component_Associations (N));
      while Present (Comp) loop
         Selector := Entity (First (Choices (Comp)));

         --  C++ constructors

         if Is_CPP_Constructor_Call (Expression (Comp)) then
            Append_List_To (L,
              Build_Initialization_Call (Loc,
                Id_Ref            =>
                  Make_Selected_Component (Loc,
                    Prefix        => New_Copy_Tree (Target),
                    Selector_Name => New_Occurrence_Of (Selector, Loc)),
                Typ               => Etype (Selector),
                Enclos_Type       => Typ,
                With_Default_Init => True,
                Constructor_Ref   => Expression (Comp)));

         --  Ada 2005 (AI-287): For each default-initialized component generate
         --  a call to the corresponding IP subprogram if available.

         elsif Box_Present (Comp)
           and then Has_Non_Null_Base_Init_Proc (Etype (Selector))
         then
            if Ekind (Selector) /= E_Discriminant then
               Generate_Finalization_Actions;
            end if;

            --  Ada 2005 (AI-287): If the component type has tasks then
            --  generate the activation chain and master entities (except
            --  in case of an allocator because in that case these entities
            --  are generated by Build_Task_Allocate_Block_With_Init_Stmts).

            declare
               Ctype            : constant Entity_Id := Etype (Selector);
               Inside_Allocator : Boolean            := False;
               P                : Node_Id            := Parent (N);

            begin
               if Is_Task_Type (Ctype) or else Has_Task (Ctype) then
                  while Present (P) loop
                     if Nkind (P) = N_Allocator then
                        Inside_Allocator := True;
                        exit;
                     end if;

                     P := Parent (P);
                  end loop;

                  if not Inside_Init_Proc and not Inside_Allocator then
                     Build_Activation_Chain_Entity (N);
                  end if;
               end if;
            end;

            Append_List_To (L,
              Build_Initialization_Call (Loc,
                Id_Ref            => Make_Selected_Component (Loc,
                                       Prefix        => New_Copy_Tree (Target),
                                       Selector_Name =>
                                         New_Occurrence_Of (Selector, Loc)),
                Typ               => Etype (Selector),
                Enclos_Type       => Typ,
                With_Default_Init => True));

         --  Prepare for component assignment

         elsif Ekind (Selector) /= E_Discriminant
           or else Nkind (N) = N_Extension_Aggregate
         then
            --  All the discriminants have now been assigned

            --  This is now a good moment to initialize and attach all the
            --  controllers. Their position may depend on the discriminants.

            if Ekind (Selector) /= E_Discriminant then
               Generate_Finalization_Actions;
            end if;

            Comp_Type := Underlying_Type (Etype (Selector));
            Comp_Expr :=
              Make_Selected_Component (Loc,
                Prefix        => New_Copy_Tree (Target),
                Selector_Name => New_Occurrence_Of (Selector, Loc));

            if Nkind (Expression (Comp)) = N_Qualified_Expression then
               Expr_Q := Expression (Expression (Comp));
            else
               Expr_Q := Expression (Comp);
            end if;

            --  Now either create the assignment or generate the code for the
            --  inner aggregate top-down.

            if Is_Delayed_Aggregate (Expr_Q) then

               --  We have the following case of aggregate nesting inside
               --  an object declaration:

               --    type Arr_Typ is array (Integer range <>) of ...;

               --    type Rec_Typ (...) is record
               --       Obj_Arr_Typ : Arr_Typ (A .. B);
               --    end record;

               --    Obj_Rec_Typ : Rec_Typ := (...,
               --      Obj_Arr_Typ => (X => (...), Y => (...)));

               --  The length of the ranges of the aggregate and Obj_Add_Typ
               --  are equal (B - A = Y - X), but they do not coincide (X /=
               --  A and B /= Y). This case requires array sliding which is
               --  performed in the following manner:

               --    subtype Arr_Sub is Arr_Typ (X .. Y);
               --    Temp : Arr_Sub;
               --    Temp (X) := (...);
               --    ...
               --    Temp (Y) := (...);
               --    Obj_Rec_Typ.Obj_Arr_Typ := Temp;

               if Ekind (Comp_Type) = E_Array_Subtype
                 and then Is_Int_Range_Bounds (Aggregate_Bounds (Expr_Q))
                 and then Is_Int_Range_Bounds (First_Index (Comp_Type))
                 and then not
                   Compatible_Int_Bounds
                     (Agg_Bounds => Aggregate_Bounds (Expr_Q),
                      Typ_Bounds => First_Index (Comp_Type))
               then
                  --  Create the array subtype with bounds equal to those of
                  --  the corresponding aggregate.

                  declare
                     SubE : constant Entity_Id := Make_Temporary (Loc, 'T');

                     SubD : constant Node_Id :=
                       Make_Subtype_Declaration (Loc,
                         Defining_Identifier => SubE,
                         Subtype_Indication  =>
                           Make_Subtype_Indication (Loc,
                             Subtype_Mark =>
                               New_Occurrence_Of (Etype (Comp_Type), Loc),
                             Constraint =>
                               Make_Index_Or_Discriminant_Constraint
                                 (Loc,
                                  Constraints => New_List (
                                    New_Copy_Tree
                                      (Aggregate_Bounds (Expr_Q))))));

                     --  Create a temporary array of the above subtype which
                     --  will be used to capture the aggregate assignments.

                     TmpE : constant Entity_Id := Make_Temporary (Loc, 'A', N);

                     TmpD : constant Node_Id :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => TmpE,
                         Object_Definition   => New_Occurrence_Of (SubE, Loc));

                  begin
                     Set_No_Initialization (TmpD);
                     Append_To (L, SubD);
                     Append_To (L, TmpD);

                     --  Expand aggregate into assignments to the temp array

                     Append_List_To (L,
                       Late_Expansion (Expr_Q, Comp_Type,
                         New_Occurrence_Of (TmpE, Loc)));

                     --  Slide

                     Append_To (L,
                       Make_Assignment_Statement (Loc,
                         Name       => New_Copy_Tree (Comp_Expr),
                         Expression => New_Occurrence_Of (TmpE, Loc)));
                  end;

               --  Normal case (sliding not required)

               else
                  Append_List_To (L,
                    Late_Expansion (Expr_Q, Comp_Type, Comp_Expr));
               end if;

            --  Expr_Q is not delayed aggregate

            else
               if Has_Discriminants (Typ) then
                  Replace_Discriminants (Expr_Q);

                  --  If the component is an array type that depends on
                  --  discriminants, and the expression is a single Others
                  --  clause, create an explicit subtype for it because the
                  --  backend has troubles recovering the actual bounds.

                  if Nkind (Expr_Q) = N_Aggregate
                    and then Is_Array_Type (Comp_Type)
                    and then Present (Component_Associations (Expr_Q))
                  then
                     declare
                        Assoc : constant Node_Id :=
                                  First (Component_Associations (Expr_Q));
                        Decl  : Node_Id;

                     begin
                        if Nkind (First (Choices (Assoc))) = N_Others_Choice
                        then
                           Decl :=
                             Build_Actual_Subtype_Of_Component
                               (Comp_Type, Comp_Expr);

                           --  If the component type does not in fact depend on
                           --  discriminants, the subtype declaration is empty.

                           if Present (Decl) then
                              Append_To (L, Decl);
                              Set_Etype (Comp_Expr, Defining_Entity (Decl));
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               if Modify_Tree_For_C
                 and then Nkind (Expr_Q) = N_Aggregate
                 and then Is_Array_Type (Etype (Expr_Q))
                 and then Present (First_Index (Etype (Expr_Q)))
               then
                  declare
                     Expr_Q_Type : constant Node_Id := Etype (Expr_Q);
                  begin
                     Append_List_To (L,
                       Build_Array_Aggr_Code
                         (N           => Expr_Q,
                          Ctype       => Component_Type (Expr_Q_Type),
                          Index       => First_Index (Expr_Q_Type),
                          Into        => Comp_Expr,
                          Scalar_Comp =>
                            Is_Scalar_Type (Component_Type (Expr_Q_Type))));
                  end;

               else
                  --  Handle an initialization expression of a controlled type
                  --  in case it denotes a function call. In general such a
                  --  scenario will produce a transient scope, but this will
                  --  lead to wrong order of initialization, adjustment, and
                  --  finalization in the context of aggregates.

                  --    Target.Comp := Ctrl_Func_Call;

                  --    begin                                  --  scope
                  --       Trans_Obj : ... := Ctrl_Func_Call;  --  object
                  --       Target.Comp := Trans_Obj;
                  --       Finalize (Trans_Obj);
                  --    end
                  --    Target.Comp._tag := ...;
                  --    Adjust (Target.Comp);

                  --  In the example above, the call to Finalize occurs too
                  --  early and as a result it may leave the record component
                  --  in a bad state. Finalization of the transient object
                  --  should really happen after adjustment.

                  --  To avoid this scenario, perform in-place side-effect
                  --  removal of the function call. This eliminates the
                  --  transient property of the function result and ensures
                  --  correct order of actions.

                  --    Res : ... := Ctrl_Func_Call;
                  --    Target.Comp := Res;
                  --    Target.Comp._tag := ...;
                  --    Adjust (Target.Comp);
                  --    Finalize (Res);

                  if Needs_Finalization (Comp_Type)
                    and then Nkind (Expr_Q) /= N_Aggregate
                  then
                     Initialize_Ctrl_Record_Component
                       (Rec_Comp   => Comp_Expr,
                        Comp_Typ   => Etype (Selector),
                        Init_Expr  => Expr_Q,
                        Stmts      => L);

                  --  Otherwise perform single component initialization

                  else
                     Initialize_Record_Component
                       (Rec_Comp  => Comp_Expr,
                        Comp_Typ  => Etype (Selector),
                        Init_Expr => Expr_Q,
                        Stmts     => L);
                  end if;
               end if;
            end if;

         --  comment would be good here ???

         elsif Ekind (Selector) = E_Discriminant
           and then Nkind (N) /= N_Extension_Aggregate
           and then Nkind (Parent (N)) = N_Component_Association
           and then Is_Constrained (Typ)
         then
            --  We must check that the discriminant value imposed by the
            --  context is the same as the value given in the subaggregate,
            --  because after the expansion into assignments there is no
            --  record on which to perform a regular discriminant check.

            declare
               D_Val : Elmt_Id;
               Disc  : Entity_Id;

            begin
               D_Val := First_Elmt (Discriminant_Constraint (Typ));
               Disc  := First_Discriminant (Typ);
               while Chars (Disc) /= Chars (Selector) loop
                  Next_Discriminant (Disc);
                  Next_Elmt (D_Val);
               end loop;

               pragma Assert (Present (D_Val));

               --  This check cannot performed for components that are
               --  constrained by a current instance, because this is not a
               --  value that can be compared with the actual constraint.

               if Nkind (Node (D_Val)) /= N_Attribute_Reference
                 or else not Is_Entity_Name (Prefix (Node (D_Val)))
                 or else not Is_Type (Entity (Prefix (Node (D_Val))))
               then
                  Append_To (L,
                  Make_Raise_Constraint_Error (Loc,
                    Condition =>
                      Make_Op_Ne (Loc,
                        Left_Opnd  => New_Copy_Tree (Node (D_Val)),
                        Right_Opnd => Expression (Comp)),
                    Reason    => CE_Discriminant_Check_Failed));

               else
                  --  Find self-reference in previous discriminant assignment,
                  --  and replace with proper expression.

                  declare
                     Ass : Node_Id;

                  begin
                     Ass := First (L);
                     while Present (Ass) loop
                        if Nkind (Ass) = N_Assignment_Statement
                          and then Nkind (Name (Ass)) = N_Selected_Component
                          and then Chars (Selector_Name (Name (Ass))) =
                                                                 Chars (Disc)
                        then
                           Set_Expression
                             (Ass, New_Copy_Tree (Expression (Comp)));
                           exit;
                        end if;
                        Next (Ass);
                     end loop;
                  end;
               end if;
            end;
         end if;

         Next (Comp);
      end loop;

      --  If the type is tagged, the tag needs to be initialized (unless we
      --  are in VM-mode where tags are implicit). It is done late in the
      --  initialization process because in some cases, we call the init
      --  proc of an ancestor which will not leave out the right tag.

      if Ancestor_Is_Expression then
         null;

      --  For CPP types we generated a call to the C++ default constructor
      --  before the components have been initialized to ensure the proper
      --  initialization of the _Tag component (see above).

      elsif Is_CPP_Class (Typ) then
         null;

      elsif Is_Tagged_Type (Typ) and then Tagged_Type_Expansion then
         Instr :=
           Make_OK_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix => New_Copy_Tree (Target),
                 Selector_Name =>
                   New_Occurrence_Of
                     (First_Tag_Component (Base_Type (Typ)), Loc)),

             Expression =>
               Unchecked_Convert_To (RTE (RE_Tag),
                 New_Occurrence_Of
                   (Node (First_Elmt (Access_Disp_Table (Base_Type (Typ)))),
                    Loc)));

         Append_To (L, Instr);

         --  Ada 2005 (AI-251): If the tagged type has been derived from an
         --  abstract interfaces we must also initialize the tags of the
         --  secondary dispatch tables.

         if Has_Interfaces (Base_Type (Typ)) then
            Init_Secondary_Tags
              (Typ            => Base_Type (Typ),
               Target         => Target,
               Stmts_List     => L,
               Init_Tags_List => L);
         end if;
      end if;

      --  If the controllers have not been initialized yet (by lack of non-
      --  discriminant components), let's do it now.

      Generate_Finalization_Actions;

      return L;
   end Build_Record_Aggr_Code;

   ---------------------------------------
   -- Collect_Initialization_Statements --
   ---------------------------------------

   procedure Collect_Initialization_Statements
     (Obj        : Entity_Id;
      N          : Node_Id;
      Node_After : Node_Id)
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Init_Actions : constant List_Id    := New_List;
      Init_Node    : Node_Id;
      Comp_Stmt    : Node_Id;

   begin
      --  Nothing to do if Obj is already frozen, as in this case we known we
      --  won't need to move the initialization statements about later on.

      if Is_Frozen (Obj) then
         return;
      end if;

      Init_Node := N;
      while Next (Init_Node) /= Node_After loop
         Append_To (Init_Actions, Remove_Next (Init_Node));
      end loop;

      if not Is_Empty_List (Init_Actions) then
         Comp_Stmt := Make_Compound_Statement (Loc, Actions => Init_Actions);
         Insert_Action_After (Init_Node, Comp_Stmt);
         Set_Initialization_Statements (Obj, Comp_Stmt);
      end if;
   end Collect_Initialization_Statements;

   -------------------------------
   -- Convert_Aggr_In_Allocator --
   -------------------------------

   procedure Convert_Aggr_In_Allocator
     (Alloc :  Node_Id;
      Decl  :  Node_Id;
      Aggr  :  Node_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Aggr);
      Typ  : constant Entity_Id  := Etype (Aggr);
      Temp : constant Entity_Id  := Defining_Identifier (Decl);

      Occ  : constant Node_Id :=
        Unchecked_Convert_To (Typ,
          Make_Explicit_Dereference (Loc, New_Occurrence_Of (Temp, Loc)));

   begin
      if Is_Array_Type (Typ) then
         Convert_Array_Aggr_In_Allocator (Decl, Aggr, Occ);

      elsif Has_Default_Init_Comps (Aggr) then
         declare
            L          : constant List_Id := New_List;
            Init_Stmts : List_Id;

         begin
            Init_Stmts := Late_Expansion (Aggr, Typ, Occ);

            if Has_Task (Typ) then
               Build_Task_Allocate_Block_With_Init_Stmts (L, Aggr, Init_Stmts);
               Insert_Actions (Alloc, L);
            else
               Insert_Actions (Alloc, Init_Stmts);
            end if;
         end;

      else
         Insert_Actions (Alloc, Late_Expansion (Aggr, Typ, Occ));
      end if;
   end Convert_Aggr_In_Allocator;

   --------------------------------
   -- Convert_Aggr_In_Assignment --
   --------------------------------

   procedure Convert_Aggr_In_Assignment (N : Node_Id) is
      Aggr : Node_Id            := Expression (N);
      Typ  : constant Entity_Id := Etype (Aggr);
      Occ  : constant Node_Id   := New_Copy_Tree (Name (N));

   begin
      if Nkind (Aggr) = N_Qualified_Expression then
         Aggr := Expression (Aggr);
      end if;

      Insert_Actions_After (N, Late_Expansion (Aggr, Typ, Occ));
   end Convert_Aggr_In_Assignment;

   ---------------------------------
   -- Convert_Aggr_In_Object_Decl --
   ---------------------------------

   procedure Convert_Aggr_In_Object_Decl (N : Node_Id) is
      Obj  : constant Entity_Id  := Defining_Identifier (N);
      Aggr : Node_Id             := Expression (N);
      Loc  : constant Source_Ptr := Sloc (Aggr);
      Typ  : constant Entity_Id  := Etype (Aggr);
      Occ  : constant Node_Id    := New_Occurrence_Of (Obj, Loc);

      function Discriminants_Ok return Boolean;
      --  If the object type is constrained, the discriminants in the
      --  aggregate must be checked against the discriminants of the subtype.
      --  This cannot be done using Apply_Discriminant_Checks because after
      --  expansion there is no aggregate left to check.

      ----------------------
      -- Discriminants_Ok --
      ----------------------

      function Discriminants_Ok return Boolean is
         Cond  : Node_Id := Empty;
         Check : Node_Id;
         D     : Entity_Id;
         Disc1 : Elmt_Id;
         Disc2 : Elmt_Id;
         Val1  : Node_Id;
         Val2  : Node_Id;

      begin
         D := First_Discriminant (Typ);
         Disc1 := First_Elmt (Discriminant_Constraint (Typ));
         Disc2 := First_Elmt (Discriminant_Constraint (Etype (Obj)));
         while Present (Disc1) and then Present (Disc2) loop
            Val1 := Node (Disc1);
            Val2 := Node (Disc2);

            if not Is_OK_Static_Expression (Val1)
              or else not Is_OK_Static_Expression (Val2)
            then
               Check := Make_Op_Ne (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Val1),
                 Right_Opnd => Duplicate_Subexpr (Val2));

               if No (Cond) then
                  Cond := Check;

               else
                  Cond := Make_Or_Else (Loc,
                    Left_Opnd => Cond,
                    Right_Opnd => Check);
               end if;

            elsif Expr_Value (Val1) /= Expr_Value (Val2) then
               Apply_Compile_Time_Constraint_Error (Aggr,
                 Msg    => "incorrect value for discriminant&??",
                 Reason => CE_Discriminant_Check_Failed,
                 Ent    => D);
               return False;
            end if;

            Next_Discriminant (D);
            Next_Elmt (Disc1);
            Next_Elmt (Disc2);
         end loop;

         --  If any discriminant constraint is nonstatic, emit a check

         if Present (Cond) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Discriminant_Check_Failed));
         end if;

         return True;
      end Discriminants_Ok;

   --  Start of processing for Convert_Aggr_In_Object_Decl

   begin
      Set_Assignment_OK (Occ);

      if Nkind (Aggr) = N_Qualified_Expression then
         Aggr := Expression (Aggr);
      end if;

      if Has_Discriminants (Typ)
        and then Typ /= Etype (Obj)
        and then Is_Constrained (Etype (Obj))
        and then not Discriminants_Ok
      then
         return;
      end if;

      --  If the context is an extended return statement, it has its own
      --  finalization machinery (i.e. works like a transient scope) and
      --  we do not want to create an additional one, because objects on
      --  the finalization list of the return must be moved to the caller's
      --  finalization list to complete the return.

      --  However, if the aggregate is limited, it is built in place, and the
      --  controlled components are not assigned to intermediate temporaries
      --  so there is no need for a transient scope in this case either.

      if Requires_Transient_Scope (Typ)
        and then Ekind (Current_Scope) /= E_Return_Statement
        and then not Is_Limited_Type (Typ)
      then
         Establish_Transient_Scope (Aggr, Manage_Sec_Stack => False);
      end if;

      declare
         Node_After : constant Node_Id := Next (N);
      begin
         Insert_Actions_After (N, Late_Expansion (Aggr, Typ, Occ));
         Collect_Initialization_Statements (Obj, N, Node_After);
      end;

      Set_No_Initialization (N);
      Initialize_Discriminants (N, Typ);
   end Convert_Aggr_In_Object_Decl;

   -------------------------------------
   -- Convert_Array_Aggr_In_Allocator --
   -------------------------------------

   procedure Convert_Array_Aggr_In_Allocator
     (Decl   : Node_Id;
      Aggr   : Node_Id;
      Target : Node_Id)
   is
      Aggr_Code : List_Id;
      Typ       : constant Entity_Id := Etype (Aggr);
      Ctyp      : constant Entity_Id := Component_Type (Typ);

   begin
      --  The target is an explicit dereference of the allocated object.
      --  Generate component assignments to it, as for an aggregate that
      --  appears on the right-hand side of an assignment statement.

      Aggr_Code :=
        Build_Array_Aggr_Code (Aggr,
          Ctype       => Ctyp,
          Index       => First_Index (Typ),
          Into        => Target,
          Scalar_Comp => Is_Scalar_Type (Ctyp));

      Insert_Actions_After (Decl, Aggr_Code);
   end Convert_Array_Aggr_In_Allocator;

   ----------------------------
   -- Convert_To_Assignments --
   ----------------------------

   procedure Convert_To_Assignments (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      T    : Entity_Id;
      Temp : Entity_Id;

      Aggr_Code   : List_Id;
      Instr       : Node_Id;
      Target_Expr : Node_Id;
      Parent_Kind : Node_Kind;
      Unc_Decl    : Boolean := False;
      Parent_Node : Node_Id;

   begin
      pragma Assert (Nkind_In (N, N_Aggregate, N_Extension_Aggregate));
      pragma Assert (not Is_Static_Dispatch_Table_Aggregate (N));
      pragma Assert (Is_Record_Type (Typ));

      Parent_Node := Parent (N);
      Parent_Kind := Nkind (Parent_Node);

      if Parent_Kind = N_Qualified_Expression then
         --  Check if we are in an unconstrained declaration because in this
         --  case the current delayed expansion mechanism doesn't work when
         --  the declared object size depends on the initializing expr.

         Parent_Node := Parent (Parent_Node);
         Parent_Kind := Nkind (Parent_Node);

         if Parent_Kind = N_Object_Declaration then
            Unc_Decl :=
              not Is_Entity_Name (Object_Definition (Parent_Node))
                or else (Nkind (N) = N_Aggregate
                          and then
                            Has_Discriminants
                              (Entity (Object_Definition (Parent_Node))))
                or else Is_Class_Wide_Type
                          (Entity (Object_Definition (Parent_Node)));
         end if;
      end if;

      --  Just set the Delay flag in the cases where the transformation will be
      --  done top down from above.

      if False

         --  Internal aggregate (transformed when expanding the parent)

         or else Parent_Kind = N_Aggregate
         or else Parent_Kind = N_Extension_Aggregate
         or else Parent_Kind = N_Component_Association

         --  Allocator (see Convert_Aggr_In_Allocator)

         or else Parent_Kind = N_Allocator

         --  Object declaration (see Convert_Aggr_In_Object_Decl)

         or else (Parent_Kind = N_Object_Declaration and then not Unc_Decl)

         --  Safe assignment (see Convert_Aggr_Assignments). So far only the
         --  assignments in init procs are taken into account.

         or else (Parent_Kind = N_Assignment_Statement
                   and then Inside_Init_Proc)

         --  (Ada 2005) An inherently limited type in a return statement, which
         --  will be handled in a build-in-place fashion, and may be rewritten
         --  as an extended return and have its own finalization machinery.
         --  In the case of a simple return, the aggregate needs to be delayed
         --  until the scope for the return statement has been created, so
         --  that any finalization chain will be associated with that scope.
         --  For extended returns, we delay expansion to avoid the creation
         --  of an unwanted transient scope that could result in premature
         --  finalization of the return object (which is built in place
         --  within the caller's scope).

         or else Is_Build_In_Place_Aggregate_Return (N)
      then
         Set_Expansion_Delayed (N);
         return;
      end if;

      --  Otherwise, if a transient scope is required, create it now. If we
      --  are within an initialization procedure do not create such, because
      --  the target of the assignment must not be declared within a local
      --  block, and because cleanup will take place on return from the
      --  initialization procedure.

      --  Should the condition be more restrictive ???

      if Requires_Transient_Scope (Typ) and then not Inside_Init_Proc then
         Establish_Transient_Scope (N, Manage_Sec_Stack => False);
      end if;

      --  If the aggregate is nonlimited, create a temporary. If it is limited
      --  and context is an assignment, this is a subaggregate for an enclosing
      --  aggregate being expanded. It must be built in place, so use target of
      --  the current assignment.

      if Is_Limited_Type (Typ)
        and then Nkind (Parent (N)) = N_Assignment_Statement
      then
         Target_Expr := New_Copy_Tree (Name (Parent (N)));
         Insert_Actions (Parent (N),
           Build_Record_Aggr_Code (N, Typ, Target_Expr));
         Rewrite (Parent (N), Make_Null_Statement (Loc));

      --  Generating C, do not declare a temporary to initialize an aggregate
      --  assigned to Out or In_Out parameters whose type has no discriminants.
      --  This avoids stack overflow errors at run time.

      elsif Modify_Tree_For_C
        and then Nkind (Parent (N)) = N_Assignment_Statement
        and then Nkind (Name (Parent (N))) = N_Identifier
        and then Ekind_In (Entity (Name (Parent (N))), E_Out_Parameter,
                                                       E_In_Out_Parameter)
        and then not Has_Discriminants (Etype (Entity (Name (Parent (N)))))
      then
         Target_Expr := New_Copy_Tree (Name (Parent (N)));
         Insert_Actions (Parent (N),
           Build_Record_Aggr_Code (N, Typ, Target_Expr));
         Rewrite (Parent (N), Make_Null_Statement (Loc));

      else
         Temp := Make_Temporary (Loc, 'A', N);

         --  If the type inherits unknown discriminants, use the view with
         --  known discriminants if available.

         if Has_Unknown_Discriminants (Typ)
           and then Present (Underlying_Record_View (Typ))
         then
            T := Underlying_Record_View (Typ);
         else
            T := Typ;
         end if;

         Instr :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => New_Occurrence_Of (T, Loc));

         Set_No_Initialization (Instr);
         Insert_Action (N, Instr);
         Initialize_Discriminants (Instr, T);

         Target_Expr := New_Occurrence_Of (Temp, Loc);
         Aggr_Code   := Build_Record_Aggr_Code (N, T, Target_Expr);

         --  Save the last assignment statement associated with the aggregate
         --  when building a controlled object. This reference is utilized by
         --  the finalization machinery when marking an object as successfully
         --  initialized.

         if Needs_Finalization (T) then
            Set_Last_Aggregate_Assignment (Temp, Last (Aggr_Code));
         end if;

         Insert_Actions (N, Aggr_Code);
         Rewrite (N, New_Occurrence_Of (Temp, Loc));
         Analyze_And_Resolve (N, T);
      end if;
   end Convert_To_Assignments;

   ---------------------------
   -- Convert_To_Positional --
   ---------------------------

   procedure Convert_To_Positional
     (N                    : Node_Id;
      Max_Others_Replicate : Nat     := 32;
      Handle_Bit_Packed    : Boolean := False)
   is
      Typ : constant Entity_Id := Etype (N);

      Static_Components : Boolean := True;

      procedure Check_Static_Components;
      --  Check whether all components of the aggregate are compile-time known
      --  values, and can be passed as is to the back-end without further
      --  expansion.

      function Flatten
        (N   : Node_Id;
         Ix  : Node_Id;
         Ixb : Node_Id) return Boolean;
      --  Convert the aggregate into a purely positional form if possible. On
      --  entry the bounds of all dimensions are known to be static, and the
      --  total number of components is safe enough to expand.

      function Is_Flat (N : Node_Id; Dims : Int) return Boolean;
      --  Return True iff the array N is flat (which is not trivial in the case
      --  of multidimensional aggregates).

      function Is_Static_Element (N : Node_Id) return Boolean;
      --  Return True if N, an element of a component association list, i.e.
      --  N_Component_Association or N_Iterated_Component_Association, has a
      --  compile-time known value and can be passed as is to the back-end
      --  without further expansion.
      --  An Iterated_Component_Association is treated as nonstatic in most
      --  cases for now, so there are possibilities for optimization.

      -----------------------------
      -- Check_Static_Components --
      -----------------------------

      --  Could use some comments in this body ???

      procedure Check_Static_Components is
         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         Static_Components := True;

         if Nkind (N) = N_String_Literal then
            null;

         elsif Present (Expressions (N)) then
            Expr := First (Expressions (N));
            while Present (Expr) loop
               if Nkind (Expr) /= N_Aggregate
                 or else not Compile_Time_Known_Aggregate (Expr)
                 or else Expansion_Delayed (Expr)
               then
                  Static_Components := False;
                  exit;
               end if;

               Next (Expr);
            end loop;
         end if;

         if Nkind (N) = N_Aggregate
           and then Present (Component_Associations (N))
         then
            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop
               if not Is_Static_Element (Assoc) then
                  Static_Components := False;
                  exit;
               end if;

               Next (Assoc);
            end loop;
         end if;
      end Check_Static_Components;

      -------------
      -- Flatten --
      -------------

      function Flatten
        (N   : Node_Id;
         Ix  : Node_Id;
         Ixb : Node_Id) return Boolean
      is
         Loc : constant Source_Ptr := Sloc (N);
         Blo : constant Node_Id    := Type_Low_Bound (Etype (Ixb));
         Lo  : constant Node_Id    := Type_Low_Bound (Etype (Ix));
         Hi  : constant Node_Id    := Type_High_Bound (Etype (Ix));
         Lov : Uint;
         Hiv : Uint;

         Others_Present : Boolean := False;

      begin
         if Nkind (Original_Node (N)) = N_String_Literal then
            return True;
         end if;

         if not Compile_Time_Known_Value (Lo)
           or else not Compile_Time_Known_Value (Hi)
         then
            return False;
         end if;

         Lov := Expr_Value (Lo);
         Hiv := Expr_Value (Hi);

         --  Check if there is an others choice

         if Present (Component_Associations (N)) then
            declare
               Assoc   : Node_Id;
               Choice  : Node_Id;

            begin
               Assoc := First (Component_Associations (N));
               while Present (Assoc) loop

                  --  If this is a box association, flattening is in general
                  --  not possible because at this point we cannot tell if the
                  --  default is static or even exists.

                  if Box_Present (Assoc) then
                     return False;

                  elsif Nkind (Assoc) = N_Iterated_Component_Association then
                     return False;
                  end if;

                  Choice := First (Choice_List (Assoc));

                  while Present (Choice) loop
                     if Nkind (Choice) = N_Others_Choice then
                        Others_Present := True;
                     end if;

                     Next (Choice);
                  end loop;

                  Next (Assoc);
               end loop;
            end;
         end if;

         --  If the low bound is not known at compile time and others is not
         --  present we can proceed since the bounds can be obtained from the
         --  aggregate.

         if Hiv < Lov
           or else (not Compile_Time_Known_Value (Blo) and then Others_Present)
         then
            return False;
         end if;

         --  Determine if set of alternatives is suitable for conversion and
         --  build an array containing the values in sequence.

         declare
            Vals : array (UI_To_Int (Lov) .. UI_To_Int (Hiv))
                     of Node_Id := (others => Empty);
            --  The values in the aggregate sorted appropriately

            Vlist : List_Id;
            --  Same data as Vals in list form

            Rep_Count : Nat;
            --  Used to validate Max_Others_Replicate limit

            Elmt         : Node_Id;
            Num          : Int := UI_To_Int (Lov);
            Choice_Index : Int;
            Choice       : Node_Id;
            Lo, Hi       : Node_Id;

         begin
            if Present (Expressions (N)) then
               Elmt := First (Expressions (N));
               while Present (Elmt) loop
                  if Nkind (Elmt) = N_Aggregate
                    and then Present (Next_Index (Ix))
                    and then
                      not Flatten (Elmt, Next_Index (Ix), Next_Index (Ixb))
                  then
                     return False;
                  end if;

                  --  Duplicate expression for each index it covers

                  Vals (Num) := New_Copy_Tree (Elmt);
                  Num := Num + 1;

                  Next (Elmt);
               end loop;
            end if;

            if No (Component_Associations (N)) then
               return True;
            end if;

            Elmt := First (Component_Associations (N));

            if Nkind (Expression (Elmt)) = N_Aggregate then
               if Present (Next_Index (Ix))
                 and then
                   not Flatten
                         (Expression (Elmt), Next_Index (Ix), Next_Index (Ixb))
               then
                  return False;
               end if;
            end if;

            Component_Loop : while Present (Elmt) loop
               Choice := First (Choice_List (Elmt));
               Choice_Loop : while Present (Choice) loop

                  --  If we have an others choice, fill in the missing elements
                  --  subject to the limit established by Max_Others_Replicate.

                  if Nkind (Choice) = N_Others_Choice then
                     Rep_Count := 0;

                     --  If the expression involves a construct that generates
                     --  a loop, we must generate individual assignments and
                     --  no flattening is possible.

                     if Nkind (Expression (Elmt)) = N_Quantified_Expression
                     then
                        return False;
                     end if;

                     for J in Vals'Range loop
                        if No (Vals (J)) then
                           Vals (J)  := New_Copy_Tree (Expression (Elmt));
                           Rep_Count := Rep_Count + 1;

                           --  Check for maximum others replication. Note that
                           --  we skip this test if either of the restrictions
                           --  No_Elaboration_Code or No_Implicit_Loops is
                           --  active, if this is a preelaborable unit or
                           --  a predefined unit, or if the unit must be
                           --  placed in data memory. This also ensures that
                           --  predefined units get the same level of constant
                           --  folding in Ada 95 and Ada 2005, where their
                           --  categorization has changed.

                           declare
                              P : constant Entity_Id :=
                                    Cunit_Entity (Current_Sem_Unit);

                           begin
                              --  Check if duplication is always OK and, if so,
                              --  continue processing.

                              if Restriction_Active (No_Elaboration_Code)
                                or else Restriction_Active (No_Implicit_Loops)
                                or else
                                  (Ekind (Current_Scope) = E_Package
                                    and then Static_Elaboration_Desired
                                               (Current_Scope))
                                or else Is_Preelaborated (P)
                                or else (Ekind (P) = E_Package_Body
                                          and then
                                            Is_Preelaborated (Spec_Entity (P)))
                                or else
                                  Is_Predefined_Unit (Get_Source_Unit (P))
                              then
                                 null;

                              --  If duplication is not always OK, continue
                              --  only if either the element is static or is
                              --  an aggregate which can itself be flattened,
                              --  and the replication count is not too high.

                              elsif (Is_Static_Element (Elmt)
                                       or else
                                     (Nkind (Expression (Elmt)) = N_Aggregate
                                       and then Present (Next_Index (Ix))))
                                and then Rep_Count <= Max_Others_Replicate
                              then
                                 null;

                              --  Return False in all the other cases

                              else
                                 return False;
                              end if;
                           end;
                        end if;
                     end loop;

                     if Rep_Count = 0
                       and then Warn_On_Redundant_Constructs
                     then
                        Error_Msg_N ("there are no others?r?", Elmt);
                     end if;

                     exit Component_Loop;

                  --  Case of a subtype mark, identifier or expanded name

                  elsif Is_Entity_Name (Choice)
                    and then Is_Type (Entity (Choice))
                  then
                     Lo := Type_Low_Bound  (Etype (Choice));
                     Hi := Type_High_Bound (Etype (Choice));

                  --  Case of subtype indication

                  elsif Nkind (Choice) = N_Subtype_Indication then
                     Lo := Low_Bound  (Range_Expression (Constraint (Choice)));
                     Hi := High_Bound (Range_Expression (Constraint (Choice)));

                  --  Case of a range

                  elsif Nkind (Choice) = N_Range then
                     Lo := Low_Bound (Choice);
                     Hi := High_Bound (Choice);

                  --  Normal subexpression case

                  else pragma Assert (Nkind (Choice) in N_Subexpr);
                     if not Compile_Time_Known_Value (Choice) then
                        return False;

                     else
                        Choice_Index := UI_To_Int (Expr_Value (Choice));

                        if Choice_Index in Vals'Range then
                           Vals (Choice_Index) :=
                             New_Copy_Tree (Expression (Elmt));
                           goto Continue;

                        --  Choice is statically out-of-range, will be
                        --  rewritten to raise Constraint_Error.

                        else
                           return False;
                        end if;
                     end if;
                  end if;

                  --  Range cases merge with Lo,Hi set

                  if not Compile_Time_Known_Value (Lo)
                       or else
                     not Compile_Time_Known_Value (Hi)
                  then
                     return False;

                  else
                     for J in UI_To_Int (Expr_Value (Lo)) ..
                              UI_To_Int (Expr_Value (Hi))
                     loop
                        Vals (J) := New_Copy_Tree (Expression (Elmt));
                     end loop;
                  end if;

               <<Continue>>
                  Next (Choice);
               end loop Choice_Loop;

               Next (Elmt);
            end loop Component_Loop;

            --  If we get here the conversion is possible

            Vlist := New_List;
            for J in Vals'Range loop
               Append (Vals (J), Vlist);
            end loop;

            Rewrite (N, Make_Aggregate (Loc, Expressions => Vlist));
            Set_Aggregate_Bounds (N, Aggregate_Bounds (Original_Node (N)));
            return True;
         end;
      end Flatten;

      -------------
      -- Is_Flat --
      -------------

      function Is_Flat (N : Node_Id; Dims : Int) return Boolean is
         Elmt : Node_Id;

      begin
         if Dims = 0 then
            return True;

         elsif Nkind (N) = N_Aggregate then
            if Present (Component_Associations (N)) then
               return False;

            else
               Elmt := First (Expressions (N));
               while Present (Elmt) loop
                  if not Is_Flat (Elmt, Dims - 1) then
                     return False;
                  end if;

                  Next (Elmt);
               end loop;

               return True;
            end if;
         else
            return True;
         end if;
      end Is_Flat;

      -------------------------
      --  Is_Static_Element  --
      -------------------------

      function Is_Static_Element (N : Node_Id) return Boolean is
         Expr : constant Node_Id := Expression (N);

      begin
         if Nkind_In (Expr, N_Integer_Literal, N_Real_Literal) then
            return True;

         elsif Is_Entity_Name (Expr)
           and then Present (Entity (Expr))
           and then Ekind (Entity (Expr)) = E_Enumeration_Literal
         then
            return True;

         elsif Nkind (N) = N_Iterated_Component_Association then
            return False;

         elsif Nkind (Expr) = N_Aggregate
           and then Compile_Time_Known_Aggregate (Expr)
           and then not Expansion_Delayed (Expr)
         then
            return True;

         else
            return False;
         end if;
      end Is_Static_Element;

   --  Start of processing for Convert_To_Positional

   begin
      --  Only convert to positional when generating C in case of an
      --  object declaration, this is the only case where aggregates are
      --  supported in C.

      if Modify_Tree_For_C and then not Is_CCG_Supported_Aggregate (N) then
         return;
      end if;

      --  Ada 2005 (AI-287): Do not convert in case of default initialized
      --  components because in this case will need to call the corresponding
      --  IP procedure.

      if Has_Default_Init_Comps (N) then
         return;
      end if;

      --  A subaggregate may have been flattened but is not known to be
      --  Compile_Time_Known. Set that flag in cases that cannot require
      --  elaboration code, so that the aggregate can be used as the
      --  initial value of a thread-local variable.

      if Is_Flat (N, Number_Dimensions (Typ)) then
         if Static_Array_Aggregate (N) then
            Set_Compile_Time_Known_Aggregate (N);
         end if;

         return;
      end if;

      if Is_Bit_Packed_Array (Typ) and then not Handle_Bit_Packed then
         return;
      end if;

      --  Do not convert to positional if controlled components are involved
      --  since these require special processing

      if Has_Controlled_Component (Typ) then
         return;
      end if;

      Check_Static_Components;

      --  If the size is known, or all the components are static, try to
      --  build a fully positional aggregate.

      --  The size of the type may not be known for an aggregate with
      --  discriminated array components, but if the components are static
      --  it is still possible to verify statically that the length is
      --  compatible with the upper bound of the type, and therefore it is
      --  worth flattening such aggregates as well.

      --  For now the back-end expands these aggregates into individual
      --  assignments to the target anyway, but it is conceivable that
      --  it will eventually be able to treat such aggregates statically???

      if Aggr_Size_OK (N, Typ)
        and then Flatten (N, First_Index (Typ), First_Index (Base_Type (Typ)))
      then
         if Static_Components then
            Set_Compile_Time_Known_Aggregate (N);
            Set_Expansion_Delayed (N, False);
         end if;

         Analyze_And_Resolve (N, Typ);
      end if;

      --  If Static_Elaboration_Desired has been specified, diagnose aggregates
      --  that will still require initialization code.

      if (Ekind (Current_Scope) = E_Package
        and then Static_Elaboration_Desired (Current_Scope))
        and then Nkind (Parent (N)) = N_Object_Declaration
      then
         declare
            Expr : Node_Id;

         begin
            if Nkind (N) = N_Aggregate and then Present (Expressions (N)) then
               Expr := First (Expressions (N));
               while Present (Expr) loop
                  if Nkind_In (Expr, N_Integer_Literal, N_Real_Literal)
                    or else
                      (Is_Entity_Name (Expr)
                        and then Ekind (Entity (Expr)) = E_Enumeration_Literal)
                  then
                     null;

                  else
                     Error_Msg_N
                       ("non-static object requires elaboration code??", N);
                     exit;
                  end if;

                  Next (Expr);
               end loop;

               if Present (Component_Associations (N)) then
                  Error_Msg_N ("object requires elaboration code??", N);
               end if;
            end if;
         end;
      end if;
   end Convert_To_Positional;

   ----------------------------
   -- Expand_Array_Aggregate --
   ----------------------------

   --  Array aggregate expansion proceeds as follows:

   --  1. If requested we generate code to perform all the array aggregate
   --     bound checks, specifically

   --         (a) Check that the index range defined by aggregate bounds is
   --             compatible with corresponding index subtype.

   --         (b) If an others choice is present check that no aggregate
   --             index is outside the bounds of the index constraint.

   --         (c) For multidimensional arrays make sure that all subaggregates
   --             corresponding to the same dimension have the same bounds.

   --  2. Check for packed array aggregate which can be converted to a
   --     constant so that the aggregate disappears completely.

   --  3. Check case of nested aggregate. Generally nested aggregates are
   --     handled during the processing of the parent aggregate.

   --  4. Check if the aggregate can be statically processed. If this is the
   --     case pass it as is to Gigi. Note that a necessary condition for
   --     static processing is that the aggregate be fully positional.

   --  5. If in place aggregate expansion is possible (i.e. no need to create
   --     a temporary) then mark the aggregate as such and return. Otherwise
   --     create a new temporary and generate the appropriate initialization
   --     code.

   procedure Expand_Array_Aggregate (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Typ  : constant Entity_Id := Etype (N);
      Ctyp : constant Entity_Id := Component_Type (Typ);
      --  Typ is the correct constrained array subtype of the aggregate
      --  Ctyp is the corresponding component type.

      Aggr_Dimension : constant Pos := Number_Dimensions (Typ);
      --  Number of aggregate index dimensions

      Aggr_Low  : array (1 .. Aggr_Dimension) of Node_Id;
      Aggr_High : array (1 .. Aggr_Dimension) of Node_Id;
      --  Low and High bounds of the constraint for each aggregate index

      Aggr_Index_Typ : array (1 .. Aggr_Dimension) of Entity_Id;
      --  The type of each index

      In_Place_Assign_OK_For_Declaration : Boolean := False;
      --  True if we are to generate an in place assignment for a declaration

      Maybe_In_Place_OK : Boolean;
      --  If the type is neither controlled nor packed and the aggregate
      --  is the expression in an assignment, assignment in place may be
      --  possible, provided other conditions are met on the LHS.

      Others_Present : array (1 .. Aggr_Dimension) of Boolean :=
        (others => False);
      --  If Others_Present (J) is True, then there is an others choice in one
      --  of the subaggregates of N at dimension J.

      function Aggr_Assignment_OK_For_Backend (N : Node_Id) return Boolean;
      --  Returns true if an aggregate assignment can be done by the back end

      procedure Build_Constrained_Type (Positional : Boolean);
      --  If the subtype is not static or unconstrained, build a constrained
      --  type using the computable sizes of the aggregate and its sub-
      --  aggregates.

      procedure Check_Bounds (Aggr_Bounds : Node_Id; Index_Bounds : Node_Id);
      --  Checks that the bounds of Aggr_Bounds are within the bounds defined
      --  by Index_Bounds.

      procedure Check_Same_Aggr_Bounds (Sub_Aggr : Node_Id; Dim : Pos);
      --  Checks that in a multidimensional array aggregate all subaggregates
      --  corresponding to the same dimension have the same bounds. Sub_Aggr is
      --  an array subaggregate. Dim is the dimension corresponding to the
      --  subaggregate.

      procedure Compute_Others_Present (Sub_Aggr : Node_Id; Dim : Pos);
      --  Computes the values of array Others_Present. Sub_Aggr is the array
      --  subaggregate we start the computation from. Dim is the dimension
      --  corresponding to the subaggregate.

      function In_Place_Assign_OK return Boolean;
      --  Simple predicate to determine whether an aggregate assignment can
      --  be done in place, because none of the new values can depend on the
      --  components of the target of the assignment.

      procedure Others_Check (Sub_Aggr : Node_Id; Dim : Pos);
      --  Checks that if an others choice is present in any subaggregate, no
      --  aggregate index is outside the bounds of the index constraint.
      --  Sub_Aggr is an array subaggregate. Dim is the dimension corresponding
      --  to the subaggregate.

      function Safe_Left_Hand_Side (N : Node_Id) return Boolean;
      --  In addition to Maybe_In_Place_OK, in order for an aggregate to be
      --  built directly into the target of the assignment it must be free
      --  of side effects.

      ------------------------------------
      -- Aggr_Assignment_OK_For_Backend --
      ------------------------------------

      --  Backend processing by Gigi/gcc is possible only if all the following
      --  conditions are met:

      --    1. N consists of a single OTHERS choice, possibly recursively

      --    2. The array type has no null ranges (the purpose of this is to
      --       avoid a bogus warning for an out-of-range value).

      --    3. The array type has no atomic components

      --    4. The component type is elementary

      --    5. The component size is a multiple of Storage_Unit

      --    6. The component size is Storage_Unit or the value is of the form
      --       M * (1 + A**1 + A**2 + .. A**(K-1)) where A = 2**(Storage_Unit)
      --       and M in 1 .. A-1. This can also be viewed as K occurrences of
      --       the 8-bit value M, concatenated together.

      --  The ultimate goal is to generate a call to a fast memset routine
      --  specifically optimized for the target.

      function Aggr_Assignment_OK_For_Backend (N : Node_Id) return Boolean is
         Csiz      : Uint;
         Ctyp      : Entity_Id;
         Expr      : Node_Id;
         High      : Node_Id;
         Index     : Entity_Id;
         Low       : Node_Id;
         Nunits    : Int;
         Remainder : Uint;
         Value     : Uint;

      begin
         --  Recurse as far as possible to find the innermost component type

         Ctyp := Etype (N);
         Expr := N;
         while Is_Array_Type (Ctyp) loop
            if Nkind (Expr) /= N_Aggregate
              or else not Is_Others_Aggregate (Expr)
            then
               return False;
            end if;

            Index := First_Index (Ctyp);
            while Present (Index) loop
               Get_Index_Bounds (Index, Low, High);

               if Is_Null_Range (Low, High) then
                  return False;
               end if;

               Next_Index (Index);
            end loop;

            Expr := Expression (First (Component_Associations (Expr)));

            for J in 1 .. Number_Dimensions (Ctyp) - 1 loop
               if Nkind (Expr) /= N_Aggregate
                 or else not Is_Others_Aggregate (Expr)
               then
                  return False;
               end if;

               Expr := Expression (First (Component_Associations (Expr)));
            end loop;

            if Has_Atomic_Components (Ctyp) then
               return False;
            end if;

            Csiz := Component_Size (Ctyp);
            Ctyp := Component_Type (Ctyp);

            if Is_Atomic_Or_VFA (Ctyp) then
               return False;
            end if;
         end loop;

         --  An Iterated_Component_Association involves a loop (in most cases)
         --  and is never static.

         if Nkind (Parent (Expr)) = N_Iterated_Component_Association then
            return False;
         end if;

         --  Access types need to be dealt with specially

         if Is_Access_Type (Ctyp) then

            --  Component_Size is not set by Layout_Type if the component
            --  type is an access type ???

            Csiz := Esize (Ctyp);

            --  Fat pointers are rejected as they are not really elementary
            --  for the backend.

            if Csiz /= System_Address_Size then
               return False;
            end if;

            --  The supported expressions are NULL and constants, others are
            --  rejected upfront to avoid being analyzed below, which can be
            --  problematic for some of them, for example allocators.

            if Nkind (Expr) /= N_Null and then not Is_Entity_Name (Expr) then
               return False;
            end if;

         --  Scalar types are OK if their size is a multiple of Storage_Unit

         elsif Is_Scalar_Type (Ctyp) then
            if Csiz mod System_Storage_Unit /= 0 then
               return False;
            end if;

         --  Composite types are rejected

         else
            return False;
         end if;

         --  The expression needs to be analyzed if True is returned

         Analyze_And_Resolve (Expr, Ctyp);

         --  Strip away any conversions from the expression as they simply
         --  qualify the real expression.

         while Nkind_In (Expr, N_Unchecked_Type_Conversion,
                               N_Type_Conversion)
         loop
            Expr := Expression (Expr);
         end loop;

         Nunits := UI_To_Int (Csiz) / System_Storage_Unit;

         if Nunits = 1 then
            return True;
         end if;

         if not Compile_Time_Known_Value (Expr) then
            return False;
         end if;

         --  The only supported value for floating point is 0.0

         if Is_Floating_Point_Type (Ctyp) then
            return Expr_Value_R (Expr) = Ureal_0;
         end if;

         --  For other types, we can look into the value as an integer

         Value := Expr_Value (Expr);

         if Has_Biased_Representation (Ctyp) then
            Value := Value - Expr_Value (Type_Low_Bound (Ctyp));
         end if;

         --  Values 0 and -1 immediately satisfy the last check

         if Value = Uint_0 or else Value = Uint_Minus_1 then
            return True;
         end if;

         --  We need to work with an unsigned value

         if Value < 0 then
            Value := Value + 2**(System_Storage_Unit * Nunits);
         end if;

         Remainder := Value rem 2**System_Storage_Unit;

         for J in 1 .. Nunits - 1 loop
            Value := Value / 2**System_Storage_Unit;

            if Value rem 2**System_Storage_Unit /= Remainder then
               return False;
            end if;
         end loop;

         return True;
      end Aggr_Assignment_OK_For_Backend;

      ----------------------------
      -- Build_Constrained_Type --
      ----------------------------

      procedure Build_Constrained_Type (Positional : Boolean) is
         Loc      : constant Source_Ptr := Sloc (N);
         Agg_Type : constant Entity_Id  := Make_Temporary (Loc, 'A');
         Comp     : Node_Id;
         Decl     : Node_Id;
         Typ      : constant Entity_Id := Etype (N);
         Indexes  : constant List_Id   := New_List;
         Num      : Nat;
         Sub_Agg  : Node_Id;

      begin
         --  If the aggregate is purely positional, all its subaggregates
         --  have the same size. We collect the dimensions from the first
         --  subaggregate at each level.

         if Positional then
            Sub_Agg := N;

            for D in 1 .. Number_Dimensions (Typ) loop
               Sub_Agg := First (Expressions (Sub_Agg));

               Comp := Sub_Agg;
               Num := 0;
               while Present (Comp) loop
                  Num := Num + 1;
                  Next (Comp);
               end loop;

               Append_To (Indexes,
                 Make_Range (Loc,
                   Low_Bound  => Make_Integer_Literal (Loc, 1),
                   High_Bound => Make_Integer_Literal (Loc, Num)));
            end loop;

         else
            --  We know the aggregate type is unconstrained and the aggregate
            --  is not processable by the back end, therefore not necessarily
            --  positional. Retrieve each dimension bounds (computed earlier).

            for D in 1 .. Number_Dimensions (Typ) loop
               Append_To (Indexes,
                 Make_Range (Loc,
                   Low_Bound  => Aggr_Low  (D),
                   High_Bound => Aggr_High (D)));
            end loop;
         end if;

         Decl :=
           Make_Full_Type_Declaration (Loc,
               Defining_Identifier => Agg_Type,
               Type_Definition     =>
                 Make_Constrained_Array_Definition (Loc,
                   Discrete_Subtype_Definitions => Indexes,
                   Component_Definition         =>
                     Make_Component_Definition (Loc,
                       Aliased_Present    => False,
                       Subtype_Indication =>
                         New_Occurrence_Of (Component_Type (Typ), Loc))));

         Insert_Action (N, Decl);
         Analyze (Decl);
         Set_Etype (N, Agg_Type);
         Set_Is_Itype (Agg_Type);
         Freeze_Itype (Agg_Type, N);
      end Build_Constrained_Type;

      ------------------
      -- Check_Bounds --
      ------------------

      procedure Check_Bounds (Aggr_Bounds : Node_Id; Index_Bounds : Node_Id) is
         Aggr_Lo : Node_Id;
         Aggr_Hi : Node_Id;

         Ind_Lo  : Node_Id;
         Ind_Hi  : Node_Id;

         Cond    : Node_Id := Empty;

      begin
         Get_Index_Bounds (Aggr_Bounds, Aggr_Lo, Aggr_Hi);
         Get_Index_Bounds (Index_Bounds, Ind_Lo, Ind_Hi);

         --  Generate the following test:

         --    [constraint_error when
         --      Aggr_Lo <= Aggr_Hi and then
         --        (Aggr_Lo < Ind_Lo or else Aggr_Hi > Ind_Hi)]

         --  As an optimization try to see if some tests are trivially vacuous
         --  because we are comparing an expression against itself.

         if Aggr_Lo = Ind_Lo and then Aggr_Hi = Ind_Hi then
            Cond := Empty;

         elsif Aggr_Hi = Ind_Hi then
            Cond :=
              Make_Op_Lt (Loc,
                Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                Right_Opnd => Duplicate_Subexpr_Move_Checks (Ind_Lo));

         elsif Aggr_Lo = Ind_Lo then
            Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Hi),
                Right_Opnd => Duplicate_Subexpr_Move_Checks (Ind_Hi));

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Lt (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr_Move_Checks (Ind_Lo)),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                    Right_Opnd => Duplicate_Subexpr (Ind_Hi)));
         end if;

         if Present (Cond) then
            Cond :=
              Make_And_Then (Loc,
                Left_Opnd =>
                  Make_Op_Le (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr_Move_Checks (Aggr_Hi)),

                Right_Opnd => Cond);

            Set_Analyzed (Left_Opnd  (Left_Opnd (Cond)), False);
            Set_Analyzed (Right_Opnd (Left_Opnd (Cond)), False);
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Range_Check_Failed));
         end if;
      end Check_Bounds;

      ----------------------------
      -- Check_Same_Aggr_Bounds --
      ----------------------------

      procedure Check_Same_Aggr_Bounds (Sub_Aggr : Node_Id; Dim : Pos) is
         Sub_Lo : constant Node_Id := Low_Bound (Aggregate_Bounds (Sub_Aggr));
         Sub_Hi : constant Node_Id := High_Bound (Aggregate_Bounds (Sub_Aggr));
         --  The bounds of this specific subaggregate

         Aggr_Lo : constant Node_Id := Aggr_Low (Dim);
         Aggr_Hi : constant Node_Id := Aggr_High (Dim);
         --  The bounds of the aggregate for this dimension

         Ind_Typ : constant Entity_Id := Aggr_Index_Typ (Dim);
         --  The index type for this dimension.xxx

         Cond  : Node_Id := Empty;
         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         --  If index checks are on generate the test

         --    [constraint_error when
         --      Aggr_Lo /= Sub_Lo or else Aggr_Hi /= Sub_Hi]

         --  As an optimization try to see if some tests are trivially vacuos
         --  because we are comparing an expression against itself. Also for
         --  the first dimension the test is trivially vacuous because there
         --  is just one aggregate for dimension 1.

         if Index_Checks_Suppressed (Ind_Typ) then
            Cond := Empty;

         elsif Dim = 1 or else (Aggr_Lo = Sub_Lo and then Aggr_Hi = Sub_Hi)
         then
            Cond := Empty;

         elsif Aggr_Hi = Sub_Hi then
            Cond :=
              Make_Op_Ne (Loc,
                Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                Right_Opnd => Duplicate_Subexpr_Move_Checks (Sub_Lo));

         elsif Aggr_Lo = Sub_Lo then
            Cond :=
              Make_Op_Ne (Loc,
                Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Hi),
                Right_Opnd => Duplicate_Subexpr_Move_Checks (Sub_Hi));

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Aggr_Lo),
                    Right_Opnd => Duplicate_Subexpr_Move_Checks (Sub_Lo)),

                Right_Opnd =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Aggr_Hi),
                    Right_Opnd => Duplicate_Subexpr (Sub_Hi)));
         end if;

         if Present (Cond) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Length_Check_Failed));
         end if;

         --  Now look inside the subaggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Check_Same_Aggr_Bounds (Expr, Dim + 1);
                  Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Check_Same_Aggr_Bounds (Expr, Dim + 1);
                  Next (Assoc);
               end loop;
            end if;
         end if;
      end Check_Same_Aggr_Bounds;

      ----------------------------
      -- Compute_Others_Present --
      ----------------------------

      procedure Compute_Others_Present (Sub_Aggr : Node_Id; Dim : Pos) is
         Assoc : Node_Id;
         Expr  : Node_Id;

      begin
         if Present (Component_Associations (Sub_Aggr)) then
            Assoc := Last (Component_Associations (Sub_Aggr));

            if Nkind (First (Choice_List (Assoc))) = N_Others_Choice then
               Others_Present (Dim) := True;
            end if;
         end if;

         --  Now look inside the subaggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Compute_Others_Present (Expr, Dim + 1);
                  Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Compute_Others_Present (Expr, Dim + 1);
                  Next (Assoc);
               end loop;
            end if;
         end if;
      end Compute_Others_Present;

      ------------------------
      -- In_Place_Assign_OK --
      ------------------------

      function In_Place_Assign_OK return Boolean is
         Aggr_In : Node_Id;
         Aggr_Lo : Node_Id;
         Aggr_Hi : Node_Id;
         Obj_In  : Node_Id;
         Obj_Lo  : Node_Id;
         Obj_Hi  : Node_Id;

         function Safe_Aggregate (Aggr : Node_Id) return Boolean;
         --  Check recursively that each component of a (sub)aggregate does not
         --  depend on the variable being assigned to.

         function Safe_Component (Expr : Node_Id) return Boolean;
         --  Verify that an expression cannot depend on the variable being
         --  assigned to. Room for improvement here (but less than before).

         --------------------
         -- Safe_Aggregate --
         --------------------

         function Safe_Aggregate (Aggr : Node_Id) return Boolean is
            Expr : Node_Id;

         begin
            if Nkind (Parent (Aggr)) = N_Iterated_Component_Association then
               return False;
            end if;

            if Present (Expressions (Aggr)) then
               Expr := First (Expressions (Aggr));
               while Present (Expr) loop
                  if Nkind (Expr) = N_Aggregate then
                     if not Safe_Aggregate (Expr) then
                        return False;
                     end if;

                  elsif not Safe_Component (Expr) then
                     return False;
                  end if;

                  Next (Expr);
               end loop;
            end if;

            if Present (Component_Associations (Aggr)) then
               Expr := First (Component_Associations (Aggr));
               while Present (Expr) loop
                  if Nkind (Expression (Expr)) = N_Aggregate then
                     if not Safe_Aggregate (Expression (Expr)) then
                        return False;
                     end if;

                  --  If association has a box, no way to determine yet
                  --  whether default can be assigned in place.

                  elsif Box_Present (Expr) then
                     return False;

                  elsif not Safe_Component (Expression (Expr)) then
                     return False;
                  end if;

                  Next (Expr);
               end loop;
            end if;

            return True;
         end Safe_Aggregate;

         --------------------
         -- Safe_Component --
         --------------------

         function Safe_Component (Expr : Node_Id) return Boolean is
            Comp : Node_Id := Expr;

            function Check_Component (Comp : Node_Id) return Boolean;
            --  Do the recursive traversal, after copy

            ---------------------
            -- Check_Component --
            ---------------------

            function Check_Component (Comp : Node_Id) return Boolean is
            begin
               if Is_Overloaded (Comp) then
                  return False;
               end if;

               return Compile_Time_Known_Value (Comp)

                 or else (Is_Entity_Name (Comp)
                           and then Present (Entity (Comp))
                           and then No (Renamed_Object (Entity (Comp))))

                 or else (Nkind (Comp) = N_Attribute_Reference
                           and then Check_Component (Prefix (Comp)))

                 or else (Nkind (Comp) in N_Binary_Op
                           and then Check_Component (Left_Opnd  (Comp))
                           and then Check_Component (Right_Opnd (Comp)))

                 or else (Nkind (Comp) in N_Unary_Op
                           and then Check_Component (Right_Opnd (Comp)))

                 or else (Nkind (Comp) = N_Selected_Component
                           and then Check_Component (Prefix (Comp)))

                 or else (Nkind (Comp) = N_Unchecked_Type_Conversion
                           and then Check_Component (Expression (Comp)));
            end Check_Component;

         --  Start of processing for Safe_Component

         begin
            --  If the component appears in an association that may correspond
            --  to more than one element, it is not analyzed before expansion
            --  into assignments, to avoid side effects. We analyze, but do not
            --  resolve the copy, to obtain sufficient entity information for
            --  the checks that follow. If component is overloaded we assume
            --  an unsafe function call.

            if not Analyzed (Comp) then
               if Is_Overloaded (Expr) then
                  return False;

               elsif Nkind (Expr) = N_Aggregate
                  and then not Is_Others_Aggregate (Expr)
               then
                  return False;

               elsif Nkind (Expr) = N_Allocator then

                  --  For now, too complex to analyze

                  return False;

               elsif Nkind (Parent (Expr)) =
                       N_Iterated_Component_Association
               then
                  --  Ditto for iterated component associations, which in
                  --  general require an enclosing loop and involve nonstatic
                  --  expressions.

                  return False;
               end if;

               Comp := New_Copy_Tree (Expr);
               Set_Parent (Comp, Parent (Expr));
               Analyze (Comp);
            end if;

            if Nkind (Comp) = N_Aggregate then
               return Safe_Aggregate (Comp);
            else
               return Check_Component (Comp);
            end if;
         end Safe_Component;

      --  Start of processing for In_Place_Assign_OK

      begin
         if Present (Component_Associations (N)) then

            --  On assignment, sliding can take place, so we cannot do the
            --  assignment in place unless the bounds of the aggregate are
            --  statically equal to those of the target.

            --  If the aggregate is given by an others choice, the bounds are
            --  derived from the left-hand side, and the assignment is safe if
            --  the expression is.

            if Is_Others_Aggregate (N) then
               return
                 Safe_Component
                  (Expression (First (Component_Associations (N))));
            end if;

            Aggr_In := First_Index (Etype (N));

            if Nkind (Parent (N)) = N_Assignment_Statement then
               Obj_In  := First_Index (Etype (Name (Parent (N))));

            else
               --  Context is an allocator. Check bounds of aggregate against
               --  given type in qualified expression.

               pragma Assert (Nkind (Parent (Parent (N))) = N_Allocator);
               Obj_In :=
                 First_Index (Etype (Entity (Subtype_Mark (Parent (N)))));
            end if;

            while Present (Aggr_In) loop
               Get_Index_Bounds (Aggr_In, Aggr_Lo, Aggr_Hi);
               Get_Index_Bounds (Obj_In, Obj_Lo, Obj_Hi);

               if not Compile_Time_Known_Value (Aggr_Lo)
                 or else not Compile_Time_Known_Value (Obj_Lo)
                 or else not Compile_Time_Known_Value (Obj_Hi)
                 or else Expr_Value (Aggr_Lo) /= Expr_Value (Obj_Lo)
               then
                  return False;

               --  For an assignment statement we require static matching of
               --  bounds. Ditto for an allocator whose qualified expression
               --  is a constrained type. If the expression in the allocator
               --  is an unconstrained array, we accept an upper bound that
               --  is not static, to allow for nonstatic expressions of the
               --  base type. Clearly there are further possibilities (with
               --  diminishing returns) for safely building arrays in place
               --  here.

               elsif Nkind (Parent (N)) = N_Assignment_Statement
                 or else Is_Constrained (Etype (Parent (N)))
               then
                  if not Compile_Time_Known_Value (Aggr_Hi)
                    or else Expr_Value (Aggr_Hi) /= Expr_Value (Obj_Hi)
                  then
                     return False;
                  end if;
               end if;

               Next_Index (Aggr_In);
               Next_Index (Obj_In);
            end loop;
         end if;

         --  Now check the component values themselves

         return Safe_Aggregate (N);
      end In_Place_Assign_OK;

      ------------------
      -- Others_Check --
      ------------------

      procedure Others_Check (Sub_Aggr : Node_Id; Dim : Pos) is
         Aggr_Lo : constant Node_Id := Aggr_Low (Dim);
         Aggr_Hi : constant Node_Id := Aggr_High (Dim);
         --  The bounds of the aggregate for this dimension

         Ind_Typ : constant Entity_Id := Aggr_Index_Typ (Dim);
         --  The index type for this dimension

         Need_To_Check : Boolean := False;

         Choices_Lo : Node_Id := Empty;
         Choices_Hi : Node_Id := Empty;
         --  The lowest and highest discrete choices for a named subaggregate

         Nb_Choices : Int := -1;
         --  The number of discrete non-others choices in this subaggregate

         Nb_Elements : Uint := Uint_0;
         --  The number of elements in a positional aggregate

         Cond : Node_Id := Empty;

         Assoc  : Node_Id;
         Choice : Node_Id;
         Expr   : Node_Id;

      begin
         --  Check if we have an others choice. If we do make sure that this
         --  subaggregate contains at least one element in addition to the
         --  others choice.

         if Range_Checks_Suppressed (Ind_Typ) then
            Need_To_Check := False;

         elsif Present (Expressions (Sub_Aggr))
           and then Present (Component_Associations (Sub_Aggr))
         then
            Need_To_Check := True;

         elsif Present (Component_Associations (Sub_Aggr)) then
            Assoc := Last (Component_Associations (Sub_Aggr));

            if Nkind (First (Choice_List (Assoc))) /= N_Others_Choice then
               Need_To_Check := False;

            else
               --  Count the number of discrete choices. Start with -1 because
               --  the others choice does not count.

               --  Is there some reason we do not use List_Length here ???

               Nb_Choices := -1;
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Choice := First (Choice_List (Assoc));
                  while Present (Choice) loop
                     Nb_Choices := Nb_Choices + 1;
                     Next (Choice);
                  end loop;

                  Next (Assoc);
               end loop;

               --  If there is only an others choice nothing to do

               Need_To_Check := (Nb_Choices > 0);
            end if;

         else
            Need_To_Check := False;
         end if;

         --  If we are dealing with a positional subaggregate with an others
         --  choice then compute the number or positional elements.

         if Need_To_Check and then Present (Expressions (Sub_Aggr)) then
            Expr := First (Expressions (Sub_Aggr));
            Nb_Elements := Uint_0;
            while Present (Expr) loop
               Nb_Elements := Nb_Elements + 1;
               Next (Expr);
            end loop;

         --  If the aggregate contains discrete choices and an others choice
         --  compute the smallest and largest discrete choice values.

         elsif Need_To_Check then
            Compute_Choices_Lo_And_Choices_Hi : declare

               Table : Case_Table_Type (1 .. Nb_Choices);
               --  Used to sort all the different choice values

               J    : Pos := 1;
               Low  : Node_Id;
               High : Node_Id;

            begin
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Choice := First (Choice_List (Assoc));
                  while Present (Choice) loop
                     if Nkind (Choice) = N_Others_Choice then
                        exit;
                     end if;

                     Get_Index_Bounds (Choice, Low, High);
                     Table (J).Choice_Lo := Low;
                     Table (J).Choice_Hi := High;

                     J := J + 1;
                     Next (Choice);
                  end loop;

                  Next (Assoc);
               end loop;

               --  Sort the discrete choices

               Sort_Case_Table (Table);

               Choices_Lo := Table (1).Choice_Lo;
               Choices_Hi := Table (Nb_Choices).Choice_Hi;
            end Compute_Choices_Lo_And_Choices_Hi;
         end if;

         --  If no others choice in this subaggregate, or the aggregate
         --  comprises only an others choice, nothing to do.

         if not Need_To_Check then
            Cond := Empty;

         --  If we are dealing with an aggregate containing an others choice
         --  and positional components, we generate the following test:

         --    if Ind_Typ'Pos (Aggr_Lo) + (Nb_Elements - 1) >
         --            Ind_Typ'Pos (Aggr_Hi)
         --    then
         --       raise Constraint_Error;
         --    end if;

         elsif Nb_Elements > Uint_0 then
            Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  =>
                  Make_Op_Add (Loc,
                    Left_Opnd  =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Ind_Typ, Loc),
                        Attribute_Name => Name_Pos,
                        Expressions    =>
                          New_List
                            (Duplicate_Subexpr_Move_Checks (Aggr_Lo))),
                Right_Opnd => Make_Integer_Literal (Loc, Nb_Elements - 1)),

                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Ind_Typ, Loc),
                    Attribute_Name => Name_Pos,
                    Expressions    => New_List (
                      Duplicate_Subexpr_Move_Checks (Aggr_Hi))));

         --  If we are dealing with an aggregate containing an others choice
         --  and discrete choices we generate the following test:

         --    [constraint_error when
         --      Choices_Lo < Aggr_Lo or else Choices_Hi > Aggr_Hi];

         else
            Cond :=
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Lt (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Choices_Lo),
                    Right_Opnd => Duplicate_Subexpr_Move_Checks (Aggr_Lo)),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Choices_Hi),
                    Right_Opnd => Duplicate_Subexpr (Aggr_Hi)));
         end if;

         if Present (Cond) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Length_Check_Failed));
            --  Questionable reason code, shouldn't that be a
            --  CE_Range_Check_Failed ???
         end if;

         --  Now look inside the subaggregate to see if there is more work

         if Dim < Aggr_Dimension then

            --  Process positional components

            if Present (Expressions (Sub_Aggr)) then
               Expr := First (Expressions (Sub_Aggr));
               while Present (Expr) loop
                  Others_Check (Expr, Dim + 1);
                  Next (Expr);
               end loop;
            end if;

            --  Process component associations

            if Present (Component_Associations (Sub_Aggr)) then
               Assoc := First (Component_Associations (Sub_Aggr));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Others_Check (Expr, Dim + 1);
                  Next (Assoc);
               end loop;
            end if;
         end if;
      end Others_Check;

      -------------------------
      -- Safe_Left_Hand_Side --
      -------------------------

      function Safe_Left_Hand_Side (N : Node_Id) return Boolean is
         function Is_Safe_Index (Indx : Node_Id) return Boolean;
         --  If the left-hand side includes an indexed component, check that
         --  the indexes are free of side effects.

         -------------------
         -- Is_Safe_Index --
         -------------------

         function Is_Safe_Index (Indx : Node_Id) return Boolean is
         begin
            if Is_Entity_Name (Indx) then
               return True;

            elsif Nkind (Indx) = N_Integer_Literal then
               return True;

            elsif Nkind (Indx) = N_Function_Call
              and then Is_Entity_Name (Name (Indx))
              and then Has_Pragma_Pure_Function (Entity (Name (Indx)))
            then
               return True;

            elsif Nkind (Indx) = N_Type_Conversion
              and then Is_Safe_Index (Expression (Indx))
            then
               return True;

            else
               return False;
            end if;
         end Is_Safe_Index;

      --  Start of processing for Safe_Left_Hand_Side

      begin
         if Is_Entity_Name (N) then
            return True;

         elsif Nkind_In (N, N_Explicit_Dereference, N_Selected_Component)
           and then Safe_Left_Hand_Side (Prefix (N))
         then
            return True;

         elsif Nkind (N) = N_Indexed_Component
           and then Safe_Left_Hand_Side (Prefix (N))
           and then Is_Safe_Index (First (Expressions (N)))
         then
            return True;

         elsif Nkind (N) = N_Unchecked_Type_Conversion then
            return Safe_Left_Hand_Side (Expression (N));

         else
            return False;
         end if;
      end Safe_Left_Hand_Side;

      --  Local variables

      Tmp : Entity_Id;
      --  Holds the temporary aggregate value

      Tmp_Decl : Node_Id;
      --  Holds the declaration of Tmp

      Aggr_Code   : List_Id;
      Parent_Node : Node_Id;
      Parent_Kind : Node_Kind;

   --  Start of processing for Expand_Array_Aggregate

   begin
      --  Do not touch the special aggregates of attributes used for Asm calls

      if Is_RTE (Ctyp, RE_Asm_Input_Operand)
        or else Is_RTE (Ctyp, RE_Asm_Output_Operand)
      then
         return;

      --  Do not expand an aggregate for an array type which contains tasks if
      --  the aggregate is associated with an unexpanded return statement of a
      --  build-in-place function. The aggregate is expanded when the related
      --  return statement (rewritten into an extended return) is processed.
      --  This delay ensures that any temporaries and initialization code
      --  generated for the aggregate appear in the proper return block and
      --  use the correct _chain and _master.

      elsif Has_Task (Base_Type (Etype (N)))
        and then Nkind (Parent (N)) = N_Simple_Return_Statement
        and then Is_Build_In_Place_Function
                   (Return_Applies_To (Return_Statement_Entity (Parent (N))))
      then
         return;

      --  Do not attempt expansion if error already detected. We may reach this
      --  point in spite of previous errors when compiling with -gnatq, to
      --  force all possible errors (this is the usual ACATS mode).

      elsif Error_Posted (N) then
         return;
      end if;

      --  If the semantic analyzer has determined that aggregate N will raise
      --  Constraint_Error at run time, then the aggregate node has been
      --  replaced with an N_Raise_Constraint_Error node and we should
      --  never get here.

      pragma Assert (not Raises_Constraint_Error (N));

      --  STEP 1a

      --  Check that the index range defined by aggregate bounds is
      --  compatible with corresponding index subtype.

      Index_Compatibility_Check : declare
         Aggr_Index_Range : Node_Id := First_Index (Typ);
         --  The current aggregate index range

         Index_Constraint : Node_Id := First_Index (Etype (Typ));
         --  The corresponding index constraint against which we have to
         --  check the above aggregate index range.

      begin
         Compute_Others_Present (N, 1);

         for J in 1 .. Aggr_Dimension loop
            --  There is no need to emit a check if an others choice is present
            --  for this array aggregate dimension since in this case one of
            --  N's subaggregates has taken its bounds from the context and
            --  these bounds must have been checked already. In addition all
            --  subaggregates corresponding to the same dimension must all have
            --  the same bounds (checked in (c) below).

            if not Range_Checks_Suppressed (Etype (Index_Constraint))
              and then not Others_Present (J)
            then
               --  We don't use Checks.Apply_Range_Check here because it emits
               --  a spurious check. Namely it checks that the range defined by
               --  the aggregate bounds is nonempty. But we know this already
               --  if we get here.

               Check_Bounds (Aggr_Index_Range, Index_Constraint);
            end if;

            --  Save the low and high bounds of the aggregate index as well as
            --  the index type for later use in checks (b) and (c) below.

            Aggr_Low  (J) := Low_Bound (Aggr_Index_Range);
            Aggr_High (J) := High_Bound (Aggr_Index_Range);

            Aggr_Index_Typ (J) := Etype (Index_Constraint);

            Next_Index (Aggr_Index_Range);
            Next_Index (Index_Constraint);
         end loop;
      end Index_Compatibility_Check;

      --  STEP 1b

      --  If an others choice is present check that no aggregate index is
      --  outside the bounds of the index constraint.

      Others_Check (N, 1);

      --  STEP 1c

      --  For multidimensional arrays make sure that all subaggregates
      --  corresponding to the same dimension have the same bounds.

      if Aggr_Dimension > 1 then
         Check_Same_Aggr_Bounds (N, 1);
      end if;

      --  STEP 1d

      --  If we have a default component value, or simple initialization is
      --  required for the component type, then we replace <> in component
      --  associations by the required default value.

      declare
         Default_Val : Node_Id;
         Assoc       : Node_Id;

      begin
         if (Present (Default_Aspect_Component_Value (Typ))
              or else Needs_Simple_Initialization (Ctyp))
           and then Present (Component_Associations (N))
         then
            Assoc := First (Component_Associations (N));
            while Present (Assoc) loop
               if Nkind (Assoc) = N_Component_Association
                 and then Box_Present (Assoc)
               then
                  Set_Box_Present (Assoc, False);

                  if Present (Default_Aspect_Component_Value (Typ)) then
                     Default_Val := Default_Aspect_Component_Value (Typ);
                  else
                     Default_Val := Get_Simple_Init_Val (Ctyp, N);
                  end if;

                  Set_Expression (Assoc, New_Copy_Tree (Default_Val));
                  Analyze_And_Resolve (Expression (Assoc), Ctyp);
               end if;

               Next (Assoc);
            end loop;
         end if;
      end;

      --  STEP 2

      --  Here we test for is packed array aggregate that we can handle at
      --  compile time. If so, return with transformation done. Note that we do
      --  this even if the aggregate is nested, because once we have done this
      --  processing, there is no more nested aggregate.

      if Packed_Array_Aggregate_Handled (N) then
         return;
      end if;

      --  At this point we try to convert to positional form

      if Ekind (Current_Scope) = E_Package
        and then Static_Elaboration_Desired (Current_Scope)
      then
         Convert_To_Positional (N, Max_Others_Replicate => 100);
      else
         Convert_To_Positional (N);
      end if;

      --  if the result is no longer an aggregate (e.g. it may be a string
      --  literal, or a temporary which has the needed value), then we are
      --  done, since there is no longer a nested aggregate.

      if Nkind (N) /= N_Aggregate then
         return;

      --  We are also done if the result is an analyzed aggregate, indicating
      --  that Convert_To_Positional succeeded and reanalyzed the rewritten
      --  aggregate.

      elsif Analyzed (N) and then Is_Rewrite_Substitution (N) then
         return;
      end if;

      --  If all aggregate components are compile-time known and the aggregate
      --  has been flattened, nothing left to do. The same occurs if the
      --  aggregate is used to initialize the components of a statically
      --  allocated dispatch table.

      if Compile_Time_Known_Aggregate (N)
        or else Is_Static_Dispatch_Table_Aggregate (N)
      then
         Set_Expansion_Delayed (N, False);
         return;
      end if;

      --  Now see if back end processing is possible

      if Backend_Processing_Possible (N) then

         --  If the aggregate is static but the constraints are not, build
         --  a static subtype for the aggregate, so that Gigi can place it
         --  in static memory. Perform an unchecked_conversion to the non-
         --  static type imposed by the context.

         declare
            Itype      : constant Entity_Id := Etype (N);
            Index      : Node_Id;
            Needs_Type : Boolean := False;

         begin
            Index := First_Index (Itype);
            while Present (Index) loop
               if not Is_OK_Static_Subtype (Etype (Index)) then
                  Needs_Type := True;
                  exit;
               else
                  Next_Index (Index);
               end if;
            end loop;

            if Needs_Type then
               Build_Constrained_Type (Positional => True);
               Rewrite (N, Unchecked_Convert_To (Itype, N));
               Analyze (N);
            end if;
         end;

         return;
      end if;

      --  STEP 3

      --  Delay expansion for nested aggregates: it will be taken care of when
      --  the parent aggregate is expanded.

      Parent_Node := Parent (N);
      Parent_Kind := Nkind (Parent_Node);

      if Parent_Kind = N_Qualified_Expression then
         Parent_Node := Parent (Parent_Node);
         Parent_Kind := Nkind (Parent_Node);
      end if;

      if Parent_Kind = N_Aggregate
        or else Parent_Kind = N_Extension_Aggregate
        or else Parent_Kind = N_Component_Association
        or else (Parent_Kind = N_Object_Declaration
                  and then Needs_Finalization (Typ))
        or else (Parent_Kind = N_Assignment_Statement
                  and then Inside_Init_Proc)
      then
         Set_Expansion_Delayed (N, not Static_Array_Aggregate (N));
         return;
      end if;

      --  STEP 4

      --  Look if in place aggregate expansion is possible

      --  For object declarations we build the aggregate in place, unless
      --  the array is bit-packed.

      --  For assignments we do the assignment in place if all the component
      --  associations have compile-time known values, or are default-
      --  initialized limited components, e.g. tasks. For other cases we
      --  create a temporary. The analysis for safety of on-line assignment
      --  is delicate, i.e. we don't know how to do it fully yet ???

      --  For allocators we assign to the designated object in place if the
      --  aggregate meets the same conditions as other in-place assignments.
      --  In this case the aggregate may not come from source but was created
      --  for default initialization, e.g. with Initialize_Scalars.

      if Requires_Transient_Scope (Typ) then
         Establish_Transient_Scope (N, Manage_Sec_Stack => False);
      end if;

      --  An array of limited components is built in place

      if Is_Limited_Type (Typ) then
         Maybe_In_Place_OK := True;

      elsif Has_Default_Init_Comps (N) then
         Maybe_In_Place_OK := False;

      elsif Is_Bit_Packed_Array (Typ)
        or else Has_Controlled_Component (Typ)
      then
         Maybe_In_Place_OK := False;

      else
         Maybe_In_Place_OK :=
          (Nkind (Parent (N)) = N_Assignment_Statement
            and then In_Place_Assign_OK)

            or else
             (Nkind (Parent (Parent (N))) = N_Allocator
              and then In_Place_Assign_OK);
      end if;

      --  If this is an array of tasks, it will be expanded into build-in-place
      --  assignments. Build an activation chain for the tasks now.

      if Has_Task (Etype (N)) then
         Build_Activation_Chain_Entity (N);
      end if;

      --  Perform in-place expansion of aggregate in an object declaration.
      --  Note: actions generated for the aggregate will be captured in an
      --  expression-with-actions statement so that they can be transferred
      --  to freeze actions later if there is an address clause for the
      --  object. (Note: we don't use a block statement because this would
      --  cause generated freeze nodes to be elaborated in the wrong scope).

      --  Do not perform in-place expansion for SPARK 05 because aggregates are
      --  expected to appear in qualified form. In-place expansion eliminates
      --  the qualification and eventually violates this SPARK 05 restiction.

      --  Arrays of limited components must be built in place. The code
      --  previously excluded controlled components but this is an old
      --  oversight: the rules in 7.6 (17) are clear.

      if (not Has_Default_Init_Comps (N)
           or else Is_Limited_Type (Etype (N)))
        and then Comes_From_Source (Parent_Node)
        and then Parent_Kind = N_Object_Declaration
        and then Present (Expression (Parent_Node))
        and then not
          Must_Slide (Etype (Defining_Identifier (Parent_Node)), Typ)
        and then not Is_Bit_Packed_Array (Typ)
        and then not Restriction_Check_Required (SPARK_05)
      then
         In_Place_Assign_OK_For_Declaration := True;
         Tmp := Defining_Identifier (Parent_Node);
         Set_No_Initialization (Parent_Node);
         Set_Expression (Parent_Node, Empty);

         --  Set kind and type of the entity, for use in the analysis
         --  of the subsequent assignments. If the nominal type is not
         --  constrained, build a subtype from the known bounds of the
         --  aggregate. If the declaration has a subtype mark, use it,
         --  otherwise use the itype of the aggregate.

         Set_Ekind (Tmp, E_Variable);

         if not Is_Constrained (Typ) then
            Build_Constrained_Type (Positional => False);

         elsif Is_Entity_Name (Object_Definition (Parent_Node))
           and then Is_Constrained (Entity (Object_Definition (Parent_Node)))
         then
            Set_Etype (Tmp, Entity (Object_Definition (Parent_Node)));

         else
            Set_Size_Known_At_Compile_Time (Typ, False);
            Set_Etype (Tmp, Typ);
         end if;

      elsif Maybe_In_Place_OK
        and then Nkind (Parent (N)) = N_Qualified_Expression
        and then Nkind (Parent (Parent (N))) = N_Allocator
      then
         Set_Expansion_Delayed (N);
         return;

      --  Limited arrays in return statements are expanded when
      --  enclosing construct is expanded.

      elsif Maybe_In_Place_OK
        and then Nkind (Parent (N)) = N_Simple_Return_Statement
      then
         Set_Expansion_Delayed (N);
         return;

      --  In the remaining cases the aggregate is the RHS of an assignment

      elsif Maybe_In_Place_OK
        and then Safe_Left_Hand_Side (Name (Parent (N)))
      then
         Tmp := Name (Parent (N));

         if Etype (Tmp) /= Etype (N) then
            Apply_Length_Check (N, Etype (Tmp));

            if Nkind (N) = N_Raise_Constraint_Error then

               --  Static error, nothing further to expand

               return;
            end if;
         end if;

      --  If a slice assignment has an aggregate with a single others_choice,
      --  the assignment can be done in place even if bounds are not static,
      --  by converting it into a loop over the discrete range of the slice.

      elsif Maybe_In_Place_OK
        and then Nkind (Name (Parent (N))) = N_Slice
        and then Is_Others_Aggregate (N)
      then
         Tmp := Name (Parent (N));

         --  Set type of aggregate to be type of lhs in assignment, in order
         --  to suppress redundant length checks.

         Set_Etype (N, Etype (Tmp));

      --  Step 5

      --  In place aggregate expansion is not possible

      else
         Maybe_In_Place_OK := False;
         Tmp := Make_Temporary (Loc, 'A', N);
         Tmp_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tmp,
             Object_Definition   => New_Occurrence_Of (Typ, Loc));
         Set_No_Initialization (Tmp_Decl, True);
         Set_Warnings_Off (Tmp);

         --  If we are within a loop, the temporary will be pushed on the
         --  stack at each iteration. If the aggregate is the expression
         --  for an allocator, it will be immediately copied to the heap
         --  and can be reclaimed at once. We create a transient scope
         --  around the aggregate for this purpose.

         if Ekind (Current_Scope) = E_Loop
           and then Nkind (Parent (Parent (N))) = N_Allocator
         then
            Establish_Transient_Scope (N, Manage_Sec_Stack => False);
         end if;

         Insert_Action (N, Tmp_Decl);
      end if;

      --  Construct and insert the aggregate code. We can safely suppress index
      --  checks because this code is guaranteed not to raise CE on index
      --  checks. However we should *not* suppress all checks.

      declare
         Target : Node_Id;

      begin
         if Nkind (Tmp) = N_Defining_Identifier then
            Target := New_Occurrence_Of (Tmp, Loc);

         else
            if Has_Default_Init_Comps (N)
              and then not Maybe_In_Place_OK
            then
               --  Ada 2005 (AI-287): This case has not been analyzed???

               raise Program_Error;
            end if;

            --  Name in assignment is explicit dereference

            Target := New_Copy (Tmp);
         end if;

         --  If we are to generate an in place assignment for a declaration or
         --  an assignment statement, and the assignment can be done directly
         --  by the back end, then do not expand further.

         --  ??? We can also do that if in place expansion is not possible but
         --  then we could go into an infinite recursion.

         if (In_Place_Assign_OK_For_Declaration or else Maybe_In_Place_OK)
           and then not CodePeer_Mode
           and then not Modify_Tree_For_C
           and then not Possible_Bit_Aligned_Component (Target)
           and then not Is_Possibly_Unaligned_Slice (Target)
           and then Aggr_Assignment_OK_For_Backend (N)
         then
            if Maybe_In_Place_OK then
               return;
            end if;

            Aggr_Code :=
              New_List (
                Make_Assignment_Statement (Loc,
                  Name       => Target,
                  Expression => New_Copy_Tree (N)));

         else
            Aggr_Code :=
              Build_Array_Aggr_Code (N,
                Ctype       => Ctyp,
                Index       => First_Index (Typ),
                Into        => Target,
                Scalar_Comp => Is_Scalar_Type (Ctyp));
         end if;

         --  Save the last assignment statement associated with the aggregate
         --  when building a controlled object. This reference is utilized by
         --  the finalization machinery when marking an object as successfully
         --  initialized.

         if Needs_Finalization (Typ)
           and then Is_Entity_Name (Target)
           and then Present (Entity (Target))
           and then Ekind_In (Entity (Target), E_Constant, E_Variable)
         then
            Set_Last_Aggregate_Assignment (Entity (Target), Last (Aggr_Code));
         end if;
      end;

      --  If the aggregate is the expression in a declaration, the expanded
      --  code must be inserted after it. The defining entity might not come
      --  from source if this is part of an inlined body, but the declaration
      --  itself will.

      if Comes_From_Source (Tmp)
        or else
          (Nkind (Parent (N)) = N_Object_Declaration
            and then Comes_From_Source (Parent (N))
            and then Tmp = Defining_Entity (Parent (N)))
      then
         declare
            Node_After : constant Node_Id := Next (Parent_Node);

         begin
            Insert_Actions_After (Parent_Node, Aggr_Code);

            if Parent_Kind = N_Object_Declaration then
               Collect_Initialization_Statements
                 (Obj => Tmp, N => Parent_Node, Node_After => Node_After);
            end if;
         end;

      else
         Insert_Actions (N, Aggr_Code);
      end if;

      --  If the aggregate has been assigned in place, remove the original
      --  assignment.

      if Nkind (Parent (N)) = N_Assignment_Statement
        and then Maybe_In_Place_OK
      then
         Rewrite (Parent (N), Make_Null_Statement (Loc));

      elsif Nkind (Parent (N)) /= N_Object_Declaration
        or else Tmp /= Defining_Identifier (Parent (N))
      then
         Rewrite (N, New_Occurrence_Of (Tmp, Loc));
         Analyze_And_Resolve (N, Typ);
      end if;
   end Expand_Array_Aggregate;

   ------------------------
   -- Expand_N_Aggregate --
   ------------------------

   procedure Expand_N_Aggregate (N : Node_Id) is
   begin
      --  Record aggregate case

      if Is_Record_Type (Etype (N)) then
         Expand_Record_Aggregate (N);

      --  Array aggregate case

      else
         --  A special case, if we have a string subtype with bounds 1 .. N,
         --  where N is known at compile time, and the aggregate is of the
         --  form (others => 'x'), with a single choice and no expressions,
         --  and N is less than 80 (an arbitrary limit for now), then replace
         --  the aggregate by the equivalent string literal (but do not mark
         --  it as static since it is not).

         --  Note: this entire circuit is redundant with respect to code in
         --  Expand_Array_Aggregate that collapses others choices to positional
         --  form, but there are two problems with that circuit:

         --    a) It is limited to very small cases due to ill-understood
         --       interactions with bootstrapping. That limit is removed by
         --       use of the No_Implicit_Loops restriction.

         --    b) It incorrectly ends up with the resulting expressions being
         --       considered static when they are not. For example, the
         --       following test should fail:

         --           pragma Restrictions (No_Implicit_Loops);
         --           package NonSOthers4 is
         --              B  : constant String (1 .. 6) := (others => 'A');
         --              DH : constant String (1 .. 8) := B & "BB";
         --              X : Integer;
         --              pragma Export (C, X, Link_Name => DH);
         --           end;

         --       But it succeeds (DH looks static to pragma Export)

         --    To be sorted out ???

         if Present (Component_Associations (N)) then
            declare
               CA : constant Node_Id := First (Component_Associations (N));
               MX : constant         := 80;

            begin
               if Nkind (First (Choice_List (CA))) = N_Others_Choice
                 and then Nkind (Expression (CA)) = N_Character_Literal
                 and then No (Expressions (N))
               then
                  declare
                     T  : constant Entity_Id := Etype (N);
                     X  : constant Node_Id   := First_Index (T);
                     EC : constant Node_Id   := Expression (CA);
                     CV : constant Uint      := Char_Literal_Value (EC);
                     CC : constant Int       := UI_To_Int (CV);

                  begin
                     if Nkind (X) = N_Range
                       and then Compile_Time_Known_Value (Low_Bound (X))
                       and then Expr_Value (Low_Bound (X)) = 1
                       and then Compile_Time_Known_Value (High_Bound (X))
                     then
                        declare
                           Hi : constant Uint := Expr_Value (High_Bound (X));

                        begin
                           if Hi <= MX then
                              Start_String;

                              for J in 1 .. UI_To_Int (Hi) loop
                                 Store_String_Char (Char_Code (CC));
                              end loop;

                              Rewrite (N,
                                Make_String_Literal (Sloc (N),
                                  Strval => End_String));

                              if CC >= Int (2 ** 16) then
                                 Set_Has_Wide_Wide_Character (N);
                              elsif CC >= Int (2 ** 8) then
                                 Set_Has_Wide_Character (N);
                              end if;

                              Analyze_And_Resolve (N, T);
                              Set_Is_Static_Expression (N, False);
                              return;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;

         --  Not that special case, so normal expansion of array aggregate

         Expand_Array_Aggregate (N);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Aggregate;

   ------------------------------
   -- Expand_N_Delta_Aggregate --
   ------------------------------

   procedure Expand_N_Delta_Aggregate (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Decl : Node_Id;

   begin
      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Temporary (Loc, 'T'),
          Object_Definition   => New_Occurrence_Of (Typ, Loc),
          Expression          => New_Copy_Tree (Expression (N)));

      if Is_Array_Type (Etype (N)) then
         Expand_Delta_Array_Aggregate (N, New_List (Decl));
      else
         Expand_Delta_Record_Aggregate (N, New_List (Decl));
      end if;
   end Expand_N_Delta_Aggregate;

   ----------------------------------
   -- Expand_Delta_Array_Aggregate --
   ----------------------------------

   procedure Expand_Delta_Array_Aggregate (N : Node_Id; Deltas : List_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Temp  : constant Entity_Id  := Defining_Identifier (First (Deltas));
      Assoc : Node_Id;

      function Generate_Loop (C : Node_Id) return Node_Id;
      --  Generate a loop containing individual component assignments for
      --  choices that are ranges, subtype indications, subtype names, and
      --  iterated component associations.

      -------------------
      -- Generate_Loop --
      -------------------

      function Generate_Loop (C : Node_Id) return Node_Id is
         Sl : constant Source_Ptr := Sloc (C);
         Ix : Entity_Id;

      begin
         if Nkind (Parent (C)) = N_Iterated_Component_Association then
            Ix :=
              Make_Defining_Identifier (Loc,
                Chars => (Chars (Defining_Identifier (Parent (C)))));
         else
            Ix := Make_Temporary (Sl, 'I');
         end if;

         return
           Make_Loop_Statement (Loc,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Sl,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Sl,
                     Defining_Identifier         => Ix,
                     Discrete_Subtype_Definition => New_Copy_Tree (C))),

              Statements      => New_List (
                Make_Assignment_Statement (Sl,
                  Name       =>
                    Make_Indexed_Component (Sl,
                      Prefix      => New_Occurrence_Of (Temp, Sl),
                      Expressions => New_List (New_Occurrence_Of (Ix, Sl))),
                  Expression => New_Copy_Tree (Expression (Assoc)))),
              End_Label       => Empty);
      end Generate_Loop;

      --  Local variables

      Choice : Node_Id;

   --  Start of processing for Expand_Delta_Array_Aggregate

   begin
      Assoc := First (Component_Associations (N));
      while Present (Assoc) loop
         Choice := First (Choice_List (Assoc));
         if Nkind (Assoc) = N_Iterated_Component_Association then
            while Present (Choice) loop
               Append_To (Deltas, Generate_Loop (Choice));
               Next (Choice);
            end loop;

         else
            while Present (Choice) loop

               --  Choice can be given by a range, a subtype indication, a
               --  subtype name, a scalar value, or an entity.

               if Nkind (Choice) = N_Range
                 or else (Is_Entity_Name (Choice)
                           and then Is_Type (Entity (Choice)))
               then
                  Append_To (Deltas, Generate_Loop (Choice));

               elsif Nkind (Choice) = N_Subtype_Indication then
                  Append_To (Deltas,
                    Generate_Loop (Range_Expression (Constraint (Choice))));

               else
                  Append_To (Deltas,
                    Make_Assignment_Statement (Sloc (Choice),
                      Name       =>
                        Make_Indexed_Component (Sloc (Choice),
                          Prefix      => New_Occurrence_Of (Temp, Loc),
                          Expressions => New_List (New_Copy_Tree (Choice))),
                      Expression => New_Copy_Tree (Expression (Assoc))));
               end if;

               Next (Choice);
            end loop;
         end if;

         Next (Assoc);
      end loop;

      Insert_Actions (N, Deltas);
      Rewrite (N, New_Occurrence_Of (Temp, Loc));
   end Expand_Delta_Array_Aggregate;

   -----------------------------------
   -- Expand_Delta_Record_Aggregate --
   -----------------------------------

   procedure Expand_Delta_Record_Aggregate (N : Node_Id; Deltas : List_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Temp   : constant Entity_Id  := Defining_Identifier (First (Deltas));
      Assoc  : Node_Id;
      Choice : Node_Id;

   begin
      Assoc := First (Component_Associations (N));

      while Present (Assoc) loop
         Choice := First (Choice_List (Assoc));
         while Present (Choice) loop
            Append_To (Deltas,
              Make_Assignment_Statement (Sloc (Choice),
                Name       =>
                  Make_Selected_Component (Sloc (Choice),
                    Prefix        => New_Occurrence_Of (Temp, Loc),
                    Selector_Name => Make_Identifier (Loc, Chars (Choice))),
                Expression => New_Copy_Tree (Expression (Assoc))));
            Next (Choice);
         end loop;

         Next (Assoc);
      end loop;

      Insert_Actions (N, Deltas);
      Rewrite (N, New_Occurrence_Of (Temp, Loc));
   end Expand_Delta_Record_Aggregate;

   ----------------------------------
   -- Expand_N_Extension_Aggregate --
   ----------------------------------

   --  If the ancestor part is an expression, add a component association for
   --  the parent field. If the type of the ancestor part is not the direct
   --  parent of the expected type, build recursively the needed ancestors.
   --  If the ancestor part is a subtype_mark, replace aggregate with a
   --  declaration for a temporary of the expected type, followed by
   --  individual assignments to the given components.

   procedure Expand_N_Extension_Aggregate (N : Node_Id) is
      A   : constant Node_Id    := Ancestor_Part (N);
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      --  If the ancestor is a subtype mark, an init proc must be called
      --  on the resulting object which thus has to be materialized in
      --  the front-end

      if Is_Entity_Name (A) and then Is_Type (Entity (A)) then
         Convert_To_Assignments (N, Typ);

      --  The extension aggregate is transformed into a record aggregate
      --  of the following form (c1 and c2 are inherited components)

      --   (Exp with c3 => a, c4 => b)
      --      ==> (c1 => Exp.c1, c2 => Exp.c2, c3 => a, c4 => b)

      else
         Set_Etype (N, Typ);

         if Tagged_Type_Expansion then
            Expand_Record_Aggregate (N,
              Orig_Tag    =>
                New_Occurrence_Of
                  (Node (First_Elmt (Access_Disp_Table (Typ))), Loc),
              Parent_Expr => A);

         --  No tag is needed in the case of a VM

         else
            Expand_Record_Aggregate (N, Parent_Expr => A);
         end if;
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Extension_Aggregate;

   -----------------------------
   -- Expand_Record_Aggregate --
   -----------------------------

   procedure Expand_Record_Aggregate
     (N           : Node_Id;
      Orig_Tag    : Node_Id := Empty;
      Parent_Expr : Node_Id := Empty)
   is
      Loc      : constant Source_Ptr := Sloc  (N);
      Comps    : constant List_Id    := Component_Associations (N);
      Typ      : constant Entity_Id  := Etype (N);
      Base_Typ : constant Entity_Id  := Base_Type (Typ);

      Static_Components : Boolean := True;
      --  Flag to indicate whether all components are compile-time known,
      --  and the aggregate can be constructed statically and handled by
      --  the back-end. Set to False by Component_OK_For_Backend.

      procedure Build_Back_End_Aggregate;
      --  Build a proper aggregate to be handled by the back-end

      function Compile_Time_Known_Composite_Value (N : Node_Id) return Boolean;
      --  Returns true if N is an expression of composite type which can be
      --  fully evaluated at compile time without raising constraint error.
      --  Such expressions can be passed as is to Gigi without any expansion.
      --
      --  This returns true for N_Aggregate with Compile_Time_Known_Aggregate
      --  set and constants whose expression is such an aggregate, recursively.

      function Component_OK_For_Backend return Boolean;
      --  Check for presence of a component which makes it impossible for the
      --  backend to process the aggregate, thus requiring the use of a series
      --  of assignment statements. Cases checked for are a nested aggregate
      --  needing Late_Expansion, the presence of a tagged component which may
      --  need tag adjustment, and a bit unaligned component reference.
      --
      --  We also force expansion into assignments if a component is of a
      --  mutable type (including a private type with discriminants) because
      --  in that case the size of the component to be copied may be smaller
      --  than the side of the target, and there is no simple way for gigi
      --  to compute the size of the object to be copied.
      --
      --  NOTE: This is part of the ongoing work to define precisely the
      --  interface between front-end and back-end handling of aggregates.
      --  In general it is desirable to pass aggregates as they are to gigi,
      --  in order to minimize elaboration code. This is one case where the
      --  semantics of Ada complicate the analysis and lead to anomalies in
      --  the gcc back-end if the aggregate is not expanded into assignments.
      --
      --  NOTE: This sets the global Static_Components to False in most, but
      --  not all, cases when it returns False.

      function Has_Per_Object_Constraint (L : List_Id) return Boolean;
      --  Return True if any element of L has Has_Per_Object_Constraint set.
      --  L should be the Choices component of an N_Component_Association.

      function Has_Visible_Private_Ancestor (Id : E) return Boolean;
      --  If any ancestor of the current type is private, the aggregate
      --  cannot be built in place. We cannot rely on Has_Private_Ancestor,
      --  because it will not be set when type and its parent are in the
      --  same scope, and the parent component needs expansion.

      function Top_Level_Aggregate (N : Node_Id) return Node_Id;
      --  For nested aggregates return the ultimate enclosing aggregate; for
      --  non-nested aggregates return N.

      ------------------------------
      -- Build_Back_End_Aggregate --
      ------------------------------

      procedure Build_Back_End_Aggregate is
         Comp      : Entity_Id;
         New_Comp  : Node_Id;
         Tag_Value : Node_Id;

      begin
         if Nkind (N) = N_Aggregate then

            --  If the aggregate is static and can be handled by the back-end,
            --  nothing left to do.

            if Static_Components then
               Set_Compile_Time_Known_Aggregate (N);
               Set_Expansion_Delayed (N, False);
            end if;
         end if;

         --  If no discriminants, nothing special to do

         if not Has_Discriminants (Typ) then
            null;

         --  Case of discriminants present

         elsif Is_Derived_Type (Typ) then

            --  For untagged types, non-stored discriminants are replaced with
            --  stored discriminants, which are the ones that gigi uses to
            --  describe the type and its components.

            Generate_Aggregate_For_Derived_Type : declare
               procedure Prepend_Stored_Values (T : Entity_Id);
               --  Scan the list of stored discriminants of the type, and add
               --  their values to the aggregate being built.

               ---------------------------
               -- Prepend_Stored_Values --
               ---------------------------

               procedure Prepend_Stored_Values (T : Entity_Id) is
                  Discr      : Entity_Id;
                  First_Comp : Node_Id := Empty;

               begin
                  Discr := First_Stored_Discriminant (T);
                  while Present (Discr) loop
                     New_Comp :=
                       Make_Component_Association (Loc,
                         Choices    => New_List (
                           New_Occurrence_Of (Discr, Loc)),
                         Expression =>
                           New_Copy_Tree
                             (Get_Discriminant_Value
                                (Discr,
                                 Typ,
                                 Discriminant_Constraint (Typ))));

                     if No (First_Comp) then
                        Prepend_To (Component_Associations (N), New_Comp);
                     else
                        Insert_After (First_Comp, New_Comp);
                     end if;

                     First_Comp := New_Comp;
                     Next_Stored_Discriminant (Discr);
                  end loop;
               end Prepend_Stored_Values;

               --  Local variables

               Constraints : constant List_Id := New_List;

               Discr    : Entity_Id;
               Decl     : Node_Id;
               Num_Disc : Nat := 0;
               Num_Gird : Nat := 0;

            --  Start of processing for Generate_Aggregate_For_Derived_Type

            begin
               --  Remove the associations for the discriminant of derived type

               declare
                  First_Comp : Node_Id;

               begin
                  First_Comp := First (Component_Associations (N));
                  while Present (First_Comp) loop
                     Comp := First_Comp;
                     Next (First_Comp);

                     if Ekind (Entity (First (Choices (Comp)))) =
                          E_Discriminant
                     then
                        Remove (Comp);
                        Num_Disc := Num_Disc + 1;
                     end if;
                  end loop;
               end;

               --  Insert stored discriminant associations in the correct
               --  order. If there are more stored discriminants than new
               --  discriminants, there is at least one new discriminant that
               --  constrains more than one of the stored discriminants. In
               --  this case we need to construct a proper subtype of the
               --  parent type, in order to supply values to all the
               --  components. Otherwise there is one-one correspondence
               --  between the constraints and the stored discriminants.

               Discr := First_Stored_Discriminant (Base_Type (Typ));
               while Present (Discr) loop
                  Num_Gird := Num_Gird + 1;
                  Next_Stored_Discriminant (Discr);
               end loop;

               --  Case of more stored discriminants than new discriminants

               if Num_Gird > Num_Disc then

                  --  Create a proper subtype of the parent type, which is the
                  --  proper implementation type for the aggregate, and convert
                  --  it to the intended target type.

                  Discr := First_Stored_Discriminant (Base_Type (Typ));
                  while Present (Discr) loop
                     New_Comp :=
                       New_Copy_Tree
                         (Get_Discriminant_Value
                            (Discr,
                             Typ,
                             Discriminant_Constraint (Typ)));

                     Append (New_Comp, Constraints);
                     Next_Stored_Discriminant (Discr);
                  end loop;

                  Decl :=
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => Make_Temporary (Loc, 'T'),
                      Subtype_Indication  =>
                        Make_Subtype_Indication (Loc,
                          Subtype_Mark =>
                            New_Occurrence_Of (Etype (Base_Type (Typ)), Loc),
                          Constraint   =>
                            Make_Index_Or_Discriminant_Constraint
                              (Loc, Constraints)));

                  Insert_Action (N, Decl);
                  Prepend_Stored_Values (Base_Type (Typ));

                  Set_Etype (N, Defining_Identifier (Decl));
                  Set_Analyzed (N);

                  Rewrite (N, Unchecked_Convert_To (Typ, N));
                  Analyze (N);

               --  Case where we do not have fewer new discriminants than
               --  stored discriminants, so in this case we can simply use the
               --  stored discriminants of the subtype.

               else
                  Prepend_Stored_Values (Typ);
               end if;
            end Generate_Aggregate_For_Derived_Type;
         end if;

         if Is_Tagged_Type (Typ) then

            --  In the tagged case, _parent and _tag component must be created

            --  Reset Null_Present unconditionally. Tagged records always have
            --  at least one field (the tag or the parent).

            Set_Null_Record_Present (N, False);

            --  When the current aggregate comes from the expansion of an
            --  extension aggregate, the parent expr is replaced by an
            --  aggregate formed by selected components of this expr.

            if Present (Parent_Expr) and then Is_Empty_List (Comps) then
               Comp := First_Component_Or_Discriminant (Typ);
               while Present (Comp) loop

                  --  Skip all expander-generated components

                  if not Comes_From_Source (Original_Record_Component (Comp))
                  then
                     null;

                  else
                     New_Comp :=
                       Make_Selected_Component (Loc,
                         Prefix        =>
                           Unchecked_Convert_To (Typ,
                             Duplicate_Subexpr (Parent_Expr, True)),
                         Selector_Name => New_Occurrence_Of (Comp, Loc));

                     Append_To (Comps,
                       Make_Component_Association (Loc,
                         Choices    => New_List (
                           New_Occurrence_Of (Comp, Loc)),
                         Expression => New_Comp));

                     Analyze_And_Resolve (New_Comp, Etype (Comp));
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;
            end if;

            --  Compute the value for the Tag now, if the type is a root it
            --  will be included in the aggregate right away, otherwise it will
            --  be propagated to the parent aggregate.

            if Present (Orig_Tag) then
               Tag_Value := Orig_Tag;

            elsif not Tagged_Type_Expansion then
               Tag_Value := Empty;

            else
               Tag_Value :=
                 New_Occurrence_Of
                   (Node (First_Elmt (Access_Disp_Table (Typ))), Loc);
            end if;

            --  For a derived type, an aggregate for the parent is formed with
            --  all the inherited components.

            if Is_Derived_Type (Typ) then
               declare
                  First_Comp   : Node_Id;
                  Parent_Comps : List_Id;
                  Parent_Aggr  : Node_Id;
                  Parent_Name  : Node_Id;

               begin
                  --  Remove the inherited component association from the
                  --  aggregate and store them in the parent aggregate

                  First_Comp   := First (Component_Associations (N));
                  Parent_Comps := New_List;
                  while Present (First_Comp)
                    and then
                      Scope (Original_Record_Component
                               (Entity (First (Choices (First_Comp))))) /=
                                                                    Base_Typ
                  loop
                     Comp := First_Comp;
                     Next (First_Comp);
                     Remove (Comp);
                     Append (Comp, Parent_Comps);
                  end loop;

                  Parent_Aggr :=
                    Make_Aggregate (Loc,
                      Component_Associations => Parent_Comps);
                  Set_Etype (Parent_Aggr, Etype (Base_Type (Typ)));

                  --  Find the _parent component

                  Comp := First_Component (Typ);
                  while Chars (Comp) /= Name_uParent loop
                     Comp := Next_Component (Comp);
                  end loop;

                  Parent_Name := New_Occurrence_Of (Comp, Loc);

                  --  Insert the parent aggregate

                  Prepend_To (Component_Associations (N),
                    Make_Component_Association (Loc,
                      Choices    => New_List (Parent_Name),
                      Expression => Parent_Aggr));

                  --  Expand recursively the parent propagating the right Tag

                  Expand_Record_Aggregate
                    (Parent_Aggr, Tag_Value, Parent_Expr);

                  --  The ancestor part may be a nested aggregate that has
                  --  delayed expansion: recheck now.

                  if not Component_OK_For_Backend then
                     Convert_To_Assignments (N, Typ);
                  end if;
               end;

            --  For a root type, the tag component is added (unless compiling
            --  for the VMs, where tags are implicit).

            elsif Tagged_Type_Expansion then
               declare
                  Tag_Name  : constant Node_Id :=
                                New_Occurrence_Of
                                  (First_Tag_Component (Typ), Loc);
                  Typ_Tag   : constant Entity_Id := RTE (RE_Tag);
                  Conv_Node : constant Node_Id :=
                                Unchecked_Convert_To (Typ_Tag, Tag_Value);

               begin
                  Set_Etype (Conv_Node, Typ_Tag);
                  Prepend_To (Component_Associations (N),
                    Make_Component_Association (Loc,
                      Choices    => New_List (Tag_Name),
                      Expression => Conv_Node));
               end;
            end if;
         end if;
      end Build_Back_End_Aggregate;

      ----------------------------------------
      -- Compile_Time_Known_Composite_Value --
      ----------------------------------------

      function Compile_Time_Known_Composite_Value
        (N : Node_Id) return Boolean
      is
      begin
         --  If we have an entity name, then see if it is the name of a
         --  constant and if so, test the corresponding constant value.

         if Is_Entity_Name (N) then
            declare
               E : constant Entity_Id := Entity (N);
               V : Node_Id;
            begin
               if Ekind (E) /= E_Constant then
                  return False;
               else
                  V := Constant_Value (E);
                  return Present (V)
                    and then Compile_Time_Known_Composite_Value (V);
               end if;
            end;

         --  We have a value, see if it is compile time known

         else
            if Nkind (N) = N_Aggregate then
               return Compile_Time_Known_Aggregate (N);
            end if;

            --  All other types of values are not known at compile time

            return False;
         end if;

      end Compile_Time_Known_Composite_Value;

      ------------------------------
      -- Component_OK_For_Backend --
      ------------------------------

      function Component_OK_For_Backend return Boolean is
         C      : Node_Id;
         Expr_Q : Node_Id;

      begin
         if No (Comps) then
            return True;
         end if;

         C := First (Comps);
         while Present (C) loop

            --  If the component has box initialization, expansion is needed
            --  and component is not ready for backend.

            if Box_Present (C) then
               return False;
            end if;

            if Nkind (Expression (C)) = N_Qualified_Expression then
               Expr_Q := Expression (Expression (C));
            else
               Expr_Q := Expression (C);
            end if;

            --  Return False for array components whose bounds raise
            --  constraint error.

            declare
               Comp : constant Entity_Id := First (Choices (C));
               Indx : Node_Id;

            begin
               if Present (Etype (Comp))
                 and then Is_Array_Type (Etype (Comp))
               then
                  Indx := First_Index (Etype (Comp));
                  while Present (Indx) loop
                     if Nkind (Type_Low_Bound (Etype (Indx))) =
                          N_Raise_Constraint_Error
                       or else Nkind (Type_High_Bound (Etype (Indx))) =
                                 N_Raise_Constraint_Error
                     then
                        return False;
                     end if;

                     Indx := Next_Index (Indx);
                  end loop;
               end if;
            end;

            --  Return False if the aggregate has any associations for tagged
            --  components that may require tag adjustment.

            --  These are cases where the source expression may have a tag that
            --  could differ from the component tag (e.g., can occur for type
            --  conversions and formal parameters). (Tag adjustment not needed
            --  if Tagged_Type_Expansion because object tags are implicit in
            --  the machine.)

            if Is_Tagged_Type (Etype (Expr_Q))
              and then
                (Nkind (Expr_Q) = N_Type_Conversion
                  or else
                    (Is_Entity_Name (Expr_Q)
                      and then Is_Formal (Entity (Expr_Q))))
              and then Tagged_Type_Expansion
            then
               Static_Components := False;
               return False;

            elsif Is_Delayed_Aggregate (Expr_Q) then
               Static_Components := False;
               return False;

            elsif Nkind (Expr_Q) = N_Quantified_Expression then
               Static_Components := False;
               return False;

            elsif Possible_Bit_Aligned_Component (Expr_Q) then
               Static_Components := False;
               return False;

            elsif Modify_Tree_For_C
              and then Nkind (C) = N_Component_Association
              and then Has_Per_Object_Constraint (Choices (C))
            then
               Static_Components := False;
               return False;

            elsif Modify_Tree_For_C
              and then Nkind (Expr_Q) = N_Identifier
              and then Is_Array_Type (Etype (Expr_Q))
            then
               Static_Components := False;
               return False;

            elsif Modify_Tree_For_C
              and then Nkind (Expr_Q) = N_Type_Conversion
              and then Is_Array_Type (Etype (Expr_Q))
            then
               Static_Components := False;
               return False;
            end if;

            if Is_Elementary_Type (Etype (Expr_Q)) then
               if not Compile_Time_Known_Value (Expr_Q) then
                  Static_Components := False;
               end if;

            elsif not Compile_Time_Known_Composite_Value (Expr_Q) then
               Static_Components := False;

               if Is_Private_Type (Etype (Expr_Q))
                 and then Has_Discriminants (Etype (Expr_Q))
               then
                  return False;
               end if;
            end if;

            Next (C);
         end loop;

         return True;
      end Component_OK_For_Backend;

      -------------------------------
      -- Has_Per_Object_Constraint --
      -------------------------------

      function Has_Per_Object_Constraint (L : List_Id) return Boolean is
         N : Node_Id := First (L);
      begin
         while Present (N) loop
            if Is_Entity_Name (N)
              and then Present (Entity (N))
              and then Has_Per_Object_Constraint (Entity (N))
            then
               return True;
            end if;

            Next (N);
         end loop;

         return False;
      end Has_Per_Object_Constraint;

      -----------------------------------
      --  Has_Visible_Private_Ancestor --
      -----------------------------------

      function Has_Visible_Private_Ancestor (Id : E) return Boolean is
         R  : constant Entity_Id := Root_Type (Id);
         T1 : Entity_Id := Id;

      begin
         loop
            if Is_Private_Type (T1) then
               return True;

            elsif T1 = R then
               return False;

            else
               T1 := Etype (T1);
            end if;
         end loop;
      end Has_Visible_Private_Ancestor;

      -------------------------
      -- Top_Level_Aggregate --
      -------------------------

      function Top_Level_Aggregate (N : Node_Id) return Node_Id is
         Aggr : Node_Id;

      begin
         Aggr := N;
         while Present (Parent (Aggr))
           and then Nkind_In (Parent (Aggr), N_Aggregate,
                                             N_Component_Association)
         loop
            Aggr := Parent (Aggr);
         end loop;

         return Aggr;
      end Top_Level_Aggregate;

      --  Local variables

      Top_Level_Aggr : constant Node_Id := Top_Level_Aggregate (N);

   --  Start of processing for Expand_Record_Aggregate

   begin
      --  If the aggregate is to be assigned to an atomic/VFA variable, we have
      --  to prevent a piecemeal assignment even if the aggregate is to be
      --  expanded. We create a temporary for the aggregate, and assign the
      --  temporary instead, so that the back end can generate an atomic move
      --  for it.

      if Is_Atomic_VFA_Aggregate (N) then
         return;

      --  No special management required for aggregates used to initialize
      --  statically allocated dispatch tables

      elsif Is_Static_Dispatch_Table_Aggregate (N) then
         return;
      end if;

      --  Ada 2005 (AI-318-2): We need to convert to assignments if components
      --  are build-in-place function calls. The assignments will each turn
      --  into a build-in-place function call. If components are all static,
      --  we can pass the aggregate to the back end regardless of limitedness.

      --  Extension aggregates, aggregates in extended return statements, and
      --  aggregates for C++ imported types must be expanded.

      if Ada_Version >= Ada_2005 and then Is_Limited_View (Typ) then
         if not Nkind_In (Parent (N), N_Component_Association,
                                      N_Object_Declaration)
         then
            Convert_To_Assignments (N, Typ);

         elsif Nkind (N) = N_Extension_Aggregate
           or else Convention (Typ) = Convention_CPP
         then
            Convert_To_Assignments (N, Typ);

         elsif not Size_Known_At_Compile_Time (Typ)
           or else not Component_OK_For_Backend
           or else not Static_Components
         then
            Convert_To_Assignments (N, Typ);

         --  In all other cases, build a proper aggregate to be handled by
         --  the back-end

         else
            Build_Back_End_Aggregate;
         end if;

      --  Gigi doesn't properly handle temporaries of variable size so we
      --  generate it in the front-end

      elsif not Size_Known_At_Compile_Time (Typ)
        and then Tagged_Type_Expansion
      then
         Convert_To_Assignments (N, Typ);

      --  An aggregate used to initialize a controlled object must be turned
      --  into component assignments as the components themselves may require
      --  finalization actions such as adjustment.

      elsif Needs_Finalization (Typ) then
         Convert_To_Assignments (N, Typ);

      --  Ada 2005 (AI-287): In case of default initialized components we
      --  convert the aggregate into assignments.

      elsif Has_Default_Init_Comps (N) then
         Convert_To_Assignments (N, Typ);

      --  Check components

      elsif not Component_OK_For_Backend then
         Convert_To_Assignments (N, Typ);

      --  If an ancestor is private, some components are not inherited and we
      --  cannot expand into a record aggregate.

      elsif Has_Visible_Private_Ancestor (Typ) then
         Convert_To_Assignments (N, Typ);

      --  ??? The following was done to compile fxacc00.ads in the ACVCs. Gigi
      --  is not able to handle the aggregate for Late_Request.

      elsif Is_Tagged_Type (Typ) and then Has_Discriminants (Typ) then
         Convert_To_Assignments (N, Typ);

      --  If the tagged types covers interface types we need to initialize all
      --  hidden components containing pointers to secondary dispatch tables.

      elsif Is_Tagged_Type (Typ) and then Has_Interfaces (Typ) then
         Convert_To_Assignments (N, Typ);

      --  If some components are mutable, the size of the aggregate component
      --  may be distinct from the default size of the type component, so
      --  we need to expand to insure that the back-end copies the proper
      --  size of the data. However, if the aggregate is the initial value of
      --  a constant, the target is immutable and might be built statically
      --  if components are appropriate.

      elsif Has_Mutable_Components (Typ)
        and then
          (Nkind (Parent (Top_Level_Aggr)) /= N_Object_Declaration
            or else not Constant_Present (Parent (Top_Level_Aggr))
            or else not Static_Components)
      then
         Convert_To_Assignments (N, Typ);

      --  If the type involved has bit aligned components, then we are not sure
      --  that the back end can handle this case correctly.

      elsif Type_May_Have_Bit_Aligned_Components (Typ) then
         Convert_To_Assignments (N, Typ);

      --  When generating C, only generate an aggregate when declaring objects
      --  since C does not support aggregates in e.g. assignment statements.

      elsif Modify_Tree_For_C and then not Is_CCG_Supported_Aggregate (N) then
         Convert_To_Assignments (N, Typ);

      --  In all other cases, build a proper aggregate to be handled by gigi

      else
         Build_Back_End_Aggregate;
      end if;
   end Expand_Record_Aggregate;

   ----------------------------
   -- Has_Default_Init_Comps --
   ----------------------------

   function Has_Default_Init_Comps (N : Node_Id) return Boolean is
      Comps : constant List_Id := Component_Associations (N);
      C     : Node_Id;
      Expr  : Node_Id;

   begin
      pragma Assert (Nkind_In (N, N_Aggregate, N_Extension_Aggregate));

      if No (Comps) then
         return False;
      end if;

      if Has_Self_Reference (N) then
         return True;
      end if;

      --  Check if any direct component has default initialized components

      C := First (Comps);
      while Present (C) loop
         if Box_Present (C) then
            return True;
         end if;

         Next (C);
      end loop;

      --  Recursive call in case of aggregate expression

      C := First (Comps);
      while Present (C) loop
         Expr := Expression (C);

         if Present (Expr)
           and then Nkind_In (Expr, N_Aggregate, N_Extension_Aggregate)
           and then Has_Default_Init_Comps (Expr)
         then
            return True;
         end if;

         Next (C);
      end loop;

      return False;
   end Has_Default_Init_Comps;

   ----------------------------------------
   -- Is_Build_In_Place_Aggregate_Return --
   ----------------------------------------

   function Is_Build_In_Place_Aggregate_Return (N : Node_Id) return Boolean is
      P : Node_Id := Parent (N);

   begin
      while Nkind (P) = N_Qualified_Expression loop
         P := Parent (P);
      end loop;

      if Nkind (P) = N_Simple_Return_Statement then
         null;

      elsif Nkind (Parent (P)) = N_Extended_Return_Statement then
         P := Parent (P);

      else
         return False;
      end if;

      return
        Is_Build_In_Place_Function
          (Return_Applies_To (Return_Statement_Entity (P)));
   end Is_Build_In_Place_Aggregate_Return;

   --------------------------
   -- Is_Delayed_Aggregate --
   --------------------------

   function Is_Delayed_Aggregate (N : Node_Id) return Boolean is
      Node : Node_Id   := N;
      Kind : Node_Kind := Nkind (Node);

   begin
      if Kind = N_Qualified_Expression then
         Node := Expression (Node);
         Kind := Nkind (Node);
      end if;

      if not Nkind_In (Kind, N_Aggregate, N_Extension_Aggregate) then
         return False;
      else
         return Expansion_Delayed (Node);
      end if;
   end Is_Delayed_Aggregate;

   --------------------------------
   -- Is_CCG_Supported_Aggregate --
   --------------------------------

   function Is_CCG_Supported_Aggregate
     (N : Node_Id) return Boolean
   is
      In_Obj_Decl : Boolean := False;
      P           : Node_Id := Parent (N);

   begin
      while Present (P) loop
         if Nkind (P) = N_Object_Declaration then
            In_Obj_Decl := True;
         end if;

         P := Parent (P);
      end loop;

      --  Cases where aggregates are supported by the CCG backend

      if In_Obj_Decl then
         if Nkind (Parent (N)) = N_Object_Declaration then
            return True;

         elsif Nkind (Parent (N)) = N_Qualified_Expression
            and then Nkind_In (Parent (Parent (N)), N_Allocator,
                                                    N_Object_Declaration)
         then
            return True;
         end if;
      end if;

      return False;
   end Is_CCG_Supported_Aggregate;

   ----------------------------------------
   -- Is_Static_Dispatch_Table_Aggregate --
   ----------------------------------------

   function Is_Static_Dispatch_Table_Aggregate (N : Node_Id) return Boolean is
      Typ : constant Entity_Id := Base_Type (Etype (N));

   begin
      return Building_Static_Dispatch_Tables
        and then Tagged_Type_Expansion
        and then RTU_Loaded (Ada_Tags)

         --  Avoid circularity when rebuilding the compiler

        and then Cunit_Entity (Get_Source_Unit (N)) /= RTU_Entity (Ada_Tags)
        and then (Typ = RTE (RE_Dispatch_Table_Wrapper)
                    or else
                  Typ = RTE (RE_Address_Array)
                    or else
                  Typ = RTE (RE_Type_Specific_Data)
                    or else
                  Typ = RTE (RE_Tag_Table)
                    or else
                  (RTE_Available (RE_Interface_Data)
                     and then Typ = RTE (RE_Interface_Data))
                    or else
                  (RTE_Available (RE_Interfaces_Array)
                     and then Typ = RTE (RE_Interfaces_Array))
                    or else
                  (RTE_Available (RE_Interface_Data_Element)
                     and then Typ = RTE (RE_Interface_Data_Element)));
   end Is_Static_Dispatch_Table_Aggregate;

   -----------------------------
   -- Is_Two_Dim_Packed_Array --
   -----------------------------

   function Is_Two_Dim_Packed_Array (Typ : Entity_Id) return Boolean is
      C : constant Int := UI_To_Int (Component_Size (Typ));
   begin
      return Number_Dimensions (Typ) = 2
        and then Is_Bit_Packed_Array (Typ)
        and then (C = 1 or else C = 2 or else C = 4);
   end Is_Two_Dim_Packed_Array;

   --------------------
   -- Late_Expansion --
   --------------------

   function Late_Expansion
     (N      : Node_Id;
      Typ    : Entity_Id;
      Target : Node_Id) return List_Id
   is
      Aggr_Code : List_Id;

   begin
      if Is_Array_Type (Etype (N)) then
         Aggr_Code :=
           Build_Array_Aggr_Code
             (N           => N,
              Ctype       => Component_Type (Etype (N)),
              Index       => First_Index (Typ),
              Into        => Target,
              Scalar_Comp => Is_Scalar_Type (Component_Type (Typ)),
              Indexes     => No_List);

      --  Directly or indirectly (e.g. access protected procedure) a record

      else
         Aggr_Code := Build_Record_Aggr_Code (N, Typ, Target);
      end if;

      --  Save the last assignment statement associated with the aggregate
      --  when building a controlled object. This reference is utilized by
      --  the finalization machinery when marking an object as successfully
      --  initialized.

      if Needs_Finalization (Typ)
        and then Is_Entity_Name (Target)
        and then Present (Entity (Target))
        and then Ekind_In (Entity (Target), E_Constant, E_Variable)
      then
         Set_Last_Aggregate_Assignment (Entity (Target), Last (Aggr_Code));
      end if;

      return Aggr_Code;
   end Late_Expansion;

   ----------------------------------
   -- Make_OK_Assignment_Statement --
   ----------------------------------

   function Make_OK_Assignment_Statement
     (Sloc       : Source_Ptr;
      Name       : Node_Id;
      Expression : Node_Id) return Node_Id
   is
   begin
      Set_Assignment_OK (Name);
      return Make_Assignment_Statement (Sloc, Name, Expression);
   end Make_OK_Assignment_Statement;

   -----------------------
   -- Number_Of_Choices --
   -----------------------

   function Number_Of_Choices (N : Node_Id) return Nat is
      Assoc  : Node_Id;
      Choice : Node_Id;

      Nb_Choices : Nat := 0;

   begin
      if Present (Expressions (N)) then
         return 0;
      end if;

      Assoc := First (Component_Associations (N));
      while Present (Assoc) loop
         Choice := First (Choice_List (Assoc));
         while Present (Choice) loop
            if Nkind (Choice) /= N_Others_Choice then
               Nb_Choices := Nb_Choices + 1;
            end if;

            Next (Choice);
         end loop;

         Next (Assoc);
      end loop;

      return Nb_Choices;
   end Number_Of_Choices;

   ------------------------------------
   -- Packed_Array_Aggregate_Handled --
   ------------------------------------

   --  The current version of this procedure will handle at compile time
   --  any array aggregate that meets these conditions:

   --    One and two dimensional, bit packed
   --    Underlying packed type is modular type
   --    Bounds are within 32-bit Int range
   --    All bounds and values are static

   --  Note: for now, in the 2-D case, we only handle component sizes of
   --  1, 2, 4 (cases where an integral number of elements occupies a byte).

   function Packed_Array_Aggregate_Handled (N : Node_Id) return Boolean is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Ctyp : constant Entity_Id  := Component_Type (Typ);

      Not_Handled : exception;
      --  Exception raised if this aggregate cannot be handled

   begin
      --  Handle one- or two dimensional bit packed array

      if not Is_Bit_Packed_Array (Typ)
        or else Number_Dimensions (Typ) > 2
      then
         return False;
      end if;

      --  If two-dimensional, check whether it can be folded, and transformed
      --  into a one-dimensional aggregate for the Packed_Array_Impl_Type of
      --  the original type.

      if Number_Dimensions (Typ) = 2 then
         return Two_Dim_Packed_Array_Handled (N);
      end if;

      if not Is_Modular_Integer_Type (Packed_Array_Impl_Type (Typ)) then
         return False;
      end if;

      if not Is_Scalar_Type (Ctyp) then
         return False;
      end if;

      declare
         Csiz  : constant Nat := UI_To_Int (Component_Size (Typ));

         Lo : Node_Id;
         Hi : Node_Id;
         --  Bounds of index type

         Lob : Uint;
         Hib : Uint;
         --  Values of bounds if compile time known

         function Get_Component_Val (N : Node_Id) return Uint;
         --  Given a expression value N of the component type Ctyp, returns a
         --  value of Csiz (component size) bits representing this value. If
         --  the value is nonstatic or any other reason exists why the value
         --  cannot be returned, then Not_Handled is raised.

         -----------------------
         -- Get_Component_Val --
         -----------------------

         function Get_Component_Val (N : Node_Id) return Uint is
            Val  : Uint;

         begin
            --  We have to analyze the expression here before doing any further
            --  processing here. The analysis of such expressions is deferred
            --  till expansion to prevent some problems of premature analysis.

            Analyze_And_Resolve (N, Ctyp);

            --  Must have a compile time value. String literals have to be
            --  converted into temporaries as well, because they cannot easily
            --  be converted into their bit representation.

            if not Compile_Time_Known_Value (N)
              or else Nkind (N) = N_String_Literal
            then
               raise Not_Handled;
            end if;

            Val := Expr_Rep_Value (N);

            --  Adjust for bias, and strip proper number of bits

            if Has_Biased_Representation (Ctyp) then
               Val := Val - Expr_Value (Type_Low_Bound (Ctyp));
            end if;

            return Val mod Uint_2 ** Csiz;
         end Get_Component_Val;

      --  Here we know we have a one dimensional bit packed array

      begin
         Get_Index_Bounds (First_Index (Typ), Lo, Hi);

         --  Cannot do anything if bounds are dynamic

         if not Compile_Time_Known_Value (Lo)
              or else
            not Compile_Time_Known_Value (Hi)
         then
            return False;
         end if;

         --  Or are silly out of range of int bounds

         Lob := Expr_Value (Lo);
         Hib := Expr_Value (Hi);

         if not UI_Is_In_Int_Range (Lob)
              or else
            not UI_Is_In_Int_Range (Hib)
         then
            return False;
         end if;

         --  At this stage we have a suitable aggregate for handling at compile
         --  time. The only remaining checks are that the values of expressions
         --  in the aggregate are compile-time known (checks are performed by
         --  Get_Component_Val), and that any subtypes or ranges are statically
         --  known.

         --  If the aggregate is not fully positional at this stage, then
         --  convert it to positional form. Either this will fail, in which
         --  case we can do nothing, or it will succeed, in which case we have
         --  succeeded in handling the aggregate and transforming it into a
         --  modular value, or it will stay an aggregate, in which case we
         --  have failed to create a packed value for it.

         if Present (Component_Associations (N)) then
            Convert_To_Positional
              (N, Max_Others_Replicate => 64, Handle_Bit_Packed => True);
            return Nkind (N) /= N_Aggregate;
         end if;

         --  Otherwise we are all positional, so convert to proper value

         declare
            Lov : constant Int := UI_To_Int (Lob);
            Hiv : constant Int := UI_To_Int (Hib);

            Len : constant Nat := Int'Max (0, Hiv - Lov + 1);
            --  The length of the array (number of elements)

            Aggregate_Val : Uint;
            --  Value of aggregate. The value is set in the low order bits of
            --  this value. For the little-endian case, the values are stored
            --  from low-order to high-order and for the big-endian case the
            --  values are stored from high-order to low-order. Note that gigi
            --  will take care of the conversions to left justify the value in
            --  the big endian case (because of left justified modular type
            --  processing), so we do not have to worry about that here.

            Lit : Node_Id;
            --  Integer literal for resulting constructed value

            Shift : Nat;
            --  Shift count from low order for next value

            Incr : Int;
            --  Shift increment for loop

            Expr : Node_Id;
            --  Next expression from positional parameters of aggregate

            Left_Justified : Boolean;
            --  Set True if we are filling the high order bits of the target
            --  value (i.e. the value is left justified).

         begin
            --  For little endian, we fill up the low order bits of the target
            --  value. For big endian we fill up the high order bits of the
            --  target value (which is a left justified modular value).

            Left_Justified := Bytes_Big_Endian;

            --  Switch justification if using -gnatd8

            if Debug_Flag_8 then
               Left_Justified := not Left_Justified;
            end if;

            --  Switch justfification if reverse storage order

            if Reverse_Storage_Order (Base_Type (Typ)) then
               Left_Justified := not Left_Justified;
            end if;

            if Left_Justified then
               Shift := Csiz * (Len - 1);
               Incr  := -Csiz;
            else
               Shift := 0;
               Incr  := +Csiz;
            end if;

            --  Loop to set the values

            if Len = 0 then
               Aggregate_Val := Uint_0;
            else
               Expr := First (Expressions (N));
               Aggregate_Val := Get_Component_Val (Expr) * Uint_2 ** Shift;

               for J in 2 .. Len loop
                  Shift := Shift + Incr;
                  Next (Expr);
                  Aggregate_Val :=
                    Aggregate_Val + Get_Component_Val (Expr) * Uint_2 ** Shift;
               end loop;
            end if;

            --  Now we can rewrite with the proper value

            Lit := Make_Integer_Literal (Loc, Intval => Aggregate_Val);
            Set_Print_In_Hex (Lit);

            --  Construct the expression using this literal. Note that it is
            --  important to qualify the literal with its proper modular type
            --  since universal integer does not have the required range and
            --  also this is a left justified modular type, which is important
            --  in the big-endian case.

            Rewrite (N,
              Unchecked_Convert_To (Typ,
                Make_Qualified_Expression (Loc,
                  Subtype_Mark =>
                    New_Occurrence_Of (Packed_Array_Impl_Type (Typ), Loc),
                  Expression   => Lit)));

            Analyze_And_Resolve (N, Typ);
            return True;
         end;
      end;

   exception
      when Not_Handled =>
         return False;
   end Packed_Array_Aggregate_Handled;

   ----------------------------
   -- Has_Mutable_Components --
   ----------------------------

   function Has_Mutable_Components (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      Comp := First_Component (Typ);
      while Present (Comp) loop
         if Is_Record_Type (Etype (Comp))
           and then Has_Discriminants (Etype (Comp))
           and then not Is_Constrained (Etype (Comp))
         then
            return True;
         end if;

         Next_Component (Comp);
      end loop;

      return False;
   end Has_Mutable_Components;

   ------------------------------
   -- Initialize_Discriminants --
   ------------------------------

   procedure Initialize_Discriminants (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Bas  : constant Entity_Id  := Base_Type (Typ);
      Par  : constant Entity_Id  := Etype (Bas);
      Decl : constant Node_Id    := Parent (Par);
      Ref  : Node_Id;

   begin
      if Is_Tagged_Type (Bas)
        and then Is_Derived_Type (Bas)
        and then Has_Discriminants (Par)
        and then Has_Discriminants (Bas)
        and then Number_Discriminants (Bas) /= Number_Discriminants (Par)
        and then Nkind (Decl) = N_Full_Type_Declaration
        and then Nkind (Type_Definition (Decl)) = N_Record_Definition
        and then
          Present (Variant_Part (Component_List (Type_Definition (Decl))))
        and then Nkind (N) /= N_Extension_Aggregate
      then

         --   Call init proc to set discriminants.
         --   There should eventually be a special procedure for this ???

         Ref := New_Occurrence_Of (Defining_Identifier (N), Loc);
         Insert_Actions_After (N,
           Build_Initialization_Call (Sloc (N), Ref, Typ));
      end if;
   end Initialize_Discriminants;

   ----------------
   -- Must_Slide --
   ----------------

   function Must_Slide
     (Obj_Type : Entity_Id;
      Typ      : Entity_Id) return Boolean
   is
      L1, L2, H1, H2 : Node_Id;

   begin
      --  No sliding if the type of the object is not established yet, if it is
      --  an unconstrained type whose actual subtype comes from the aggregate,
      --  or if the two types are identical.

      if not Is_Array_Type (Obj_Type) then
         return False;

      elsif not Is_Constrained (Obj_Type) then
         return False;

      elsif Typ = Obj_Type then
         return False;

      else
         --  Sliding can only occur along the first dimension

         Get_Index_Bounds (First_Index (Typ), L1, H1);
         Get_Index_Bounds (First_Index (Obj_Type), L2, H2);

         if not Is_OK_Static_Expression (L1) or else
            not Is_OK_Static_Expression (L2) or else
            not Is_OK_Static_Expression (H1) or else
            not Is_OK_Static_Expression (H2)
         then
            return False;
         else
            return Expr_Value (L1) /= Expr_Value (L2)
                     or else
                   Expr_Value (H1) /= Expr_Value (H2);
         end if;
      end if;
   end Must_Slide;

   ---------------------------------
   -- Process_Transient_Component --
   ---------------------------------

   procedure Process_Transient_Component
     (Loc        : Source_Ptr;
      Comp_Typ   : Entity_Id;
      Init_Expr  : Node_Id;
      Fin_Call   : out Node_Id;
      Hook_Clear : out Node_Id;
      Aggr       : Node_Id := Empty;
      Stmts      : List_Id := No_List)
   is
      procedure Add_Item (Item : Node_Id);
      --  Insert arbitrary node Item into the tree depending on the values of
      --  Aggr and Stmts.

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Item : Node_Id) is
      begin
         if Present (Aggr) then
            Insert_Action (Aggr, Item);
         else
            pragma Assert (Present (Stmts));
            Append_To (Stmts, Item);
         end if;
      end Add_Item;

      --  Local variables

      Hook_Assign : Node_Id;
      Hook_Decl   : Node_Id;
      Ptr_Decl    : Node_Id;
      Res_Decl    : Node_Id;
      Res_Id      : Entity_Id;
      Res_Typ     : Entity_Id;

   --  Start of processing for Process_Transient_Component

   begin
      --  Add the access type, which provides a reference to the function
      --  result. Generate:

      --    type Res_Typ is access all Comp_Typ;

      Res_Typ := Make_Temporary (Loc, 'A');
      Set_Ekind (Res_Typ, E_General_Access_Type);
      Set_Directly_Designated_Type (Res_Typ, Comp_Typ);

      Add_Item
        (Make_Full_Type_Declaration (Loc,
           Defining_Identifier => Res_Typ,
           Type_Definition     =>
             Make_Access_To_Object_Definition (Loc,
               All_Present        => True,
               Subtype_Indication => New_Occurrence_Of (Comp_Typ, Loc))));

      --  Add the temporary which captures the result of the function call.
      --  Generate:

      --    Res : constant Res_Typ := Init_Expr'Reference;

      --  Note that this temporary is effectively a transient object because
      --  its lifetime is bounded by the current array or record component.

      Res_Id := Make_Temporary (Loc, 'R');
      Set_Ekind (Res_Id, E_Constant);
      Set_Etype (Res_Id, Res_Typ);

      --  Mark the transient object as successfully processed to avoid double
      --  finalization.

      Set_Is_Finalized_Transient (Res_Id);

      --  Signal the general finalization machinery that this transient object
      --  should not be considered for finalization actions because its cleanup
      --  will be performed by Process_Transient_Component_Completion.

      Set_Is_Ignored_Transient (Res_Id);

      Res_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Res_Id,
          Constant_Present    => True,
          Object_Definition   => New_Occurrence_Of (Res_Typ, Loc),
          Expression          =>
            Make_Reference (Loc, New_Copy_Tree (Init_Expr)));

      Add_Item (Res_Decl);

      --  Construct all pieces necessary to hook and finalize the transient
      --  result.

      Build_Transient_Object_Statements
        (Obj_Decl    => Res_Decl,
         Fin_Call    => Fin_Call,
         Hook_Assign => Hook_Assign,
         Hook_Clear  => Hook_Clear,
         Hook_Decl   => Hook_Decl,
         Ptr_Decl    => Ptr_Decl);

      --  Add the access type which provides a reference to the transient
      --  result. Generate:

      --    type Ptr_Typ is access all Comp_Typ;

      Add_Item (Ptr_Decl);

      --  Add the temporary which acts as a hook to the transient result.
      --  Generate:

      --    Hook : Ptr_Typ := null;

      Add_Item (Hook_Decl);

      --  Attach the transient result to the hook. Generate:

      --    Hook := Ptr_Typ (Res);

      Add_Item (Hook_Assign);

      --  The original initialization expression now references the value of
      --  the temporary function result. Generate:

      --    Res.all

      Rewrite (Init_Expr,
        Make_Explicit_Dereference (Loc,
          Prefix => New_Occurrence_Of (Res_Id, Loc)));
   end Process_Transient_Component;

   --------------------------------------------
   -- Process_Transient_Component_Completion --
   --------------------------------------------

   procedure Process_Transient_Component_Completion
     (Loc        : Source_Ptr;
      Aggr       : Node_Id;
      Fin_Call   : Node_Id;
      Hook_Clear : Node_Id;
      Stmts      : List_Id)
   is
      Exceptions_OK : constant Boolean :=
                        not Restriction_Active (No_Exception_Propagation);

   begin
      pragma Assert (Present (Hook_Clear));

      --  Generate the following code if exception propagation is allowed:

      --    declare
      --       Abort : constant Boolean := Triggered_By_Abort;
      --         <or>
      --       Abort : constant Boolean := False;  --  no abort

      --       E      : Exception_Occurrence;
      --       Raised : Boolean := False;

      --    begin
      --       [Abort_Defer;]

      --       begin
      --          Hook := null;
      --          [Deep_]Finalize (Res.all);

      --       exception
      --          when others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E,
      --                  Get_Curent_Excep.all.all);
      --             end if;
      --       end;

      --       [Abort_Undefer;]

      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      if Exceptions_OK then
         Abort_And_Exception : declare
            Blk_Decls : constant List_Id := New_List;
            Blk_Stmts : constant List_Id := New_List;
            Fin_Stmts : constant List_Id := New_List;

            Fin_Data : Finalization_Exception_Data;

         begin
            --  Create the declarations of the two flags and the exception
            --  occurrence.

            Build_Object_Declarations (Fin_Data, Blk_Decls, Loc);

            --  Generate:
            --    Abort_Defer;

            if Abort_Allowed then
               Append_To (Blk_Stmts,
                 Build_Runtime_Call (Loc, RE_Abort_Defer));
            end if;

            --  Wrap the hook clear and the finalization call in order to trap
            --  a potential exception.

            Append_To (Fin_Stmts, Hook_Clear);

            if Present (Fin_Call) then
               Append_To (Fin_Stmts, Fin_Call);
            end if;

            Append_To (Blk_Stmts,
              Make_Block_Statement (Loc,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements         => Fin_Stmts,
                    Exception_Handlers => New_List (
                      Build_Exception_Handler (Fin_Data)))));

            --  Generate:
            --    Abort_Undefer;

            if Abort_Allowed then
               Append_To (Blk_Stmts,
                 Build_Runtime_Call (Loc, RE_Abort_Undefer));
            end if;

            --  Reraise the potential exception with a proper "upgrade" to
            --  Program_Error if needed.

            Append_To (Blk_Stmts, Build_Raise_Statement (Fin_Data));

            --  Wrap everything in a block

            Append_To (Stmts,
              Make_Block_Statement (Loc,
                Declarations               => Blk_Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Blk_Stmts)));
         end Abort_And_Exception;

      --  Generate the following code if exception propagation is not allowed
      --  and aborts are allowed:

      --    begin
      --       Abort_Defer;
      --       Hook := null;
      --       [Deep_]Finalize (Res.all);
      --    at end
      --       Abort_Undefer_Direct;
      --    end;

      elsif Abort_Allowed then
         Abort_Only : declare
            Blk_Stmts : constant List_Id := New_List;

         begin
            Append_To (Blk_Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));
            Append_To (Blk_Stmts, Hook_Clear);

            if Present (Fin_Call) then
               Append_To (Blk_Stmts, Fin_Call);
            end if;

            Append_To (Stmts,
              Build_Abort_Undefer_Block (Loc,
                Stmts   => Blk_Stmts,
                Context => Aggr));
         end Abort_Only;

      --  Otherwise generate:

      --    Hook := null;
      --    [Deep_]Finalize (Res.all);

      else
         Append_To (Stmts, Hook_Clear);

         if Present (Fin_Call) then
            Append_To (Stmts, Fin_Call);
         end if;
      end if;
   end Process_Transient_Component_Completion;

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

   ----------------------------
   -- Static_Array_Aggregate --
   ----------------------------

   function Static_Array_Aggregate (N : Node_Id) return Boolean is
      function Is_Static_Component (Nod : Node_Id) return Boolean;
      --  Return True if Nod has a compile-time known value and can be passed
      --  as is to the back-end without further expansion.

      ---------------------------
      --  Is_Static_Component  --
      ---------------------------

      function Is_Static_Component (Nod : Node_Id) return Boolean is
      begin
         if Nkind_In (Nod, N_Integer_Literal, N_Real_Literal) then
            return True;

         elsif Is_Entity_Name (Nod)
           and then Present (Entity (Nod))
           and then Ekind (Entity (Nod)) = E_Enumeration_Literal
         then
            return True;

         elsif Nkind (Nod) = N_Aggregate
           and then Compile_Time_Known_Aggregate (Nod)
         then
            return True;

         else
            return False;
         end if;
      end Is_Static_Component;

      --  Local variables

      Bounds : constant Node_Id   := Aggregate_Bounds (N);
      Typ    : constant Entity_Id := Etype (N);

      Agg  : Node_Id;
      Expr : Node_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;

   --  Start of processing for Static_Array_Aggregate

   begin
      if Is_Packed (Typ) or else Has_Discriminants (Component_Type (Typ)) then
         return False;
      end if;

      if Present (Bounds)
        and then Nkind (Bounds) = N_Range
        and then Nkind (Low_Bound  (Bounds)) = N_Integer_Literal
        and then Nkind (High_Bound (Bounds)) = N_Integer_Literal
      then
         Lo := Low_Bound  (Bounds);
         Hi := High_Bound (Bounds);

         if No (Component_Associations (N)) then

            --  Verify that all components are static

            Expr := First (Expressions (N));
            while Present (Expr) loop
               if not Is_Static_Component (Expr) then
                  return False;
               end if;

               Next (Expr);
            end loop;

            return True;

         else
            --  We allow only a single named association, either a static
            --  range or an others_clause, with a static expression.

            Expr := First (Component_Associations (N));

            if Present (Expressions (N)) then
               return False;

            elsif Present (Next (Expr)) then
               return False;

            elsif Present (Next (First (Choice_List (Expr)))) then
               return False;

            else
               --  The aggregate is static if all components are literals,
               --  or else all its components are static aggregates for the
               --  component type. We also limit the size of a static aggregate
               --  to prevent runaway static expressions.

               if not Is_Static_Component (Expression (Expr)) then
                  return False;
               end if;

               if not Aggr_Size_OK (N, Typ) then
                  return False;
               end if;

               --  Create a positional aggregate with the right number of
               --  copies of the expression.

               Agg := Make_Aggregate (Sloc (N), New_List, No_List);

               for I in UI_To_Int (Intval (Lo)) .. UI_To_Int (Intval (Hi))
               loop
                  Append_To (Expressions (Agg), New_Copy (Expression (Expr)));

                  --  The copied expression must be analyzed and resolved.
                  --  Besides setting the type, this ensures that static
                  --  expressions are appropriately marked as such.

                  Analyze_And_Resolve
                    (Last (Expressions (Agg)), Component_Type (Typ));
               end loop;

               Set_Aggregate_Bounds (Agg, Bounds);
               Set_Etype (Agg, Typ);
               Set_Analyzed (Agg);
               Rewrite (N, Agg);
               Set_Compile_Time_Known_Aggregate (N);

               return True;
            end if;
         end if;

      else
         return False;
      end if;
   end Static_Array_Aggregate;

   ----------------------------------
   -- Two_Dim_Packed_Array_Handled --
   ----------------------------------

   function Two_Dim_Packed_Array_Handled (N : Node_Id) return Boolean is
      Loc          : constant Source_Ptr := Sloc (N);
      Typ          : constant Entity_Id  := Etype (N);
      Ctyp         : constant Entity_Id  := Component_Type (Typ);
      Comp_Size    : constant Int        := UI_To_Int (Component_Size (Typ));
      Packed_Array : constant Entity_Id  :=
                       Packed_Array_Impl_Type (Base_Type (Typ));

      One_Comp : Node_Id;
      --  Expression in original aggregate

      One_Dim : Node_Id;
      --  One-dimensional subaggregate

   begin

      --  For now, only deal with cases where an integral number of elements
      --  fit in a single byte. This includes the most common boolean case.

      if not (Comp_Size = 1 or else
              Comp_Size = 2 or else
              Comp_Size = 4)
      then
         return False;
      end if;

      Convert_To_Positional
        (N, Max_Others_Replicate => 64, Handle_Bit_Packed => True);

      --  Verify that all components are static

      if Nkind (N) = N_Aggregate
        and then Compile_Time_Known_Aggregate (N)
      then
         null;

      --  The aggregate may have been reanalyzed and converted already

      elsif Nkind (N) /= N_Aggregate then
         return True;

      --  If component associations remain, the aggregate is not static

      elsif Present (Component_Associations (N)) then
         return False;

      else
         One_Dim := First (Expressions (N));
         while Present (One_Dim) loop
            if Present (Component_Associations (One_Dim)) then
               return False;
            end if;

            One_Comp := First (Expressions (One_Dim));
            while Present (One_Comp) loop
               if not Is_OK_Static_Expression (One_Comp) then
                  return False;
               end if;

               Next (One_Comp);
            end loop;

            Next (One_Dim);
         end loop;
      end if;

      --  Two-dimensional aggregate is now fully positional so pack one
      --  dimension to create a static one-dimensional array, and rewrite
      --  as an unchecked conversion to the original type.

      declare
         Byte_Size : constant Int := UI_To_Int (Component_Size (Packed_Array));
         --  The packed array type is a byte array

         Packed_Num : Nat;
         --  Number of components accumulated in current byte

         Comps : List_Id;
         --  Assembled list of packed values for equivalent aggregate

         Comp_Val : Uint;
         --  Integer value of component

         Incr : Int;
         --  Step size for packing

         Init_Shift : Int;
         --  Endian-dependent start position for packing

         Shift : Int;
         --  Current insertion position

         Val : Int;
         --  Component of packed array being assembled

      begin
         Comps := New_List;
         Val   := 0;
         Packed_Num := 0;

         --  Account for endianness.  See corresponding comment in
         --  Packed_Array_Aggregate_Handled concerning the following.

         if Bytes_Big_Endian
           xor Debug_Flag_8
           xor Reverse_Storage_Order (Base_Type (Typ))
         then
            Init_Shift := Byte_Size - Comp_Size;
            Incr := -Comp_Size;
         else
            Init_Shift := 0;
            Incr := +Comp_Size;
         end if;

         --  Iterate over each subaggregate

         Shift := Init_Shift;
         One_Dim := First (Expressions (N));
         while Present (One_Dim) loop
            One_Comp := First (Expressions (One_Dim));
            while Present (One_Comp) loop
               if Packed_Num = Byte_Size / Comp_Size then

                  --  Byte is complete, add to list of expressions

                  Append (Make_Integer_Literal (Sloc (One_Dim), Val), Comps);
                  Val := 0;
                  Shift := Init_Shift;
                  Packed_Num := 0;

               else
                  Comp_Val := Expr_Rep_Value (One_Comp);

                  --  Adjust for bias, and strip proper number of bits

                  if Has_Biased_Representation (Ctyp) then
                     Comp_Val := Comp_Val - Expr_Value (Type_Low_Bound (Ctyp));
                  end if;

                  Comp_Val := Comp_Val mod Uint_2 ** Comp_Size;
                  Val := UI_To_Int (Val + Comp_Val * Uint_2 ** Shift);
                  Shift := Shift + Incr;
                  One_Comp := Next (One_Comp);
                  Packed_Num := Packed_Num + 1;
               end if;
            end loop;

            One_Dim := Next (One_Dim);
         end loop;

         if Packed_Num > 0 then

            --  Add final incomplete byte if present

            Append (Make_Integer_Literal (Sloc (One_Dim), Val), Comps);
         end if;

         Rewrite (N,
             Unchecked_Convert_To (Typ,
               Make_Qualified_Expression (Loc,
                 Subtype_Mark => New_Occurrence_Of (Packed_Array, Loc),
                 Expression   => Make_Aggregate (Loc, Expressions => Comps))));
         Analyze_And_Resolve (N);
         return True;
      end;
   end Two_Dim_Packed_Array_Handled;

end Exp_Aggr;
