------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with Aspects;  use Aspects;
with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Aggr; use Exp_Aggr;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch7;  use Exp_Ch7;
with Inline;   use Inline;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Urealp;   use Urealp;
with Validsw;  use Validsw;

package body Exp_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Build_Task_Array_Image
     (Loc    : Source_Ptr;
      Id_Ref : Node_Id;
      A_Type : Entity_Id;
      Dyn    : Boolean := False) return Node_Id;
   --  Build function to generate the image string for a task that is an array
   --  component, concatenating the images of each index. To avoid storage
   --  leaks, the string is built with successive slice assignments. The flag
   --  Dyn indicates whether this is called for the initialization procedure of
   --  an array of tasks, or for the name of a dynamically created task that is
   --  assigned to an indexed component.

   function Build_Task_Image_Function
     (Loc   : Source_Ptr;
      Decls : List_Id;
      Stats : List_Id;
      Res   : Entity_Id) return Node_Id;
   --  Common processing for Task_Array_Image and Task_Record_Image. Build
   --  function body that computes image.

   procedure Build_Task_Image_Prefix
      (Loc    : Source_Ptr;
       Len    : out Entity_Id;
       Res    : out Entity_Id;
       Pos    : out Entity_Id;
       Prefix : Entity_Id;
       Sum    : Node_Id;
       Decls  : List_Id;
       Stats  : List_Id);
   --  Common processing for Task_Array_Image and Task_Record_Image. Create
   --  local variables and assign prefix of name to result string.

   function Build_Task_Record_Image
     (Loc    : Source_Ptr;
      Id_Ref : Node_Id;
      Dyn    : Boolean := False) return Node_Id;
   --  Build function to generate the image string for a task that is a record
   --  component. Concatenate name of variable with that of selector. The flag
   --  Dyn indicates whether this is called for the initialization procedure of
   --  record with task components, or for a dynamically created task that is
   --  assigned to a selected component.

   function Make_CW_Equivalent_Type
     (T : Entity_Id;
      E : Node_Id) return Entity_Id;
   --  T is a class-wide type entity, E is the initial expression node that
   --  constrains T in case such as: " X: T := E" or "new T'(E)". This function
   --  returns the entity of the Equivalent type and inserts on the fly the
   --  necessary declaration such as:
   --
   --    type anon is record
   --       _parent : Root_Type (T); constrained with E discriminants (if any)
   --       Extension : String (1 .. expr to match size of E);
   --    end record;
   --
   --  This record is compatible with any object of the class of T thanks to
   --  the first field and has the same size as E thanks to the second.

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id) return Node_Id;
   --  Produce a Range node whose bounds are:
   --    Low_Bound (Literal_Type) ..
   --        Low_Bound (Literal_Type) + (Length (Literal_Typ) - 1)
   --  this is used for expanding declarations like X : String := "sdfgdfg";
   --
   --  If the index type of the target array is not integer, we generate:
   --     Low_Bound (Literal_Type) ..
   --        Literal_Type'Val
   --          (Literal_Type'Pos (Low_Bound (Literal_Type))
   --             + (Length (Literal_Typ) -1))

   function Make_Non_Empty_Check
     (Loc : Source_Ptr;
      N   : Node_Id) return Node_Id;
   --  Produce a boolean expression checking that the unidimensional array
   --  node N is not empty.

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id) return Entity_Id;
   --  Create an implicit subtype of CW_Typ attached to node N

   function Requires_Cleanup_Actions
     (L                 : List_Id;
      Lib_Level         : Boolean;
      Nested_Constructs : Boolean) return Boolean;
   --  Given a list L, determine whether it contains one of the following:
   --
   --    1) controlled objects
   --    2) library-level tagged types
   --
   --  Lib_Level is True when the list comes from a construct at the library
   --  level, and False otherwise. Nested_Constructs is True when any nested
   --  packages declared in L must be processed, and False otherwise.

   -------------------------------------
   -- Activate_Atomic_Synchronization --
   -------------------------------------

   procedure Activate_Atomic_Synchronization (N : Node_Id) is
      Msg_Node : Node_Id;

   begin
      case Nkind (Parent (N)) is

         --  Check for cases of appearing in the prefix of a construct where
         --  we don't need atomic synchronization for this kind of usage.

         when
              --  Nothing to do if we are the prefix of an attribute, since we
              --  do not want an atomic sync operation for things like 'Size.

              N_Attribute_Reference |

              --  The N_Reference node is like an attribute

              N_Reference           |

              --  Nothing to do for a reference to a component (or components)
              --  of a composite object. Only reads and updates of the object
              --  as a whole require atomic synchronization (RM C.6 (15)).

              N_Indexed_Component   |
              N_Selected_Component  |
              N_Slice               =>

            --  For all the above cases, nothing to do if we are the prefix

            if Prefix (Parent (N)) = N then
               return;
            end if;

         when others => null;
      end case;

      --  Go ahead and set the flag

      Set_Atomic_Sync_Required (N);

      --  Generate info message if requested

      if Warn_On_Atomic_Synchronization then
         case Nkind (N) is
            when N_Identifier =>
               Msg_Node := N;

            when N_Selected_Component | N_Expanded_Name =>
               Msg_Node := Selector_Name (N);

            when N_Explicit_Dereference | N_Indexed_Component =>
               Msg_Node := Empty;

            when others =>
               pragma Assert (False);
               return;
         end case;

         if Present (Msg_Node) then
            Error_Msg_N
              ("?N?info: atomic synchronization set for &", Msg_Node);
         else
            Error_Msg_N
              ("?N?info: atomic synchronization set", N);
         end if;
      end if;
   end Activate_Atomic_Synchronization;

   ----------------------
   -- Adjust_Condition --
   ----------------------

   procedure Adjust_Condition (N : Node_Id) is
   begin
      if No (N) then
         return;
      end if;

      declare
         Loc : constant Source_Ptr := Sloc (N);
         T   : constant Entity_Id  := Etype (N);
         Ti  : Entity_Id;

      begin
         --  Defend against a call where the argument has no type, or has a
         --  type that is not Boolean. This can occur because of prior errors.

         if No (T) or else not Is_Boolean_Type (T) then
            return;
         end if;

         --  Apply validity checking if needed

         if Validity_Checks_On and Validity_Check_Tests then
            Ensure_Valid (N);
         end if;

         --  Immediate return if standard boolean, the most common case,
         --  where nothing needs to be done.

         if Base_Type (T) = Standard_Boolean then
            return;
         end if;

         --  Case of zero/non-zero semantics or non-standard enumeration
         --  representation. In each case, we rewrite the node as:

         --      ityp!(N) /= False'Enum_Rep

         --  where ityp is an integer type with large enough size to hold any
         --  value of type T.

         if Nonzero_Is_True (T) or else Has_Non_Standard_Rep (T) then
            if Esize (T) <= Esize (Standard_Integer) then
               Ti := Standard_Integer;
            else
               Ti := Standard_Long_Long_Integer;
            end if;

            Rewrite (N,
              Make_Op_Ne (Loc,
                Left_Opnd  => Unchecked_Convert_To (Ti, N),
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Enum_Rep,
                    Prefix         =>
                      New_Occurrence_Of (First_Literal (T), Loc))));
            Analyze_And_Resolve (N, Standard_Boolean);

         else
            Rewrite (N, Convert_To (Standard_Boolean, N));
            Analyze_And_Resolve (N, Standard_Boolean);
         end if;
      end;
   end Adjust_Condition;

   ------------------------
   -- Adjust_Result_Type --
   ------------------------

   procedure Adjust_Result_Type (N : Node_Id; T : Entity_Id) is
   begin
      --  Ignore call if current type is not Standard.Boolean

      if Etype (N) /= Standard_Boolean then
         return;
      end if;

      --  If result is already of correct type, nothing to do. Note that
      --  this will get the most common case where everything has a type
      --  of Standard.Boolean.

      if Base_Type (T) = Standard_Boolean then
         return;

      else
         declare
            KP : constant Node_Kind := Nkind (Parent (N));

         begin
            --  If result is to be used as a Condition in the syntax, no need
            --  to convert it back, since if it was changed to Standard.Boolean
            --  using Adjust_Condition, that is just fine for this usage.

            if KP in N_Raise_xxx_Error or else KP in N_Has_Condition then
               return;

            --  If result is an operand of another logical operation, no need
            --  to reset its type, since Standard.Boolean is just fine, and
            --  such operations always do Adjust_Condition on their operands.

            elsif     KP in N_Op_Boolean
              or else KP in N_Short_Circuit
              or else KP = N_Op_Not
            then
               return;

            --  Otherwise we perform a conversion from the current type, which
            --  must be Standard.Boolean, to the desired type.

            else
               Set_Analyzed (N);
               Rewrite (N, Convert_To (T, N));
               Analyze_And_Resolve (N, T);
            end if;
         end;
      end if;
   end Adjust_Result_Type;

   --------------------------
   -- Append_Freeze_Action --
   --------------------------

   procedure Append_Freeze_Action (T : Entity_Id; N : Node_Id) is
      Fnode : Node_Id;

   begin
      Ensure_Freeze_Node (T);
      Fnode := Freeze_Node (T);

      if No (Actions (Fnode)) then
         Set_Actions (Fnode, New_List (N));
      else
         Append (N, Actions (Fnode));
      end if;

   end Append_Freeze_Action;

   ---------------------------
   -- Append_Freeze_Actions --
   ---------------------------

   procedure Append_Freeze_Actions (T : Entity_Id; L : List_Id) is
      Fnode : Node_Id;

   begin
      if No (L) then
         return;
      end if;

      Ensure_Freeze_Node (T);
      Fnode := Freeze_Node (T);

      if No (Actions (Fnode)) then
         Set_Actions (Fnode, L);
      else
         Append_List (L, Actions (Fnode));
      end if;
   end Append_Freeze_Actions;

   ------------------------------------
   -- Build_Allocate_Deallocate_Proc --
   ------------------------------------

   procedure Build_Allocate_Deallocate_Proc
     (N           : Node_Id;
      Is_Allocate : Boolean)
   is
      Desig_Typ    : Entity_Id;
      Expr         : Node_Id;
      Pool_Id      : Entity_Id;
      Proc_To_Call : Node_Id := Empty;
      Ptr_Typ      : Entity_Id;

      function Find_Finalize_Address (Typ : Entity_Id) return Entity_Id;
      --  Locate TSS primitive Finalize_Address in type Typ

      function Find_Object (E : Node_Id) return Node_Id;
      --  Given an arbitrary expression of an allocator, try to find an object
      --  reference in it, otherwise return the original expression.

      function Is_Allocate_Deallocate_Proc (Subp : Entity_Id) return Boolean;
      --  Determine whether subprogram Subp denotes a custom allocate or
      --  deallocate.

      ---------------------------
      -- Find_Finalize_Address --
      ---------------------------

      function Find_Finalize_Address (Typ : Entity_Id) return Entity_Id is
         Utyp : Entity_Id := Typ;

      begin
         --  Handle protected class-wide or task class-wide types

         if Is_Class_Wide_Type (Utyp) then
            if Is_Concurrent_Type (Root_Type (Utyp)) then
               Utyp := Root_Type (Utyp);

            elsif Is_Private_Type (Root_Type (Utyp))
              and then Present (Full_View (Root_Type (Utyp)))
              and then Is_Concurrent_Type (Full_View (Root_Type (Utyp)))
            then
               Utyp := Full_View (Root_Type (Utyp));
            end if;
         end if;

         --  Handle private types

         if Is_Private_Type (Utyp) and then Present (Full_View (Utyp)) then
            Utyp := Full_View (Utyp);
         end if;

         --  Handle protected and task types

         if Is_Concurrent_Type (Utyp)
           and then Present (Corresponding_Record_Type (Utyp))
         then
            Utyp := Corresponding_Record_Type (Utyp);
         end if;

         Utyp := Underlying_Type (Base_Type (Utyp));

         --  Deal with non-tagged derivation of private views. If the parent is
         --  now known to be protected, the finalization routine is the one
         --  defined on the corresponding record of the ancestor (corresponding
         --  records do not automatically inherit operations, but maybe they
         --  should???)

         if Is_Untagged_Derivation (Typ) then
            if Is_Protected_Type (Typ) then
               Utyp := Corresponding_Record_Type (Root_Type (Base_Type (Typ)));
            else
               Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));

               if Is_Protected_Type (Utyp) then
                  Utyp := Corresponding_Record_Type (Utyp);
               end if;
            end if;
         end if;

         --  If the underlying_type is a subtype, we are dealing with the
         --  completion of a private type. We need to access the base type and
         --  generate a conversion to it.

         if Utyp /= Base_Type (Utyp) then
            pragma Assert (Is_Private_Type (Typ));

            Utyp := Base_Type (Utyp);
         end if;

         --  When dealing with an internally built full view for a type with
         --  unknown discriminants, use the original record type.

         if Is_Underlying_Record_View (Utyp) then
            Utyp := Etype (Utyp);
         end if;

         return TSS (Utyp, TSS_Finalize_Address);
      end Find_Finalize_Address;

      -----------------
      -- Find_Object --
      -----------------

      function Find_Object (E : Node_Id) return Node_Id is
         Expr : Node_Id;

      begin
         pragma Assert (Is_Allocate);

         Expr := E;
         loop
            if Nkind_In (Expr, N_Qualified_Expression,
                               N_Unchecked_Type_Conversion)
            then
               Expr := Expression (Expr);

            elsif Nkind (Expr) = N_Explicit_Dereference then
               Expr := Prefix (Expr);

            else
               exit;
            end if;
         end loop;

         return Expr;
      end Find_Object;

      ---------------------------------
      -- Is_Allocate_Deallocate_Proc --
      ---------------------------------

      function Is_Allocate_Deallocate_Proc (Subp : Entity_Id) return Boolean is
      begin
         --  Look for a subprogram body with only one statement which is a
         --  call to Allocate_Any_Controlled / Deallocate_Any_Controlled.

         if Ekind (Subp) = E_Procedure
           and then Nkind (Parent (Parent (Subp))) = N_Subprogram_Body
         then
            declare
               HSS  : constant Node_Id :=
                        Handled_Statement_Sequence (Parent (Parent (Subp)));
               Proc : Entity_Id;

            begin
               if Present (Statements (HSS))
                 and then Nkind (First (Statements (HSS))) =
                            N_Procedure_Call_Statement
               then
                  Proc := Entity (Name (First (Statements (HSS))));

                  return
                    Is_RTE (Proc, RE_Allocate_Any_Controlled)
                      or else Is_RTE (Proc, RE_Deallocate_Any_Controlled);
               end if;
            end;
         end if;

         return False;
      end Is_Allocate_Deallocate_Proc;

   --  Start of processing for Build_Allocate_Deallocate_Proc

   begin
      --  Obtain the attributes of the allocation / deallocation

      if Nkind (N) = N_Free_Statement then
         Expr := Expression (N);
         Ptr_Typ := Base_Type (Etype (Expr));
         Proc_To_Call := Procedure_To_Call (N);

      else
         if Nkind (N) = N_Object_Declaration then
            Expr := Expression (N);
         else
            Expr := N;
         end if;

         --  In certain cases an allocator with a qualified expression may
         --  be relocated and used as the initialization expression of a
         --  temporary:

         --    before:
         --       Obj : Ptr_Typ := new Desig_Typ'(...);

         --    after:
         --       Tmp : Ptr_Typ := new Desig_Typ'(...);
         --       Obj : Ptr_Typ := Tmp;

         --  Since the allocator is always marked as analyzed to avoid infinite
         --  expansion, it will never be processed by this routine given that
         --  the designated type needs finalization actions. Detect this case
         --  and complete the expansion of the allocator.

         if Nkind (Expr) = N_Identifier
           and then Nkind (Parent (Entity (Expr))) = N_Object_Declaration
           and then Nkind (Expression (Parent (Entity (Expr)))) = N_Allocator
         then
            Build_Allocate_Deallocate_Proc (Parent (Entity (Expr)), True);
            return;
         end if;

         --  The allocator may have been rewritten into something else in which
         --  case the expansion performed by this routine does not apply.

         if Nkind (Expr) /= N_Allocator then
            return;
         end if;

         Ptr_Typ := Base_Type (Etype (Expr));
         Proc_To_Call := Procedure_To_Call (Expr);
      end if;

      Pool_Id := Associated_Storage_Pool (Ptr_Typ);
      Desig_Typ := Available_View (Designated_Type (Ptr_Typ));

      --  Handle concurrent types

      if Is_Concurrent_Type (Desig_Typ)
        and then Present (Corresponding_Record_Type (Desig_Typ))
      then
         Desig_Typ := Corresponding_Record_Type (Desig_Typ);
      end if;

      --  Do not process allocations / deallocations without a pool

      if No (Pool_Id) then
         return;

      --  Do not process allocations on / deallocations from the secondary
      --  stack.

      elsif Is_RTE (Pool_Id, RE_SS_Pool) then
         return;

      --  Do not replicate the machinery if the allocator / free has already
      --  been expanded and has a custom Allocate / Deallocate.

      elsif Present (Proc_To_Call)
        and then Is_Allocate_Deallocate_Proc (Proc_To_Call)
      then
         return;
      end if;

      if Needs_Finalization (Desig_Typ) then

         --  Certain run-time configurations and targets do not provide support
         --  for controlled types.

         if Restriction_Active (No_Finalization) then
            return;

         --  Do nothing if the access type may never allocate / deallocate
         --  objects.

         elsif No_Pool_Assigned (Ptr_Typ) then
            return;

         --  Access-to-controlled types are not supported on .NET/JVM since
         --  these targets cannot support pools and address arithmetic.

         elsif VM_Target /= No_VM then
            return;
         end if;

         --  The allocation / deallocation of a controlled object must be
         --  chained on / detached from a finalization master.

         pragma Assert (Present (Finalization_Master (Ptr_Typ)));

      --  The only other kind of allocation / deallocation supported by this
      --  routine is on / from a subpool.

      elsif Nkind (Expr) = N_Allocator
        and then No (Subpool_Handle_Name (Expr))
      then
         return;
      end if;

      declare
         Loc     : constant Source_Ptr := Sloc (N);
         Addr_Id : constant Entity_Id := Make_Temporary (Loc, 'A');
         Alig_Id : constant Entity_Id := Make_Temporary (Loc, 'L');
         Proc_Id : constant Entity_Id := Make_Temporary (Loc, 'P');
         Size_Id : constant Entity_Id := Make_Temporary (Loc, 'S');

         Actuals      : List_Id;
         Fin_Addr_Id  : Entity_Id;
         Fin_Mas_Act  : Node_Id;
         Fin_Mas_Id   : Entity_Id;
         Proc_To_Call : Entity_Id;
         Subpool      : Node_Id := Empty;

      begin
         --  Step 1: Construct all the actuals for the call to library routine
         --  Allocate_Any_Controlled / Deallocate_Any_Controlled.

         --  a) Storage pool

         Actuals := New_List (New_Reference_To (Pool_Id, Loc));

         if Is_Allocate then

            --  b) Subpool

            if Nkind (Expr) = N_Allocator then
               Subpool := Subpool_Handle_Name (Expr);
            end if;

            --  If a subpool is present it can be an arbitrary name, so make
            --  the actual by copying the tree.

            if Present (Subpool) then
               Append_To (Actuals, New_Copy_Tree (Subpool, New_Sloc => Loc));
            else
               Append_To (Actuals, Make_Null (Loc));
            end if;

            --  c) Finalization master

            if Needs_Finalization (Desig_Typ) then
               Fin_Mas_Id  := Finalization_Master (Ptr_Typ);
               Fin_Mas_Act := New_Reference_To (Fin_Mas_Id, Loc);

               --  Handle the case where the master is actually a pointer to a
               --  master. This case arises in build-in-place functions.

               if Is_Access_Type (Etype (Fin_Mas_Id)) then
                  Append_To (Actuals, Fin_Mas_Act);
               else
                  Append_To (Actuals,
                    Make_Attribute_Reference (Loc,
                      Prefix         => Fin_Mas_Act,
                      Attribute_Name => Name_Unrestricted_Access));
               end if;
            else
               Append_To (Actuals, Make_Null (Loc));
            end if;

            --  d) Finalize_Address

            --  Primitive Finalize_Address is never generated in CodePeer mode
            --  since it contains an Unchecked_Conversion.

            if Needs_Finalization (Desig_Typ) and then not CodePeer_Mode then
               Fin_Addr_Id := Find_Finalize_Address (Desig_Typ);
               pragma Assert (Present (Fin_Addr_Id));

               Append_To (Actuals,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Reference_To (Fin_Addr_Id, Loc),
                   Attribute_Name => Name_Unrestricted_Access));
            else
               Append_To (Actuals, Make_Null (Loc));
            end if;
         end if;

         --  e) Address
         --  f) Storage_Size
         --  g) Alignment

         Append_To (Actuals, New_Reference_To (Addr_Id, Loc));
         Append_To (Actuals, New_Reference_To (Size_Id, Loc));

         if Is_Allocate or else not Is_Class_Wide_Type (Desig_Typ) then
            Append_To (Actuals, New_Reference_To (Alig_Id, Loc));

         --  For deallocation of class wide types we obtain the value of
         --  alignment from the Type Specific Record of the deallocated object.
         --  This is needed because the frontend expansion of class-wide types
         --  into equivalent types confuses the backend.

         else
            --  Generate:
            --     Obj.all'Alignment

            --  ... because 'Alignment applied to class-wide types is expanded
            --  into the code that reads the value of alignment from the TSD
            --  (see Expand_N_Attribute_Reference)

            Append_To (Actuals,
              Unchecked_Convert_To (RTE (RE_Storage_Offset),
                Make_Attribute_Reference (Loc,
                  Prefix         =>
                    Make_Explicit_Dereference (Loc, Relocate_Node (Expr)),
                  Attribute_Name => Name_Alignment)));
         end if;

         --  h) Is_Controlled

         --  Generate a run-time check to determine whether a class-wide object
         --  is truly controlled.

         if Needs_Finalization (Desig_Typ) then
            if Is_Class_Wide_Type (Desig_Typ)
              or else Is_Generic_Actual_Type (Desig_Typ)
            then
               declare
                  Flag_Id   : constant Entity_Id := Make_Temporary (Loc, 'F');
                  Flag_Expr : Node_Id;
                  Param     : Node_Id;
                  Temp      : Node_Id;

               begin
                  if Is_Allocate then
                     Temp := Find_Object (Expression (Expr));
                  else
                     Temp := Expr;
                  end if;

                  --  Processing for generic actuals

                  if Is_Generic_Actual_Type (Desig_Typ) then
                     Flag_Expr :=
                       New_Reference_To (Boolean_Literals
                         (Needs_Finalization (Base_Type (Desig_Typ))), Loc);

                  --  Processing for subtype indications

                  elsif Nkind (Temp) in N_Has_Entity
                    and then Is_Type (Entity (Temp))
                  then
                     Flag_Expr :=
                       New_Reference_To (Boolean_Literals
                         (Needs_Finalization (Entity (Temp))), Loc);

                  --  Generate a runtime check to test the controlled state of
                  --  an object for the purposes of allocation / deallocation.

                  else
                     --  The following case arises when allocating through an
                     --  interface class-wide type, generate:
                     --
                     --    Temp.all

                     if Is_RTE (Etype (Temp), RE_Tag_Ptr) then
                        Param :=
                          Make_Explicit_Dereference (Loc,
                            Prefix =>
                              Relocate_Node (Temp));

                     --  Generate:
                     --    Temp'Tag

                     else
                        Param :=
                          Make_Attribute_Reference (Loc,
                            Prefix =>
                              Relocate_Node (Temp),
                            Attribute_Name => Name_Tag);
                     end if;

                     --  Generate:
                     --    Needs_Finalization (<Param>)

                     Flag_Expr :=
                       Make_Function_Call (Loc,
                         Name =>
                           New_Reference_To (RTE (RE_Needs_Finalization), Loc),
                         Parameter_Associations => New_List (Param));
                  end if;

                  --  Create the temporary which represents the finalization
                  --  state of the expression. Generate:
                  --
                  --    F : constant Boolean := <Flag_Expr>;

                  Insert_Action (N,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Flag_Id,
                      Constant_Present => True,
                      Object_Definition =>
                        New_Reference_To (Standard_Boolean, Loc),
                      Expression => Flag_Expr));

                  --  The flag acts as the last actual

                  Append_To (Actuals, New_Reference_To (Flag_Id, Loc));
               end;

            --  The object is statically known to be controlled

            else
               Append_To (Actuals, New_Reference_To (Standard_True, Loc));
            end if;

         else
            Append_To (Actuals, New_Reference_To (Standard_False, Loc));
         end if;

         --  i) On_Subpool

         if Is_Allocate then
            Append_To (Actuals,
              New_Reference_To (Boolean_Literals (Present (Subpool)), Loc));
         end if;

         --  Step 2: Build a wrapper Allocate / Deallocate which internally
         --  calls Allocate_Any_Controlled / Deallocate_Any_Controlled.

         --  Select the proper routine to call

         if Is_Allocate then
            Proc_To_Call := RTE (RE_Allocate_Any_Controlled);
         else
            Proc_To_Call := RTE (RE_Deallocate_Any_Controlled);
         end if;

         --  Create a custom Allocate / Deallocate routine which has identical
         --  profile to that of System.Storage_Pools.

         Insert_Action (N,
           Make_Subprogram_Body (Loc,
             Specification =>

               --  procedure Pnn

               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Proc_Id,
                 Parameter_Specifications => New_List (

                  --  P : Root_Storage_Pool

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Make_Temporary (Loc, 'P'),
                     Parameter_Type =>
                       New_Reference_To (RTE (RE_Root_Storage_Pool), Loc)),

                  --  A : [out] Address

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Addr_Id,
                     Out_Present         => Is_Allocate,
                     Parameter_Type      =>
                       New_Reference_To (RTE (RE_Address), Loc)),

                  --  S : Storage_Count

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Size_Id,
                     Parameter_Type      =>
                       New_Reference_To (RTE (RE_Storage_Count), Loc)),

                  --  L : Storage_Count

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Alig_Id,
                     Parameter_Type      =>
                       New_Reference_To (RTE (RE_Storage_Count), Loc)))),

             Declarations => No_List,

             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Procedure_Call_Statement (Loc,
                     Name => New_Reference_To (Proc_To_Call, Loc),
                     Parameter_Associations => Actuals)))));

         --  The newly generated Allocate / Deallocate becomes the default
         --  procedure to call when the back end processes the allocation /
         --  deallocation.

         if Is_Allocate then
            Set_Procedure_To_Call (Expr, Proc_Id);
         else
            Set_Procedure_To_Call (N, Proc_Id);
         end if;
      end;
   end Build_Allocate_Deallocate_Proc;

   ------------------------
   -- Build_Runtime_Call --
   ------------------------

   function Build_Runtime_Call (Loc : Source_Ptr; RE : RE_Id) return Node_Id is
   begin
      --  If entity is not available, we can skip making the call (this avoids
      --  junk duplicated error messages in a number of cases).

      if not RTE_Available (RE) then
         return Make_Null_Statement (Loc);
      else
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE), Loc));
      end if;
   end Build_Runtime_Call;

   ----------------------------
   -- Build_Task_Array_Image --
   ----------------------------

   --  This function generates the body for a function that constructs the
   --  image string for a task that is an array component. The function is
   --  local to the init proc for the array type, and is called for each one
   --  of the components. The constructed image has the form of an indexed
   --  component, whose prefix is the outer variable of the array type.
   --  The n-dimensional array type has known indexes Index, Index2...

   --  Id_Ref is an indexed component form created by the enclosing init proc.
   --  Its successive indexes are Val1, Val2, ... which are the loop variables
   --  in the loops that call the individual task init proc on each component.

   --  The generated function has the following structure:

   --  function F return String is
   --     Pref : string renames Task_Name;
   --     T1   : String := Index1'Image (Val1);
   --     ...
   --     Tn   : String := indexn'image (Valn);
   --     Len  : Integer := T1'Length + ... + Tn'Length + n + 1;
   --     --  Len includes commas and the end parentheses.
   --     Res  : String (1..Len);
   --     Pos  : Integer := Pref'Length;
   --
   --  begin
   --     Res (1 .. Pos) := Pref;
   --     Pos := Pos + 1;
   --     Res (Pos)    := '(';
   --     Pos := Pos + 1;
   --     Res (Pos .. Pos + T1'Length - 1) := T1;
   --     Pos := Pos + T1'Length;
   --     Res (Pos) := '.';
   --     Pos := Pos + 1;
   --     ...
   --     Res (Pos .. Pos + Tn'Length - 1) := Tn;
   --     Res (Len) := ')';
   --
   --     return Res;
   --  end F;
   --
   --  Needless to say, multidimensional arrays of tasks are rare enough that
   --  the bulkiness of this code is not really a concern.

   function Build_Task_Array_Image
     (Loc    : Source_Ptr;
      Id_Ref : Node_Id;
      A_Type : Entity_Id;
      Dyn    : Boolean := False) return Node_Id
   is
      Dims : constant Nat := Number_Dimensions (A_Type);
      --  Number of dimensions for array of tasks

      Temps : array (1 .. Dims) of Entity_Id;
      --  Array of temporaries to hold string for each index

      Indx : Node_Id;
      --  Index expression

      Len : Entity_Id;
      --  Total length of generated name

      Pos : Entity_Id;
      --  Running index for substring assignments

      Pref : constant Entity_Id := Make_Temporary (Loc, 'P');
      --  Name of enclosing variable, prefix of resulting name

      Res : Entity_Id;
      --  String to hold result

      Val : Node_Id;
      --  Value of successive indexes

      Sum : Node_Id;
      --  Expression to compute total size of string

      T : Entity_Id;
      --  Entity for name at one index position

      Decls : constant List_Id := New_List;
      Stats : constant List_Id := New_List;

   begin
      --  For a dynamic task, the name comes from the target variable. For a
      --  static one it is a formal of the enclosing init proc.

      if Dyn then
         Get_Name_String (Chars (Entity (Prefix (Id_Ref))));
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Pref,
             Object_Definition => New_Occurrence_Of (Standard_String, Loc),
             Expression =>
               Make_String_Literal (Loc,
                 Strval => String_From_Name_Buffer)));

      else
         Append_To (Decls,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Pref,
             Subtype_Mark        => New_Occurrence_Of (Standard_String, Loc),
             Name                => Make_Identifier (Loc, Name_uTask_Name)));
      end if;

      Indx := First_Index (A_Type);
      Val  := First (Expressions (Id_Ref));

      for J in 1 .. Dims loop
         T := Make_Temporary (Loc, 'T');
         Temps (J) := T;

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => T,
             Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Image,
                 Prefix         => New_Occurrence_Of (Etype (Indx), Loc),
                 Expressions    => New_List (New_Copy_Tree (Val)))));

         Next_Index (Indx);
         Next (Val);
      end loop;

      Sum := Make_Integer_Literal (Loc, Dims + 1);

      Sum :=
        Make_Op_Add (Loc,
          Left_Opnd => Sum,
          Right_Opnd =>
            Make_Attribute_Reference (Loc,
              Attribute_Name => Name_Length,
              Prefix         => New_Occurrence_Of (Pref, Loc),
              Expressions    => New_List (Make_Integer_Literal (Loc, 1))));

      for J in 1 .. Dims loop
         Sum :=
           Make_Op_Add (Loc,
             Left_Opnd  => Sum,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Length,
                 Prefix         =>
                  New_Occurrence_Of (Temps (J), Loc),
                Expressions     => New_List (Make_Integer_Literal (Loc, 1))));
      end loop;

      Build_Task_Image_Prefix (Loc, Len, Res, Pos, Pref, Sum, Decls, Stats);

      Set_Character_Literal_Name (Char_Code (Character'Pos ('(')));

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name       =>
            Make_Indexed_Component (Loc,
              Prefix      => New_Occurrence_Of (Res, Loc),
              Expressions => New_List (New_Occurrence_Of (Pos, Loc))),
          Expression =>
            Make_Character_Literal (Loc,
              Chars              => Name_Find,
              Char_Literal_Value => UI_From_Int (Character'Pos ('(')))));

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (Pos, Loc),
          Expression =>
            Make_Op_Add (Loc,
              Left_Opnd  => New_Occurrence_Of (Pos, Loc),
              Right_Opnd => Make_Integer_Literal (Loc, 1))));

      for J in 1 .. Dims loop

         Append_To (Stats,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Slice (Loc,
                 Prefix          => New_Occurrence_Of (Res, Loc),
                 Discrete_Range  =>
                   Make_Range (Loc,
                     Low_Bound  => New_Occurrence_Of  (Pos, Loc),
                     High_Bound =>
                       Make_Op_Subtract (Loc,
                         Left_Opnd  =>
                           Make_Op_Add (Loc,
                             Left_Opnd  => New_Occurrence_Of (Pos, Loc),
                             Right_Opnd =>
                               Make_Attribute_Reference (Loc,
                                 Attribute_Name => Name_Length,
                                 Prefix         =>
                                   New_Occurrence_Of (Temps (J), Loc),
                                 Expressions    =>
                                   New_List (Make_Integer_Literal (Loc, 1)))),
                         Right_Opnd => Make_Integer_Literal (Loc, 1)))),

              Expression => New_Occurrence_Of (Temps (J), Loc)));

         if J < Dims then
            Append_To (Stats,
               Make_Assignment_Statement (Loc,
                  Name       => New_Occurrence_Of (Pos, Loc),
                  Expression =>
                    Make_Op_Add (Loc,
                      Left_Opnd  => New_Occurrence_Of (Pos, Loc),
                      Right_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Attribute_Name => Name_Length,
                          Prefix         => New_Occurrence_Of (Temps (J), Loc),
                          Expressions    =>
                            New_List (Make_Integer_Literal (Loc, 1))))));

            Set_Character_Literal_Name (Char_Code (Character'Pos (',')));

            Append_To (Stats,
              Make_Assignment_Statement (Loc,
                Name => Make_Indexed_Component (Loc,
                   Prefix => New_Occurrence_Of (Res, Loc),
                   Expressions => New_List (New_Occurrence_Of (Pos, Loc))),
                Expression =>
                  Make_Character_Literal (Loc,
                    Chars              => Name_Find,
                    Char_Literal_Value => UI_From_Int (Character'Pos (',')))));

            Append_To (Stats,
              Make_Assignment_Statement (Loc,
                Name         => New_Occurrence_Of (Pos, Loc),
                  Expression =>
                    Make_Op_Add (Loc,
                      Left_Opnd  => New_Occurrence_Of (Pos, Loc),
                      Right_Opnd => Make_Integer_Literal (Loc, 1))));
         end if;
      end loop;

      Set_Character_Literal_Name (Char_Code (Character'Pos (')')));

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name        =>
            Make_Indexed_Component (Loc,
              Prefix      => New_Occurrence_Of (Res, Loc),
              Expressions => New_List (New_Occurrence_Of (Len, Loc))),
           Expression =>
             Make_Character_Literal (Loc,
               Chars              => Name_Find,
               Char_Literal_Value => UI_From_Int (Character'Pos (')')))));
      return Build_Task_Image_Function (Loc, Decls, Stats, Res);
   end Build_Task_Array_Image;

   ----------------------------
   -- Build_Task_Image_Decls --
   ----------------------------

   function Build_Task_Image_Decls
     (Loc          : Source_Ptr;
      Id_Ref       : Node_Id;
      A_Type       : Entity_Id;
      In_Init_Proc : Boolean := False) return List_Id
   is
      Decls  : constant List_Id   := New_List;
      T_Id   : Entity_Id := Empty;
      Decl   : Node_Id;
      Expr   : Node_Id   := Empty;
      Fun    : Node_Id   := Empty;
      Is_Dyn : constant Boolean :=
                 Nkind (Parent (Id_Ref)) = N_Assignment_Statement
                   and then
                 Nkind (Expression (Parent (Id_Ref))) = N_Allocator;

   begin
      --  If Discard_Names or No_Implicit_Heap_Allocations are in effect,
      --  generate a dummy declaration only.

      if Restriction_Active (No_Implicit_Heap_Allocations)
        or else Global_Discard_Names
      then
         T_Id := Make_Temporary (Loc, 'J');
         Name_Len := 0;

         return
           New_List (
             Make_Object_Declaration (Loc,
               Defining_Identifier => T_Id,
               Object_Definition => New_Occurrence_Of (Standard_String, Loc),
               Expression =>
                 Make_String_Literal (Loc,
                   Strval => String_From_Name_Buffer)));

      else
         if Nkind (Id_Ref) = N_Identifier
           or else Nkind (Id_Ref) = N_Defining_Identifier
         then
            --  For a simple variable, the image of the task is built from
            --  the name of the variable. To avoid possible conflict with the
            --  anonymous type created for a single protected object, add a
            --  numeric suffix.

            T_Id :=
              Make_Defining_Identifier (Loc,
                New_External_Name (Chars (Id_Ref), 'T', 1));

            Get_Name_String (Chars (Id_Ref));

            Expr :=
              Make_String_Literal (Loc,
                Strval => String_From_Name_Buffer);

         elsif Nkind (Id_Ref) = N_Selected_Component then
            T_Id :=
              Make_Defining_Identifier (Loc,
                New_External_Name (Chars (Selector_Name (Id_Ref)), 'T'));
            Fun := Build_Task_Record_Image (Loc, Id_Ref, Is_Dyn);

         elsif Nkind (Id_Ref) = N_Indexed_Component then
            T_Id :=
              Make_Defining_Identifier (Loc,
                New_External_Name (Chars (A_Type), 'N'));

            Fun := Build_Task_Array_Image (Loc, Id_Ref, A_Type, Is_Dyn);
         end if;
      end if;

      if Present (Fun) then
         Append (Fun, Decls);
         Expr := Make_Function_Call (Loc,
           Name => New_Occurrence_Of (Defining_Entity (Fun), Loc));

         if not In_Init_Proc and then VM_Target = No_VM then
            Set_Uses_Sec_Stack (Defining_Entity (Fun));
         end if;
      end if;

      Decl := Make_Object_Declaration (Loc,
        Defining_Identifier => T_Id,
        Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
        Constant_Present    => True,
        Expression          => Expr);

      Append (Decl, Decls);
      return Decls;
   end Build_Task_Image_Decls;

   -------------------------------
   -- Build_Task_Image_Function --
   -------------------------------

   function Build_Task_Image_Function
     (Loc   : Source_Ptr;
      Decls : List_Id;
      Stats : List_Id;
      Res   : Entity_Id) return Node_Id
   is
      Spec : Node_Id;

   begin
      Append_To (Stats,
        Make_Simple_Return_Statement (Loc,
          Expression => New_Occurrence_Of (Res, Loc)));

      Spec := Make_Function_Specification (Loc,
        Defining_Unit_Name => Make_Temporary (Loc, 'F'),
        Result_Definition  => New_Occurrence_Of (Standard_String, Loc));

      --  Calls to 'Image use the secondary stack, which must be cleaned up
      --  after the task name is built.

      return Make_Subprogram_Body (Loc,
         Specification => Spec,
         Declarations => Decls,
         Handled_Statement_Sequence =>
           Make_Handled_Sequence_Of_Statements (Loc, Statements => Stats));
   end Build_Task_Image_Function;

   -----------------------------
   -- Build_Task_Image_Prefix --
   -----------------------------

   procedure Build_Task_Image_Prefix
      (Loc    : Source_Ptr;
       Len    : out Entity_Id;
       Res    : out Entity_Id;
       Pos    : out Entity_Id;
       Prefix : Entity_Id;
       Sum    : Node_Id;
       Decls  : List_Id;
       Stats  : List_Id)
   is
   begin
      Len := Make_Temporary (Loc, 'L', Sum);

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Len,
          Object_Definition   => New_Occurrence_Of (Standard_Integer, Loc),
          Expression          => Sum));

      Res := Make_Temporary (Loc, 'R');

      Append_To (Decls,
         Make_Object_Declaration (Loc,
            Defining_Identifier => Res,
            Object_Definition =>
               Make_Subtype_Indication (Loc,
                  Subtype_Mark => New_Occurrence_Of (Standard_String, Loc),
               Constraint =>
                 Make_Index_Or_Discriminant_Constraint (Loc,
                   Constraints =>
                     New_List (
                       Make_Range (Loc,
                         Low_Bound => Make_Integer_Literal (Loc, 1),
                         High_Bound => New_Occurrence_Of (Len, Loc)))))));

      Pos := Make_Temporary (Loc, 'P');

      Append_To (Decls,
         Make_Object_Declaration (Loc,
            Defining_Identifier => Pos,
            Object_Definition   => New_Occurrence_Of (Standard_Integer, Loc)));

      --  Pos := Prefix'Length;

      Append_To (Stats,
         Make_Assignment_Statement (Loc,
            Name => New_Occurrence_Of (Pos, Loc),
            Expression =>
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Length,
                Prefix         => New_Occurrence_Of (Prefix, Loc),
                Expressions    => New_List (Make_Integer_Literal (Loc, 1)))));

      --  Res (1 .. Pos) := Prefix;

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name =>
            Make_Slice (Loc,
              Prefix          => New_Occurrence_Of (Res, Loc),
              Discrete_Range  =>
                Make_Range (Loc,
                   Low_Bound  => Make_Integer_Literal (Loc, 1),
                   High_Bound => New_Occurrence_Of (Pos, Loc))),

          Expression => New_Occurrence_Of (Prefix, Loc)));

      Append_To (Stats,
         Make_Assignment_Statement (Loc,
            Name       => New_Occurrence_Of (Pos, Loc),
            Expression =>
              Make_Op_Add (Loc,
                Left_Opnd  => New_Occurrence_Of (Pos, Loc),
                Right_Opnd => Make_Integer_Literal (Loc, 1))));
   end Build_Task_Image_Prefix;

   -----------------------------
   -- Build_Task_Record_Image --
   -----------------------------

   function Build_Task_Record_Image
     (Loc    : Source_Ptr;
      Id_Ref : Node_Id;
      Dyn    : Boolean := False) return Node_Id
   is
      Len : Entity_Id;
      --  Total length of generated name

      Pos : Entity_Id;
      --  Index into result

      Res : Entity_Id;
      --  String to hold result

      Pref : constant Entity_Id := Make_Temporary (Loc, 'P');
      --  Name of enclosing variable, prefix of resulting name

      Sum : Node_Id;
      --  Expression to compute total size of string

      Sel : Entity_Id;
      --  Entity for selector name

      Decls : constant List_Id := New_List;
      Stats : constant List_Id := New_List;

   begin
      --  For a dynamic task, the name comes from the target variable. For a
      --  static one it is a formal of the enclosing init proc.

      if Dyn then
         Get_Name_String (Chars (Entity (Prefix (Id_Ref))));
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Pref,
             Object_Definition => New_Occurrence_Of (Standard_String, Loc),
             Expression =>
               Make_String_Literal (Loc,
                 Strval => String_From_Name_Buffer)));

      else
         Append_To (Decls,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Pref,
             Subtype_Mark        => New_Occurrence_Of (Standard_String, Loc),
             Name                => Make_Identifier (Loc, Name_uTask_Name)));
      end if;

      Sel := Make_Temporary (Loc, 'S');

      Get_Name_String (Chars (Selector_Name (Id_Ref)));

      Append_To (Decls,
         Make_Object_Declaration (Loc,
           Defining_Identifier => Sel,
           Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
           Expression          =>
             Make_String_Literal (Loc,
               Strval => String_From_Name_Buffer)));

      Sum := Make_Integer_Literal (Loc, Nat (Name_Len + 1));

      Sum :=
        Make_Op_Add (Loc,
          Left_Opnd => Sum,
          Right_Opnd =>
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Length,
             Prefix =>
               New_Occurrence_Of (Pref, Loc),
             Expressions => New_List (Make_Integer_Literal (Loc, 1))));

      Build_Task_Image_Prefix (Loc, Len, Res, Pos, Pref, Sum, Decls, Stats);

      Set_Character_Literal_Name (Char_Code (Character'Pos ('.')));

      --  Res (Pos) := '.';

      Append_To (Stats,
         Make_Assignment_Statement (Loc,
           Name => Make_Indexed_Component (Loc,
              Prefix => New_Occurrence_Of (Res, Loc),
              Expressions => New_List (New_Occurrence_Of (Pos, Loc))),
           Expression =>
             Make_Character_Literal (Loc,
               Chars => Name_Find,
               Char_Literal_Value =>
                 UI_From_Int (Character'Pos ('.')))));

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name => New_Occurrence_Of (Pos, Loc),
          Expression =>
            Make_Op_Add (Loc,
              Left_Opnd => New_Occurrence_Of (Pos, Loc),
              Right_Opnd => Make_Integer_Literal (Loc, 1))));

      --  Res (Pos .. Len) := Selector;

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name => Make_Slice (Loc,
             Prefix => New_Occurrence_Of (Res, Loc),
             Discrete_Range  =>
               Make_Range (Loc,
                 Low_Bound  => New_Occurrence_Of (Pos, Loc),
                 High_Bound => New_Occurrence_Of (Len, Loc))),
          Expression => New_Occurrence_Of (Sel, Loc)));

      return Build_Task_Image_Function (Loc, Decls, Stats, Res);
   end Build_Task_Record_Image;

   ----------------------------------
   -- Component_May_Be_Bit_Aligned --
   ----------------------------------

   function Component_May_Be_Bit_Aligned (Comp : Entity_Id) return Boolean is
      UT : Entity_Id;

   begin
      --  If no component clause, then everything is fine, since the back end
      --  never bit-misaligns by default, even if there is a pragma Packed for
      --  the record.

      if No (Comp) or else No (Component_Clause (Comp)) then
         return False;
      end if;

      UT := Underlying_Type (Etype (Comp));

      --  It is only array and record types that cause trouble

      if not Is_Record_Type (UT) and then not Is_Array_Type (UT) then
         return False;

      --  If we know that we have a small (64 bits or less) record or small
      --  bit-packed array, then everything is fine, since the back end can
      --  handle these cases correctly.

      elsif Esize (Comp) <= 64
        and then (Is_Record_Type (UT) or else Is_Bit_Packed_Array (UT))
      then
         return False;

      --  Otherwise if the component is not byte aligned, we know we have the
      --  nasty unaligned case.

      elsif Normalized_First_Bit (Comp) /= Uint_0
        or else Esize (Comp) mod System_Storage_Unit /= Uint_0
      then
         return True;

      --  If we are large and byte aligned, then OK at this level

      else
         return False;
      end if;
   end Component_May_Be_Bit_Aligned;

   -----------------------------------
   -- Corresponding_Runtime_Package --
   -----------------------------------

   function Corresponding_Runtime_Package (Typ : Entity_Id) return RTU_Id is
      Pkg_Id : RTU_Id := RTU_Null;

   begin
      pragma Assert (Is_Concurrent_Type (Typ));

      if Ekind (Typ) in Protected_Kind then
         if Has_Entries (Typ)

            --  A protected type without entries that covers an interface and
            --  overrides the abstract routines with protected procedures is
            --  considered equivalent to a protected type with entries in the
            --  context of dispatching select statements. It is sufficient to
            --  check for the presence of an interface list in the declaration
            --  node to recognize this case.

           or else Present (Interface_List (Parent (Typ)))

            --  Protected types with interrupt handlers (when not using a
            --  restricted profile) are also considered equivalent to
            --  protected types with entries. The types which are used
            --  (Static_Interrupt_Protection and Dynamic_Interrupt_Protection)
            --  are derived from Protection_Entries.

           or else (Has_Attach_Handler (Typ) and then not Restricted_Profile)
           or else Has_Interrupt_Handler (Typ)
         then
            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Typ) > 1
              or else (Has_Attach_Handler (Typ)
                        and then not Restricted_Profile)
            then
               Pkg_Id := System_Tasking_Protected_Objects_Entries;
            else
               Pkg_Id := System_Tasking_Protected_Objects_Single_Entry;
            end if;

         else
            Pkg_Id := System_Tasking_Protected_Objects;
         end if;
      end if;

      return Pkg_Id;
   end Corresponding_Runtime_Package;

   -------------------------------
   -- Convert_To_Actual_Subtype --
   -------------------------------

   procedure Convert_To_Actual_Subtype (Exp : Entity_Id) is
      Act_ST : Entity_Id;

   begin
      Act_ST := Get_Actual_Subtype (Exp);

      if Act_ST = Etype (Exp) then
         return;
      else
         Rewrite (Exp, Convert_To (Act_ST, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp, Act_ST);
      end if;
   end Convert_To_Actual_Subtype;

   -----------------------------------
   -- Current_Sem_Unit_Declarations --
   -----------------------------------

   function Current_Sem_Unit_Declarations return List_Id is
      U     : Node_Id := Unit (Cunit (Current_Sem_Unit));
      Decls : List_Id;

   begin
      --  If the current unit is a package body, locate the visible
      --  declarations of the package spec.

      if Nkind (U) = N_Package_Body then
         U := Unit (Library_Unit (Cunit (Current_Sem_Unit)));
      end if;

      if Nkind (U) = N_Package_Declaration then
         U := Specification (U);
         Decls := Visible_Declarations (U);

         if No (Decls) then
            Decls := New_List;
            Set_Visible_Declarations (U, Decls);
         end if;

      else
         Decls := Declarations (U);

         if No (Decls) then
            Decls := New_List;
            Set_Declarations (U, Decls);
         end if;
      end if;

      return Decls;
   end Current_Sem_Unit_Declarations;

   -----------------------
   -- Duplicate_Subexpr --
   -----------------------

   function Duplicate_Subexpr
     (Exp      : Node_Id;
      Name_Req : Boolean := False) return Node_Id
   is
   begin
      Remove_Side_Effects (Exp, Name_Req);
      return New_Copy_Tree (Exp);
   end Duplicate_Subexpr;

   ---------------------------------
   -- Duplicate_Subexpr_No_Checks --
   ---------------------------------

   function Duplicate_Subexpr_No_Checks
     (Exp      : Node_Id;
      Name_Req : Boolean := False) return Node_Id
   is
      New_Exp : Node_Id;
   begin
      Remove_Side_Effects (Exp, Name_Req);
      New_Exp := New_Copy_Tree (Exp);
      Remove_Checks (New_Exp);
      return New_Exp;
   end Duplicate_Subexpr_No_Checks;

   -----------------------------------
   -- Duplicate_Subexpr_Move_Checks --
   -----------------------------------

   function Duplicate_Subexpr_Move_Checks
     (Exp      : Node_Id;
      Name_Req : Boolean := False) return Node_Id
   is
      New_Exp : Node_Id;
   begin
      Remove_Side_Effects (Exp, Name_Req);
      New_Exp := New_Copy_Tree (Exp);
      Remove_Checks (Exp);
      return New_Exp;
   end Duplicate_Subexpr_Move_Checks;

   --------------------
   -- Ensure_Defined --
   --------------------

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id) is
      IR : Node_Id;

   begin
      --  An itype reference must only be created if this is a local itype, so
      --  that gigi can elaborate it on the proper objstack.

      if Is_Itype (Typ) and then Scope (Typ) = Current_Scope then
         IR := Make_Itype_Reference (Sloc (N));
         Set_Itype (IR, Typ);
         Insert_Action (N, IR);
      end if;
   end Ensure_Defined;

   ---------------
   -- Entity_Of --
   ---------------

   function Entity_Of (N : Node_Id) return Entity_Id is
      Id : Entity_Id;

   begin
      Id := Empty;

      if Is_Entity_Name (N) then
         Id := Entity (N);

         --  Follow a possible chain of renamings to reach the root renamed
         --  object.

         while Present (Renamed_Object (Id)) loop
            if Is_Entity_Name (Renamed_Object (Id)) then
               Id := Entity (Renamed_Object (Id));
            else
               Id := Empty;
               exit;
            end if;
         end loop;
      end if;

      return Id;
   end Entity_Of;

   --------------------
   -- Entry_Names_OK --
   --------------------

   function Entry_Names_OK return Boolean is
   begin
      return
        not Restricted_Profile
          and then not Global_Discard_Names
          and then not Restriction_Active (No_Implicit_Heap_Allocations)
          and then not Restriction_Active (No_Local_Allocators);
   end Entry_Names_OK;

   -------------------
   -- Evaluate_Name --
   -------------------

   procedure Evaluate_Name (Nam : Node_Id) is
      K : constant Node_Kind := Nkind (Nam);

   begin
      --  For an explicit dereference, we simply force the evaluation of the
      --  name expression. The dereference provides a value that is the address
      --  for the renamed object, and it is precisely this value that we want
      --  to preserve.

      if K = N_Explicit_Dereference then
         Force_Evaluation (Prefix (Nam));

      --  For a selected component, we simply evaluate the prefix

      elsif K = N_Selected_Component then
         Evaluate_Name (Prefix (Nam));

      --  For an indexed component, or an attribute reference, we evaluate the
      --  prefix, which is itself a name, recursively, and then force the
      --  evaluation of all the subscripts (or attribute expressions).

      elsif Nkind_In (K, N_Indexed_Component, N_Attribute_Reference) then
         Evaluate_Name (Prefix (Nam));

         declare
            E : Node_Id;

         begin
            E := First (Expressions (Nam));
            while Present (E) loop
               Force_Evaluation (E);

               if Original_Node (E) /= E then
                  Set_Do_Range_Check (E, Do_Range_Check (Original_Node (E)));
               end if;

               Next (E);
            end loop;
         end;

      --  For a slice, we evaluate the prefix, as for the indexed component
      --  case and then, if there is a range present, either directly or as the
      --  constraint of a discrete subtype indication, we evaluate the two
      --  bounds of this range.

      elsif K = N_Slice then
         Evaluate_Name (Prefix (Nam));

         declare
            DR     : constant Node_Id := Discrete_Range (Nam);
            Constr : Node_Id;
            Rexpr  : Node_Id;

         begin
            if Nkind (DR) = N_Range then
               Force_Evaluation (Low_Bound (DR));
               Force_Evaluation (High_Bound (DR));

            elsif Nkind (DR) = N_Subtype_Indication then
               Constr := Constraint (DR);

               if Nkind (Constr) = N_Range_Constraint then
                  Rexpr := Range_Expression (Constr);

                  Force_Evaluation (Low_Bound (Rexpr));
                  Force_Evaluation (High_Bound (Rexpr));
               end if;
            end if;
         end;

      --  For a type conversion, the expression of the conversion must be the
      --  name of an object, and we simply need to evaluate this name.

      elsif K = N_Type_Conversion then
         Evaluate_Name (Expression (Nam));

      --  For a function call, we evaluate the call

      elsif K = N_Function_Call then
         Force_Evaluation (Nam);

      --  The remaining cases are direct name, operator symbol and character
      --  literal. In all these cases, we do nothing, since we want to
      --  reevaluate each time the renamed object is used.

      else
         return;
      end if;
   end Evaluate_Name;

   ---------------------
   -- Evolve_And_Then --
   ---------------------

   procedure Evolve_And_Then (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_And_Then (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_And_Then;

   --------------------
   -- Evolve_Or_Else --
   --------------------

   procedure Evolve_Or_Else (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_Or_Else (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_Or_Else;

   -----------------------------------------
   -- Expand_Static_Predicates_In_Choices --
   -----------------------------------------

   procedure Expand_Static_Predicates_In_Choices (N : Node_Id) is
      pragma Assert (Nkind_In (N, N_Case_Statement_Alternative, N_Variant));

      Choices : constant List_Id := Discrete_Choices (N);

      Choice : Node_Id;
      Next_C : Node_Id;
      P      : Node_Id;
      C      : Node_Id;

   begin
      Choice := First (Choices);
      while Present (Choice) loop
         Next_C := Next (Choice);

         --  Check for name of subtype with static predicate

         if Is_Entity_Name (Choice)
           and then Is_Type (Entity (Choice))
           and then Has_Predicates (Entity (Choice))
         then
            --  Loop through entries in predicate list, converting to choices
            --  and inserting in the list before the current choice. Note that
            --  if the list is empty, corresponding to a False predicate, then
            --  no choices are inserted.

            P := First (Static_Predicate (Entity (Choice)));
            while Present (P) loop

               --  If low bound and high bounds are equal, copy simple choice

               if Expr_Value (Low_Bound (P)) = Expr_Value (High_Bound (P)) then
                  C := New_Copy (Low_Bound (P));

               --  Otherwise copy a range

               else
                  C := New_Copy (P);
               end if;

               --  Change Sloc to referencing choice (rather than the Sloc of
               --  the predicate declaration element itself).

               Set_Sloc (C, Sloc (Choice));
               Insert_Before (Choice, C);
               Next (P);
            end loop;

            --  Delete the predicated entry

            Remove (Choice);
         end if;

         --  Move to next choice to check

         Choice := Next_C;
      end loop;
   end Expand_Static_Predicates_In_Choices;

   ------------------------------
   -- Expand_Subtype_From_Expr --
   ------------------------------

   --  This function is applicable for both static and dynamic allocation of
   --  objects which are constrained by an initial expression. Basically it
   --  transforms an unconstrained subtype indication into a constrained one.

   --  The expression may also be transformed in certain cases in order to
   --  avoid multiple evaluation. In the static allocation case, the general
   --  scheme is:

   --     Val : T := Expr;

   --        is transformed into

   --     Val : Constrained_Subtype_of_T := Maybe_Modified_Expr;
   --
   --  Here are the main cases :
   --
   --  <if Expr is a Slice>
   --    Val : T ([Index_Subtype (Expr)]) := Expr;
   --
   --  <elsif Expr is a String Literal>
   --    Val : T (T'First .. T'First + Length (string literal) - 1) := Expr;
   --
   --  <elsif Expr is Constrained>
   --    subtype T is Type_Of_Expr
   --    Val : T := Expr;
   --
   --  <elsif Expr is an entity_name>
   --    Val : T (constraints taken from Expr) := Expr;
   --
   --  <else>
   --    type Axxx is access all T;
   --    Rval : Axxx := Expr'ref;
   --    Val  : T (constraints taken from Rval) := Rval.all;

   --    ??? note: when the Expression is allocated in the secondary stack
   --              we could use it directly instead of copying it by declaring
   --              Val : T (...) renames Rval.all

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id)
   is
      Loc     : constant Source_Ptr := Sloc (N);
      Exp_Typ : constant Entity_Id  := Etype (Exp);
      T       : Entity_Id;

   begin
      --  In general we cannot build the subtype if expansion is disabled,
      --  because internal entities may not have been defined. However, to
      --  avoid some cascaded errors, we try to continue when the expression is
      --  an array (or string), because it is safe to compute the bounds. It is
      --  in fact required to do so even in a generic context, because there
      --  may be constants that depend on the bounds of a string literal, both
      --  standard string types and more generally arrays of characters.

      if not Expander_Active
        and then (No (Etype (Exp)) or else not Is_String_Type (Etype (Exp)))
      then
         return;
      end if;

      if Nkind (Exp) = N_Slice then
         declare
            Slice_Type : constant Entity_Id := Etype (First_Index (Exp_Typ));

         begin
            Rewrite (Subtype_Indic,
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Reference_To (Unc_Type, Loc),
                Constraint =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List
                      (New_Reference_To (Slice_Type, Loc)))));

            --  This subtype indication may be used later for constraint checks
            --  we better make sure that if a variable was used as a bound of
            --  of the original slice, its value is frozen.

            Force_Evaluation (Low_Bound (Scalar_Range (Slice_Type)));
            Force_Evaluation (High_Bound (Scalar_Range (Slice_Type)));
         end;

      elsif Ekind (Exp_Typ) = E_String_Literal_Subtype then
         Rewrite (Subtype_Indic,
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Reference_To (Unc_Type, Loc),
             Constraint =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => New_List (
                   Make_Literal_Range (Loc,
                     Literal_Typ => Exp_Typ)))));

      --  If the type of the expression is an internally generated type it
      --  may not be necessary to create a new subtype. However there are two
      --  exceptions: references to the current instances, and aliased array
      --  object declarations for which the backend needs to create a template.

      elsif Is_Constrained (Exp_Typ)
        and then not Is_Class_Wide_Type (Unc_Type)
        and then
          (Nkind (N) /= N_Object_Declaration
            or else not Is_Entity_Name (Expression (N))
            or else not Comes_From_Source (Entity (Expression (N)))
            or else not Is_Array_Type (Exp_Typ)
            or else not Aliased_Present (N))
      then
         if Is_Itype (Exp_Typ) then

            --  Within an initialization procedure, a selected component
            --  denotes a component of the enclosing record, and it appears as
            --  an actual in a call to its own initialization procedure. If
            --  this component depends on the outer discriminant, we must
            --  generate the proper actual subtype for it.

            if Nkind (Exp) = N_Selected_Component
              and then Within_Init_Proc
            then
               declare
                  Decl : constant Node_Id :=
                           Build_Actual_Subtype_Of_Component (Exp_Typ, Exp);
               begin
                  if Present (Decl) then
                     Insert_Action (N, Decl);
                     T := Defining_Identifier (Decl);
                  else
                     T := Exp_Typ;
                  end if;
               end;

            --  No need to generate a new subtype

            else
               T := Exp_Typ;
            end if;

         else
            T := Make_Temporary (Loc, 'T');

            Insert_Action (N,
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => T,
                Subtype_Indication  => New_Reference_To (Exp_Typ, Loc)));

            --  This type is marked as an itype even though it has an explicit
            --  declaration since otherwise Is_Generic_Actual_Type can get
            --  set, resulting in the generation of spurious errors. (See
            --  sem_ch8.Analyze_Package_Renaming and sem_type.covers)

            Set_Is_Itype (T);
            Set_Associated_Node_For_Itype (T, Exp);
         end if;

         Rewrite (Subtype_Indic, New_Reference_To (T, Loc));

      --  Nothing needs to be done for private types with unknown discriminants
      --  if the underlying type is not an unconstrained composite type or it
      --  is an unchecked union.

      elsif Is_Private_Type (Unc_Type)
        and then Has_Unknown_Discriminants (Unc_Type)
        and then (not Is_Composite_Type (Underlying_Type (Unc_Type))
                   or else Is_Constrained (Underlying_Type (Unc_Type))
                   or else Is_Unchecked_Union (Underlying_Type (Unc_Type)))
      then
         null;

      --  Case of derived type with unknown discriminants where the parent type
      --  also has unknown discriminants.

      elsif Is_Record_Type (Unc_Type)
        and then not Is_Class_Wide_Type (Unc_Type)
        and then Has_Unknown_Discriminants (Unc_Type)
        and then Has_Unknown_Discriminants (Underlying_Type (Unc_Type))
      then
         --  Nothing to be done if no underlying record view available

         if No (Underlying_Record_View (Unc_Type)) then
            null;

         --  Otherwise use the Underlying_Record_View to create the proper
         --  constrained subtype for an object of a derived type with unknown
         --  discriminants.

         else
            Remove_Side_Effects (Exp);
            Rewrite (Subtype_Indic,
              Make_Subtype_From_Expr (Exp, Underlying_Record_View (Unc_Type)));
         end if;

      --  Renamings of class-wide interface types require no equivalent
      --  constrained type declarations because we only need to reference
      --  the tag component associated with the interface. The same is
      --  presumably true for class-wide types in general, so this test
      --  is broadened to include all class-wide renamings, which also
      --  avoids cases of unbounded recursion in Remove_Side_Effects.
      --  (Is this really correct, or are there some cases of class-wide
      --  renamings that require action in this procedure???)

      elsif Present (N)
        and then Nkind (N) = N_Object_Renaming_Declaration
        and then Is_Class_Wide_Type (Unc_Type)
      then
         null;

      --  In Ada 95 nothing to be done if the type of the expression is limited
      --  because in this case the expression cannot be copied, and its use can
      --  only be by reference.

      --  In Ada 2005 the context can be an object declaration whose expression
      --  is a function that returns in place. If the nominal subtype has
      --  unknown discriminants, the call still provides constraints on the
      --  object, and we have to create an actual subtype from it.

      --  If the type is class-wide, the expression is dynamically tagged and
      --  we do not create an actual subtype either. Ditto for an interface.
      --  For now this applies only if the type is immutably limited, and the
      --  function being called is build-in-place. This will have to be revised
      --  when build-in-place functions are generalized to other types.

      elsif Is_Limited_View (Exp_Typ)
        and then
         (Is_Class_Wide_Type (Exp_Typ)
           or else Is_Interface (Exp_Typ)
           or else not Has_Unknown_Discriminants (Exp_Typ)
           or else not Is_Composite_Type (Unc_Type))
      then
         null;

      --  For limited objects initialized with build in place function calls,
      --  nothing to be done; otherwise we prematurely introduce an N_Reference
      --  node in the expression initializing the object, which breaks the
      --  circuitry that detects and adds the additional arguments to the
      --  called function.

      elsif Is_Build_In_Place_Function_Call (Exp) then
         null;

      else
         Remove_Side_Effects (Exp);
         Rewrite (Subtype_Indic,
           Make_Subtype_From_Expr (Exp, Unc_Type));
      end if;
   end Expand_Subtype_From_Expr;

   ------------------------
   -- Find_Interface_ADT --
   ------------------------

   function Find_Interface_ADT
     (T     : Entity_Id;
      Iface : Entity_Id) return Elmt_Id
   is
      ADT : Elmt_Id;
      Typ : Entity_Id := T;

   begin
      pragma Assert (Is_Interface (Iface));

      --  Handle private types

      if Has_Private_Declaration (Typ) and then Present (Full_View (Typ)) then
         Typ := Full_View (Typ);
      end if;

      --  Handle access types

      if Is_Access_Type (Typ) then
         Typ := Designated_Type (Typ);
      end if;

      --  Handle task and protected types implementing interfaces

      if Is_Concurrent_Type (Typ) then
         Typ := Corresponding_Record_Type (Typ);
      end if;

      pragma Assert
        (not Is_Class_Wide_Type (Typ)
          and then Ekind (Typ) /= E_Incomplete_Type);

      if Is_Ancestor (Iface, Typ, Use_Full_View => True) then
         return First_Elmt (Access_Disp_Table (Typ));

      else
         ADT := Next_Elmt (Next_Elmt (First_Elmt (Access_Disp_Table (Typ))));
         while Present (ADT)
           and then Present (Related_Type (Node (ADT)))
           and then Related_Type (Node (ADT)) /= Iface
           and then not Is_Ancestor (Iface, Related_Type (Node (ADT)),
                                     Use_Full_View => True)
         loop
            Next_Elmt (ADT);
         end loop;

         pragma Assert (Present (Related_Type (Node (ADT))));
         return ADT;
      end if;
   end Find_Interface_ADT;

   ------------------------
   -- Find_Interface_Tag --
   ------------------------

   function Find_Interface_Tag
     (T     : Entity_Id;
      Iface : Entity_Id) return Entity_Id
   is
      AI_Tag : Entity_Id;
      Found  : Boolean   := False;
      Typ    : Entity_Id := T;

      procedure Find_Tag (Typ : Entity_Id);
      --  Internal subprogram used to recursively climb to the ancestors

      --------------
      -- Find_Tag --
      --------------

      procedure Find_Tag (Typ : Entity_Id) is
         AI_Elmt : Elmt_Id;
         AI      : Node_Id;

      begin
         --  This routine does not handle the case in which the interface is an
         --  ancestor of Typ. That case is handled by the enclosing subprogram.

         pragma Assert (Typ /= Iface);

         --  Climb to the root type handling private types

         if Present (Full_View (Etype (Typ))) then
            if Full_View (Etype (Typ)) /= Typ then
               Find_Tag (Full_View (Etype (Typ)));
            end if;

         elsif Etype (Typ) /= Typ then
            Find_Tag (Etype (Typ));
         end if;

         --  Traverse the list of interfaces implemented by the type

         if not Found
           and then Present (Interfaces (Typ))
           and then not (Is_Empty_Elmt_List (Interfaces (Typ)))
         then
            --  Skip the tag associated with the primary table

            pragma Assert (Etype (First_Tag_Component (Typ)) = RTE (RE_Tag));
            AI_Tag := Next_Tag_Component (First_Tag_Component (Typ));
            pragma Assert (Present (AI_Tag));

            AI_Elmt := First_Elmt (Interfaces (Typ));
            while Present (AI_Elmt) loop
               AI := Node (AI_Elmt);

               if AI = Iface
                 or else Is_Ancestor (Iface, AI, Use_Full_View => True)
               then
                  Found := True;
                  return;
               end if;

               AI_Tag := Next_Tag_Component (AI_Tag);
               Next_Elmt (AI_Elmt);
            end loop;
         end if;
      end Find_Tag;

   --  Start of processing for Find_Interface_Tag

   begin
      pragma Assert (Is_Interface (Iface));

      --  Handle access types

      if Is_Access_Type (Typ) then
         Typ := Designated_Type (Typ);
      end if;

      --  Handle class-wide types

      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      --  Handle private types

      if Has_Private_Declaration (Typ) and then Present (Full_View (Typ)) then
         Typ := Full_View (Typ);
      end if;

      --  Handle entities from the limited view

      if Ekind (Typ) = E_Incomplete_Type then
         pragma Assert (Present (Non_Limited_View (Typ)));
         Typ := Non_Limited_View (Typ);
      end if;

      --  Handle task and protected types implementing interfaces

      if Is_Concurrent_Type (Typ) then
         Typ := Corresponding_Record_Type (Typ);
      end if;

      --  If the interface is an ancestor of the type, then it shared the
      --  primary dispatch table.

      if Is_Ancestor (Iface, Typ, Use_Full_View => True) then
         pragma Assert (Etype (First_Tag_Component (Typ)) = RTE (RE_Tag));
         return First_Tag_Component (Typ);

      --  Otherwise we need to search for its associated tag component

      else
         Find_Tag (Typ);
         pragma Assert (Found);
         return AI_Tag;
      end if;
   end Find_Interface_Tag;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op (T : Entity_Id; Name : Name_Id) return Entity_Id is
      Prim : Elmt_Id;
      Typ  : Entity_Id := T;
      Op   : Entity_Id;

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Typ := Underlying_Type (Typ);

      --  Loop through primitive operations

      Prim := First_Elmt (Primitive_Operations (Typ));
      while Present (Prim) loop
         Op := Node (Prim);

         --  We can retrieve primitive operations by name if it is an internal
         --  name. For equality we must check that both of its operands have
         --  the same type, to avoid confusion with user-defined equalities
         --  than may have a non-symmetric signature.

         exit when Chars (Op) = Name
           and then
             (Name /= Name_Op_Eq
               or else Etype (First_Formal (Op)) = Etype (Last_Formal (Op)));

         Next_Elmt (Prim);

         --  Raise Program_Error if no primitive found

         if No (Prim) then
            raise Program_Error;
         end if;
      end loop;

      return Node (Prim);
   end Find_Prim_Op;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op
     (T    : Entity_Id;
      Name : TSS_Name_Type) return Entity_Id
   is
      Inher_Op  : Entity_Id := Empty;
      Own_Op    : Entity_Id := Empty;
      Prim_Elmt : Elmt_Id;
      Prim_Id   : Entity_Id;
      Typ       : Entity_Id := T;

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Typ := Underlying_Type (Typ);

      --  This search is based on the assertion that the dispatching version
      --  of the TSS routine always precedes the real primitive.

      Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
      while Present (Prim_Elmt) loop
         Prim_Id := Node (Prim_Elmt);

         if Is_TSS (Prim_Id, Name) then
            if Present (Alias (Prim_Id)) then
               Inher_Op := Prim_Id;
            else
               Own_Op := Prim_Id;
            end if;
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;

      if Present (Own_Op) then
         return Own_Op;
      elsif Present (Inher_Op) then
         return Inher_Op;
      else
         raise Program_Error;
      end if;
   end Find_Prim_Op;

   ----------------------------
   -- Find_Protection_Object --
   ----------------------------

   function Find_Protection_Object (Scop : Entity_Id) return Entity_Id is
      S : Entity_Id;

   begin
      S := Scop;
      while Present (S) loop
         if Ekind_In (S, E_Entry, E_Entry_Family, E_Function, E_Procedure)
           and then Present (Protection_Object (S))
         then
            return Protection_Object (S);
         end if;

         S := Scope (S);
      end loop;

      --  If we do not find a Protection object in the scope chain, then
      --  something has gone wrong, most likely the object was never created.

      raise Program_Error;
   end Find_Protection_Object;

   --------------------------
   -- Find_Protection_Type --
   --------------------------

   function Find_Protection_Type (Conc_Typ : Entity_Id) return Entity_Id is
      Comp : Entity_Id;
      Typ  : Entity_Id := Conc_Typ;

   begin
      if Is_Concurrent_Type (Typ) then
         Typ := Corresponding_Record_Type (Typ);
      end if;

      --  Since restriction violations are not considered serious errors, the
      --  expander remains active, but may leave the corresponding record type
      --  malformed. In such cases, component _object is not available so do
      --  not look for it.

      if not Analyzed (Typ) then
         return Empty;
      end if;

      Comp := First_Component (Typ);
      while Present (Comp) loop
         if Chars (Comp) = Name_uObject then
            return Base_Type (Etype (Comp));
         end if;

         Next_Component (Comp);
      end loop;

      --  The corresponding record of a protected type should always have an
      --  _object field.

      raise Program_Error;
   end Find_Protection_Type;

   ----------------------
   -- Force_Evaluation --
   ----------------------

   procedure Force_Evaluation (Exp : Node_Id; Name_Req : Boolean := False) is
   begin
      Remove_Side_Effects (Exp, Name_Req, Variable_Ref => True);
   end Force_Evaluation;

   ---------------------------------
   -- Fully_Qualified_Name_String --
   ---------------------------------

   function Fully_Qualified_Name_String
     (E          : Entity_Id;
      Append_NUL : Boolean := True) return String_Id
   is
      procedure Internal_Full_Qualified_Name (E : Entity_Id);
      --  Compute recursively the qualified name without NUL at the end, adding
      --  it to the currently started string being generated

      ----------------------------------
      -- Internal_Full_Qualified_Name --
      ----------------------------------

      procedure Internal_Full_Qualified_Name (E : Entity_Id) is
         Ent : Entity_Id;

      begin
         --  Deal properly with child units

         if Nkind (E) = N_Defining_Program_Unit_Name then
            Ent := Defining_Identifier (E);
         else
            Ent := E;
         end if;

         --  Compute qualification recursively (only "Standard" has no scope)

         if Present (Scope (Scope (Ent))) then
            Internal_Full_Qualified_Name (Scope (Ent));
            Store_String_Char (Get_Char_Code ('.'));
         end if;

         --  Every entity should have a name except some expanded blocks
         --  don't bother about those.

         if Chars (Ent) = No_Name then
            return;
         end if;

         --  Generates the entity name in upper case

         Get_Decoded_Name_String (Chars (Ent));
         Set_All_Upper_Case;
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         return;
      end Internal_Full_Qualified_Name;

   --  Start of processing for Full_Qualified_Name

   begin
      Start_String;
      Internal_Full_Qualified_Name (E);

      if Append_NUL then
         Store_String_Char (Get_Char_Code (ASCII.NUL));
      end if;

      return End_String;
   end Fully_Qualified_Name_String;

   ------------------------
   -- Generate_Poll_Call --
   ------------------------

   procedure Generate_Poll_Call (N : Node_Id) is
   begin
      --  No poll call if polling not active

      if not Polling_Required then
         return;

      --  Otherwise generate require poll call

      else
         Insert_Before_And_Analyze (N,
           Make_Procedure_Call_Statement (Sloc (N),
             Name => New_Occurrence_Of (RTE (RE_Poll), Sloc (N))));
      end if;
   end Generate_Poll_Call;

   ---------------------------------
   -- Get_Current_Value_Condition --
   ---------------------------------

   --  Note: the implementation of this procedure is very closely tied to the
   --  implementation of Set_Current_Value_Condition. In the Get procedure, we
   --  interpret Current_Value fields set by the Set procedure, so the two
   --  procedures need to be closely coordinated.

   procedure Get_Current_Value_Condition
     (Var : Node_Id;
      Op  : out Node_Kind;
      Val : out Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Var);
      Ent : constant Entity_Id  := Entity (Var);

      procedure Process_Current_Value_Condition
        (N : Node_Id;
         S : Boolean);
      --  N is an expression which holds either True (S = True) or False (S =
      --  False) in the condition. This procedure digs out the expression and
      --  if it refers to Ent, sets Op and Val appropriately.

      -------------------------------------
      -- Process_Current_Value_Condition --
      -------------------------------------

      procedure Process_Current_Value_Condition
        (N : Node_Id;
         S : Boolean)
      is
         Cond      : Node_Id;
         Prev_Cond : Node_Id;
         Sens      : Boolean;

      begin
         Cond := N;
         Sens := S;

         loop
            Prev_Cond := Cond;

            --  Deal with NOT operators, inverting sense

            while Nkind (Cond) = N_Op_Not loop
               Cond := Right_Opnd (Cond);
               Sens := not Sens;
            end loop;

            --  Deal with conversions, qualifications, and expressions with
            --  actions.

            while Nkind_In (Cond,
                    N_Type_Conversion,
                    N_Qualified_Expression,
                    N_Expression_With_Actions)
            loop
               Cond := Expression (Cond);
            end loop;

            exit when Cond = Prev_Cond;
         end loop;

         --  Deal with AND THEN and AND cases

         if Nkind_In (Cond, N_And_Then, N_Op_And) then

            --  Don't ever try to invert a condition that is of the form of an
            --  AND or AND THEN (since we are not doing sufficiently general
            --  processing to allow this).

            if Sens = False then
               Op  := N_Empty;
               Val := Empty;
               return;
            end if;

            --  Recursively process AND and AND THEN branches

            Process_Current_Value_Condition (Left_Opnd (Cond), True);

            if Op /= N_Empty then
               return;
            end if;

            Process_Current_Value_Condition (Right_Opnd (Cond), True);
            return;

         --  Case of relational operator

         elsif Nkind (Cond) in N_Op_Compare then
            Op := Nkind (Cond);

            --  Invert sense of test if inverted test

            if Sens = False then
               case Op is
                  when N_Op_Eq => Op := N_Op_Ne;
                  when N_Op_Ne => Op := N_Op_Eq;
                  when N_Op_Lt => Op := N_Op_Ge;
                  when N_Op_Gt => Op := N_Op_Le;
                  when N_Op_Le => Op := N_Op_Gt;
                  when N_Op_Ge => Op := N_Op_Lt;
                  when others  => raise Program_Error;
               end case;
            end if;

            --  Case of entity op value

            if Is_Entity_Name (Left_Opnd (Cond))
              and then Ent = Entity (Left_Opnd (Cond))
              and then Compile_Time_Known_Value (Right_Opnd (Cond))
            then
               Val := Right_Opnd (Cond);

            --  Case of value op entity

            elsif Is_Entity_Name (Right_Opnd (Cond))
              and then Ent = Entity (Right_Opnd (Cond))
              and then Compile_Time_Known_Value (Left_Opnd (Cond))
            then
               Val := Left_Opnd (Cond);

               --  We are effectively swapping operands

               case Op is
                  when N_Op_Eq => null;
                  when N_Op_Ne => null;
                  when N_Op_Lt => Op := N_Op_Gt;
                  when N_Op_Gt => Op := N_Op_Lt;
                  when N_Op_Le => Op := N_Op_Ge;
                  when N_Op_Ge => Op := N_Op_Le;
                  when others  => raise Program_Error;
               end case;

            else
               Op := N_Empty;
            end if;

            return;

         elsif Nkind_In (Cond,
                 N_Type_Conversion,
                 N_Qualified_Expression,
                 N_Expression_With_Actions)
         then
            Cond := Expression (Cond);

         --  Case of Boolean variable reference, return as though the
         --  reference had said var = True.

         else
            if Is_Entity_Name (Cond) and then Ent = Entity (Cond) then
               Val := New_Occurrence_Of (Standard_True, Sloc (Cond));

               if Sens = False then
                  Op := N_Op_Ne;
               else
                  Op := N_Op_Eq;
               end if;
            end if;
         end if;
      end Process_Current_Value_Condition;

   --  Start of processing for Get_Current_Value_Condition

   begin
      Op  := N_Empty;
      Val := Empty;

      --  Immediate return, nothing doing, if this is not an object

      if Ekind (Ent) not in Object_Kind then
         return;
      end if;

      --  Otherwise examine current value

      declare
         CV   : constant Node_Id := Current_Value (Ent);
         Sens : Boolean;
         Stm  : Node_Id;

      begin
         --  If statement. Condition is known true in THEN section, known False
         --  in any ELSIF or ELSE part, and unknown outside the IF statement.

         if Nkind (CV) = N_If_Statement then

            --  Before start of IF statement

            if Loc < Sloc (CV) then
               return;

               --  After end of IF statement

            elsif Loc >= Sloc (CV) + Text_Ptr (UI_To_Int (End_Span (CV))) then
               return;
            end if;

            --  At this stage we know that we are within the IF statement, but
            --  unfortunately, the tree does not record the SLOC of the ELSE so
            --  we cannot use a simple SLOC comparison to distinguish between
            --  the then/else statements, so we have to climb the tree.

            declare
               N : Node_Id;

            begin
               N := Parent (Var);
               while Parent (N) /= CV loop
                  N := Parent (N);

                  --  If we fall off the top of the tree, then that's odd, but
                  --  perhaps it could occur in some error situation, and the
                  --  safest response is simply to assume that the outcome of
                  --  the condition is unknown. No point in bombing during an
                  --  attempt to optimize things.

                  if No (N) then
                     return;
                  end if;
               end loop;

               --  Now we have N pointing to a node whose parent is the IF
               --  statement in question, so now we can tell if we are within
               --  the THEN statements.

               if Is_List_Member (N)
                 and then List_Containing (N) = Then_Statements (CV)
               then
                  Sens := True;

               --  If the variable reference does not come from source, we
               --  cannot reliably tell whether it appears in the else part.
               --  In particular, if it appears in generated code for a node
               --  that requires finalization, it may be attached to a list
               --  that has not been yet inserted into the code. For now,
               --  treat it as unknown.

               elsif not Comes_From_Source (N) then
                  return;

               --  Otherwise we must be in ELSIF or ELSE part

               else
                  Sens := False;
               end if;
            end;

            --  ELSIF part. Condition is known true within the referenced
            --  ELSIF, known False in any subsequent ELSIF or ELSE part,
            --  and unknown before the ELSE part or after the IF statement.

         elsif Nkind (CV) = N_Elsif_Part then

            --  if the Elsif_Part had condition_actions, the elsif has been
            --  rewritten as a nested if, and the original elsif_part is
            --  detached from the tree, so there is no way to obtain useful
            --  information on the current value of the variable.
            --  Can this be improved ???

            if No (Parent (CV)) then
               return;
            end if;

            Stm := Parent (CV);

            --  Before start of ELSIF part

            if Loc < Sloc (CV) then
               return;

               --  After end of IF statement

            elsif Loc >= Sloc (Stm) +
              Text_Ptr (UI_To_Int (End_Span (Stm)))
            then
               return;
            end if;

            --  Again we lack the SLOC of the ELSE, so we need to climb the
            --  tree to see if we are within the ELSIF part in question.

            declare
               N : Node_Id;

            begin
               N := Parent (Var);
               while Parent (N) /= Stm loop
                  N := Parent (N);

                  --  If we fall off the top of the tree, then that's odd, but
                  --  perhaps it could occur in some error situation, and the
                  --  safest response is simply to assume that the outcome of
                  --  the condition is unknown. No point in bombing during an
                  --  attempt to optimize things.

                  if No (N) then
                     return;
                  end if;
               end loop;

               --  Now we have N pointing to a node whose parent is the IF
               --  statement in question, so see if is the ELSIF part we want.
               --  the THEN statements.

               if N = CV then
                  Sens := True;

                  --  Otherwise we must be in subsequent ELSIF or ELSE part

               else
                  Sens := False;
               end if;
            end;

         --  Iteration scheme of while loop. The condition is known to be
         --  true within the body of the loop.

         elsif Nkind (CV) = N_Iteration_Scheme then
            declare
               Loop_Stmt : constant Node_Id := Parent (CV);

            begin
               --  Before start of body of loop

               if Loc < Sloc (Loop_Stmt) then
                  return;

               --  After end of LOOP statement

               elsif Loc >= Sloc (End_Label (Loop_Stmt)) then
                  return;

               --  We are within the body of the loop

               else
                  Sens := True;
               end if;
            end;

         --  All other cases of Current_Value settings

         else
            return;
         end if;

         --  If we fall through here, then we have a reportable condition, Sens
         --  is True if the condition is true and False if it needs inverting.

         Process_Current_Value_Condition (Condition (CV), Sens);
      end;
   end Get_Current_Value_Condition;

   ---------------------
   -- Get_Stream_Size --
   ---------------------

   function Get_Stream_Size (E : Entity_Id) return Uint is
   begin
      --  If we have a Stream_Size clause for this type use it

      if Has_Stream_Size_Clause (E) then
         return Static_Integer (Expression (Stream_Size_Clause (E)));

      --  Otherwise the Stream_Size if the size of the type

      else
         return Esize (E);
      end if;
   end Get_Stream_Size;

   ---------------------------
   -- Has_Access_Constraint --
   ---------------------------

   function Has_Access_Constraint (E : Entity_Id) return Boolean is
      Disc : Entity_Id;
      T    : constant Entity_Id := Etype (E);

   begin
      if Has_Per_Object_Constraint (E) and then Has_Discriminants (T) then
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

   ----------------------------------
   -- Has_Following_Address_Clause --
   ----------------------------------

   --  Should this function check the private part in a package ???

   function Has_Following_Address_Clause (D : Node_Id) return Boolean is
      Id   : constant Entity_Id := Defining_Identifier (D);
      Decl : Node_Id;

   begin
      Decl := Next (D);
      while Present (Decl) loop
         if Nkind (Decl) = N_At_Clause
           and then Chars (Identifier (Decl)) = Chars (Id)
         then
            return True;

         elsif Nkind (Decl) = N_Attribute_Definition_Clause
           and then Chars (Decl) = Name_Address
           and then Chars (Name (Decl)) = Chars (Id)
         then
            return True;
         end if;

         Next (Decl);
      end loop;

      return False;
   end Has_Following_Address_Clause;

   --------------------
   -- Homonym_Number --
   --------------------

   function Homonym_Number (Subp : Entity_Id) return Nat is
      Count : Nat;
      Hom   : Entity_Id;

   begin
      Count := 1;
      Hom := Homonym (Subp);
      while Present (Hom) loop
         if Scope (Hom) = Scope (Subp) then
            Count := Count + 1;
         end if;

         Hom := Homonym (Hom);
      end loop;

      return Count;
   end Homonym_Number;

   -----------------------------------
   -- In_Library_Level_Package_Body --
   -----------------------------------

   function In_Library_Level_Package_Body (Id : Entity_Id) return Boolean is
   begin
      --  First determine whether the entity appears at the library level, then
      --  look at the containing unit.

      if Is_Library_Level_Entity (Id) then
         declare
            Container : constant Node_Id := Cunit (Get_Source_Unit (Id));

         begin
            return Nkind (Unit (Container)) = N_Package_Body;
         end;
      end if;

      return False;
   end In_Library_Level_Package_Body;

   ------------------------------
   -- In_Unconditional_Context --
   ------------------------------

   function In_Unconditional_Context (Node : Node_Id) return Boolean is
      P : Node_Id;

   begin
      P := Node;
      while Present (P) loop
         case Nkind (P) is
            when N_Subprogram_Body =>
               return True;

            when N_If_Statement =>
               return False;

            when N_Loop_Statement =>
               return False;

            when N_Case_Statement =>
               return False;

            when others =>
               P := Parent (P);
         end case;
      end loop;

      return False;
   end In_Unconditional_Context;

   -------------------
   -- Insert_Action --
   -------------------

   procedure Insert_Action (Assoc_Node : Node_Id; Ins_Action : Node_Id) is
   begin
      if Present (Ins_Action) then
         Insert_Actions (Assoc_Node, New_List (Ins_Action));
      end if;
   end Insert_Action;

   --  Version with check(s) suppressed

   procedure Insert_Action
     (Assoc_Node : Node_Id; Ins_Action : Node_Id; Suppress : Check_Id)
   is
   begin
      Insert_Actions (Assoc_Node, New_List (Ins_Action), Suppress);
   end Insert_Action;

   -------------------------
   -- Insert_Action_After --
   -------------------------

   procedure Insert_Action_After
     (Assoc_Node : Node_Id;
      Ins_Action : Node_Id)
   is
   begin
      Insert_Actions_After (Assoc_Node, New_List (Ins_Action));
   end Insert_Action_After;

   --------------------
   -- Insert_Actions --
   --------------------

   procedure Insert_Actions (Assoc_Node : Node_Id; Ins_Actions : List_Id) is
      N : Node_Id;
      P : Node_Id;

      Wrapped_Node : Node_Id := Empty;

   begin
      if No (Ins_Actions) or else Is_Empty_List (Ins_Actions) then
         return;
      end if;

      --  Ignore insert of actions from inside default expression (or other
      --  similar "spec expression") in the special spec-expression analyze
      --  mode. Any insertions at this point have no relevance, since we are
      --  only doing the analyze to freeze the types of any static expressions.
      --  See section "Handling of Default Expressions" in the spec of package
      --  Sem for further details.

      if In_Spec_Expression then
         return;
      end if;

      --  If the action derives from stuff inside a record, then the actions
      --  are attached to the current scope, to be inserted and analyzed on
      --  exit from the scope. The reason for this is that we may also be
      --  generating freeze actions at the same time, and they must eventually
      --  be elaborated in the correct order.

      if Is_Record_Type (Current_Scope)
        and then not Is_Frozen (Current_Scope)
      then
         if No (Scope_Stack.Table
                  (Scope_Stack.Last).Pending_Freeze_Actions)
         then
            Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions :=
              Ins_Actions;
         else
            Append_List
              (Ins_Actions,
               Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions);
         end if;

         return;
      end if;

      --  We now intend to climb up the tree to find the right point to
      --  insert the actions. We start at Assoc_Node, unless this node is a
      --  subexpression in which case we start with its parent. We do this for
      --  two reasons. First it speeds things up. Second, if Assoc_Node is
      --  itself one of the special nodes like N_And_Then, then we assume that
      --  an initial request to insert actions for such a node does not expect
      --  the actions to get deposited in the node for later handling when the
      --  node is expanded, since clearly the node is being dealt with by the
      --  caller. Note that in the subexpression case, N is always the child we
      --  came from.

      --  N_Raise_xxx_Error is an annoying special case, it is a statement if
      --  it has type Standard_Void_Type, and a subexpression otherwise.
      --  otherwise. Procedure calls, and similarly procedure attribute
      --  references, are also statements.

      if Nkind (Assoc_Node) in N_Subexpr
        and then (Nkind (Assoc_Node) not in N_Raise_xxx_Error
                   or else Etype (Assoc_Node) /= Standard_Void_Type)
        and then Nkind (Assoc_Node) /= N_Procedure_Call_Statement
        and then (Nkind (Assoc_Node) /= N_Attribute_Reference
                   or else
                     not Is_Procedure_Attribute_Name
                           (Attribute_Name (Assoc_Node)))
      then
         N := Assoc_Node;
         P := Parent (Assoc_Node);

      --  Non-subexpression case. Note that N is initially Empty in this case
      --  (N is only guaranteed Non-Empty in the subexpr case).

      else
         N := Empty;
         P := Assoc_Node;
      end if;

      --  Capture root of the transient scope

      if Scope_Is_Transient then
         Wrapped_Node := Node_To_Be_Wrapped;
      end if;

      loop
         pragma Assert (Present (P));

         --  Make sure that inserted actions stay in the transient scope

         if Present (Wrapped_Node) and then N = Wrapped_Node then
            Store_Before_Actions_In_Scope (Ins_Actions);
            return;
         end if;

         case Nkind (P) is

            --  Case of right operand of AND THEN or OR ELSE. Put the actions
            --  in the Actions field of the right operand. They will be moved
            --  out further when the AND THEN or OR ELSE operator is expanded.
            --  Nothing special needs to be done for the left operand since
            --  in that case the actions are executed unconditionally.

            when N_Short_Circuit =>
               if N = Right_Opnd (P) then

                  --  We are now going to either append the actions to the
                  --  actions field of the short-circuit operation. We will
                  --  also analyze the actions now.

                  --  This analysis is really too early, the proper thing would
                  --  be to just park them there now, and only analyze them if
                  --  we find we really need them, and to it at the proper
                  --  final insertion point. However attempting to this proved
                  --  tricky, so for now we just kill current values before and
                  --  after the analyze call to make sure we avoid peculiar
                  --  optimizations from this out of order insertion.

                  Kill_Current_Values;

                  if Present (Actions (P)) then
                     Insert_List_After_And_Analyze
                       (Last (Actions (P)), Ins_Actions);
                  else
                     Set_Actions (P, Ins_Actions);
                     Analyze_List (Actions (P));
                  end if;

                  Kill_Current_Values;

                  return;
               end if;

            --  Then or Else dependent expression of an if expression. Add
            --  actions to Then_Actions or Else_Actions field as appropriate.
            --  The actions will be moved further out when the if is expanded.

            when N_If_Expression =>
               declare
                  ThenX : constant Node_Id := Next (First (Expressions (P)));
                  ElseX : constant Node_Id := Next (ThenX);

               begin
                  --  If the enclosing expression is already analyzed, as
                  --  is the case for nested elaboration checks, insert the
                  --  conditional further out.

                  if Analyzed (P) then
                     null;

                  --  Actions belong to the then expression, temporarily place
                  --  them as Then_Actions of the if expression. They will be
                  --  moved to the proper place later when the if expression
                  --  is expanded.

                  elsif N = ThenX then
                     if Present (Then_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Then_Actions (P)), Ins_Actions);
                     else
                        Set_Then_Actions (P, Ins_Actions);
                        Analyze_List (Then_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the else expression, temporarily place
                  --  them as Else_Actions of the if expression. They will be
                  --  moved to the proper place later when the if expression
                  --  is expanded.

                  elsif N = ElseX then
                     if Present (Else_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Else_Actions (P)), Ins_Actions);
                     else
                        Set_Else_Actions (P, Ins_Actions);
                        Analyze_List (Else_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the condition. In this case they are
                  --  unconditionally executed, and so we can continue the
                  --  search for the proper insert point.

                  else
                     null;
                  end if;
               end;

            --  Alternative of case expression, we place the action in the
            --  Actions field of the case expression alternative, this will
            --  be handled when the case expression is expanded.

            when N_Case_Expression_Alternative =>
               if Present (Actions (P)) then
                  Insert_List_After_And_Analyze
                    (Last (Actions (P)), Ins_Actions);
               else
                  Set_Actions (P, Ins_Actions);
                  Analyze_List (Actions (P));
               end if;

               return;

            --  Case of appearing within an Expressions_With_Actions node. When
            --  the new actions come from the expression of the expression with
            --  actions, they must be added to the existing actions. The other
            --  alternative is when the new actions are related to one of the
            --  existing actions of the expression with actions. In that case
            --  they must be inserted further up the tree.

            when N_Expression_With_Actions =>
               if N = Expression (P) then
                  if Is_Empty_List (Actions (P)) then
                     Append_List_To (Actions (P), Ins_Actions);
                     Analyze_List (Actions (P));
                  else
                     Insert_List_After_And_Analyze
                       (Last (Actions (P)), Ins_Actions);
                  end if;
                  return;
               end if;

            --  Case of appearing in the condition of a while expression or
            --  elsif. We insert the actions into the Condition_Actions field.
            --  They will be moved further out when the while loop or elsif
            --  is analyzed.

            when N_Iteration_Scheme |
                 N_Elsif_Part
            =>
               if N = Condition (P) then
                  if Present (Condition_Actions (P)) then
                     Insert_List_After_And_Analyze
                       (Last (Condition_Actions (P)), Ins_Actions);
                  else
                     Set_Condition_Actions (P, Ins_Actions);

                     --  Set the parent of the insert actions explicitly. This
                     --  is not a syntactic field, but we need the parent field
                     --  set, in particular so that freeze can understand that
                     --  it is dealing with condition actions, and properly
                     --  insert the freezing actions.

                     Set_Parent (Ins_Actions, P);
                     Analyze_List (Condition_Actions (P));
                  end if;

                  return;
               end if;

            --  Statements, declarations, pragmas, representation clauses

            when
               --  Statements

               N_Procedure_Call_Statement               |
               N_Statement_Other_Than_Procedure_Call    |

               --  Pragmas

               N_Pragma                                 |

               --  Representation_Clause

               N_At_Clause                              |
               N_Attribute_Definition_Clause            |
               N_Enumeration_Representation_Clause      |
               N_Record_Representation_Clause           |

               --  Declarations

               N_Abstract_Subprogram_Declaration        |
               N_Entry_Body                             |
               N_Exception_Declaration                  |
               N_Exception_Renaming_Declaration         |
               N_Expression_Function                    |
               N_Formal_Abstract_Subprogram_Declaration |
               N_Formal_Concrete_Subprogram_Declaration |
               N_Formal_Object_Declaration              |
               N_Formal_Type_Declaration                |
               N_Full_Type_Declaration                  |
               N_Function_Instantiation                 |
               N_Generic_Function_Renaming_Declaration  |
               N_Generic_Package_Declaration            |
               N_Generic_Package_Renaming_Declaration   |
               N_Generic_Procedure_Renaming_Declaration |
               N_Generic_Subprogram_Declaration         |
               N_Implicit_Label_Declaration             |
               N_Incomplete_Type_Declaration            |
               N_Number_Declaration                     |
               N_Object_Declaration                     |
               N_Object_Renaming_Declaration            |
               N_Package_Body                           |
               N_Package_Body_Stub                      |
               N_Package_Declaration                    |
               N_Package_Instantiation                  |
               N_Package_Renaming_Declaration           |
               N_Private_Extension_Declaration          |
               N_Private_Type_Declaration               |
               N_Procedure_Instantiation                |
               N_Protected_Body                         |
               N_Protected_Body_Stub                    |
               N_Protected_Type_Declaration             |
               N_Single_Task_Declaration                |
               N_Subprogram_Body                        |
               N_Subprogram_Body_Stub                   |
               N_Subprogram_Declaration                 |
               N_Subprogram_Renaming_Declaration        |
               N_Subtype_Declaration                    |
               N_Task_Body                              |
               N_Task_Body_Stub                         |
               N_Task_Type_Declaration                  |

               --  Use clauses can appear in lists of declarations

               N_Use_Package_Clause                     |
               N_Use_Type_Clause                        |

               --  Freeze entity behaves like a declaration or statement

               N_Freeze_Entity                          |
               N_Freeze_Generic_Entity
            =>
               --  Do not insert here if the item is not a list member (this
               --  happens for example with a triggering statement, and the
               --  proper approach is to insert before the entire select).

               if not Is_List_Member (P) then
                  null;

               --  Do not insert if parent of P is an N_Component_Association
               --  node (i.e. we are in the context of an N_Aggregate or
               --  N_Extension_Aggregate node. In this case we want to insert
               --  before the entire aggregate.

               elsif Nkind (Parent (P)) = N_Component_Association then
                  null;

               --  Do not insert if the parent of P is either an N_Variant node
               --  or an N_Record_Definition node, meaning in either case that
               --  P is a member of a component list, and that therefore the
               --  actions should be inserted outside the complete record
               --  declaration.

               elsif Nkind_In (Parent (P), N_Variant, N_Record_Definition) then
                  null;

               --  Do not insert freeze nodes within the loop generated for
               --  an aggregate, because they may be elaborated too late for
               --  subsequent use in the back end: within a package spec the
               --  loop is part of the elaboration procedure and is only
               --  elaborated during the second pass.

               --  If the loop comes from source, or the entity is local to the
               --  loop itself it must remain within.

               elsif Nkind (Parent (P)) = N_Loop_Statement
                 and then not Comes_From_Source (Parent (P))
                 and then Nkind (First (Ins_Actions)) = N_Freeze_Entity
                 and then
                   Scope (Entity (First (Ins_Actions))) /= Current_Scope
               then
                  null;

               --  Otherwise we can go ahead and do the insertion

               elsif P = Wrapped_Node then
                  Store_Before_Actions_In_Scope (Ins_Actions);
                  return;

               else
                  Insert_List_Before_And_Analyze (P, Ins_Actions);
                  return;
               end if;

            --  A special case, N_Raise_xxx_Error can act either as a statement
            --  or a subexpression. We tell the difference by looking at the
            --  Etype. It is set to Standard_Void_Type in the statement case.

            when
               N_Raise_xxx_Error =>
                  if Etype (P) = Standard_Void_Type then
                     if  P = Wrapped_Node then
                        Store_Before_Actions_In_Scope (Ins_Actions);
                     else
                        Insert_List_Before_And_Analyze (P, Ins_Actions);
                     end if;

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  If a component association appears within a loop created for
            --  an array aggregate, attach the actions to the association so
            --  they can be subsequently inserted within the loop. For other
            --  component associations insert outside of the aggregate. For
            --  an association that will generate a loop, its Loop_Actions
            --  attribute is already initialized (see exp_aggr.adb).

            --  The list of loop_actions can in turn generate additional ones,
            --  that are inserted before the associated node. If the associated
            --  node is outside the aggregate, the new actions are collected
            --  at the end of the loop actions, to respect the order in which
            --  they are to be elaborated.

            when
               N_Component_Association =>
                  if Nkind (Parent (P)) = N_Aggregate
                    and then Present (Loop_Actions (P))
                  then
                     if Is_Empty_List (Loop_Actions (P)) then
                        Set_Loop_Actions (P, Ins_Actions);
                        Analyze_List (Ins_Actions);

                     else
                        declare
                           Decl : Node_Id;

                        begin
                           --  Check whether these actions were generated by a
                           --  declaration that is part of the loop_ actions
                           --  for the component_association.

                           Decl := Assoc_Node;
                           while Present (Decl) loop
                              exit when Parent (Decl) = P
                                and then Is_List_Member (Decl)
                                and then
                                  List_Containing (Decl) = Loop_Actions (P);
                              Decl := Parent (Decl);
                           end loop;

                           if Present (Decl) then
                              Insert_List_Before_And_Analyze
                                (Decl, Ins_Actions);
                           else
                              Insert_List_After_And_Analyze
                                (Last (Loop_Actions (P)), Ins_Actions);
                           end if;
                        end;
                     end if;

                     return;

                  else
                     null;
                  end if;

            --  Another special case, an attribute denoting a procedure call

            when
               N_Attribute_Reference =>
                  if Is_Procedure_Attribute_Name (Attribute_Name (P)) then
                     if P = Wrapped_Node then
                        Store_Before_Actions_In_Scope (Ins_Actions);
                     else
                        Insert_List_Before_And_Analyze (P, Ins_Actions);
                     end if;

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  A contract node should not belong to the tree

            when N_Contract =>
               raise Program_Error;

            --  For all other node types, keep climbing tree

            when
               N_Abortable_Part                         |
               N_Accept_Alternative                     |
               N_Access_Definition                      |
               N_Access_Function_Definition             |
               N_Access_Procedure_Definition            |
               N_Access_To_Object_Definition            |
               N_Aggregate                              |
               N_Allocator                              |
               N_Aspect_Specification                   |
               N_Case_Expression                        |
               N_Case_Statement_Alternative             |
               N_Character_Literal                      |
               N_Compilation_Unit                       |
               N_Compilation_Unit_Aux                   |
               N_Component_Clause                       |
               N_Component_Declaration                  |
               N_Component_Definition                   |
               N_Component_List                         |
               N_Constrained_Array_Definition           |
               N_Decimal_Fixed_Point_Definition         |
               N_Defining_Character_Literal             |
               N_Defining_Identifier                    |
               N_Defining_Operator_Symbol               |
               N_Defining_Program_Unit_Name             |
               N_Delay_Alternative                      |
               N_Delta_Constraint                       |
               N_Derived_Type_Definition                |
               N_Designator                             |
               N_Digits_Constraint                      |
               N_Discriminant_Association               |
               N_Discriminant_Specification             |
               N_Empty                                  |
               N_Entry_Body_Formal_Part                 |
               N_Entry_Call_Alternative                 |
               N_Entry_Declaration                      |
               N_Entry_Index_Specification              |
               N_Enumeration_Type_Definition            |
               N_Error                                  |
               N_Exception_Handler                      |
               N_Expanded_Name                          |
               N_Explicit_Dereference                   |
               N_Extension_Aggregate                    |
               N_Floating_Point_Definition              |
               N_Formal_Decimal_Fixed_Point_Definition  |
               N_Formal_Derived_Type_Definition         |
               N_Formal_Discrete_Type_Definition        |
               N_Formal_Floating_Point_Definition       |
               N_Formal_Modular_Type_Definition         |
               N_Formal_Ordinary_Fixed_Point_Definition |
               N_Formal_Package_Declaration             |
               N_Formal_Private_Type_Definition         |
               N_Formal_Incomplete_Type_Definition      |
               N_Formal_Signed_Integer_Type_Definition  |
               N_Function_Call                          |
               N_Function_Specification                 |
               N_Generic_Association                    |
               N_Handled_Sequence_Of_Statements         |
               N_Identifier                             |
               N_In                                     |
               N_Index_Or_Discriminant_Constraint       |
               N_Indexed_Component                      |
               N_Integer_Literal                        |
               N_Iterator_Specification                 |
               N_Itype_Reference                        |
               N_Label                                  |
               N_Loop_Parameter_Specification           |
               N_Mod_Clause                             |
               N_Modular_Type_Definition                |
               N_Not_In                                 |
               N_Null                                   |
               N_Op_Abs                                 |
               N_Op_Add                                 |
               N_Op_And                                 |
               N_Op_Concat                              |
               N_Op_Divide                              |
               N_Op_Eq                                  |
               N_Op_Expon                               |
               N_Op_Ge                                  |
               N_Op_Gt                                  |
               N_Op_Le                                  |
               N_Op_Lt                                  |
               N_Op_Minus                               |
               N_Op_Mod                                 |
               N_Op_Multiply                            |
               N_Op_Ne                                  |
               N_Op_Not                                 |
               N_Op_Or                                  |
               N_Op_Plus                                |
               N_Op_Rem                                 |
               N_Op_Rotate_Left                         |
               N_Op_Rotate_Right                        |
               N_Op_Shift_Left                          |
               N_Op_Shift_Right                         |
               N_Op_Shift_Right_Arithmetic              |
               N_Op_Subtract                            |
               N_Op_Xor                                 |
               N_Operator_Symbol                        |
               N_Ordinary_Fixed_Point_Definition        |
               N_Others_Choice                          |
               N_Package_Specification                  |
               N_Parameter_Association                  |
               N_Parameter_Specification                |
               N_Pop_Constraint_Error_Label             |
               N_Pop_Program_Error_Label                |
               N_Pop_Storage_Error_Label                |
               N_Pragma_Argument_Association            |
               N_Procedure_Specification                |
               N_Protected_Definition                   |
               N_Push_Constraint_Error_Label            |
               N_Push_Program_Error_Label               |
               N_Push_Storage_Error_Label               |
               N_Qualified_Expression                   |
               N_Quantified_Expression                  |
               N_Raise_Expression                       |
               N_Range                                  |
               N_Range_Constraint                       |
               N_Real_Literal                           |
               N_Real_Range_Specification               |
               N_Record_Definition                      |
               N_Reference                              |
               N_SCIL_Dispatch_Table_Tag_Init           |
               N_SCIL_Dispatching_Call                  |
               N_SCIL_Membership_Test                   |
               N_Selected_Component                     |
               N_Signed_Integer_Type_Definition         |
               N_Single_Protected_Declaration           |
               N_Slice                                  |
               N_String_Literal                         |
               N_Subprogram_Info                        |
               N_Subtype_Indication                     |
               N_Subunit                                |
               N_Task_Definition                        |
               N_Terminate_Alternative                  |
               N_Triggering_Alternative                 |
               N_Type_Conversion                        |
               N_Unchecked_Expression                   |
               N_Unchecked_Type_Conversion              |
               N_Unconstrained_Array_Definition         |
               N_Unused_At_End                          |
               N_Unused_At_Start                        |
               N_Variant                                |
               N_Variant_Part                           |
               N_Validate_Unchecked_Conversion          |
               N_With_Clause
            =>
               null;

         end case;

         --  If we fall through above tests, keep climbing tree

         N := P;

         if Nkind (Parent (N)) = N_Subunit then

            --  This is the proper body corresponding to a stub. Insertion must
            --  be done at the point of the stub, which is in the declarative
            --  part of the parent unit.

            P := Corresponding_Stub (Parent (N));

         else
            P := Parent (N);
         end if;
      end loop;
   end Insert_Actions;

   --  Version with check(s) suppressed

   procedure Insert_Actions
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id;
      Suppress    : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Sva : constant Suppress_Array := Scope_Suppress.Suppress;
         begin
            Scope_Suppress.Suppress := (others => True);
            Insert_Actions (Assoc_Node, Ins_Actions);
            Scope_Suppress.Suppress := Sva;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress.Suppress (Suppress);
         begin
            Scope_Suppress.Suppress (Suppress) := True;
            Insert_Actions (Assoc_Node, Ins_Actions);
            Scope_Suppress.Suppress (Suppress) := Svg;
         end;
      end if;
   end Insert_Actions;

   --------------------------
   -- Insert_Actions_After --
   --------------------------

   procedure Insert_Actions_After
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id)
   is
   begin
      if Scope_Is_Transient and then Assoc_Node = Node_To_Be_Wrapped then
         Store_After_Actions_In_Scope (Ins_Actions);
      else
         Insert_List_After_And_Analyze (Assoc_Node, Ins_Actions);
      end if;
   end Insert_Actions_After;

   ---------------------------------
   -- Insert_Library_Level_Action --
   ---------------------------------

   procedure Insert_Library_Level_Action (N : Node_Id) is
      Aux : constant Node_Id := Aux_Decls_Node (Cunit (Main_Unit));

   begin
      Push_Scope (Cunit_Entity (Main_Unit));
      --  ??? should this be Current_Sem_Unit instead of Main_Unit?

      if No (Actions (Aux)) then
         Set_Actions (Aux, New_List (N));
      else
         Append (N, Actions (Aux));
      end if;

      Analyze (N);
      Pop_Scope;
   end Insert_Library_Level_Action;

   ----------------------------------
   -- Insert_Library_Level_Actions --
   ----------------------------------

   procedure Insert_Library_Level_Actions (L : List_Id) is
      Aux : constant Node_Id := Aux_Decls_Node (Cunit (Main_Unit));

   begin
      if Is_Non_Empty_List (L) then
         Push_Scope (Cunit_Entity (Main_Unit));
         --  ??? should this be Current_Sem_Unit instead of Main_Unit?

         if No (Actions (Aux)) then
            Set_Actions (Aux, L);
            Analyze_List (L);
         else
            Insert_List_After_And_Analyze (Last (Actions (Aux)), L);
         end if;

         Pop_Scope;
      end if;
   end Insert_Library_Level_Actions;

   ----------------------
   -- Inside_Init_Proc --
   ----------------------

   function Inside_Init_Proc return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Is_Init_Proc (S) then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end Inside_Init_Proc;

   ----------------------------
   -- Is_All_Null_Statements --
   ----------------------------

   function Is_All_Null_Statements (L : List_Id) return Boolean is
      Stm : Node_Id;

   begin
      Stm := First (L);
      while Present (Stm) loop
         if Nkind (Stm) /= N_Null_Statement then
            return False;
         end if;

         Next (Stm);
      end loop;

      return True;
   end Is_All_Null_Statements;

   --------------------------------------------------
   -- Is_Displacement_Of_Object_Or_Function_Result --
   --------------------------------------------------

   function Is_Displacement_Of_Object_Or_Function_Result
     (Obj_Id : Entity_Id) return Boolean
   is
      function Is_Controlled_Function_Call (N : Node_Id) return Boolean;
      --  Determine if particular node denotes a controlled function call

      function Is_Displace_Call (N : Node_Id) return Boolean;
      --  Determine whether a particular node is a call to Ada.Tags.Displace.
      --  The call might be nested within other actions such as conversions.

      function Is_Source_Object (N : Node_Id) return Boolean;
      --  Determine whether a particular node denotes a source object

      ---------------------------------
      -- Is_Controlled_Function_Call --
      ---------------------------------

      function Is_Controlled_Function_Call (N : Node_Id) return Boolean is
         Expr : Node_Id := Original_Node (N);

      begin
         if Nkind (Expr) = N_Function_Call then
            Expr := Name (Expr);
         end if;

         --  The function call may appear in object.operation format

         if Nkind (Expr) = N_Selected_Component then
            Expr := Selector_Name (Expr);
         end if;

         return
           Nkind_In (Expr, N_Expanded_Name, N_Identifier)
             and then Ekind (Entity (Expr)) = E_Function
             and then Needs_Finalization (Etype (Entity (Expr)));
      end Is_Controlled_Function_Call;

      ----------------------
      -- Is_Displace_Call --
      ----------------------

      function Is_Displace_Call (N : Node_Id) return Boolean is
         Call : Node_Id := N;

      begin
         --  Strip various actions which may precede a call to Displace

         loop
            if Nkind (Call) = N_Explicit_Dereference then
               Call := Prefix (Call);

            elsif Nkind_In (Call, N_Type_Conversion,
                                  N_Unchecked_Type_Conversion)
            then
               Call := Expression (Call);

            else
               exit;
            end if;
         end loop;

         return
           Present (Call)
             and then Nkind (Call) = N_Function_Call
             and then Is_RTE (Entity (Name (Call)), RE_Displace);
      end Is_Displace_Call;

      ----------------------
      -- Is_Source_Object --
      ----------------------

      function Is_Source_Object (N : Node_Id) return Boolean is
      begin
         return
           Present (N)
             and then Nkind (N) in N_Has_Entity
             and then Is_Object (Entity (N))
             and then Comes_From_Source (N);
      end Is_Source_Object;

      --  Local variables

      Decl      : constant Node_Id   := Parent (Obj_Id);
      Obj_Typ   : constant Entity_Id := Base_Type (Etype (Obj_Id));
      Orig_Decl : constant Node_Id   := Original_Node (Decl);

   --  Start of processing for Is_Displacement_Of_Object_Or_Function_Result

   begin
      --  Case 1:

      --     Obj : CW_Type := Function_Call (...);

      --  rewritten into:

      --     Tmp : ... := Function_Call (...)'reference;
      --     Obj : CW_Type renames (... Ada.Tags.Displace (Tmp));

      --  where the return type of the function and the class-wide type require
      --  dispatch table pointer displacement.

      --  Case 2:

      --     Obj : CW_Type := Src_Obj;

      --  rewritten into:

      --     Obj : CW_Type renames (... Ada.Tags.Displace (Src_Obj));

      --  where the type of the source object and the class-wide type require
      --  dispatch table pointer displacement.

      return
        Nkind (Decl) = N_Object_Renaming_Declaration
          and then Nkind (Orig_Decl) = N_Object_Declaration
          and then Comes_From_Source (Orig_Decl)
          and then Is_Class_Wide_Type (Obj_Typ)
          and then Is_Displace_Call (Renamed_Object (Obj_Id))
          and then
            (Is_Controlled_Function_Call (Expression (Orig_Decl))
              or else Is_Source_Object (Expression (Orig_Decl)));
   end Is_Displacement_Of_Object_Or_Function_Result;

   ------------------------------
   -- Is_Finalizable_Transient --
   ------------------------------

   function Is_Finalizable_Transient
     (Decl     : Node_Id;
      Rel_Node : Node_Id) return Boolean
   is
      Obj_Id  : constant Entity_Id := Defining_Identifier (Decl);
      Obj_Typ : constant Entity_Id := Base_Type (Etype (Obj_Id));
      Desig   : Entity_Id := Obj_Typ;

      function Initialized_By_Access (Trans_Id : Entity_Id) return Boolean;
      --  Determine whether transient object Trans_Id is initialized either
      --  by a function call which returns an access type or simply renames
      --  another pointer.

      function Initialized_By_Aliased_BIP_Func_Call
        (Trans_Id : Entity_Id) return Boolean;
      --  Determine whether transient object Trans_Id is initialized by a
      --  build-in-place function call where the BIPalloc parameter is of
      --  value 1 and BIPaccess is not null. This case creates an aliasing
      --  between the returned value and the value denoted by BIPaccess.

      function Is_Aliased
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean;
      --  Determine whether transient object Trans_Id has been renamed or
      --  aliased through 'reference in the statement list starting from
      --  First_Stmt.

      function Is_Allocated (Trans_Id : Entity_Id) return Boolean;
      --  Determine whether transient object Trans_Id is allocated on the heap

      function Is_Iterated_Container
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean;
      --  Determine whether transient object Trans_Id denotes a container which
      --  is in the process of being iterated in the statement list starting
      --  from First_Stmt.

      ---------------------------
      -- Initialized_By_Access --
      ---------------------------

      function Initialized_By_Access (Trans_Id : Entity_Id) return Boolean is
         Expr : constant Node_Id := Expression (Parent (Trans_Id));

      begin
         return
           Present (Expr)
             and then Nkind (Expr) /= N_Reference
             and then Is_Access_Type (Etype (Expr));
      end Initialized_By_Access;

      ------------------------------------------
      -- Initialized_By_Aliased_BIP_Func_Call --
      ------------------------------------------

      function Initialized_By_Aliased_BIP_Func_Call
        (Trans_Id : Entity_Id) return Boolean
      is
         Call : Node_Id := Expression (Parent (Trans_Id));

      begin
         --  Build-in-place calls usually appear in 'reference format

         if Nkind (Call) = N_Reference then
            Call := Prefix (Call);
         end if;

         if Is_Build_In_Place_Function_Call (Call) then
            declare
               Access_Nam : Name_Id := No_Name;
               Access_OK  : Boolean := False;
               Actual     : Node_Id;
               Alloc_Nam  : Name_Id := No_Name;
               Alloc_OK   : Boolean := False;
               Formal     : Node_Id;
               Func_Id    : Entity_Id;
               Param      : Node_Id;

            begin
               --  Examine all parameter associations of the function call

               Param := First (Parameter_Associations (Call));
               while Present (Param) loop
                  if Nkind (Param) = N_Parameter_Association
                    and then Nkind (Selector_Name (Param)) = N_Identifier
                  then
                     Actual := Explicit_Actual_Parameter (Param);
                     Formal := Selector_Name (Param);

                     --  Construct the names of formals BIPaccess and BIPalloc
                     --  using the function name retrieved from an arbitrary
                     --  formal.

                     if Access_Nam = No_Name
                       and then Alloc_Nam = No_Name
                       and then Present (Entity (Formal))
                     then
                        Func_Id := Scope (Entity (Formal));

                        Access_Nam :=
                          New_External_Name (Chars (Func_Id),
                            BIP_Formal_Suffix (BIP_Object_Access));

                        Alloc_Nam :=
                          New_External_Name (Chars (Func_Id),
                            BIP_Formal_Suffix (BIP_Alloc_Form));
                     end if;

                     --  A match for BIPaccess => Temp has been found

                     if Chars (Formal) = Access_Nam
                       and then Nkind (Actual) /= N_Null
                     then
                        Access_OK := True;
                     end if;

                     --  A match for BIPalloc => 1 has been found

                     if Chars (Formal) = Alloc_Nam
                       and then Nkind (Actual) = N_Integer_Literal
                       and then Intval (Actual) = Uint_1
                     then
                        Alloc_OK := True;
                     end if;
                  end if;

                  Next (Param);
               end loop;

               return Access_OK and Alloc_OK;
            end;
         end if;

         return False;
      end Initialized_By_Aliased_BIP_Func_Call;

      ----------------
      -- Is_Aliased --
      ----------------

      function Is_Aliased
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean
      is
         function Find_Renamed_Object (Ren_Decl : Node_Id) return Entity_Id;
         --  Given an object renaming declaration, retrieve the entity of the
         --  renamed name. Return Empty if the renamed name is anything other
         --  than a variable or a constant.

         -------------------------
         -- Find_Renamed_Object --
         -------------------------

         function Find_Renamed_Object (Ren_Decl : Node_Id) return Entity_Id is
            Ren_Obj : Node_Id := Empty;

            function Find_Object (N : Node_Id) return Traverse_Result;
            --  Try to detect an object which is either a constant or a
            --  variable.

            -----------------
            -- Find_Object --
            -----------------

            function Find_Object (N : Node_Id) return Traverse_Result is
            begin
               --  Stop the search once a constant or a variable has been
               --  detected.

               if Nkind (N) = N_Identifier
                 and then Present (Entity (N))
                 and then Ekind_In (Entity (N), E_Constant, E_Variable)
               then
                  Ren_Obj := Entity (N);
                  return Abandon;
               end if;

               return OK;
            end Find_Object;

            procedure Search is new Traverse_Proc (Find_Object);

            --  Local variables

            Typ : constant Entity_Id := Etype (Defining_Identifier (Ren_Decl));

         --  Start of processing for Find_Renamed_Object

         begin
            --  Actions related to dispatching calls may appear as renamings of
            --  tags. Do not process this type of renaming because it does not
            --  use the actual value of the object.

            if not Is_RTE (Typ, RE_Tag_Ptr) then
               Search (Name (Ren_Decl));
            end if;

            return Ren_Obj;
         end Find_Renamed_Object;

         --  Local variables

         Expr    : Node_Id;
         Ren_Obj : Entity_Id;
         Stmt    : Node_Id;

      --  Start of processing for Is_Aliased

      begin
         Stmt := First_Stmt;
         while Present (Stmt) loop
            if Nkind (Stmt) = N_Object_Declaration then
               Expr := Expression (Stmt);

               if Present (Expr)
                 and then Nkind (Expr) = N_Reference
                 and then Nkind (Prefix (Expr)) = N_Identifier
                 and then Entity (Prefix (Expr)) = Trans_Id
               then
                  return True;
               end if;

            elsif Nkind (Stmt) = N_Object_Renaming_Declaration then
               Ren_Obj := Find_Renamed_Object (Stmt);

               if Present (Ren_Obj) and then Ren_Obj = Trans_Id then
                  return True;
               end if;
            end if;

            Next (Stmt);
         end loop;

         return False;
      end Is_Aliased;

      ------------------
      -- Is_Allocated --
      ------------------

      function Is_Allocated (Trans_Id : Entity_Id) return Boolean is
         Expr : constant Node_Id := Expression (Parent (Trans_Id));
      begin
         return
           Is_Access_Type (Etype (Trans_Id))
             and then Present (Expr)
             and then Nkind (Expr) = N_Allocator;
      end Is_Allocated;

      ---------------------------
      -- Is_Iterated_Container --
      ---------------------------

      function Is_Iterated_Container
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean
      is
         Aspect : Node_Id;
         Call   : Node_Id;
         Iter   : Entity_Id;
         Param  : Node_Id;
         Stmt   : Node_Id;
         Typ    : Entity_Id;

      begin
         --  It is not possible to iterate over containers in non-Ada 2012 code

         if Ada_Version < Ada_2012 then
            return False;
         end if;

         Typ := Etype (Trans_Id);

         --  Handle access type created for secondary stack use

         if Is_Access_Type (Typ) then
            Typ := Designated_Type (Typ);
         end if;

         --  Look for aspect Default_Iterator

         if Has_Aspects (Parent (Typ)) then
            Aspect := Find_Value_Of_Aspect (Typ, Aspect_Default_Iterator);

            if Present (Aspect) then
               Iter := Entity (Aspect);

               --  Examine the statements following the container object and
               --  look for a call to the default iterate routine where the
               --  first parameter is the transient. Such a call appears as:

               --     It : Access_To_CW_Iterator :=
               --            Iterate (Tran_Id.all, ...)'reference;

               Stmt := First_Stmt;
               while Present (Stmt) loop

                  --  Detect an object declaration which is initialized by a
                  --  secondary stack function call.

                  if Nkind (Stmt) = N_Object_Declaration
                    and then Present (Expression (Stmt))
                    and then Nkind (Expression (Stmt)) = N_Reference
                    and then Nkind (Prefix (Expression (Stmt))) =
                               N_Function_Call
                  then
                     Call := Prefix (Expression (Stmt));

                     --  The call must invoke the default iterate routine of
                     --  the container and the transient object must appear as
                     --  the first actual parameter. Skip any calls whose names
                     --  are not entities.

                     if Is_Entity_Name (Name (Call))
                       and then Entity (Name (Call)) = Iter
                       and then Present (Parameter_Associations (Call))
                     then
                        Param := First (Parameter_Associations (Call));

                        if Nkind (Param) = N_Explicit_Dereference
                          and then Entity (Prefix (Param)) = Trans_Id
                        then
                           return True;
                        end if;
                     end if;
                  end if;

                  Next (Stmt);
               end loop;
            end if;
         end if;

         return False;
      end Is_Iterated_Container;

   --  Start of processing for Is_Finalizable_Transient

   begin
      --  Handle access types

      if Is_Access_Type (Desig) then
         Desig := Available_View (Designated_Type (Desig));
      end if;

      return
        Ekind_In (Obj_Id, E_Constant, E_Variable)
          and then Needs_Finalization (Desig)
          and then Requires_Transient_Scope (Desig)
          and then Nkind (Rel_Node) /= N_Simple_Return_Statement

          --  Do not consider renamed or 'reference-d transient objects because
          --  the act of renaming extends the object's lifetime.

          and then not Is_Aliased (Obj_Id, Decl)

          --  Do not consider transient objects allocated on the heap since
          --  they are attached to a finalization master.

          and then not Is_Allocated (Obj_Id)

          --  If the transient object is a pointer, check that it is not
          --  initialized by a function which returns a pointer or acts as a
          --  renaming of another pointer.

          and then
            (not Is_Access_Type (Obj_Typ)
               or else not Initialized_By_Access (Obj_Id))

          --  Do not consider transient objects which act as indirect aliases
          --  of build-in-place function results.

          and then not Initialized_By_Aliased_BIP_Func_Call (Obj_Id)

          --  Do not consider conversions of tags to class-wide types

          and then not Is_Tag_To_Class_Wide_Conversion (Obj_Id)

          --  Do not consider containers in the context of iterator loops. Such
          --  transient objects must exist for as long as the loop is around,
          --  otherwise any operation carried out by the iterator will fail.

          and then not Is_Iterated_Container (Obj_Id, Decl);
   end Is_Finalizable_Transient;

   ---------------------------------
   -- Is_Fully_Repped_Tagged_Type --
   ---------------------------------

   function Is_Fully_Repped_Tagged_Type (T : Entity_Id) return Boolean is
      U    : constant Entity_Id := Underlying_Type (T);
      Comp : Entity_Id;

   begin
      if No (U) or else not Is_Tagged_Type (U) then
         return False;
      elsif Has_Discriminants (U) then
         return False;
      elsif not Has_Specified_Layout (U) then
         return False;
      end if;

      --  Here we have a tagged type, see if it has any unlayed out fields
      --  other than a possible tag and parent fields. If so, we return False.

      Comp := First_Component (U);
      while Present (Comp) loop
         if not Is_Tag (Comp)
           and then Chars (Comp) /= Name_uParent
           and then No (Component_Clause (Comp))
         then
            return False;
         else
            Next_Component (Comp);
         end if;
      end loop;

      --  All components are layed out

      return True;
   end Is_Fully_Repped_Tagged_Type;

   ----------------------------------
   -- Is_Library_Level_Tagged_Type --
   ----------------------------------

   function Is_Library_Level_Tagged_Type (Typ : Entity_Id) return Boolean is
   begin
      return Is_Tagged_Type (Typ) and then Is_Library_Level_Entity (Typ);
   end Is_Library_Level_Tagged_Type;

   --------------------------
   -- Is_Non_BIP_Func_Call --
   --------------------------

   function Is_Non_BIP_Func_Call (Expr : Node_Id) return Boolean is
   begin
      --  The expected call is of the format
      --
      --    Func_Call'reference

      return
        Nkind (Expr) = N_Reference
          and then Nkind (Prefix (Expr)) = N_Function_Call
          and then not Is_Build_In_Place_Function_Call (Prefix (Expr));
   end Is_Non_BIP_Func_Call;

   ----------------------------------
   -- Is_Possibly_Unaligned_Object --
   ----------------------------------

   function Is_Possibly_Unaligned_Object (N : Node_Id) return Boolean is
      T  : constant Entity_Id := Etype (N);

   begin
      --  If renamed object, apply test to underlying object

      if Is_Entity_Name (N)
        and then Is_Object (Entity (N))
        and then Present (Renamed_Object (Entity (N)))
      then
         return Is_Possibly_Unaligned_Object (Renamed_Object (Entity (N)));
      end if;

      --  Tagged and controlled types and aliased types are always aligned, as
      --  are concurrent types.

      if Is_Aliased (T)
        or else Has_Controlled_Component (T)
        or else Is_Concurrent_Type (T)
        or else Is_Tagged_Type (T)
        or else Is_Controlled (T)
      then
         return False;
      end if;

      --  If this is an element of a packed array, may be unaligned

      if Is_Ref_To_Bit_Packed_Array (N) then
         return True;
      end if;

      --  Case of indexed component reference: test whether prefix is unaligned

      if Nkind (N) = N_Indexed_Component then
         return Is_Possibly_Unaligned_Object (Prefix (N));

      --  Case of selected component reference

      elsif Nkind (N) = N_Selected_Component then
         declare
            P : constant Node_Id   := Prefix (N);
            C : constant Entity_Id := Entity (Selector_Name (N));
            M : Nat;
            S : Nat;

         begin
            --  If component reference is for an array with non-static bounds,
            --  then it is always aligned: we can only process unaligned arrays
            --  with static bounds (more precisely compile time known bounds).

            if Is_Array_Type (T)
              and then not Compile_Time_Known_Bounds (T)
            then
               return False;
            end if;

            --  If component is aliased, it is definitely properly aligned

            if Is_Aliased (C) then
               return False;
            end if;

            --  If component is for a type implemented as a scalar, and the
            --  record is packed, and the component is other than the first
            --  component of the record, then the component may be unaligned.

            if Is_Packed (Etype (P))
              and then Represented_As_Scalar (Etype (C))
              and then First_Entity (Scope (C)) /= C
            then
               return True;
            end if;

            --  Compute maximum possible alignment for T

            --  If alignment is known, then that settles things

            if Known_Alignment (T) then
               M := UI_To_Int (Alignment (T));

            --  If alignment is not known, tentatively set max alignment

            else
               M := Ttypes.Maximum_Alignment;

               --  We can reduce this if the Esize is known since the default
               --  alignment will never be more than the smallest power of 2
               --  that does not exceed this Esize value.

               if Known_Esize (T) then
                  S := UI_To_Int (Esize (T));

                  while (M / 2) >= S loop
                     M := M / 2;
                  end loop;
               end if;
            end if;

            --  The following code is historical, it used to be present but it
            --  is too cautious, because the front-end does not know the proper
            --  default alignments for the target. Also, if the alignment is
            --  not known, the front end can't know in any case! If a copy is
            --  needed, the back-end will take care of it. This whole section
            --  including this comment can be removed later ???

            --  If the component reference is for a record that has a specified
            --  alignment, and we either know it is too small, or cannot tell,
            --  then the component may be unaligned.

            --  What is the following commented out code ???

            --  if Known_Alignment (Etype (P))
            --    and then Alignment (Etype (P)) < Ttypes.Maximum_Alignment
            --    and then M > Alignment (Etype (P))
            --  then
            --     return True;
            --  end if;

            --  Case of component clause present which may specify an
            --  unaligned position.

            if Present (Component_Clause (C)) then

               --  Otherwise we can do a test to make sure that the actual
               --  start position in the record, and the length, are both
               --  consistent with the required alignment. If not, we know
               --  that we are unaligned.

               declare
                  Align_In_Bits : constant Nat := M * System_Storage_Unit;
               begin
                  if Component_Bit_Offset (C) mod Align_In_Bits /= 0
                    or else Esize (C) mod Align_In_Bits /= 0
                  then
                     return True;
                  end if;
               end;
            end if;

            --  Otherwise, for a component reference, test prefix

            return Is_Possibly_Unaligned_Object (P);
         end;

      --  If not a component reference, must be aligned

      else
         return False;
      end if;
   end Is_Possibly_Unaligned_Object;

   ---------------------------------
   -- Is_Possibly_Unaligned_Slice --
   ---------------------------------

   function Is_Possibly_Unaligned_Slice (N : Node_Id) return Boolean is
   begin
      --  Go to renamed object

      if Is_Entity_Name (N)
        and then Is_Object (Entity (N))
        and then Present (Renamed_Object (Entity (N)))
      then
         return Is_Possibly_Unaligned_Slice (Renamed_Object (Entity (N)));
      end if;

      --  The reference must be a slice

      if Nkind (N) /= N_Slice then
         return False;
      end if;

      --  Always assume the worst for a nested record component with a
      --  component clause, which gigi/gcc does not appear to handle well.
      --  It is not clear why this special test is needed at all ???

      if Nkind (Prefix (N)) = N_Selected_Component
        and then Nkind (Prefix (Prefix (N))) = N_Selected_Component
        and then
          Present (Component_Clause (Entity (Selector_Name (Prefix (N)))))
      then
         return True;
      end if;

      --  We only need to worry if the target has strict alignment

      if not Target_Strict_Alignment then
         return False;
      end if;

      --  If it is a slice, then look at the array type being sliced

      declare
         Sarr : constant Node_Id := Prefix (N);
         --  Prefix of the slice, i.e. the array being sliced

         Styp : constant Entity_Id := Etype (Prefix (N));
         --  Type of the array being sliced

         Pref : Node_Id;
         Ptyp : Entity_Id;

      begin
         --  The problems arise if the array object that is being sliced
         --  is a component of a record or array, and we cannot guarantee
         --  the alignment of the array within its containing object.

         --  To investigate this, we look at successive prefixes to see
         --  if we have a worrisome indexed or selected component.

         Pref := Sarr;
         loop
            --  Case of array is part of an indexed component reference

            if Nkind (Pref) = N_Indexed_Component then
               Ptyp := Etype (Prefix (Pref));

               --  The only problematic case is when the array is packed, in
               --  which case we really know nothing about the alignment of
               --  individual components.

               if Is_Bit_Packed_Array (Ptyp) then
                  return True;
               end if;

            --  Case of array is part of a selected component reference

            elsif Nkind (Pref) = N_Selected_Component then
               Ptyp := Etype (Prefix (Pref));

               --  We are definitely in trouble if the record in question
               --  has an alignment, and either we know this alignment is
               --  inconsistent with the alignment of the slice, or we don't
               --  know what the alignment of the slice should be.

               if Known_Alignment (Ptyp)
                 and then (Unknown_Alignment (Styp)
                            or else Alignment (Styp) > Alignment (Ptyp))
               then
                  return True;
               end if;

               --  We are in potential trouble if the record type is packed.
               --  We could special case when we know that the array is the
               --  first component, but that's not such a simple case ???

               if Is_Packed (Ptyp) then
                  return True;
               end if;

               --  We are in trouble if there is a component clause, and
               --  either we do not know the alignment of the slice, or
               --  the alignment of the slice is inconsistent with the
               --  bit position specified by the component clause.

               declare
                  Field : constant Entity_Id := Entity (Selector_Name (Pref));
               begin
                  if Present (Component_Clause (Field))
                    and then
                      (Unknown_Alignment (Styp)
                        or else
                         (Component_Bit_Offset (Field) mod
                           (System_Storage_Unit * Alignment (Styp))) /= 0)
                  then
                     return True;
                  end if;
               end;

            --  For cases other than selected or indexed components we know we
            --  are OK, since no issues arise over alignment.

            else
               return False;
            end if;

            --  We processed an indexed component or selected component
            --  reference that looked safe, so keep checking prefixes.

            Pref := Prefix (Pref);
         end loop;
      end;
   end Is_Possibly_Unaligned_Slice;

   -------------------------------
   -- Is_Related_To_Func_Return --
   -------------------------------

   function Is_Related_To_Func_Return (Id : Entity_Id) return Boolean is
      Expr : constant Node_Id := Related_Expression (Id);
   begin
      return
        Present (Expr)
          and then Nkind (Expr) = N_Explicit_Dereference
          and then Nkind (Parent (Expr)) = N_Simple_Return_Statement;
   end Is_Related_To_Func_Return;

   --------------------------------
   -- Is_Ref_To_Bit_Packed_Array --
   --------------------------------

   function Is_Ref_To_Bit_Packed_Array (N : Node_Id) return Boolean is
      Result : Boolean;
      Expr   : Node_Id;

   begin
      if Is_Entity_Name (N)
        and then Is_Object (Entity (N))
        and then Present (Renamed_Object (Entity (N)))
      then
         return Is_Ref_To_Bit_Packed_Array (Renamed_Object (Entity (N)));
      end if;

      if Nkind_In (N, N_Indexed_Component, N_Selected_Component) then
         if Is_Bit_Packed_Array (Etype (Prefix (N))) then
            Result := True;
         else
            Result := Is_Ref_To_Bit_Packed_Array (Prefix (N));
         end if;

         if Result and then Nkind (N) = N_Indexed_Component then
            Expr := First (Expressions (N));
            while Present (Expr) loop
               Force_Evaluation (Expr);
               Next (Expr);
            end loop;
         end if;

         return Result;

      else
         return False;
      end if;
   end Is_Ref_To_Bit_Packed_Array;

   --------------------------------
   -- Is_Ref_To_Bit_Packed_Slice --
   --------------------------------

   function Is_Ref_To_Bit_Packed_Slice (N : Node_Id) return Boolean is
   begin
      if Nkind (N) = N_Type_Conversion then
         return Is_Ref_To_Bit_Packed_Slice (Expression (N));

      elsif Is_Entity_Name (N)
        and then Is_Object (Entity (N))
        and then Present (Renamed_Object (Entity (N)))
      then
         return Is_Ref_To_Bit_Packed_Slice (Renamed_Object (Entity (N)));

      elsif Nkind (N) = N_Slice
        and then Is_Bit_Packed_Array (Etype (Prefix (N)))
      then
         return True;

      elsif Nkind_In (N, N_Indexed_Component, N_Selected_Component) then
         return Is_Ref_To_Bit_Packed_Slice (Prefix (N));

      else
         return False;
      end if;
   end Is_Ref_To_Bit_Packed_Slice;

   -----------------------
   -- Is_Renamed_Object --
   -----------------------

   function Is_Renamed_Object (N : Node_Id) return Boolean is
      Pnod : constant Node_Id   := Parent (N);
      Kind : constant Node_Kind := Nkind (Pnod);
   begin
      if Kind = N_Object_Renaming_Declaration then
         return True;
      elsif Nkind_In (Kind, N_Indexed_Component, N_Selected_Component) then
         return Is_Renamed_Object (Pnod);
      else
         return False;
      end if;
   end Is_Renamed_Object;

   --------------------------------------
   -- Is_Secondary_Stack_BIP_Func_Call --
   --------------------------------------

   function Is_Secondary_Stack_BIP_Func_Call (Expr : Node_Id) return Boolean is
      Call : Node_Id := Expr;

   begin
      --  Build-in-place calls usually appear in 'reference format. Note that
      --  the accessibility check machinery may add an extra 'reference due to
      --  side effect removal.

      while Nkind (Call) = N_Reference loop
         Call := Prefix (Call);
      end loop;

      if Nkind_In (Call, N_Qualified_Expression,
                         N_Unchecked_Type_Conversion)
      then
         Call := Expression (Call);
      end if;

      if Is_Build_In_Place_Function_Call (Call) then
         declare
            Access_Nam : Name_Id := No_Name;
            Actual     : Node_Id;
            Param      : Node_Id;
            Formal     : Node_Id;

         begin
            --  Examine all parameter associations of the function call

            Param := First (Parameter_Associations (Call));
            while Present (Param) loop
               if Nkind (Param) = N_Parameter_Association
                 and then Nkind (Selector_Name (Param)) = N_Identifier
               then
                  Formal := Selector_Name (Param);
                  Actual := Explicit_Actual_Parameter (Param);

                  --  Construct the name of formal BIPalloc. It is much easier
                  --  to extract the name of the function using an arbitrary
                  --  formal's scope rather than the Name field of Call.

                  if Access_Nam = No_Name
                    and then Present (Entity (Formal))
                  then
                     Access_Nam :=
                       New_External_Name
                         (Chars (Scope (Entity (Formal))),
                          BIP_Formal_Suffix (BIP_Alloc_Form));
                  end if;

                  --  A match for BIPalloc => 2 has been found

                  if Chars (Formal) = Access_Nam
                    and then Nkind (Actual) = N_Integer_Literal
                    and then Intval (Actual) = Uint_2
                  then
                     return True;
                  end if;
               end if;

               Next (Param);
            end loop;
         end;
      end if;

      return False;
   end Is_Secondary_Stack_BIP_Func_Call;

   -------------------------------------
   -- Is_Tag_To_Class_Wide_Conversion --
   -------------------------------------

   function Is_Tag_To_Class_Wide_Conversion
     (Obj_Id : Entity_Id) return Boolean
   is
      Expr : constant Node_Id := Expression (Parent (Obj_Id));

   begin
      return
        Is_Class_Wide_Type (Etype (Obj_Id))
          and then Present (Expr)
          and then Nkind (Expr) = N_Unchecked_Type_Conversion
          and then Etype (Expression (Expr)) = RTE (RE_Tag);
   end Is_Tag_To_Class_Wide_Conversion;

   ----------------------------
   -- Is_Untagged_Derivation --
   ----------------------------

   function Is_Untagged_Derivation (T : Entity_Id) return Boolean is
   begin
      return (not Is_Tagged_Type (T) and then Is_Derived_Type (T))
               or else
             (Is_Private_Type (T) and then Present (Full_View (T))
               and then not Is_Tagged_Type (Full_View (T))
               and then Is_Derived_Type (Full_View (T))
               and then Etype (Full_View (T)) /= T);
   end Is_Untagged_Derivation;

   ---------------------------
   -- Is_Volatile_Reference --
   ---------------------------

   function Is_Volatile_Reference (N : Node_Id) return Boolean is
   begin
      if Nkind (N) in N_Has_Etype
        and then Present (Etype (N))
        and then Treat_As_Volatile (Etype (N))
      then
         return True;

      elsif Is_Entity_Name (N) then
         return Treat_As_Volatile (Entity (N));

      elsif Nkind (N) = N_Slice then
         return Is_Volatile_Reference (Prefix (N));

      elsif Nkind_In (N, N_Indexed_Component, N_Selected_Component) then
         if (Is_Entity_Name (Prefix (N))
              and then Has_Volatile_Components (Entity (Prefix (N))))
           or else (Present (Etype (Prefix (N)))
                     and then Has_Volatile_Components (Etype (Prefix (N))))
         then
            return True;
         else
            return Is_Volatile_Reference (Prefix (N));
         end if;

      else
         return False;
      end if;
   end Is_Volatile_Reference;

   --------------------------
   -- Is_VM_By_Copy_Actual --
   --------------------------

   function Is_VM_By_Copy_Actual (N : Node_Id) return Boolean is
   begin
      return VM_Target /= No_VM
        and then (Nkind (N) = N_Slice
                    or else
                      (Nkind (N) = N_Identifier
                        and then Present (Renamed_Object (Entity (N)))
                        and then Nkind (Renamed_Object (Entity (N))) =
                                                                 N_Slice));
   end Is_VM_By_Copy_Actual;

   --------------------
   -- Kill_Dead_Code --
   --------------------

   procedure Kill_Dead_Code (N : Node_Id; Warn : Boolean := False) is
      W : Boolean := Warn;
      --  Set False if warnings suppressed

   begin
      if Present (N) then
         Remove_Warning_Messages (N);

         --  Generate warning if appropriate

         if W then

            --  We suppress the warning if this code is under control of an
            --  if statement, whose condition is a simple identifier, and
            --  either we are in an instance, or warnings off is set for this
            --  identifier. The reason for killing it in the instance case is
            --  that it is common and reasonable for code to be deleted in
            --  instances for various reasons.

            if Nkind (Parent (N)) = N_If_Statement then
               declare
                  C : constant Node_Id := Condition (Parent (N));
               begin
                  if Nkind (C) = N_Identifier
                    and then
                      (In_Instance
                        or else (Present (Entity (C))
                                  and then Has_Warnings_Off (Entity (C))))
                  then
                     W := False;
                  end if;
               end;
            end if;

            --  Generate warning if not suppressed

            if W then
               Error_Msg_F
                 ("?t?this code can never be executed and has been deleted!",
                  N);
            end if;
         end if;

         --  Recurse into block statements and bodies to process declarations
         --  and statements.

         if Nkind (N) = N_Block_Statement
           or else Nkind (N) = N_Subprogram_Body
           or else Nkind (N) = N_Package_Body
         then
            Kill_Dead_Code (Declarations (N), False);
            Kill_Dead_Code (Statements (Handled_Statement_Sequence (N)));

            if Nkind (N) = N_Subprogram_Body then
               Set_Is_Eliminated (Defining_Entity (N));
            end if;

         elsif Nkind (N) = N_Package_Declaration then
            Kill_Dead_Code (Visible_Declarations (Specification (N)));
            Kill_Dead_Code (Private_Declarations (Specification (N)));

            --  ??? After this point, Delete_Tree has been called on all
            --  declarations in Specification (N), so references to entities
            --  therein look suspicious.

            declare
               E : Entity_Id := First_Entity (Defining_Entity (N));
            begin
               while Present (E) loop
                  if Ekind (E) = E_Operator then
                     Set_Is_Eliminated (E);
                  end if;

                  Next_Entity (E);
               end loop;
            end;

         --  Recurse into composite statement to kill individual statements in
         --  particular instantiations.

         elsif Nkind (N) = N_If_Statement then
            Kill_Dead_Code (Then_Statements (N));
            Kill_Dead_Code (Elsif_Parts (N));
            Kill_Dead_Code (Else_Statements (N));

         elsif Nkind (N) = N_Loop_Statement then
            Kill_Dead_Code (Statements (N));

         elsif Nkind (N) = N_Case_Statement then
            declare
               Alt : Node_Id;
            begin
               Alt := First (Alternatives (N));
               while Present (Alt) loop
                  Kill_Dead_Code (Statements (Alt));
                  Next (Alt);
               end loop;
            end;

         elsif Nkind (N) = N_Case_Statement_Alternative then
            Kill_Dead_Code (Statements (N));

         --  Deal with dead instances caused by deleting instantiations

         elsif Nkind (N) in N_Generic_Instantiation then
            Remove_Dead_Instance (N);
         end if;
      end if;
   end Kill_Dead_Code;

   --  Case where argument is a list of nodes to be killed

   procedure Kill_Dead_Code (L : List_Id; Warn : Boolean := False) is
      N : Node_Id;
      W : Boolean;
   begin
      W := Warn;
      if Is_Non_Empty_List (L) then
         N := First (L);
         while Present (N) loop
            Kill_Dead_Code (N, W);
            W := False;
            Next (N);
         end loop;
      end if;
   end Kill_Dead_Code;

   ------------------------
   -- Known_Non_Negative --
   ------------------------

   function Known_Non_Negative (Opnd : Node_Id) return Boolean is
   begin
      if Is_OK_Static_Expression (Opnd) and then Expr_Value (Opnd) >= 0 then
         return True;

      else
         declare
            Lo : constant Node_Id := Type_Low_Bound (Etype (Opnd));
         begin
            return
              Is_OK_Static_Expression (Lo) and then Expr_Value (Lo) >= 0;
         end;
      end if;
   end Known_Non_Negative;

   --------------------
   -- Known_Non_Null --
   --------------------

   function Known_Non_Null (N : Node_Id) return Boolean is
   begin
      --  Checks for case where N is an entity reference

      if Is_Entity_Name (N) and then Present (Entity (N)) then
         declare
            E   : constant Entity_Id := Entity (N);
            Op  : Node_Kind;
            Val : Node_Id;

         begin
            --  First check if we are in decisive conditional

            Get_Current_Value_Condition (N, Op, Val);

            if Known_Null (Val) then
               if Op = N_Op_Eq then
                  return False;
               elsif Op = N_Op_Ne then
                  return True;
               end if;
            end if;

            --  If OK to do replacement, test Is_Known_Non_Null flag

            if OK_To_Do_Constant_Replacement (E) then
               return Is_Known_Non_Null (E);

            --  Otherwise if not safe to do replacement, then say so

            else
               return False;
            end if;
         end;

      --  True if access attribute

      elsif Nkind (N) = N_Attribute_Reference
        and then Nam_In (Attribute_Name (N), Name_Access,
                                             Name_Unchecked_Access,
                                             Name_Unrestricted_Access)
      then
         return True;

      --  True if allocator

      elsif Nkind (N) = N_Allocator then
         return True;

      --  For a conversion, true if expression is known non-null

      elsif Nkind (N) = N_Type_Conversion then
         return Known_Non_Null (Expression (N));

      --  Above are all cases where the value could be determined to be
      --  non-null. In all other cases, we don't know, so return False.

      else
         return False;
      end if;
   end Known_Non_Null;

   ----------------
   -- Known_Null --
   ----------------

   function Known_Null (N : Node_Id) return Boolean is
   begin
      --  Checks for case where N is an entity reference

      if Is_Entity_Name (N) and then Present (Entity (N)) then
         declare
            E   : constant Entity_Id := Entity (N);
            Op  : Node_Kind;
            Val : Node_Id;

         begin
            --  Constant null value is for sure null

            if Ekind (E) = E_Constant
              and then Known_Null (Constant_Value (E))
            then
               return True;
            end if;

            --  First check if we are in decisive conditional

            Get_Current_Value_Condition (N, Op, Val);

            if Known_Null (Val) then
               if Op = N_Op_Eq then
                  return True;
               elsif Op = N_Op_Ne then
                  return False;
               end if;
            end if;

            --  If OK to do replacement, test Is_Known_Null flag

            if OK_To_Do_Constant_Replacement (E) then
               return Is_Known_Null (E);

            --  Otherwise if not safe to do replacement, then say so

            else
               return False;
            end if;
         end;

      --  True if explicit reference to null

      elsif Nkind (N) = N_Null then
         return True;

      --  For a conversion, true if expression is known null

      elsif Nkind (N) = N_Type_Conversion then
         return Known_Null (Expression (N));

      --  Above are all cases where the value could be determined to be null.
      --  In all other cases, we don't know, so return False.

      else
         return False;
      end if;
   end Known_Null;

   -----------------------------
   -- Make_CW_Equivalent_Type --
   -----------------------------

   --  Create a record type used as an equivalent of any member of the class
   --  which takes its size from exp.

   --  Generate the following code:

   --   type Equiv_T is record
   --     _parent :  T (List of discriminant constraints taken from Exp);
   --     Ext__50 : Storage_Array (1 .. (Exp'size - Typ'object_size)/8);
   --   end Equiv_T;
   --
   --   ??? Note that this type does not guarantee same alignment as all
   --   derived types

   function Make_CW_Equivalent_Type
     (T : Entity_Id;
      E : Node_Id) return Entity_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      Root_Typ    : constant Entity_Id  := Root_Type (T);
      List_Def    : constant List_Id    := Empty_List;
      Comp_List   : constant List_Id    := New_List;
      Equiv_Type  : Entity_Id;
      Range_Type  : Entity_Id;
      Str_Type    : Entity_Id;
      Constr_Root : Entity_Id;
      Sizexpr     : Node_Id;

   begin
      --  If the root type is already constrained, there are no discriminants
      --  in the expression.

      if not Has_Discriminants (Root_Typ)
        or else Is_Constrained (Root_Typ)
      then
         Constr_Root := Root_Typ;
      else
         Constr_Root := Make_Temporary (Loc, 'R');

         --  subtype cstr__n is T (List of discr constraints taken from Exp)

         Append_To (List_Def,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Constr_Root,
             Subtype_Indication  => Make_Subtype_From_Expr (E, Root_Typ)));
      end if;

      --  Generate the range subtype declaration

      Range_Type := Make_Temporary (Loc, 'G');

      if not Is_Interface (Root_Typ) then

         --  subtype rg__xx is
         --    Storage_Offset range 1 .. (Expr'size - typ'size) / Storage_Unit

         Sizexpr :=
           Make_Op_Subtract (Loc,
             Left_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   OK_Convert_To (T, Duplicate_Subexpr_No_Checks (E)),
                 Attribute_Name => Name_Size),
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Constr_Root, Loc),
                 Attribute_Name => Name_Object_Size));
      else
         --  subtype rg__xx is
         --    Storage_Offset range 1 .. Expr'size / Storage_Unit

         Sizexpr :=
           Make_Attribute_Reference (Loc,
             Prefix =>
               OK_Convert_To (T, Duplicate_Subexpr_No_Checks (E)),
             Attribute_Name => Name_Size);
      end if;

      Set_Paren_Count (Sizexpr, 1);

      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Range_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Offset), Loc),
              Constraint => Make_Range_Constraint (Loc,
                Range_Expression =>
                  Make_Range (Loc,
                    Low_Bound => Make_Integer_Literal (Loc, 1),
                    High_Bound =>
                      Make_Op_Divide (Loc,
                        Left_Opnd => Sizexpr,
                        Right_Opnd => Make_Integer_Literal (Loc,
                            Intval => System_Storage_Unit)))))));

      --  subtype str__nn is Storage_Array (rg__x);

      Str_Type := Make_Temporary (Loc, 'S');
      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Str_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints =>
                    New_List (New_Reference_To (Range_Type, Loc))))));

      --  type Equiv_T is record
      --    [ _parent : Tnn; ]
      --    E : Str_Type;
      --  end Equiv_T;

      Equiv_Type := Make_Temporary (Loc, 'T');
      Set_Ekind (Equiv_Type, E_Record_Type);
      Set_Parent_Subtype (Equiv_Type, Constr_Root);

      --  Set Is_Class_Wide_Equivalent_Type very early to trigger the special
      --  treatment for this type. In particular, even though _parent's type
      --  is a controlled type or contains controlled components, we do not
      --  want to set Has_Controlled_Component on it to avoid making it gain
      --  an unwanted _controller component.

      Set_Is_Class_Wide_Equivalent_Type (Equiv_Type);

      if not Is_Interface (Root_Typ) then
         Append_To (Comp_List,
           Make_Component_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uParent),
             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => False,
                 Subtype_Indication => New_Reference_To (Constr_Root, Loc))));
      end if;

      Append_To (Comp_List,
        Make_Component_Declaration (Loc,
          Defining_Identifier  => Make_Temporary (Loc, 'C'),
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present    => False,
              Subtype_Indication => New_Reference_To (Str_Type, Loc))));

      Append_To (List_Def,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Equiv_Type,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Items => Comp_List,
                  Variant_Part    => Empty))));

      --  Suppress all checks during the analysis of the expanded code to avoid
      --  the generation of spurious warnings under ZFP run-time.

      Insert_Actions (E, List_Def, Suppress => All_Checks);
      return Equiv_Type;
   end Make_CW_Equivalent_Type;

   -------------------------
   -- Make_Invariant_Call --
   -------------------------

   function Make_Invariant_Call (Expr : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (Expr);
      Typ : Entity_Id;

   begin
      Typ := Etype (Expr);

      --  Subtypes may be subject to invariants coming from their respective
      --  base types.

      if Ekind_In (Typ, E_Array_Subtype,
                        E_Private_Subtype,
                        E_Record_Subtype)
      then
         Typ := Base_Type (Typ);
      end if;

      pragma Assert
        (Has_Invariants (Typ) and then Present (Invariant_Procedure (Typ)));

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (Invariant_Procedure (Typ), Loc),
          Parameter_Associations => New_List (Relocate_Node (Expr)));
   end Make_Invariant_Call;

   ------------------------
   -- Make_Literal_Range --
   ------------------------

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id) return Node_Id
   is
      Lo          : constant Node_Id :=
                      New_Copy_Tree (String_Literal_Low_Bound (Literal_Typ));
      Index       : constant Entity_Id := Etype (Lo);

      Hi          : Node_Id;
      Length_Expr : constant Node_Id :=
                      Make_Op_Subtract (Loc,
                        Left_Opnd =>
                          Make_Integer_Literal (Loc,
                            Intval => String_Literal_Length (Literal_Typ)),
                        Right_Opnd =>
                          Make_Integer_Literal (Loc, 1));

   begin
      Set_Analyzed (Lo, False);

         if Is_Integer_Type (Index) then
            Hi :=
              Make_Op_Add (Loc,
                Left_Opnd  => New_Copy_Tree (Lo),
                Right_Opnd => Length_Expr);
         else
            Hi :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Val,
                Prefix => New_Occurrence_Of (Index, Loc),
                Expressions => New_List (
                 Make_Op_Add (Loc,
                   Left_Opnd =>
                     Make_Attribute_Reference (Loc,
                       Attribute_Name => Name_Pos,
                       Prefix => New_Occurrence_Of (Index, Loc),
                       Expressions => New_List (New_Copy_Tree (Lo))),
                  Right_Opnd => Length_Expr)));
         end if;

         return
           Make_Range (Loc,
             Low_Bound  => Lo,
             High_Bound => Hi);
   end Make_Literal_Range;

   --------------------------
   -- Make_Non_Empty_Check --
   --------------------------

   function Make_Non_Empty_Check
     (Loc : Source_Ptr;
      N   : Node_Id) return Node_Id
   is
   begin
      return
        Make_Op_Ne (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Attribute_Name => Name_Length,
              Prefix => Duplicate_Subexpr_No_Checks (N, Name_Req => True)),
          Right_Opnd =>
            Make_Integer_Literal (Loc, 0));
   end Make_Non_Empty_Check;

   -------------------------
   -- Make_Predicate_Call --
   -------------------------

   function Make_Predicate_Call
     (Typ  : Entity_Id;
      Expr : Node_Id;
      Mem  : Boolean := False) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Expr);

   begin
      pragma Assert (Present (Predicate_Function (Typ)));

      --  Call special membership version if requested and available

      if Mem then
         declare
            PFM : constant Entity_Id := Predicate_Function_M (Typ);
         begin
            if Present (PFM) then
               return
                 Make_Function_Call (Loc,
                   Name                   => New_Occurrence_Of (PFM, Loc),
                   Parameter_Associations => New_List (Relocate_Node (Expr)));
            end if;
         end;
      end if;

      --  Case of calling normal predicate function

      return
          Make_Function_Call (Loc,
            Name                   =>
              New_Occurrence_Of (Predicate_Function (Typ), Loc),
            Parameter_Associations => New_List (Relocate_Node (Expr)));
   end Make_Predicate_Call;

   --------------------------
   -- Make_Predicate_Check --
   --------------------------

   function Make_Predicate_Check
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Expr);
      Nam : Name_Id;

   begin
      --  If predicate checks are suppressed, then return a null statement.
      --  For this call, we check only the scope setting. If the caller wants
      --  to check a specific entity's setting, they must do it manually.

      if Predicate_Checks_Suppressed (Empty) then
         return Make_Null_Statement (Loc);
      end if;

      --  Compute proper name to use, we need to get this right so that the
      --  right set of check policies apply to the Check pragma we are making.

      if Has_Dynamic_Predicate_Aspect (Typ) then
         Nam := Name_Dynamic_Predicate;
      elsif Has_Static_Predicate_Aspect (Typ) then
         Nam := Name_Static_Predicate;
      else
         Nam := Name_Predicate;
      end if;

      return
        Make_Pragma (Loc,
          Pragma_Identifier            => Make_Identifier (Loc, Name_Check),
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Nam)),
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Predicate_Call (Typ, Expr))));
   end Make_Predicate_Check;

   ----------------------------
   -- Make_Subtype_From_Expr --
   ----------------------------

   --  1. If Expr is an unconstrained array expression, creates
   --    Unc_Type(Expr'first(1)..Expr'last(1),..., Expr'first(n)..Expr'last(n))

   --  2. If Expr is a unconstrained discriminated type expression, creates
   --    Unc_Type(Expr.Discr1, ... , Expr.Discr_n)

   --  3. If Expr is class-wide, creates an implicit class wide subtype

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id) return Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      List_Constr : constant List_Id    := New_List;
      D           : Entity_Id;

      Full_Subtyp  : Entity_Id;
      Priv_Subtyp  : Entity_Id;
      Utyp         : Entity_Id;
      Full_Exp     : Node_Id;

   begin
      if Is_Private_Type (Unc_Typ)
        and then Has_Unknown_Discriminants (Unc_Typ)
      then
         --  Prepare the subtype completion, Go to base type to
         --  find underlying type, because the type may be a generic
         --  actual or an explicit subtype.

         Utyp        := Underlying_Type (Base_Type (Unc_Typ));
         Full_Subtyp := Make_Temporary (Loc, 'C');
         Full_Exp    :=
           Unchecked_Convert_To (Utyp, Duplicate_Subexpr_No_Checks (E));
         Set_Parent (Full_Exp, Parent (E));

         Priv_Subtyp := Make_Temporary (Loc, 'P');

         Insert_Action (E,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Full_Subtyp,
             Subtype_Indication  => Make_Subtype_From_Expr (Full_Exp, Utyp)));

         --  Define the dummy private subtype

         Set_Ekind          (Priv_Subtyp, Subtype_Kind (Ekind (Unc_Typ)));
         Set_Etype          (Priv_Subtyp, Base_Type (Unc_Typ));
         Set_Scope          (Priv_Subtyp, Full_Subtyp);
         Set_Is_Constrained (Priv_Subtyp);
         Set_Is_Tagged_Type (Priv_Subtyp, Is_Tagged_Type (Unc_Typ));
         Set_Is_Itype       (Priv_Subtyp);
         Set_Associated_Node_For_Itype (Priv_Subtyp, E);

         if Is_Tagged_Type  (Priv_Subtyp) then
            Set_Class_Wide_Type
              (Base_Type (Priv_Subtyp), Class_Wide_Type (Unc_Typ));
            Set_Direct_Primitive_Operations (Priv_Subtyp,
              Direct_Primitive_Operations (Unc_Typ));
         end if;

         Set_Full_View (Priv_Subtyp, Full_Subtyp);

         return New_Reference_To (Priv_Subtyp, Loc);

      elsif Is_Array_Type (Unc_Typ) then
         for J in 1 .. Number_Dimensions (Unc_Typ) loop
            Append_To (List_Constr,
              Make_Range (Loc,
                Low_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Duplicate_Subexpr_No_Checks (E),
                    Attribute_Name => Name_First,
                    Expressions => New_List (
                      Make_Integer_Literal (Loc, J))),

                High_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Duplicate_Subexpr_No_Checks (E),
                    Attribute_Name => Name_Last,
                    Expressions    => New_List (
                      Make_Integer_Literal (Loc, J)))));
         end loop;

      elsif Is_Class_Wide_Type (Unc_Typ) then
         declare
            CW_Subtype : Entity_Id;
            EQ_Typ     : Entity_Id := Empty;

         begin
            --  A class-wide equivalent type is not needed when VM_Target
            --  because the VM back-ends handle the class-wide object
            --  initialization itself (and doesn't need or want the
            --  additional intermediate type to handle the assignment).

            if Expander_Active and then Tagged_Type_Expansion then

               --  If this is the class_wide type of a completion that is a
               --  record subtype, set the type of the class_wide type to be
               --  the full base type, for use in the expanded code for the
               --  equivalent type. Should this be done earlier when the
               --  completion is analyzed ???

               if Is_Private_Type (Etype (Unc_Typ))
                 and then
                   Ekind (Full_View (Etype (Unc_Typ))) = E_Record_Subtype
               then
                  Set_Etype (Unc_Typ, Base_Type (Full_View (Etype (Unc_Typ))));
               end if;

               EQ_Typ := Make_CW_Equivalent_Type (Unc_Typ, E);
            end if;

            CW_Subtype := New_Class_Wide_Subtype (Unc_Typ, E);
            Set_Equivalent_Type (CW_Subtype, EQ_Typ);
            Set_Cloned_Subtype (CW_Subtype, Base_Type (Unc_Typ));

            return New_Occurrence_Of (CW_Subtype, Loc);
         end;

      --  Indefinite record type with discriminants

      else
         D := First_Discriminant (Unc_Typ);
         while Present (D) loop
            Append_To (List_Constr,
              Make_Selected_Component (Loc,
                Prefix        => Duplicate_Subexpr_No_Checks (E),
                Selector_Name => New_Reference_To (D, Loc)));

            Next_Discriminant (D);
         end loop;
      end if;

      return
        Make_Subtype_Indication (Loc,
          Subtype_Mark => New_Reference_To (Unc_Typ, Loc),
          Constraint   =>
            Make_Index_Or_Discriminant_Constraint (Loc,
              Constraints => List_Constr));
   end Make_Subtype_From_Expr;

   -----------------------------
   -- May_Generate_Large_Temp --
   -----------------------------

   --  At the current time, the only types that we return False for (i.e. where
   --  we decide we know they cannot generate large temps) are ones where we
   --  know the size is 256 bits or less at compile time, and we are still not
   --  doing a thorough job on arrays and records ???

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean is
   begin
      if not Size_Known_At_Compile_Time (Typ) then
         return False;

      elsif Esize (Typ) /= 0 and then Esize (Typ) <= 256 then
         return False;

      elsif Is_Array_Type (Typ) and then Present (Packed_Array_Type (Typ)) then
         return May_Generate_Large_Temp (Packed_Array_Type (Typ));

      --  We could do more here to find other small types ???

      else
         return True;
      end if;
   end May_Generate_Large_Temp;

   ------------------------
   -- Needs_Finalization --
   ------------------------

   function Needs_Finalization (T : Entity_Id) return Boolean is
      function Has_Some_Controlled_Component (Rec : Entity_Id) return Boolean;
      --  If type is not frozen yet, check explicitly among its components,
      --  because the Has_Controlled_Component flag is not necessarily set.

      -----------------------------------
      -- Has_Some_Controlled_Component --
      -----------------------------------

      function Has_Some_Controlled_Component
        (Rec : Entity_Id) return Boolean
      is
         Comp : Entity_Id;

      begin
         if Has_Controlled_Component (Rec) then
            return True;

         elsif not Is_Frozen (Rec) then
            if Is_Record_Type (Rec) then
               Comp := First_Entity (Rec);

               while Present (Comp) loop
                  if not Is_Type (Comp)
                    and then Needs_Finalization (Etype (Comp))
                  then
                     return True;
                  end if;

                  Next_Entity (Comp);
               end loop;

               return False;

            elsif Is_Array_Type (Rec) then
               return Needs_Finalization (Component_Type (Rec));

            else
               return Has_Controlled_Component (Rec);
            end if;
         else
            return False;
         end if;
      end Has_Some_Controlled_Component;

   --  Start of processing for Needs_Finalization

   begin
      --  Certain run-time configurations and targets do not provide support
      --  for controlled types.

      if Restriction_Active (No_Finalization) then
         return False;

      --  C, C++, CIL and Java types are not considered controlled. It is
      --  assumed that the non-Ada side will handle their clean up.

      elsif Convention (T) = Convention_C
        or else Convention (T) = Convention_CIL
        or else Convention (T) = Convention_CPP
        or else Convention (T) = Convention_Java
      then
         return False;

      else
         --  Class-wide types are treated as controlled because derivations
         --  from the root type can introduce controlled components.

         return
           Is_Class_Wide_Type (T)
             or else Is_Controlled (T)
             or else Has_Controlled_Component (T)
             or else Has_Some_Controlled_Component (T)
             or else
               (Is_Concurrent_Type (T)
                 and then Present (Corresponding_Record_Type (T))
                 and then Needs_Finalization (Corresponding_Record_Type (T)));
      end if;
   end Needs_Finalization;

   ----------------------------
   -- Needs_Constant_Address --
   ----------------------------

   function Needs_Constant_Address
     (Decl : Node_Id;
      Typ  : Entity_Id) return Boolean
   is
   begin

      --  If we have no initialization of any kind, then we don't need to place
      --  any restrictions on the address clause, because the object will be
      --  elaborated after the address clause is evaluated. This happens if the
      --  declaration has no initial expression, or the type has no implicit
      --  initialization, or the object is imported.

      --  The same holds for all initialized scalar types and all access types.
      --  Packed bit arrays of size up to 64 are represented using a modular
      --  type with an initialization (to zero) and can be processed like other
      --  initialized scalar types.

      --  If the type is controlled, code to attach the object to a
      --  finalization chain is generated at the point of declaration, and
      --  therefore the elaboration of the object cannot be delayed: the
      --  address expression must be a constant.

      if No (Expression (Decl))
        and then not Needs_Finalization (Typ)
        and then
          (not Has_Non_Null_Base_Init_Proc (Typ)
            or else Is_Imported (Defining_Identifier (Decl)))
      then
         return False;

      elsif (Present (Expression (Decl)) and then Is_Scalar_Type (Typ))
        or else Is_Access_Type (Typ)
        or else
          (Is_Bit_Packed_Array (Typ)
            and then Is_Modular_Integer_Type (Packed_Array_Type (Typ)))
      then
         return False;

      else

         --  Otherwise, we require the address clause to be constant because
         --  the call to the initialization procedure (or the attach code) has
         --  to happen at the point of the declaration.

         --  Actually the IP call has been moved to the freeze actions anyway,
         --  so maybe we can relax this restriction???

         return True;
      end if;
   end Needs_Constant_Address;

   ----------------------------
   -- New_Class_Wide_Subtype --
   ----------------------------

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id) return Entity_Id
   is
      Res       : constant Entity_Id := Create_Itype (E_Void, N);
      Res_Name  : constant Name_Id   := Chars (Res);
      Res_Scope : constant Entity_Id := Scope (Res);

   begin
      Copy_Node (CW_Typ, Res);
      Set_Comes_From_Source (Res, False);
      Set_Sloc (Res, Sloc (N));
      Set_Is_Itype (Res);
      Set_Associated_Node_For_Itype (Res, N);
      Set_Is_Public (Res, False);   --  By default, may be changed below.
      Set_Public_Status (Res);
      Set_Chars (Res, Res_Name);
      Set_Scope (Res, Res_Scope);
      Set_Ekind (Res, E_Class_Wide_Subtype);
      Set_Next_Entity (Res, Empty);
      Set_Etype (Res, Base_Type (CW_Typ));
      Set_Is_Frozen (Res, False);
      Set_Freeze_Node (Res, Empty);
      return (Res);
   end New_Class_Wide_Subtype;

   --------------------------------
   -- Non_Limited_Designated_Type --
   ---------------------------------

   function Non_Limited_Designated_Type (T : Entity_Id) return Entity_Id is
      Desig : constant Entity_Id := Designated_Type (T);
   begin
      if Ekind (Desig) = E_Incomplete_Type
        and then Present (Non_Limited_View (Desig))
      then
         return Non_Limited_View (Desig);
      else
         return Desig;
      end if;
   end Non_Limited_Designated_Type;

   -----------------------------------
   -- OK_To_Do_Constant_Replacement --
   -----------------------------------

   function OK_To_Do_Constant_Replacement (E : Entity_Id) return Boolean is
      ES : constant Entity_Id := Scope (E);
      CS : Entity_Id;

   begin
      --  Do not replace statically allocated objects, because they may be
      --  modified outside the current scope.

      if Is_Statically_Allocated (E) then
         return False;

      --  Do not replace aliased or volatile objects, since we don't know what
      --  else might change the value.

      elsif Is_Aliased (E) or else Treat_As_Volatile (E) then
         return False;

      --  Debug flag -gnatdM disconnects this optimization

      elsif Debug_Flag_MM then
         return False;

      --  Otherwise check scopes

      else
         CS := Current_Scope;

         loop
            --  If we are in right scope, replacement is safe

            if CS = ES then
               return True;

            --  Packages do not affect the determination of safety

            elsif Ekind (CS) = E_Package then
               exit when CS = Standard_Standard;
               CS := Scope (CS);

            --  Blocks do not affect the determination of safety

            elsif Ekind (CS) = E_Block then
               CS := Scope (CS);

            --  Loops do not affect the determination of safety. Note that we
            --  kill all current values on entry to a loop, so we are just
            --  talking about processing within a loop here.

            elsif Ekind (CS) = E_Loop then
               CS := Scope (CS);

            --  Otherwise, the reference is dubious, and we cannot be sure that
            --  it is safe to do the replacement.

            else
               exit;
            end if;
         end loop;

         return False;
      end if;
   end OK_To_Do_Constant_Replacement;

   ------------------------------------
   -- Possible_Bit_Aligned_Component --
   ------------------------------------

   function Possible_Bit_Aligned_Component (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is

         --  Case of indexed component

         when N_Indexed_Component =>
            declare
               P    : constant Node_Id   := Prefix (N);
               Ptyp : constant Entity_Id := Etype (P);

            begin
               --  If we know the component size and it is less than 64, then
               --  we are definitely OK. The back end always does assignment of
               --  misaligned small objects correctly.

               if Known_Static_Component_Size (Ptyp)
                 and then Component_Size (Ptyp) <= 64
               then
                  return False;

               --  Otherwise, we need to test the prefix, to see if we are
               --  indexing from a possibly unaligned component.

               else
                  return Possible_Bit_Aligned_Component (P);
               end if;
            end;

         --  Case of selected component

         when N_Selected_Component =>
            declare
               P    : constant Node_Id   := Prefix (N);
               Comp : constant Entity_Id := Entity (Selector_Name (N));

            begin
               --  If there is no component clause, then we are in the clear
               --  since the back end will never misalign a large component
               --  unless it is forced to do so. In the clear means we need
               --  only the recursive test on the prefix.

               if Component_May_Be_Bit_Aligned (Comp) then
                  return True;
               else
                  return Possible_Bit_Aligned_Component (P);
               end if;
            end;

         --  For a slice, test the prefix, if that is possibly misaligned,
         --  then for sure the slice is!

         when N_Slice =>
            return Possible_Bit_Aligned_Component (Prefix (N));

         --  For an unchecked conversion, check whether the expression may
         --  be bit-aligned.

         when N_Unchecked_Type_Conversion =>
            return Possible_Bit_Aligned_Component (Expression (N));

         --  If we have none of the above, it means that we have fallen off the
         --  top testing prefixes recursively, and we now have a stand alone
         --  object, where we don't have a problem.

         when others =>
            return False;

      end case;
   end Possible_Bit_Aligned_Component;

   -----------------------------------------------
   -- Process_Statements_For_Controlled_Objects --
   -----------------------------------------------

   procedure Process_Statements_For_Controlled_Objects (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      function Are_Wrapped (L : List_Id) return Boolean;
      --  Determine whether list L contains only one statement which is a block

      function Wrap_Statements_In_Block (L : List_Id) return Node_Id;
      --  Given a list of statements L, wrap it in a block statement and return
      --  the generated node.

      -----------------
      -- Are_Wrapped --
      -----------------

      function Are_Wrapped (L : List_Id) return Boolean is
         Stmt : constant Node_Id := First (L);
      begin
         return
           Present (Stmt)
             and then No (Next (Stmt))
             and then Nkind (Stmt) = N_Block_Statement;
      end Are_Wrapped;

      ------------------------------
      -- Wrap_Statements_In_Block --
      ------------------------------

      function Wrap_Statements_In_Block (L : List_Id) return Node_Id is
      begin
         return
           Make_Block_Statement (Loc,
             Declarations => No_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => L));
      end Wrap_Statements_In_Block;

      --  Local variables

      Block : Node_Id;

   --  Start of processing for Process_Statements_For_Controlled_Objects

   begin
      --  Whenever a non-handled statement list is wrapped in a block, the
      --  block must be explicitly analyzed to redecorate all entities in the
      --  list and ensure that a finalizer is properly built.

      case Nkind (N) is
         when N_Elsif_Part             |
              N_If_Statement           |
              N_Conditional_Entry_Call |
              N_Selective_Accept       =>

            --  Check the "then statements" for elsif parts and if statements

            if Nkind_In (N, N_Elsif_Part, N_If_Statement)
              and then not Is_Empty_List (Then_Statements (N))
              and then not Are_Wrapped (Then_Statements (N))
              and then Requires_Cleanup_Actions
                         (Then_Statements (N), False, False)
            then
               Block := Wrap_Statements_In_Block (Then_Statements (N));
               Set_Then_Statements (N, New_List (Block));

               Analyze (Block);
            end if;

            --  Check the "else statements" for conditional entry calls, if
            --  statements and selective accepts.

            if Nkind_In (N, N_Conditional_Entry_Call,
                            N_If_Statement,
                            N_Selective_Accept)
              and then not Is_Empty_List (Else_Statements (N))
              and then not Are_Wrapped (Else_Statements (N))
              and then Requires_Cleanup_Actions
                         (Else_Statements (N), False, False)
            then
               Block := Wrap_Statements_In_Block (Else_Statements (N));
               Set_Else_Statements (N, New_List (Block));

               Analyze (Block);
            end if;

         when N_Abortable_Part             |
              N_Accept_Alternative         |
              N_Case_Statement_Alternative |
              N_Delay_Alternative          |
              N_Entry_Call_Alternative     |
              N_Exception_Handler          |
              N_Loop_Statement             |
              N_Triggering_Alternative     =>

            if not Is_Empty_List (Statements (N))
              and then not Are_Wrapped (Statements (N))
              and then Requires_Cleanup_Actions (Statements (N), False, False)
            then
               Block := Wrap_Statements_In_Block (Statements (N));
               Set_Statements (N, New_List (Block));

               Analyze (Block);
            end if;

         when others                       =>
            null;
      end case;
   end Process_Statements_For_Controlled_Objects;

   ----------------------
   -- Remove_Init_Call --
   ----------------------

   function Remove_Init_Call
     (Var        : Entity_Id;
      Rep_Clause : Node_Id) return Node_Id
   is
      Par : constant Node_Id   := Parent (Var);
      Typ : constant Entity_Id := Etype (Var);

      Init_Proc : Entity_Id;
      --  Initialization procedure for Typ

      function Find_Init_Call_In_List (From : Node_Id) return Node_Id;
      --  Look for init call for Var starting at From and scanning the
      --  enclosing list until Rep_Clause or the end of the list is reached.

      ----------------------------
      -- Find_Init_Call_In_List --
      ----------------------------

      function Find_Init_Call_In_List (From : Node_Id) return Node_Id is
         Init_Call : Node_Id;

      begin
         Init_Call := From;
         while Present (Init_Call) and then Init_Call /= Rep_Clause loop
            if Nkind (Init_Call) = N_Procedure_Call_Statement
              and then Is_Entity_Name (Name (Init_Call))
              and then Entity (Name (Init_Call)) = Init_Proc
            then
               return Init_Call;
            end if;

            Next (Init_Call);
         end loop;

         return Empty;
      end Find_Init_Call_In_List;

      Init_Call : Node_Id;

   --  Start of processing for Find_Init_Call

   begin
      if Present (Initialization_Statements (Var)) then
         Init_Call := Initialization_Statements (Var);
         Set_Initialization_Statements (Var, Empty);

      elsif not Has_Non_Null_Base_Init_Proc (Typ) then

         --  No init proc for the type, so obviously no call to be found

         return Empty;

      else
         --  We might be able to handle other cases below by just properly
         --  setting Initialization_Statements at the point where the init proc
         --  call is generated???

         Init_Proc := Base_Init_Proc (Typ);

         --  First scan the list containing the declaration of Var

         Init_Call := Find_Init_Call_In_List (From => Next (Par));

         --  If not found, also look on Var's freeze actions list, if any,
         --  since the init call may have been moved there (case of an address
         --  clause applying to Var).

         if No (Init_Call) and then Present (Freeze_Node (Var)) then
            Init_Call :=
              Find_Init_Call_In_List (First (Actions (Freeze_Node (Var))));
         end if;

         --  If the initialization call has actuals that use the secondary
         --  stack, the call may have been wrapped into a temporary block, in
         --  which case the block itself has to be removed.

         if No (Init_Call) and then Nkind (Next (Par)) = N_Block_Statement then
            declare
               Blk : constant Node_Id := Next (Par);
            begin
               if Present
                    (Find_Init_Call_In_List
                      (First (Statements (Handled_Statement_Sequence (Blk)))))
               then
                  Init_Call := Blk;
               end if;
            end;
         end if;
      end if;

      if Present (Init_Call) then
         Remove (Init_Call);
      end if;
      return Init_Call;
   end Remove_Init_Call;

   -------------------------
   -- Remove_Side_Effects --
   -------------------------

   procedure Remove_Side_Effects
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False)
   is
      Loc          : constant Source_Ptr      := Sloc (Exp);
      Exp_Type     : constant Entity_Id       := Etype (Exp);
      Svg_Suppress : constant Suppress_Record := Scope_Suppress;
      Def_Id       : Entity_Id;
      E            : Node_Id;
      New_Exp      : Node_Id;
      Ptr_Typ_Decl : Node_Id;
      Ref_Type     : Entity_Id;
      Res          : Node_Id;

      function Side_Effect_Free (N : Node_Id) return Boolean;
      --  Determines if the tree N represents an expression that is known not
      --  to have side effects, and for which no processing is required.

      function Side_Effect_Free (L : List_Id) return Boolean;
      --  Determines if all elements of the list L are side effect free

      function Safe_Prefixed_Reference (N : Node_Id) return Boolean;
      --  The argument N is a construct where the Prefix is dereferenced if it
      --  is an access type and the result is a variable. The call returns True
      --  if the construct is side effect free (not considering side effects in
      --  other than the prefix which are to be tested by the caller).

      function Within_In_Parameter (N : Node_Id) return Boolean;
      --  Determines if N is a subcomponent of a composite in-parameter. If so,
      --  N is not side-effect free when the actual is global and modifiable
      --  indirectly from within a subprogram, because it may be passed by
      --  reference. The front-end must be conservative here and assume that
      --  this may happen with any array or record type. On the other hand, we
      --  cannot create temporaries for all expressions for which this
      --  condition is true, for various reasons that might require clearing up
      --  ??? For example, discriminant references that appear out of place, or
      --  spurious type errors with class-wide expressions. As a result, we
      --  limit the transformation to loop bounds, which is so far the only
      --  case that requires it.

      -----------------------------
      -- Safe_Prefixed_Reference --
      -----------------------------

      function Safe_Prefixed_Reference (N : Node_Id) return Boolean is
      begin
         --  If prefix is not side effect free, definitely not safe

         if not Side_Effect_Free (Prefix (N)) then
            return False;

         --  If the prefix is of an access type that is not access-to-constant,
         --  then this construct is a variable reference, which means it is to
         --  be considered to have side effects if Variable_Ref is set True.

         elsif Is_Access_Type (Etype (Prefix (N)))
           and then not Is_Access_Constant (Etype (Prefix (N)))
           and then Variable_Ref
         then
            --  Exception is a prefix that is the result of a previous removal
            --  of side-effects.

            return Is_Entity_Name (Prefix (N))
              and then not Comes_From_Source (Prefix (N))
              and then Ekind (Entity (Prefix (N))) = E_Constant
              and then Is_Internal_Name (Chars (Entity (Prefix (N))));

         --  If the prefix is an explicit dereference then this construct is a
         --  variable reference, which means it is to be considered to have
         --  side effects if Variable_Ref is True.

         --  We do NOT exclude dereferences of access-to-constant types because
         --  we handle them as constant view of variables.

         elsif Nkind (Prefix (N)) = N_Explicit_Dereference
           and then Variable_Ref
         then
            return False;

         --  Note: The following test is the simplest way of solving a complex
         --  problem uncovered by the following test (Side effect on loop bound
         --  that is a subcomponent of a global variable:

         --    with Text_Io; use Text_Io;
         --    procedure Tloop is
         --      type X is
         --        record
         --          V : Natural := 4;
         --          S : String (1..5) := (others => 'a');
         --        end record;
         --      X1 : X;

         --      procedure Modi;

         --      generic
         --        with procedure Action;
         --      procedure Loop_G (Arg : X; Msg : String)

         --      procedure Loop_G (Arg : X; Msg : String) is
         --      begin
         --        Put_Line ("begin loop_g " & Msg & " will loop till: "
         --                  & Natural'Image (Arg.V));
         --        for Index in 1 .. Arg.V loop
         --          Text_Io.Put_Line
         --            (Natural'Image (Index) & " " & Arg.S (Index));
         --          if Index > 2 then
         --            Modi;
         --          end if;
         --        end loop;
         --        Put_Line ("end loop_g " & Msg);
         --      end;

         --      procedure Loop1 is new Loop_G (Modi);
         --      procedure Modi is
         --      begin
         --        X1.V := 1;
         --        Loop1 (X1, "from modi");
         --      end;
         --
         --    begin
         --      Loop1 (X1, "initial");
         --    end;

         --  The output of the above program should be:

         --    begin loop_g initial will loop till:  4
         --     1 a
         --     2 a
         --     3 a
         --    begin loop_g from modi will loop till:  1
         --     1 a
         --    end loop_g from modi
         --     4 a
         --    begin loop_g from modi will loop till:  1
         --     1 a
         --    end loop_g from modi
         --    end loop_g initial

         --  If a loop bound is a subcomponent of a global variable, a
         --  modification of that variable within the loop may incorrectly
         --  affect the execution of the loop.

         elsif Nkind (Parent (Parent (N))) = N_Loop_Parameter_Specification
           and then Within_In_Parameter (Prefix (N))
           and then Variable_Ref
         then
            return False;

         --  All other cases are side effect free

         else
            return True;
         end if;
      end Safe_Prefixed_Reference;

      ----------------------
      -- Side_Effect_Free --
      ----------------------

      function Side_Effect_Free (N : Node_Id) return Boolean is
      begin
         --  Note on checks that could raise Constraint_Error. Strictly, if we
         --  take advantage of 11.6, these checks do not count as side effects.
         --  However, we would prefer to consider that they are side effects,
         --  since the backend CSE does not work very well on expressions which
         --  can raise Constraint_Error. On the other hand if we don't consider
         --  them to be side effect free, then we get some awkward expansions
         --  in -gnato mode, resulting in code insertions at a point where we
         --  do not have a clear model for performing the insertions.

         --  Special handling for entity names

         if Is_Entity_Name (N) then

            --  Variables are considered to be a side effect if Variable_Ref
            --  is set or if we have a volatile reference and Name_Req is off.
            --  If Name_Req is True then we can't help returning a name which
            --  effectively allows multiple references in any case.

            if Is_Variable (N, Use_Original_Node => False) then
               return not Variable_Ref
                 and then (not Is_Volatile_Reference (N) or else Name_Req);

            --  Any other entity (e.g. a subtype name) is definitely side
            --  effect free.

            else
               return True;
            end if;

         --  A value known at compile time is always side effect free

         elsif Compile_Time_Known_Value (N) then
            return True;

         --  A variable renaming is not side-effect free, because the renaming
         --  will function like a macro in the front-end in some cases, and an
         --  assignment can modify the component designated by N, so we need to
         --  create a temporary for it.

         --  The guard testing for Entity being present is needed at least in
         --  the case of rewritten predicate expressions, and may well also be
         --  appropriate elsewhere. Obviously we can't go testing the entity
         --  field if it does not exist, so it's reasonable to say that this is
         --  not the renaming case if it does not exist.

         elsif Is_Entity_Name (Original_Node (N))
           and then Present (Entity (Original_Node (N)))
           and then Is_Renaming_Of_Object (Entity (Original_Node (N)))
           and then Ekind (Entity (Original_Node (N))) /= E_Constant
         then
            declare
               RO : constant Node_Id :=
                      Renamed_Object (Entity (Original_Node (N)));

            begin
               --  If the renamed object is an indexed component, or an
               --  explicit dereference, then the designated object could
               --  be modified by an assignment.

               if Nkind_In (RO, N_Indexed_Component,
                                N_Explicit_Dereference)
               then
                  return False;

               --  A selected component must have a safe prefix

               elsif Nkind (RO) = N_Selected_Component then
                  return Safe_Prefixed_Reference (RO);

               --  In all other cases, designated object cannot be changed so
               --  we are side effect free.

               else
                  return True;
               end if;
            end;

         --  Remove_Side_Effects generates an object renaming declaration to
         --  capture the expression of a class-wide expression. In VM targets
         --  the frontend performs no expansion for dispatching calls to
         --  class- wide types since they are handled by the VM. Hence, we must
         --  locate here if this node corresponds to a previous invocation of
         --  Remove_Side_Effects to avoid a never ending loop in the frontend.

         elsif VM_Target /= No_VM
            and then not Comes_From_Source (N)
            and then Nkind (Parent (N)) = N_Object_Renaming_Declaration
            and then Is_Class_Wide_Type (Etype (N))
         then
            return True;
         end if;

         --  For other than entity names and compile time known values,
         --  check the node kind for special processing.

         case Nkind (N) is

            --  An attribute reference is side effect free if its expressions
            --  are side effect free and its prefix is side effect free or
            --  is an entity reference.

            --  Is this right? what about x'first where x is a variable???

            when N_Attribute_Reference =>
               return Side_Effect_Free (Expressions (N))
                 and then Attribute_Name (N) /= Name_Input
                 and then (Is_Entity_Name (Prefix (N))
                            or else Side_Effect_Free (Prefix (N)));

            --  A binary operator is side effect free if and both operands are
            --  side effect free. For this purpose binary operators include
            --  membership tests and short circuit forms.

            when N_Binary_Op | N_Membership_Test | N_Short_Circuit =>
               return Side_Effect_Free (Left_Opnd  (N))
                        and then
                      Side_Effect_Free (Right_Opnd (N));

            --  An explicit dereference is side effect free only if it is
            --  a side effect free prefixed reference.

            when N_Explicit_Dereference =>
               return Safe_Prefixed_Reference (N);

            --  An expression with action is side effect free if its expression
            --  is side effect free and it has no actions.

            when N_Expression_With_Actions =>
               return Is_Empty_List (Actions (N))
                        and then
                      Side_Effect_Free (Expression (N));

            --  A call to _rep_to_pos is side effect free, since we generate
            --  this pure function call ourselves. Moreover it is critically
            --  important to make this exception, since otherwise we can have
            --  discriminants in array components which don't look side effect
            --  free in the case of an array whose index type is an enumeration
            --  type with an enumeration rep clause.

            --  All other function calls are not side effect free

            when N_Function_Call =>
               return Nkind (Name (N)) = N_Identifier
                 and then Is_TSS (Name (N), TSS_Rep_To_Pos)
                 and then
                   Side_Effect_Free (First (Parameter_Associations (N)));

            --  An indexed component is side effect free if it is a side
            --  effect free prefixed reference and all the indexing
            --  expressions are side effect free.

            when N_Indexed_Component =>
               return Side_Effect_Free (Expressions (N))
                 and then Safe_Prefixed_Reference (N);

            --  A type qualification is side effect free if the expression
            --  is side effect free.

            when N_Qualified_Expression =>
               return Side_Effect_Free (Expression (N));

            --  A selected component is side effect free only if it is a side
            --  effect free prefixed reference. If it designates a component
            --  with a rep. clause it must be treated has having a potential
            --  side effect, because it may be modified through a renaming, and
            --  a subsequent use of the renaming as a macro will yield the
            --  wrong value. This complex interaction between renaming and
            --  removing side effects is a reminder that the latter has become
            --  a headache to maintain, and that it should be removed in favor
            --  of the gcc mechanism to capture values ???

            when N_Selected_Component =>
               if Nkind (Parent (N)) = N_Explicit_Dereference
                 and then Has_Non_Standard_Rep (Designated_Type (Etype (N)))
               then
                  return False;
               else
                  return Safe_Prefixed_Reference (N);
               end if;

            --  A range is side effect free if the bounds are side effect free

            when N_Range =>
               return Side_Effect_Free (Low_Bound (N))
                 and then Side_Effect_Free (High_Bound (N));

            --  A slice is side effect free if it is a side effect free
            --  prefixed reference and the bounds are side effect free.

            when N_Slice =>
               return Side_Effect_Free (Discrete_Range (N))
                 and then Safe_Prefixed_Reference (N);

            --  A type conversion is side effect free if the expression to be
            --  converted is side effect free.

            when N_Type_Conversion =>
               return Side_Effect_Free (Expression (N));

            --  A unary operator is side effect free if the operand
            --  is side effect free.

            when N_Unary_Op =>
               return Side_Effect_Free (Right_Opnd (N));

            --  An unchecked type conversion is side effect free only if it
            --  is safe and its argument is side effect free.

            when N_Unchecked_Type_Conversion =>
               return Safe_Unchecked_Type_Conversion (N)
                 and then Side_Effect_Free (Expression (N));

            --  An unchecked expression is side effect free if its expression
            --  is side effect free.

            when N_Unchecked_Expression =>
               return Side_Effect_Free (Expression (N));

            --  A literal is side effect free

            when N_Character_Literal    |
                 N_Integer_Literal      |
                 N_Real_Literal         |
                 N_String_Literal       =>
               return True;

            --  We consider that anything else has side effects. This is a bit
            --  crude, but we are pretty close for most common cases, and we
            --  are certainly correct (i.e. we never return True when the
            --  answer should be False).

            when others =>
               return False;
         end case;
      end Side_Effect_Free;

      --  A list is side effect free if all elements of the list are side
      --  effect free.

      function Side_Effect_Free (L : List_Id) return Boolean is
         N : Node_Id;

      begin
         if L = No_List or else L = Error_List then
            return True;

         else
            N := First (L);
            while Present (N) loop
               if not Side_Effect_Free (N) then
                  return False;
               else
                  Next (N);
               end if;
            end loop;

            return True;
         end if;
      end Side_Effect_Free;

      -------------------------
      -- Within_In_Parameter --
      -------------------------

      function Within_In_Parameter (N : Node_Id) return Boolean is
      begin
         if not Comes_From_Source (N) then
            return False;

         elsif Is_Entity_Name (N) then
            return Ekind (Entity (N)) = E_In_Parameter;

         elsif Nkind_In (N, N_Indexed_Component, N_Selected_Component) then
            return Within_In_Parameter (Prefix (N));

         else
            return False;
         end if;
      end Within_In_Parameter;

   --  Start of processing for Remove_Side_Effects

   begin
      --  Handle cases in which there is nothing to do

      if not Expander_Active then
         return;
      end if;

      --  Cannot generate temporaries if the invocation to remove side effects
      --  was issued too early and the type of the expression is not resolved
      --  (this happens because routines Duplicate_Subexpr_XX implicitly invoke
      --  Remove_Side_Effects).

      if No (Exp_Type)
        or else Ekind (Exp_Type) = E_Access_Attribute_Type
      then
         return;

      --  No action needed for side-effect free expressions

      elsif Side_Effect_Free (Exp) then
         return;
      end if;

      --  The remaining procesaing is done with all checks suppressed

      --  Note: from now on, don't use return statements, instead do a goto
      --  Leave, to ensure that we properly restore Scope_Suppress.Suppress.

      Scope_Suppress.Suppress := (others => True);

      --  If it is a scalar type and we need to capture the value, just make
      --  a copy. Likewise for a function call, an attribute reference, an
      --  allocator, or an operator. And if we have a volatile reference and
      --  Name_Req is not set (see comments above for Side_Effect_Free).

      if Is_Elementary_Type (Exp_Type)
        and then (Variable_Ref
                   or else Nkind_In (Exp, N_Function_Call,
                                          N_Attribute_Reference,
                                          N_Allocator)
                   or else Nkind (Exp) in N_Op
                   or else (not Name_Req and then Is_Volatile_Reference (Exp)))
      then
         Def_Id := Make_Temporary (Loc, 'R', Exp);
         Set_Etype (Def_Id, Exp_Type);
         Res := New_Reference_To (Def_Id, Loc);

         --  If the expression is a packed reference, it must be reanalyzed and
         --  expanded, depending on context. This is the case for actuals where
         --  a constraint check may capture the actual before expansion of the
         --  call is complete.

         if Nkind (Exp) = N_Indexed_Component
           and then Is_Packed (Etype (Prefix (Exp)))
         then
            Set_Analyzed (Exp, False);
            Set_Analyzed (Prefix (Exp), False);
         end if;

         E :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Exp_Type, Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Exp));

         Set_Assignment_OK (E);
         Insert_Action (Exp, E);

      --  If the expression has the form v.all then we can just capture the
      --  pointer, and then do an explicit dereference on the result.

      elsif Nkind (Exp) = N_Explicit_Dereference then
         Def_Id := Make_Temporary (Loc, 'R', Exp);
         Res :=
           Make_Explicit_Dereference (Loc, New_Reference_To (Def_Id, Loc));

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   =>
               New_Reference_To (Etype (Prefix (Exp)), Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Prefix (Exp))));

      --  Similar processing for an unchecked conversion of an expression of
      --  the form v.all, where we want the same kind of treatment.

      elsif Nkind (Exp) = N_Unchecked_Type_Conversion
        and then Nkind (Expression (Exp)) = N_Explicit_Dereference
      then
         Remove_Side_Effects (Expression (Exp), Name_Req, Variable_Ref);
         goto Leave;

      --  If this is a type conversion, leave the type conversion and remove
      --  the side effects in the expression. This is important in several
      --  circumstances: for change of representations, and also when this is a
      --  view conversion to a smaller object, where gigi can end up creating
      --  its own temporary of the wrong size.

      elsif Nkind (Exp) = N_Type_Conversion then
         Remove_Side_Effects (Expression (Exp), Name_Req, Variable_Ref);
         goto Leave;

      --  If this is an unchecked conversion that Gigi can't handle, make
      --  a copy or a use a renaming to capture the value.

      elsif Nkind (Exp) = N_Unchecked_Type_Conversion
        and then not Safe_Unchecked_Type_Conversion (Exp)
      then
         if CW_Or_Has_Controlled_Part (Exp_Type) then

            --  Use a renaming to capture the expression, rather than create
            --  a controlled temporary.

            Def_Id := Make_Temporary (Loc, 'R', Exp);
            Res := New_Reference_To (Def_Id, Loc);

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        => New_Reference_To (Exp_Type, Loc),
                Name                => Relocate_Node (Exp)));

         else
            Def_Id := Make_Temporary (Loc, 'R', Exp);
            Set_Etype (Def_Id, Exp_Type);
            Res := New_Reference_To (Def_Id, Loc);

            E :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Reference_To (Exp_Type, Loc),
                Constant_Present    => not Is_Variable (Exp),
                Expression          => Relocate_Node (Exp));

            Set_Assignment_OK (E);
            Insert_Action (Exp, E);
         end if;

      --  For expressions that denote objects, we can use a renaming scheme.
      --  This is needed for correctness in the case of a volatile object of
      --  a non-volatile type because the Make_Reference call of the "default"
      --  approach would generate an illegal access value (an access value
      --  cannot designate such an object - see Analyze_Reference). We skip
      --  using this scheme if we have an object of a volatile type and we do
      --  not have Name_Req set true (see comments above for Side_Effect_Free).

      --  In Ada 2012 a qualified expression is an object, but for purposes of
      --  removing side effects it still need to be transformed into a separate
      --  declaration, particularly if the expression is an aggregate.

      elsif Is_Object_Reference (Exp)
        and then Nkind (Exp) /= N_Function_Call
        and then Nkind (Exp) /= N_Qualified_Expression
        and then (Name_Req or else not Treat_As_Volatile (Exp_Type))
      then
         Def_Id := Make_Temporary (Loc, 'R', Exp);

         if Nkind (Exp) = N_Selected_Component
           and then Nkind (Prefix (Exp)) = N_Function_Call
           and then Is_Array_Type (Exp_Type)
         then
            --  Avoid generating a variable-sized temporary, by generating
            --  the renaming declaration just for the function call. The
            --  transformation could be refined to apply only when the array
            --  component is constrained by a discriminant???

            Res :=
              Make_Selected_Component (Loc,
                Prefix => New_Occurrence_Of (Def_Id, Loc),
                Selector_Name => Selector_Name (Exp));

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        =>
                  New_Reference_To (Base_Type (Etype (Prefix (Exp))), Loc),
                Name                => Relocate_Node (Prefix (Exp))));

         else
            Res := New_Reference_To (Def_Id, Loc);

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        => New_Reference_To (Exp_Type, Loc),
                Name                => Relocate_Node (Exp)));
         end if;

         --  If this is a packed reference, or a selected component with
         --  a non-standard representation, a reference to the temporary
         --  will be replaced by a copy of the original expression (see
         --  Exp_Ch2.Expand_Renaming). Otherwise the temporary must be
         --  elaborated by gigi, and is of course not to be replaced in-line
         --  by the expression it renames, which would defeat the purpose of
         --  removing the side-effect.

         if Nkind_In (Exp, N_Selected_Component, N_Indexed_Component)
           and then Has_Non_Standard_Rep (Etype (Prefix (Exp)))
         then
            null;
         else
            Set_Is_Renaming_Of_Object (Def_Id, False);
         end if;

      --  Otherwise we generate a reference to the value

      else
         --  An expression which is in SPARK mode is considered side effect
         --  free if the resulting value is captured by a variable or a
         --  constant.

         if SPARK_Mode
           and then Nkind (Parent (Exp)) = N_Object_Declaration
         then
            goto Leave;
         end if;

         --  Special processing for function calls that return a limited type.
         --  We need to build a declaration that will enable build-in-place
         --  expansion of the call. This is not done if the context is already
         --  an object declaration, to prevent infinite recursion.

         --  This is relevant only in Ada 2005 mode. In Ada 95 programs we have
         --  to accommodate functions returning limited objects by reference.

         if Ada_Version >= Ada_2005
           and then Nkind (Exp) = N_Function_Call
           and then Is_Limited_View (Etype (Exp))
           and then Nkind (Parent (Exp)) /= N_Object_Declaration
         then
            declare
               Obj  : constant Entity_Id := Make_Temporary (Loc, 'F', Exp);
               Decl : Node_Id;

            begin
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Obj,
                   Object_Definition   => New_Occurrence_Of (Exp_Type, Loc),
                   Expression          => Relocate_Node (Exp));

               Insert_Action (Exp, Decl);
               Set_Etype (Obj, Exp_Type);
               Rewrite (Exp, New_Occurrence_Of (Obj, Loc));
               goto Leave;
            end;
         end if;

         Def_Id := Make_Temporary (Loc, 'R', Exp);

         --  The regular expansion of functions with side effects involves the
         --  generation of an access type to capture the return value found on
         --  the secondary stack. Since SPARK (and why) cannot process access
         --  types, use a different approach which ignores the secondary stack
         --  and "copies" the returned object.

         if SPARK_Mode then
            Res := New_Reference_To (Def_Id, Loc);
            Ref_Type := Exp_Type;

         --  Regular expansion utilizing an access type and 'reference

         else
            Res :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Reference_To (Def_Id, Loc));

            --  Generate:
            --    type Ann is access all <Exp_Type>;

            Ref_Type := Make_Temporary (Loc, 'A');

            Ptr_Typ_Decl :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Ref_Type,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present        => True,
                    Subtype_Indication =>
                      New_Reference_To (Exp_Type, Loc)));

            Insert_Action (Exp, Ptr_Typ_Decl);
         end if;

         E := Exp;
         if Nkind (E) = N_Explicit_Dereference then
            New_Exp := Relocate_Node (Prefix (E));
         else
            E := Relocate_Node (E);

            --  Do not generate a 'reference in SPARK mode since the access
            --  type is not created in the first place.

            if SPARK_Mode then
               New_Exp := E;

            --  Otherwise generate reference, marking the value as non-null
            --  since we know it cannot be null and we don't want a check.

            else
               New_Exp := Make_Reference (Loc, E);
               Set_Is_Known_Non_Null (Def_Id);
            end if;
         end if;

         if Is_Delayed_Aggregate (E) then

            --  The expansion of nested aggregates is delayed until the
            --  enclosing aggregate is expanded. As aggregates are often
            --  qualified, the predicate applies to qualified expressions as
            --  well, indicating that the enclosing aggregate has not been
            --  expanded yet. At this point the aggregate is part of a
            --  stand-alone declaration, and must be fully expanded.

            if Nkind (E) = N_Qualified_Expression then
               Set_Expansion_Delayed (Expression (E), False);
               Set_Analyzed (Expression (E), False);
            else
               Set_Expansion_Delayed (E, False);
            end if;

            Set_Analyzed (E, False);
         end if;

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Ref_Type, Loc),
             Constant_Present    => True,
             Expression          => New_Exp));
      end if;

      --  Preserve the Assignment_OK flag in all copies, since at least one
      --  copy may be used in a context where this flag must be set (otherwise
      --  why would the flag be set in the first place).

      Set_Assignment_OK (Res, Assignment_OK (Exp));

      --  Finally rewrite the original expression and we are done

      Rewrite (Exp, Res);
      Analyze_And_Resolve (Exp, Exp_Type);

   <<Leave>>
      Scope_Suppress := Svg_Suppress;
   end Remove_Side_Effects;

   ---------------------------
   -- Represented_As_Scalar --
   ---------------------------

   function Represented_As_Scalar (T : Entity_Id) return Boolean is
      UT : constant Entity_Id := Underlying_Type (T);
   begin
      return Is_Scalar_Type (UT)
        or else (Is_Bit_Packed_Array (UT)
                  and then Is_Scalar_Type (Packed_Array_Type (UT)));
   end Represented_As_Scalar;

   ------------------------------
   -- Requires_Cleanup_Actions --
   ------------------------------

   function Requires_Cleanup_Actions
     (N         : Node_Id;
      Lib_Level : Boolean) return Boolean
   is
      At_Lib_Level : constant Boolean :=
                       Lib_Level
                         and then Nkind_In (N, N_Package_Body,
                                               N_Package_Specification);
      --  N is at the library level if the top-most context is a package and
      --  the path taken to reach N does not inlcude non-package constructs.

   begin
      case Nkind (N) is
         when N_Accept_Statement      |
              N_Block_Statement       |
              N_Entry_Body            |
              N_Package_Body          |
              N_Protected_Body        |
              N_Subprogram_Body       |
              N_Task_Body             =>
            return
              Requires_Cleanup_Actions (Declarations (N), At_Lib_Level, True)
                or else
                  (Present (Handled_Statement_Sequence (N))
                    and then
                      Requires_Cleanup_Actions
                        (Statements (Handled_Statement_Sequence (N)),
                         At_Lib_Level, True));

         when N_Package_Specification =>
            return
              Requires_Cleanup_Actions
                (Visible_Declarations (N), At_Lib_Level, True)
                  or else
              Requires_Cleanup_Actions
                (Private_Declarations (N), At_Lib_Level, True);

         when others                  =>
            return False;
      end case;
   end Requires_Cleanup_Actions;

   ------------------------------
   -- Requires_Cleanup_Actions --
   ------------------------------

   function Requires_Cleanup_Actions
     (L                 : List_Id;
      Lib_Level         : Boolean;
      Nested_Constructs : Boolean) return Boolean
   is
      Decl    : Node_Id;
      Expr    : Node_Id;
      Obj_Id  : Entity_Id;
      Obj_Typ : Entity_Id;
      Pack_Id : Entity_Id;
      Typ     : Entity_Id;

   begin
      if No (L)
        or else Is_Empty_List (L)
      then
         return False;
      end if;

      Decl := First (L);
      while Present (Decl) loop

         --  Library-level tagged types

         if Nkind (Decl) = N_Full_Type_Declaration then
            Typ := Defining_Identifier (Decl);

            if Is_Tagged_Type (Typ)
              and then Is_Library_Level_Entity (Typ)
              and then Convention (Typ) = Convention_Ada
              and then Present (Access_Disp_Table (Typ))
              and then RTE_Available (RE_Unregister_Tag)
              and then not No_Run_Time_Mode
              and then not Is_Abstract_Type (Typ)
            then
               return True;
            end if;

         --  Regular object declarations

         elsif Nkind (Decl) = N_Object_Declaration then
            Obj_Id  := Defining_Identifier (Decl);
            Obj_Typ := Base_Type (Etype (Obj_Id));
            Expr    := Expression (Decl);

            --  Bypass any form of processing for objects which have their
            --  finalization disabled. This applies only to objects at the
            --  library level.

            if Lib_Level and then Finalize_Storage_Only (Obj_Typ) then
               null;

            --  Transient variables are treated separately in order to minimize
            --  the size of the generated code. See Exp_Ch7.Process_Transient_
            --  Objects.

            elsif Is_Processed_Transient (Obj_Id) then
               null;

            --  The object is of the form:
            --    Obj : Typ [:= Expr];
            --
            --  Do not process the incomplete view of a deferred constant. Do
            --  not consider tag-to-class-wide conversions.

            elsif not Is_Imported (Obj_Id)
              and then Needs_Finalization (Obj_Typ)
              and then not (Ekind (Obj_Id) = E_Constant
                             and then not Has_Completion (Obj_Id))
              and then not Is_Tag_To_Class_Wide_Conversion (Obj_Id)
            then
               return True;

            --  The object is of the form:
            --    Obj : Access_Typ := Non_BIP_Function_Call'reference;
            --
            --    Obj : Access_Typ :=
            --            BIP_Function_Call (BIPalloc => 2, ...)'reference;

            elsif Is_Access_Type (Obj_Typ)
              and then Needs_Finalization
                         (Available_View (Designated_Type (Obj_Typ)))
              and then Present (Expr)
              and then
                (Is_Secondary_Stack_BIP_Func_Call (Expr)
                  or else
                    (Is_Non_BIP_Func_Call (Expr)
                      and then not Is_Related_To_Func_Return (Obj_Id)))
            then
               return True;

            --  Processing for "hook" objects generated for controlled
            --  transients declared inside an Expression_With_Actions.

            elsif Is_Access_Type (Obj_Typ)
              and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
              and then Nkind (Status_Flag_Or_Transient_Decl (Obj_Id)) =
                                                      N_Object_Declaration
              and then Is_Finalizable_Transient
                         (Status_Flag_Or_Transient_Decl (Obj_Id), Decl)
            then
               return True;

            --  Processing for intermediate results of if expressions where
            --  one of the alternatives uses a controlled function call.

            elsif Is_Access_Type (Obj_Typ)
              and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
              and then Nkind (Status_Flag_Or_Transient_Decl (Obj_Id)) =
                                                      N_Defining_Identifier
              and then Present (Expr)
              and then Nkind (Expr) = N_Null
            then
               return True;

            --  Simple protected objects which use type System.Tasking.
            --  Protected_Objects.Protection to manage their locks should be
            --  treated as controlled since they require manual cleanup.

            elsif Ekind (Obj_Id) = E_Variable
              and then
                (Is_Simple_Protected_Type (Obj_Typ)
                  or else Has_Simple_Protected_Object (Obj_Typ))
            then
               return True;
            end if;

         --  Specific cases of object renamings

         elsif Nkind (Decl) = N_Object_Renaming_Declaration then
            Obj_Id  := Defining_Identifier (Decl);
            Obj_Typ := Base_Type (Etype (Obj_Id));

            --  Bypass any form of processing for objects which have their
            --  finalization disabled. This applies only to objects at the
            --  library level.

            if Lib_Level and then Finalize_Storage_Only (Obj_Typ) then
               null;

            --  Return object of a build-in-place function. This case is
            --  recognized and marked by the expansion of an extended return
            --  statement (see Expand_N_Extended_Return_Statement).

            elsif Needs_Finalization (Obj_Typ)
              and then Is_Return_Object (Obj_Id)
              and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
            then
               return True;

            --  Detect a case where a source object has been initialized by
            --  a controlled function call or another object which was later
            --  rewritten as a class-wide conversion of Ada.Tags.Displace.

            --     Obj1 : CW_Type := Src_Obj;
            --     Obj2 : CW_Type := Function_Call (...);

            --     Obj1 : CW_Type renames (... Ada.Tags.Displace (Src_Obj));
            --     Tmp  : ... := Function_Call (...)'reference;
            --     Obj2 : CW_Type renames (... Ada.Tags.Displace (Tmp));

            elsif Is_Displacement_Of_Object_Or_Function_Result (Obj_Id) then
               return True;
            end if;

         --  Inspect the freeze node of an access-to-controlled type and look
         --  for a delayed finalization master. This case arises when the
         --  freeze actions are inserted at a later time than the expansion of
         --  the context. Since Build_Finalizer is never called on a single
         --  construct twice, the master will be ultimately left out and never
         --  finalized. This is also needed for freeze actions of designated
         --  types themselves, since in some cases the finalization master is
         --  associated with a designated type's freeze node rather than that
         --  of the access type (see handling for freeze actions in
         --  Build_Finalization_Master).

         elsif Nkind (Decl) = N_Freeze_Entity
           and then Present (Actions (Decl))
         then
            Typ := Entity (Decl);

            if ((Is_Access_Type (Typ)
                  and then not Is_Access_Subprogram_Type (Typ)
                  and then Needs_Finalization
                             (Available_View (Designated_Type (Typ))))
               or else
                (Is_Type (Typ)
                  and then Needs_Finalization (Typ)))
              and then Requires_Cleanup_Actions
                         (Actions (Decl), Lib_Level, Nested_Constructs)
            then
               return True;
            end if;

         --  Nested package declarations

         elsif Nested_Constructs
           and then Nkind (Decl) = N_Package_Declaration
         then
            Pack_Id := Defining_Unit_Name (Specification (Decl));

            if Nkind (Pack_Id) = N_Defining_Program_Unit_Name then
               Pack_Id := Defining_Identifier (Pack_Id);
            end if;

            if Ekind (Pack_Id) /= E_Generic_Package
              and then
                Requires_Cleanup_Actions (Specification (Decl), Lib_Level)
            then
               return True;
            end if;

         --  Nested package bodies

         elsif Nested_Constructs and then Nkind (Decl) = N_Package_Body then
            Pack_Id := Corresponding_Spec (Decl);

            if Ekind (Pack_Id) /= E_Generic_Package
              and then Requires_Cleanup_Actions (Decl, Lib_Level)
            then
               return True;
            end if;
         end if;

         Next (Decl);
      end loop;

      return False;
   end Requires_Cleanup_Actions;

   ------------------------------------
   -- Safe_Unchecked_Type_Conversion --
   ------------------------------------

   --  Note: this function knows quite a bit about the exact requirements of
   --  Gigi with respect to unchecked type conversions, and its code must be
   --  coordinated with any changes in Gigi in this area.

   --  The above requirements should be documented in Sinfo ???

   function Safe_Unchecked_Type_Conversion (Exp : Node_Id) return Boolean is
      Otyp   : Entity_Id;
      Ityp   : Entity_Id;
      Oalign : Uint;
      Ialign : Uint;
      Pexp   : constant Node_Id := Parent (Exp);

   begin
      --  If the expression is the RHS of an assignment or object declaration
      --   we are always OK because there will always be a target.

      --  Object renaming declarations, (generated for view conversions of
      --  actuals in inlined calls), like object declarations, provide an
      --  explicit type, and are safe as well.

      if (Nkind (Pexp) = N_Assignment_Statement
           and then Expression (Pexp) = Exp)
        or else Nkind_In (Pexp, N_Object_Declaration,
                                N_Object_Renaming_Declaration)
      then
         return True;

      --  If the expression is the prefix of an N_Selected_Component we should
      --  also be OK because GCC knows to look inside the conversion except if
      --  the type is discriminated. We assume that we are OK anyway if the
      --  type is not set yet or if it is controlled since we can't afford to
      --  introduce a temporary in this case.

      elsif Nkind (Pexp) = N_Selected_Component
        and then Prefix (Pexp) = Exp
      then
         if No (Etype (Pexp)) then
            return True;
         else
            return
              not Has_Discriminants (Etype (Pexp))
                or else Is_Constrained (Etype (Pexp));
         end if;
      end if;

      --  Set the output type, this comes from Etype if it is set, otherwise we
      --  take it from the subtype mark, which we assume was already fully
      --  analyzed.

      if Present (Etype (Exp)) then
         Otyp := Etype (Exp);
      else
         Otyp := Entity (Subtype_Mark (Exp));
      end if;

      --  The input type always comes from the expression, and we assume
      --  this is indeed always analyzed, so we can simply get the Etype.

      Ityp := Etype (Expression (Exp));

      --  Initialize alignments to unknown so far

      Oalign := No_Uint;
      Ialign := No_Uint;

      --  Replace a concurrent type by its corresponding record type and each
      --  type by its underlying type and do the tests on those. The original
      --  type may be a private type whose completion is a concurrent type, so
      --  find the underlying type first.

      if Present (Underlying_Type (Otyp)) then
         Otyp := Underlying_Type (Otyp);
      end if;

      if Present (Underlying_Type (Ityp)) then
         Ityp := Underlying_Type (Ityp);
      end if;

      if Is_Concurrent_Type (Otyp) then
         Otyp := Corresponding_Record_Type (Otyp);
      end if;

      if Is_Concurrent_Type (Ityp) then
         Ityp := Corresponding_Record_Type (Ityp);
      end if;

      --  If the base types are the same, we know there is no problem since
      --  this conversion will be a noop.

      if Implementation_Base_Type (Otyp) = Implementation_Base_Type (Ityp) then
         return True;

      --  Same if this is an upwards conversion of an untagged type, and there
      --  are no constraints involved (could be more general???)

      elsif Etype (Ityp) = Otyp
        and then not Is_Tagged_Type (Ityp)
        and then not Has_Discriminants (Ityp)
        and then No (First_Rep_Item (Base_Type (Ityp)))
      then
         return True;

      --  If the expression has an access type (object or subprogram) we assume
      --  that the conversion is safe, because the size of the target is safe,
      --  even if it is a record (which might be treated as having unknown size
      --  at this point).

      elsif Is_Access_Type (Ityp) then
         return True;

      --  If the size of output type is known at compile time, there is never
      --  a problem. Note that unconstrained records are considered to be of
      --  known size, but we can't consider them that way here, because we are
      --  talking about the actual size of the object.

      --  We also make sure that in addition to the size being known, we do not
      --  have a case which might generate an embarrassingly large temp in
      --  stack checking mode.

      elsif Size_Known_At_Compile_Time (Otyp)
        and then
          (not Stack_Checking_Enabled
            or else not May_Generate_Large_Temp (Otyp))
        and then not (Is_Record_Type (Otyp) and then not Is_Constrained (Otyp))
      then
         return True;

      --  If either type is tagged, then we know the alignment is OK so
      --  Gigi will be able to use pointer punning.

      elsif Is_Tagged_Type (Otyp) or else Is_Tagged_Type (Ityp) then
         return True;

      --  If either type is a limited record type, we cannot do a copy, so say
      --  safe since there's nothing else we can do.

      elsif Is_Limited_Record (Otyp) or else Is_Limited_Record (Ityp) then
         return True;

      --  Conversions to and from packed array types are always ignored and
      --  hence are safe.

      elsif Is_Packed_Array_Type (Otyp)
        or else Is_Packed_Array_Type (Ityp)
      then
         return True;
      end if;

      --  The only other cases known to be safe is if the input type's
      --  alignment is known to be at least the maximum alignment for the
      --  target or if both alignments are known and the output type's
      --  alignment is no stricter than the input's. We can use the component
      --  type alignement for an array if a type is an unpacked array type.

      if Present (Alignment_Clause (Otyp)) then
         Oalign := Expr_Value (Expression (Alignment_Clause (Otyp)));

      elsif Is_Array_Type (Otyp)
        and then Present (Alignment_Clause (Component_Type (Otyp)))
      then
         Oalign := Expr_Value (Expression (Alignment_Clause
                                           (Component_Type (Otyp))));
      end if;

      if Present (Alignment_Clause (Ityp)) then
         Ialign := Expr_Value (Expression (Alignment_Clause (Ityp)));

      elsif Is_Array_Type (Ityp)
        and then Present (Alignment_Clause (Component_Type (Ityp)))
      then
         Ialign := Expr_Value (Expression (Alignment_Clause
                                           (Component_Type (Ityp))));
      end if;

      if Ialign /= No_Uint and then Ialign > Maximum_Alignment then
         return True;

      elsif Ialign /= No_Uint and then Oalign /= No_Uint
        and then Ialign <= Oalign
      then
         return True;

      --   Otherwise, Gigi cannot handle this and we must make a temporary

      else
         return False;
      end if;
   end Safe_Unchecked_Type_Conversion;

   ---------------------------------
   -- Set_Current_Value_Condition --
   ---------------------------------

   --  Note: the implementation of this procedure is very closely tied to the
   --  implementation of Get_Current_Value_Condition. Here we set required
   --  Current_Value fields, and in Get_Current_Value_Condition, we interpret
   --  them, so they must have a consistent view.

   procedure Set_Current_Value_Condition (Cnode : Node_Id) is

      procedure Set_Entity_Current_Value (N : Node_Id);
      --  If N is an entity reference, where the entity is of an appropriate
      --  kind, then set the current value of this entity to Cnode, unless
      --  there is already a definite value set there.

      procedure Set_Expression_Current_Value (N : Node_Id);
      --  If N is of an appropriate form, sets an appropriate entry in current
      --  value fields of relevant entities. Multiple entities can be affected
      --  in the case of an AND or AND THEN.

      ------------------------------
      -- Set_Entity_Current_Value --
      ------------------------------

      procedure Set_Entity_Current_Value (N : Node_Id) is
      begin
         if Is_Entity_Name (N) then
            declare
               Ent : constant Entity_Id := Entity (N);

            begin
               --  Don't capture if not safe to do so

               if not Safe_To_Capture_Value (N, Ent, Cond => True) then
                  return;
               end if;

               --  Here we have a case where the Current_Value field may need
               --  to be set. We set it if it is not already set to a compile
               --  time expression value.

               --  Note that this represents a decision that one condition
               --  blots out another previous one. That's certainly right if
               --  they occur at the same level. If the second one is nested,
               --  then the decision is neither right nor wrong (it would be
               --  equally OK to leave the outer one in place, or take the new
               --  inner one. Really we should record both, but our data
               --  structures are not that elaborate.

               if Nkind (Current_Value (Ent)) not in N_Subexpr then
                  Set_Current_Value (Ent, Cnode);
               end if;
            end;
         end if;
      end Set_Entity_Current_Value;

      ----------------------------------
      -- Set_Expression_Current_Value --
      ----------------------------------

      procedure Set_Expression_Current_Value (N : Node_Id) is
         Cond : Node_Id;

      begin
         Cond := N;

         --  Loop to deal with (ignore for now) any NOT operators present. The
         --  presence of NOT operators will be handled properly when we call
         --  Get_Current_Value_Condition.

         while Nkind (Cond) = N_Op_Not loop
            Cond := Right_Opnd (Cond);
         end loop;

         --  For an AND or AND THEN, recursively process operands

         if Nkind (Cond) = N_Op_And or else Nkind (Cond) = N_And_Then then
            Set_Expression_Current_Value (Left_Opnd (Cond));
            Set_Expression_Current_Value (Right_Opnd (Cond));
            return;
         end if;

         --  Check possible relational operator

         if Nkind (Cond) in N_Op_Compare then
            if Compile_Time_Known_Value (Right_Opnd (Cond)) then
               Set_Entity_Current_Value (Left_Opnd (Cond));
            elsif Compile_Time_Known_Value (Left_Opnd (Cond)) then
               Set_Entity_Current_Value (Right_Opnd (Cond));
            end if;

         elsif Nkind_In (Cond,
                 N_Type_Conversion,
                 N_Qualified_Expression,
                 N_Expression_With_Actions)
         then
            Set_Expression_Current_Value (Expression (Cond));

         --  Check possible boolean variable reference

         else
            Set_Entity_Current_Value (Cond);
         end if;
      end Set_Expression_Current_Value;

   --  Start of processing for Set_Current_Value_Condition

   begin
      Set_Expression_Current_Value (Condition (Cnode));
   end Set_Current_Value_Condition;

   --------------------------
   -- Set_Elaboration_Flag --
   --------------------------

   procedure Set_Elaboration_Flag (N : Node_Id; Spec_Id : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Entity_Id  := Elaboration_Entity (Spec_Id);
      Asn : Node_Id;

   begin
      if Present (Ent) then

         --  Nothing to do if at the compilation unit level, because in this
         --  case the flag is set by the binder generated elaboration routine.

         if Nkind (Parent (N)) = N_Compilation_Unit then
            null;

         --  Here we do need to generate an assignment statement

         else
            Check_Restriction (No_Elaboration_Code, N);
            Asn :=
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Ent, Loc),
                Expression => Make_Integer_Literal (Loc, Uint_1));

            if Nkind (Parent (N)) = N_Subunit then
               Insert_After (Corresponding_Stub (Parent (N)), Asn);
            else
               Insert_After (N, Asn);
            end if;

            Analyze (Asn);

            --  Kill current value indication. This is necessary because the
            --  tests of this flag are inserted out of sequence and must not
            --  pick up bogus indications of the wrong constant value.

            Set_Current_Value (Ent, Empty);
         end if;
      end if;
   end Set_Elaboration_Flag;

   ----------------------------
   -- Set_Renamed_Subprogram --
   ----------------------------

   procedure Set_Renamed_Subprogram (N : Node_Id; E : Entity_Id) is
   begin
      --  If input node is an identifier, we can just reset it

      if Nkind (N) = N_Identifier then
         Set_Chars  (N, Chars (E));
         Set_Entity (N, E);

         --  Otherwise we have to do a rewrite, preserving Comes_From_Source

      else
         declare
            CS : constant Boolean := Comes_From_Source (N);
         begin
            Rewrite (N, Make_Identifier (Sloc (N), Chars (E)));
            Set_Entity (N, E);
            Set_Comes_From_Source (N, CS);
            Set_Analyzed (N, True);
         end;
      end if;
   end Set_Renamed_Subprogram;

   ----------------------------------
   -- Silly_Boolean_Array_Not_Test --
   ----------------------------------

   --  This procedure implements an odd and silly test. We explicitly check
   --  for the case where the 'First of the component type is equal to the
   --  'Last of this component type, and if this is the case, we make sure
   --  that constraint error is raised. The reason is that the NOT is bound
   --  to cause CE in this case, and we will not otherwise catch it.

   --  No such check is required for AND and OR, since for both these cases
   --  False op False = False, and True op True = True. For the XOR case,
   --  see Silly_Boolean_Array_Xor_Test.

   --  Believe it or not, this was reported as a bug. Note that nearly always,
   --  the test will evaluate statically to False, so the code will be
   --  statically removed, and no extra overhead caused.

   procedure Silly_Boolean_Array_Not_Test (N : Node_Id; T : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      CT  : constant Entity_Id  := Component_Type (T);

   begin
      --  The check we install is

      --    constraint_error when
      --      component_type'first = component_type'last
      --        and then array_type'Length /= 0)

      --  We need the last guard because we don't want to raise CE for empty
      --  arrays since no out of range values result. (Empty arrays with a
      --  component type of True .. True -- very useful -- even the ACATS
      --  does not test that marginal case!)

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition =>
            Make_And_Then (Loc,
              Left_Opnd =>
                Make_Op_Eq (Loc,
                  Left_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (CT, Loc),
                      Attribute_Name => Name_First),

                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (CT, Loc),
                      Attribute_Name => Name_Last)),

              Right_Opnd => Make_Non_Empty_Check (Loc, Right_Opnd (N))),
          Reason => CE_Range_Check_Failed));
   end Silly_Boolean_Array_Not_Test;

   ----------------------------------
   -- Silly_Boolean_Array_Xor_Test --
   ----------------------------------

   --  This procedure implements an odd and silly test. We explicitly check
   --  for the XOR case where the component type is True .. True, since this
   --  will raise constraint error. A special check is required since CE
   --  will not be generated otherwise (cf Expand_Packed_Not).

   --  No such check is required for AND and OR, since for both these cases
   --  False op False = False, and True op True = True, and no check is
   --  required for the case of False .. False, since False xor False = False.
   --  See also Silly_Boolean_Array_Not_Test

   procedure Silly_Boolean_Array_Xor_Test (N : Node_Id; T : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      CT  : constant Entity_Id  := Component_Type (T);

   begin
      --  The check we install is

      --    constraint_error when
      --      Boolean (component_type'First)
      --        and then Boolean (component_type'Last)
      --        and then array_type'Length /= 0)

      --  We need the last guard because we don't want to raise CE for empty
      --  arrays since no out of range values result (Empty arrays with a
      --  component type of True .. True -- very useful -- even the ACATS
      --  does not test that marginal case!).

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition =>
            Make_And_Then (Loc,
              Left_Opnd =>
                Make_And_Then (Loc,
                  Left_Opnd =>
                    Convert_To (Standard_Boolean,
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (CT, Loc),
                        Attribute_Name => Name_First)),

                  Right_Opnd =>
                    Convert_To (Standard_Boolean,
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (CT, Loc),
                        Attribute_Name => Name_Last))),

              Right_Opnd => Make_Non_Empty_Check (Loc, Right_Opnd (N))),
          Reason => CE_Range_Check_Failed));
   end Silly_Boolean_Array_Xor_Test;

   --------------------------
   -- Target_Has_Fixed_Ops --
   --------------------------

   Integer_Sized_Small : Ureal;
   --  Set to 2.0 ** -(Integer'Size - 1) the first time that this function is
   --  called (we don't want to compute it more than once!)

   Long_Integer_Sized_Small : Ureal;
   --  Set to 2.0 ** -(Long_Integer'Size - 1) the first time that this function
   --  is called (we don't want to compute it more than once)

   First_Time_For_THFO : Boolean := True;
   --  Set to False after first call (if Fractional_Fixed_Ops_On_Target)

   function Target_Has_Fixed_Ops
     (Left_Typ   : Entity_Id;
      Right_Typ  : Entity_Id;
      Result_Typ : Entity_Id) return Boolean
   is
      function Is_Fractional_Type (Typ : Entity_Id) return Boolean;
      --  Return True if the given type is a fixed-point type with a small
      --  value equal to 2 ** (-(T'Object_Size - 1)) and whose values have
      --  an absolute value less than 1.0. This is currently limited to
      --  fixed-point types that map to Integer or Long_Integer.

      ------------------------
      -- Is_Fractional_Type --
      ------------------------

      function Is_Fractional_Type (Typ : Entity_Id) return Boolean is
      begin
         if Esize (Typ) = Standard_Integer_Size then
            return Small_Value (Typ) = Integer_Sized_Small;

         elsif Esize (Typ) = Standard_Long_Integer_Size then
            return Small_Value (Typ) = Long_Integer_Sized_Small;

         else
            return False;
         end if;
      end Is_Fractional_Type;

   --  Start of processing for Target_Has_Fixed_Ops

   begin
      --  Return False if Fractional_Fixed_Ops_On_Target is false

      if not Fractional_Fixed_Ops_On_Target then
         return False;
      end if;

      --  Here the target has Fractional_Fixed_Ops, if first time, compute
      --  standard constants used by Is_Fractional_Type.

      if First_Time_For_THFO then
         First_Time_For_THFO := False;

         Integer_Sized_Small :=
           UR_From_Components
             (Num   => Uint_1,
              Den   => UI_From_Int (Standard_Integer_Size - 1),
              Rbase => 2);

         Long_Integer_Sized_Small :=
           UR_From_Components
             (Num   => Uint_1,
              Den   => UI_From_Int (Standard_Long_Integer_Size - 1),
              Rbase => 2);
      end if;

      --  Return True if target supports fixed-by-fixed multiply/divide for
      --  fractional fixed-point types (see Is_Fractional_Type) and the operand
      --  and result types are equivalent fractional types.

      return Is_Fractional_Type (Base_Type (Left_Typ))
        and then Is_Fractional_Type (Base_Type (Right_Typ))
        and then Is_Fractional_Type (Base_Type (Result_Typ))
        and then Esize (Left_Typ) = Esize (Right_Typ)
        and then Esize (Left_Typ) = Esize (Result_Typ);
   end Target_Has_Fixed_Ops;

   ------------------------------------------
   -- Type_May_Have_Bit_Aligned_Components --
   ------------------------------------------

   function Type_May_Have_Bit_Aligned_Components
     (Typ : Entity_Id) return Boolean
   is
   begin
      --  Array type, check component type

      if Is_Array_Type (Typ) then
         return
           Type_May_Have_Bit_Aligned_Components (Component_Type (Typ));

      --  Record type, check components

      elsif Is_Record_Type (Typ) then
         declare
            E : Entity_Id;

         begin
            E := First_Component_Or_Discriminant (Typ);
            while Present (E) loop
               if Component_May_Be_Bit_Aligned (E)
                 or else Type_May_Have_Bit_Aligned_Components (Etype (E))
               then
                  return True;
               end if;

               Next_Component_Or_Discriminant (E);
            end loop;

            return False;
         end;

      --  Type other than array or record is always OK

      else
         return False;
      end if;
   end Type_May_Have_Bit_Aligned_Components;

   ----------------------------------
   -- Within_Case_Or_If_Expression --
   ----------------------------------

   function Within_Case_Or_If_Expression (N : Node_Id) return Boolean is
      Par : Node_Id;

   begin
      --  Locate an enclosing case or if expression. Note that these constructs
      --  can be expanded into Expression_With_Actions, hence the test of the
      --  original node.

      Par := Parent (N);
      while Present (Par) loop
         if Nkind_In (Original_Node (Par), N_Case_Expression,
                                           N_If_Expression)
         then
            return True;

         --  Prevent the search from going too far

         elsif Is_Body_Or_Package_Declaration (Par) then
            return False;
         end if;

         Par := Parent (Par);
      end loop;

      return False;
   end Within_Case_Or_If_Expression;

   ----------------------------
   -- Wrap_Cleanup_Procedure --
   ----------------------------

   procedure Wrap_Cleanup_Procedure (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Stseq : constant Node_Id    := Handled_Statement_Sequence (N);
      Stmts : constant List_Id    := Statements (Stseq);

   begin
      if Abort_Allowed then
         Prepend_To (Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));
         Append_To  (Stmts, Build_Runtime_Call (Loc, RE_Abort_Undefer));
      end if;
   end Wrap_Cleanup_Procedure;

end Exp_Util;
