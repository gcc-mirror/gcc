------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
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
with Exp_Ch11; use Exp_Ch11;
with Ghost;    use Ghost;
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
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
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

with GNAT.HTable; use GNAT.HTable;

package body Exp_Util is

   ---------------------------------------------------------
   -- Handling of inherited class-wide pre/postconditions --
   ---------------------------------------------------------

   --  Following AI12-0113, the expression for a class-wide condition is
   --  transformed for a subprogram that inherits it, by replacing calls
   --  to primitive operations of the original controlling type into the
   --  corresponding overriding operations of the derived type. The following
   --  hash table manages this mapping, and is expanded on demand whenever
   --  such inherited expression needs to be constructed.

   --  The mapping is also used to check whether an inherited operation has
   --  a condition that depends on overridden operations. For such an
   --  operation we must create a wrapper that is then treated as a normal
   --  overriding. In SPARK mode such operations are illegal.

   --  For a given root type there may be several type extensions with their
   --  own overriding operations, so at various times a given operation of
   --  the root will be mapped into different overridings. The root type is
   --  also mapped into the current type extension to indicate that its
   --  operations are mapped into the overriding operations of that current
   --  type extension.

   --  The contents of the map are as follows:

   --    Key                                Value

   --    Discriminant (Entity_Id)           Discriminant (Entity_Id)
   --    Discriminant (Entity_Id)           Non-discriminant name (Entity_Id)
   --    Discriminant (Entity_Id)           Expression (Node_Id)
   --    Primitive subprogram (Entity_Id)   Primitive subprogram (Entity_Id)
   --    Type (Entity_Id)                   Type (Entity_Id)

   Type_Map_Size : constant := 511;

   subtype Type_Map_Header is Integer range 0 .. Type_Map_Size - 1;
   function Type_Map_Hash (Id : Entity_Id) return Type_Map_Header;

   package Type_Map is new GNAT.HTable.Simple_HTable
     (Header_Num => Type_Map_Header,
      Key        => Entity_Id,
      Element    => Node_Or_Entity_Id,
      No_element => Empty,
      Hash       => Type_Map_Hash,
      Equal      => "=");

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

   procedure Evaluate_Slice_Bounds (Slice : Node_Id);
   --  Force evaluation of bounds of a slice, which may be given by a range
   --  or by a subtype indication with or without a constraint.

   function Find_DIC_Type (Typ : Entity_Id) return Entity_Id;
   --  Subsidiary to all Build_DIC_Procedure_xxx routines. Find the type which
   --  defines the Default_Initial_Condition pragma of type Typ. This is either
   --  Typ itself or a parent type when the pragma is inherited.

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

         --  Check for cases of appearing in the prefix of a construct where we
         --  don't need atomic synchronization for this kind of usage.

         when
            --  Nothing to do if we are the prefix of an attribute, since we
            --  do not want an atomic sync operation for things like 'Size.

              N_Attribute_Reference

            --  The N_Reference node is like an attribute

            | N_Reference

            --  Nothing to do for a reference to a component (or components)
            --  of a composite object. Only reads and updates of the object
            --  as a whole require atomic synchronization (RM C.6 (15)).

            | N_Indexed_Component
            | N_Selected_Component
            | N_Slice
         =>
            --  For all the above cases, nothing to do if we are the prefix

            if Prefix (Parent (N)) = N then
               return;
            end if;

         when others =>
            null;
      end case;

      --  Nothing to do for the identifier in an object renaming declaration,
      --  the renaming itself does not need atomic synchronization.

      if Nkind (Parent (N)) = N_Object_Renaming_Declaration then
         return;
      end if;

      --  Go ahead and set the flag

      Set_Atomic_Sync_Required (N);

      --  Generate info message if requested

      if Warn_On_Atomic_Synchronization then
         case Nkind (N) is
            when N_Identifier =>
               Msg_Node := N;

            when N_Expanded_Name
               | N_Selected_Component
            =>
               Msg_Node := Selector_Name (N);

            when N_Explicit_Dereference
               | N_Indexed_Component
            =>
               Msg_Node := Empty;

            when others =>
               pragma Assert (False);
               return;
         end case;

         if Present (Msg_Node) then
            Error_Msg_N
              ("info: atomic synchronization set for &?N?", Msg_Node);
         else
            Error_Msg_N
              ("info: atomic synchronization set?N?", N);
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
            --  must be Standard.Boolean, to the desired type. Use the base
            --  type to prevent spurious constraint checks that are extraneous
            --  to the transformation. The type and its base have the same
            --  representation, standard or otherwise.

            else
               Set_Analyzed (N);
               Rewrite (N, Convert_To (Base_Type (T), N));
               Analyze_And_Resolve (N, Base_Type (T));
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
      function Find_Object (E : Node_Id) return Node_Id;
      --  Given an arbitrary expression of an allocator, try to find an object
      --  reference in it, otherwise return the original expression.

      function Is_Allocate_Deallocate_Proc (Subp : Entity_Id) return Boolean;
      --  Determine whether subprogram Subp denotes a custom allocate or
      --  deallocate.

      -----------------
      -- Find_Object --
      -----------------

      function Find_Object (E : Node_Id) return Node_Id is
         Expr : Node_Id;

      begin
         pragma Assert (Is_Allocate);

         Expr := E;
         loop
            if Nkind (Expr) = N_Explicit_Dereference then
               Expr := Prefix (Expr);

            elsif Nkind (Expr) = N_Qualified_Expression then
               Expr := Expression (Expr);

            elsif Nkind (Expr) = N_Unchecked_Type_Conversion then

               --  When interface class-wide types are involved in allocation,
               --  the expander introduces several levels of address arithmetic
               --  to perform dispatch table displacement. In this scenario the
               --  object appears as:

               --    Tag_Ptr (Base_Address (<object>'Address))

               --  Detect this case and utilize the whole expression as the
               --  "object" since it now points to the proper dispatch table.

               if Is_RTE (Etype (Expr), RE_Tag_Ptr) then
                  exit;

               --  Continue to strip the object

               else
                  Expr := Expression (Expr);
               end if;

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

      --  Local variables

      Desig_Typ    : Entity_Id;
      Expr         : Node_Id;
      Needs_Fin    : Boolean;
      Pool_Id      : Entity_Id;
      Proc_To_Call : Node_Id := Empty;
      Ptr_Typ      : Entity_Id;

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

      --  Optimize the case where we are using the default Global_Pool_Object,
      --  and we don't need the heavy finalization machinery.

      elsif Pool_Id = RTE (RE_Global_Pool_Object)
        and then not Needs_Finalization (Desig_Typ)
      then
         return;

      --  Do not replicate the machinery if the allocator / free has already
      --  been expanded and has a custom Allocate / Deallocate.

      elsif Present (Proc_To_Call)
        and then Is_Allocate_Deallocate_Proc (Proc_To_Call)
      then
         return;
      end if;

      --  Finalization actions are required when the object to be allocated or
      --  deallocated needs these actions and the associated access type is not
      --  subject to pragma No_Heap_Finalization.

      Needs_Fin :=
        Needs_Finalization (Desig_Typ)
          and then not No_Heap_Finalization (Ptr_Typ);

      if Needs_Fin then

         --  Certain run-time configurations and targets do not provide support
         --  for controlled types.

         if Restriction_Active (No_Finalization) then
            return;

         --  Do nothing if the access type may never allocate / deallocate
         --  objects.

         elsif No_Pool_Assigned (Ptr_Typ) then
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

         Actuals := New_List (New_Occurrence_Of (Pool_Id, Loc));

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

            if Needs_Fin then
               Fin_Mas_Id  := Finalization_Master (Ptr_Typ);
               Fin_Mas_Act := New_Occurrence_Of (Fin_Mas_Id, Loc);

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

            if Needs_Fin and then not CodePeer_Mode then
               Fin_Addr_Id := Finalize_Address (Desig_Typ);
               pragma Assert (Present (Fin_Addr_Id));

               Append_To (Actuals,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Fin_Addr_Id, Loc),
                   Attribute_Name => Name_Unrestricted_Access));
            else
               Append_To (Actuals, Make_Null (Loc));
            end if;
         end if;

         --  e) Address
         --  f) Storage_Size
         --  g) Alignment

         Append_To (Actuals, New_Occurrence_Of (Addr_Id, Loc));
         Append_To (Actuals, New_Occurrence_Of (Size_Id, Loc));

         if Is_Allocate or else not Is_Class_Wide_Type (Desig_Typ) then
            Append_To (Actuals, New_Occurrence_Of (Alig_Id, Loc));

         --  For deallocation of class-wide types we obtain the value of
         --  alignment from the Type Specific Record of the deallocated object.
         --  This is needed because the frontend expansion of class-wide types
         --  into equivalent types confuses the back end.

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

         if Needs_Fin then
            Is_Controlled : declare
               Flag_Id   : constant Entity_Id := Make_Temporary (Loc, 'F');
               Flag_Expr : Node_Id;
               Param     : Node_Id;
               Pref      : Node_Id;
               Temp      : Node_Id;

            begin
               if Is_Allocate then
                  Temp := Find_Object (Expression (Expr));
               else
                  Temp := Expr;
               end if;

               --  Processing for allocations where the expression is a subtype
               --  indication.

               if Is_Allocate
                 and then Is_Entity_Name (Temp)
                 and then Is_Type (Entity (Temp))
               then
                  Flag_Expr :=
                    New_Occurrence_Of
                      (Boolean_Literals
                         (Needs_Finalization (Entity (Temp))), Loc);

               --  The allocation / deallocation of a class-wide object relies
               --  on a runtime check to determine whether the object is truly
               --  controlled or not. Depending on this check, the finalization
               --  machinery will request or reclaim extra storage reserved for
               --  a list header.

               elsif Is_Class_Wide_Type (Desig_Typ) then

                  --  Detect a special case where interface class-wide types
                  --  are involved as the object appears as:

                  --    Tag_Ptr (Base_Address (<object>'Address))

                  --  The expression already yields the proper tag, generate:

                  --    Temp.all

                  if Is_RTE (Etype (Temp), RE_Tag_Ptr) then
                     Param :=
                       Make_Explicit_Dereference (Loc,
                         Prefix => Relocate_Node (Temp));

                  --  In the default case, obtain the tag of the object about
                  --  to be allocated / deallocated. Generate:

                  --    Temp'Tag

                  --  If the object is an unchecked conversion (typically to
                  --  an access to class-wide type), we must preserve the
                  --  conversion to ensure that the object is seen as tagged
                  --  in the code that follows.

                  else
                     Pref := Temp;

                     if Nkind (Parent (Pref)) = N_Unchecked_Type_Conversion
                     then
                        Pref := Parent (Pref);
                     end if;

                     Param :=
                       Make_Attribute_Reference (Loc,
                         Prefix         => Relocate_Node (Pref),
                         Attribute_Name => Name_Tag);
                  end if;

                  --  Generate:
                  --    Needs_Finalization (<Param>)

                  Flag_Expr :=
                    Make_Function_Call (Loc,
                      Name                   =>
                        New_Occurrence_Of (RTE (RE_Needs_Finalization), Loc),
                      Parameter_Associations => New_List (Param));

               --  Processing for generic actuals

               elsif Is_Generic_Actual_Type (Desig_Typ) then
                  Flag_Expr :=
                    New_Occurrence_Of (Boolean_Literals
                      (Needs_Finalization (Base_Type (Desig_Typ))), Loc);

               --  The object does not require any specialized checks, it is
               --  known to be controlled.

               else
                  Flag_Expr := New_Occurrence_Of (Standard_True, Loc);
               end if;

               --  Create the temporary which represents the finalization state
               --  of the expression. Generate:
               --
               --    F : constant Boolean := <Flag_Expr>;

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Flag_Id,
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Boolean, Loc),
                    Expression          => Flag_Expr));

               Append_To (Actuals, New_Occurrence_Of (Flag_Id, Loc));
            end Is_Controlled;

         --  The object is not controlled

         else
            Append_To (Actuals, New_Occurrence_Of (Standard_False, Loc));
         end if;

         --  i) On_Subpool

         if Is_Allocate then
            Append_To (Actuals,
              New_Occurrence_Of (Boolean_Literals (Present (Subpool)), Loc));
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
             Specification              =>

               --  procedure Pnn

               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name       => Proc_Id,
                 Parameter_Specifications => New_List (

                  --  P : Root_Storage_Pool

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Make_Temporary (Loc, 'P'),
                     Parameter_Type      =>
                       New_Occurrence_Of (RTE (RE_Root_Storage_Pool), Loc)),

                  --  A : [out] Address

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Addr_Id,
                     Out_Present         => Is_Allocate,
                     Parameter_Type      =>
                       New_Occurrence_Of (RTE (RE_Address), Loc)),

                  --  S : Storage_Count

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Size_Id,
                     Parameter_Type      =>
                       New_Occurrence_Of (RTE (RE_Storage_Count), Loc)),

                  --  L : Storage_Count

                   Make_Parameter_Specification (Loc,
                     Defining_Identifier => Alig_Id,
                     Parameter_Type      =>
                       New_Occurrence_Of (RTE (RE_Storage_Count), Loc)))),

             Declarations               => No_List,

             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Procedure_Call_Statement (Loc,
                     Name                   =>
                       New_Occurrence_Of (Proc_To_Call, Loc),
                     Parameter_Associations => Actuals)))),
           Suppress => All_Checks);

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

   -------------------------------
   -- Build_Abort_Undefer_Block --
   -------------------------------

   function Build_Abort_Undefer_Block
     (Loc     : Source_Ptr;
      Stmts   : List_Id;
      Context : Node_Id) return Node_Id
   is
      Exceptions_OK : constant Boolean :=
                        not Restriction_Active (No_Exception_Propagation);

      AUD    : Entity_Id;
      Blk    : Node_Id;
      Blk_Id : Entity_Id;
      HSS    : Node_Id;

   begin
      --  The block should be generated only when undeferring abort in the
      --  context of a potential exception.

      pragma Assert (Abort_Allowed and Exceptions_OK);

      --  Generate:
      --    begin
      --       <Stmts>
      --    at end
      --       Abort_Undefer_Direct;
      --    end;

      AUD := RTE (RE_Abort_Undefer_Direct);

      HSS :=
        Make_Handled_Sequence_Of_Statements (Loc,
          Statements  => Stmts,
          At_End_Proc => New_Occurrence_Of (AUD, Loc));

      Blk :=
        Make_Block_Statement (Loc,
          Handled_Statement_Sequence => HSS);
      Set_Is_Abort_Block (Blk);

      Add_Block_Identifier  (Blk, Blk_Id);
      Expand_At_End_Handler (HSS, Blk_Id);

      --  Present the Abort_Undefer_Direct function to the back end to inline
      --  the call to the routine.

      Add_Inlined_Body (AUD, Context);

      return Blk;
   end Build_Abort_Undefer_Block;

   ---------------------------------
   -- Build_Class_Wide_Expression --
   ---------------------------------

   procedure Build_Class_Wide_Expression
     (Prag          : Node_Id;
      Subp          : Entity_Id;
      Par_Subp      : Entity_Id;
      Adjust_Sloc   : Boolean;
      Needs_Wrapper : out Boolean)
   is
      function Replace_Entity (N : Node_Id) return Traverse_Result;
      --  Replace reference to formal of inherited operation or to primitive
      --  operation of root type, with corresponding entity for derived type,
      --  when constructing the class-wide condition of an overriding
      --  subprogram.

      --------------------
      -- Replace_Entity --
      --------------------

      function Replace_Entity (N : Node_Id) return Traverse_Result is
         New_E : Entity_Id;

      begin
         if Adjust_Sloc then
            Adjust_Inherited_Pragma_Sloc (N);
         end if;

         if Nkind (N) = N_Identifier
           and then Present (Entity (N))
           and then
             (Is_Formal (Entity (N)) or else Is_Subprogram (Entity (N)))
           and then
             (Nkind (Parent (N)) /= N_Attribute_Reference
               or else Attribute_Name (Parent (N)) /= Name_Class)
         then
            --  The replacement does not apply to dispatching calls within the
            --  condition, but only to calls whose static tag is that of the
            --  parent type.

            if Is_Subprogram (Entity (N))
              and then Nkind (Parent (N)) = N_Function_Call
              and then Present (Controlling_Argument (Parent (N)))
            then
               return OK;
            end if;

            --  Determine whether entity has a renaming

            New_E := Type_Map.Get (Entity (N));

            if Present (New_E) then
               Rewrite (N, New_Occurrence_Of (New_E, Sloc (N)));

               --  If the entity is an overridden primitive and we are not
               --  in GNATprove mode, we must build a wrapper for the current
               --  inherited operation. If the reference is the prefix of an
               --  attribute such as 'Result (or others ???) there is no need
               --  for a wrapper: the condition is just rewritten in terms of
               --  the inherited subprogram.

               if Is_Subprogram (New_E)
                  and then Nkind (Parent (N)) /= N_Attribute_Reference
                  and then not GNATprove_Mode
               then
                  Needs_Wrapper := True;
               end if;
            end if;

            --  Check that there are no calls left to abstract operations if
            --  the current subprogram is not abstract.

            if Nkind (Parent (N)) = N_Function_Call
              and then N = Name (Parent (N))
            then
               if not Is_Abstract_Subprogram (Subp)
                 and then Is_Abstract_Subprogram (Entity (N))
               then
                  Error_Msg_Sloc   := Sloc (Current_Scope);
                  Error_Msg_Node_2 := Subp;
                  if Comes_From_Source (Subp) then
                     Error_Msg_NE
                       ("cannot call abstract subprogram & in inherited "
                        & "condition for&#", Subp, Entity (N));
                  else
                     Error_Msg_NE
                       ("cannot call abstract subprogram & in inherited "
                        & "condition for inherited&#", Subp, Entity (N));
                  end if;

               --  In SPARK mode, reject an inherited condition for an
               --  inherited operation if it contains a call to an overriding
               --  operation, because this implies that the pre/postconditions
               --  of the inherited operation have changed silently.

               elsif SPARK_Mode = On
                 and then Warn_On_Suspicious_Contract
                 and then Present (Alias (Subp))
                 and then Present (New_E)
                 and then Comes_From_Source (New_E)
               then
                  Error_Msg_N
                    ("cannot modify inherited condition (SPARK RM 6.1.1(1))",
                     Parent (Subp));
                  Error_Msg_Sloc   := Sloc (New_E);
                  Error_Msg_Node_2 := Subp;
                  Error_Msg_NE
                    ("\overriding of&# forces overriding of&",
                     Parent (Subp), New_E);
               end if;
            end if;

            --  Update type of function call node, which should be the same as
            --  the function's return type.

            if Is_Subprogram (Entity (N))
              and then Nkind (Parent (N)) = N_Function_Call
            then
               Set_Etype (Parent (N), Etype (Entity (N)));
            end if;

         --  The whole expression will be reanalyzed

         elsif Nkind (N) in N_Has_Etype then
            Set_Analyzed (N, False);
         end if;

         return OK;
      end Replace_Entity;

      procedure Replace_Condition_Entities is
        new Traverse_Proc (Replace_Entity);

      --  Local variables

      Par_Formal  : Entity_Id;
      Subp_Formal : Entity_Id;

   --  Start of processing for Build_Class_Wide_Expression

   begin
      Needs_Wrapper := False;

      --  Add mapping from old formals to new formals

      Par_Formal  := First_Formal (Par_Subp);
      Subp_Formal := First_Formal (Subp);

      while Present (Par_Formal) and then Present (Subp_Formal) loop
         Type_Map.Set (Par_Formal, Subp_Formal);
         Next_Formal (Par_Formal);
         Next_Formal (Subp_Formal);
      end loop;

      Replace_Condition_Entities (Prag);
   end Build_Class_Wide_Expression;

   --------------------
   -- Build_DIC_Call --
   --------------------

   function Build_DIC_Call
     (Loc    : Source_Ptr;
      Obj_Id : Entity_Id;
      Typ    : Entity_Id) return Node_Id
   is
      Proc_Id    : constant Entity_Id := DIC_Procedure (Typ);
      Formal_Typ : constant Entity_Id := Etype (First_Formal (Proc_Id));

   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Id, Loc),
          Parameter_Associations => New_List (
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark => New_Occurrence_Of (Formal_Typ, Loc),
              Expression   => New_Occurrence_Of (Obj_Id, Loc))));
   end Build_DIC_Call;

   ------------------------------
   -- Build_DIC_Procedure_Body --
   ------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_DIC_Procedure_Body
     (Typ        : Entity_Id;
      For_Freeze : Boolean := False)
   is
      procedure Add_DIC_Check
        (DIC_Prag : Node_Id;
         DIC_Expr : Node_Id;
         Stmts    : in out List_Id);
      --  Subsidiary to all Add_xxx_DIC routines. Add a runtime check to verify
      --  assertion expression DIC_Expr of pragma DIC_Prag. All generated code
      --  is added to list Stmts.

      procedure Add_Inherited_DIC
        (DIC_Prag  : Node_Id;
         Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id;
         Stmts     : in out List_Id);
      --  Add a runtime check to verify the assertion expression of inherited
      --  pragma DIC_Prag. Par_Typ is parent type, which is also the owner of
      --  the DIC pragma. Deriv_Typ is the derived type inheriting the DIC
      --  pragma. All generated code is added to list Stmts.

      procedure Add_Inherited_Tagged_DIC
        (DIC_Prag  : Node_Id;
         Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id;
         Stmts     : in out List_Id);
      --  Add a runtime check to verify assertion expression DIC_Expr of
      --  inherited pragma DIC_Prag. This routine applies class-wide pre- and
      --  postcondition-like runtime semantics to the check. Par_Typ is the
      --  parent type whose DIC pragma is being inherited. Deriv_Typ is the
      --  derived type inheriting the DIC pragma. All generated code is added
      --  to list Stmts.

      procedure Add_Own_DIC
        (DIC_Prag : Node_Id;
         DIC_Typ  : Entity_Id;
         Stmts    : in out List_Id);
      --  Add a runtime check to verify the assertion expression of pragma
      --  DIC_Prag. DIC_Typ is the owner of the DIC pragma. All generated code
      --  is added to list Stmts.

      -------------------
      -- Add_DIC_Check --
      -------------------

      procedure Add_DIC_Check
        (DIC_Prag : Node_Id;
         DIC_Expr : Node_Id;
         Stmts    : in out List_Id)
      is
         Loc : constant Source_Ptr := Sloc (DIC_Prag);
         Nam : constant Name_Id    := Original_Aspect_Pragma_Name (DIC_Prag);

      begin
         --  The DIC pragma is ignored, nothing left to do

         if Is_Ignored (DIC_Prag) then
            null;

         --  Otherwise the DIC expression must be checked at run time.
         --  Generate:

         --    pragma Check (<Nam>, <DIC_Expr>);

         else
            Append_New_To (Stmts,
              Make_Pragma (Loc,
                Pragma_Identifier            =>
                  Make_Identifier (Loc, Name_Check),

                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Loc,
                    Expression => Make_Identifier (Loc, Nam)),

                  Make_Pragma_Argument_Association (Loc,
                    Expression => DIC_Expr))));
         end if;
      end Add_DIC_Check;

      -----------------------
      -- Add_Inherited_DIC --
      -----------------------

      procedure Add_Inherited_DIC
        (DIC_Prag  : Node_Id;
         Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id;
         Stmts     : in out List_Id)
      is
         Deriv_Proc : constant Entity_Id  := DIC_Procedure (Deriv_Typ);
         Deriv_Obj  : constant Entity_Id  := First_Entity  (Deriv_Proc);
         Par_Proc   : constant Entity_Id  := DIC_Procedure (Par_Typ);
         Par_Obj    : constant Entity_Id  := First_Entity  (Par_Proc);
         Loc        : constant Source_Ptr := Sloc (DIC_Prag);

      begin
         pragma Assert (Present (Deriv_Proc) and then Present (Par_Proc));

         --  Verify the inherited DIC assertion expression by calling the DIC
         --  procedure of the parent type.

         --  Generate:
         --    <Par_Typ>DIC (Par_Typ (_object));

         Append_New_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name                   => New_Occurrence_Of (Par_Proc, Loc),
             Parameter_Associations => New_List (
               Convert_To
                 (Typ  => Etype (Par_Obj),
                  Expr => New_Occurrence_Of (Deriv_Obj, Loc)))));
      end Add_Inherited_DIC;

      ------------------------------
      -- Add_Inherited_Tagged_DIC --
      ------------------------------

      procedure Add_Inherited_Tagged_DIC
        (DIC_Prag  : Node_Id;
         Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id;
         Stmts     : in out List_Id)
      is
         Deriv_Proc : constant Entity_Id := DIC_Procedure (Deriv_Typ);
         DIC_Args   : constant List_Id   :=
                        Pragma_Argument_Associations (DIC_Prag);
         DIC_Arg    : constant Node_Id   := First (DIC_Args);
         DIC_Expr   : constant Node_Id   := Expression_Copy (DIC_Arg);
         Par_Proc   : constant Entity_Id := DIC_Procedure (Par_Typ);

         Expr : Node_Id;

      begin
         --  The processing of an inherited DIC assertion expression starts off
         --  with a copy of the original parent expression where all references
         --  to the parent type have already been replaced with references to
         --  the _object formal parameter of the parent type's DIC procedure.

         pragma Assert (Present (DIC_Expr));
         Expr := New_Copy_Tree (DIC_Expr);

         --  Perform the following substitutions:

         --    * Replace a reference to the _object parameter of the parent
         --      type's DIC procedure with a reference to the _object parameter
         --      of the derived types' DIC procedure.

         --    * Replace a reference to a discriminant of the parent type with
         --      a suitable value from the point of view of the derived type.

         --    * Replace a call to an overridden parent primitive with a call
         --      to the overriding derived type primitive.

         --    * Replace a call to an inherited parent primitive with a call to
         --      the internally-generated inherited derived type primitive.

         --  Note that primitives defined in the private part are automatically
         --  handled by the overriding/inheritance mechanism and do not require
         --  an extra replacement pass.

         pragma Assert (Present (Deriv_Proc) and then Present (Par_Proc));

         Replace_References
           (Expr      => Expr,
            Par_Typ   => Par_Typ,
            Deriv_Typ => Deriv_Typ,
            Par_Obj   => First_Formal (Par_Proc),
            Deriv_Obj => First_Formal (Deriv_Proc));

         --  Once the DIC assertion expression is fully processed, add a check
         --  to the statements of the DIC procedure.

         Add_DIC_Check
           (DIC_Prag => DIC_Prag,
            DIC_Expr => Expr,
            Stmts    => Stmts);
      end Add_Inherited_Tagged_DIC;

      -----------------
      -- Add_Own_DIC --
      -----------------

      procedure Add_Own_DIC
        (DIC_Prag : Node_Id;
         DIC_Typ  : Entity_Id;
         Stmts    : in out List_Id)
      is
         DIC_Args : constant List_Id   :=
                      Pragma_Argument_Associations (DIC_Prag);
         DIC_Arg  : constant Node_Id   := First (DIC_Args);
         DIC_Asp  : constant Node_Id   := Corresponding_Aspect (DIC_Prag);
         DIC_Expr : constant Node_Id   := Get_Pragma_Arg (DIC_Arg);
         DIC_Proc : constant Entity_Id := DIC_Procedure (DIC_Typ);
         Obj_Id   : constant Entity_Id := First_Formal (DIC_Proc);

         procedure Preanalyze_Own_DIC_For_ASIS;
         --  Preanalyze the original DIC expression of an aspect or a source
         --  pragma for ASIS.

         ---------------------------------
         -- Preanalyze_Own_DIC_For_ASIS --
         ---------------------------------

         procedure Preanalyze_Own_DIC_For_ASIS is
            Expr : Node_Id := Empty;

         begin
            --  The DIC pragma is a source construct, preanalyze the original
            --  expression of the pragma.

            if Comes_From_Source (DIC_Prag) then
               Expr := DIC_Expr;

            --  Otherwise preanalyze the expression of the corresponding aspect

            elsif Present (DIC_Asp) then
               Expr := Expression (DIC_Asp);
            end if;

            --  The expression must be subjected to the same substitutions as
            --  the copy used in the generation of the runtime check.

            if Present (Expr) then
               Replace_Type_References
                 (Expr   => Expr,
                  Typ    => DIC_Typ,
                  Obj_Id => Obj_Id);

               Preanalyze_Assert_Expression (Expr, Any_Boolean);
            end if;
         end Preanalyze_Own_DIC_For_ASIS;

         --  Local variables

         Typ_Decl : constant Node_Id := Declaration_Node (DIC_Typ);

         Expr : Node_Id;

      --  Start of processing for Add_Own_DIC

      begin
         Expr := New_Copy_Tree (DIC_Expr);

         --  Perform the following substitution:

         --    * Replace the current instance of DIC_Typ with a reference to
         --    the _object formal parameter of the DIC procedure.

         Replace_Type_References
           (Expr   => Expr,
            Typ    => DIC_Typ,
            Obj_Id => Obj_Id);

         --  Preanalyze the DIC expression to detect errors and at the same
         --  time capture the visibility of the proper package part.

         Set_Parent (Expr, Typ_Decl);
         Preanalyze_Assert_Expression (Expr, Any_Boolean);

         --  Save a copy of the expression with all replacements and analysis
         --  already taken place in case a derived type inherits the pragma.
         --  The copy will be used as the foundation of the derived type's own
         --  version of the DIC assertion expression.

         if Is_Tagged_Type (DIC_Typ) then
            Set_Expression_Copy (DIC_Arg, New_Copy_Tree (Expr));
         end if;

         --  If the pragma comes from an aspect specification, replace the
         --  saved expression because all type references must be substituted
         --  for the call to Preanalyze_Spec_Expression in Check_Aspect_At_xxx
         --  routines.

         if Present (DIC_Asp) then
            Set_Entity (Identifier (DIC_Asp), New_Copy_Tree (Expr));
         end if;

         --  Preanalyze the original DIC expression for ASIS

         if ASIS_Mode then
            Preanalyze_Own_DIC_For_ASIS;
         end if;

         --  Once the DIC assertion expression is fully processed, add a check
         --  to the statements of the DIC procedure.

         Add_DIC_Check
           (DIC_Prag => DIC_Prag,
            DIC_Expr => Expr,
            Stmts    => Stmts);
      end Add_Own_DIC;

      --  Local variables

      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM : constant Ghost_Mode_Type := Ghost_Mode;
      --  Save the Ghost mode to restore on exit

      DIC_Prag     : Node_Id;
      DIC_Typ      : Entity_Id;
      Dummy_1      : Entity_Id;
      Dummy_2      : Entity_Id;
      Proc_Body    : Node_Id;
      Proc_Body_Id : Entity_Id;
      Proc_Decl    : Node_Id;
      Proc_Id      : Entity_Id;
      Stmts        : List_Id := No_List;

      Build_Body : Boolean := False;
      --  Flag set when the type requires a DIC procedure body to be built

      Work_Typ : Entity_Id;
      --  The working type

   --  Start of processing for Build_DIC_Procedure_Body

   begin
      Work_Typ := Base_Type (Typ);

      --  Do not process class-wide types as these are Itypes, but lack a first
      --  subtype (see below).

      if Is_Class_Wide_Type (Work_Typ) then
         return;

      --  Do not process the underlying full view of a private type. There is
      --  no way to get back to the partial view, plus the body will be built
      --  by the full view or the base type.

      elsif Is_Underlying_Full_View (Work_Typ) then
         return;

      --  Use the first subtype when dealing with various base types

      elsif Is_Itype (Work_Typ) then
         Work_Typ := First_Subtype (Work_Typ);

      --  The input denotes the corresponding record type of a protected or a
      --  task type. Work with the concurrent type because the corresponding
      --  record type may not be visible to clients of the type.

      elsif Ekind (Work_Typ) = E_Record_Type
        and then Is_Concurrent_Record_Type (Work_Typ)
      then
         Work_Typ := Corresponding_Concurrent_Type (Work_Typ);
      end if;

      --  The working type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the DIC procedure is properly marked as Ghost.

      Set_Ghost_Mode (Work_Typ);

      --  The working type must be either define a DIC pragma of its own or
      --  inherit one from a parent type.

      pragma Assert (Has_DIC (Work_Typ));

      --  Recover the type which defines the DIC pragma. This is either the
      --  working type itself or a parent type when the pragma is inherited.

      DIC_Typ := Find_DIC_Type (Work_Typ);
      pragma Assert (Present (DIC_Typ));

      DIC_Prag := Get_Pragma (DIC_Typ, Pragma_Default_Initial_Condition);
      pragma Assert (Present (DIC_Prag));

      --  Nothing to do if pragma DIC appears without an argument or its sole
      --  argument is "null".

      if not Is_Verifiable_DIC_Pragma (DIC_Prag) then
         goto Leave;
      end if;

      --  The working type may lack a DIC procedure declaration. This may be
      --  due to several reasons:

      --    * The working type's own DIC pragma does not contain a verifiable
      --      assertion expression. In this case there is no need to build a
      --      DIC procedure because there is nothing to check.

      --    * The working type derives from a parent type. In this case a DIC
      --      procedure should be built only when the inherited DIC pragma has
      --      a verifiable assertion expression.

      Proc_Id := DIC_Procedure (Work_Typ);

      --  Build a DIC procedure declaration when the working type derives from
      --  a parent type.

      if No (Proc_Id) then
         Build_DIC_Procedure_Declaration (Work_Typ);
         Proc_Id := DIC_Procedure (Work_Typ);
      end if;

      --  At this point there should be a DIC procedure declaration

      pragma Assert (Present (Proc_Id));
      Proc_Decl := Unit_Declaration_Node (Proc_Id);

      --  Nothing to do if the DIC procedure already has a body

      if Present (Corresponding_Body (Proc_Decl)) then
         goto Leave;
      end if;

      --  Emulate the environment of the DIC procedure by installing its scope
      --  and formal parameters.

      Push_Scope (Proc_Id);
      Install_Formals (Proc_Id);

      --  The working type defines its own DIC pragma. Replace the current
      --  instance of the working type with the formal of the DIC procedure.
      --  Note that there is no need to consider inherited DIC pragmas from
      --  parent types because the working type's DIC pragma "hides" all
      --  inherited DIC pragmas.

      if Has_Own_DIC (Work_Typ) then
         pragma Assert (DIC_Typ = Work_Typ);

         Add_Own_DIC
           (DIC_Prag => DIC_Prag,
            DIC_Typ  => DIC_Typ,
            Stmts    => Stmts);

         Build_Body := True;

      --  Otherwise the working type inherits a DIC pragma from a parent type.
      --  This processing is carried out when the type is frozen because the
      --  state of all parent discriminants is known at that point. Note that
      --  it is semantically sound to delay the creation of the DIC procedure
      --  body till the freeze point. If the type has a DIC pragma of its own,
      --  then the DIC procedure body would have already been constructed at
      --  the end of the visible declarations and all parent DIC pragmas are
      --  effectively "hidden" and irrelevant.

      elsif For_Freeze then
         pragma Assert (Has_Inherited_DIC (Work_Typ));
         pragma Assert (DIC_Typ /= Work_Typ);

         --  The working type is tagged. The verification of the assertion
         --  expression is subject to the same semantics as class-wide pre-
         --  and postconditions.

         if Is_Tagged_Type (Work_Typ) then
            Add_Inherited_Tagged_DIC
              (DIC_Prag  => DIC_Prag,
               Par_Typ   => DIC_Typ,
               Deriv_Typ => Work_Typ,
               Stmts     => Stmts);

         --  Otherwise the working type is not tagged. Verify the assertion
         --  expression of the inherited DIC pragma by directly calling the
         --  DIC procedure of the parent type.

         else
            Add_Inherited_DIC
              (DIC_Prag  => DIC_Prag,
               Par_Typ   => DIC_Typ,
               Deriv_Typ => Work_Typ,
               Stmts     => Stmts);
         end if;

         Build_Body := True;
      end if;

      End_Scope;

      if Build_Body then

         --  Produce an empty completing body in the following cases:
         --    * Assertions are disabled
         --    * The DIC Assertion_Policy is Ignore
         --    * Pragma DIC appears without an argument
         --    * Pragma DIC appears with argument "null"

         if No (Stmts) then
            Stmts := New_List (Make_Null_Statement (Loc));
         end if;

         --  Generate:
         --    procedure <Work_Typ>DIC (_object : <Work_Typ>) is
         --    begin
         --       <Stmts>
         --    end <Work_Typ>DIC;

         Proc_Body :=
           Make_Subprogram_Body (Loc,
             Specification                =>
               Copy_Subprogram_Spec (Parent (Proc_Id)),
             Declarations                 => Empty_List,
               Handled_Statement_Sequence =>
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => Stmts));
         Proc_Body_Id := Defining_Entity (Proc_Body);

         --  Perform minor decoration in case the body is not analyzed

         Set_Ekind (Proc_Body_Id, E_Subprogram_Body);
         Set_Etype (Proc_Body_Id, Standard_Void_Type);
         Set_Scope (Proc_Body_Id, Current_Scope);

         --  Link both spec and body to avoid generating duplicates

         Set_Corresponding_Body (Proc_Decl, Proc_Body_Id);
         Set_Corresponding_Spec (Proc_Body, Proc_Id);

         --  The body should not be inserted into the tree when the context
         --  is ASIS or a generic unit because it is not part of the template.
         --  Note that the body must still be generated in order to resolve the
         --  DIC assertion expression.

         if ASIS_Mode or Inside_A_Generic then
            null;

         --  Semi-insert the body into the tree for GNATprove by setting its
         --  Parent field. This allows for proper upstream tree traversals.

         elsif GNATprove_Mode then
            Set_Parent (Proc_Body, Parent (Declaration_Node (Work_Typ)));

         --  Otherwise the body is part of the freezing actions of the working
         --  type.

         else
            Append_Freeze_Action (Work_Typ, Proc_Body);
         end if;
      end if;

   <<Leave>>
      Restore_Ghost_Mode (Saved_GM);
   end Build_DIC_Procedure_Body;

   -------------------------------------
   -- Build_DIC_Procedure_Declaration --
   -------------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_DIC_Procedure_Declaration (Typ : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM : constant Ghost_Mode_Type := Ghost_Mode;
      --  Save the Ghost mode to restore on exit

      DIC_Prag  : Node_Id;
      DIC_Typ   : Entity_Id;
      Proc_Decl : Node_Id;
      Proc_Id   : Entity_Id;
      Typ_Decl  : Node_Id;

      CRec_Typ : Entity_Id;
      --  The corresponding record type of Full_Typ

      Full_Base : Entity_Id;
      --  The base type of Full_Typ

      Full_Typ : Entity_Id;
      --  The full view of working type

      Obj_Id : Entity_Id;
      --  The _object formal parameter of the DIC procedure

      Priv_Typ : Entity_Id;
      --  The partial view of working type

      Work_Typ : Entity_Id;
      --  The working type

   begin
      Work_Typ := Base_Type (Typ);

      --  Do not process class-wide types as these are Itypes, but lack a first
      --  subtype (see below).

      if Is_Class_Wide_Type (Work_Typ) then
         return;

      --  Do not process the underlying full view of a private type. There is
      --  no way to get back to the partial view, plus the body will be built
      --  by the full view or the base type.

      elsif Is_Underlying_Full_View (Work_Typ) then
         return;

      --  Use the first subtype when dealing with various base types

      elsif Is_Itype (Work_Typ) then
         Work_Typ := First_Subtype (Work_Typ);

      --  The input denotes the corresponding record type of a protected or a
      --  task type. Work with the concurrent type because the corresponding
      --  record type may not be visible to clients of the type.

      elsif Ekind (Work_Typ) = E_Record_Type
        and then Is_Concurrent_Record_Type (Work_Typ)
      then
         Work_Typ := Corresponding_Concurrent_Type (Work_Typ);
      end if;

      --  The working type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the DIC procedure is properly marked as Ghost.

      Set_Ghost_Mode (Work_Typ);

      --  The type must be either subject to a DIC pragma or inherit one from a
      --  parent type.

      pragma Assert (Has_DIC (Work_Typ));

      --  Recover the type which defines the DIC pragma. This is either the
      --  working type itself or a parent type when the pragma is inherited.

      DIC_Typ := Find_DIC_Type (Work_Typ);
      pragma Assert (Present (DIC_Typ));

      DIC_Prag := Get_Pragma (DIC_Typ, Pragma_Default_Initial_Condition);
      pragma Assert (Present (DIC_Prag));

      --  Nothing to do if pragma DIC appears without an argument or its sole
      --  argument is "null".

      if not Is_Verifiable_DIC_Pragma (DIC_Prag) then
         goto Leave;

      --  Nothing to do if the type already has a DIC procedure

      elsif Present (DIC_Procedure (Work_Typ)) then
         goto Leave;
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name (Chars (Work_Typ), "Default_Initial_Condition"));

      --  Perform minor decoration in case the declaration is not analyzed

      Set_Ekind (Proc_Id, E_Procedure);
      Set_Etype (Proc_Id, Standard_Void_Type);
      Set_Scope (Proc_Id, Current_Scope);

      Set_Is_DIC_Procedure (Proc_Id);
      Set_DIC_Procedure (Work_Typ, Proc_Id);

      --  The DIC procedure requires debug info when the assertion expression
      --  is subject to Source Coverage Obligations.

      if Opt.Generate_SCO then
         Set_Needs_Debug_Info (Proc_Id);
      end if;

      --  Obtain all views of the input type

      Get_Views (Work_Typ, Priv_Typ, Full_Typ, Full_Base, CRec_Typ);

      --  Associate the DIC procedure and various relevant flags with all views

      Propagate_DIC_Attributes (Priv_Typ,  From_Typ => Work_Typ);
      Propagate_DIC_Attributes (Full_Typ,  From_Typ => Work_Typ);
      Propagate_DIC_Attributes (Full_Base, From_Typ => Work_Typ);
      Propagate_DIC_Attributes (CRec_Typ,  From_Typ => Work_Typ);

      --  The declaration of the DIC procedure must be inserted after the
      --  declaration of the partial view as this allows for proper external
      --  visibility.

      if Present (Priv_Typ) then
         Typ_Decl := Declaration_Node (Priv_Typ);

      --  Derived types with the full view as parent do not have a partial
      --  view. Insert the DIC procedure after the derived type.

      else
         Typ_Decl := Declaration_Node (Full_Typ);
      end if;

      --  The type should have a declarative node

      pragma Assert (Present (Typ_Decl));

      --  Create the formal parameter which emulates the variable-like behavior
      --  of the type's current instance.

      Obj_Id := Make_Defining_Identifier (Loc, Chars => Name_uObject);

      --  Perform minor decoration in case the declaration is not analyzed

      Set_Ekind (Obj_Id, E_In_Parameter);
      Set_Etype (Obj_Id, Work_Typ);
      Set_Scope (Obj_Id, Proc_Id);

      Set_First_Entity (Proc_Id, Obj_Id);

      --  Generate:
      --    procedure <Work_Typ>DIC (_object : <Work_Typ>);

      Proc_Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Id,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => Obj_Id,
                  Parameter_Type      =>
                    New_Occurrence_Of (Work_Typ, Loc)))));

      --  The declaration should not be inserted into the tree when the context
      --  is ASIS or a generic unit because it is not part of the template.

      if ASIS_Mode or Inside_A_Generic then
         null;

      --  Semi-insert the declaration into the tree for GNATprove by setting
      --  its Parent field. This allows for proper upstream tree traversals.

      elsif GNATprove_Mode then
         Set_Parent (Proc_Decl, Parent (Typ_Decl));

      --  Otherwise insert the declaration

      else
         Insert_After_And_Analyze (Typ_Decl, Proc_Decl);
      end if;

   <<Leave>>
      Restore_Ghost_Mode (Saved_GM);
   end Build_DIC_Procedure_Declaration;

   ------------------------------------
   -- Build_Invariant_Procedure_Body --
   ------------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_Invariant_Procedure_Body
     (Typ               : Entity_Id;
      Partial_Invariant : Boolean := False)
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      Pragmas_Seen : Elist_Id := No_Elist;
      --  This list contains all invariant pragmas processed so far. The list
      --  is used to avoid generating redundant invariant checks.

      Produced_Check : Boolean := False;
      --  This flag tracks whether the type has produced at least one invariant
      --  check. The flag is used as a sanity check at the end of the routine.

      --  NOTE: most of the routines in Build_Invariant_Procedure_Body are
      --  intentionally unnested to avoid deep indentation of code.

      --  NOTE: all Add_xxx_Invariants routines are reactive. In other words
      --  they emit checks, loops (for arrays) and case statements (for record
      --  variant parts) only when there are invariants to verify. This keeps
      --  the body of the invariant procedure free of useless code.

      procedure Add_Array_Component_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id);
      --  Generate an invariant check for each component of array type T.
      --  Obj_Id denotes the entity of the _object formal parameter of the
      --  invariant procedure. All created checks are added to list Checks.

      procedure Add_Inherited_Invariants
        (T         : Entity_Id;
         Priv_Typ  : Entity_Id;
         Full_Typ  : Entity_Id;
         Obj_Id    : Entity_Id;
         Checks    : in out List_Id);
      --  Generate an invariant check for each inherited class-wide invariant
      --  coming from all parent types of type T. Priv_Typ and Full_Typ denote
      --  the partial and full view of the parent type. Obj_Id denotes the
      --  entity of the _object formal parameter of the invariant procedure.
      --  All created checks are added to list Checks.

      procedure Add_Interface_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id);
      --  Generate an invariant check for each inherited class-wide invariant
      --  coming from all interfaces implemented by type T. Obj_Id denotes the
      --  entity of the _object formal parameter of the invariant procedure.
      --  All created checks are added to list Checks.

      procedure Add_Invariant_Check
        (Prag      : Node_Id;
         Expr      : Node_Id;
         Checks    : in out List_Id;
         Inherited : Boolean := False);
      --  Subsidiary to all Add_xxx_Invariant routines. Add a runtime check to
      --  verify assertion expression Expr of pragma Prag. All generated code
      --  is added to list Checks. Flag Inherited should be set when the pragma
      --  is inherited from a parent or interface type.

      procedure Add_Own_Invariants
        (T         : Entity_Id;
         Obj_Id    : Entity_Id;
         Checks    : in out List_Id;
         Priv_Item : Node_Id := Empty);
      --  Generate an invariant check for each invariant found for type T.
      --  Obj_Id denotes the entity of the _object formal parameter of the
      --  invariant procedure. All created checks are added to list Checks.
      --  Priv_Item denotes the first rep item of the private type.

      procedure Add_Parent_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id);
      --  Generate an invariant check for each inherited class-wide invariant
      --  coming from all parent types of type T. Obj_Id denotes the entity of
      --  the _object formal parameter of the invariant procedure. All created
      --  checks are added to list Checks.

      procedure Add_Record_Component_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id);
      --  Generate an invariant check for each component of record type T.
      --  Obj_Id denotes the entity of the _object formal parameter of the
      --  invariant procedure. All created checks are added to list Checks.

      ------------------------------------
      -- Add_Array_Component_Invariants --
      ------------------------------------

      procedure Add_Array_Component_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id)
      is
         Comp_Typ : constant Entity_Id := Component_Type (T);
         Dims     : constant Pos       := Number_Dimensions (T);

         procedure Process_Array_Component
           (Indices     : List_Id;
            Comp_Checks : in out List_Id);
         --  Generate an invariant check for an array component identified by
         --  the indices in list Indices. All created checks are added to list
         --  Comp_Checks.

         procedure Process_One_Dimension
           (Dim        : Pos;
            Indices    : List_Id;
            Dim_Checks : in out List_Id);
         --  Generate a loop over the Nth dimension Dim of an array type. List
         --  Indices contains all array indices for the dimension. All created
         --  checks are added to list Dim_Checks.

         -----------------------------
         -- Process_Array_Component --
         -----------------------------

         procedure Process_Array_Component
           (Indices     : List_Id;
            Comp_Checks : in out List_Id)
         is
            Proc_Id : Entity_Id;

         begin
            if Has_Invariants (Comp_Typ) then

               --  In GNATprove mode, the component invariants are checked by
               --  other means. They should not be added to the array type
               --  invariant procedure, so that the procedure can be used to
               --  check the array type invariants if any.

               if GNATprove_Mode then
                  null;

               else
                  Proc_Id := Invariant_Procedure (Base_Type (Comp_Typ));

                  --  The component type should have an invariant procedure
                  --  if it has invariants of its own or inherits class-wide
                  --  invariants from parent or interface types.

                  pragma Assert (Present (Proc_Id));

                  --  Generate:
                  --    <Comp_Typ>Invariant (_object (<Indices>));

                  --  Note that the invariant procedure may have a null body if
                  --  assertions are disabled or Assertion_Policy Ignore is in
                  --  effect.

                  if not Has_Null_Body (Proc_Id) then
                     Append_New_To (Comp_Checks,
                       Make_Procedure_Call_Statement (Loc,
                         Name                   =>
                           New_Occurrence_Of (Proc_Id, Loc),
                         Parameter_Associations => New_List (
                           Make_Indexed_Component (Loc,
                             Prefix      => New_Occurrence_Of (Obj_Id, Loc),
                             Expressions => New_Copy_List (Indices)))));
                  end if;
               end if;

               Produced_Check := True;
            end if;
         end Process_Array_Component;

         ---------------------------
         -- Process_One_Dimension --
         ---------------------------

         procedure Process_One_Dimension
           (Dim        : Pos;
            Indices    : List_Id;
            Dim_Checks : in out List_Id)
         is
            Comp_Checks : List_Id := No_List;
            Index       : Entity_Id;

         begin
            --  Generate the invariant checks for the array component after all
            --  dimensions have produced their respective loops.

            if Dim > Dims then
               Process_Array_Component
                 (Indices     => Indices,
                  Comp_Checks => Dim_Checks);

            --  Otherwise create a loop for the current dimension

            else
               --  Create a new loop variable for each dimension

               Index :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name ('I', Dim));
               Append_To (Indices, New_Occurrence_Of (Index, Loc));

               Process_One_Dimension
                 (Dim        => Dim + 1,
                  Indices    => Indices,
                  Dim_Checks => Comp_Checks);

               --  Generate:
               --    for I<Dim> in _object'Range (<Dim>) loop
               --       <Comp_Checks>
               --    end loop;

               --  Note that the invariant procedure may have a null body if
               --  assertions are disabled or Assertion_Policy Ignore is in
               --  effect.

               if Present (Comp_Checks) then
                  Append_New_To (Dim_Checks,
                    Make_Implicit_Loop_Statement (T,
                      Identifier       => Empty,
                      Iteration_Scheme =>
                        Make_Iteration_Scheme (Loc,
                          Loop_Parameter_Specification =>
                            Make_Loop_Parameter_Specification (Loc,
                              Defining_Identifier         => Index,
                              Discrete_Subtype_Definition =>
                                Make_Attribute_Reference (Loc,
                                  Prefix         =>
                                    New_Occurrence_Of (Obj_Id, Loc),
                                  Attribute_Name => Name_Range,
                                  Expressions    => New_List (
                                    Make_Integer_Literal (Loc, Dim))))),
                      Statements       => Comp_Checks));
               end if;
            end if;
         end Process_One_Dimension;

      --  Start of processing for Add_Array_Component_Invariants

      begin
         Process_One_Dimension
           (Dim        => 1,
            Indices    => New_List,
            Dim_Checks => Checks);
      end Add_Array_Component_Invariants;

      ------------------------------
      -- Add_Inherited_Invariants --
      ------------------------------

      procedure Add_Inherited_Invariants
        (T         : Entity_Id;
         Priv_Typ  : Entity_Id;
         Full_Typ  : Entity_Id;
         Obj_Id    : Entity_Id;
         Checks    : in out List_Id)
      is
         Deriv_Typ     : Entity_Id;
         Expr          : Node_Id;
         Prag          : Node_Id;
         Prag_Expr     : Node_Id;
         Prag_Expr_Arg : Node_Id;
         Prag_Typ      : Node_Id;
         Prag_Typ_Arg  : Node_Id;

         Par_Proc : Entity_Id;
         --  The "partial" invariant procedure of Par_Typ

         Par_Typ : Entity_Id;
         --  The suitable view of the parent type used in the substitution of
         --  type attributes.

      begin
         if not Present (Priv_Typ) and then not Present (Full_Typ) then
            return;
         end if;

         --  When the type inheriting the class-wide invariant is a concurrent
         --  type, use the corresponding record type because it contains all
         --  primitive operations of the concurrent type and allows for proper
         --  substitution.

         if Is_Concurrent_Type (T) then
            Deriv_Typ := Corresponding_Record_Type (T);
         else
            Deriv_Typ := T;
         end if;

               pragma Assert (Present (Deriv_Typ));

         --  Determine which rep item chain to use. Precedence is given to that
         --  of the parent type's partial view since it usually carries all the
         --  class-wide invariants.

         if Present (Priv_Typ) then
            Prag := First_Rep_Item (Priv_Typ);
         else
            Prag := First_Rep_Item (Full_Typ);
         end if;

         while Present (Prag) loop
            if Nkind (Prag) = N_Pragma
              and then Pragma_Name (Prag) = Name_Invariant
            then
               --  Nothing to do if the pragma was already processed

               if Contains (Pragmas_Seen, Prag) then
                  return;

               --  Nothing to do when the caller requests the processing of all
               --  inherited class-wide invariants, but the pragma does not
               --  fall in this category.

               elsif not Class_Present (Prag) then
                  return;
               end if;

               --  Extract the arguments of the invariant pragma

               Prag_Typ_Arg  := First (Pragma_Argument_Associations (Prag));
               Prag_Expr_Arg := Next (Prag_Typ_Arg);
               Prag_Expr     := Expression_Copy (Prag_Expr_Arg);
               Prag_Typ      := Get_Pragma_Arg (Prag_Typ_Arg);

               --  The pragma applies to the partial view of the parent type

               if Present (Priv_Typ)
                 and then Entity (Prag_Typ) = Priv_Typ
               then
                  Par_Typ := Priv_Typ;

               --  The pragma applies to the full view of the parent type

               elsif Present (Full_Typ)
                 and then Entity (Prag_Typ) = Full_Typ
               then
                  Par_Typ := Full_Typ;

               --  Otherwise the pragma does not belong to the parent type and
               --  should not be considered.

               else
                  return;
               end if;

               --  Perform the following substitutions:

               --    * Replace a reference to the _object parameter of the
               --      parent type's partial invariant procedure with a
               --      reference to the _object parameter of the derived
               --      type's full invariant procedure.

               --    * Replace a reference to a discriminant of the parent type
               --      with a suitable value from the point of view of the
               --      derived type.

               --    * Replace a call to an overridden parent primitive with a
               --      call to the overriding derived type primitive.

               --    * Replace a call to an inherited parent primitive with a
               --      call to the internally-generated inherited derived type
               --      primitive.

               Expr := New_Copy_Tree (Prag_Expr);

               --  The parent type must have a "partial" invariant procedure
               --  because class-wide invariants are captured exclusively by
               --  it.

               Par_Proc := Partial_Invariant_Procedure (Par_Typ);
               pragma Assert (Present (Par_Proc));

               Replace_References
                 (Expr      => Expr,
                  Par_Typ   => Par_Typ,
                  Deriv_Typ => Deriv_Typ,
                  Par_Obj   => First_Formal (Par_Proc),
                  Deriv_Obj => Obj_Id);

               Add_Invariant_Check (Prag, Expr, Checks, Inherited => True);
            end if;

            Next_Rep_Item (Prag);
         end loop;
      end Add_Inherited_Invariants;

      ------------------------------
      -- Add_Interface_Invariants --
      ------------------------------

      procedure Add_Interface_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id)
      is
         Iface_Elmt : Elmt_Id;
         Ifaces     : Elist_Id;

      begin
         --  Generate an invariant check for each class-wide invariant coming
         --  from all interfaces implemented by type T.

         if Is_Tagged_Type (T) then
            Collect_Interfaces (T, Ifaces);

            --  Process the class-wide invariants of all implemented interfaces

            Iface_Elmt := First_Elmt (Ifaces);
            while Present (Iface_Elmt) loop

               --  The Full_Typ parameter is intentionally left Empty because
               --  interfaces are treated as the partial view of a private type
               --  in order to achieve uniformity with the general case.

               Add_Inherited_Invariants
                 (T         => T,
                  Priv_Typ  => Node (Iface_Elmt),
                  Full_Typ  => Empty,
                  Obj_Id    => Obj_Id,
                  Checks    => Checks);

               Next_Elmt (Iface_Elmt);
            end loop;
         end if;
      end Add_Interface_Invariants;

      -------------------------
      -- Add_Invariant_Check --
      -------------------------

      procedure Add_Invariant_Check
        (Prag      : Node_Id;
         Expr      : Node_Id;
         Checks    : in out List_Id;
         Inherited : Boolean := False)
      is
         Args    : constant List_Id    := Pragma_Argument_Associations (Prag);
         Nam     : constant Name_Id    := Original_Aspect_Pragma_Name (Prag);
         Ploc    : constant Source_Ptr := Sloc (Prag);
         Str_Arg : constant Node_Id    := Next (Next (First (Args)));

         Assoc : List_Id;
         Str   : String_Id;

      begin
         --  The invariant is ignored, nothing left to do

         if Is_Ignored (Prag) then
            null;

         --  Otherwise the invariant is checked. Build a pragma Check to verify
         --  the expression at run time.

         else
            Assoc := New_List (
              Make_Pragma_Argument_Association (Ploc,
                Expression => Make_Identifier (Ploc, Nam)),
              Make_Pragma_Argument_Association (Ploc,
                Expression => Expr));

            --  Handle the String argument (if any)

            if Present (Str_Arg) then
               Str := Strval (Get_Pragma_Arg (Str_Arg));

               --  When inheriting an invariant, modify the message from
               --  "failed invariant" to "failed inherited invariant".

               if Inherited then
                  String_To_Name_Buffer (Str);

                  if Name_Buffer (1 .. 16) = "failed invariant" then
                     Insert_Str_In_Name_Buffer ("inherited ", 8);
                     Str := String_From_Name_Buffer;
                  end if;
               end if;

               Append_To (Assoc,
                 Make_Pragma_Argument_Association (Ploc,
                   Expression => Make_String_Literal (Ploc, Str)));
            end if;

            --  Generate:
            --    pragma Check (<Nam>, <Expr>, <Str>);

            Append_New_To (Checks,
              Make_Pragma (Ploc,
                Chars                        => Name_Check,
                Pragma_Argument_Associations => Assoc));
         end if;

         --  Output an info message when inheriting an invariant and the
         --  listing option is enabled.

         if Inherited and Opt.List_Inherited_Aspects then
            Error_Msg_Sloc := Sloc (Prag);
            Error_Msg_N
              ("info: & inherits `Invariant''Class` aspect from #?L?", Typ);
         end if;

         --  Add the pragma to the list of processed pragmas

         Append_New_Elmt (Prag, Pragmas_Seen);
         Produced_Check := True;
      end Add_Invariant_Check;

      ---------------------------
      -- Add_Parent_Invariants --
      ---------------------------

      procedure Add_Parent_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id)
      is
         Dummy_1 : Entity_Id;
         Dummy_2 : Entity_Id;

         Curr_Typ : Entity_Id;
         --  The entity of the current type being examined

         Full_Typ : Entity_Id;
         --  The full view of Par_Typ

         Par_Typ : Entity_Id;
         --  The entity of the parent type

         Priv_Typ : Entity_Id;
         --  The partial view of Par_Typ

      begin
         --  Do not process array types because they cannot have true parent
         --  types. This also prevents the generation of a duplicate invariant
         --  check when the input type is an array base type because its Etype
         --  denotes the first subtype, both of which share the same component
         --  type.

         if Is_Array_Type (T) then
            return;
         end if;

         --  Climb the parent type chain

         Curr_Typ := T;
         loop
            --  Do not consider subtypes as they inherit the invariants
            --  from their base types.

            Par_Typ := Base_Type (Etype (Curr_Typ));

            --  Stop the climb once the root of the parent chain is
            --  reached.

            exit when Curr_Typ = Par_Typ;

            --  Process the class-wide invariants of the parent type

            Get_Views (Par_Typ, Priv_Typ, Full_Typ, Dummy_1, Dummy_2);

            --  Process the elements of an array type

            if Is_Array_Type (Full_Typ) then
               Add_Array_Component_Invariants (Full_Typ, Obj_Id, Checks);

            --  Process the components of a record type

            elsif Ekind (Full_Typ) = E_Record_Type then
               Add_Record_Component_Invariants (Full_Typ, Obj_Id, Checks);
            end if;

            Add_Inherited_Invariants
              (T         => T,
               Priv_Typ  => Priv_Typ,
               Full_Typ  => Full_Typ,
               Obj_Id    => Obj_Id,
               Checks    => Checks);

            Curr_Typ := Par_Typ;
         end loop;
      end Add_Parent_Invariants;

      ------------------------
      -- Add_Own_Invariants --
      ------------------------

      procedure Add_Own_Invariants
        (T         : Entity_Id;
         Obj_Id    : Entity_Id;
         Checks    : in out List_Id;
         Priv_Item : Node_Id := Empty)
      is
         ASIS_Expr     : Node_Id;
         Expr          : Node_Id;
         Prag          : Node_Id;
         Prag_Asp      : Node_Id;
         Prag_Expr     : Node_Id;
         Prag_Expr_Arg : Node_Id;
         Prag_Typ      : Node_Id;
         Prag_Typ_Arg  : Node_Id;

      begin
         if not Present (T) then
            return;
         end if;

         Prag := First_Rep_Item (T);
         while Present (Prag) loop
            if Nkind (Prag) = N_Pragma
              and then Pragma_Name (Prag) = Name_Invariant
            then
               --  Stop the traversal of the rep item chain once a specific
               --  item is encountered.

               if Present (Priv_Item) and then Prag = Priv_Item then
                  exit;
               end if;

               --  Nothing to do if the pragma was already processed

               if Contains (Pragmas_Seen, Prag) then
                  return;
               end if;

               --  Extract the arguments of the invariant pragma

               Prag_Typ_Arg  := First (Pragma_Argument_Associations (Prag));
               Prag_Expr_Arg := Next (Prag_Typ_Arg);
               Prag_Expr     := Get_Pragma_Arg (Prag_Expr_Arg);
               Prag_Typ      := Get_Pragma_Arg (Prag_Typ_Arg);
               Prag_Asp      := Corresponding_Aspect (Prag);

               --  Verify the pragma belongs to T, otherwise the pragma applies
               --  to a parent type in which case it will be processed later by
               --  Add_Parent_Invariants or Add_Interface_Invariants.

               if Entity (Prag_Typ) /= T then
                  return;
               end if;

               Expr := New_Copy_Tree (Prag_Expr);

               --  Substitute all references to type T with references to the
               --  _object formal parameter.

               Replace_Type_References (Expr, T, Obj_Id);

               --  Preanalyze the invariant expression to detect errors and at
               --  the same time capture the visibility of the proper package
               --  part.

               Set_Parent (Expr, Parent (Prag_Expr));
               Preanalyze_Assert_Expression (Expr, Any_Boolean);

               --  Save a copy of the expression when T is tagged to detect
               --  errors and capture the visibility of the proper package part
               --  for the generation of inherited type invariants.

               if Is_Tagged_Type (T) then
                  Set_Expression_Copy (Prag_Expr_Arg, New_Copy_Tree (Expr));
               end if;

               --  If the pragma comes from an aspect specification, replace
               --  the saved expression because all type references must be
               --  substituted for the call to Preanalyze_Spec_Expression in
               --  Check_Aspect_At_xxx routines.

               if Present (Prag_Asp) then
                  Set_Entity (Identifier (Prag_Asp), New_Copy_Tree (Expr));
               end if;

               --  Analyze the original invariant expression for ASIS

               if ASIS_Mode then
                  ASIS_Expr := Empty;

                  if Comes_From_Source (Prag) then
                     ASIS_Expr := Prag_Expr;
                  elsif Present (Prag_Asp) then
                     ASIS_Expr := Expression (Prag_Asp);
                  end if;

                  if Present (ASIS_Expr) then
                     Replace_Type_References (ASIS_Expr, T, Obj_Id);
                     Preanalyze_Assert_Expression (ASIS_Expr, Any_Boolean);
                  end if;
               end if;

               Add_Invariant_Check (Prag, Expr, Checks);
            end if;

            Next_Rep_Item (Prag);
         end loop;
      end Add_Own_Invariants;

      -------------------------------------
      -- Add_Record_Component_Invariants --
      -------------------------------------

      procedure Add_Record_Component_Invariants
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id)
      is
         procedure Process_Component_List
           (Comp_List : Node_Id;
            CL_Checks : in out List_Id);
         --  Generate invariant checks for all record components found in
         --  component list Comp_List, including variant parts. All created
         --  checks are added to list CL_Checks.

         procedure Process_Record_Component
           (Comp_Id     : Entity_Id;
            Comp_Checks : in out List_Id);
         --  Generate an invariant check for a record component identified by
         --  Comp_Id. All created checks are added to list Comp_Checks.

         ----------------------------
         -- Process_Component_List --
         ----------------------------

         procedure Process_Component_List
           (Comp_List : Node_Id;
            CL_Checks : in out List_Id)
         is
            Comp       : Node_Id;
            Var        : Node_Id;
            Var_Alts   : List_Id := No_List;
            Var_Checks : List_Id := No_List;
            Var_Stmts  : List_Id;

            Produced_Variant_Check : Boolean := False;
            --  This flag tracks whether the component has produced at least
            --  one invariant check.

         begin
            --  Traverse the component items

            Comp := First (Component_Items (Comp_List));
            while Present (Comp) loop
               if Nkind (Comp) = N_Component_Declaration then

                  --  Generate the component invariant check

                  Process_Record_Component
                    (Comp_Id     => Defining_Entity (Comp),
                     Comp_Checks => CL_Checks);
               end if;

               Next (Comp);
            end loop;

            --  Traverse the variant part

            if Present (Variant_Part (Comp_List)) then
               Var := First (Variants (Variant_Part (Comp_List)));
               while Present (Var) loop
                  Var_Checks := No_List;

                  --  Generate invariant checks for all components and variant
                  --  parts that qualify.

                  Process_Component_List
                    (Comp_List => Component_List (Var),
                     CL_Checks => Var_Checks);

                  --  The components of the current variant produced at least
                  --  one invariant check.

                  if Present (Var_Checks) then
                     Var_Stmts := Var_Checks;
                     Produced_Variant_Check := True;

                  --  Otherwise there are either no components with invariants,
                  --  assertions are disabled, or Assertion_Policy Ignore is in
                  --  effect.

                  else
                     Var_Stmts := New_List (Make_Null_Statement (Loc));
                  end if;

                  Append_New_To (Var_Alts,
                    Make_Case_Statement_Alternative (Loc,
                      Discrete_Choices =>
                        New_Copy_List (Discrete_Choices (Var)),
                      Statements       => Var_Stmts));

                  Next (Var);
               end loop;

               --  Create a case statement which verifies the invariant checks
               --  of a particular component list depending on the discriminant
               --  values only when there is at least one real invariant check.

               if Produced_Variant_Check then
                  Append_New_To (CL_Checks,
                    Make_Case_Statement (Loc,
                      Expression   =>
                        Make_Selected_Component (Loc,
                          Prefix        => New_Occurrence_Of (Obj_Id, Loc),
                          Selector_Name =>
                            New_Occurrence_Of
                              (Entity (Name (Variant_Part (Comp_List))), Loc)),
                      Alternatives => Var_Alts));
               end if;
            end if;
         end Process_Component_List;

         ------------------------------
         -- Process_Record_Component --
         ------------------------------

         procedure Process_Record_Component
           (Comp_Id     : Entity_Id;
            Comp_Checks : in out List_Id)
         is
            Comp_Typ : constant Entity_Id := Etype (Comp_Id);
            Proc_Id  : Entity_Id;

            Produced_Component_Check : Boolean := False;
            --  This flag tracks whether the component has produced at least
            --  one invariant check.

         begin
            --  Nothing to do for internal component _parent. Note that it is
            --  not desirable to check whether the component comes from source
            --  because protected type components are relocated to an internal
            --  corresponding record, but still need processing.

            if Chars (Comp_Id) = Name_uParent then
               return;
            end if;

            --  Verify the invariant of the component. Note that an access
            --  type may have an invariant when it acts as the full view of a
            --  private type and the invariant appears on the partial view. In
            --  this case verify the access value itself.

            if Has_Invariants (Comp_Typ) then

               --  In GNATprove mode, the component invariants are checked by
               --  other means. They should not be added to the record type
               --  invariant procedure, so that the procedure can be used to
               --  check the record type invariants if any.

               if GNATprove_Mode then
                  null;

               else
                  Proc_Id := Invariant_Procedure (Base_Type (Comp_Typ));

                  --  The component type should have an invariant procedure
                  --  if it has invariants of its own or inherits class-wide
                  --  invariants from parent or interface types.

                  pragma Assert (Present (Proc_Id));

                  --  Generate:
                  --    <Comp_Typ>Invariant (T (_object).<Comp_Id>);

                  --  Note that the invariant procedure may have a null body if
                  --  assertions are disabled or Assertion_Policy Ignore is in
                  --  effect.

                  if not Has_Null_Body (Proc_Id) then
                     Append_New_To (Comp_Checks,
                       Make_Procedure_Call_Statement (Loc,
                         Name                   =>
                           New_Occurrence_Of (Proc_Id, Loc),
                         Parameter_Associations => New_List (
                           Make_Selected_Component (Loc,
                             Prefix        =>
                               Unchecked_Convert_To
                                 (T, New_Occurrence_Of (Obj_Id, Loc)),
                             Selector_Name =>
                               New_Occurrence_Of (Comp_Id, Loc)))));
                  end if;
               end if;

               Produced_Check           := True;
               Produced_Component_Check := True;
            end if;

            if Produced_Component_Check and then Has_Unchecked_Union (T) then
               Error_Msg_NE
                 ("invariants cannot be checked on components of "
                  & "unchecked_union type &?", Comp_Id, T);
            end if;
         end Process_Record_Component;

         --  Local variables

         Comps : Node_Id;
         Def   : Node_Id;

      --  Start of processing for Add_Record_Component_Invariants

      begin
         --  An untagged derived type inherits the components of its parent
         --  type. In order to avoid creating redundant invariant checks, do
         --  not process the components now. Instead wait until the ultimate
         --  parent of the untagged derivation chain is reached.

         if not Is_Untagged_Derivation (T) then
            Def := Type_Definition (Parent (T));

            if Nkind (Def) = N_Derived_Type_Definition then
               Def := Record_Extension_Part (Def);
            end if;

            pragma Assert (Nkind (Def) = N_Record_Definition);
            Comps := Component_List (Def);

            if Present (Comps) then
               Process_Component_List
                 (Comp_List => Comps,
                  CL_Checks => Checks);
            end if;
         end if;
      end Add_Record_Component_Invariants;

      --  Local variables

      Saved_GM : constant Ghost_Mode_Type := Ghost_Mode;
      --  Save the Ghost mode to restore on exit

      Dummy        : Entity_Id;
      Priv_Item    : Node_Id;
      Proc_Body    : Node_Id;
      Proc_Body_Id : Entity_Id;
      Proc_Decl    : Node_Id;
      Proc_Id      : Entity_Id;
      Stmts        : List_Id := No_List;

      CRec_Typ : Entity_Id := Empty;
      --  The corresponding record type of Full_Typ

      Full_Proc : Entity_Id := Empty;
      --  The entity of the "full" invariant procedure

      Full_Typ : Entity_Id := Empty;
      --  The full view of the working type

      Obj_Id : Entity_Id := Empty;
      --  The _object formal parameter of the invariant procedure

      Part_Proc : Entity_Id := Empty;
      --  The entity of the "partial" invariant procedure

      Priv_Typ : Entity_Id := Empty;
      --  The partial view of the working type

      Work_Typ : Entity_Id := Empty;
      --  The working type

   --  Start of processing for Build_Invariant_Procedure_Body

   begin
      Work_Typ := Typ;

      --  The input type denotes the implementation base type of a constrained
      --  array type. Work with the first subtype as all invariant pragmas are
      --  on its rep item chain.

      if Ekind (Work_Typ) = E_Array_Type and then Is_Itype (Work_Typ) then
         Work_Typ := First_Subtype (Work_Typ);

      --  The input type denotes the corresponding record type of a protected
      --  or task type. Work with the concurrent type because the corresponding
      --  record type may not be visible to clients of the type.

      elsif Ekind (Work_Typ) = E_Record_Type
        and then Is_Concurrent_Record_Type (Work_Typ)
      then
         Work_Typ := Corresponding_Concurrent_Type (Work_Typ);
      end if;

      --  The working type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the invariant procedure is properly marked as Ghost.

      Set_Ghost_Mode (Work_Typ);

      --  The type must either have invariants of its own, inherit class-wide
      --  invariants from parent types or interfaces, or be an array or record
      --  type whose components have invariants.

      pragma Assert (Has_Invariants (Work_Typ));

      --  Interfaces are treated as the partial view of a private type in order
      --  to achieve uniformity with the general case.

      if Is_Interface (Work_Typ) then
         Priv_Typ := Work_Typ;

      --  Otherwise obtain both views of the type

      else
         Get_Views (Work_Typ, Priv_Typ, Full_Typ, Dummy, CRec_Typ);
      end if;

      --  The caller requests a body for the partial invariant procedure

      if Partial_Invariant then
         Full_Proc := Invariant_Procedure (Work_Typ);
         Proc_Id   := Partial_Invariant_Procedure (Work_Typ);

         --  The "full" invariant procedure body was already created

         if Present (Full_Proc)
           and then Present
                      (Corresponding_Body (Unit_Declaration_Node (Full_Proc)))
         then
            --  This scenario happens only when the type is an untagged
            --  derivation from a private parent and the underlying full
            --  view was processed before the partial view.

            pragma Assert
              (Is_Untagged_Private_Derivation (Priv_Typ, Full_Typ));

            --  Nothing to do because the processing of the underlying full
            --  view already checked the invariants of the partial view.

            goto Leave;
         end if;

         --  Create a declaration for the "partial" invariant procedure if it
         --  is not available.

         if No (Proc_Id) then
            Build_Invariant_Procedure_Declaration
              (Typ               => Work_Typ,
               Partial_Invariant => True);

            Proc_Id := Partial_Invariant_Procedure (Work_Typ);
         end if;

      --  The caller requests a body for the "full" invariant procedure

      else
         Proc_Id   := Invariant_Procedure (Work_Typ);
         Part_Proc := Partial_Invariant_Procedure (Work_Typ);

         --  Create a declaration for the "full" invariant procedure if it is
         --  not available.

         if No (Proc_Id) then
            Build_Invariant_Procedure_Declaration (Work_Typ);
            Proc_Id := Invariant_Procedure (Work_Typ);
         end if;
      end if;

      --  At this point there should be an invariant procedure declaration

      pragma Assert (Present (Proc_Id));
      Proc_Decl := Unit_Declaration_Node (Proc_Id);

      --  Nothing to do if the invariant procedure already has a body

      if Present (Corresponding_Body (Proc_Decl)) then
         goto Leave;
      end if;

      --  Emulate the environment of the invariant procedure by installing its
      --  scope and formal parameters. Note that this is not needed, but having
      --  the scope installed helps with the detection of invariant-related
      --  errors.

      Push_Scope (Proc_Id);
      Install_Formals (Proc_Id);

      Obj_Id := First_Formal (Proc_Id);
      pragma Assert (Present (Obj_Id));

      --  The "partial" invariant procedure verifies the invariants of the
      --  partial view only.

      if Partial_Invariant then
         pragma Assert (Present (Priv_Typ));

         Add_Own_Invariants
           (T      => Priv_Typ,
            Obj_Id => Obj_Id,
            Checks => Stmts);

      --  Otherwise the "full" invariant procedure verifies the invariants of
      --  the full view, all array or record components, as well as class-wide
      --  invariants inherited from parent types or interfaces. In addition, it
      --  indirectly verifies the invariants of the partial view by calling the
      --  "partial" invariant procedure.

      else
         pragma Assert (Present (Full_Typ));

         --  Check the invariants of the partial view by calling the "partial"
         --  invariant procedure. Generate:

         --    <Work_Typ>Partial_Invariant (_object);

         if Present (Part_Proc) then
            Append_New_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name                   => New_Occurrence_Of (Part_Proc, Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Obj_Id, Loc))));

            Produced_Check := True;
         end if;

         Priv_Item := Empty;

         --  Derived subtypes do not have a partial view

         if Present (Priv_Typ) then

            --  The processing of the "full" invariant procedure intentionally
            --  skips the partial view because a) this may result in changes of
            --  visibility and b) lead to duplicate checks. However, when the
            --  full view is the underlying full view of an untagged derived
            --  type whose parent type is private, partial invariants appear on
            --  the rep item chain of the partial view only.

            --    package Pack_1 is
            --       type Root ... is private;
            --    private
            --       <full view of Root>
            --    end Pack_1;

            --    with Pack_1;
            --    package Pack_2 is
            --       type Child is new Pack_1.Root with Type_Invariant => ...;
            --       <underlying full view of Child>
            --    end Pack_2;

            --  As a result, the processing of the full view must also consider
            --  all invariants of the partial view.

            if Is_Untagged_Private_Derivation (Priv_Typ, Full_Typ) then
               null;

            --  Otherwise the invariants of the partial view are ignored

            else
               --  Note that the rep item chain is shared between the partial
               --  and full views of a type. To avoid processing the invariants
               --  of the partial view, signal the logic to stop when the first
               --  rep item of the partial view has been reached.

               Priv_Item := First_Rep_Item (Priv_Typ);

               --  Ignore the invariants of the partial view by eliminating the
               --  view.

               Priv_Typ := Empty;
            end if;
         end if;

         --  Process the invariants of the full view and in certain cases those
         --  of the partial view. This also handles any invariants on array or
         --  record components.

         Add_Own_Invariants
           (T         => Priv_Typ,
            Obj_Id    => Obj_Id,
            Checks    => Stmts,
            Priv_Item => Priv_Item);

         Add_Own_Invariants
           (T         => Full_Typ,
            Obj_Id    => Obj_Id,
            Checks    => Stmts,
            Priv_Item => Priv_Item);

         --  Process the elements of an array type

         if Is_Array_Type (Full_Typ) then
            Add_Array_Component_Invariants (Full_Typ, Obj_Id, Stmts);

         --  Process the components of a record type

         elsif Ekind (Full_Typ) = E_Record_Type then
            Add_Record_Component_Invariants (Full_Typ, Obj_Id, Stmts);

         --  Process the components of a corresponding record

         elsif Present (CRec_Typ) then
            Add_Record_Component_Invariants (CRec_Typ, Obj_Id, Stmts);
         end if;

         --  Process the inherited class-wide invariants of all parent types.
         --  This also handles any invariants on record components.

         Add_Parent_Invariants (Full_Typ, Obj_Id, Stmts);

         --  Process the inherited class-wide invariants of all implemented
         --  interface types.

         Add_Interface_Invariants (Full_Typ, Obj_Id, Stmts);
      end if;

      End_Scope;

      --  At this point there should be at least one invariant check. If this
      --  is not the case, then the invariant-related flags were not properly
      --  set, or there is a missing invariant procedure on one of the array
      --  or record components.

      pragma Assert (Produced_Check);

      --  Account for the case where assertions are disabled or all invariant
      --  checks are subject to Assertion_Policy Ignore. Produce a completing
      --  empty body.

      if No (Stmts) then
         Stmts := New_List (Make_Null_Statement (Loc));
      end if;

      --  Generate:
      --    procedure <Work_Typ>[Partial_]Invariant (_object : <Obj_Typ>) is
      --    begin
      --       <Stmts>
      --    end <Work_Typ>[Partial_]Invariant;

      Proc_Body :=
        Make_Subprogram_Body (Loc,
          Specification                =>
            Copy_Subprogram_Spec (Parent (Proc_Id)),
          Declarations                 => Empty_List,
            Handled_Statement_Sequence =>
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => Stmts));
      Proc_Body_Id := Defining_Entity (Proc_Body);

      --  Perform minor decoration in case the body is not analyzed

      Set_Ekind (Proc_Body_Id, E_Subprogram_Body);
      Set_Etype (Proc_Body_Id, Standard_Void_Type);
      Set_Scope (Proc_Body_Id, Current_Scope);

      --  Link both spec and body to avoid generating duplicates

      Set_Corresponding_Body (Proc_Decl, Proc_Body_Id);
      Set_Corresponding_Spec (Proc_Body, Proc_Id);

      --  The body should not be inserted into the tree when the context is
      --  ASIS or a generic unit because it is not part of the template. Note
      --  that the body must still be generated in order to resolve the
      --  invariants.

      if ASIS_Mode or Inside_A_Generic then
         null;

      --  Semi-insert the body into the tree for GNATprove by setting its
      --  Parent field. This allows for proper upstream tree traversals.

      elsif GNATprove_Mode then
         Set_Parent (Proc_Body, Parent (Declaration_Node (Work_Typ)));

      --  Otherwise the body is part of the freezing actions of the type

      else
         Append_Freeze_Action (Work_Typ, Proc_Body);
      end if;

   <<Leave>>
      Restore_Ghost_Mode (Saved_GM);
   end Build_Invariant_Procedure_Body;

   -------------------------------------------
   -- Build_Invariant_Procedure_Declaration --
   -------------------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_Invariant_Procedure_Declaration
     (Typ               : Entity_Id;
      Partial_Invariant : Boolean := False)
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM : constant Ghost_Mode_Type := Ghost_Mode;
      --  Save the Ghost mode to restore on exit

      Proc_Decl : Node_Id;
      Proc_Id   : Entity_Id;
      Proc_Nam  : Name_Id;
      Typ_Decl  : Node_Id;

      CRec_Typ : Entity_Id;
      --  The corresponding record type of Full_Typ

      Full_Base : Entity_Id;
      --  The base type of Full_Typ

      Full_Typ : Entity_Id;
      --  The full view of working type

      Obj_Id : Entity_Id;
      --  The _object formal parameter of the invariant procedure

      Obj_Typ : Entity_Id;
      --  The type of the _object formal parameter

      Priv_Typ : Entity_Id;
      --  The partial view of working type

      Work_Typ : Entity_Id;
      --  The working type

   begin
      Work_Typ := Typ;

      --  The input type denotes the implementation base type of a constrained
      --  array type. Work with the first subtype as all invariant pragmas are
      --  on its rep item chain.

      if Ekind (Work_Typ) = E_Array_Type and then Is_Itype (Work_Typ) then
         Work_Typ := First_Subtype (Work_Typ);

      --  The input denotes the corresponding record type of a protected or a
      --  task type. Work with the concurrent type because the corresponding
      --  record type may not be visible to clients of the type.

      elsif Ekind (Work_Typ) = E_Record_Type
        and then Is_Concurrent_Record_Type (Work_Typ)
      then
         Work_Typ := Corresponding_Concurrent_Type (Work_Typ);
      end if;

      --  The working type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the invariant procedure is properly marked as Ghost.

      Set_Ghost_Mode (Work_Typ);

      --  The type must either have invariants of its own, inherit class-wide
      --  invariants from parent or interface types, or be an array or record
      --  type whose components have invariants.

      pragma Assert (Has_Invariants (Work_Typ));

      --  Nothing to do if the type already has a "partial" invariant procedure

      if Partial_Invariant then
         if Present (Partial_Invariant_Procedure (Work_Typ)) then
            goto Leave;
         end if;

      --  Nothing to do if the type already has a "full" invariant procedure

      elsif Present (Invariant_Procedure (Work_Typ)) then
         goto Leave;
      end if;

      --  The caller requests the declaration of the "partial" invariant
      --  procedure.

      if Partial_Invariant then
         Proc_Nam := New_External_Name (Chars (Work_Typ), "Partial_Invariant");

      --  Otherwise the caller requests the declaration of the "full" invariant
      --  procedure.

      else
         Proc_Nam := New_External_Name (Chars (Work_Typ), "Invariant");
      end if;

      Proc_Id := Make_Defining_Identifier (Loc, Chars => Proc_Nam);

      --  Perform minor decoration in case the declaration is not analyzed

      Set_Ekind (Proc_Id, E_Procedure);
      Set_Etype (Proc_Id, Standard_Void_Type);
      Set_Scope (Proc_Id, Current_Scope);

      if Partial_Invariant then
         Set_Is_Partial_Invariant_Procedure (Proc_Id);
         Set_Partial_Invariant_Procedure (Work_Typ, Proc_Id);
      else
         Set_Is_Invariant_Procedure (Proc_Id);
         Set_Invariant_Procedure (Work_Typ, Proc_Id);
      end if;

      --  The invariant procedure requires debug info when the invariants are
      --  subject to Source Coverage Obligations.

      if Opt.Generate_SCO then
         Set_Needs_Debug_Info (Proc_Id);
      end if;

      --  Obtain all views of the input type

      Get_Views (Work_Typ, Priv_Typ, Full_Typ, Full_Base, CRec_Typ);

      --  Associate the invariant procedure with all views

      Propagate_Invariant_Attributes (Priv_Typ,  From_Typ => Work_Typ);
      Propagate_Invariant_Attributes (Full_Typ,  From_Typ => Work_Typ);
      Propagate_Invariant_Attributes (Full_Base, From_Typ => Work_Typ);
      Propagate_Invariant_Attributes (CRec_Typ,  From_Typ => Work_Typ);

      --  The declaration of the invariant procedure is inserted after the
      --  declaration of the partial view as this allows for proper external
      --  visibility.

      if Present (Priv_Typ) then
         Typ_Decl := Declaration_Node (Priv_Typ);

      --  Derived types with the full view as parent do not have a partial
      --  view. Insert the invariant procedure after the derived type.

      else
         Typ_Decl := Declaration_Node (Full_Typ);
      end if;

      --  The type should have a declarative node

      pragma Assert (Present (Typ_Decl));

      --  Create the formal parameter which emulates the variable-like behavior
      --  of the current type instance.

      Obj_Id := Make_Defining_Identifier (Loc, Chars => Name_uObject);

      --  When generating an invariant procedure declaration for an abstract
      --  type (including interfaces), use the class-wide type as the _object
      --  type. This has several desirable effects:

      --    * The invariant procedure does not become a primitive of the type.
      --      This eliminates the need to either special case the treatment of
      --      invariant procedures, or to make it a predefined primitive and
      --      force every derived type to potentially provide an empty body.

      --    * The invariant procedure does not need to be declared as abstract.
      --      This allows for a proper body, which in turn avoids redundant
      --      processing of the same invariants for types with multiple views.

      --    * The class-wide type allows for calls to abstract primitives
      --      within a nonabstract subprogram. The calls are treated as
      --      dispatching and require additional processing when they are
      --      remapped to call primitives of derived types. See routine
      --      Replace_References for details.

      if Is_Abstract_Type (Work_Typ) then
         Obj_Typ := Class_Wide_Type (Work_Typ);
      else
         Obj_Typ := Work_Typ;
      end if;

      --  Perform minor decoration in case the declaration is not analyzed

      Set_Ekind (Obj_Id, E_In_Parameter);
      Set_Etype (Obj_Id, Obj_Typ);
      Set_Scope (Obj_Id, Proc_Id);

      Set_First_Entity (Proc_Id, Obj_Id);

      --  Generate:
      --    procedure <Work_Typ>[Partial_]Invariant (_object : <Obj_Typ>);

      Proc_Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Id,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => Obj_Id,
                  Parameter_Type      => New_Occurrence_Of (Obj_Typ, Loc)))));

      --  The declaration should not be inserted into the tree when the context
      --  is ASIS or a generic unit because it is not part of the template.

      if ASIS_Mode or Inside_A_Generic then
         null;

      --  Semi-insert the declaration into the tree for GNATprove by setting
      --  its Parent field. This allows for proper upstream tree traversals.

      elsif GNATprove_Mode then
         Set_Parent (Proc_Decl, Parent (Typ_Decl));

      --  Otherwise insert the declaration

      else
         pragma Assert (Present (Typ_Decl));
         Insert_After_And_Analyze (Typ_Decl, Proc_Decl);
      end if;

   <<Leave>>
      Restore_Ghost_Mode (Saved_GM);
   end Build_Invariant_Procedure_Declaration;

   --------------------------
   -- Build_Procedure_Form --
   --------------------------

   procedure Build_Procedure_Form (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Subp : constant Entity_Id := Defining_Entity (N);

      Func_Formal  : Entity_Id;
      Proc_Formals : List_Id;
      Proc_Decl    : Node_Id;

   begin
      --  No action needed if this transformation was already done, or in case
      --  of subprogram renaming declarations.

      if Nkind (Specification (N)) = N_Procedure_Specification
        or else Nkind (N) = N_Subprogram_Renaming_Declaration
      then
         return;
      end if;

      --  Ditto when dealing with an expression function, where both the
      --  original expression and the generated declaration end up being
      --  expanded here.

      if Rewritten_For_C (Subp) then
         return;
      end if;

      Proc_Formals := New_List;

      --  Create a list of formal parameters with the same types as the
      --  function.

      Func_Formal := First_Formal (Subp);
      while Present (Func_Formal) loop
         Append_To (Proc_Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Chars (Func_Formal)),
             Parameter_Type      =>
               New_Occurrence_Of (Etype (Func_Formal), Loc)));

         Next_Formal (Func_Formal);
      end loop;

      --  Add an extra out parameter to carry the function result

      Name_Len := 6;
      Name_Buffer (1 .. Name_Len) := "RESULT";
      Append_To (Proc_Formals,
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Chars => Name_Find),
          Out_Present         => True,
          Parameter_Type      => New_Occurrence_Of (Etype (Subp), Loc)));

      --  The new procedure declaration is inserted immediately after the
      --  function declaration. The processing in Build_Procedure_Body_Form
      --  relies on this order.

      Proc_Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       =>
                Make_Defining_Identifier (Loc, Chars (Subp)),
              Parameter_Specifications => Proc_Formals));

      Insert_After_And_Analyze (Unit_Declaration_Node (Subp), Proc_Decl);

      --  Entity of procedure must remain invisible so that it does not
      --  overload subsequent references to the original function.

      Set_Is_Immediately_Visible (Defining_Entity (Proc_Decl), False);

      --  Mark the function as having a procedure form and link the function
      --  and its internally built procedure.

      Set_Rewritten_For_C (Subp);
      Set_Corresponding_Procedure (Subp, Defining_Entity (Proc_Decl));
      Set_Corresponding_Function (Defining_Entity (Proc_Decl), Subp);
   end Build_Procedure_Form;

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
             Name => New_Occurrence_Of (RTE (RE), Loc));
      end if;
   end Build_Runtime_Call;

   ------------------------
   -- Build_SS_Mark_Call --
   ------------------------

   function Build_SS_Mark_Call
     (Loc  : Source_Ptr;
      Mark : Entity_Id) return Node_Id
   is
   begin
      --  Generate:
      --    Mark : constant Mark_Id := SS_Mark;

      return
        Make_Object_Declaration (Loc,
          Defining_Identifier => Mark,
          Constant_Present    => True,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Mark_Id), Loc),
          Expression          =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (RE_SS_Mark), Loc)));
   end Build_SS_Mark_Call;

   ---------------------------
   -- Build_SS_Release_Call --
   ---------------------------

   function Build_SS_Release_Call
     (Loc  : Source_Ptr;
      Mark : Entity_Id) return Node_Id
   is
   begin
      --  Generate:
      --    SS_Release (Mark);

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_SS_Release), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Mark, Loc)));
   end Build_SS_Release_Call;

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

         if not In_Init_Proc then
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

      --  Indicate that the result is an internal temporary, so it does not
      --  receive a bogus initialization when declaration is expanded. This
      --  is both efficient, and prevents anomalies in the handling of
      --  dynamic objects on the secondary stack.

      Set_Is_Internal (Res);
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

   ---------------------------------------
   -- Build_Transient_Object_Statements --
   ---------------------------------------

   procedure Build_Transient_Object_Statements
     (Obj_Decl     : Node_Id;
      Fin_Call     : out Node_Id;
      Hook_Assign  : out Node_Id;
      Hook_Clear   : out Node_Id;
      Hook_Decl    : out Node_Id;
      Ptr_Decl     : out Node_Id;
      Finalize_Obj : Boolean := True)
   is
      Loc     : constant Source_Ptr := Sloc (Obj_Decl);
      Obj_Id  : constant Entity_Id  := Defining_Entity (Obj_Decl);
      Obj_Typ : constant Entity_Id  := Base_Type (Etype (Obj_Id));

      Desig_Typ : Entity_Id;
      Hook_Expr : Node_Id;
      Hook_Id   : Entity_Id;
      Obj_Ref   : Node_Id;
      Ptr_Typ   : Entity_Id;

   begin
      --  Recover the type of the object

      Desig_Typ := Obj_Typ;

      if Is_Access_Type (Desig_Typ) then
         Desig_Typ := Available_View (Designated_Type (Desig_Typ));
      end if;

      --  Create an access type which provides a reference to the transient
      --  object. Generate:

      --    type Ptr_Typ is access all Desig_Typ;

      Ptr_Typ := Make_Temporary (Loc, 'A');
      Set_Ekind (Ptr_Typ, E_General_Access_Type);
      Set_Directly_Designated_Type (Ptr_Typ, Desig_Typ);

      Ptr_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication => New_Occurrence_Of (Desig_Typ, Loc)));

      --  Create a temporary check which acts as a hook to the transient
      --  object. Generate:

      --    Hook : Ptr_Typ := null;

      Hook_Id := Make_Temporary (Loc, 'T');
      Set_Ekind (Hook_Id, E_Variable);
      Set_Etype (Hook_Id, Ptr_Typ);

      Hook_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Hook_Id,
          Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc),
          Expression          => Make_Null (Loc));

      --  Mark the temporary as a hook. This signals the machinery in
      --  Build_Finalizer to recognize this special case.

      Set_Status_Flag_Or_Transient_Decl (Hook_Id, Obj_Decl);

      --  Hook the transient object to the temporary. Generate:

      --    Hook := Ptr_Typ (Obj_Id);
      --      <or>
      --    Hool := Obj_Id'Unrestricted_Access;

      if Is_Access_Type (Obj_Typ) then
         Hook_Expr :=
           Unchecked_Convert_To (Ptr_Typ, New_Occurrence_Of (Obj_Id, Loc));
      else
         Hook_Expr :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Obj_Id, Loc),
             Attribute_Name => Name_Unrestricted_Access);
      end if;

      Hook_Assign :=
        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (Hook_Id, Loc),
          Expression => Hook_Expr);

      --  Crear the hook prior to finalizing the object. Generate:

      --    Hook := null;

      Hook_Clear :=
        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (Hook_Id, Loc),
          Expression => Make_Null (Loc));

      --  Finalize the object. Generate:

      --    [Deep_]Finalize (Obj_Ref[.all]);

      if Finalize_Obj then
         Obj_Ref := New_Occurrence_Of (Obj_Id, Loc);

         if Is_Access_Type (Obj_Typ) then
            Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);
            Set_Etype (Obj_Ref, Desig_Typ);
         end if;

         Fin_Call :=
           Make_Final_Call
             (Obj_Ref => Obj_Ref,
              Typ     => Desig_Typ);

      --  Otherwise finalize the hook. Generate:

      --    [Deep_]Finalize (Hook.all);

      else
         Fin_Call :=
           Make_Final_Call (
             Obj_Ref =>
               Make_Explicit_Dereference (Loc,
                 Prefix => New_Occurrence_Of (Hook_Id, Loc)),
             Typ     => Desig_Typ);
      end if;
   end Build_Transient_Object_Statements;

   -----------------------------
   -- Check_Float_Op_Overflow --
   -----------------------------

   procedure Check_Float_Op_Overflow (N : Node_Id) is
   begin
      --  Return if no check needed

      if not Is_Floating_Point_Type (Etype (N))
        or else not (Do_Overflow_Check (N) and then Check_Float_Overflow)

        --  In CodePeer_Mode, rely on the overflow check flag being set instead
        --  and do not expand the code for float overflow checking.

        or else CodePeer_Mode
      then
         return;
      end if;

      --  Otherwise we replace the expression by

      --  do Tnn : constant ftype := expression;
      --     constraint_error when not Tnn'Valid;
      --  in Tnn;

      declare
         Loc : constant Source_Ptr := Sloc (N);
         Tnn : constant Entity_Id  := Make_Temporary (Loc, 'T', N);
         Typ : constant Entity_Id  := Etype (N);

      begin
         --  Turn off the Do_Overflow_Check flag, since we are doing that work
         --  right here. We also set the node as analyzed to prevent infinite
         --  recursion from repeating the operation in the expansion.

         Set_Do_Overflow_Check (N, False);
         Set_Analyzed (N, True);

         --  Do the rewrite to include the check

         Rewrite (N,
           Make_Expression_With_Actions (Loc,
             Actions    => New_List (
               Make_Object_Declaration (Loc,
                 Defining_Identifier => Tnn,
                 Object_Definition   => New_Occurrence_Of (Typ, Loc),
                 Constant_Present    => True,
                 Expression          => Relocate_Node (N)),
               Make_Raise_Constraint_Error (Loc,
                 Condition =>
                   Make_Op_Not (Loc,
                     Right_Opnd =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Tnn, Loc),
                         Attribute_Name => Name_Valid)),
                 Reason    => CE_Overflow_Check_Failed)),
             Expression => New_Occurrence_Of (Tnn, Loc)));

         Analyze_And_Resolve (N, Typ);
      end;
   end Check_Float_Op_Overflow;

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

   ----------------------------------------
   -- Containing_Package_With_Ext_Axioms --
   ----------------------------------------

   function Containing_Package_With_Ext_Axioms
     (E : Entity_Id) return Entity_Id
   is
   begin
      --  E is the package or generic package which is externally axiomatized

      if Ekind_In (E, E_Generic_Package, E_Package)
        and then Has_Annotate_Pragma_For_External_Axiomatization (E)
      then
         return E;
      end if;

      --  If E's scope is axiomatized, E is axiomatized

      if Present (Scope (E)) then
         declare
            First_Ax_Parent_Scope : constant Entity_Id :=
              Containing_Package_With_Ext_Axioms (Scope (E));
         begin
            if Present (First_Ax_Parent_Scope) then
               return First_Ax_Parent_Scope;
            end if;
         end;
      end if;

      --  Otherwise, if E is a package instance, it is axiomatized if the
      --  corresponding generic package is axiomatized.

      if Ekind (E) = E_Package then
         declare
            Par  : constant Node_Id := Parent (E);
            Decl : Node_Id;

         begin
            if Nkind (Par) = N_Defining_Program_Unit_Name then
               Decl := Parent (Par);
            else
               Decl := Par;
            end if;

            if Present (Generic_Parent (Decl)) then
               return
                 Containing_Package_With_Ext_Axioms (Generic_Parent (Decl));
            end if;
         end;
      end if;

      return Empty;
   end Containing_Package_With_Ext_Axioms;

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
   -- Corresponding_Runtime_Package --
   -----------------------------------

   function Corresponding_Runtime_Package (Typ : Entity_Id) return RTU_Id is
      function Has_One_Entry_And_No_Queue (T : Entity_Id) return Boolean;
      --  Return True if protected type T has one entry and the maximum queue
      --  length is one.

      --------------------------------
      -- Has_One_Entry_And_No_Queue --
      --------------------------------

      function Has_One_Entry_And_No_Queue (T : Entity_Id) return Boolean is
         Item     : Entity_Id;
         Is_First : Boolean := True;

      begin
         Item := First_Entity (T);
         while Present (Item) loop
            if Is_Entry (Item) then

               --  The protected type has more than one entry

               if not Is_First then
                  return False;
               end if;

               --  The queue length is not one

               if not Restriction_Active (No_Entry_Queue)
                 and then Get_Max_Queue_Length (Item) /= Uint_1
               then
                  return False;
               end if;

               Is_First := False;
            end if;

            Next_Entity (Item);
         end loop;

         return True;
      end Has_One_Entry_And_No_Queue;

      --  Local variables

      Pkg_Id : RTU_Id := RTU_Null;

   --  Start of processing for Corresponding_Runtime_Package

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
              or else Restriction_Active (No_Select_Statements) = False
              or else not Has_One_Entry_And_No_Queue (Typ)
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
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Renaming_Req : Boolean := False) return Node_Id
   is
   begin
      Remove_Side_Effects (Exp, Name_Req, Renaming_Req);
      return New_Copy_Tree (Exp);
   end Duplicate_Subexpr;

   ---------------------------------
   -- Duplicate_Subexpr_No_Checks --
   ---------------------------------

   function Duplicate_Subexpr_No_Checks
     (Exp           : Node_Id;
      Name_Req      : Boolean   := False;
      Renaming_Req  : Boolean   := False;
      Related_Id    : Entity_Id := Empty;
      Is_Low_Bound  : Boolean   := False;
      Is_High_Bound : Boolean   := False) return Node_Id
   is
      New_Exp : Node_Id;

   begin
      Remove_Side_Effects
        (Exp           => Exp,
         Name_Req      => Name_Req,
         Renaming_Req  => Renaming_Req,
         Related_Id    => Related_Id,
         Is_Low_Bound  => Is_Low_Bound,
         Is_High_Bound => Is_High_Bound);

      New_Exp := New_Copy_Tree (Exp);
      Remove_Checks (New_Exp);
      return New_Exp;
   end Duplicate_Subexpr_No_Checks;

   -----------------------------------
   -- Duplicate_Subexpr_Move_Checks --
   -----------------------------------

   function Duplicate_Subexpr_Move_Checks
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Renaming_Req : Boolean := False) return Node_Id
   is
      New_Exp : Node_Id;

   begin
      Remove_Side_Effects (Exp, Name_Req, Renaming_Req);
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
   begin
      --  For an attribute reference or an indexed component, evaluate the
      --  prefix, which is itself a name, recursively, and then force the
      --  evaluation of all the subscripts (or attribute expressions).

      case Nkind (Nam) is
         when N_Attribute_Reference
            | N_Indexed_Component
         =>
            Evaluate_Name (Prefix (Nam));

            declare
               E : Node_Id;

            begin
               E := First (Expressions (Nam));
               while Present (E) loop
                  Force_Evaluation (E);

                  if Original_Node (E) /= E then
                     Set_Do_Range_Check
                       (E, Do_Range_Check (Original_Node (E)));
                  end if;

                  Next (E);
               end loop;
            end;

         --  For an explicit dereference, we simply force the evaluation of
         --  the name expression. The dereference provides a value that is the
         --  address for the renamed object, and it is precisely this value
         --  that we want to preserve.

         when N_Explicit_Dereference =>
            Force_Evaluation (Prefix (Nam));

         --  For a function call, we evaluate the call

         when N_Function_Call =>
            Force_Evaluation (Nam);

         --  For a qualified expression, we evaluate the underlying object
         --  name if any, otherwise we force the evaluation of the underlying
         --  expression.

         when N_Qualified_Expression =>
            if Is_Object_Reference (Expression (Nam)) then
               Evaluate_Name (Expression (Nam));
            else
               Force_Evaluation (Expression (Nam));
            end if;

         --  For a selected component, we simply evaluate the prefix

         when N_Selected_Component =>
            Evaluate_Name (Prefix (Nam));

         --  For a slice, we evaluate the prefix, as for the indexed component
         --  case and then, if there is a range present, either directly or as
         --  the constraint of a discrete subtype indication, we evaluate the
         --  two bounds of this range.

         when N_Slice =>
            Evaluate_Name (Prefix (Nam));
            Evaluate_Slice_Bounds (Nam);

         --  For a type conversion, the expression of the conversion must be
         --  the name of an object, and we simply need to evaluate this name.

         when N_Type_Conversion =>
            Evaluate_Name (Expression (Nam));

         --  The remaining cases are direct name, operator symbol and character
         --  literal. In all these cases, we do nothing, since we want to
         --  reevaluate each time the renamed object is used.

         when others =>
            null;
      end case;
   end Evaluate_Name;

   ---------------------------
   -- Evaluate_Slice_Bounds --
   ---------------------------

   procedure Evaluate_Slice_Bounds (Slice : Node_Id) is
      DR     : constant Node_Id := Discrete_Range (Slice);
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
   end Evaluate_Slice_Bounds;

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

   -----------------------------------
   -- Exceptions_In_Finalization_OK --
   -----------------------------------

   function Exceptions_In_Finalization_OK return Boolean is
   begin
      return
        not (Restriction_Active (No_Exception_Handlers)    or else
             Restriction_Active (No_Exception_Propagation) or else
             Restriction_Active (No_Exceptions));
   end Exceptions_In_Finalization_OK;

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

            P := First (Static_Discrete_Predicate (Entity (Choice)));
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
      Exp           : Node_Id;
      Related_Id    : Entity_Id := Empty)
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

      --  In GNATprove mode, these extra subtypes are not needed

      if GNATprove_Mode then
         return;
      end if;

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
                Subtype_Mark => New_Occurrence_Of (Unc_Type, Loc),
                Constraint =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List
                      (New_Occurrence_Of (Slice_Type, Loc)))));

            --  This subtype indication may be used later for constraint checks
            --  we better make sure that if a variable was used as a bound of
            --  of the original slice, its value is frozen.

            Evaluate_Slice_Bounds (Exp);
         end;

      elsif Ekind (Exp_Typ) = E_String_Literal_Subtype then
         Rewrite (Subtype_Indic,
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Occurrence_Of (Unc_Type, Loc),
             Constraint =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => New_List (
                   Make_Literal_Range (Loc,
                     Literal_Typ => Exp_Typ)))));

      --  If the type of the expression is an internally generated type it
      --  may not be necessary to create a new subtype. However there are two
      --  exceptions: references to the current instances, and aliased array
      --  object declarations for which the back end has to create a template.

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
                Subtype_Indication  => New_Occurrence_Of (Exp_Typ, Loc)));

            --  This type is marked as an itype even though it has an explicit
            --  declaration since otherwise Is_Generic_Actual_Type can get
            --  set, resulting in the generation of spurious errors. (See
            --  sem_ch8.Analyze_Package_Renaming and sem_type.covers)

            Set_Is_Itype (T);
            Set_Associated_Node_For_Itype (T, Exp);
         end if;

         Rewrite (Subtype_Indic, New_Occurrence_Of (T, Loc));

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

         --  If this is a limited type derived from a type with unknown
         --  discriminants, do not expand either, so that subsequent expansion
         --  of the call can add build-in-place parameters to call.

         if No (Underlying_Record_View (Unc_Type))
           or else Is_Limited_Type (Unc_Type)
         then
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
           Make_Subtype_From_Expr (Exp, Unc_Type, Related_Id));
      end if;
   end Expand_Subtype_From_Expr;

   ---------------------------------------------
   -- Expression_Contains_Primitives_Calls_Of --
   ---------------------------------------------

   function Expression_Contains_Primitives_Calls_Of
     (Expr : Node_Id;
      Typ  : Entity_Id) return Boolean
   is
      U_Typ : constant Entity_Id := Unique_Entity (Typ);

      Calls_OK : Boolean := False;
      --  This flag is set to True when expression Expr contains at least one
      --  call to a nondispatching primitive function of Typ.

      function Search_Primitive_Calls (N : Node_Id) return Traverse_Result;
      --  Search for nondispatching calls to primitive functions of type Typ

      ----------------------------
      -- Search_Primitive_Calls --
      ----------------------------

      function Search_Primitive_Calls (N : Node_Id) return Traverse_Result is
         Disp_Typ : Entity_Id;
         Subp     : Entity_Id;

      begin
         --  Detect a function call that could denote a nondispatching
         --  primitive of the input type.

         if Nkind (N) = N_Function_Call
           and then Is_Entity_Name (Name (N))
         then
            Subp := Entity (Name (N));

            --  Do not consider function calls with a controlling argument, as
            --  those are always dispatching calls.

            if Is_Dispatching_Operation (Subp)
              and then No (Controlling_Argument (N))
            then
               Disp_Typ := Find_Dispatching_Type (Subp);

               --  To qualify as a suitable primitive, the dispatching type of
               --  the function must be the input type.

               if Present (Disp_Typ)
                 and then Unique_Entity (Disp_Typ) = U_Typ
               then
                  Calls_OK := True;

                  --  There is no need to continue the traversal, as one such
                  --  call suffices.

                  return Abandon;
               end if;
            end if;
         end if;

         return OK;
      end Search_Primitive_Calls;

      procedure Search_Calls is new Traverse_Proc (Search_Primitive_Calls);

   --  Start of processing for Expression_Contains_Primitives_Calls_Of_Type

   begin
      Search_Calls (Expr);
      return Calls_OK;
   end Expression_Contains_Primitives_Calls_Of;

   ----------------------
   -- Finalize_Address --
   ----------------------

   function Finalize_Address (Typ : Entity_Id) return Entity_Id is
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

      --  Deal with untagged derivation of private views. If the parent is
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
   end Finalize_Address;

   -------------------
   -- Find_DIC_Type --
   -------------------

   function Find_DIC_Type (Typ : Entity_Id) return Entity_Id is
      Curr_Typ : Entity_Id;
      --  The current type being examined in the parent hierarchy traversal

      DIC_Typ : Entity_Id;
      --  The type which carries the DIC pragma. This variable denotes the
      --  partial view when private types are involved.

      Par_Typ : Entity_Id;
      --  The parent type of the current type. This variable denotes the full
      --  view when private types are involved.

   begin
      --  The input type defines its own DIC pragma, therefore it is the owner

      if Has_Own_DIC (Typ) then
         DIC_Typ := Typ;

      --  Otherwise the DIC pragma is inherited from a parent type

      else
         pragma Assert (Has_Inherited_DIC (Typ));

         --  Climb the parent chain

         Curr_Typ := Typ;
         loop
            --  Inspect the parent type. Do not consider subtypes as they
            --  inherit the DIC attributes from their base types.

            DIC_Typ := Base_Type (Etype (Curr_Typ));

            --  Look at the full view of a private type because the type may
            --  have a hidden parent introduced in the full view.

            Par_Typ := DIC_Typ;

            if Is_Private_Type (Par_Typ)
              and then Present (Full_View (Par_Typ))
            then
               Par_Typ := Full_View (Par_Typ);
            end if;

            --  Stop the climb once the nearest parent type which defines a DIC
            --  pragma of its own is encountered or when the root of the parent
            --  chain is reached.

            exit when Has_Own_DIC (DIC_Typ) or else Curr_Typ = Par_Typ;

            Curr_Typ := Par_Typ;
         end loop;
      end if;

      return DIC_Typ;
   end Find_DIC_Type;

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

   ---------------------------
   -- Find_Optional_Prim_Op --
   ---------------------------

   function Find_Optional_Prim_Op
     (T : Entity_Id; Name : Name_Id) return Entity_Id
   is
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
      end loop;

      return Node (Prim); -- Empty if not found
   end Find_Optional_Prim_Op;

   ---------------------------
   -- Find_Optional_Prim_Op --
   ---------------------------

   function Find_Optional_Prim_Op
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
         return Empty;
      end if;
   end Find_Optional_Prim_Op;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op
     (T : Entity_Id; Name : Name_Id) return Entity_Id
   is
      Result : constant Entity_Id := Find_Optional_Prim_Op (T, Name);
   begin
      if No (Result) then
         raise Program_Error;
      end if;

      return Result;
   end Find_Prim_Op;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op
     (T    : Entity_Id;
      Name : TSS_Name_Type) return Entity_Id
   is
      Result : constant Entity_Id := Find_Optional_Prim_Op (T, Name);
   begin
      if No (Result) then
         raise Program_Error;
      end if;

      return Result;
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

   -----------------------
   -- Find_Hook_Context --
   -----------------------

   function Find_Hook_Context (N : Node_Id) return Node_Id is
      Par : Node_Id;
      Top : Node_Id;

      Wrapped_Node : Node_Id;
      --  Note: if we are in a transient scope, we want to reuse it as
      --  the context for actions insertion, if possible. But if N is itself
      --  part of the stored actions for the current transient scope,
      --  then we need to insert at the appropriate (inner) location in
      --  the not as an action on Node_To_Be_Wrapped.

      In_Cond_Expr : constant Boolean := Within_Case_Or_If_Expression (N);

   begin
      --  When the node is inside a case/if expression, the lifetime of any
      --  temporary controlled object is extended. Find a suitable insertion
      --  node by locating the topmost case or if expressions.

      if In_Cond_Expr then
         Par := N;
         Top := N;
         while Present (Par) loop
            if Nkind_In (Original_Node (Par), N_Case_Expression,
                                              N_If_Expression)
            then
               Top := Par;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         --  The topmost case or if expression is now recovered, but it may
         --  still not be the correct place to add generated code. Climb to
         --  find a parent that is part of a declarative or statement list,
         --  and is not a list of actuals in a call.

         Par := Top;
         while Present (Par) loop
            if Is_List_Member (Par)
              and then not Nkind_In (Par, N_Component_Association,
                                          N_Discriminant_Association,
                                          N_Parameter_Association,
                                          N_Pragma_Argument_Association)
              and then not Nkind_In (Parent (Par), N_Function_Call,
                                                   N_Procedure_Call_Statement,
                                                   N_Entry_Call_Statement)

            then
               return Par;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return Par;

      else
         Par := N;
         while Present (Par) loop

            --  Keep climbing past various operators

            if Nkind (Parent (Par)) in N_Op
              or else Nkind_In (Parent (Par), N_And_Then, N_Or_Else)
            then
               Par := Parent (Par);
            else
               exit;
            end if;
         end loop;

         Top := Par;

         --  The node may be located in a pragma in which case return the
         --  pragma itself:

         --    pragma Precondition (... and then Ctrl_Func_Call ...);

         --  Similar case occurs when the node is related to an object
         --  declaration or assignment:

         --    Obj [: Some_Typ] := ... and then Ctrl_Func_Call ...;

         --  Another case to consider is when the node is part of a return
         --  statement:

         --    return ... and then Ctrl_Func_Call ...;

         --  Another case is when the node acts as a formal in a procedure
         --  call statement:

         --    Proc (... and then Ctrl_Func_Call ...);

         if Scope_Is_Transient then
            Wrapped_Node := Node_To_Be_Wrapped;
         else
            Wrapped_Node := Empty;
         end if;

         while Present (Par) loop
            if Par = Wrapped_Node
              or else Nkind_In (Par, N_Assignment_Statement,
                                     N_Object_Declaration,
                                     N_Pragma,
                                     N_Procedure_Call_Statement,
                                     N_Simple_Return_Statement)
            then
               return Par;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         --  Return the topmost short circuit operator

         return Top;
      end if;
   end Find_Hook_Context;

   ------------------------------
   -- Following_Address_Clause --
   ------------------------------

   function Following_Address_Clause (D : Node_Id) return Node_Id is
      Id     : constant Entity_Id := Defining_Identifier (D);
      Result : Node_Id;
      Par    : Node_Id;

      function Check_Decls (D : Node_Id) return Node_Id;
      --  This internal function differs from the main function in that it
      --  gets called to deal with a following package private part, and
      --  it checks declarations starting with D (the main function checks
      --  declarations following D). If D is Empty, then Empty is returned.

      -----------------
      -- Check_Decls --
      -----------------

      function Check_Decls (D : Node_Id) return Node_Id is
         Decl : Node_Id;

      begin
         Decl := D;
         while Present (Decl) loop
            if Nkind (Decl) = N_At_Clause
              and then Chars (Identifier (Decl)) = Chars (Id)
            then
               return Decl;

            elsif Nkind (Decl) = N_Attribute_Definition_Clause
              and then Chars (Decl) = Name_Address
              and then Chars (Name (Decl)) = Chars (Id)
            then
               return Decl;
            end if;

            Next (Decl);
         end loop;

         --  Otherwise not found, return Empty

         return Empty;
      end Check_Decls;

      --  Start of processing for Following_Address_Clause

   begin
      --  If parser detected no address clause for the identifier in question,
      --  then the answer is a quick NO, without the need for a search.

      if not Get_Name_Table_Boolean1 (Chars (Id)) then
         return Empty;
      end if;

      --  Otherwise search current declarative unit

      Result := Check_Decls (Next (D));

      if Present (Result) then
         return Result;
      end if;

      --  Check for possible package private part following

      Par := Parent (D);

      if Nkind (Par) = N_Package_Specification
        and then Visible_Declarations (Par) = List_Containing (D)
        and then Present (Private_Declarations (Par))
      then
         --  Private part present, check declarations there

         return Check_Decls (First (Private_Declarations (Par)));

      else
         --  No private part, clause not found, return Empty

         return Empty;
      end if;
   end Following_Address_Clause;

   ----------------------
   -- Force_Evaluation --
   ----------------------

   procedure Force_Evaluation
     (Exp           : Node_Id;
      Name_Req      : Boolean   := False;
      Related_Id    : Entity_Id := Empty;
      Is_Low_Bound  : Boolean   := False;
      Is_High_Bound : Boolean   := False;
      Mode          : Force_Evaluation_Mode := Relaxed)
   is
   begin
      Remove_Side_Effects
        (Exp                => Exp,
         Name_Req           => Name_Req,
         Variable_Ref       => True,
         Renaming_Req       => False,
         Related_Id         => Related_Id,
         Is_Low_Bound       => Is_Low_Bound,
         Is_High_Bound      => Is_High_Bound,
         Check_Side_Effects =>
           Is_Static_Expression (Exp)
             or else Mode = Relaxed);
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

            --  If the tree has been otherwise rewritten there is nothing
            --  else to be done either.

            if Nkind (Stm) /= N_If_Statement then
               return;
            end if;

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

   -----------------------------------------------------
   -- Has_Annotate_Pragma_For_External_Axiomatization --
   -----------------------------------------------------

   function Has_Annotate_Pragma_For_External_Axiomatization
     (E : Entity_Id) return Boolean
   is
      function Is_Annotate_Pragma_For_External_Axiomatization
        (N : Node_Id) return Boolean;
      --  Returns whether N is
      --    pragma Annotate (GNATprove, External_Axiomatization);

      ----------------------------------------------------
      -- Is_Annotate_Pragma_For_External_Axiomatization --
      ----------------------------------------------------

      --  The general form of pragma Annotate is

      --    pragma Annotate (IDENTIFIER [, IDENTIFIER {, ARG}]);
      --    ARG ::= NAME | EXPRESSION

      --  The first two arguments are by convention intended to refer to an
      --  external tool and a tool-specific function. These arguments are
      --  not analyzed.

      --  The following is used to annotate a package specification which
      --  GNATprove should treat specially, because the axiomatization of
      --  this unit is given by the user instead of being automatically
      --  generated.

      --    pragma Annotate (GNATprove, External_Axiomatization);

      function Is_Annotate_Pragma_For_External_Axiomatization
        (N : Node_Id) return Boolean
      is
         Name_GNATprove               : constant String :=
                                          "gnatprove";
         Name_External_Axiomatization : constant String :=
                                          "external_axiomatization";
         --  Special names

      begin
         if Nkind (N) = N_Pragma
           and then Get_Pragma_Id (N) = Pragma_Annotate
           and then List_Length (Pragma_Argument_Associations (N)) = 2
         then
            declare
               Arg1 : constant Node_Id :=
                        First (Pragma_Argument_Associations (N));
               Arg2 : constant Node_Id := Next (Arg1);
               Nam1 : Name_Id;
               Nam2 : Name_Id;

            begin
               --  Fill in Name_Buffer with Name_GNATprove first, and then with
               --  Name_External_Axiomatization so that Name_Find returns the
               --  corresponding name. This takes care of all possible casings.

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Name_GNATprove);
               Nam1 := Name_Find;

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Name_External_Axiomatization);
               Nam2 := Name_Find;

               return Chars (Get_Pragma_Arg (Arg1)) = Nam1
                         and then
                      Chars (Get_Pragma_Arg (Arg2)) = Nam2;
            end;

         else
            return False;
         end if;
      end Is_Annotate_Pragma_For_External_Axiomatization;

      --  Local variables

      Decl      : Node_Id;
      Vis_Decls : List_Id;
      N         : Node_Id;

   --  Start of processing for Has_Annotate_Pragma_For_External_Axiomatization

   begin
      if Nkind (Parent (E)) = N_Defining_Program_Unit_Name then
         Decl := Parent (Parent (E));
      else
         Decl := Parent (E);
      end if;

      Vis_Decls := Visible_Declarations (Decl);

      N := First (Vis_Decls);
      while Present (N) loop

         --  Skip declarations generated by the frontend. Skip all pragmas
         --  that are not the desired Annotate pragma. Stop the search on
         --  the first non-pragma source declaration.

         if Comes_From_Source (N) then
            if Nkind (N) = N_Pragma then
               if Is_Annotate_Pragma_For_External_Axiomatization (N) then
                  return True;
               end if;
            else
               return False;
            end if;
         end if;

         Next (N);
      end loop;

      return False;
   end Has_Annotate_Pragma_For_External_Axiomatization;

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
            when N_Subprogram_Body => return True;
            when N_If_Statement    => return False;
            when N_Loop_Statement  => return False;
            when N_Case_Statement  => return False;
            when others            => P := Parent (P);
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

      --  N_Raise_xxx_Error is an annoying special case, it is a statement
      --  if it has type Standard_Void_Type, and a subexpression otherwise.
      --  Procedure calls, and similarly procedure attribute references, are
      --  also statements.

      if Nkind (Assoc_Node) in N_Subexpr
        and then (Nkind (Assoc_Node) not in N_Raise_xxx_Error
                   or else Etype (Assoc_Node) /= Standard_Void_Type)
        and then Nkind (Assoc_Node) /= N_Procedure_Call_Statement
        and then (Nkind (Assoc_Node) /= N_Attribute_Reference
                   or else not Is_Procedure_Attribute_Name
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

                  --  If P has already been expanded, we can't park new actions
                  --  on it, so we need to expand them immediately, introducing
                  --  an Expression_With_Actions. N can't be an expression
                  --  with actions, or else then the actions would have been
                  --  inserted at an inner level.

                  if Analyzed (P) then
                     pragma Assert (Nkind (N) /= N_Expression_With_Actions);
                     Rewrite (N,
                       Make_Expression_With_Actions (Sloc (N),
                         Actions    => Ins_Actions,
                         Expression => Relocate_Node (N)));
                     Analyze_And_Resolve (N);

                  elsif Present (Actions (P)) then
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
            --  existing actions of the expression with actions, and should
            --  never reach here: if actions are inserted on a statement
            --  within the Actions of an expression with actions, or on some
            --  subexpression of such a statement, then the outermost proper
            --  insertion point is right before the statement, and we should
            --  never climb up as far as the N_Expression_With_Actions itself.

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

               else
                  raise Program_Error;
               end if;

            --  Case of appearing in the condition of a while expression or
            --  elsif. We insert the actions into the Condition_Actions field.
            --  They will be moved further out when the while loop or elsif
            --  is analyzed.

            when N_Elsif_Part
               | N_Iteration_Scheme
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

                 N_Procedure_Call_Statement
               | N_Statement_Other_Than_Procedure_Call

               --  Pragmas

               | N_Pragma

               --  Representation_Clause

               | N_At_Clause
               | N_Attribute_Definition_Clause
               | N_Enumeration_Representation_Clause
               | N_Record_Representation_Clause

               --  Declarations

               | N_Abstract_Subprogram_Declaration
               | N_Entry_Body
               | N_Exception_Declaration
               | N_Exception_Renaming_Declaration
               | N_Expression_Function
               | N_Formal_Abstract_Subprogram_Declaration
               | N_Formal_Concrete_Subprogram_Declaration
               | N_Formal_Object_Declaration
               | N_Formal_Type_Declaration
               | N_Full_Type_Declaration
               | N_Function_Instantiation
               | N_Generic_Function_Renaming_Declaration
               | N_Generic_Package_Declaration
               | N_Generic_Package_Renaming_Declaration
               | N_Generic_Procedure_Renaming_Declaration
               | N_Generic_Subprogram_Declaration
               | N_Implicit_Label_Declaration
               | N_Incomplete_Type_Declaration
               | N_Number_Declaration
               | N_Object_Declaration
               | N_Object_Renaming_Declaration
               | N_Package_Body
               | N_Package_Body_Stub
               | N_Package_Declaration
               | N_Package_Instantiation
               | N_Package_Renaming_Declaration
               | N_Private_Extension_Declaration
               | N_Private_Type_Declaration
               | N_Procedure_Instantiation
               | N_Protected_Body
               | N_Protected_Body_Stub
               | N_Protected_Type_Declaration
               | N_Single_Task_Declaration
               | N_Subprogram_Body
               | N_Subprogram_Body_Stub
               | N_Subprogram_Declaration
               | N_Subprogram_Renaming_Declaration
               | N_Subtype_Declaration
               | N_Task_Body
               | N_Task_Body_Stub
               | N_Task_Type_Declaration

               --  Use clauses can appear in lists of declarations

               | N_Use_Package_Clause
               | N_Use_Type_Clause

               --  Freeze entity behaves like a declaration or statement

               | N_Freeze_Entity
               | N_Freeze_Generic_Entity
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

            when N_Raise_xxx_Error =>
               if Etype (P) = Standard_Void_Type then
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

            --  If a component association appears within a loop created for
            --  an array aggregate, attach the actions to the association so
            --  they can be subsequently inserted within the loop. For other
            --  component associations insert outside of the aggregate. For
            --  an association that will generate a loop, its Loop_Actions
            --  attribute is already initialized (see exp_aggr.adb).

            --  The list of Loop_Actions can in turn generate additional ones,
            --  that are inserted before the associated node. If the associated
            --  node is outside the aggregate, the new actions are collected
            --  at the end of the Loop_Actions, to respect the order in which
            --  they are to be elaborated.

            when N_Component_Association
               | N_Iterated_Component_Association
            =>
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
                        --  declaration that is part of the Loop_Actions for
                        --  the component_association.

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

            when N_Attribute_Reference =>
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

            when N_Abortable_Part
               | N_Accept_Alternative
               | N_Access_Definition
               | N_Access_Function_Definition
               | N_Access_Procedure_Definition
               | N_Access_To_Object_Definition
               | N_Aggregate
               | N_Allocator
               | N_Aspect_Specification
               | N_Case_Expression
               | N_Case_Statement_Alternative
               | N_Character_Literal
               | N_Compilation_Unit
               | N_Compilation_Unit_Aux
               | N_Component_Clause
               | N_Component_Declaration
               | N_Component_Definition
               | N_Component_List
               | N_Constrained_Array_Definition
               | N_Decimal_Fixed_Point_Definition
               | N_Defining_Character_Literal
               | N_Defining_Identifier
               | N_Defining_Operator_Symbol
               | N_Defining_Program_Unit_Name
               | N_Delay_Alternative
               | N_Delta_Aggregate
               | N_Delta_Constraint
               | N_Derived_Type_Definition
               | N_Designator
               | N_Digits_Constraint
               | N_Discriminant_Association
               | N_Discriminant_Specification
               | N_Empty
               | N_Entry_Body_Formal_Part
               | N_Entry_Call_Alternative
               | N_Entry_Declaration
               | N_Entry_Index_Specification
               | N_Enumeration_Type_Definition
               | N_Error
               | N_Exception_Handler
               | N_Expanded_Name
               | N_Explicit_Dereference
               | N_Extension_Aggregate
               | N_Floating_Point_Definition
               | N_Formal_Decimal_Fixed_Point_Definition
               | N_Formal_Derived_Type_Definition
               | N_Formal_Discrete_Type_Definition
               | N_Formal_Floating_Point_Definition
               | N_Formal_Modular_Type_Definition
               | N_Formal_Ordinary_Fixed_Point_Definition
               | N_Formal_Package_Declaration
               | N_Formal_Private_Type_Definition
               | N_Formal_Incomplete_Type_Definition
               | N_Formal_Signed_Integer_Type_Definition
               | N_Function_Call
               | N_Function_Specification
               | N_Generic_Association
               | N_Handled_Sequence_Of_Statements
               | N_Identifier
               | N_In
               | N_Index_Or_Discriminant_Constraint
               | N_Indexed_Component
               | N_Integer_Literal
               | N_Iterator_Specification
               | N_Itype_Reference
               | N_Label
               | N_Loop_Parameter_Specification
               | N_Mod_Clause
               | N_Modular_Type_Definition
               | N_Not_In
               | N_Null
               | N_Op_Abs
               | N_Op_Add
               | N_Op_And
               | N_Op_Concat
               | N_Op_Divide
               | N_Op_Eq
               | N_Op_Expon
               | N_Op_Ge
               | N_Op_Gt
               | N_Op_Le
               | N_Op_Lt
               | N_Op_Minus
               | N_Op_Mod
               | N_Op_Multiply
               | N_Op_Ne
               | N_Op_Not
               | N_Op_Or
               | N_Op_Plus
               | N_Op_Rem
               | N_Op_Rotate_Left
               | N_Op_Rotate_Right
               | N_Op_Shift_Left
               | N_Op_Shift_Right
               | N_Op_Shift_Right_Arithmetic
               | N_Op_Subtract
               | N_Op_Xor
               | N_Operator_Symbol
               | N_Ordinary_Fixed_Point_Definition
               | N_Others_Choice
               | N_Package_Specification
               | N_Parameter_Association
               | N_Parameter_Specification
               | N_Pop_Constraint_Error_Label
               | N_Pop_Program_Error_Label
               | N_Pop_Storage_Error_Label
               | N_Pragma_Argument_Association
               | N_Procedure_Specification
               | N_Protected_Definition
               | N_Push_Constraint_Error_Label
               | N_Push_Program_Error_Label
               | N_Push_Storage_Error_Label
               | N_Qualified_Expression
               | N_Quantified_Expression
               | N_Raise_Expression
               | N_Range
               | N_Range_Constraint
               | N_Real_Literal
               | N_Real_Range_Specification
               | N_Record_Definition
               | N_Reference
               | N_SCIL_Dispatch_Table_Tag_Init
               | N_SCIL_Dispatching_Call
               | N_SCIL_Membership_Test
               | N_Selected_Component
               | N_Signed_Integer_Type_Definition
               | N_Single_Protected_Declaration
               | N_Slice
               | N_String_Literal
               | N_Subtype_Indication
               | N_Subunit
               | N_Target_Name
               | N_Task_Definition
               | N_Terminate_Alternative
               | N_Triggering_Alternative
               | N_Type_Conversion
               | N_Unchecked_Expression
               | N_Unchecked_Type_Conversion
               | N_Unconstrained_Array_Definition
               | N_Unused_At_End
               | N_Unused_At_Start
               | N_Variant
               | N_Variant_Part
               | N_Validate_Unchecked_Conversion
               | N_With_Clause
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

   ------------------------
   -- Insert_Declaration --
   ------------------------

   procedure Insert_Declaration (N : Node_Id; Decl : Node_Id) is
      P : Node_Id;

   begin
      pragma Assert (Nkind (N) in N_Subexpr);

      --  Climb until we find a procedure or a package

      P := N;
      loop
         pragma Assert (Present (Parent (P)));
         P := Parent (P);

         if Is_List_Member (P) then
            exit when Nkind_In (Parent (P), N_Package_Specification,
                                            N_Subprogram_Body);

            --  Special handling for handled sequence of statements, we must
            --  insert in the statements not the exception handlers!

            if Nkind (Parent (P)) = N_Handled_Sequence_Of_Statements then
               P := First (Statements (Parent (P)));
               exit;
            end if;
         end if;
      end loop;

      --  Now do the insertion

      Insert_Before (P, Decl);
      Analyze (Decl);
   end Insert_Declaration;

   ---------------------------------
   -- Insert_Library_Level_Action --
   ---------------------------------

   procedure Insert_Library_Level_Action (N : Node_Id) is
      Aux : constant Node_Id := Aux_Decls_Node (Cunit (Main_Unit));

   begin
      Push_Scope (Cunit_Entity (Current_Sem_Unit));
      --  And not Main_Unit as previously. If the main unit is a body,
      --  the scope needed to analyze the actions is the entity of the
      --  corresponding declaration.

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
      --  Determine if particular node denotes a controlled function call. The
      --  call may have been heavily expanded.

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
         --  When a function call appears in Object.Operation format, the
         --  original representation has several possible forms depending on
         --  the availability and form of actual parameters:

         --    Obj.Func                    N_Selected_Component
         --    Obj.Func (Actual)           N_Indexed_Component
         --    Obj.Func (Formal => Actual) N_Function_Call, whose Name is an
         --                                N_Selected_Component

         loop
            if Nkind (Expr) = N_Function_Call then
               Expr := Name (Expr);

            --  "Obj.Func (Actual)" case

            elsif Nkind (Expr) = N_Indexed_Component then
               Expr := Prefix (Expr);

            --  "Obj.Func" or "Obj.Func (Formal => Actual) case

            elsif Nkind (Expr) = N_Selected_Component then
               Expr := Selector_Name (Expr);

            else
               exit;
            end if;
         end loop;

         return
           Nkind (Expr) in N_Has_Entity
             and then Present (Entity (Expr))
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
         --  A controlled transient object is not considered aliased when it
         --  appears inside an expression_with_actions node even when there are
         --  explicit aliases of it:

         --    do
         --       Trans_Id : Ctrl_Typ ...;  --  transient object
         --       Alias : ... := Trans_Id;  --  object is aliased
         --       Val : constant Boolean :=
         --               ... Alias ...;    --  aliasing ends
         --       <finalize Trans_Id>       --  object safe to finalize
         --    in Val end;

         --  Expansion ensures that all aliases are encapsulated in the actions
         --  list and do not leak to the expression by forcing the evaluation
         --  of the expression.

         if Nkind (Rel_Node) = N_Expression_With_Actions then
            return False;

         --  Otherwise examine the statements after the controlled transient
         --  object and look for various forms of aliasing.

         else
            Stmt := First_Stmt;
            while Present (Stmt) loop
               if Nkind (Stmt) = N_Object_Declaration then
                  Expr := Expression (Stmt);

                  --  Aliasing of the form:
                  --    Obj : ... := Trans_Id'reference;

                  if Present (Expr)
                    and then Nkind (Expr) = N_Reference
                    and then Nkind (Prefix (Expr)) = N_Identifier
                    and then Entity (Prefix (Expr)) = Trans_Id
                  then
                     return True;
                  end if;

               elsif Nkind (Stmt) = N_Object_Renaming_Declaration then
                  Ren_Obj := Find_Renamed_Object (Stmt);

                  --  Aliasing of the form:
                  --    Obj : ... renames ... Trans_Id ...;

                  if Present (Ren_Obj) and then Ren_Obj = Trans_Id then
                     return True;
                  end if;
               end if;

               Next (Stmt);
            end loop;

            return False;
         end if;
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

         --  Look for aspect Default_Iterator. It may be part of a type
         --  declaration for a container, or inherited from a base type
         --  or parent type.

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
                 and then Nkind (Prefix (Expression (Stmt))) = N_Function_Call
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

         return False;
      end Is_Iterated_Container;

      --  Local variables

      Desig : Entity_Id := Obj_Typ;

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

          --  Do not consider a transient object that was already processed

          and then not Is_Finalized_Transient (Obj_Id)

          --  Do not consider renamed or 'reference-d transient objects because
          --  the act of renaming extends the object's lifetime.

          and then not Is_Aliased (Obj_Id, Decl)

          --  Do not consider transient objects allocated on the heap since
          --  they are attached to a finalization master.

          and then not Is_Allocated (Obj_Id)

          --  If the transient object is a pointer, check that it is not
          --  initialized by a function that returns a pointer or acts as a
          --  renaming of another pointer.

          and then
            (not Is_Access_Type (Obj_Typ)
               or else not Initialized_By_Access (Obj_Id))

          --  Do not consider transient objects which act as indirect aliases
          --  of build-in-place function results.

          and then not Initialized_By_Aliased_BIP_Func_Call (Obj_Id)

          --  Do not consider conversions of tags to class-wide types

          and then not Is_Tag_To_Class_Wide_Conversion (Obj_Id)

          --  Do not consider iterators because those are treated as normal
          --  controlled objects and are processed by the usual finalization
          --  machinery. This avoids the double finalization of an iterator.

          and then not Is_Iterator (Desig)

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

   ------------------------------------
   -- Is_Object_Access_BIP_Func_Call --
   ------------------------------------

   function Is_Object_Access_BIP_Func_Call
      (Expr   : Node_Id;
       Obj_Id : Entity_Id) return Boolean
   is
      Access_Nam : Name_Id := No_Name;
      Actual     : Node_Id;
      Call       : Node_Id;
      Formal     : Node_Id;
      Param      : Node_Id;

   begin
      --  Build-in-place calls usually appear in 'reference format. Note that
      --  the accessibility check machinery may add an extra 'reference due to
      --  side effect removal.

      Call := Expr;
      while Nkind (Call) = N_Reference loop
         Call := Prefix (Call);
      end loop;

      if Nkind_In (Call, N_Qualified_Expression,
                         N_Unchecked_Type_Conversion)
      then
         Call := Expression (Call);
      end if;

      if Is_Build_In_Place_Function_Call (Call) then

         --  Examine all parameter associations of the function call

         Param := First (Parameter_Associations (Call));
         while Present (Param) loop
            if Nkind (Param) = N_Parameter_Association
              and then Nkind (Selector_Name (Param)) = N_Identifier
            then
               Formal := Selector_Name (Param);
               Actual := Explicit_Actual_Parameter (Param);

               --  Construct the name of formal BIPaccess. It is much easier to
               --  extract the name of the function using an arbitrary formal's
               --  scope rather than the Name field of Call.

               if Access_Nam = No_Name and then Present (Entity (Formal)) then
                  Access_Nam :=
                    New_External_Name
                      (Chars (Scope (Entity (Formal))),
                       BIP_Formal_Suffix (BIP_Object_Access));
               end if;

               --  A match for BIPaccess => Obj_Id'Unrestricted_Access has been
               --  found.

               if Chars (Formal) = Access_Nam
                 and then Nkind (Actual) = N_Attribute_Reference
                 and then Attribute_Name (Actual) = Name_Unrestricted_Access
                 and then Nkind (Prefix (Actual)) = N_Identifier
                 and then Entity (Prefix (Actual)) = Obj_Id
               then
                  return True;
               end if;
            end if;

            Next (Param);
         end loop;
      end if;

      return False;
   end Is_Object_Access_BIP_Func_Call;

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
            --  not known, the front end can't know in any case. If a copy is
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
      Alloc_Nam : Name_Id := No_Name;
      Actual    : Node_Id;
      Call      : Node_Id := Expr;
      Formal    : Node_Id;
      Param     : Node_Id;

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

         --  Examine all parameter associations of the function call

         Param := First (Parameter_Associations (Call));
         while Present (Param) loop
            if Nkind (Param) = N_Parameter_Association
              and then Nkind (Selector_Name (Param)) = N_Identifier
            then
               Formal := Selector_Name (Param);
               Actual := Explicit_Actual_Parameter (Param);

               --  Construct the name of formal BIPalloc. It is much easier to
               --  extract the name of the function using an arbitrary formal's
               --  scope rather than the Name field of Call.

               if Alloc_Nam = No_Name and then Present (Entity (Formal)) then
                  Alloc_Nam :=
                    New_External_Name
                      (Chars (Scope (Entity (Formal))),
                       BIP_Formal_Suffix (BIP_Alloc_Form));
               end if;

               --  A match for BIPalloc => 2 has been found

               if Chars (Formal) = Alloc_Nam
                 and then Nkind (Actual) = N_Integer_Literal
                 and then Intval (Actual) = Uint_2
               then
                  return True;
               end if;
            end if;

            Next (Param);
         end loop;
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

   ------------------------------------
   -- Is_Untagged_Private_Derivation --
   ------------------------------------

   function Is_Untagged_Private_Derivation
     (Priv_Typ : Entity_Id;
      Full_Typ : Entity_Id) return Boolean
   is
   begin
      return
        Present (Priv_Typ)
          and then Is_Untagged_Derivation (Priv_Typ)
          and then Is_Private_Type (Etype (Priv_Typ))
          and then Present (Full_Typ)
          and then Is_Itype (Full_Typ);
   end Is_Untagged_Private_Derivation;

   ---------------------------
   -- Is_Volatile_Reference --
   ---------------------------

   function Is_Volatile_Reference (N : Node_Id) return Boolean is
   begin
      --  Only source references are to be treated as volatile, internally
      --  generated stuff cannot have volatile external effects.

      if not Comes_From_Source (N) then
         return False;

      --  Never true for reference to a type

      elsif Is_Entity_Name (N) and then Is_Type (Entity (N)) then
         return False;

      --  Never true for a compile time known constant

      elsif Compile_Time_Known_Value (N) then
         return False;

      --  True if object reference with volatile type

      elsif Is_Volatile_Object (N) then
         return True;

      --  True if reference to volatile entity

      elsif Is_Entity_Name (N) then
         return Treat_As_Volatile (Entity (N));

      --  True for slice of volatile array

      elsif Nkind (N) = N_Slice then
         return Is_Volatile_Reference (Prefix (N));

      --  True if volatile component

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

      --  Otherwise false

      else
         return False;
      end if;
   end Is_Volatile_Reference;

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

            --  Could we use Is_Statically_Unevaluated here???

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
            Kill_Dead_Code (Elsif_Parts     (N));
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

         --  At this point in the expansion, non-limited view of the type
         --  must be available, otherwise the error will be reported later.

         if From_Limited_With (Constr_Root)
           and then Present (Non_Limited_View (Constr_Root))
         then
            Constr_Root := Non_Limited_View (Constr_Root);
         end if;

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
                 Prefix => New_Occurrence_Of (Constr_Root, Loc),
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
              Subtype_Mark => New_Occurrence_Of (RTE (RE_Storage_Offset), Loc),
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
              Subtype_Mark => New_Occurrence_Of (RTE (RE_Storage_Array), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints =>
                    New_List (New_Occurrence_Of (Range_Type, Loc))))));

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

      --  A class-wide equivalent type does not require initialization

      Set_Suppress_Initialization (Equiv_Type);

      if not Is_Interface (Root_Typ) then
         Append_To (Comp_List,
           Make_Component_Declaration (Loc,
             Defining_Identifier  =>
               Make_Defining_Identifier (Loc, Name_uParent),
             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => False,
                 Subtype_Indication => New_Occurrence_Of (Constr_Root, Loc))));
      end if;

      Append_To (Comp_List,
        Make_Component_Declaration (Loc,
          Defining_Identifier  => Make_Temporary (Loc, 'C'),
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present    => False,
              Subtype_Indication => New_Occurrence_Of (Str_Type, Loc))));

      Append_To (List_Def,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Equiv_Type,
          Type_Definition     =>
            Make_Record_Definition (Loc,
              Component_List  =>
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
      Typ : constant Entity_Id  := Base_Type (Etype (Expr));

      Proc_Id : Entity_Id;

   begin
      pragma Assert (Has_Invariants (Typ));

      Proc_Id := Invariant_Procedure (Typ);
      pragma Assert (Present (Proc_Id));

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Id, Loc),
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

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   function Make_Predicate_Call
     (Typ  : Entity_Id;
      Expr : Node_Id;
      Mem  : Boolean := False) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      Saved_GM : constant Ghost_Mode_Type := Ghost_Mode;
      --  Save the Ghost mode to restore on exit

      Call    : Node_Id;
      Func_Id : Entity_Id;

   begin
      pragma Assert (Present (Predicate_Function (Typ)));

      --  The related type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the call is properly marked as Ghost.

      Set_Ghost_Mode (Typ);

      --  Call special membership version if requested and available

      if Mem and then Present (Predicate_Function_M (Typ)) then
         Func_Id := Predicate_Function_M (Typ);
      else
         Func_Id := Predicate_Function (Typ);
      end if;

      --  Case of calling normal predicate function

      Call :=
        Make_Function_Call (Loc,
          Name                   => New_Occurrence_Of (Func_Id, Loc),
          Parameter_Associations => New_List (Relocate_Node (Expr)));

      Restore_Ghost_Mode (Saved_GM);

      return Call;
   end Make_Predicate_Call;

   --------------------------
   -- Make_Predicate_Check --
   --------------------------

   function Make_Predicate_Check
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id
   is
      procedure Replace_Subtype_Reference (N : Node_Id);
      --  Replace current occurrences of the subtype to which a dynamic
      --  predicate applies, by the expression that triggers a predicate
      --  check. This is needed for aspect Predicate_Failure, for which
      --  we do not generate a wrapper procedure, but simply modify the
      --  expression for the pragma of the predicate check.

      --------------------------------
      --  Replace_Subtype_Reference --
      --------------------------------

      procedure Replace_Subtype_Reference (N : Node_Id) is
      begin
         Rewrite (N, New_Copy_Tree (Expr));

         --  We want to treat the node as if it comes from source, so
         --  that ASIS will not ignore it.

         Set_Comes_From_Source (N, True);
      end Replace_Subtype_Reference;

      procedure Replace_Subtype_References is
        new Replace_Type_References_Generic (Replace_Subtype_Reference);

      --  Local variables

      Loc       : constant Source_Ptr := Sloc (Expr);
      Arg_List  : List_Id;
      Fail_Expr : Node_Id;
      Nam       : Name_Id;

   --  Start of processing for Make_Predicate_Check

   begin
      --  If predicate checks are suppressed, then return a null statement. For
      --  this call, we check only the scope setting. If the caller wants to
      --  check a specific entity's setting, they must do it manually.

      if Predicate_Checks_Suppressed (Empty) then
         return Make_Null_Statement (Loc);
      end if;

      --  Do not generate a check within an internal subprogram (stream
      --  functions and the like, including including predicate functions).

      if Within_Internal_Subprogram then
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

      Arg_List := New_List (
        Make_Pragma_Argument_Association (Loc,
          Expression => Make_Identifier (Loc, Nam)),
        Make_Pragma_Argument_Association (Loc,
          Expression => Make_Predicate_Call (Typ, Expr)));

      --  If subtype has Predicate_Failure defined, add the correponding
      --  expression as an additional pragma parameter, after replacing
      --  current instances with the expression being checked.

      if Has_Aspect (Typ, Aspect_Predicate_Failure) then
         Fail_Expr :=
           New_Copy_Tree
             (Expression (Find_Aspect (Typ, Aspect_Predicate_Failure)));
         Replace_Subtype_References (Fail_Expr, Typ);

         Append_To (Arg_List,
           Make_Pragma_Argument_Association (Loc,
             Expression => Fail_Expr));
      end if;

      return
        Make_Pragma (Loc,
          Chars                        => Name_Check,
          Pragma_Argument_Associations => Arg_List);
   end Make_Predicate_Check;

   ----------------------------
   -- Make_Subtype_From_Expr --
   ----------------------------

   --  1. If Expr is an unconstrained array expression, creates
   --    Unc_Type(Expr'first(1)..Expr'last(1),..., Expr'first(n)..Expr'last(n))

   --  2. If Expr is a unconstrained discriminated type expression, creates
   --    Unc_Type(Expr.Discr1, ... , Expr.Discr_n)

   --  3. If Expr is class-wide, creates an implicit class-wide subtype

   function Make_Subtype_From_Expr
     (E          : Node_Id;
      Unc_Typ    : Entity_Id;
      Related_Id : Entity_Id := Empty) return Node_Id
   is
      List_Constr : constant List_Id    := New_List;
      Loc         : constant Source_Ptr := Sloc (E);
      D           : Entity_Id;
      Full_Exp    : Node_Id;
      Full_Subtyp : Entity_Id;
      High_Bound  : Entity_Id;
      Index_Typ   : Entity_Id;
      Low_Bound   : Entity_Id;
      Priv_Subtyp : Entity_Id;
      Utyp        : Entity_Id;

   begin
      if Is_Private_Type (Unc_Typ)
        and then Has_Unknown_Discriminants (Unc_Typ)
      then
         --  The caller requests a unique external name for both the private
         --  and the full subtype.

         if Present (Related_Id) then
            Full_Subtyp :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Related_Id), 'C'));
            Priv_Subtyp :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Related_Id), 'P'));

         else
            Full_Subtyp := Make_Temporary (Loc, 'C');
            Priv_Subtyp := Make_Temporary (Loc, 'P');
         end if;

         --  Prepare the subtype completion. Use the base type to find the
         --  underlying type because the type may be a generic actual or an
         --  explicit subtype.

         Utyp := Underlying_Type (Base_Type (Unc_Typ));

         Full_Exp :=
           Unchecked_Convert_To (Utyp, Duplicate_Subexpr_No_Checks (E));
         Set_Parent (Full_Exp, Parent (E));

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

         return New_Occurrence_Of (Priv_Subtyp, Loc);

      elsif Is_Array_Type (Unc_Typ) then
         Index_Typ := First_Index (Unc_Typ);
         for J in 1 .. Number_Dimensions (Unc_Typ) loop

            --  Capture the bounds of each index constraint in case the context
            --  is an object declaration of an unconstrained type initialized
            --  by a function call:

            --    Obj : Unconstr_Typ := Func_Call;

            --  This scenario requires secondary scope management and the index
            --  constraint cannot depend on the temporary used to capture the
            --  result of the function call.

            --    SS_Mark;
            --    Temp : Unconstr_Typ_Ptr := Func_Call'reference;
            --    subtype S is Unconstr_Typ (Temp.all'First .. Temp.all'Last);
            --    Obj : S := Temp.all;
            --    SS_Release;  --  Temp is gone at this point, bounds of S are
            --                 --  non existent.

            --  Generate:
            --    Low_Bound : constant Base_Type (Index_Typ) := E'First (J);

            Low_Bound := Make_Temporary (Loc, 'B');
            Insert_Action (E,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Low_Bound,
                Object_Definition   =>
                  New_Occurrence_Of (Base_Type (Etype (Index_Typ)), Loc),
                Constant_Present    => True,
                Expression          =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Duplicate_Subexpr_No_Checks (E),
                    Attribute_Name => Name_First,
                    Expressions    => New_List (
                      Make_Integer_Literal (Loc, J)))));

            --  Generate:
            --    High_Bound : constant Base_Type (Index_Typ) := E'Last (J);

            High_Bound := Make_Temporary (Loc, 'B');
            Insert_Action (E,
              Make_Object_Declaration (Loc,
                Defining_Identifier => High_Bound,
                Object_Definition   =>
                  New_Occurrence_Of (Base_Type (Etype (Index_Typ)), Loc),
                Constant_Present    => True,
                Expression          =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Duplicate_Subexpr_No_Checks (E),
                    Attribute_Name => Name_Last,
                    Expressions    => New_List (
                      Make_Integer_Literal (Loc, J)))));

            Append_To (List_Constr,
              Make_Range (Loc,
                Low_Bound  => New_Occurrence_Of (Low_Bound,  Loc),
                High_Bound => New_Occurrence_Of (High_Bound, Loc)));

            Index_Typ := Next_Index (Index_Typ);
         end loop;

      elsif Is_Class_Wide_Type (Unc_Typ) then
         declare
            CW_Subtype : Entity_Id;
            EQ_Typ     : Entity_Id := Empty;

         begin
            --  A class-wide equivalent type is not needed on VM targets
            --  because the VM back-ends handle the class-wide object
            --  initialization itself (and doesn't need or want the
            --  additional intermediate type to handle the assignment).

            if Expander_Active and then Tagged_Type_Expansion then

               --  If this is the class-wide type of a completion that is a
               --  record subtype, set the type of the class-wide type to be
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
                Selector_Name => New_Occurrence_Of (D, Loc)));

            Next_Discriminant (D);
         end loop;
      end if;

      return
        Make_Subtype_Indication (Loc,
          Subtype_Mark => New_Occurrence_Of (Unc_Typ, Loc),
          Constraint   =>
            Make_Index_Or_Discriminant_Constraint (Loc,
              Constraints => List_Constr));
   end Make_Subtype_From_Expr;

   ---------------
   -- Map_Types --
   ---------------

   procedure Map_Types (Parent_Type : Entity_Id; Derived_Type : Entity_Id) is

      --  NOTE: Most of the routines in Map_Types are intentionally unnested to
      --  avoid deep indentation of code.

      --  NOTE: Routines which deal with discriminant mapping operate on the
      --  [underlying/record] full view of various types because those views
      --  contain all discriminants and stored constraints.

      procedure Add_Primitive (Prim : Entity_Id; Par_Typ : Entity_Id);
      --  Subsidiary to Map_Primitives. Find a primitive in the inheritance or
      --  overriding chain starting from Prim whose dispatching type is parent
      --  type Par_Typ and add a mapping between the result and primitive Prim.

      function Ancestor_Primitive (Subp : Entity_Id) return Entity_Id;
      --  Subsidiary to Map_Primitives. Return the next ancestor primitive in
      --  the inheritance or overriding chain of subprogram Subp. Return Empty
      --  if no such primitive is available.

      function Build_Chain
        (Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id) return Elist_Id;
      --  Subsidiary to Map_Discriminants. Recreate the derivation chain from
      --  parent type Par_Typ leading down towards derived type Deriv_Typ. The
      --  list has the form:
      --
      --    head                                              tail
      --    v                                                 v
      --    <Ancestor_N> -> <Ancestor_N-1> -> <Ancestor_1> -> Deriv_Typ
      --
      --  Note that Par_Typ is not part of the resulting derivation chain

      function Discriminated_View (Typ : Entity_Id) return Entity_Id;
      --  Return the view of type Typ which could potentially contains either
      --  the discriminants or stored constraints of the type.

      function Find_Discriminant_Value
        (Discr     : Entity_Id;
         Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id;
         Typ_Elmt  : Elmt_Id) return Node_Or_Entity_Id;
      --  Subsidiary to Map_Discriminants. Find the value of discriminant Discr
      --  in the derivation chain starting from parent type Par_Typ leading to
      --  derived type Deriv_Typ. The returned value is one of the following:
      --
      --    * An entity which is either a discriminant or a non-discriminant
      --      name, and renames/constraints Discr.
      --
      --    * An expression which constraints Discr
      --
      --  Typ_Elmt is an element of the derivation chain created by routine
      --  Build_Chain and denotes the current ancestor being examined.

      procedure Map_Discriminants
        (Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id);
      --  Map each discriminant of type Par_Typ to a meaningful constraint
      --  from the point of view of type Deriv_Typ.

      procedure Map_Primitives (Par_Typ : Entity_Id; Deriv_Typ : Entity_Id);
      --  Map each primitive of type Par_Typ to a corresponding primitive of
      --  type Deriv_Typ.

      -------------------
      -- Add_Primitive --
      -------------------

      procedure Add_Primitive (Prim : Entity_Id; Par_Typ : Entity_Id) is
         Par_Prim : Entity_Id;

      begin
         --  Inspect the inheritance chain through the Alias attribute and the
         --  overriding chain through the Overridden_Operation looking for an
         --  ancestor primitive with the appropriate dispatching type.

         Par_Prim := Prim;
         while Present (Par_Prim) loop
            exit when Find_Dispatching_Type (Par_Prim) = Par_Typ;
            Par_Prim := Ancestor_Primitive (Par_Prim);
         end loop;

         --  Create a mapping of the form:

         --    parent type primitive -> derived type primitive

         if Present (Par_Prim) then
            Type_Map.Set (Par_Prim, Prim);
         end if;
      end Add_Primitive;

      ------------------------
      -- Ancestor_Primitive --
      ------------------------

      function Ancestor_Primitive (Subp : Entity_Id) return Entity_Id is
         Inher_Prim : constant Entity_Id := Alias (Subp);
         Over_Prim  : constant Entity_Id := Overridden_Operation (Subp);

      begin
         --  The current subprogram overrides an ancestor primitive

         if Present (Over_Prim) then
            return Over_Prim;

         --  The current subprogram is an internally generated alias of an
         --  inherited ancestor primitive.

         elsif Present (Inher_Prim) then
            return Inher_Prim;

         --  Otherwise the current subprogram is the root of the inheritance or
         --  overriding chain.

         else
            return Empty;
         end if;
      end Ancestor_Primitive;

      -----------------
      -- Build_Chain --
      -----------------

      function Build_Chain
        (Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id) return Elist_Id
      is
         Anc_Typ  : Entity_Id;
         Chain    : Elist_Id;
         Curr_Typ : Entity_Id;

      begin
         Chain := New_Elmt_List;

         --  Add the derived type to the derivation chain

         Prepend_Elmt (Deriv_Typ, Chain);

         --  Examine all ancestors starting from the derived type climbing
         --  towards parent type Par_Typ.

         Curr_Typ := Deriv_Typ;
         loop
            --  Handle the case where the current type is a record which
            --  derives from a subtype.

            --    subtype Sub_Typ is Par_Typ ...
            --    type Deriv_Typ is Sub_Typ ...

            if Ekind (Curr_Typ) = E_Record_Type
              and then Present (Parent_Subtype (Curr_Typ))
            then
               Anc_Typ := Parent_Subtype (Curr_Typ);

            --  Handle the case where the current type is a record subtype of
            --  another subtype.

            --    subtype Sub_Typ1 is Par_Typ ...
            --    subtype Sub_Typ2 is Sub_Typ1 ...

            elsif Ekind (Curr_Typ) = E_Record_Subtype
              and then Present (Cloned_Subtype (Curr_Typ))
            then
               Anc_Typ := Cloned_Subtype (Curr_Typ);

            --  Otherwise use the direct parent type

            else
               Anc_Typ := Etype (Curr_Typ);
            end if;

            --  Use the first subtype when dealing with itypes

            if Is_Itype (Anc_Typ) then
               Anc_Typ := First_Subtype (Anc_Typ);
            end if;

            --  Work with the view which contains the discriminants and stored
            --  constraints.

            Anc_Typ := Discriminated_View (Anc_Typ);

            --  Stop the climb when either the parent type has been reached or
            --  there are no more ancestors left to examine.

            exit when Anc_Typ = Curr_Typ or else Anc_Typ = Par_Typ;

            Prepend_Unique_Elmt (Anc_Typ, Chain);
            Curr_Typ := Anc_Typ;
         end loop;

         return Chain;
      end Build_Chain;

      ------------------------
      -- Discriminated_View --
      ------------------------

      function Discriminated_View (Typ : Entity_Id) return Entity_Id is
         T : Entity_Id;

      begin
         T := Typ;

         --  Use the [underlying] full view when dealing with private types
         --  because the view contains all inherited discriminants or stored
         --  constraints.

         if Is_Private_Type (T) then
            if Present (Underlying_Full_View (T)) then
               T := Underlying_Full_View (T);

            elsif Present (Full_View (T)) then
               T := Full_View (T);
            end if;
         end if;

         --  Use the underlying record view when the type is an extenstion of
         --  a parent type with unknown discriminants because the view contains
         --  all inherited discriminants or stored constraints.

         if Ekind (T) = E_Record_Type
           and then Present (Underlying_Record_View (T))
         then
            T := Underlying_Record_View (T);
         end if;

         return T;
      end Discriminated_View;

      -----------------------------
      -- Find_Discriminant_Value --
      -----------------------------

      function Find_Discriminant_Value
        (Discr     : Entity_Id;
         Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id;
         Typ_Elmt  : Elmt_Id) return Node_Or_Entity_Id
      is
         Discr_Pos : constant Uint      := Discriminant_Number (Discr);
         Typ       : constant Entity_Id := Node (Typ_Elmt);

         function Find_Constraint_Value
           (Constr : Node_Or_Entity_Id) return Node_Or_Entity_Id;
         --  Given constraint Constr, find what it denotes. This is either:
         --
         --    * An entity which is either a discriminant or a name
         --
         --    * An expression

         ---------------------------
         -- Find_Constraint_Value --
         ---------------------------

         function Find_Constraint_Value
           (Constr : Node_Or_Entity_Id) return Node_Or_Entity_Id
         is
         begin
            if Nkind (Constr) in N_Entity then

               --  The constraint denotes a discriminant of the curren type
               --  which renames the ancestor discriminant:

               --              vv
               --    type Typ (D1 : ...; DN : ...) is
               --      new Anc (Discr => D1) with ...
               --                        ^^

               if Ekind (Constr) = E_Discriminant then

                  --  The discriminant belongs to derived type Deriv_Typ. This
                  --  is the final value for the ancestor discriminant as the
                  --  derivations chain has been fully exhausted.

                  if Typ = Deriv_Typ then
                     return Constr;

                  --  Otherwise the discriminant may be renamed or constrained
                  --  at a lower level. Continue looking down the derivation
                  --  chain.

                  else
                     return
                       Find_Discriminant_Value
                         (Discr     => Constr,
                          Par_Typ   => Par_Typ,
                          Deriv_Typ => Deriv_Typ,
                          Typ_Elmt  => Next_Elmt (Typ_Elmt));
                  end if;

               --  Otherwise the constraint denotes a reference to some name
               --  which results in a Girder discriminant:

               --    vvvv
               --    Name : ...;
               --    type Typ (D1 : ...; DN : ...) is
               --      new Anc (Discr => Name) with ...
               --                        ^^^^

               --  Return the name as this is the proper constraint of the
               --  discriminant.

               else
                  return Constr;
               end if;

            --  The constraint denotes a reference to a name

            elsif Is_Entity_Name (Constr) then
               return Find_Constraint_Value (Entity (Constr));

            --  Otherwise the current constraint is an expression which yields
            --  a Girder discriminant:

            --    type Typ (D1 : ...; DN : ...) is
            --      new Anc (Discr => <expression>) with ...
            --                         ^^^^^^^^^^

            --  Return the expression as this is the proper constraint of the
            --  discriminant.

            else
               return Constr;
            end if;
         end Find_Constraint_Value;

         --  Local variables

         Constrs : constant Elist_Id := Stored_Constraint (Typ);

         Constr_Elmt : Elmt_Id;
         Pos         : Uint;
         Typ_Discr   : Entity_Id;

      --  Start of processing for Find_Discriminant_Value

      begin
         --  The algorithm for finding the value of a discriminant works as
         --  follows. First, it recreates the derivation chain from Par_Typ
         --  to Deriv_Typ as a list:

         --     Par_Typ      (shown for completeness)
         --        v
         --    Ancestor_N  <-- head of chain
         --        v
         --    Ancestor_1
         --        v
         --    Deriv_Typ   <--  tail of chain

         --  The algorithm then traces the fate of a parent discriminant down
         --  the derivation chain. At each derivation level, the discriminant
         --  may be either inherited or constrained.

         --    1) Discriminant is inherited: there are two cases, depending on
         --    which type is inheriting.

         --    1.1) Deriv_Typ is inheriting:

         --      type Ancestor (D_1 : ...) is tagged ...
         --      type Deriv_Typ is new Ancestor ...

         --    In this case the inherited discriminant is the final value of
         --    the parent discriminant because the end of the derivation chain
         --    has been reached.

         --    1.2) Some other type is inheriting:

         --      type Ancestor_1 (D_1 : ...) is tagged ...
         --      type Ancestor_2 is new Ancestor_1 ...

         --    In this case the algorithm continues to trace the fate of the
         --    inherited discriminant down the derivation chain because it may
         --    be further inherited or constrained.

         --    2) Discriminant is constrained: there are three cases, depending
         --    on what the constraint is.

         --    2.1) The constraint is another discriminant (aka renaming):

         --      type Ancestor_1 (D_1 : ...) is tagged ...
         --      type Ancestor_2 (D_2 : ...) is new Ancestor_1 (D_1 => D_2) ...

         --    In this case the constraining discriminant becomes the one to
         --    track down the derivation chain. The algorithm already knows
         --    that D_2 constrains D_1, therefore if the algorithm finds the
         --    value of D_2, then this would also be the value for D_1.

         --    2.2) The constraint is a name (aka Girder):

         --      Name : ...
         --      type Ancestor_1 (D_1 : ...) is tagged ...
         --      type Ancestor_2 is new Ancestor_1 (D_1 => Name) ...

         --    In this case the name is the final value of D_1 because the
         --    discriminant cannot be further constrained.

         --    2.3) The constraint is an expression (aka Girder):

         --      type Ancestor_1 (D_1 : ...) is tagged ...
         --      type Ancestor_2 is new Ancestor_1 (D_1 => 1 + 2) ...

         --    Similar to 2.2, the expression is the final value of D_1

         Pos := Uint_1;

         --  When a derived type constrains its parent type, all constaints
         --  appear in the Stored_Constraint list. Examine the list looking
         --  for a positional match.

         if Present (Constrs) then
            Constr_Elmt := First_Elmt (Constrs);
            while Present (Constr_Elmt) loop

               --  The position of the current constraint matches that of the
               --  ancestor discriminant.

               if Pos = Discr_Pos then
                  return Find_Constraint_Value (Node (Constr_Elmt));
               end if;

               Next_Elmt (Constr_Elmt);
               Pos := Pos + 1;
            end loop;

         --  Otherwise the derived type does not constraint its parent type in
         --  which case it inherits the parent discriminants.

         else
            Typ_Discr := First_Discriminant (Typ);
            while Present (Typ_Discr) loop

               --  The position of the current discriminant matches that of the
               --  ancestor discriminant.

               if Pos = Discr_Pos then
                  return Find_Constraint_Value (Typ_Discr);
               end if;

               Next_Discriminant (Typ_Discr);
               Pos := Pos + 1;
            end loop;
         end if;

         --  A discriminant must always have a corresponding value. This is
         --  either another discriminant, a name, or an expression. If this
         --  point is reached, them most likely the derivation chain employs
         --  the wrong views of types.

         pragma Assert (False);

         return Empty;
      end Find_Discriminant_Value;

      -----------------------
      -- Map_Discriminants --
      -----------------------

      procedure Map_Discriminants
        (Par_Typ   : Entity_Id;
         Deriv_Typ : Entity_Id)
      is
         Deriv_Chain : constant Elist_Id := Build_Chain (Par_Typ, Deriv_Typ);

         Discr     : Entity_Id;
         Discr_Val : Node_Or_Entity_Id;

      begin
         --  Examine each discriminant of parent type Par_Typ and find a
         --  suitable value for it from the point of view of derived type
         --  Deriv_Typ.

         if Has_Discriminants (Par_Typ) then
            Discr := First_Discriminant (Par_Typ);
            while Present (Discr) loop
               Discr_Val :=
                 Find_Discriminant_Value
                   (Discr     => Discr,
                    Par_Typ   => Par_Typ,
                    Deriv_Typ => Deriv_Typ,
                    Typ_Elmt  => First_Elmt (Deriv_Chain));

               --  Create a mapping of the form:

               --    parent type discriminant -> value

               Type_Map.Set (Discr, Discr_Val);

               Next_Discriminant (Discr);
            end loop;
         end if;
      end Map_Discriminants;

      --------------------
      -- Map_Primitives --
      --------------------

      procedure Map_Primitives (Par_Typ : Entity_Id; Deriv_Typ : Entity_Id) is
         Deriv_Prim : Entity_Id;
         Par_Prim   : Entity_Id;
         Par_Prims  : Elist_Id;
         Prim_Elmt  : Elmt_Id;

      begin
         --  Inspect the primitives of the derived type and determine whether
         --  they relate to the primitives of the parent type. If there is a
         --  meaningful relation, create a mapping of the form:

         --    parent type primitive -> perived type primitive

         if Present (Direct_Primitive_Operations (Deriv_Typ)) then
            Prim_Elmt := First_Elmt (Direct_Primitive_Operations (Deriv_Typ));
            while Present (Prim_Elmt) loop
               Deriv_Prim := Node (Prim_Elmt);

               if Is_Subprogram (Deriv_Prim)
                 and then Find_Dispatching_Type (Deriv_Prim) = Deriv_Typ
               then
                  Add_Primitive (Deriv_Prim, Par_Typ);
               end if;

               Next_Elmt (Prim_Elmt);
            end loop;
         end if;

         --  If the parent operation is an interface operation, the overriding
         --  indicator is not present. Instead, we get from the interface
         --  operation the primitive of the current type that implements it.

         if Is_Interface (Par_Typ) then
            Par_Prims := Collect_Primitive_Operations (Par_Typ);

            if Present (Par_Prims) then
               Prim_Elmt := First_Elmt (Par_Prims);

               while Present (Prim_Elmt) loop
                  Par_Prim   := Node (Prim_Elmt);
                  Deriv_Prim :=
                    Find_Primitive_Covering_Interface (Deriv_Typ, Par_Prim);

                  if Present (Deriv_Prim) then
                     Type_Map.Set (Par_Prim, Deriv_Prim);
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
            end if;
         end if;
      end Map_Primitives;

   --  Start of processing for Map_Types

   begin
      --  Nothing to do if there are no types to work with

      if No (Parent_Type) or else No (Derived_Type) then
         return;

      --  Nothing to do if the mapping already exists

      elsif Type_Map.Get (Parent_Type) = Derived_Type then
         return;

      --  Nothing to do if both types are not tagged. Note that untagged types
      --  do not have primitive operations and their discriminants are already
      --  handled by gigi.

      elsif not Is_Tagged_Type (Parent_Type)
        or else not Is_Tagged_Type (Derived_Type)
      then
         return;
      end if;

      --  Create a mapping of the form

      --    parent type -> derived type

      --  to prevent any subsequent attempts to produce the same relations

      Type_Map.Set (Parent_Type, Derived_Type);

      --  Create mappings of the form

      --    parent type discriminant -> derived type discriminant
      --      <or>
      --    parent type discriminant -> constraint

      --  Note that mapping of discriminants breaks privacy because it needs to
      --  work with those views which contains the discriminants and any stored
      --  constraints.

      Map_Discriminants
        (Par_Typ   => Discriminated_View (Parent_Type),
         Deriv_Typ => Discriminated_View (Derived_Type));

      --  Create mappings of the form

      --    parent type primitive -> derived type primitive

      Map_Primitives
        (Par_Typ   => Parent_Type,
         Deriv_Typ => Derived_Type);
   end Map_Types;

   ----------------------------
   -- Matching_Standard_Type --
   ----------------------------

   function Matching_Standard_Type (Typ : Entity_Id) return Entity_Id is
      pragma Assert (Is_Scalar_Type (Typ));
      Siz : constant Uint := Esize (Typ);

   begin
      --  Floating-point cases

      if Is_Floating_Point_Type (Typ) then
         if Siz <= Esize (Standard_Short_Float) then
            return Standard_Short_Float;
         elsif Siz <= Esize (Standard_Float) then
            return Standard_Float;
         elsif Siz <= Esize (Standard_Long_Float) then
            return Standard_Long_Float;
         elsif Siz <= Esize (Standard_Long_Long_Float) then
            return Standard_Long_Long_Float;
         else
            raise Program_Error;
         end if;

      --  Integer cases (includes fixed-point types)

      --  Unsigned integer cases (includes normal enumeration types)

      elsif Is_Unsigned_Type (Typ) then
         if Siz <= Esize (Standard_Short_Short_Unsigned) then
            return Standard_Short_Short_Unsigned;
         elsif Siz <= Esize (Standard_Short_Unsigned) then
            return Standard_Short_Unsigned;
         elsif Siz <= Esize (Standard_Unsigned) then
            return Standard_Unsigned;
         elsif Siz <= Esize (Standard_Long_Unsigned) then
            return Standard_Long_Unsigned;
         elsif Siz <= Esize (Standard_Long_Long_Unsigned) then
            return Standard_Long_Long_Unsigned;
         else
            raise Program_Error;
         end if;

      --  Signed integer cases

      else
         if Siz <= Esize (Standard_Short_Short_Integer) then
            return Standard_Short_Short_Integer;
         elsif Siz <= Esize (Standard_Short_Integer) then
            return Standard_Short_Integer;
         elsif Siz <= Esize (Standard_Integer) then
            return Standard_Integer;
         elsif Siz <= Esize (Standard_Long_Integer) then
            return Standard_Long_Integer;
         elsif Siz <= Esize (Standard_Long_Long_Integer) then
            return Standard_Long_Long_Integer;
         else
            raise Program_Error;
         end if;
      end if;
   end Matching_Standard_Type;

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

      elsif Is_Array_Type (Typ)
        and then Present (Packed_Array_Impl_Type (Typ))
      then
         return May_Generate_Large_Temp (Packed_Array_Impl_Type (Typ));

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

            else
               return
                 Is_Array_Type (Rec)
                   and then Needs_Finalization (Component_Type (Rec));
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

      --  C++ types are not considered controlled. It is assumed that the
      --  non-Ada side will handle their clean up.

      elsif Convention (T) = Convention_CPP then
         return False;

      --  Never needs finalization if Disable_Controlled set

      elsif Disable_Controlled (T) then
         return False;

      elsif Is_Class_Wide_Type (T) and then Disable_Controlled (Etype (T)) then
         return False;

      else
         --  Class-wide types are treated as controlled because derivations
         --  from the root type can introduce controlled components.

         return
           Is_Class_Wide_Type (T)
             or else Is_Controlled (T)
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
            and then Is_Modular_Integer_Type (Packed_Array_Impl_Type (Typ)))
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
      if Has_Non_Limited_View (Desig) then
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
      --  Do not process an unanalyzed node because it is not yet decorated and
      --  most checks performed below will fail.

      if not Analyzed (N) then
         return False;
      end if;

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
         --  then for sure the slice is.

         when N_Slice =>
            return Possible_Bit_Aligned_Component (Prefix (N));

         --  For an unchecked conversion, check whether the expression may
         --  be bit-aligned.

         when N_Unchecked_Type_Conversion =>
            return Possible_Bit_Aligned_Component (Expression (N));

         --  If we have none of the above, it means that we have fallen off the
         --  top testing prefixes recursively, and we now have a stand alone
         --  object, where we don't have a problem, unless this is a renaming,
         --  in which case we need to look into the renamed object.

         when others =>
            if Is_Entity_Name (N)
              and then Present (Renamed_Object (Entity (N)))
            then
               return
                 Possible_Bit_Aligned_Component (Renamed_Object (Entity (N)));
            else
               return False;
            end if;
      end case;
   end Possible_Bit_Aligned_Component;

   -----------------------------------------------
   -- Process_Statements_For_Controlled_Objects --
   -----------------------------------------------

   procedure Process_Statements_For_Controlled_Objects (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      function Are_Wrapped (L : List_Id) return Boolean;
      --  Determine whether list L contains only one statement which is a block

      function Wrap_Statements_In_Block
        (L    : List_Id;
         Scop : Entity_Id := Current_Scope) return Node_Id;
      --  Given a list of statements L, wrap it in a block statement and return
      --  the generated node. Scop is either the current scope or the scope of
      --  the context (if applicable).

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

      function Wrap_Statements_In_Block
        (L    : List_Id;
         Scop : Entity_Id := Current_Scope) return Node_Id
      is
         Block_Id  : Entity_Id;
         Block_Nod : Node_Id;
         Iter_Loop : Entity_Id;

      begin
         Block_Nod :=
           Make_Block_Statement (Loc,
             Declarations               => No_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => L));

         --  Create a label for the block in case the block needs to manage the
         --  secondary stack. A label allows for flag Uses_Sec_Stack to be set.

         Add_Block_Identifier (Block_Nod, Block_Id);

         --  When wrapping the statements of an iterator loop, check whether
         --  the loop requires secondary stack management and if so, propagate
         --  the appropriate flags to the block. This ensures that the cursor
         --  is properly cleaned up at each iteration of the loop.

         Iter_Loop := Find_Enclosing_Iterator_Loop (Scop);

         if Present (Iter_Loop) then
            Set_Uses_Sec_Stack (Block_Id, Uses_Sec_Stack (Iter_Loop));

            --  Secondary stack reclamation is suppressed when the associated
            --  iterator loop contains a return statement which uses the stack.

            Set_Sec_Stack_Needed_For_Return
              (Block_Id, Sec_Stack_Needed_For_Return (Iter_Loop));
         end if;

         return Block_Nod;
      end Wrap_Statements_In_Block;

      --  Local variables

      Block : Node_Id;

   --  Start of processing for Process_Statements_For_Controlled_Objects

   begin
      --  Whenever a non-handled statement list is wrapped in a block, the
      --  block must be explicitly analyzed to redecorate all entities in the
      --  list and ensure that a finalizer is properly built.

      case Nkind (N) is
         when N_Conditional_Entry_Call
            | N_Elsif_Part
            | N_If_Statement
            | N_Selective_Accept
         =>
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

         when N_Abortable_Part
            | N_Accept_Alternative
            | N_Case_Statement_Alternative
            | N_Delay_Alternative
            | N_Entry_Call_Alternative
            | N_Exception_Handler
            | N_Loop_Statement
            | N_Triggering_Alternative
         =>
            if not Is_Empty_List (Statements (N))
              and then not Are_Wrapped (Statements (N))
              and then Requires_Cleanup_Actions (Statements (N), False, False)
            then
               if Nkind (N) = N_Loop_Statement
                 and then Present (Identifier (N))
               then
                  Block :=
                    Wrap_Statements_In_Block
                      (L    => Statements (N),
                       Scop => Entity (Identifier (N)));
               else
                  Block := Wrap_Statements_In_Block (Statements (N));
               end if;

               Set_Statements (N, New_List (Block));
               Analyze (Block);
            end if;

         when others =>
            null;
      end case;
   end Process_Statements_For_Controlled_Objects;

   ------------------
   -- Power_Of_Two --
   ------------------

   function Power_Of_Two (N : Node_Id) return Nat is
      Typ : constant Entity_Id := Etype (N);
      pragma Assert (Is_Integer_Type (Typ));

      Siz : constant Nat := UI_To_Int (Esize (Typ));
      Val : Uint;

   begin
      if not Compile_Time_Known_Value (N) then
         return 0;

      else
         Val := Expr_Value (N);
         for J in 1 .. Siz - 1 loop
            if Val = Uint_2 ** J then
               return J;
            end if;
         end loop;

         return 0;
      end if;
   end Power_Of_Two;

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
     (Exp                : Node_Id;
      Name_Req           : Boolean   := False;
      Renaming_Req       : Boolean   := False;
      Variable_Ref       : Boolean   := False;
      Related_Id         : Entity_Id := Empty;
      Is_Low_Bound       : Boolean   := False;
      Is_High_Bound      : Boolean   := False;
      Check_Side_Effects : Boolean   := True)
   is
      function Build_Temporary
        (Loc         : Source_Ptr;
         Id          : Character;
         Related_Nod : Node_Id := Empty) return Entity_Id;
      --  Create an external symbol of the form xxx_FIRST/_LAST if Related_Nod
      --  is present (xxx is taken from the Chars field of Related_Nod),
      --  otherwise it generates an internal temporary.

      ---------------------
      -- Build_Temporary --
      ---------------------

      function Build_Temporary
        (Loc         : Source_Ptr;
         Id          : Character;
         Related_Nod : Node_Id := Empty) return Entity_Id
      is
         Temp_Nam : Name_Id;

      begin
         --  The context requires an external symbol

         if Present (Related_Id) then
            if Is_Low_Bound then
               Temp_Nam := New_External_Name (Chars (Related_Id), "_FIRST");
            else pragma Assert (Is_High_Bound);
               Temp_Nam := New_External_Name (Chars (Related_Id), "_LAST");
            end if;

            return Make_Defining_Identifier (Loc, Temp_Nam);

         --  Otherwise generate an internal temporary

         else
            return Make_Temporary (Loc, Id, Related_Nod);
         end if;
      end Build_Temporary;

      --  Local variables

      Loc          : constant Source_Ptr      := Sloc (Exp);
      Exp_Type     : constant Entity_Id       := Etype (Exp);
      Svg_Suppress : constant Suppress_Record := Scope_Suppress;
      Def_Id       : Entity_Id;
      E            : Node_Id;
      New_Exp      : Node_Id;
      Ptr_Typ_Decl : Node_Id;
      Ref_Type     : Entity_Id;
      Res          : Node_Id;

   --  Start of processing for Remove_Side_Effects

   begin
      --  Handle cases in which there is nothing to do. In GNATprove mode,
      --  removal of side effects is useful for the light expansion of
      --  renamings. This removal should only occur when not inside a
      --  generic and not doing a pre-analysis.

      if not Expander_Active
        and (Inside_A_Generic or not Full_Analysis or not GNATprove_Mode)
      then
         return;

      --  Cannot generate temporaries if the invocation to remove side effects
      --  was issued too early and the type of the expression is not resolved
      --  (this happens because routines Duplicate_Subexpr_XX implicitly invoke
      --  Remove_Side_Effects).

      elsif No (Exp_Type)
        or else Ekind (Exp_Type) = E_Access_Attribute_Type
      then
         return;

      --  Nothing to do if prior expansion determined that a function call does
      --  not require side effect removal.

      elsif Nkind (Exp) = N_Function_Call
        and then No_Side_Effect_Removal (Exp)
      then
         return;

      --  No action needed for side-effect free expressions

      elsif Check_Side_Effects
        and then Side_Effect_Free (Exp, Name_Req, Variable_Ref)
      then
         return;
      end if;

      --  The remaining processing is done with all checks suppressed

      --  Note: from now on, don't use return statements, instead do a goto
      --  Leave, to ensure that we properly restore Scope_Suppress.Suppress.

      Scope_Suppress.Suppress := (others => True);

      --  If this is an elementary or a small not by-reference record type, and
      --  we need to capture the value, just make a constant; this is cheap and
      --  objects of both kinds of types can be bit aligned, so it might not be
      --  possible to generate a reference to them. Likewise if this is not a
      --  name reference, except for a type conversion because we would enter
      --  an infinite recursion with Checks.Apply_Predicate_Check if the target
      --  type has predicates (and type conversions need a specific treatment
      --  anyway, see below). Also do it if we have a volatile reference and
      --  Name_Req is not set (see comments for Side_Effect_Free).

      if (Is_Elementary_Type (Exp_Type)
           or else (Is_Record_Type (Exp_Type)
                     and then Known_Static_RM_Size (Exp_Type)
                     and then RM_Size (Exp_Type) <= 64
                     and then not Has_Discriminants (Exp_Type)
                     and then not Is_By_Reference_Type (Exp_Type)))
        and then (Variable_Ref
                   or else (not Is_Name_Reference (Exp)
                             and then Nkind (Exp) /= N_Type_Conversion)
                   or else (not Name_Req
                             and then Is_Volatile_Reference (Exp)))
      then
         Def_Id := Build_Temporary (Loc, 'R', Exp);
         Set_Etype (Def_Id, Exp_Type);
         Res := New_Occurrence_Of (Def_Id, Loc);

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

         --  Generate:
         --    Rnn : Exp_Type renames Expr;

         if Renaming_Req then
            E :=
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        => New_Occurrence_Of (Exp_Type, Loc),
                Name                => Relocate_Node (Exp));

         --  Generate:
         --    Rnn : constant Exp_Type := Expr;

         else
            E :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Occurrence_Of (Exp_Type, Loc),
                Constant_Present    => True,
                Expression          => Relocate_Node (Exp));

            Set_Assignment_OK (E);
         end if;

         Insert_Action (Exp, E);

      --  If the expression has the form v.all then we can just capture the
      --  pointer, and then do an explicit dereference on the result, but
      --  this is not right if this is a volatile reference.

      elsif Nkind (Exp) = N_Explicit_Dereference
        and then not Is_Volatile_Reference (Exp)
      then
         Def_Id := Build_Temporary (Loc, 'R', Exp);
         Res :=
           Make_Explicit_Dereference (Loc, New_Occurrence_Of (Def_Id, Loc));

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   =>
               New_Occurrence_Of (Etype (Prefix (Exp)), Loc),
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

         --  Generating C code the type conversion of an access to constrained
         --  array type into an access to unconstrained array type involves
         --  initializing a fat pointer and the expression must be free of
         --  side effects to safely compute its bounds.

         if Modify_Tree_For_C
           and then Is_Access_Type (Etype (Exp))
           and then Is_Array_Type (Designated_Type (Etype (Exp)))
           and then not Is_Constrained (Designated_Type (Etype (Exp)))
         then
            Def_Id := Build_Temporary (Loc, 'R', Exp);
            Set_Etype (Def_Id, Exp_Type);
            Res := New_Occurrence_Of (Def_Id, Loc);

            Insert_Action (Exp,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Occurrence_Of (Exp_Type, Loc),
                Constant_Present    => True,
                Expression          => Relocate_Node (Exp)));
         else
            goto Leave;
         end if;

      --  If this is an unchecked conversion that Gigi can't handle, make
      --  a copy or a use a renaming to capture the value.

      elsif Nkind (Exp) = N_Unchecked_Type_Conversion
        and then not Safe_Unchecked_Type_Conversion (Exp)
      then
         if CW_Or_Has_Controlled_Part (Exp_Type) then

            --  Use a renaming to capture the expression, rather than create
            --  a controlled temporary.

            Def_Id := Build_Temporary (Loc, 'R', Exp);
            Res    := New_Occurrence_Of (Def_Id, Loc);

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        => New_Occurrence_Of (Exp_Type, Loc),
                Name                => Relocate_Node (Exp)));

         else
            Def_Id := Build_Temporary (Loc, 'R', Exp);
            Set_Etype (Def_Id, Exp_Type);
            Res    := New_Occurrence_Of (Def_Id, Loc);

            E :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Occurrence_Of (Exp_Type, Loc),
                Constant_Present    => not Is_Variable (Exp),
                Expression          => Relocate_Node (Exp));

            Set_Assignment_OK (E);
            Insert_Action (Exp, E);
         end if;

      --  For expressions that denote names, we can use a renaming scheme.
      --  This is needed for correctness in the case of a volatile object of
      --  a non-volatile type because the Make_Reference call of the "default"
      --  approach would generate an illegal access value (an access value
      --  cannot designate such an object - see Analyze_Reference).

      elsif Is_Name_Reference (Exp)

        --  We skip using this scheme if we have an object of a volatile
        --  type and we do not have Name_Req set true (see comments for
        --  Side_Effect_Free).

        and then (Name_Req or else not Treat_As_Volatile (Exp_Type))
      then
         Def_Id := Build_Temporary (Loc, 'R', Exp);
         Res := New_Occurrence_Of (Def_Id, Loc);

         Insert_Action (Exp,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Subtype_Mark        => New_Occurrence_Of (Exp_Type, Loc),
             Name                => Relocate_Node (Exp)));

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

      --  Avoid generating a variable-sized temporary, by generating the
      --  reference just for the function call. The transformation could be
      --  refined to apply only when the array component is constrained by a
      --  discriminant???

      elsif Nkind (Exp) = N_Selected_Component
        and then Nkind (Prefix (Exp)) = N_Function_Call
        and then Is_Array_Type (Exp_Type)
      then
         Remove_Side_Effects (Prefix (Exp), Name_Req, Variable_Ref);
         goto Leave;

      --  Otherwise we generate a reference to the expression

      else
         --  An expression which is in SPARK mode is considered side effect
         --  free if the resulting value is captured by a variable or a
         --  constant.

         if GNATprove_Mode
           and then Nkind (Parent (Exp)) = N_Object_Declaration
         then
            goto Leave;

         --  When generating C code we cannot consider side effect free object
         --  declarations that have discriminants and are initialized by means
         --  of a function call since on this target there is no secondary
         --  stack to store the return value and the expander may generate an
         --  extra call to the function to compute the discriminant value. In
         --  addition, for targets that have secondary stack, the expansion of
         --  functions with side effects involves the generation of an access
         --  type to capture the return value stored in the secondary stack;
         --  by contrast when generating C code such expansion generates an
         --  internal object declaration (no access type involved) which must
         --  be identified here to avoid entering into a never-ending loop
         --  generating internal object declarations.

         elsif Modify_Tree_For_C
           and then Nkind (Parent (Exp)) = N_Object_Declaration
           and then
             (Nkind (Exp) /= N_Function_Call
                or else not Has_Discriminants (Exp_Type)
                or else Is_Internal_Name
                          (Chars (Defining_Identifier (Parent (Exp)))))
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

         Def_Id := Build_Temporary (Loc, 'R', Exp);

         --  The regular expansion of functions with side effects involves the
         --  generation of an access type to capture the return value found on
         --  the secondary stack. Since SPARK (and why) cannot process access
         --  types, use a different approach which ignores the secondary stack
         --  and "copies" the returned object.
         --  When generating C code, no need for a 'reference since the
         --  secondary stack is not supported.

         if GNATprove_Mode or Modify_Tree_For_C then
            Res := New_Occurrence_Of (Def_Id, Loc);
            Ref_Type := Exp_Type;

         --  Regular expansion utilizing an access type and 'reference

         else
            Res :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Def_Id, Loc));

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
                      New_Occurrence_Of (Exp_Type, Loc)));

            Insert_Action (Exp, Ptr_Typ_Decl);
         end if;

         E := Exp;
         if Nkind (E) = N_Explicit_Dereference then
            New_Exp := Relocate_Node (Prefix (E));

         else
            E := Relocate_Node (E);

            --  Do not generate a 'reference in SPARK mode or C generation
            --  since the access type is not created in the first place.

            if GNATprove_Mode or Modify_Tree_For_C then
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

         --  Generating C code of object declarations that have discriminants
         --  and are initialized by means of a function call we propagate the
         --  discriminants of the parent type to the internally built object.
         --  This is needed to avoid generating an extra call to the called
         --  function.

         --  For example, if we generate here the following declaration, it
         --  will be expanded later adding an extra call to evaluate the value
         --  of the discriminant (needed to compute the size of the object).
         --
         --     type Rec (D : Integer) is ...
         --     Obj : constant Rec := SomeFunc;

         if Modify_Tree_For_C
           and then Nkind (Parent (Exp)) = N_Object_Declaration
           and then Has_Discriminants (Exp_Type)
           and then Nkind (Exp) = N_Function_Call
         then
            Insert_Action (Exp,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Copy_Tree
                                         (Object_Definition (Parent (Exp))),
                Constant_Present    => True,
                Expression          => New_Exp));
         else
            Insert_Action (Exp,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Object_Definition   => New_Occurrence_Of (Ref_Type, Loc),
                Constant_Present    => True,
                Expression          => New_Exp));
         end if;
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

   ------------------------
   -- Replace_References --
   ------------------------

   procedure Replace_References
     (Expr      : Node_Id;
      Par_Typ   : Entity_Id;
      Deriv_Typ : Entity_Id;
      Par_Obj   : Entity_Id := Empty;
      Deriv_Obj : Entity_Id := Empty)
   is
      function Is_Deriv_Obj_Ref (Ref : Node_Id) return Boolean;
      --  Determine whether node Ref denotes some component of Deriv_Obj

      function Replace_Ref (Ref : Node_Id) return Traverse_Result;
      --  Substitute a reference to an entity with the corresponding value
      --  stored in table Type_Map.

      function Type_Of_Formal
        (Call   : Node_Id;
         Actual : Node_Id) return Entity_Id;
      --  Find the type of the formal parameter which corresponds to actual
      --  parameter Actual in subprogram call Call.

      ----------------------
      -- Is_Deriv_Obj_Ref --
      ----------------------

      function Is_Deriv_Obj_Ref (Ref : Node_Id) return Boolean is
         Par : constant Node_Id := Parent (Ref);

      begin
         --  Detect the folowing selected component form:

         --    Deriv_Obj.(something)

         return
           Nkind (Par) = N_Selected_Component
             and then Is_Entity_Name (Prefix (Par))
             and then Entity (Prefix (Par)) = Deriv_Obj;
      end Is_Deriv_Obj_Ref;

      -----------------
      -- Replace_Ref --
      -----------------

      function Replace_Ref (Ref : Node_Id) return Traverse_Result is
         procedure Remove_Controlling_Arguments (From_Arg : Node_Id);
         --  Reset the Controlling_Argument of all function calls that
         --  encapsulate node From_Arg.

         ----------------------------------
         -- Remove_Controlling_Arguments --
         ----------------------------------

         procedure Remove_Controlling_Arguments (From_Arg : Node_Id) is
            Par : Node_Id;

         begin
            Par := From_Arg;
            while Present (Par) loop
               if Nkind (Par) = N_Function_Call
                 and then Present (Controlling_Argument (Par))
               then
                  Set_Controlling_Argument (Par, Empty);

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Par := Parent (Par);
            end loop;
         end Remove_Controlling_Arguments;

         --  Local variables

         Context : constant Node_Id    := Parent (Ref);
         Loc     : constant Source_Ptr := Sloc (Ref);
         Ref_Id  : Entity_Id;
         Result  : Traverse_Result;

         New_Ref : Node_Id;
         --  The new reference which is intended to substitute the old one

         Old_Ref : Node_Id;
         --  The reference designated for replacement. In certain cases this
         --  may be a node other than Ref.

         Val : Node_Or_Entity_Id;
         --  The corresponding value of Ref from the type map

      --  Start of processing for Replace_Ref

      begin
         --  Assume that the input reference is to be replaced and that the
         --  traversal should examine the children of the reference.

         Old_Ref := Ref;
         Result  := OK;

         --  The input denotes a meaningful reference

         if Nkind (Ref) in N_Has_Entity and then Present (Entity (Ref)) then
            Ref_Id := Entity (Ref);
            Val    := Type_Map.Get (Ref_Id);

            --  The reference has a corresponding value in the type map, a
            --  substitution is possible.

            if Present (Val) then

               --  The reference denotes a discriminant

               if Ekind (Ref_Id) = E_Discriminant then
                  if Nkind (Val) in N_Entity then

                     --  The value denotes another discriminant. Replace as
                     --  follows:

                     --    _object.Discr -> _object.Val

                     if Ekind (Val) = E_Discriminant then
                        New_Ref := New_Occurrence_Of (Val, Loc);

                     --  Otherwise the value denotes the entity of a name which
                     --  constraints the discriminant. Replace as follows:

                     --    _object.Discr -> Val

                     else
                        pragma Assert (Is_Deriv_Obj_Ref (Old_Ref));

                        New_Ref := New_Occurrence_Of (Val, Loc);
                        Old_Ref := Parent (Old_Ref);
                     end if;

                  --  Otherwise the value denotes an arbitrary expression which
                  --  constraints the discriminant. Replace as follows:

                  --    _object.Discr -> Val

                  else
                     pragma Assert (Is_Deriv_Obj_Ref (Old_Ref));

                     New_Ref := New_Copy_Tree (Val);
                     Old_Ref := Parent (Old_Ref);
                  end if;

               --  Otherwise the reference denotes a primitive. Replace as
               --  follows:

               --    Primitive -> Val

               else
                  pragma Assert (Nkind (Val) in N_Entity);
                  New_Ref := New_Occurrence_Of (Val, Loc);
               end if;

            --  The reference mentions the _object parameter of the parent
            --  type's DIC or type invariant procedure. Replace as follows:

            --    _object -> _object

            elsif Present (Par_Obj)
              and then Present (Deriv_Obj)
              and then Ref_Id = Par_Obj
            then
               New_Ref := New_Occurrence_Of (Deriv_Obj, Loc);

               --  The type of the _object parameter is class-wide when the
               --  expression comes from an assertion pragma that applies to
               --  an abstract parent type or an interface. The class-wide type
               --  facilitates the preanalysis of the expression by treating
               --  calls to abstract primitives that mention the current
               --  instance of the type as dispatching. Once the calls are
               --  remapped to invoke overriding or inherited primitives, the
               --  calls no longer need to be dispatching. Examine all function
               --  calls that encapsulate the _object parameter and reset their
               --  Controlling_Argument attribute.

               if Is_Class_Wide_Type (Etype (Par_Obj))
                 and then Is_Abstract_Type (Root_Type (Etype (Par_Obj)))
               then
                  Remove_Controlling_Arguments (Old_Ref);
               end if;

               --  The reference to _object acts as an actual parameter in a
               --  subprogram call which may be invoking a primitive of the
               --  parent type:

               --    Primitive (... _object ...);

               --  The parent type primitive may not be overridden nor
               --  inherited when it is declared after the derived type
               --  definition:

               --    type Parent is tagged private;
               --    type Child is new Parent with private;
               --    procedure Primitive (Obj : Parent);

               --  In this scenario the _object parameter is converted to the
               --  parent type. Due to complications with partial/full views
               --  and view swaps, the parent type is taken from the formal
               --  parameter of the subprogram being called.

               if Nkind_In (Context, N_Function_Call,
                                     N_Procedure_Call_Statement)
                 and then No (Type_Map.Get (Entity (Name (Context))))
               then
                  New_Ref :=
                    Convert_To (Type_Of_Formal (Context, Old_Ref), New_Ref);

                  --  Do not process the generated type conversion because
                  --  both the parent type and the derived type are in the
                  --  Type_Map table. This will clobber the type conversion
                  --  by resetting its subtype mark.

                  Result := Skip;
               end if;

            --  Otherwise there is nothing to replace

            else
               New_Ref := Empty;
            end if;

            if Present (New_Ref) then
               Rewrite (Old_Ref, New_Ref);

               --  Update the return type when the context of the reference
               --  acts as the name of a function call. Note that the update
               --  should not be performed when the reference appears as an
               --  actual in the call.

               if Nkind (Context) = N_Function_Call
                 and then Name (Context) = Old_Ref
               then
                  Set_Etype (Context, Etype (Val));
               end if;
            end if;
         end if;

         --  Reanalyze the reference due to potential replacements

         if Nkind (Old_Ref) in N_Has_Etype then
            Set_Analyzed (Old_Ref, False);
         end if;

         return Result;
      end Replace_Ref;

      procedure Replace_Refs is new Traverse_Proc (Replace_Ref);

      --------------------
      -- Type_Of_Formal --
      --------------------

      function Type_Of_Formal
        (Call   : Node_Id;
         Actual : Node_Id) return Entity_Id
      is
         A : Node_Id;
         F : Entity_Id;

      begin
         --  Examine the list of actual and formal parameters in parallel

         A := First (Parameter_Associations (Call));
         F := First_Formal (Entity (Name (Call)));
         while Present (A) and then Present (F) loop
            if A = Actual then
               return Etype (F);
            end if;

            Next (A);
            Next_Formal (F);
         end loop;

         --  The actual parameter must always have a corresponding formal

         pragma Assert (False);

         return Empty;
      end Type_Of_Formal;

   --  Start of processing for Replace_References

   begin
      --  Map the attributes of the parent type to the proper corresponding
      --  attributes of the derived type.

      Map_Types
        (Parent_Type  => Par_Typ,
         Derived_Type => Deriv_Typ);

      --  Inspect the input expression and perform substitutions where
      --  necessary.

      Replace_Refs (Expr);
   end Replace_References;

   -----------------------------
   -- Replace_Type_References --
   -----------------------------

   procedure Replace_Type_References
     (Expr   : Node_Id;
      Typ    : Entity_Id;
      Obj_Id : Entity_Id)
   is
      procedure Replace_Type_Ref (N : Node_Id);
      --  Substitute a single reference of the current instance of type Typ
      --  with a reference to Obj_Id.

      ----------------------
      -- Replace_Type_Ref --
      ----------------------

      procedure Replace_Type_Ref (N : Node_Id) is
      begin
         --  Decorate the reference to Typ even though it may be rewritten
         --  further down. This is done for two reasons:

         --    * ASIS has all necessary semantic information in the original
         --      tree.

         --    * Routines which examine properties of the Original_Node have
         --      some semantic information.

         if Nkind (N) = N_Identifier then
            Set_Entity (N, Typ);
            Set_Etype  (N, Typ);

         elsif Nkind (N) = N_Selected_Component then
            Analyze (Prefix (N));
            Set_Entity (Selector_Name (N), Typ);
            Set_Etype  (Selector_Name (N), Typ);
         end if;

         --  Perform the following substitution:

         --    Typ --> _object

         Rewrite (N, New_Occurrence_Of (Obj_Id, Sloc (N)));
         Set_Comes_From_Source (N, True);
      end Replace_Type_Ref;

      procedure Replace_Type_Refs is
        new Replace_Type_References_Generic (Replace_Type_Ref);

   --  Start of processing for Replace_Type_References

   begin
      Replace_Type_Refs (Expr, Typ);
   end Replace_Type_References;

   ---------------------------
   -- Represented_As_Scalar --
   ---------------------------

   function Represented_As_Scalar (T : Entity_Id) return Boolean is
      UT : constant Entity_Id := Underlying_Type (T);
   begin
      return Is_Scalar_Type (UT)
        or else (Is_Bit_Packed_Array (UT)
                  and then Is_Scalar_Type (Packed_Array_Impl_Type (UT)));
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
         when N_Accept_Statement
            | N_Block_Statement
            | N_Entry_Body
            | N_Package_Body
            | N_Protected_Body
            | N_Subprogram_Body
            | N_Task_Body
         =>
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

         when others =>
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

            --  Ignored Ghost types do not need any cleanup actions because
            --  they will not appear in the final tree.

            if Is_Ignored_Ghost_Entity (Typ) then
               null;

            elsif Is_Tagged_Type (Typ)
              and then Is_Library_Level_Entity (Typ)
              and then Convention (Typ) = Convention_Ada
              and then Present (Access_Disp_Table (Typ))
              and then RTE_Available (RE_Unregister_Tag)
              and then not Is_Abstract_Type (Typ)
              and then not No_Run_Time_Mode
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

            --  Finalization of transient objects are treated separately in
            --  order to handle sensitive cases. These include:

            --    * Aggregate expansion
            --    * If, case, and expression with actions expansion
            --    * Transient scopes

            --  If one of those contexts has marked the transient object as
            --  ignored, do not generate finalization actions for it.

            elsif Is_Finalized_Transient (Obj_Id)
              or else Is_Ignored_Transient (Obj_Id)
            then
               null;

            --  Ignored Ghost objects do not need any cleanup actions because
            --  they will not appear in the final tree.

            elsif Is_Ignored_Ghost_Entity (Obj_Id) then
               null;

            --  The expansion of iterator loops generates an object declaration
            --  where the Ekind is explicitly set to loop parameter. This is to
            --  ensure that the loop parameter behaves as a constant from user
            --  code point of view. Such object are never controlled and do not
            --  require cleanup actions. An iterator loop over a container of
            --  controlled objects does not produce such object declarations.

            elsif Ekind (Obj_Id) = E_Loop_Parameter then
               return False;

            --  The object is of the form:
            --    Obj : [constant] Typ [:= Expr];
            --
            --  Do not process tag-to-class-wide conversions because they do
            --  not yield an object. Do not process the incomplete view of a
            --  deferred constant. Note that an object initialized by means
            --  of a build-in-place function call may appear as a deferred
            --  constant after expansion activities. These kinds of objects
            --  must be finalized.

            elsif not Is_Imported (Obj_Id)
              and then Needs_Finalization (Obj_Typ)
              and then not Is_Tag_To_Class_Wide_Conversion (Obj_Id)
              and then not (Ekind (Obj_Id) = E_Constant
                             and then not Has_Completion (Obj_Id)
                             and then No (BIP_Initialization_Call (Obj_Id)))
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

            --  Processing for "hook" objects generated for transient objects
            --  declared inside an Expression_With_Actions.

            elsif Is_Access_Type (Obj_Typ)
              and then Present (Status_Flag_Or_Transient_Decl (Obj_Id))
              and then Nkind (Status_Flag_Or_Transient_Decl (Obj_Id)) =
                                                        N_Object_Declaration
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
              and then (Is_Simple_Protected_Type (Obj_Typ)
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

            --  Ignored Ghost object renamings do not need any cleanup actions
            --  because they will not appear in the final tree.

            elsif Is_Ignored_Ghost_Entity (Obj_Id) then
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

            --  Freeze nodes for ignored Ghost types do not need cleanup
            --  actions because they will never appear in the final tree.

            if Is_Ignored_Ghost_Entity (Typ) then
               null;

            elsif ((Is_Access_Type (Typ)
                      and then not Is_Access_Subprogram_Type (Typ)
                      and then Needs_Finalization
                                 (Available_View (Designated_Type (Typ))))
                    or else (Is_Type (Typ) and then Needs_Finalization (Typ)))
              and then Requires_Cleanup_Actions
                         (Actions (Decl), Lib_Level, Nested_Constructs)
            then
               return True;
            end if;

         --  Nested package declarations

         elsif Nested_Constructs
           and then Nkind (Decl) = N_Package_Declaration
         then
            Pack_Id := Defining_Entity (Decl);

            --  Do not inspect an ignored Ghost package because all code found
            --  within will not appear in the final tree.

            if Is_Ignored_Ghost_Entity (Pack_Id) then
               null;

            elsif Ekind (Pack_Id) /= E_Generic_Package
              and then Requires_Cleanup_Actions
                         (Specification (Decl), Lib_Level)
            then
               return True;
            end if;

         --  Nested package bodies

         elsif Nested_Constructs and then Nkind (Decl) = N_Package_Body then

            --  Do not inspect an ignored Ghost package body because all code
            --  found within will not appear in the final tree.

            if Is_Ignored_Ghost_Entity (Defining_Entity (Decl)) then
               null;

            elsif Ekind (Corresponding_Spec (Decl)) /= E_Generic_Package
              and then Requires_Cleanup_Actions (Decl, Lib_Level)
            then
               return True;
            end if;

         elsif Nkind (Decl) = N_Block_Statement
           and then

           --  Handle a rare case caused by a controlled transient object
           --  created as part of a record init proc. The variable is wrapped
           --  in a block, but the block is not associated with a transient
           --  scope.

           (Inside_Init_Proc

           --  Handle the case where the original context has been wrapped in
           --  a block to avoid interference between exception handlers and
           --  At_End handlers. Treat the block as transparent and process its
           --  contents.

             or else Is_Finalization_Wrapper (Decl))
         then
            if Requires_Cleanup_Actions (Decl, Lib_Level) then
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
      --  we are always OK because there will always be a target.

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

      --  The input type always comes from the expression, and we assume this
      --  is indeed always analyzed, so we can simply get the Etype.

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

      --  If either type is tagged, then we know the alignment is OK so Gigi
      --  will be able to use pointer punning.

      elsif Is_Tagged_Type (Otyp) or else Is_Tagged_Type (Ityp) then
         return True;

      --  If either type is a limited record type, we cannot do a copy, so say
      --  safe since there's nothing else we can do.

      elsif Is_Limited_Record (Otyp) or else Is_Limited_Record (Ityp) then
         return True;

      --  Conversions to and from packed array types are always ignored and
      --  hence are safe.

      elsif Is_Packed_Array_Impl_Type (Otyp)
        or else Is_Packed_Array_Impl_Type (Ityp)
      then
         return True;
      end if;

      --  The only other cases known to be safe is if the input type's
      --  alignment is known to be at least the maximum alignment for the
      --  target or if both alignments are known and the output type's
      --  alignment is no stricter than the input's. We can use the component
      --  type alignment for an array if a type is an unpacked array type.

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

      elsif Ialign /= No_Uint
        and then Oalign /= No_Uint
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

            --  If the subprogram is in the current declarative part and
            --  'access has been applied to it, generate an elaboration
            --  check at the beginning of the declarations of the body.

            if Nkind (N) = N_Subprogram_Body
              and then Address_Taken (Spec_Id)
              and then
                Ekind_In (Scope (Spec_Id), E_Block, E_Procedure, E_Function)
            then
               declare
                  Loc   : constant Source_Ptr := Sloc (N);
                  Decls : constant List_Id    := Declarations (N);
                  Chk   : Node_Id;

               begin
                  --  No need to generate this check if first entry in the
                  --  declaration list is a raise of Program_Error now.

                  if Present (Decls)
                    and then Nkind (First (Decls)) = N_Raise_Program_Error
                  then
                     return;
                  end if;

                  --  Otherwise generate the check

                  Chk :=
                    Make_Raise_Program_Error (Loc,
                      Condition =>
                        Make_Op_Eq (Loc,
                          Left_Opnd  => New_Occurrence_Of (Ent, Loc),
                          Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),
                      Reason    => PE_Access_Before_Elaboration);

                  if No (Decls) then
                     Set_Declarations (N, New_List (Chk));
                  else
                     Prepend (Chk, Decls);
                  end if;

                  Analyze (Chk);
               end;
            end if;
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

   ----------------------
   -- Side_Effect_Free --
   ----------------------

   function Side_Effect_Free
     (N            : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False) return Boolean
   is
      Typ : constant Entity_Id := Etype (N);
      --  Result type of the expression

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

         if not Side_Effect_Free (Prefix (N), Name_Req, Variable_Ref) then
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

   --  Start of processing for Side_Effect_Free

   begin
      --  If volatile reference, always consider it to have side effects

      if Is_Volatile_Reference (N) then
         return False;
      end if;

      --  Note on checks that could raise Constraint_Error. Strictly, if we
      --  take advantage of 11.6, these checks do not count as side effects.
      --  However, we would prefer to consider that they are side effects,
      --  since the back end CSE does not work very well on expressions which
      --  can raise Constraint_Error. On the other hand if we don't consider
      --  them to be side effect free, then we get some awkward expansions
      --  in -gnato mode, resulting in code insertions at a point where we
      --  do not have a clear model for performing the insertions.

      --  Special handling for entity names

      if Is_Entity_Name (N) then

         --  A type reference is always side effect free

         if Is_Type (Entity (N)) then
            return True;

         --  Variables are considered to be a side effect if Variable_Ref
         --  is set or if we have a volatile reference and Name_Req is off.
         --  If Name_Req is True then we can't help returning a name which
         --  effectively allows multiple references in any case.

         elsif Is_Variable (N, Use_Original_Node => False) then
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

      elsif not Tagged_Type_Expansion
        and then not Comes_From_Source (N)
        and then Nkind (Parent (N)) = N_Object_Renaming_Declaration
        and then Is_Class_Wide_Type (Typ)
      then
         return True;

      --  Generating C the type conversion of an access to constrained array
      --  type into an access to unconstrained array type involves initializing
      --  a fat pointer and the expression cannot be assumed to be free of side
      --  effects since it must referenced several times to compute its bounds.

      elsif Modify_Tree_For_C
        and then Nkind (N) = N_Type_Conversion
        and then Is_Access_Type (Typ)
        and then Is_Array_Type (Designated_Type (Typ))
        and then not Is_Constrained (Designated_Type (Typ))
      then
         return False;
      end if;

      --  For other than entity names and compile time known values,
      --  check the node kind for special processing.

      case Nkind (N) is

         --  An attribute reference is side effect free if its expressions
         --  are side effect free and its prefix is side effect free or
         --  is an entity reference.

         --  Is this right? what about x'first where x is a variable???

         when N_Attribute_Reference =>
            return
              Side_Effect_Free (Expressions (N), Name_Req, Variable_Ref)
                and then Attribute_Name (N) /= Name_Input
                and then (Is_Entity_Name (Prefix (N))
                           or else Side_Effect_Free
                                     (Prefix (N), Name_Req, Variable_Ref));

         --  A binary operator is side effect free if and both operands are
         --  side effect free. For this purpose binary operators include
         --  membership tests and short circuit forms.

         when N_Binary_Op
            | N_Membership_Test
            | N_Short_Circuit
         =>
            return Side_Effect_Free (Left_Opnd  (N), Name_Req, Variable_Ref)
                     and then
                   Side_Effect_Free (Right_Opnd (N), Name_Req, Variable_Ref);

         --  An explicit dereference is side effect free only if it is
         --  a side effect free prefixed reference.

         when N_Explicit_Dereference =>
            return Safe_Prefixed_Reference (N);

         --  An expression with action is side effect free if its expression
         --  is side effect free and it has no actions.

         when N_Expression_With_Actions =>
            return
              Is_Empty_List (Actions (N))
                and then Side_Effect_Free
                           (Expression (N), Name_Req, Variable_Ref);

         --  A call to _rep_to_pos is side effect free, since we generate
         --  this pure function call ourselves. Moreover it is critically
         --  important to make this exception, since otherwise we can have
         --  discriminants in array components which don't look side effect
         --  free in the case of an array whose index type is an enumeration
         --  type with an enumeration rep clause.

         --  All other function calls are not side effect free

         when N_Function_Call =>
            return
              Nkind (Name (N)) = N_Identifier
                and then Is_TSS (Name (N), TSS_Rep_To_Pos)
                and then Side_Effect_Free
                           (First (Parameter_Associations (N)),
                            Name_Req, Variable_Ref);

         --  An IF expression is side effect free if it's of a scalar type, and
         --  all its components are all side effect free (conditions and then
         --  actions and else actions). We restrict to scalar types, since it
         --  is annoying to deal with things like (if A then B else C)'First
         --  where the type involved is a string type.

         when N_If_Expression =>
            return
              Is_Scalar_Type (Typ)
                and then Side_Effect_Free
                           (Expressions (N), Name_Req, Variable_Ref);

         --  An indexed component is side effect free if it is a side
         --  effect free prefixed reference and all the indexing
         --  expressions are side effect free.

         when N_Indexed_Component =>
            return
              Side_Effect_Free (Expressions (N), Name_Req, Variable_Ref)
                and then Safe_Prefixed_Reference (N);

         --  A type qualification, type conversion, or unchecked expression is
         --  side effect free if the expression is side effect free.

         when N_Qualified_Expression
            | N_Type_Conversion
            | N_Unchecked_Expression
         =>
            return Side_Effect_Free (Expression (N), Name_Req, Variable_Ref);

         --  A selected component is side effect free only if it is a side
         --  effect free prefixed reference.

         when N_Selected_Component =>
            return Safe_Prefixed_Reference (N);

         --  A range is side effect free if the bounds are side effect free

         when N_Range =>
            return Side_Effect_Free (Low_Bound (N),  Name_Req, Variable_Ref)
                     and then
                   Side_Effect_Free (High_Bound (N), Name_Req, Variable_Ref);

         --  A slice is side effect free if it is a side effect free
         --  prefixed reference and the bounds are side effect free.

         when N_Slice =>
            return
               Side_Effect_Free (Discrete_Range (N), Name_Req, Variable_Ref)
                 and then Safe_Prefixed_Reference (N);

         --  A unary operator is side effect free if the operand
         --  is side effect free.

         when N_Unary_Op =>
            return Side_Effect_Free (Right_Opnd (N), Name_Req, Variable_Ref);

         --  An unchecked type conversion is side effect free only if it
         --  is safe and its argument is side effect free.

         when N_Unchecked_Type_Conversion =>
            return
              Safe_Unchecked_Type_Conversion (N)
                and then Side_Effect_Free
                           (Expression (N), Name_Req, Variable_Ref);

         --  A literal is side effect free

         when N_Character_Literal
            | N_Integer_Literal
            | N_Real_Literal
            | N_String_Literal
         =>
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

   function Side_Effect_Free
     (L            : List_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False) return Boolean
   is
      N : Node_Id;

   begin
      if L = No_List or else L = Error_List then
         return True;

      else
         N := First (L);
         while Present (N) loop
            if not Side_Effect_Free (N, Name_Req, Variable_Ref) then
               return False;
            else
               Next (N);
            end if;
         end loop;

         return True;
      end if;
   end Side_Effect_Free;

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
      --  does not test that marginal case).

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
      --  does not test that marginal case).

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
   --  called (we don't want to compute it more than once).

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

   -------------------
   -- Type_Map_Hash --
   -------------------

   function Type_Map_Hash (Id : Entity_Id) return Type_Map_Header is
   begin
      return Type_Map_Header (Id mod Type_Map_Size);
   end Type_Map_Hash;

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

   -------------------------------
   -- Update_Primitives_Mapping --
   -------------------------------

   procedure Update_Primitives_Mapping
     (Inher_Id : Entity_Id;
      Subp_Id  : Entity_Id)
   is
   begin
      Map_Types
        (Parent_Type  => Find_Dispatching_Type (Inher_Id),
         Derived_Type => Find_Dispatching_Type (Subp_Id));
   end Update_Primitives_Mapping;

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

   --------------------------------
   -- Within_Internal_Subprogram --
   --------------------------------

   function Within_Internal_Subprogram return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then not Is_Subprogram (S) loop
         S := Scope (S);
      end loop;

      return Present (S)
        and then Get_TSS_Name (S) /= TSS_Null
        and then not Is_Predicate_Function (S)
        and then not Is_Predicate_Function_M (S);
   end Within_Internal_Subprogram;

end Exp_Util;
