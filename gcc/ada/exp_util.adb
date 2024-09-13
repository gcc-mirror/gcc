------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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
with Casing;         use Casing;
with Checks;         use Checks;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Aggr;       use Exp_Aggr;
with Exp_Ch6;        use Exp_Ch6;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Ch11;       use Exp_Ch11;
with Freeze;         use Freeze;
with Ghost;          use Ghost;
with Inline;         use Inline;
with Itypes;         use Itypes;
with Lib;            use Lib;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch12;       use Sem_Ch12;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Disp;       use Sem_Disp;
with Sem_Elab;       use Sem_Elab;
with Sem_Eval;       use Sem_Eval;
with Sem_Res;        use Sem_Res;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sinfo.Utils;    use Sinfo.Utils;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Validsw;        use Validsw;
with Warnsw;         use Warnsw;

with GNAT.HTable;
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
      No_Element => Empty,
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

   function Is_Uninitialized_Aggregate
     (Exp : Node_Id;
      T   : Entity_Id) return Boolean;
   --  Determine whether an array aggregate used in an object declaration
   --  is uninitialized, when the aggregate is declared with a box and
   --  the component type has no default value. Such an aggregate can be
   --  optimized away to prevent the copying of uninitialized data, and
   --  the bounds of the aggregate can be propagated directly to the
   --  object declaration.

   function Is_Verifiable_DIC_Pragma (Prag : Node_Id) return Boolean;
   --  Determine whether pragma Default_Initial_Condition denoted by Prag has
   --  an assertion expression that should be verified at run time.

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

   function Side_Effect_Free_Attribute (Name : Name_Id) return Boolean;
   --  Return True if the evaluation of the given attribute is considered
   --  side-effect-free, independently of its prefix and expressions.

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
              ("atomic synchronization set for &?.n?", Msg_Node);
         else
            Error_Msg_N
              ("atomic synchronization set?.n?", N);
         end if;
      end if;
   end Activate_Atomic_Synchronization;

   ----------------------
   -- Adjust_Condition --
   ----------------------

   procedure Adjust_Condition (N : Node_Id) is

      function Is_Hardbool_Type (T : Entity_Id) return Boolean;
      --  Return True iff T is a type annotated with the
      --  Machine_Attribute pragma "hardbool".

      ----------------------
      -- Is_Hardbool_Type --
      ----------------------

      function Is_Hardbool_Type (T : Entity_Id) return Boolean is

         function Find_Hardbool_Pragma
           (Id : Entity_Id) return Node_Id;
         --  Return a Rep_Item associated with entity Id that
         --  corresponds to the Hardbool Machine_Attribute pragma, if
         --  any, or Empty otherwise.

         function Pragma_Arg_To_String (Item : Node_Id) return String is
            (To_String (Strval (Expr_Value_S (Item))));
         --  Return the pragma argument Item as a String

         function Hardbool_Pragma_P (Item : Node_Id) return Boolean is
            (Nkind (Item) = N_Pragma
               and then
             Pragma_Name (Item) = Name_Machine_Attribute
               and then
             Pragma_Arg_To_String
               (Get_Pragma_Arg
                  (Next (First (Pragma_Argument_Associations (Item)))))
               = "hardbool");
         --  Return True iff representation Item is a "hardbool"
         --  Machine_Attribute pragma.

         --------------------------
         -- Find_Hardbool_Pragma --
         --------------------------

         function Find_Hardbool_Pragma
           (Id : Entity_Id) return Node_Id
         is
            Item : Node_Id;

         begin
            if not Has_Gigi_Rep_Item (Id) then
               return Empty;
            end if;

            Item := First_Rep_Item (Id);
            while Present (Item) loop
               if Hardbool_Pragma_P (Item) then
                  return Item;
               end if;
               Item := Next_Rep_Item (Item);
            end loop;

            return Empty;
         end Find_Hardbool_Pragma;

      --  Start of processing for Is_Hardbool_Type

      begin
         return Present (Find_Hardbool_Pragma (T));
      end Is_Hardbool_Type;

   --  Start of processing for Adjust_Condition

   begin
      if No (N) then
         return;
      end if;

      declare
         Loc : constant Source_Ptr := Sloc (N);
         T   : constant Entity_Id  := Etype (N);

      begin
         --  Defend against a call where the argument has no type, or has a
         --  type that is not Boolean. This can occur because of prior errors.

         if No (T) or else not Is_Boolean_Type (T) then
            return;
         end if;

         --  Apply validity checking if needed

         if Validity_Checks_On
           and then
             (Validity_Check_Tests or else Is_Hardbool_Type (T))

           --  no check needed here if validity has already been checked
           and then not
             (Validity_Check_Operands and then
               (Nkind (N) in N_Op or else Nkind (Parent (N)) in N_Op))
         then
            Ensure_Valid (N);
         end if;

         --  Immediate return if standard boolean, the most common case,
         --  where nothing needs to be done.

         if Base_Type (T) = Standard_Boolean then
            return;
         end if;

         --  Case of zero/nonzero semantics or nonstandard enumeration
         --  representation. In each case, we rewrite the node as:

         --      ityp!(N) /= False'Enum_Rep

         --  where ityp is an integer type with large enough size to hold any
         --  value of type T.

         if Nonzero_Is_True (T) or else Has_Non_Standard_Rep (T) then
            Rewrite (N,
              Make_Op_Ne (Loc,
                Left_Opnd  =>
                  Unchecked_Convert_To
                    (Integer_Type_For (Esize (T), Uns => False), N),
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
              or else (KP in N_Type_Conversion
                           | N_Unchecked_Type_Conversion
                        and then Is_Boolean_Type (Etype (Parent (N))))
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

   ----------------------------------------
   -- Attribute_Constrained_Static_Value --
   ----------------------------------------

   function Attribute_Constrained_Static_Value (Pref : Node_Id) return Boolean
   is
      Ptyp       : constant Entity_Id := Etype (Pref);
      Formal_Ent : constant Entity_Id := Param_Entity (Pref);

      function Is_Constrained_Aliased_View (Obj : Node_Id) return Boolean;
      --  Ada 2005 (AI-363): Returns True if the object name Obj denotes a
      --  view of an aliased object whose subtype is constrained.

      ---------------------------------
      -- Is_Constrained_Aliased_View --
      ---------------------------------

      function Is_Constrained_Aliased_View (Obj : Node_Id) return Boolean is
         E : Entity_Id;

      begin
         if Is_Entity_Name (Obj) then
            E := Entity (Obj);

            if Present (Renamed_Object (E)) then
               return Is_Constrained_Aliased_View (Renamed_Object (E));
            else
               return Is_Aliased (E) and then Is_Constrained (Etype (E));
            end if;

         else
            return Is_Aliased_View (Obj)
              and then
                (Is_Constrained (Etype (Obj))
                 or else
                   (Nkind (Obj) = N_Explicit_Dereference
                    and then
                      not Object_Type_Has_Constrained_Partial_View
                        (Typ  => Base_Type (Etype (Obj)),
                         Scop => Current_Scope)));
         end if;
      end Is_Constrained_Aliased_View;

   --  Start of processing for Attribute_Constrained_Static_Value

   begin
      --  We are in a case where the attribute is known statically, and
      --  implicit dereferences have been rewritten.

      pragma Assert
        (not (Present (Formal_Ent)
              and then Ekind (Formal_Ent) /= E_Constant
              and then Present (Extra_Constrained (Formal_Ent)))
         and then
           not (Is_Access_Type (Etype (Pref))
                and then (not Is_Entity_Name (Pref)
                          or else Is_Object (Entity (Pref))))
         and then
           not (Nkind (Pref) = N_Identifier
                and then Ekind (Entity (Pref)) = E_Variable
                and then Present (Extra_Constrained (Entity (Pref)))));

      if Is_Entity_Name (Pref) then
         declare
            Ent : constant Entity_Id := Entity (Pref);
            Res : Boolean;

         begin
            --  (RM J.4) obsolescent cases

            if Is_Type (Ent) then

               --  Private type

               if Is_Private_Type (Ent) then
                  Res := not Has_Discriminants (Ent)
                    or else Is_Constrained (Ent);

               --  It not a private type, must be a generic actual type
               --  that corresponded to a private type. We know that this
               --  correspondence holds, since otherwise the reference
               --  within the generic template would have been illegal.

               else
                  if Is_Composite_Type (Underlying_Type (Ent)) then
                     Res := Is_Constrained (Ent);
                  else
                     Res := True;
                  end if;
               end if;

            else

               --  If the prefix is not a variable or is aliased, then
               --  definitely true; if it's a formal parameter without an
               --  associated extra formal, then treat it as constrained.

               --  Ada 2005 (AI-363): An aliased prefix must be known to be
               --  constrained in order to set the attribute to True.

               if not Is_Variable (Pref)
                 or else Present (Formal_Ent)
                 or else (Ada_Version < Ada_2005
                          and then Is_Aliased_View (Pref))
                 or else (Ada_Version >= Ada_2005
                          and then Is_Constrained_Aliased_View (Pref))
               then
                  Res := True;

               --  Variable case, look at type to see if it is constrained.
               --  Note that the one case where this is not accurate (the
               --  procedure formal case), has been handled above.

               --  We use the Underlying_Type here (and below) in case the
               --  type is private without discriminants, but the full type
               --  has discriminants. This case is illegal, but we generate
               --  it internally for passing to the Extra_Constrained
               --  parameter.

               else
                  --  In Ada 2012, test for case of a limited tagged type,
                  --  in which case the attribute is always required to
                  --  return True. The underlying type is tested, to make
                  --  sure we also return True for cases where there is an
                  --  unconstrained object with an untagged limited partial
                  --  view which has defaulted discriminants (such objects
                  --  always produce a False in earlier versions of
                  --  Ada). (Ada 2012: AI05-0214)

                  Res :=
                    Is_Constrained (Underlying_Type (Etype (Ent)))
                    or else
                      (Ada_Version >= Ada_2012
                       and then Is_Tagged_Type (Underlying_Type (Ptyp))
                       and then Is_Limited_Type (Ptyp));
               end if;
            end if;

            return Res;
         end;

      --  Prefix is not an entity name. These are also cases where we can
      --  always tell at compile time by looking at the form and type of the
      --  prefix. If an explicit dereference of an object with constrained
      --  partial view, this is unconstrained (Ada 2005: AI95-0363). If the
      --  underlying type is a limited tagged type, then Constrained is
      --  required to always return True (Ada 2012: AI05-0214).

      else
         return not Is_Variable (Pref)
           or else
             (Nkind (Pref) = N_Explicit_Dereference
              and then
                not Object_Type_Has_Constrained_Partial_View
                  (Typ  => Base_Type (Ptyp),
                   Scop => Current_Scope))
           or else Is_Constrained (Underlying_Type (Ptyp))
           or else (Ada_Version >= Ada_2012
                    and then Is_Tagged_Type (Underlying_Type (Ptyp))
                    and then Is_Limited_Type (Ptyp));
      end if;
   end Attribute_Constrained_Static_Value;

   ------------------------------------
   -- Build_Allocate_Deallocate_Proc --
   ------------------------------------

   procedure Build_Allocate_Deallocate_Proc
     (N    : Node_Id;
      Mark : Node_Id := Empty)
   is
      Is_Allocate : constant Boolean := Nkind (N) /= N_Free_Statement;

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

      Desig_Typ                : Entity_Id;
      Expr                     : Node_Id;
      Needs_Fin                : Boolean;
      Pool_Id                  : Entity_Id;
      Proc_To_Call             : Node_Id;
      Ptr_Typ                  : Entity_Id;
      Use_Secondary_Stack_Pool : Boolean;

   --  Start of processing for Build_Allocate_Deallocate_Proc

   begin
      --  Obtain the attributes of the allocation

      if Is_Allocate then
         if Nkind (N) in N_Assignment_Statement | N_Object_Declaration then
            Expr := Expression (N);
         else
            Expr := N;
         end if;

         --  Deal with type conversions created for interface types

         if Nkind (Expr) = N_Unchecked_Type_Conversion then
            Expr := Expression (Expr);
         end if;

         --  In certain cases, an allocator with a qualified expression may be
         --  relocated and used as the initialization expression of a temporary
         --  and the analysis of the declaration of this temporary may in turn
         --  create another temporary:

         --    before:
         --       Obj : Ptr_Typ := new Desig_Typ'(...);

         --    after:
         --       Tmp2 : Ptr_Typ := new Desig_Typ'(...);
         --       [constraint_error when Tmp2...]
         --       Tmp1 : Ptr_Typ := Tmp2
         --       Obj  : Ptr_Typ := Tmp1;

         --  Detect this case where we are invoked on Tmp1's declaration by
         --  recognizing Tmp2 and then proceed to its declaration instead.

         if Nkind (Expr) = N_Identifier
           and then Nkind (Parent (Entity (Expr))) = N_Object_Declaration
           and then Nkind (Expression (Parent (Entity (Expr)))) = N_Allocator
         then
            Build_Allocate_Deallocate_Proc (Parent (Entity (Expr)), Mark);
            return;
         end if;

         pragma Assert (Nkind (Expr) = N_Allocator);

         Ptr_Typ := Base_Type (Etype (Expr));
         Proc_To_Call := Procedure_To_Call (Expr);

      --  Obtain the attributes of the deallocation

      else
         Expr := Expression (N);
         Ptr_Typ := Base_Type (Etype (Expr));
         Proc_To_Call := Procedure_To_Call (N);
      end if;

      Pool_Id := Associated_Storage_Pool (Ptr_Typ);
      Desig_Typ := Available_View (Designated_Type (Ptr_Typ));

      --  Handle concurrent types

      if Is_Concurrent_Type (Desig_Typ)
        and then Present (Corresponding_Record_Type (Desig_Typ))
      then
         Desig_Typ := Corresponding_Record_Type (Desig_Typ);
      end if;

      Use_Secondary_Stack_Pool :=
        Is_RTE (Pool_Id, RE_SS_Pool)
          or else (Nkind (Expr) = N_Allocator
                    and then Is_RTE (Storage_Pool (Expr), RE_SS_Pool));

      --  Do not process allocations / deallocations without a pool

      if No (Pool_Id) then
         return;

      --  Do not process allocations from the return stack

      elsif Is_RTE (Pool_Id, RE_RS_Pool) then
         return;

      --  Do not process allocations on / deallocations from the secondary
      --  stack, except for access types used to implement indirect temps.

      elsif Use_Secondary_Stack_Pool
        and then not Old_Attr_Util.Indirect_Temps
                       .Is_Access_Type_For_Indirect_Temp (Ptr_Typ)
      then
         return;

      --  Optimize the case where we are using the default Global_Pool_Object,
      --  and we don't need the heavy finalization machinery.

      elsif Is_RTE (Pool_Id, RE_Global_Pool_Object)
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
          and then not Has_Relaxed_Finalization (Desig_Typ)
          and then not No_Heap_Finalization (Ptr_Typ);

      --  The allocation/deallocation of a controlled object must be associated
      --  with an attachment to/detachment from a finalization collection, but
      --  the implementation cannot guarantee this property for every anonymous
      --  access type, see Build_Anonymous_Collection.

      if Needs_Fin and then No (Finalization_Collection (Ptr_Typ)) then
         pragma Assert (Ekind (Ptr_Typ) = E_Anonymous_Access_Type);
         Needs_Fin := False;
      end if;

      if Needs_Fin then

         --  Do nothing if the access type may never allocate / deallocate
         --  objects.

         if No_Pool_Assigned (Ptr_Typ) then
            return;
         end if;

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
         Alloc_Expr   : Node_Id := Empty;
         Fin_Coll_Id  : Entity_Id;
         Proc_To_Call : Entity_Id;
         Ptr_Coll_Id  : Entity_Id;
         Subpool      : Node_Id := Empty;

      begin
         --  When we are building an allocator procedure, extract the allocator
         --  node for later processing and calculation of alignment.

         if Is_Allocate then
            --  Extract the qualified expression if there is one from the
            --  allocator.

            if Nkind (Expression (Expr)) = N_Qualified_Expression then
               Alloc_Expr := Expression (Expr);
            end if;
         end if;

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

            --  c) Finalization collection

            Fin_Coll_Id := Make_Temporary (Loc, 'C');
            Ptr_Coll_Id := Finalization_Collection (Ptr_Typ);

            --  Create the temporary which represents the collection of
            --  the expression. Generate:
            --
            --    C : Finalization_Collection_Ptr :=
            --          Finalization_Collection (Ptr_Typ)'Access
            --
            --  Handle the case where a collection is actually a pointer
            --  to a collection. This arises in build-in-place functions.

            Insert_Action (N,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Fin_Coll_Id,
                Object_Definition   =>
                  New_Occurrence_Of
                    (RTE (RE_Finalization_Collection_Ptr), Loc),
                  Expression        =>
                    (if not Needs_Fin
                      then Make_Null (Loc)
                      elsif Is_Access_Type (Etype (Ptr_Coll_Id))
                      then New_Occurrence_Of (Ptr_Coll_Id, Loc)
                      else
                        Make_Attribute_Reference (Loc,
                          Prefix         =>
                            New_Occurrence_Of (Ptr_Coll_Id, Loc),
                          Attribute_Name => Name_Unrestricted_Access))));

            Append_To (Actuals, New_Occurrence_Of (Fin_Coll_Id, Loc));
         end if;

         --  d) Address
         --  e) Storage_Size
         --  f) Alignment

         Append_To (Actuals, New_Occurrence_Of (Addr_Id, Loc));
         Append_To (Actuals, New_Occurrence_Of (Size_Id, Loc));

         --  Class-wide allocations without expressions and non-class-wide
         --  allocations can be performed without getting the alignment from
         --  the type's Type Specific Record.

         if (Is_Allocate and then No (Alloc_Expr))
           or else not Is_Class_Wide_Type (Desig_Typ)
         then
            Append_To (Actuals, New_Occurrence_Of (Alig_Id, Loc));

         --  For operations on class-wide types we obtain the value of
         --  alignment from the Type Specific Record of the relevant object.
         --  This is needed because the frontend expansion of class-wide types
         --  into equivalent types confuses the back end.

         else
            --  Generate:
            --     Obj.all'Alignment
            --   or
            --     Alloc_Expr'Alignment

            --  ... because 'Alignment applied to class-wide types is expanded
            --  into the code that reads the value of alignment from the TSD
            --  (see Expand_N_Attribute_Reference)

            Append_To (Actuals,
              Unchecked_Convert_To (RTE (RE_Storage_Offset),
                Make_Attribute_Reference (Loc,
                  Prefix         =>
                    (if No (Alloc_Expr) then
                       Make_Explicit_Dereference (Loc, Relocate_Node (Expr))
                     else
                       Relocate_Node (Expression (Alloc_Expr))),
                  Attribute_Name => Name_Alignment)));
         end if;

         --  g) Is_Controlled

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

               --  Finalize_Address is not generated in CodePeer mode because
               --  the body contains address arithmetic. So we don't want to
               --  generate the attach or detach in this case.

               if CodePeer_Mode then
                  null;

               --  Nothing to generate if the flag is statically false

               elsif Is_Entity_Name (Flag_Expr)
                 and then Entity (Flag_Expr) = Standard_False
               then
                  null;

               --  Generate:
               --    if F then
               --       Attach_Object_To_Collection
               --         (Temp.all'Address,
               --          Desig_Typ_FD'Access,
               --          Fin_Coll_Id.all);
               --    end if;

               elsif Is_Allocate then
                  declare
                     Stmt : Node_Id;
                     Temp : Entity_Id;

                  begin
                     --  The original allocator must have been rewritten by
                     --  the caller at this point and a temporary introduced.

                     case Nkind (N) is
                        when N_Assignment_Statement =>
                           Temp := New_Copy_Tree (Name (N));

                        when N_Object_Declaration =>
                           Temp :=
                             New_Occurrence_Of (Defining_Identifier (N), Loc);

                        when others =>
                           raise Program_Error;
                     end case;

                     Stmt :=
                       Make_If_Statement (Loc,
                         Condition       =>
                           New_Occurrence_Of (Flag_Id, Loc),
                         Then_Statements => New_List (
                           Make_Procedure_Call_Statement (Loc,
                             Name =>
                               New_Occurrence_Of
                                 (RTE (RE_Attach_Object_To_Collection), Loc),
                             Parameter_Associations => New_List (
                               Make_Address_For_Finalize (Loc,
                                 Make_Explicit_Dereference (Loc, Temp),
                                 Desig_Typ),
                               Make_Attribute_Reference (Loc,
                                 Prefix =>
                                   New_Occurrence_Of
                                    (Finalize_Address (Desig_Typ), Loc),
                                 Attribute_Name => Name_Unrestricted_Access),
                               Make_Explicit_Dereference (Loc,
                                 New_Occurrence_Of (Fin_Coll_Id, Loc))))));

                     --  If we have a mark past the initialization, then insert
                     --  the statement there, otherwise insert after either the
                     --  assignment or the last initialization statement of the
                     --  declaration of the temporary.

                     if Present (Mark) then
                        Insert_Action (Mark, Stmt, Suppress => All_Checks);

                     elsif Nkind (N) = N_Assignment_Statement then
                        Insert_After_And_Analyze
                          (N, Stmt, Suppress => All_Checks);

                     else
                        Insert_After_And_Analyze
                          (Find_Last_Init (N), Stmt, Suppress => All_Checks);
                     end if;
                  end;

               --  Generate:
               --    if F then
               --       Detach_Object_From_Collection (Temp.all'Address);
               --    end if;

               else
                  Insert_Action (N,
                    Make_If_Statement (Loc,
                      Condition       => New_Occurrence_Of (Flag_Id, Loc),
                      Then_Statements => New_List (
                        Make_Procedure_Call_Statement (Loc,
                          Name =>
                            New_Occurrence_Of
                              (RTE (RE_Detach_Object_From_Collection), Loc),
                          Parameter_Associations => New_List (
                            Make_Address_For_Finalize (Loc,
                              Make_Explicit_Dereference (Loc,
                                New_Occurrence_Of
                                  (Entity (Expression (N)), Loc)),
                                Desig_Typ))))),
                    Suppress => All_Checks);
               end if;

            end Is_Controlled;

         --  The object is not controlled

         else
            Append_To (Actuals, New_Occurrence_Of (Standard_False, Loc));
         end if;

         --  h) On_Subpool

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

         --  Create a custom Allocate/Deallocate routine which has identical
         --  profile to that of System.Storage_Pools, except for a secondary
         --  stack allocation where the profile must be identical to that of
         --  the System.Secondary_Stack.SS_Allocate procedure (deallocation
         --  is not supported for the secondary stack).

         declare
            function Pool_Param return Node_Id is (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Make_Temporary (Loc, 'P'),
                Parameter_Type      =>
                  New_Occurrence_Of (RTE (RE_Root_Storage_Pool), Loc)));
            --  P : Root_Storage_Pool

            function Address_Param return Node_Id is (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Addr_Id,
                Out_Present         => Is_Allocate,
                Parameter_Type      =>
                  New_Occurrence_Of (RTE (RE_Address), Loc)));
            --  A : [out] Address

            function Size_Param return Node_Id is (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Size_Id,
                Parameter_Type      =>
                  New_Occurrence_Of (RTE (RE_Storage_Count), Loc)));
            --  S : Storage_Count

            function Alignment_Param return Node_Id is (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Alig_Id,
                Parameter_Type      =>
                  New_Occurrence_Of (RTE (RE_Storage_Count), Loc)));
            --  L : Storage_Count

            Formal_Params : constant List_Id :=
              (if Use_Secondary_Stack_Pool
                then New_List (Address_Param, Size_Param, Alignment_Param)
                else
                  New_List
                    (Pool_Param, Address_Param, Size_Param, Alignment_Param));
            --  The list of formal parameters of the routine

         begin
            Insert_Action (N,
              Make_Subprogram_Body (Loc,
                Specification              =>
                  --  procedure Pnn
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name       => Proc_Id,
                    Parameter_Specifications => Formal_Params),

                Declarations               => No_List,

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (
                      Make_Procedure_Call_Statement (Loc,
                        Name                   =>
                          New_Occurrence_Of (Proc_To_Call, Loc),
                        Parameter_Associations => Actuals)))),
              Suppress => All_Checks);
         end;

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
     (Pragma_Or_Expr : Node_Id;
      Subp           : Entity_Id;
      Par_Subp       : Entity_Id;
      Adjust_Sloc    : Boolean)
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

         if Nkind (N) in N_Identifier | N_Expanded_Name | N_Operator_Symbol
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

      Par_Typ  : constant Entity_Id := Find_Dispatching_Type (Par_Subp);
      Subp_Typ : constant Entity_Id := Find_Dispatching_Type (Subp);

   --  Start of processing for Build_Class_Wide_Expression

   begin
      pragma Assert (Par_Typ /= Subp_Typ);

      Update_Primitives_Mapping (Par_Subp, Subp);
      Map_Formals (Par_Subp, Subp);
      Replace_Condition_Entities (Pragma_Or_Expr);
   end Build_Class_Wide_Expression;

   --------------------
   -- Build_DIC_Call --
   --------------------

   function Build_DIC_Call
     (Loc      : Source_Ptr;
      Obj_Name : Node_Id;
      Typ      : Entity_Id) return Node_Id
   is
      Proc_Id    : constant Entity_Id := DIC_Procedure (Typ);
      Formal_Typ : constant Entity_Id := Etype (First_Formal (Proc_Id));

   begin
      --  The DIC procedure has a null body if assertions are disabled or
      --  Assertion_Policy Ignore is in effect. In that case, it would be
      --  nice to generate a null statement instead of a call to the DIC
      --  procedure, but doing that seems to interfere with the determination
      --  of ECRs (early call regions) in SPARK. ???

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Id, Loc),
          Parameter_Associations => New_List (
            Unchecked_Convert_To (Formal_Typ, Obj_Name)));
   end Build_DIC_Call;

   ------------------------------
   -- Build_DIC_Procedure_Body --
   ------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_DIC_Procedure_Body
     (Typ         : Entity_Id;
      Partial_DIC : Boolean := False)
   is
      Pragmas_Seen : Elist_Id := No_Elist;
      --  This list contains all DIC pragmas processed so far. The list is used
      --  to avoid redundant Default_Initial_Condition checks.

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
        (DIC_Prag : Node_Id;
         Expr     : Node_Id;
         Stmts    : in out List_Id);
      --  Add a runtime check to verify assertion expression DIC_Expr of
      --  inherited pragma DIC_Prag. This routine applies class-wide pre-
      --  and postcondition-like runtime semantics to the check. Expr is
      --  the assertion expression after substitution has been performed
      --  (via Replace_References). All generated code is added to list Stmts.

      procedure Add_Inherited_DICs
        (T         : Entity_Id;
         Priv_Typ  : Entity_Id;
         Full_Typ  : Entity_Id;
         Obj_Id    : Entity_Id;
         Checks    : in out List_Id);
      --  Generate a DIC check for each inherited Default_Initial_Condition
      --  coming from all parent types of type T. Priv_Typ and Full_Typ denote
      --  the partial and full view of the parent type. Obj_Id denotes the
      --  entity of the _object formal parameter of the DIC procedure. All
      --  created checks are added to list Checks.

      procedure Add_Own_DIC
        (DIC_Prag : Node_Id;
         DIC_Typ  : Entity_Id;
         Obj_Id   : Entity_Id;
         Stmts    : in out List_Id);
      --  Add a runtime check to verify the assertion expression of pragma
      --  DIC_Prag. DIC_Typ is the owner of the DIC pragma. Obj_Id is the
      --  object to substitute in the assertion expression for any references
      --  to the current instance of the type All generated code is added to
      --  list Stmts.

      procedure Add_Parent_DICs
        (T      : Entity_Id;
         Obj_Id : Entity_Id;
         Checks : in out List_Id);
      --  Generate a Default_Initial_Condition check for each inherited DIC
      --  aspect coming from all parent types of type T. Obj_Id denotes the
      --  entity of the _object formal parameter of the DIC procedure. All
      --  created checks are added to list Checks.

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

         --  Add the pragma to the list of processed pragmas

         Append_New_Elmt (DIC_Prag, Pragmas_Seen);
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
        (DIC_Prag : Node_Id;
         Expr     : Node_Id;
         Stmts    : in out List_Id)
      is
      begin
         --  Once the DIC assertion expression is fully processed, add a check
         --  to the statements of the DIC procedure.

         Add_DIC_Check
           (DIC_Prag => DIC_Prag,
            DIC_Expr => Expr,
            Stmts    => Stmts);
      end Add_Inherited_Tagged_DIC;

      ------------------------
      -- Add_Inherited_DICs --
      ------------------------

      procedure Add_Inherited_DICs
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
         if No (Priv_Typ) and then No (Full_Typ) then
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
              and then Pragma_Name (Prag) = Name_Default_Initial_Condition
            then
               --  Nothing to do if the pragma was already processed

               if Contains (Pragmas_Seen, Prag) then
                  return;
               end if;

               --  Extract arguments of the Default_Initial_Condition pragma

               Prag_Expr_Arg := First (Pragma_Argument_Associations (Prag));
               Prag_Expr     := Expression_Copy (Prag_Expr_Arg);

               --  Pick up the implicit second argument of the pragma, which
               --  indicates the type that the pragma applies to.

               Prag_Typ_Arg  := Next (Prag_Expr_Arg);
               if Present (Prag_Typ_Arg) then
                  Prag_Typ := Get_Pragma_Arg (Prag_Typ_Arg);
               else
                  Prag_Typ := Empty;
               end if;

               --  The pragma applies to the partial view of the parent type

               if Present (Priv_Typ)
                 and then Present (Prag_Typ)
                 and then Entity (Prag_Typ) = Priv_Typ
               then
                  Par_Typ := Priv_Typ;

               --  The pragma applies to the full view of the parent type

               elsif Present (Full_Typ)
                 and then Present (Prag_Typ)
                 and then Entity (Prag_Typ) = Full_Typ
               then
                  Par_Typ := Full_Typ;

               --  Otherwise the pragma does not belong to the parent type and
               --  should not be considered.

               else
                  return;
               end if;

               --  Substitute references in the DIC expression that are related
               --  to the partial type with corresponding references related to
               --  the derived type (call to Replace_References below).

               Expr := New_Copy_Tree (Prag_Expr);

               Par_Proc := Partial_DIC_Procedure (Par_Typ);

               --  If there's not a partial DIC procedure (such as when a
               --  full type doesn't have its own DIC, but is inherited from
               --  a type with DIC), get the full DIC procedure.

               if No (Par_Proc) then
                  Par_Proc := DIC_Procedure (Par_Typ);
               end if;

               Replace_References
                 (Expr      => Expr,
                  Par_Typ   => Par_Typ,
                  Deriv_Typ => Deriv_Typ,
                  Par_Obj   => First_Formal (Par_Proc),
                  Deriv_Obj => Obj_Id);

               --  Why are there different actions depending on whether T is
               --  tagged? Can these be unified? ???

               if Is_Tagged_Type (T) then
                  Add_Inherited_Tagged_DIC
                    (DIC_Prag  => Prag,
                     Expr      => Expr,
                     Stmts     => Checks);

               else
                  Add_Inherited_DIC
                    (DIC_Prag  => Prag,
                     Par_Typ   => Par_Typ,
                     Deriv_Typ => Deriv_Typ,
                     Stmts     => Checks);
               end if;

               --  Leave as soon as we get a DIC pragma, since we'll visit
               --  the pragmas of the parents, so will get to any "inherited"
               --  pragmas that way.

               return;
            end if;

            Next_Rep_Item (Prag);
         end loop;
      end Add_Inherited_DICs;

      -----------------
      -- Add_Own_DIC --
      -----------------

      procedure Add_Own_DIC
        (DIC_Prag : Node_Id;
         DIC_Typ  : Entity_Id;
         Obj_Id   : Entity_Id;
         Stmts    : in out List_Id)
      is
         DIC_Args : constant List_Id   :=
                      Pragma_Argument_Associations (DIC_Prag);
         DIC_Arg  : constant Node_Id   := First (DIC_Args);
         DIC_Asp  : constant Node_Id   := Corresponding_Aspect (DIC_Prag);
         DIC_Expr : constant Node_Id   := Get_Pragma_Arg (DIC_Arg);

         --  Local variables

         Typ_Decl : constant Node_Id := Declaration_Node (DIC_Typ);

         Expr : Node_Id;

      --  Start of processing for Add_Own_DIC

      begin
         pragma Assert (Present (DIC_Expr));

         --  We need to preanalyze the expression itself inside a generic to
         --  be able to capture global references present in it.

         if Inside_A_Generic then
            Expr := DIC_Expr;
         else
            Expr := New_Copy_Tree (DIC_Expr);
         end if;

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
            Set_Expression_Copy (DIC_Asp, New_Copy_Tree (Expr));
         end if;

         --  Once the DIC assertion expression is fully processed, add a check
         --  to the statements of the DIC procedure (unless the type is an
         --  abstract type, in which case we don't want the possibility of
         --  generating a call to an abstract function of the type; such DIC
         --  procedures can never be called in any case, so not generating the
         --  check at all is OK).

         if not Is_Abstract_Type (DIC_Typ) or else GNATprove_Mode then
            Add_DIC_Check
              (DIC_Prag => DIC_Prag,
               DIC_Expr => Expr,
               Stmts    => Stmts);
         end if;
      end Add_Own_DIC;

      ---------------------
      -- Add_Parent_DICs --
      ---------------------

      procedure Add_Parent_DICs
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

         Op_Node  : Elmt_Id;
         Par_Prim : Entity_Id;
         Prim     : Entity_Id;

      begin
         --  Map the overridden primitive to the overriding one; required by
         --  Replace_References (called by Add_Inherited_DICs) to handle calls
         --  to parent primitives.

         Op_Node := First_Elmt (Primitive_Operations (T));
         while Present (Op_Node) loop
            Prim := Node (Op_Node);

            if Present (Overridden_Operation (Prim))
              and then Comes_From_Source (Prim)
            then
               Par_Prim := Overridden_Operation (Prim);

               --  Create a mapping of the form:
               --    parent type primitive -> derived type primitive

               Type_Map.Set (Par_Prim, Prim);
            end if;

            Next_Elmt (Op_Node);
         end loop;

         --  Climb the parent type chain

         Curr_Typ := T;
         loop
            --  Do not consider subtypes, as they inherit the DICs from their
            --  base types.

            Par_Typ := Base_Type (Etype (Base_Type (Curr_Typ)));

            --  Stop the climb once the root of the parent chain is
            --  reached.

            exit when Curr_Typ = Par_Typ;

            --  Process the DICs of the parent type

            Get_Views (Par_Typ, Priv_Typ, Full_Typ, Dummy_1, Dummy_2);

            --  Only try to inherit a DIC pragma from the parent type Par_Typ
            --  if it Has_Own_DIC pragma. The loop will proceed up the parent
            --  chain to find all types that have their own DIC.

            if Has_Own_DIC (Par_Typ) then
               Add_Inherited_DICs
                 (T         => T,
                  Priv_Typ  => Priv_Typ,
                  Full_Typ  => Full_Typ,
                  Obj_Id    => Obj_Id,
                  Checks    => Checks);
            end if;

            Curr_Typ := Par_Typ;
         end loop;
      end Add_Parent_DICs;

      --  Local variables

      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      DIC_Prag     : Node_Id;
      DIC_Typ      : Entity_Id;
      Dummy_1      : Entity_Id;
      Dummy_2      : Entity_Id;
      Proc_Body    : Node_Id;
      Proc_Body_Id : Entity_Id;
      Proc_Decl    : Node_Id;
      Proc_Id      : Entity_Id;
      Stmts        : List_Id := No_List;

      CRec_Typ : Entity_Id := Empty;
      --  The corresponding record type of Full_Typ

      Full_Typ : Entity_Id := Empty;
      --  The full view of the working type

      Obj_Id : Entity_Id := Empty;
      --  The _object formal parameter of the invariant procedure

      Part_Proc : Entity_Id := Empty;
      --  The entity of the "partial" invariant procedure

      Priv_Typ : Entity_Id := Empty;
      --  The partial view of the working type

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

      --  Use the first subtype when dealing with implicit base types

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

      --  Obtain both views of the type

      Get_Views (Work_Typ, Priv_Typ, Full_Typ, Dummy_1, CRec_Typ);

      --  The caller requests a body for the partial DIC procedure

      if Partial_DIC then
         Proc_Id   := Partial_DIC_Procedure (Work_Typ);

         --  The "full" DIC procedure body was already created

         --  Create a declaration for the "partial" DIC procedure if it
         --  is not available.

         if No (Proc_Id) then
            Build_DIC_Procedure_Declaration
              (Typ         => Work_Typ,
               Partial_DIC => True);

            Proc_Id := Partial_DIC_Procedure (Work_Typ);
         end if;

      --  The caller requests a body for the "full" DIC procedure

      else
         Proc_Id   := DIC_Procedure (Work_Typ);
         Part_Proc := Partial_DIC_Procedure (Work_Typ);

         --  Create a declaration for the "full" DIC procedure if it is
         --  not available.

         if No (Proc_Id) then
            Build_DIC_Procedure_Declaration (Work_Typ);
            Proc_Id := DIC_Procedure (Work_Typ);
         end if;
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

      Obj_Id := First_Formal (Proc_Id);
      pragma Assert (Present (Obj_Id));

      --  The "partial" DIC procedure verifies the DICs of the partial view
      --  only.

      if Partial_DIC then
         pragma Assert (Present (Priv_Typ));

         if Has_Own_DIC (Work_Typ) then  -- If we're testing this then maybe
            Add_Own_DIC        -- we shouldn't be calling Find_DIC_Typ above???
              (DIC_Prag => DIC_Prag,
               DIC_Typ  => DIC_Typ,  -- Should this just be Work_Typ???
               Obj_Id   => Obj_Id,
               Stmts    => Stmts);
         end if;

      --  Otherwise, the "full" DIC procedure verifies the DICs inherited from
      --  parent types, as well as indirectly verifying the DICs of the partial
      --  view by calling the "partial" DIC procedure.

      else
         --  Check the DIC of the partial view by calling the "partial" DIC
         --  procedure, unless the partial DIC body is empty. Generate:

         --    <Work_Typ>Partial_DIC (_object);

         if Present (Part_Proc) and then not Has_Null_Body (Part_Proc) then
            Append_New_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name                   => New_Occurrence_Of (Part_Proc, Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Obj_Id, Loc))));
         end if;

         --  Process inherited Default_Initial_Conditions for all parent types

         Add_Parent_DICs (Work_Typ, Obj_Id, Stmts);
      end if;

      End_Scope;

      --  Produce an empty completing body in the following cases:
      --    * Assertions are disabled
      --    * The DIC Assertion_Policy is Ignore

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

      Mutate_Ekind     (Proc_Body_Id, E_Subprogram_Body);
      Set_Etype        (Proc_Body_Id, Standard_Void_Type);
      Set_Scope        (Proc_Body_Id, Current_Scope);
      Set_SPARK_Pragma (Proc_Body_Id, SPARK_Pragma (Proc_Id));
      Set_SPARK_Pragma_Inherited
                       (Proc_Body_Id, SPARK_Pragma_Inherited (Proc_Id));

      --  Link both spec and body to avoid generating duplicates

      Set_Corresponding_Body (Proc_Decl, Proc_Body_Id);
      Set_Corresponding_Spec (Proc_Body, Proc_Id);

      --  The body should not be inserted into the tree when the context
      --  is a generic unit because it is not part of the template.
      --  Note that the body must still be generated in order to resolve the
      --  DIC assertion expression.

      if Inside_A_Generic then
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

   <<Leave>>
      Restore_Ghost_Region (Saved_GM, Saved_IGR);
   end Build_DIC_Procedure_Body;

   -------------------------------------
   -- Build_DIC_Procedure_Declaration --
   -------------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_DIC_Procedure_Declaration
     (Typ         : Entity_Id;
      Partial_DIC : Boolean := False)
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      DIC_Prag  : Node_Id;
      DIC_Typ   : Entity_Id;
      Proc_Decl : Node_Id;
      Proc_Id   : Entity_Id;
      Proc_Nam  : Name_Id;
      Typ_Decl  : Node_Id;

      CRec_Typ : Entity_Id;
      --  The corresponding record type of Full_Typ

      Full_Typ : Entity_Id;
      --  The full view of working type

      Obj_Id : Entity_Id;
      --  The _object formal parameter of the DIC procedure

      Priv_Typ : Entity_Id;
      --  The partial view of working type

      UFull_Typ : Entity_Id;
      --  The underlying full view of Full_Typ

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
      end if;

      --  Nothing to do if the type already has a "partial" DIC procedure

      if Partial_DIC then
         if Present (Partial_DIC_Procedure (Work_Typ)) then
            goto Leave;
         end if;

      --  Nothing to do if the type already has a "full" DIC procedure

      elsif Present (DIC_Procedure (Work_Typ)) then
         goto Leave;
      end if;

      --  The caller requests the declaration of the "partial" DIC procedure

      if Partial_DIC then
         Proc_Nam := New_External_Name (Chars (Work_Typ), "Partial_DIC");

      --  Otherwise the caller requests the declaration of the "full" DIC
      --  procedure.

      else
         Proc_Nam := New_External_Name (Chars (Work_Typ), "DIC");
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc, Chars => Proc_Nam);

      --  Perform minor decoration in case the declaration is not analyzed

      Mutate_Ekind               (Proc_Id, E_Procedure);
      Set_Etype                  (Proc_Id, Standard_Void_Type);
      Set_Is_DIC_Procedure       (Proc_Id);
      Set_Scope                  (Proc_Id, Current_Scope);
      Set_SPARK_Pragma           (Proc_Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (Proc_Id);

      Set_DIC_Procedure (Work_Typ, Proc_Id);

      --  The DIC procedure requires debug info when the assertion expression
      --  is subject to Source Coverage Obligations.

      if Generate_SCO then
         Set_Debug_Info_Needed (Proc_Id);
      end if;

      --  Obtain all views of the input type

      Get_Views (Work_Typ, Priv_Typ, Full_Typ, UFull_Typ, CRec_Typ);

      --  Associate the DIC procedure and various flags with all views

      Propagate_DIC_Attributes (Priv_Typ,  From_Typ => Work_Typ);
      Propagate_DIC_Attributes (Full_Typ,  From_Typ => Work_Typ);
      Propagate_DIC_Attributes (UFull_Typ, From_Typ => Work_Typ);
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

      Mutate_Ekind (Obj_Id, E_In_Parameter);
      Set_Etype (Obj_Id, Work_Typ);
      Set_Scope (Obj_Id, Proc_Id);

      Set_First_Entity (Proc_Id, Obj_Id);
      Set_Last_Entity  (Proc_Id, Obj_Id);

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
      --  is a generic unit because it is not part of the template.

      if Inside_A_Generic then
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
      Restore_Ghost_Region (Saved_GM, Saved_IGR);
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

                  --  The invariant procedure has a null body if assertions are
                  --  disabled or Assertion_Policy Ignore is in effect.

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
         if No (Priv_Typ) and then No (Full_Typ) then
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

         if Inherited and List_Inherited_Aspects then
            Error_Msg_Sloc := Sloc (Prag);
            Error_Msg_N
              ("info: & inherits `Invariant''Class` aspect from #?.l?", Typ);
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
         Expr          : Node_Id;
         Prag          : Node_Id;
         Prag_Asp      : Node_Id;
         Prag_Expr     : Node_Id;
         Prag_Expr_Arg : Node_Id;
         Prag_Typ      : Node_Id;
         Prag_Typ_Arg  : Node_Id;

      begin
         if No (T) then
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

               --  We need to preanalyze the expression itself inside a generic
               --  to be able to capture global references present in it.

               if Inside_A_Generic then
                  Expr := Prag_Expr;
               else
                  Expr := New_Copy_Tree (Prag_Expr);
               end if;

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
                  Set_Expression_Copy (Prag_Asp, New_Copy_Tree (Expr));
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

                  --  However, given that the invariant procedure is built by
                  --  the expander, it is not available compiling generic units
                  --  or when the sources have errors, since expansion is then
                  --  disabled.

                  pragma Assert (Present (Proc_Id)
                    or else not Expander_Active);

                  --  Generate:
                  --    <Comp_Typ>Invariant (T (_object).<Comp_Id>);

                  --  Note that the invariant procedure may have a null body if
                  --  assertions are disabled or Assertion_Policy Ignore is in
                  --  effect.

                  if Present (Proc_Id)
                    and then not Has_Null_Body (Proc_Id)
                  then
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
                  & "unchecked_union type &??", Comp_Id, T);
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

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

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

      --  Do not process the underlying full view of a private type. There is
      --  no way to get back to the partial view, plus the body will be built
      --  by the full view or the base type.

      if Is_Underlying_Full_View (Work_Typ) then
         return;

      --  The input type denotes the implementation base type of a constrained
      --  array type. Work with the first subtype as all invariant pragmas are
      --  on its rep item chain.

      elsif Ekind (Work_Typ) = E_Array_Type and then Is_Itype (Work_Typ) then
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

      Mutate_Ekind (Proc_Body_Id, E_Subprogram_Body);
      Set_Etype (Proc_Body_Id, Standard_Void_Type);
      Set_Scope (Proc_Body_Id, Current_Scope);

      --  Link both spec and body to avoid generating duplicates

      Set_Corresponding_Body (Proc_Decl, Proc_Body_Id);
      Set_Corresponding_Spec (Proc_Body, Proc_Id);

      --  The body should not be inserted into the tree when the context is
      --  a generic unit because it is not part of the template. Note
      --  that the body must still be generated in order to resolve the
      --  invariants.

      if Inside_A_Generic then
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
      Restore_Ghost_Region (Saved_GM, Saved_IGR);
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

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      Proc_Decl : Node_Id;
      Proc_Id   : Entity_Id;
      Proc_Nam  : Name_Id;
      Typ_Decl  : Node_Id;

      CRec_Typ : Entity_Id;
      --  The corresponding record type of Full_Typ

      Full_Typ : Entity_Id;
      --  The full view of working type

      Obj_Id : Entity_Id;
      --  The _object formal parameter of the invariant procedure

      Obj_Typ : Entity_Id;
      --  The type of the _object formal parameter

      Priv_Typ : Entity_Id;
      --  The partial view of working type

      UFull_Typ : Entity_Id;
      --  The underlying full view of Full_Typ

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

      Mutate_Ekind (Proc_Id, E_Procedure);
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

      if Generate_SCO then
         Set_Debug_Info_Needed (Proc_Id);
      end if;

      --  Obtain all views of the input type

      Get_Views (Work_Typ, Priv_Typ, Full_Typ, UFull_Typ, CRec_Typ);

      --  Associate the invariant procedure and various flags with all views

      Propagate_Invariant_Attributes (Priv_Typ,  From_Typ => Work_Typ);
      Propagate_Invariant_Attributes (Full_Typ,  From_Typ => Work_Typ);
      Propagate_Invariant_Attributes (UFull_Typ, From_Typ => Work_Typ);
      Propagate_Invariant_Attributes (CRec_Typ,  From_Typ => Work_Typ);

      --  The declaration of the invariant procedure is inserted after the
      --  declaration of the partial view as this allows for proper external
      --  visibility.

      if Present (Priv_Typ) then
         Typ_Decl := Declaration_Node (Priv_Typ);

      --  Anonymous arrays in object declarations have no explicit declaration
      --  so use the related object declaration as the insertion point.

      elsif Is_Itype (Work_Typ) and then Is_Array_Type (Work_Typ) then
         Typ_Decl := Associated_Node_For_Itype (Work_Typ);

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

      Mutate_Ekind (Obj_Id, E_In_Parameter);
      Set_Etype (Obj_Id, Obj_Typ);
      Set_Scope (Obj_Id, Proc_Id);

      Set_First_Entity (Proc_Id, Obj_Id);
      Set_Last_Entity  (Proc_Id, Obj_Id);

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
      --  is a generic unit because it is not part of the template.

      if Inside_A_Generic then
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
      Restore_Ghost_Region (Saved_GM, Saved_IGR);
   end Build_Invariant_Procedure_Declaration;

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
   --     Pref : String renames Task_Name;
   --     T1   : constant String := Index1'Image (Val1);
   --     ...
   --     Tn   : constant String := Indexn'Image (Valn);
   --     Len  : constant Integer :=
   --       Pref'Length + T1'Length + ... + Tn'Length + n + 1;
   --     --  Len includes commas and the end parentheses
   --
   --     Res  : String (1 .. Len);
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
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
             Expression          =>
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
             Constant_Present    => True,
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

      Set_Character_Literal_Name (Get_Char_Code ('('));

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name       =>
            Make_Indexed_Component (Loc,
              Prefix      => New_Occurrence_Of (Res, Loc),
              Expressions => New_List (New_Occurrence_Of (Pos, Loc))),
          Expression =>
            Make_Character_Literal (Loc,
              Chars              => Name_Find,
              Char_Literal_Value => UI_From_CC (Get_Char_Code ('(')))));

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

            Set_Character_Literal_Name (Get_Char_Code (','));

            Append_To (Stats,
              Make_Assignment_Statement (Loc,
                Name => Make_Indexed_Component (Loc,
                   Prefix => New_Occurrence_Of (Res, Loc),
                   Expressions => New_List (New_Occurrence_Of (Pos, Loc))),
                Expression =>
                  Make_Character_Literal (Loc,
                    Chars              => Name_Find,
                    Char_Literal_Value => UI_From_CC (Get_Char_Code (',')))));

            Append_To (Stats,
              Make_Assignment_Statement (Loc,
                Name         => New_Occurrence_Of (Pos, Loc),
                  Expression =>
                    Make_Op_Add (Loc,
                      Left_Opnd  => New_Occurrence_Of (Pos, Loc),
                      Right_Opnd => Make_Integer_Literal (Loc, 1))));
         end if;
      end loop;

      Set_Character_Literal_Name (Get_Char_Code (')'));

      Append_To (Stats,
        Make_Assignment_Statement (Loc,
          Name        =>
            Make_Indexed_Component (Loc,
              Prefix      => New_Occurrence_Of (Res, Loc),
              Expressions => New_List (New_Occurrence_Of (Len, Loc))),
           Expression =>
             Make_Character_Literal (Loc,
               Chars              => Name_Find,
               Char_Literal_Value => UI_From_CC (Get_Char_Code (')')))));
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

      Component_Suffix_Index : constant Int :=
        (if In_Init_Proc then -1 else 0);
      --  If an init proc calls Build_Task_Image_Decls twice for its
      --  _Parent component (to split early/late initialization), we don't
      --  want two decls with the same name. Hence, the -1 suffix.

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
                New_External_Name (Chars (Selector_Name (Id_Ref)), 'T',
                  Suffix_Index => Component_Suffix_Index));
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
          Constant_Present    => True,
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
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
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

      Set_Character_Literal_Name (Get_Char_Code ('.'));

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
                 UI_From_CC (Get_Char_Code ('.')))));

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

   ----------------------------------------
   -- Build_Temporary_On_Secondary_Stack --
   ----------------------------------------

   function Build_Temporary_On_Secondary_Stack
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Code : List_Id) return Entity_Id
   is
      Acc_Typ   : Entity_Id;
      Alloc     : Node_Id;
      Alloc_Obj : Entity_Id;

   begin
      pragma Assert (RTE_Available (RE_SS_Pool)
        and then not Needs_Finalization (Typ));

      Acc_Typ := Make_Temporary (Loc, 'A');
      Mutate_Ekind (Acc_Typ, E_Access_Type);
      Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));

      Append_To (Code,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Acc_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication =>
                New_Occurrence_Of (Typ, Loc))));

      Alloc :=
        Make_Allocator (Loc, Expression => New_Occurrence_Of (Typ, Loc));
      Set_No_Initialization (Alloc);

      Alloc_Obj := Make_Temporary (Loc, 'R');

      Append_To (Code,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Alloc_Obj,
          Constant_Present    => True,
          Object_Definition   =>
            New_Occurrence_Of (Acc_Typ, Loc),
          Expression          => Alloc));

      Set_Uses_Sec_Stack (Current_Scope);

      return Alloc_Obj;
   end Build_Temporary_On_Secondary_Stack;

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

   function Component_May_Be_Bit_Aligned
     (Comp      : Entity_Id;
      For_Slice : Boolean := False) return Boolean
   is
      UT : Entity_Id;

   begin
      --  If no component clause, then everything is fine, since the back end
      --  never misaligns from byte boundaries by default, even if there is a
      --  pragma Pack for the record.

      if No (Comp) or else No (Component_Clause (Comp)) then
         return False;
      end if;

      UT := Underlying_Type (Etype (Comp));

      --  It is only array and record types that cause trouble

      if not Is_Record_Type (UT) and then not Is_Array_Type (UT) then
         return False;

      --  If we know that we have a small (at most the maximum integer size)
      --  bit-packed array or record without variant part, then everything is
      --  fine, since the back end can handle these cases correctly, except if
      --  a slice is involved.

      elsif Known_Esize (Comp)
        and then Esize (Comp) <= System_Max_Integer_Size
        and then (Is_Bit_Packed_Array (UT)
                   or else (Is_Record_Type (UT)
                             and then not Has_Variant_Part (UT)))
        and then not For_Slice
      then
         return False;

      elsif not Known_Normalized_First_Bit (Comp) then
         return True;

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

   -------------------------------
   -- Convert_To_Actual_Subtype --
   -------------------------------

   procedure Convert_To_Actual_Subtype (Exp : Node_Id) is
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

      if Is_Protected_Type (Typ) then
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
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Renaming_Req : Boolean := False) return Node_Id
   is
      New_Exp : Node_Id;

   begin
      Remove_Side_Effects
        (Exp          => Exp,
         Name_Req     => Name_Req,
         Renaming_Req => Renaming_Req);

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

   -------------------------
   -- Enclosing_Init_Proc --
   -------------------------

   function Enclosing_Init_Proc return Entity_Id is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Is_Init_Proc (S) then
            return S;
         else
            S := Scope (S);
         end if;
      end loop;

      return Empty;
   end Enclosing_Init_Proc;

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

   -------------------
   -- Evaluate_Name --
   -------------------

   procedure Evaluate_Name (Nam : Node_Id) is
   begin
      case Nkind (Nam) is
         --  For an aggregate, force its evaluation

         when N_Aggregate =>
            Force_Evaluation (Nam);

         --  For an attribute reference or an indexed component, evaluate the
         --  prefix, which is itself a name, recursively, and then force the
         --  evaluation of all the subscripts (or attribute expressions).

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

                  if Is_Rewrite_Substitution (E) then
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

         --  For a function call, we evaluate the call; same for an operator

         when N_Function_Call
            | N_Op
         =>
            Force_Evaluation (Nam);

         --  For a qualified expression, we evaluate the expression

         when N_Qualified_Expression =>
            Evaluate_Name (Expression (Nam));

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

         --  The remaining cases are direct name and character literal. In all
         --  these cases, we do nothing, since we want to reevaluate each time
         --  the renamed object is used. ??? There are more remaining cases, at
         --  least in the GNATprove_Mode, where this routine is called in more
         --  contexts than in GNAT.

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

   -------------------------------
   -- Expand_Sliding_Conversion --
   -------------------------------

   procedure Expand_Sliding_Conversion (N : Node_Id; Arr_Typ : Entity_Id) is

      pragma Assert (Is_Array_Type (Arr_Typ)
                      and then not Is_Constrained (Arr_Typ)
                      and then Is_Fixed_Lower_Bound_Array_Subtype (Arr_Typ));

      Constraints : List_Id;
      Index       : Node_Id := First_Index (Arr_Typ);
      Loc         : constant Source_Ptr := Sloc (N);
      Subt_Decl   : Node_Id;
      Subt        : Entity_Id;
      Subt_Low    : Node_Id;
      Subt_High   : Node_Id;

      Act_Subt    : Entity_Id;
      Act_Index   : Node_Id;
      Act_Low     : Node_Id;
      Act_High    : Node_Id;
      Adjust_Incr : Node_Id;
      Dimension   : Int := 0;
      All_FLBs_Match : Boolean := True;

   begin
      --  This procedure is called during semantic analysis, and we only expand
      --  a sliding conversion when Expander_Active, to avoid doing it during
      --  preanalysis (which can lead to problems with the target subtype not
      --  getting properly expanded during later full analysis). Also, sliding
      --  should never be needed for string literals, because their bounds are
      --  determined directly based on the fixed lower bound of Arr_Typ and
      --  their length.

      if Expander_Active and then Nkind (N) /= N_String_Literal then
         Constraints := New_List;

         Act_Subt  := Get_Actual_Subtype (N);
         Act_Index := First_Index (Act_Subt);

         --  Loop over the indexes of the fixed-lower-bound array type or
         --  subtype to build up an index constraint for constructing the
         --  subtype that will be the target of a conversion of the array
         --  object that may need a sliding conversion.

         while Present (Index) loop
            pragma Assert (Present (Act_Index));

            Dimension := Dimension + 1;

            Get_Index_Bounds (Act_Index, Act_Low, Act_High);

            --  If Index defines a normal unconstrained range (range <>),
            --  then we will simply use the bounds of the actual subtype's
            --  corresponding index range.

            if not Is_Fixed_Lower_Bound_Index_Subtype (Etype (Index)) then
               Subt_Low  := Act_Low;
               Subt_High := Act_High;

            --  Otherwise, a range will be created with a low bound given by
            --  the fixed lower bound of the array subtype's index, and with
            --  high bound given by (Actual'Length + fixed lower bound - 1).

            else
               if Nkind (Index) = N_Subtype_Indication then
                  Subt_Low :=
                    New_Copy_Tree
                      (Low_Bound (Range_Expression (Constraint (Index))));
               else
                  pragma Assert (Nkind (Index) = N_Range);

                  Subt_Low := New_Copy_Tree (Low_Bound (Index));
               end if;

               --  If either we have a nonstatic lower bound, or the target and
               --  source subtypes are statically known to have unequal lower
               --  bounds, then we will need to make a subtype conversion to
               --  slide the bounds. However, if all of the indexes' lower
               --  bounds are static and known to be equal (the common case),
               --  then no conversion will be needed, and we'll end up not
               --  creating the subtype or the conversion (though we still
               --  build up the index constraint, which will simply be unused).

               if not (Compile_Time_Known_Value (Subt_Low)
                        and then Compile_Time_Known_Value (Act_Low))
                 or else Expr_Value (Subt_Low) /= Expr_Value (Act_Low)
               then
                  All_FLBs_Match := False;
               end if;

               --  Apply 'Pos to lower bound, which may be of an enumeration
               --  type, before subtracting.

               Adjust_Incr :=
                 Make_Op_Subtract (Loc,
                   Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (Etype (Act_Index), Loc),
                      Attribute_Name =>
                        Name_Pos,
                      Expressions    =>
                        New_List (New_Copy_Tree (Subt_Low))),
                   Make_Integer_Literal (Loc, 1));

               --  Apply 'Val to the result of adding the increment to the
               --  length, to handle indexes of enumeration types.

               Subt_High :=
                 Make_Attribute_Reference (Loc,
                   Prefix         =>
                     New_Occurrence_Of (Etype (Act_Index), Loc),
                   Attribute_Name =>
                     Name_Val,
                   Expressions    =>
                     New_List (Make_Op_Add (Loc,
                                 Make_Attribute_Reference (Loc,
                                   Prefix         =>
                                     New_Occurrence_Of (Act_Subt, Loc),
                                   Attribute_Name =>
                                     Name_Length,
                                   Expressions    =>
                                     New_List
                                       (Make_Integer_Literal
                                          (Loc, Dimension))),
                                 Adjust_Incr)));
            end if;

            Append (Make_Range (Loc, Subt_Low, Subt_High), Constraints);

            Next (Index);
            Next (Act_Index);
         end loop;

         --  If for each index with a fixed lower bound (FLB), the lower bound
         --  of the corresponding index of the actual subtype is statically
         --  known be equal to the FLB, then a sliding conversion isn't needed
         --  at all, so just return without building a subtype or conversion.

         if All_FLBs_Match then
            return;
         end if;

         --  A sliding conversion is needed, so create the target subtype using
         --  the index constraint created above, and rewrite the expression
         --  as a conversion to that subtype.

         Subt := Make_Temporary (Loc, 'S', Related_Node => N);
         Set_Is_Internal (Subt);

         Subt_Decl :=
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Subt,
             Subtype_Indication  =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (Arr_Typ,  Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => Constraints)));

         Mark_Rewrite_Insertion (Subt_Decl);

         --  The actual subtype is an Itype, so we analyze the declaration,
         --  but do not attach it to the tree.

         Set_Parent (Subt_Decl, N);
         Set_Is_Itype (Subt);
         Analyze (Subt_Decl, Suppress => All_Checks);
         Set_Associated_Node_For_Itype (Subt, N);
         Set_Has_Delayed_Freeze (Subt, False);

         --  We need to freeze the actual subtype immediately.  This is needed
         --  because otherwise this Itype will not get frozen at all, and it is
         --  always safe to freeze on creation because any associated types
         --  must be frozen at this point.

         Freeze_Itype (Subt, N);

         Rewrite (N,
                  Make_Type_Conversion (Loc,
                    Subtype_Mark =>
                      New_Occurrence_Of (Subt, Loc),
                    Expression   => Relocate_Node (N)));
         Analyze (N);
      end if;
   end Expand_Sliding_Conversion;

   -----------------------------------------
   -- Expand_Static_Predicates_In_Choices --
   -----------------------------------------

   procedure Expand_Static_Predicates_In_Choices (N : Node_Id) is
      pragma Assert (Nkind (N) in N_Case_Statement_Alternative | N_Variant);

      Choices : List_Id := Discrete_Choices (N);

      Choice : Node_Id;
      Next_C : Node_Id;
      P      : Node_Id;
      C      : Node_Id;

   begin
      --  If this is an "others" alternative, we need to process any static
      --  predicates in its Others_Discrete_Choices.

      if Nkind (First (Choices)) = N_Others_Choice then
         Choices := Others_Discrete_Choices (First (Choices));
      end if;

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

      Set_Has_SP_Choice (N, False);
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

   --     Val : Constrained_Subtype_Of_T := Maybe_Modified_Expr;
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

      --  In GNATprove mode, these extra subtypes are not needed, unless Exp is
      --  a static expression. In that case, the subtype will be constrained
      --  while the original type might be unconstrained, so expanding the type
      --  is necessary both for passing legality checks in GNAT and for precise
      --  analysis in GNATprove.

      if GNATprove_Mode and then not Is_Static_Expression (Exp) then
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
            --  the original slice, its value is frozen.

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
         if Is_Itype (Exp_Typ)

           --  When this is for an object declaration, the caller may want to
           --  set Is_Constr_Subt_For_U_Nominal on the subtype, so we must make
           --  sure that either the subtype has been built for the expression,
           --  typically for an aggregate, or the flag is already set on it;
           --  otherwise it could end up being set on the nominal constrained
           --  subtype of an object and thus later cause the failure to detect
           --  non-statically-matching subtypes on 'Access of this object.

           and then (Nkind (N) /= N_Object_Declaration
                      or else Nkind (Original_Node (Exp)) = N_Aggregate
                      or else Is_Constr_Subt_For_U_Nominal (Exp_Typ))
         then
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
            --  sem_ch8.Analyze_Package_Renaming and Sem_Type.Covers.)

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

      elsif Is_Inherently_Limited_Type (Exp_Typ)
        and then
         (Is_Class_Wide_Type (Exp_Typ)
           or else Is_Interface (Exp_Typ)
           or else not Has_Unknown_Discriminants (Exp_Typ)
           or else not Is_Composite_Type (Unc_Type))
      then
         null;

      --  For limited objects initialized with build-in-place function calls,
      --  nothing to be done; otherwise we prematurely introduce an N_Reference
      --  node in the expression initializing the object, which breaks the
      --  circuitry that detects and adds the additional arguments to the
      --  called function.

      elsif Is_Build_In_Place_Function_Call (Exp) then
         null;

     --  If the expression is an uninitialized aggregate, no need to build
     --  a subtype from the expression, because this may require the use of
     --  dynamic memory to create the object.

      elsif Is_Uninitialized_Aggregate (Exp, Exp_Typ) then
         Rewrite (Subtype_Indic, New_Occurrence_Of (Etype (Exp), Sloc (N)));
         if Nkind (N) = N_Object_Declaration then
            Set_Expression (N, Empty);
            Set_No_Initialization (N);
         end if;

      else
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
      Btyp : constant Entity_Id := Base_Type (Typ);
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

      if Is_Untagged_Derivation (Btyp) then
         if Is_Protected_Type (Btyp) then
            Utyp := Corresponding_Record_Type (Root_Type (Btyp));

         else
            Utyp := Underlying_Type (Root_Type (Btyp));

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

   -----------------------------
   -- Find_Controlled_Prim_Op --
   -----------------------------

   function Find_Controlled_Prim_Op
     (T : Entity_Id; Name : Name_Id) return Entity_Id
   is
      Op_Name : constant Name_Id := Name_Of_Controlled_Prim_Op (T, Name);

   begin
      if Op_Name = No_Name then
         return Empty;
      end if;

      return Find_Optional_Prim_Op (T, Op_Name);
   end Find_Controlled_Prim_Op;

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
      AI_Tag : Entity_Id := Empty;
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
         return First_Tag_Component (Typ);

      --  Otherwise we need to search for its associated tag component

      else
         Find_Tag (Typ);
         return AI_Tag;
      end if;
   end Find_Interface_Tag;

   --------------------
   -- Find_Last_Init --
   --------------------

   function Find_Last_Init (Decl : Node_Id) return Node_Id is
      Obj_Id : constant Entity_Id := Defining_Identifier (Decl);

      Init_Typ : Entity_Id;
      --  The initialization type of the related object declaration. Note
      --  that this is not necessarily the same type as Obj_Typ because of
      --  possible type derivations.

      Obj_Typ : Entity_Id;
      --  The (designated) type of the object declaration

      function Find_Last_Init_In_Block (Blk : Node_Id) return Node_Id;
      --  Find the last initialization call within the statements of block Blk

      function Is_Init_Call (N : Node_Id) return Boolean;
      --  Determine whether node N denotes one of the initialization procedures
      --  of types Init_Typ or Typ.

      function Next_Suitable_Statement (Stmt : Node_Id) return Node_Id;
      --  Obtain the next statement which follows list member Stmt while
      --  ignoring artifacts related to access-before-elaboration checks.

      -----------------------------
      -- Find_Last_Init_In_Block --
      -----------------------------

      function Find_Last_Init_In_Block (Blk : Node_Id) return Node_Id is
         HSS  : constant Node_Id := Handled_Statement_Sequence (Blk);

         Stmt : Node_Id;

      begin
         --  Examine the individual statements of the block in reverse to
         --  locate the last initialization call.

         if Present (HSS) and then Present (Statements (HSS)) then
            Stmt := Last (Statements (HSS));

            while Present (Stmt) loop
               --  Peek inside nested blocks in case aborts are allowed

               if Nkind (Stmt) = N_Block_Statement then
                  return Find_Last_Init_In_Block (Stmt);

               elsif Is_Init_Call (Stmt) then
                  return Stmt;
               end if;

               Prev (Stmt);
            end loop;
         end if;

         return Empty;
      end Find_Last_Init_In_Block;

      ------------------
      -- Is_Init_Call --
      ------------------

      function Is_Init_Call (N : Node_Id) return Boolean is
         function Is_Init_Proc_Of
           (Subp : Entity_Id;
            Typ  : Entity_Id) return Boolean;
         --  Determine whether subprogram Subp_Id is a valid init proc of
         --  type Typ.

         ---------------------
         -- Is_Init_Proc_Of --
         ---------------------

         function Is_Init_Proc_Of
           (Subp : Entity_Id;
            Typ  : Entity_Id) return Boolean
         is
            Deep_Init : Entity_Id := Empty;
            Prim_Init : Entity_Id := Empty;
            Type_Init : Entity_Id := Empty;

         begin
            --  Obtain all possible initialization routines of the
            --  related type and try to match the subprogram entity
            --  against one of them.

            --  Deep_Initialize

            Deep_Init := TSS (Typ, TSS_Deep_Initialize);

            --  Primitive Initialize

            if Is_Controlled (Typ) then
               Prim_Init := Find_Controlled_Prim_Op (Typ, Name_Initialize);

               if Present (Prim_Init) then
                  Prim_Init := Ultimate_Alias (Prim_Init);
               end if;
            end if;

            --  Type initialization routine

            if Has_Non_Null_Base_Init_Proc (Typ) then
               Type_Init := Base_Init_Proc (Typ);
            end if;

            return
              (Present (Deep_Init) and then Subp = Deep_Init)
                or else
              (Present (Prim_Init) and then Subp = Prim_Init)
                or else
              (Present (Type_Init) and then Subp = Type_Init);
         end Is_Init_Proc_Of;

         --  Local variables

         Call_Id : Entity_Id;

      --  Start of processing for Is_Init_Call

      begin
         if Nkind (N) = N_Procedure_Call_Statement
           and then Is_Entity_Name (Name (N))
         then
            Call_Id := Entity (Name (N));

            --  Consider both the type of the object declaration and its
            --  related initialization type.

            return
              Is_Init_Proc_Of (Call_Id, Init_Typ)
                or else
              Is_Init_Proc_Of (Call_Id, Obj_Typ);
         end if;

         return False;
      end Is_Init_Call;

      -----------------------------
      -- Next_Suitable_Statement --
      -----------------------------

      function Next_Suitable_Statement (Stmt : Node_Id) return Node_Id is
         Result : Node_Id;

      begin
         --  Skip call markers and Program_Error raises installed by the
         --  ABE mechanism.

         Result := Next (Stmt);
         while Present (Result) loop
            exit when Nkind (Result) not in
                        N_Call_Marker | N_Raise_Program_Error;

            Next (Result);
         end loop;

         return Result;
      end Next_Suitable_Statement;

      --  Local variables

      Call      : Node_Id;
      Last_Init : Node_Id;
      Stmt      : Node_Id;
      Stmt_2    : Node_Id;

      Deep_Init_Found : Boolean := False;
      --  A flag set when a call to [Deep_]Initialize has been found

   --  Start of processing for Find_Last_Init

   begin
      Last_Init := Decl;

      --  Objects that capture controlled function results do not require
      --  initialization.

      if Nkind (Decl) = N_Object_Declaration
        and then Nkind (Expression (Decl)) = N_Reference
      then
         return Last_Init;
      end if;

      Obj_Typ := Base_Type (Etype (Obj_Id));

      if Is_Access_Type (Obj_Typ) then
         Obj_Typ := Base_Type (Available_View (Designated_Type (Obj_Typ)));
      end if;

      --  Handle the initialization type of the object declaration

      if Is_Class_Wide_Type (Obj_Typ)
        and then Nkind (Decl) = N_Object_Declaration
        and then Nkind (Expression (Decl)) = N_Allocator
      then
         Init_Typ := Base_Type (Etype (Expression (Expression (Decl))));
      else
         Init_Typ := Obj_Typ;
      end if;

      loop
         if Is_Private_Type (Init_Typ)
           and then Present (Full_View (Init_Typ))
         then
            Init_Typ := Base_Type (Full_View (Init_Typ));

         elsif Is_Concurrent_Type (Init_Typ)
           and then Present (Corresponding_Record_Type (Init_Typ))
         then
            Init_Typ := Corresponding_Record_Type (Init_Typ);

         elsif Is_Untagged_Derivation (Init_Typ) then
            Init_Typ := Root_Type (Init_Typ);

         else
            exit;
         end if;
      end loop;

      if Present (Freeze_Node (Obj_Id)) then
         Stmt := First (Actions (Freeze_Node (Obj_Id)));
      else
         Stmt := Next_Suitable_Statement (Decl);
      end if;

      --  For an object with suppressed initialization, we check whether
      --  there is in fact no initialization expression. If there is not,
      --  then this is an object declaration that has been turned into a
      --  different object declaration that calls the build-in-place
      --  function in a 'Reference attribute, as in "F(...)'Reference".
      --  We search for that later object declaration, so that the
      --  attachment will be inserted after the call. Otherwise, if the
      --  call raises an exception, we will finalize the (uninitialized)
      --  object, which is wrong.

      if Nkind (Decl) = N_Object_Declaration
        and then No_Initialization (Decl)
      then
         if No (Expression (Last_Init)) then
            loop
               Next (Last_Init);

               exit when No (Last_Init);
               exit when Nkind (Last_Init) = N_Object_Declaration
                 and then Nkind (Expression (Last_Init)) = N_Reference
                 and then Nkind (Prefix (Expression (Last_Init))) =
                            N_Function_Call
                 and then Is_Expanded_Build_In_Place_Call
                            (Prefix (Expression (Last_Init)));
            end loop;
         end if;

         return Last_Init;

      --  If the initialization is in the declaration, we're done, so
      --  early return if we have no more statements or they have been
      --  rewritten, which means that they were in the source code.

      elsif No (Stmt) or else Original_Node (Stmt) /= Stmt then
         return Last_Init;

      --  In all other cases the initialization calls follow the related
      --  object. The general structure of object initialization built by
      --  routine Default_Initialize_Object is as follows:

      --   [begin                                --  aborts allowed
      --       Abort_Defer;]
      --       Type_Init_Proc (Obj);
      --      [begin]                            --  exceptions allowed
      --          Deep_Initialize (Obj);
      --      [exception                         --  exceptions allowed
      --          when others =>
      --             Deep_Finalize (Obj, Self => False);
      --             raise;
      --       end;]
      --   [at end                               --  aborts allowed
      --       Abort_Undefer;
      --    end;]

      --  When aborts are allowed, the initialization calls are housed
      --  within a block.

      elsif Nkind (Stmt) = N_Block_Statement then
         Call := Find_Last_Init_In_Block (Stmt);

         if Present (Call) then
            Last_Init := Call;
         end if;

      --  Otherwise the initialization calls follow the related object

      else
         Stmt_2 := Next_Suitable_Statement (Stmt);

         --  Check for an optional call to Deep_Initialize which may
         --  appear within a block depending on whether the object has
         --  controlled components.

         if Present (Stmt_2) then
            if Nkind (Stmt_2) = N_Block_Statement then
               Call := Find_Last_Init_In_Block (Stmt_2);

               if Present (Call) then
                  Deep_Init_Found := True;
                  Last_Init       := Call;
               end if;

            elsif Is_Init_Call (Stmt_2) then
               Deep_Init_Found := True;
               Last_Init       := Stmt_2;
            end if;
         end if;

         --  If the object lacks a call to Deep_Initialize, then it must
         --  have a call to its related type init proc.

         if not Deep_Init_Found and then Is_Init_Call (Stmt) then
            Last_Init := Stmt;
         end if;
      end if;

      return Last_Init;
   end Find_Last_Init;

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

      --  We cannot find the operation if there is no full view available

      if No (Typ) then
         return Empty;
      end if;

      --  Loop through primitive operations

      Prim := First_Elmt (Primitive_Operations (Typ));
      while Present (Prim) loop
         Op := Node (Prim);

         --  We can retrieve primitive operations by name if it is an internal
         --  name. For equality we must check that both of its operands have
         --  the same type, to avoid confusion with user-defined equalities
         --  than may have a asymmetric signature.

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
         if Ekind (S) in E_Entry | E_Entry_Family | E_Function | E_Procedure
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

   function Find_Storage_Op
     (Typ : Entity_Id;
      Nam : Name_Id) return Entity_Id
   is
      use Sem_Util.Storage_Model_Support;

   begin
      if Has_Storage_Model_Type_Aspect (Typ) then
         return Get_Storage_Model_Type_Entity (Typ, Nam);

      --  Otherwise we assume that Typ is a descendant of Root_Storage_Pool

      else
         return Find_Prim_Op (Typ, Nam);
      end if;
   end Find_Storage_Op;

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
            if Nkind (Original_Node (Par)) in
                 N_Case_Expression | N_If_Expression
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
              and then Nkind (Par) not in N_Component_Association
                                        | N_Discriminant_Association
                                        | N_Parameter_Association
                                        | N_Pragma_Argument_Association
                                        | N_Aggregate
                                        | N_Delta_Aggregate
                                        | N_Extension_Aggregate
                                        | N_Elsif_Part
              and then Nkind (Parent (Par)) not in N_Function_Call
                                                 | N_Procedure_Call_Statement
                                                 | N_Entry_Call_Statement
                                                 | N_Aggregate
                                                 | N_Delta_Aggregate
                                                 | N_Extension_Aggregate
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
              or else Nkind (Parent (Par)) in N_And_Then | N_Or_Else
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
              or else Nkind (Par) in N_Assignment_Statement
                                   | N_Object_Declaration
                                   | N_Pragma
                                   | N_Procedure_Call_Statement
                                   | N_Simple_Return_Statement
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
      Discr_Number  : Int       := 0;
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
         Discr_Number       => Discr_Number,
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
         Set_Casing (All_Upper_Case);
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

      procedure Process_Current_Value_Condition (N : Node_Id; S : Boolean);
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

            while Nkind (Cond) in N_Type_Conversion
                                | N_Qualified_Expression
                                | N_Expression_With_Actions
            loop
               Cond := Expression (Cond);
            end loop;

            exit when Cond = Prev_Cond;
         end loop;

         --  Deal with AND THEN and AND cases

         if Nkind (Cond) in N_And_Then | N_Op_And then

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
            pragma Assert (Op'Valid);

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

         elsif Nkind (Cond) in N_Type_Conversion
                             | N_Qualified_Expression
                             | N_Expression_With_Actions
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

      if not Is_Object (Ent) then
         return;
      end if;

      --  In GNATprove mode we don't want to use current value optimizer, in
      --  particular for loop invariant expressions and other assertions that
      --  act as cut points for proof. The optimizer often folds expressions
      --  into True/False where they trivially follow from the previous
      --  assignments, but this deprives proof from the information needed to
      --  discharge checks that are beyond the scope of the value optimizer.

      if GNATprove_Mode then
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

            --  In condition of IF statement

            elsif In_Subtree (N => Var, Root => Condition (CV)) then
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

            --  In condition of ELSIF part

            elsif In_Subtree (N => Var, Root => Condition (CV)) then
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

               --  In condition of while loop

               elsif In_Subtree (N => Var, Root => Condition (CV)) then
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

   -----------------------
   -- Get_Index_Subtype --
   -----------------------

   function Get_Index_Subtype (N : Node_Id) return Entity_Id is
      P_Type : Entity_Id := Etype (Prefix (N));
      Indx   : Node_Id;
      J      : Int;

   begin
      if Is_Access_Type (P_Type) then
         P_Type := Designated_Type (P_Type);
      end if;

      if No (Expressions (N)) then
         J := 1;
      else
         J := UI_To_Int (Expr_Value (First (Expressions (N))));
      end if;

      Indx := First_Index (P_Type);
      while J > 1 loop
         Next_Index (Indx);
         J := J - 1;
      end loop;

      return Etype (Indx);
   end Get_Index_Subtype;

   -----------------------
   -- Get_Mapped_Entity --
   -----------------------

   function Get_Mapped_Entity (E : Entity_Id) return Entity_Id is
   begin
      return Type_Map.Get (E);
   end Get_Mapped_Entity;

   ---------------------
   -- Get_Stream_Size --
   ---------------------

   function Get_Stream_Size (E : Entity_Id) return Uint is
   begin
      --  If we have a Stream_Size clause for this type use it

      if Has_Stream_Size_Clause (E) then
         return Static_Integer (Expression (Stream_Size_Clause (E)));

      --  Otherwise the Stream_Size is the size of the type

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

   ---------------------
   -- Has_Tag_Of_Type --
   ---------------------

   function Has_Tag_Of_Type (Exp : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Exp);

   begin
      pragma Assert (Is_Tagged_Type (Typ));

      --  The tag of an object of a class-wide type is that of its
      --  initialization expression.

      if Is_Class_Wide_Type (Typ) then
         return False;
      end if;

      --  The tag of a stand-alone object of a specific tagged type T
      --  identifies T.

      if Is_Entity_Name (Exp)
        and then Ekind (Entity (Exp)) in E_Constant | E_Variable
      then
         return True;

      else
         case Nkind (Exp) is
            --  The tag of a component or an aggregate of a specific tagged
            --  type T identifies T.

            when N_Indexed_Component
              |  N_Selected_Component
              |  N_Aggregate
              |  N_Extension_Aggregate
            =>
               return True;

            --  The tag of the result returned by a function whose result
            --  type is a specific tagged type T identifies T.

            when N_Function_Call =>
               return True;

            when N_Explicit_Dereference =>
               return Is_Captured_Function_Call (Exp);

            --  For a tagged type, the operand of a qualified expression
            --  shall resolve to be of the type of the expression.

            when N_Qualified_Expression =>
               return Has_Tag_Of_Type (Expression (Exp));

            when others =>
               return False;
         end case;
      end if;
   end Has_Tag_Of_Type;

   --------------------
   -- Homonym_Number --
   --------------------

   function Homonym_Number (Subp : Entity_Id) return Pos is
      Hom   : Entity_Id := Homonym (Subp);
      Count : Pos := 1;

   begin
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

   ----------------------------
   -- Init_Proc_Level_Formal --
   ----------------------------

   function Init_Proc_Level_Formal (Proc : Entity_Id) return Entity_Id is
      Form : Entity_Id;

   begin
      --  Go through the formals of the initialization procedure Proc to find
      --  the extra accessibility level parameter associated with the object
      --  being initialized.

      Form := First_Formal (Proc);
      while Present (Form) loop
         if Chars (Form) = Name_uInit_Level then
            return Form;
         end if;

         Next_Formal (Form);
      end loop;

      --  No formal was found, return Empty

      return Empty;
   end Init_Proc_Level_Formal;

   -------------------
   -- Insert_Action --
   -------------------

   procedure Insert_Action
     (Assoc_Node   : Node_Id;
      Ins_Action   : Node_Id;
      Spec_Expr_OK : Boolean := False)
   is
   begin
      if Present (Ins_Action) then
         Insert_Actions
           (Assoc_Node   => Assoc_Node,
            Ins_Actions  => New_List (Ins_Action),
            Spec_Expr_OK => Spec_Expr_OK);
      end if;
   end Insert_Action;

   --  Version with check(s) suppressed

   procedure Insert_Action
     (Assoc_Node   : Node_Id;
      Ins_Action   : Node_Id;
      Suppress     : Check_Id;
      Spec_Expr_OK : Boolean := False)
   is
   begin
      Insert_Actions
        (Assoc_Node   => Assoc_Node,
         Ins_Actions  => New_List (Ins_Action),
         Suppress     => Suppress,
         Spec_Expr_OK => Spec_Expr_OK);
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

   procedure Insert_Actions
     (Assoc_Node   : Node_Id;
      Ins_Actions  : List_Id;
      Spec_Expr_OK : Boolean := False)
   is
      N : Node_Id;
      P : Node_Id;

      Wrapped_Node : Node_Id := Empty;

   begin
      if Is_Empty_List (Ins_Actions) then
         return;
      end if;

      --  Insert the action when the context is "Handling of Default and Per-
      --  Object Expressions" only when requested by the caller.

      if Spec_Expr_OK then
         null;

      --  Ignore insert of actions from inside default expression (or other
      --  similar "spec expression") in the special spec-expression analyze
      --  mode. Any insertions at this point have no relevance, since we are
      --  only doing the analyze to freeze the types of any static expressions.
      --  See section "Handling of Default and Per-Object Expressions" in the
      --  spec of package Sem for further details.

      elsif In_Spec_Expression then
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

      --  Nonsubexpression case. Note that N is initially Empty in this case
      --  (N is only guaranteed non-Empty in the subexpr case).

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
                  --  moved to the proper place later when the if expression is
                  --  expanded.

                  elsif N = ThenX then
                     if Present (Then_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Then_Actions (P)), Ins_Actions);
                     else
                        Set_Then_Actions (P, Ins_Actions);
                        Analyze_List (Then_Actions (P));
                     end if;

                     return;

                  --  Else_Actions is treated the same as Then_Actions above

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
               if Present (Condition (P)) and then N = Condition (P) then
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
               | N_Single_Task_Declaration
               | N_Subprogram_Body
               | N_Subprogram_Body_Stub
               | N_Subprogram_Declaration
               | N_Subprogram_Renaming_Declaration
               | N_Subtype_Declaration
               | N_Task_Body
               | N_Task_Body_Stub

               --  Other things that can occur in stmt or decl lists

               | N_Itype_Reference

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

               elsif Nkind (Parent (P)) in N_Variant | N_Record_Definition then
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

            --  the expansion of Task and protected type declarations can
            --  create declarations for temporaries which, like other actions
            --  are inserted and analyzed before the current declaraation.
            --  However, the current scope is the synchronized type, and
            --  for unnesting it is critical that the proper scope for these
            --  generated entities be the enclosing one.

            when N_Task_Type_Declaration
               | N_Protected_Type_Declaration =>

               Push_Scope (Scope (Current_Scope));
               Insert_List_Before_And_Analyze (P, Ins_Actions);
               Pop_Scope;
               return;

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
               | N_Iterated_Element_Association
            =>
               if Nkind (Parent (P)) in N_Aggregate | N_Delta_Aggregate

                 --  We must not climb up out of an N_Iterated_xxx_Association
                 --  because the actions might contain references to the loop
                 --  parameter, except if we come from the Discrete_Choices of
                 --  N_Iterated_Component_Association which cannot contain any.
                 --  But it turns out that setting the Loop_Actions field in
                 --  the case of an N_Component_Association when the field was
                 --  not already set can lead to gigi assertion failures that
                 --  are presumably due to malformed trees, so don't do that.

                 and then
                   not (Nkind (P) = N_Iterated_Component_Association
                          and then Is_List_Member (N)
                          and then List_Containing (N) = Discrete_Choices (P))
                 and then
                   not (Nkind (P) = N_Component_Association
                          and then No (Loop_Actions (P)))
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
               end if;

            --  Special case: an attribute denoting a procedure call

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

            --  Special case: a marker

            when N_Call_Marker
               | N_Variable_Reference_Marker
            =>
               if Is_List_Member (P) then
                  Insert_List_Before_And_Analyze (P, Ins_Actions);
                  return;
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
               | N_External_Initializer
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
               | N_Interpolated_String_Literal
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
     (Assoc_Node   : Node_Id;
      Ins_Actions  : List_Id;
      Suppress     : Check_Id;
      Spec_Expr_OK : Boolean := False)
   is
   begin
      if Suppress = All_Checks then
         declare
            Sva : constant Suppress_Array := Scope_Suppress.Suppress;
         begin
            Scope_Suppress.Suppress := (others => True);
            Insert_Actions (Assoc_Node, Ins_Actions, Spec_Expr_OK);
            Scope_Suppress.Suppress := Sva;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress.Suppress (Suppress);
         begin
            Scope_Suppress.Suppress (Suppress) := True;
            Insert_Actions (Assoc_Node, Ins_Actions, Spec_Expr_OK);
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
   begin
      return Present (Enclosing_Init_Proc);
   end Inside_Init_Proc;

   ----------------------
   -- Integer_Type_For --
   ----------------------

   function Integer_Type_For (S : Uint; Uns : Boolean) return Entity_Id is
   begin
      pragma Assert
        (Standard_Long_Integer_Size in
         Standard_Integer_Size | Standard_Long_Long_Integer_Size);
      --  So we don't need to check for Standard_Long_Integer_Size below
      pragma Assert (S <= System_Max_Integer_Size);

      --  This is the canonical 32-bit type

      if S <= Standard_Integer_Size then
         if Uns then
            return Standard_Unsigned;
         else
            return Standard_Integer;
         end if;

      --  This is the canonical 64-bit type

      elsif S <= Standard_Long_Long_Integer_Size then
         if Uns then
            return Standard_Long_Long_Unsigned;
         else
            return Standard_Long_Long_Integer;
         end if;

      --  This is the canonical 128-bit type

      elsif S <= Standard_Long_Long_Long_Integer_Size then
         if Uns then
            return Standard_Long_Long_Long_Unsigned;
         else
            return Standard_Long_Long_Long_Integer;
         end if;

      else
         raise Program_Error;
      end if;
   end Integer_Type_For;

   -------------------------------
   -- Is_Captured_Function_Call --
   -------------------------------

   function Is_Captured_Function_Call (N : Node_Id) return Boolean is
   begin
      if Nkind (N) = N_Explicit_Dereference
        and then Is_Entity_Name (Prefix (N))
        and then Ekind (Entity (Prefix (N))) = E_Constant
      then
         declare
            Value : constant Node_Id := Constant_Value (Entity (Prefix (N)));

         begin
            return Present (Value)
              and then Nkind (Value) = N_Reference
              and then Nkind (Prefix (Value)) = N_Function_Call;
         end;

      else
         return False;
      end if;
   end Is_Captured_Function_Call;

   ------------------------------------------
   -- Is_Conversion_Or_Reference_To_Formal --
   ------------------------------------------

   function Is_Conversion_Or_Reference_To_Formal (N : Node_Id) return Boolean
   is
   begin
      return Nkind (N) in N_Type_Conversion | N_Unchecked_Type_Conversion
        or else (Nkind (N) = N_Explicit_Dereference
                  and then Nkind (Prefix (N)) in N_Type_Conversion
                                              |  N_Unchecked_Type_Conversion)
        or else (Is_Entity_Name (N)
                  and then Present (Entity (N))
                  and then Is_Formal (Entity (N)));
   end Is_Conversion_Or_Reference_To_Formal;

   --------------------------------------------------
   -- Is_Expanded_Class_Wide_Interface_Object_Decl --
   --------------------------------------------------

   function Is_Expanded_Class_Wide_Interface_Object_Decl
      (N : Node_Id) return Boolean is
   begin
      return not Comes_From_Source (N)
        and then Nkind (Original_Node (N)) = N_Object_Declaration
        and then Nkind (N) = N_Object_Renaming_Declaration
        and then Is_Class_Wide_Type (Etype (Defining_Identifier (N)))
        and then Is_Interface (Etype (Defining_Identifier (N)))
        and then Nkind (Name (N)) = N_Explicit_Dereference;
   end Is_Expanded_Class_Wide_Interface_Object_Decl;

   ------------------------------
   -- Is_Finalizable_Transient --
   ------------------------------

   function Is_Finalizable_Transient
     (Decl : Node_Id;
      N    : Node_Id) return Boolean
   is
      Obj_Id  : constant Entity_Id := Defining_Identifier (Decl);
      Obj_Typ : constant Entity_Id := Base_Type (Etype (Obj_Id));

      function Initialized_By_Aliased_BIP_Func_Call
        (Trans_Id : Entity_Id) return Boolean;
      --  Determine whether transient object Trans_Id is initialized by a
      --  build-in-place function call where the BIPalloc parameter either
      --  does not exist or is Caller_Allocation, and BIPaccess is not null.
      --  This case creates an aliasing between the returned value and the
      --  value denoted by BIPaccess.

      function Initialized_By_Reference (Trans_Id : Entity_Id) return Boolean;
      --  Determine whether transient object Trans_Id is initialized by a
      --  reference to another object. This is the only case where we can
      --  possibly finalize a transient object through an access value.

      function Is_Aliased
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean;
      --  Determine whether transient object Trans_Id has been renamed or
      --  aliased through 'reference in the statement list starting from
      --  First_Stmt.

      function Is_Indexed_Container
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean;
      --  Determine whether transient object Trans_Id denotes a container which
      --  is in the process of being indexed in the statement list starting
      --  from First_Stmt.

      function Is_Iterated_Container
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean;
      --  Determine whether transient object Trans_Id denotes a container which
      --  is in the process of being iterated in the statement list starting
      --  from First_Stmt.

      function Is_Part_Of_BIP_Return_Statement (N : Node_Id) return Boolean;
      --  Return True if N is directly part of a build-in-place return
      --  statement.

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

         Call := Unqual_Conv (Call);

         --  We search for a formal with a matching suffix. We can't search
         --  for the full name, because of the code at the end of Sem_Ch6.-
         --  Create_Extra_Formals, which copies the Extra_Formals over to
         --  the Alias of an instance, which will cause the formals to have
         --  "incorrect" names. See also Exp_Ch6.Build_In_Place_Formal.

         if Is_Build_In_Place_Function_Call (Call) then
            declare
               Caller_Allocation_Val : constant Uint :=
                 UI_From_Int (BIP_Allocation_Form'Pos (Caller_Allocation));
               Access_Suffix         : constant String :=
                 BIP_Formal_Suffix (BIP_Object_Access);
               Alloc_Suffix          : constant String :=
                 BIP_Formal_Suffix (BIP_Alloc_Form);

               function Has_Suffix (Name, Suffix : String) return Boolean;
               --  Return True if Name has suffix Suffix

               ----------------
               -- Has_Suffix --
               ----------------

               function Has_Suffix (Name, Suffix : String) return Boolean is
                  Len : constant Natural := Suffix'Length;

               begin
                  return Name'Length > Len
                    and then Name (Name'Last - Len + 1 .. Name'Last) = Suffix;
               end Has_Suffix;

               Access_OK  : Boolean := False;
               Alloc_OK   : Boolean := True;
               Param      : Node_Id;

            begin
               --  Examine all parameter associations of the function call

               Param := First (Parameter_Associations (Call));

               while Present (Param) loop
                  if Nkind (Param) = N_Parameter_Association
                    and then Nkind (Selector_Name (Param)) = N_Identifier
                  then
                     declare
                        Actual : constant Node_Id :=
                          Explicit_Actual_Parameter (Param);
                        Formal : constant Node_Id :=
                          Selector_Name (Param);
                        Name   : constant String :=
                          Get_Name_String (Chars (Formal));

                     begin
                        --  A nonnull BIPaccess has been found

                        if Has_Suffix (Name, Access_Suffix)
                          and then Nkind (Actual) /= N_Null
                        then
                           Access_OK := True;

                        --  A BIPalloc has been found

                        elsif Has_Suffix (Name, Alloc_Suffix)
                          and then Nkind (Actual) = N_Integer_Literal
                        then
                           Alloc_OK := Intval (Actual) = Caller_Allocation_Val;
                        end if;
                     end;
                  end if;

                  Next (Param);
               end loop;

               return Access_OK and Alloc_OK;
            end;
         end if;

         return False;
      end Initialized_By_Aliased_BIP_Func_Call;

      ------------------------------
      -- Initialized_By_Reference --
      ------------------------------

      function Initialized_By_Reference (Trans_Id : Entity_Id) return Boolean
      is
         Expr : constant Node_Id := Expression (Parent (Trans_Id));

      begin
         return Present (Expr) and then Nkind (Expr) = N_Reference;
      end Initialized_By_Reference;

      ----------------
      -- Is_Aliased --
      ----------------

      function Is_Aliased
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean
      is
         function Find_Renamed_Object (Ren_Decl : Node_Id) return Entity_Id;
         --  Given an object renaming declaration, retrieve the entity within
         --  the renamed name, recursively if this entity is itself a renaming.
         --  Return Empty if the renamed name contains anything other than a
         --  variable or a constant.

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
                 and then Ekind (Entity (N)) in E_Constant | E_Variable
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

            --  For renamings generated by Expand_N_Object_Declaration to deal
            --  with (class-wide) interface objects, there is an intermediate
            --  temporary of an anonymous access type used to hold the result
            --  of the displacement of the address of the renamed object.

            if Present (Ren_Obj)
              and then Ekind (Ren_Obj) = E_Constant
              and then Is_Itype (Etype (Ren_Obj))
              and then Ekind (Etype (Ren_Obj)) = E_Anonymous_Access_Type
              and then
                Is_Class_Wide_Type (Directly_Designated_Type (Etype (Ren_Obj)))
              and then
                Is_Interface (Directly_Designated_Type (Etype (Ren_Obj)))
            then
               Search (Constant_Value (Ren_Obj));
            end if;

            --  Recurse if Ren_Obj is itself a renaming

            if Present (Ren_Obj)
              and then Ekind (Ren_Obj) in E_Constant | E_Variable
              and then Present (Renamed_Object (Ren_Obj))
            then
               return Find_Renamed_Object (Declaration_Node (Ren_Obj));
            else
               return Ren_Obj;
            end if;
         end Find_Renamed_Object;

         --  Local variables

         Expr    : Node_Id;
         Ren_Obj : Entity_Id;
         Stmt    : Node_Id;

      --  Start of processing for Is_Aliased

      begin
         --  Examine the statements following the controlled object and look
         --  for various forms of aliasing.

         Stmt := First_Stmt;
         while Present (Stmt) loop
            --  Transient objects initialized by a reference are finalized
            --  (see Initialized_By_Reference above), so we must make sure
            --  not to finalize the referenced object twice. And we cannot
            --  finalize it at all if it is referenced by the nontransient
            --  object serviced by the transient scope.

            if Nkind (Stmt) = N_Object_Declaration then
               Expr := Expression (Stmt);

               --  Aliasing of the form:
               --    Obj : ... := Trans_Id'reference;

               if Present (Expr)
                 and then Nkind (Expr) = N_Reference
                 and then Is_Entity_Name (Prefix (Expr))
                 and then Entity (Prefix (Expr)) = Trans_Id
               then
                  return True;
               end if;

            --  (Transient) renamings are never finalized so we need not bother
            --  about finalizing transient renamed objects twice. Therefore, we
            --  we only need to look at the nontransient object serviced by the
            --  transient scope, if it exists and is declared as a renaming.

            elsif Nkind (Stmt) = N_Object_Renaming_Declaration
              and then Stmt = N
            then
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
      end Is_Aliased;

      --------------------------
      -- Is_Indexed_Container --
      --------------------------

      function Is_Indexed_Container
        (Trans_Id   : Entity_Id;
         First_Stmt : Node_Id) return Boolean
      is
         Aspect : Node_Id;
         Call   : Node_Id;
         Index  : Entity_Id;
         Param  : Node_Id;
         Stmt   : Node_Id;
         Typ    : Entity_Id;

      begin
         --  It is not possible to iterate over containers in non-Ada 2012 code

         if Ada_Version < Ada_2012 then
            return False;
         end if;

         Typ := Etype (Trans_Id);

         --  Handle access type created for the reference below

         if Is_Access_Type (Typ) then
            Typ := Designated_Type (Typ);
         end if;

         --  Look for aspect Constant_Indexing. It may be part of a type
         --  declaration for a container, or inherited from a base type
         --  or parent type.

         Aspect := Find_Value_Of_Aspect (Typ, Aspect_Constant_Indexing);

         if Present (Aspect) then
            Index := Entity (Aspect);

            --  Examine the statements following the container object and
            --  look for a call to the default indexing routine where the
            --  first parameter is the transient. Such a call appears as:

            --     It : Access_To_Constant_Reference_Type :=
            --            Constant_Indexing (Trans_Id.all, ...)'reference;

            Stmt := First_Stmt;
            while Present (Stmt) loop

               --  Detect an object declaration which is initialized by a
               --  controlled function call.

               if Nkind (Stmt) = N_Object_Declaration
                 and then Present (Expression (Stmt))
                 and then Nkind (Expression (Stmt)) = N_Reference
                 and then Nkind (Prefix (Expression (Stmt))) = N_Function_Call
               then
                  Call := Prefix (Expression (Stmt));

                  --  The call must invoke the default indexing routine of
                  --  the container and the transient object must appear as
                  --  the first actual parameter. Skip any calls whose names
                  --  are not entities.

                  if Is_Entity_Name (Name (Call))
                    and then Entity (Name (Call)) = Index
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
      end Is_Indexed_Container;

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

         --  Handle access type created for the reference below

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
            --            Iterate (Trans_Id.all, ...)'reference;

            Stmt := First_Stmt;
            while Present (Stmt) loop

               --  Detect an object declaration which is initialized by a
               --  controlled function call.

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

      -------------------------------------
      -- Is_Part_Of_BIP_Return_Statement --
      -------------------------------------

      function Is_Part_Of_BIP_Return_Statement (N : Node_Id) return Boolean is
         Subp    : constant Entity_Id := Current_Subprogram;
         Context : Node_Id;
      begin
         --  First check if N is part of a BIP function

         if No (Subp)
           or else not Is_Build_In_Place_Function (Subp)
         then
            return False;
         end if;

         --  Then check whether N is a complete part of a return statement
         --  Should we consider other node kinds to go up the tree???

         Context := N;
         loop
            case Nkind (Context) is
               when N_Expression_With_Actions => Context := Parent (Context);
               when N_Simple_Return_Statement => return True;
               when others                    => return False;
            end case;
         end loop;
      end Is_Part_Of_BIP_Return_Statement;

      --  Local variables

      Desig : Entity_Id := Obj_Typ;

   --  Start of processing for Is_Finalizable_Transient

   begin
      --  Handle access types

      if Is_Access_Type (Desig) then
         Desig := Available_View (Designated_Type (Desig));
      end if;

      return
        Ekind (Obj_Id) in E_Constant | E_Variable
          and then Needs_Finalization (Desig)
          and then Nkind (N) /= N_Simple_Return_Statement
          and then not Is_Part_Of_BIP_Return_Statement (N)

          --  Do not consider a transient object that was already processed

          and then not Is_Finalized_Transient (Obj_Id)

          --  Do not consider renamed or 'reference-d transient objects because
          --  the act of renaming extends the object's lifetime.

          and then not Is_Aliased (Obj_Id, Decl)

          --  If the transient object is of an access type, check that it is
          --  initialized by a reference to another object.

          and then (not Is_Access_Type (Obj_Typ)
                     or else Initialized_By_Reference (Obj_Id))

          --  Do not consider transient objects which act as indirect aliases
          --  of build-in-place function results.

          and then not Initialized_By_Aliased_BIP_Func_Call (Obj_Id)

          --  Do not consider iterators because those are treated as normal
          --  controlled objects and are processed by the usual finalization
          --  machinery. This avoids the double finalization of an iterator.

          and then not Is_Iterator (Desig)

          --  Do not consider containers in the context of iterator loops. Such
          --  transient objects must exist for as long as the loop is around,
          --  otherwise any operation carried out by the iterator will fail.

          and then not Is_Iterated_Container (Obj_Id, Decl)

          --  Likewise for indexed containers in the context of iterator loops

          and then not Is_Indexed_Container (Obj_Id, Decl);
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

      --  Here we have a tagged type, see if it has any component (other than
      --  tag and parent) with no component_clause. If so, we return False.

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

      --  All components have clauses

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
      T : constant Entity_Id := Etype (N);

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
            --  If component reference is for an array with nonstatic bounds,
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

            --  Case of component clause present which may specify an
            --  unaligned position.

            if Present (Component_Clause (C)) then

               --  Otherwise we can do a test to make sure that the actual
               --  start position in the record, and the length, are both
               --  consistent with the required alignment. If not, we know
               --  that we are unaligned.

               declare
                  Align_In_Bits : constant Nat := M * System_Storage_Unit;
                  Comp : Entity_Id;

               begin
                  Comp := C;

                  --  For a component inherited in a record extension, the
                  --  clause is inherited but position and size are not set.

                  if Is_Base_Type (Etype (P))
                    and then Is_Tagged_Type (Etype (P))
                    and then Present (Original_Record_Component (Comp))
                  then
                     Comp := Original_Record_Component (Comp);
                  end if;

                  if Component_Bit_Offset (Comp) mod Align_In_Bits /= 0
                    or else Esize (Comp) mod Align_In_Bits /= 0
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
               --  know what the alignment of the slice should be. But this
               --  really matters only if the target has strict alignment.

               if Target_Strict_Alignment
                 and then Known_Alignment (Ptyp)
                 and then (not Known_Alignment (Styp)
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
                      (not Known_Alignment (Styp)
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
      --  In the case of a function with a class-wide result that returns
      --  a call to a function with a specific result, we introduce a
      --  type conversion for the return expression. We do not want that
      --  type conversion to influence the result of this function.

      return
        Present (Expr)
          and then Nkind (Unqual_Conv (Expr)) = N_Explicit_Dereference
          and then (Nkind (Parent (Expr)) = N_Simple_Return_Statement
                     or else
                       (Nkind (Parent (Expr)) in N_Object_Declaration
                                               | N_Object_Renaming_Declaration
                         and then
                        Is_Return_Object (Defining_Entity (Parent (Expr)))));
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

      if Nkind (N) in N_Indexed_Component | N_Selected_Component then
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

      elsif Nkind (N) in N_Indexed_Component | N_Selected_Component then
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
      elsif Kind in N_Indexed_Component | N_Selected_Component then
         return Is_Renamed_Object (Pnod);
      else
         return False;
      end if;
   end Is_Renamed_Object;

   --------------------------------------
   -- Is_Secondary_Stack_BIP_Func_Call --
   --------------------------------------

   function Is_Secondary_Stack_BIP_Func_Call (Expr : Node_Id) return Boolean is
      Actual    : Node_Id;
      Call      : Node_Id := Expr;
      Formal    : Node_Id;
      Param     : Node_Id;

   begin
      --  Build-in-place calls usually appear in 'reference format. Note that
      --  the accessibility check machinery may add an extra 'reference due to
      --  side-effect removal.

      while Nkind (Call) = N_Reference loop
         Call := Prefix (Call);
      end loop;

      Call := Unqual_Conv (Call);

      if Is_Build_In_Place_Function_Call (Call) then

         --  Examine all parameter associations of the function call

         Param := First (Parameter_Associations (Call));
         while Present (Param) loop
            if Nkind (Param) = N_Parameter_Association then
               Formal := Selector_Name (Param);
               Actual := Explicit_Actual_Parameter (Param);

               --  A match for BIPalloc => 2 has been found

               if Is_Build_In_Place_Entity (Formal)
                 and then BIP_Suffix_Kind (Formal) = BIP_Alloc_Form
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

   ------------------------------
   -- Is_Secondary_Stack_Thunk --
   ------------------------------

   function Is_Secondary_Stack_Thunk (Id : Entity_Id) return Boolean is
   begin
      return Ekind (Id) = E_Function
        and then Is_Thunk (Id)
        and then Has_Controlling_Result (Id);
   end Is_Secondary_Stack_Thunk;

   ----------------------------
   -- Is_Statically_Disabled --
   ----------------------------

   function Is_Statically_Disabled
     (N             : Node_Id;
      Value         : Boolean;
      Include_Valid : Boolean)
      return Boolean
   is
      function Is_Discrete_Literal (N : Node_Id) return Boolean;
      --  Returns whether N is an integer, character or enumeration literal

      -------------------------
      -- Is_Discrete_Literal --
      -------------------------

      function Is_Discrete_Literal (N : Node_Id) return Boolean is
        (Nkind (N) in N_Integer_Literal | N_Character_Literal
          or else (Nkind (N) in N_Identifier | N_Expanded_Name
                    and then Ekind (Entity (N)) = E_Enumeration_Literal));

      Expr_N : constant Node_Id :=
        (if Is_Static_Expression (N)
           and then Entity (N) in Standard_True | Standard_False
           and then Is_Rewrite_Substitution (N)
         then Original_Node (N)
         else N);

   --  Start of processing for Is_Statically_Disabled

   begin
      --  A "statically disabled" condition which evaluates to Value is either:

      case Nkind (Expr_N) is

         --  an AND or AND THEN operator when:
         --  - Value is True and both operands are statically disabled
         --    conditions evaluated to True.
         --  - Value is False and at least one operand is a statically disabled
         --    condition evaluated to False.

         when N_Op_And | N_And_Then =>
            return
              (if Value then
                 (Is_Statically_Disabled
                    (Left_Opnd (Expr_N), Value, Include_Valid)
                  and then Is_Statically_Disabled
                    (Right_Opnd (Expr_N), Value, Include_Valid))
               else
                 (Is_Statically_Disabled
                    (Left_Opnd (Expr_N), Value, Include_Valid)
                  or else Is_Statically_Disabled
                    (Right_Opnd (Expr_N), Value, Include_Valid)));

         --  an OR or OR ELSE operator when:
         --  - Value is True and at least one operand is a statically disabled
         --    condition evaluated to True.
         --  - Value is False and both operands are statically disabled
         --    conditions evaluated to False.

         when N_Op_Or | N_Or_Else =>
            return
              (if Value then
                 (Is_Statically_Disabled
                    (Left_Opnd (Expr_N), Value, Include_Valid)
                  or else Is_Statically_Disabled
                    (Right_Opnd (Expr_N), Value, Include_Valid))
               else
                 (Is_Statically_Disabled
                    (Left_Opnd (Expr_N), Value, Include_Valid)
                  and then Is_Statically_Disabled
                    (Right_Opnd (Expr_N), Value, Include_Valid)));

         --  a NOT operator when the right operand is a statically disabled
         --  condition evaluated to the negation of Value.

         when N_Op_Not =>
            return Is_Statically_Disabled
              (Right_Opnd (Expr_N), not Value, Include_Valid);

         --  a static constant when it is of a boolean type with aspect
         --  Warnings Off.

         when N_Identifier | N_Expanded_Name =>
            return Is_Static_Expression (Expr_N)
              and then Value = Is_True (Expr_Value (Expr_N))
              and then Ekind (Entity (Expr_N)) = E_Constant
              and then Has_Warnings_Off (Entity (Expr_N));

         --  a relational_operator where one operand is a static constant with
         --  aspect Warnings Off and the other operand is a literal of the
         --  corresponding type.

         when N_Op_Compare =>
            declare
               Left  : constant Node_Id := Left_Opnd (Expr_N);
               Right : constant Node_Id := Right_Opnd (Expr_N);
            begin
               return
                 Is_Static_Expression (N)
                   and then Value = Is_True (Expr_Value (N))
                   and then
                     ((Is_Discrete_Literal (Right)
                         and then Nkind (Left) in N_Identifier
                                                | N_Expanded_Name
                         and then Ekind (Entity (Left)) = E_Constant
                         and then Has_Warnings_Off (Entity (Left)))
                      or else
                        (Is_Discrete_Literal (Left)
                           and then Nkind (Right) in N_Identifier
                                                   | N_Expanded_Name
                           and then Ekind (Entity (Right)) = E_Constant
                           and then Has_Warnings_Off (Entity (Right))));
            end;

         --  a reference to 'Valid or 'Valid_Scalar if Include_Valid is True

         when N_Attribute_Reference =>
            return Include_Valid
              and then Get_Attribute_Id (Attribute_Name (Expr_N)) in
                Attribute_Valid | Attribute_Valid_Scalars
              and then Value;

         when others =>
            return False;
      end case;
   end Is_Statically_Disabled;

   --------------------------------
   -- Is_Uninitialized_Aggregate --
   --------------------------------

   function Is_Uninitialized_Aggregate
     (Exp : Node_Id;
      T   : Entity_Id) return Boolean
   is
      Comp      : Node_Id;
      Comp_Type : Entity_Id;
      Typ       : Entity_Id;

   begin
      if Nkind (Exp) /= N_Aggregate then
         return False;
      end if;

      Preanalyze_And_Resolve (Exp, T);
      Typ  := Etype (Exp);

      if No (Typ)
        or else Ekind (Typ) /= E_Array_Subtype
        or else Present (Expressions (Exp))
        or else No (Component_Associations (Exp))
      then
         return False;
      else
         Comp_Type := Component_Type (Typ);
         Comp := First (Component_Associations (Exp));

         if not Box_Present (Comp)
           or else Present (Next (Comp))
         then
            return False;
         end if;

         return Is_Scalar_Type (Comp_Type)
           and then No (Default_Aspect_Component_Value (Typ));
      end if;
   end Is_Uninitialized_Aggregate;

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

   ------------------------------
   -- Is_Verifiable_DIC_Pragma --
   ------------------------------

   function Is_Verifiable_DIC_Pragma (Prag : Node_Id) return Boolean is
      Args : constant List_Id := Pragma_Argument_Associations (Prag);

   begin
      --  To qualify as verifiable, a DIC pragma must have a non-null argument

      return
        Present (Args)

          --  If there are args, but the first arg is Empty, then treat the
          --  pragma the same as having no args (there may be a second arg that
          --  is an implicitly added type arg, and Empty is a placeholder).

          and then Present (Get_Pragma_Arg (First (Args)))

          and then Nkind (Get_Pragma_Arg (First (Args))) /= N_Null;
   end Is_Verifiable_DIC_Pragma;

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

      elsif Is_Volatile_Object_Ref (N) then
         return True;

      --  True if reference to volatile entity

      elsif Is_Entity_Name (N) then
         return Treat_As_Volatile (Entity (N));

      --  True for slice of volatile array

      elsif Nkind (N) = N_Slice then
         return Is_Volatile_Reference (Prefix (N));

      --  True if volatile component

      elsif Nkind (N) in N_Indexed_Component | N_Selected_Component then
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

         --  Update the internal structures of the ABE mechanism in case the
         --  dead node is an elaboration scenario.

         Kill_Elaboration_Scenario (N);

         --  Generate warning if appropriate

         if W then

            --  We suppress the warning if this code is under control of an
            --  if/case statement and either
            --    a) we are in an instance and the condition/selector
            --       has a statically known value; or
            --    b) the selector of a case statement is a simple identifier
            --       and warnings off is set for this identifier; or
            --    c) the condition of an if statement is a "statically
            --       disabled" condition which evaluates to False as described
            --       in section 7.3.2 of SPARK User's Guide.
            --  Dead code is common and reasonable in instances, so we don't
            --  want a warning in that case.

            declare
               C : Node_Id := Empty;
            begin
               if Nkind (Parent (N)) = N_If_Statement then
                  C := Condition (Parent (N));

                  if Is_Statically_Disabled
                    (C, Value => False, Include_Valid => False)
                  then
                     W := False;
                  end if;

               elsif Nkind (Parent (N)) = N_Case_Statement_Alternative then
                  C := Expression (Parent (Parent (N)));

                  if Nkind (C) = N_Identifier
                    and then Present (Entity (C))
                    and then Has_Warnings_Off (Entity (C))
                  then
                     W := False;
                  end if;
               end if;

               if Present (C)
                 and then (In_Instance and Compile_Time_Known_Value (C))
               then
                  W := False;
               end if;
            end;

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

      N := First (L);
      while Present (N) loop
         Kill_Dead_Code (N, W);
         W := False;
         Next (N);
      end loop;
   end Kill_Dead_Code;

   -----------------------------
   -- Make_CW_Equivalent_Type --
   -----------------------------

   --  Create a record type used as an equivalent of any member of the class
   --  which takes its size from exp.

   --  Generate the following code:

   --   type Equiv_T is record
   --     _parent : T (List of discriminant constraints taken from Exp);
   --     Cnn : Storage_Array (1 .. (Exp'size - Typ'object_size)/Storage_Unit);
   --   end Equiv_T;
   --
   --  Note that this type does not guarantee same alignment as all derived
   --  types.
   --
   --  Note: for the freezing circuitry, this looks like a record extension,
   --  and so we need to make sure that the scalar storage order is the same
   --  as that of the parent type. (This does not change anything for the
   --  representation of the extension part.)

   function Make_CW_Equivalent_Type
     (T        : Entity_Id;
      E        : Node_Id;
      List_Def : out List_Id) return Entity_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      Root_Typ    : constant Entity_Id  := Root_Type (T);
      Root_Utyp   : constant Entity_Id  := Underlying_Type (Root_Typ);
      Comp_List   : constant List_Id    := New_List;

      Equiv_Type  : Entity_Id;
      Range_Type  : Entity_Id;
      Str_Type    : Entity_Id;
      Constr_Root : Entity_Id;
      Size_Attr   : Node_Id;
      Size_Expr   : Node_Id;

   begin
      List_Def := New_List;

      --  If the root type is already constrained, there are no discriminants
      --  in the expression.

      if not Has_Discriminants (Root_Typ)
        or else Is_Constrained (Root_Typ)
      then
         Constr_Root := Root_Typ;

         --  At this point in the expansion, nonlimited view of the type
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

      --  If the expression is known to have the tag of its type, then we can
      --  use it directly for the prefix of the Size attribute; otherwise we
      --  need to convert it first to the class-wide type to force a call to
      --  the _Size primitive operation.

      if No (E) then
         Size_Attr := Make_Integer_Literal (Loc, RM_Size (T));

      elsif Has_Tag_Of_Type (E) then
         if not Has_Discriminants (Etype (E))
           or else Is_Constrained (Etype (E))
         then
            Size_Attr :=
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Etype (E), Loc),
                Attribute_Name => Name_Object_Size);

         else
            Size_Attr :=
              Make_Attribute_Reference (Loc,
                Prefix => Duplicate_Subexpr_No_Checks (E),
                Attribute_Name => Name_Size);
         end if;

      else
         Size_Attr :=
           Make_Attribute_Reference (Loc,
             Prefix => OK_Convert_To (T, Duplicate_Subexpr_No_Checks (E)),
             Attribute_Name => Name_Size);
      end if;

      if not Is_Interface (Root_Typ) and then Present (E) then

         --  subtype rg__xx is
         --    Storage_Offset range 1 .. (Exp'size - Typ'object_size)
         --                                / Storage_Unit

         Size_Expr :=
           Make_Op_Subtract (Loc,
             Left_Opnd => Size_Attr,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Constr_Root, Loc),
                 Attribute_Name => Name_Object_Size));
      else
         --  subtype rg__xx is
         --    Storage_Offset range 1 .. (Exp'size - Ada.Tags.Tag'object_size)
         --                                / Storage_Unit

         Size_Expr :=
           Make_Op_Subtract (Loc,
             Left_Opnd => Size_Attr,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (RTE (RE_Tag), Loc),
                 Attribute_Name => Name_Object_Size));
      end if;

      Set_Paren_Count (Size_Expr, 1);

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
                        Left_Opnd => Size_Expr,
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
      --    _Parent : Snn;          -- not interface
      --    _Tag    : Ada.Tags.Tag  -- interface
      --    Cnn     : Str_Type;
      --  end Equiv_T;

      Equiv_Type := Make_Temporary (Loc, 'T');
      Mutate_Ekind (Equiv_Type, E_Record_Type);

      if not Is_Interface (Root_Typ) then
         Set_Parent_Subtype (Equiv_Type, Constr_Root);
      end if;

      --  Set Is_Class_Wide_Equivalent_Type very early to trigger the special
      --  treatment for this type. In particular, even though _parent's type
      --  is a controlled type or contains controlled components, we do not
      --  want to set Has_Controlled_Component on it to avoid making it gain
      --  an unwanted _controller component.

      Set_Is_Class_Wide_Equivalent_Type (Equiv_Type);

      --  A class-wide equivalent type does not require initialization unless
      --  no expression is present - in which case initialization gets
      --  generated as part of the mutably tagged type machinery.

      if Present (E) then
         Set_Suppress_Initialization (Equiv_Type);
      end if;

      if not Is_Interface (Root_Typ) and Present (E) then
         Append_To (Comp_List,
           Make_Component_Declaration (Loc,
             Defining_Identifier  =>
               Make_Defining_Identifier (Loc, Name_uParent),
             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => False,
                 Subtype_Indication => New_Occurrence_Of (Constr_Root, Loc))));

         Set_Reverse_Storage_Order
           (Equiv_Type, Reverse_Storage_Order (Base_Type (Root_Utyp)));
         Set_Reverse_Bit_Order
           (Equiv_Type, Reverse_Bit_Order (Base_Type (Root_Utyp)));

      else
         Append_To (Comp_List,
           Make_Component_Declaration (Loc,
             Defining_Identifier  =>
               Make_Defining_Identifier (Loc, Name_uTag),
             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => False,
                 Subtype_Indication =>
                   New_Occurrence_Of (RTE (RE_Tag), Loc))));

         Set_Is_Tag (Defining_Identifier (Last (Comp_List)));
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

      return Equiv_Type;
   end Make_CW_Equivalent_Type;

   -------------------------
   -- Make_Invariant_Call --
   -------------------------

   function Make_Invariant_Call (Expr : Node_Id) return Node_Id is
      Loc      : constant Source_Ptr := Sloc (Expr);
      Typ      : constant Entity_Id  := Base_Type (Etype (Expr));
      pragma Assert (Has_Invariants (Typ));
      Proc_Id  : constant Entity_Id := Invariant_Procedure (Typ);
      pragma Assert (Present (Proc_Id));
      Inv_Typ  : constant Entity_Id :=
        Base_Type (Etype (First_Formal (Proc_Id)));

      Arg : Node_Id;

   begin
      --  The invariant procedure has a null body if assertions are disabled or
      --  Assertion_Policy Ignore is in effect. In that case, generate a null
      --  statement instead of a call to the invariant procedure.

      if Has_Null_Body (Proc_Id) then
         return Make_Null_Statement (Loc);

      else
         --  As done elsewhere, for example in Build_Initialization_Call, we
         --  may need to bridge the gap between views of the type.

         if Inv_Typ /= Typ then
            Arg := OK_Convert_To (Inv_Typ, Expr);
         else
            Arg := Relocate_Node (Expr);
         end if;

         return
           Make_Procedure_Call_Statement (Loc,
             Name                   => New_Occurrence_Of (Proc_Id, Loc),
             Parameter_Associations => New_List (Arg));
      end if;
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
      Length_Expr : constant Node_Id :=
                      Make_Op_Subtract (Loc,
                        Left_Opnd  =>
                          Make_Integer_Literal (Loc,
                            Intval => String_Literal_Length (Literal_Typ)),
                        Right_Opnd => Make_Integer_Literal (Loc, 1));

      Hi : Node_Id;

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
             Prefix         => New_Occurrence_Of (Index, Loc),
             Expressions    => New_List (
               Make_Op_Add (Loc,
                 Left_Opnd  =>
                   Make_Attribute_Reference (Loc,
                     Attribute_Name => Name_Pos,
                     Prefix         => New_Occurrence_Of (Index, Loc),
                     Expressions    => New_List (New_Copy_Tree (Lo))),
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
     (Typ         : Entity_Id;
      Expr        : Node_Id;
      Static_Mem  : Boolean := False;
      Dynamic_Mem : Node_Id := Empty) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      Call         : Node_Id;
      Func_Id      : Entity_Id;
      Param_Assocs : List_Id;
   begin
      Func_Id := Predicate_Function (Typ);
      pragma Assert (Present (Func_Id));

      --  The related type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the call is properly marked as Ghost.

      Set_Ghost_Mode (Typ);

      --  Case of calling normal predicate function

      --  If the type is tagged, the expression may be class-wide, in which
      --  case it has to be converted to its root type, given that the
      --  generated predicate function is not dispatching. The conversion is
      --  type-safe and does not need validation, which matters when private
      --  extensions are involved.

      if Is_Tagged_Type (Typ) then
         Param_Assocs := New_List (OK_Convert_To (Typ, Relocate_Node (Expr)));
      else
         Param_Assocs := New_List (Relocate_Node (Expr));
      end if;

      if Predicate_Function_Needs_Membership_Parameter (Typ) then
         --  Pass in parameter indicating whether this call is for a
         --  membership test.
         Append ((if Present (Dynamic_Mem)
                    then Dynamic_Mem
                    else New_Occurrence_Of
                           (Boolean_Literals (Static_Mem), Loc)),
                 Param_Assocs);
      end if;

      Call :=
        Make_Function_Call (Loc,
          Name                   => New_Occurrence_Of (Func_Id, Loc),
          Parameter_Associations => Param_Assocs);

      Restore_Ghost_Region (Saved_GM, Saved_IGR);

      return Call;
   end Make_Predicate_Call;

   --------------------------
   -- Make_Predicate_Check --
   --------------------------

   function Make_Predicate_Check
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      --  Local variables

      Args : List_Id;
      Nam  : Name_Id;

   --  Start of processing for Make_Predicate_Check

   begin
      --  If predicate checks are suppressed, then return a null statement. For
      --  this call, we check only the scope setting. If the caller wants to
      --  check a specific entity's setting, they must do it manually.

      if Predicate_Checks_Suppressed (Empty) then
         return Make_Null_Statement (Loc);
      end if;

      --  Do not generate a check within stream functions and the like.

      if not Predicate_Check_In_Scope (Expr) then
         return Make_Null_Statement (Loc);
      end if;

      --  Compute proper name to use, we need to get this right so that the
      --  right set of check policies apply to the Check pragma we are making.
      --  The presence or not of a Ghost_Predicate does not influence the
      --  choice of the applicable check policy.

      if Has_Dynamic_Predicate_Aspect (Typ) then
         Nam := Name_Dynamic_Predicate;
      elsif Has_Static_Predicate_Aspect (Typ) then
         Nam := Name_Static_Predicate;
      else
         Nam := Name_Predicate;
      end if;

      Args := New_List (
        Make_Pragma_Argument_Association (Loc,
          Expression => Make_Identifier (Loc, Nam)),
        Make_Pragma_Argument_Association (Loc,
          Expression => Make_Predicate_Call (Typ, Expr)));

      --  If the subtype is subject to pragma Predicate_Failure, add the
      --  failure expression as an additional parameter.

      return
        Make_Pragma (Loc,
          Chars                        => Name_Check,
          Pragma_Argument_Associations => Args);
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

         Mutate_Ekind       (Priv_Subtyp, Subtype_Kind (Ekind (Unc_Typ)));
         Set_Etype          (Priv_Subtyp, Base_Type (Unc_Typ));
         Set_Scope          (Priv_Subtyp, Full_Subtyp);
         Set_Is_Constrained (Priv_Subtyp);
         Set_Is_Tagged_Type (Priv_Subtyp, Is_Tagged_Type (Unc_Typ));
         Set_Is_Itype       (Priv_Subtyp);
         Set_Associated_Node_For_Itype (Priv_Subtyp, E);

         Set_Direct_Primitive_Operations
           (Priv_Subtyp, Direct_Primitive_Operations (Unc_Typ));

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

            Next_Index (Index_Typ);
         end loop;

      elsif Is_Class_Wide_Type (Unc_Typ) then
         declare
            CW_Subtype : constant Entity_Id :=
                           New_Class_Wide_Subtype (Unc_Typ, E);
            Equiv_Def : List_Id;

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

               Set_Equivalent_Type
                 (CW_Subtype, Make_CW_Equivalent_Type (Unc_Typ, E, Equiv_Def));

                --  Suppress all checks during the analysis of the expanded
                --  code to avoid the generation of spurious warnings under
                --  ZFP run-time.

               Insert_Actions
                 (E, Equiv_Def, Suppress => All_Checks);
            end if;

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

   -----------------------------------
   -- Make_Tag_Assignment_From_Type --
   -----------------------------------

   function Make_Tag_Assignment_From_Type
     (Loc    : Source_Ptr;
      Target : Node_Id;
      Typ    : Entity_Id) return Node_Id
   is
      Nam : constant Node_Id :=
              Make_Selected_Component (Loc,
                Prefix => Target,
                Selector_Name =>
                  New_Occurrence_Of (First_Tag_Component (Typ), Loc));

   begin
      Set_Assignment_OK (Nam);

      return
        Make_Assignment_Statement (Loc,
          Name       => Nam,
          Expression =>
            Unchecked_Convert_To (RTE (RE_Tag),
              New_Occurrence_Of
                (Node (First_Elmt (Access_Disp_Table (Typ))), Loc)));
   end Make_Tag_Assignment_From_Type;

   -----------------------------
   -- Make_Variant_Comparison --
   -----------------------------

   function Make_Variant_Comparison
     (Loc      : Source_Ptr;
      Typ      : Entity_Id;
      Mode     : Name_Id;
      Curr_Val : Node_Id;
      Old_Val  : Node_Id) return Node_Id
   is
      function Big_Integer_Lt return Entity_Id;
      --  Returns the entity of the predefined "<" function from
      --  Ada.Numerics.Big_Numbers.Big_Integers.

      --------------------
      -- Big_Integer_Lt --
      --------------------

      function Big_Integer_Lt return Entity_Id is
         Big_Integers : constant Entity_Id :=
           RTU_Entity (Ada_Numerics_Big_Numbers_Big_Integers);

         E : Entity_Id := First_Entity (Big_Integers);

      begin
         while Present (E) loop
            if Chars (E) = Name_Op_Lt then
               return E;
            end if;
            Next_Entity (E);
         end loop;

         raise Program_Error;
      end Big_Integer_Lt;

   --  Start of processing for Make_Variant_Comparison

   begin
      if Mode = Name_Increases then
         return Make_Op_Gt (Loc, Curr_Val, Old_Val);

      else pragma Assert (Mode = Name_Decreases);

         --  For discrete expressions use the "<" operator

         if Is_Discrete_Type (Typ) then
            return Make_Op_Lt (Loc, Curr_Val, Old_Val);

         --  For Big_Integer expressions use the "<" function, because the
         --  operator on private type might not be visible and won't be
         --  resolved.

         else pragma Assert (Is_RTE (Base_Type (Typ), RE_Big_Integer));
            return
              Make_Function_Call (Loc,
                Name                   =>
                  New_Occurrence_Of (Big_Integer_Lt, Loc),
                Parameter_Associations =>
                  New_List (Curr_Val, Old_Val));
         end if;
      end if;
   end Make_Variant_Comparison;

   -----------------
   -- Map_Formals --
   -----------------

   procedure Map_Formals
     (Parent_Subp  : Entity_Id;
      Derived_Subp : Entity_Id;
      Force_Update : Boolean := False)
   is
      Par_Formal  : Entity_Id := First_Formal (Parent_Subp);
      Subp_Formal : Entity_Id := First_Formal (Derived_Subp);

   begin
      if Force_Update then
         Type_Map.Set (Parent_Subp, Derived_Subp);
      end if;

      --  At this stage either we are under regular processing and the caller
      --  has previously ensured that these primitives are already mapped (by
      --  means of calling previously to Update_Primitives_Mapping), or we are
      --  processing a late-overriding primitive and Force_Update updated above
      --  the mapping of these primitives.

      while Present (Par_Formal) and then Present (Subp_Formal) loop
         Type_Map.Set (Par_Formal, Subp_Formal);
         Next_Formal (Par_Formal);
         Next_Formal (Subp_Formal);
      end loop;
   end Map_Formals;

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
      --    * An entity which is either a discriminant or a nondiscriminant
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
            --  It is possible that an internally generated alias could be
            --  set to a subprogram which overrides the same aliased primitive,
            --  so return Empty in this case.

            if Ancestor_Primitive (Inher_Prim) = Subp then
               return Empty;
            end if;

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
               --  which results in a Stored discriminant:

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
            --  a Stored discriminant:

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

         --    2.2) The constraint is a name (aka Stored):

         --      Name : ...
         --      type Ancestor_1 (D_1 : ...) is tagged ...
         --      type Ancestor_2 is new Ancestor_1 (D_1 => Name) ...

         --    In this case the name is the final value of D_1 because the
         --    discriminant cannot be further constrained.

         --    2.3) The constraint is an expression (aka Stored):

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

         --    parent type primitive -> derived type primitive

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

      else
         return Small_Integer_Type_For (Siz, Is_Unsigned_Type (Typ));
      end if;
   end Matching_Standard_Type;

   -----------------------------
   -- May_Generate_Large_Temp --
   -----------------------------

   --  At the current time, the only types that we return False for (i.e. where
   --  we decide we know they cannot generate large temps) are ones where we
   --  know the size is 256 bits or less at compile time, and we are still not
   --  doing a thorough job on arrays and records.

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean is
   begin
      if not Size_Known_At_Compile_Time (Typ) then
         return False;
      end if;

      if Known_Esize (Typ) and then Esize (Typ) <= 256 then
         return False;
      end if;

      if Is_Array_Type (Typ)
        and then Present (Packed_Array_Impl_Type (Typ))
      then
         return May_Generate_Large_Temp (Packed_Array_Impl_Type (Typ));
      end if;

      return True;
   end May_Generate_Large_Temp;

   --------------------------------
   -- Name_Of_Controlled_Prim_Op --
   --------------------------------

   function Name_Of_Controlled_Prim_Op
     (Typ : Entity_Id;
      Nam : Name_Id) return Name_Id
   is
   begin
      pragma Assert (Is_Controlled (Typ));

      --  The aspect Finalizable may change the name of the primitives when
      --  present, but it's a GNAT extension.

      if Core_Extensions_Allowed then
         declare
            Rep : constant Node_Id :=
              Get_Rep_Item (Typ, Name_Finalizable, Check_Parents => True);

            Assoc : Node_Id;

         begin
            if Present (Rep) then
               Assoc := First (Component_Associations (Expression (Rep)));
               while Present (Assoc) loop
                  if Chars (First (Choices (Assoc))) = Nam then
                     return Chars (Expression (Assoc));
                  end if;

                  Next (Assoc);
               end loop;

               return No_Name;
            end if;
         end;
      end if;

      return Nam;
   end Name_Of_Controlled_Prim_Op;

   --------------------------------------------
   -- Needs_Conditional_Null_Excluding_Check --
   --------------------------------------------

   function Needs_Conditional_Null_Excluding_Check
     (Typ : Entity_Id) return Boolean
   is
   begin
      return
        Is_Array_Type (Typ) and then Can_Never_Be_Null (Component_Type (Typ));
   end Needs_Conditional_Null_Excluding_Check;

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
      --  Packed bit array types of size up to the maximum integer size are
      --  represented using a modular type with an initialization (to zero) and
      --  can be processed like other initialized scalar types.

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
      Res : constant Entity_Id := Create_Itype (E_Void, N);

      --  Capture relevant attributes of the class-wide subtype which must be
      --  restored after the copy.

      Res_Chars  : constant Name_Id   := Chars (Res);
      Res_Is_CGE : constant Boolean   := Is_Checked_Ghost_Entity (Res);
      Res_Is_IGE : constant Boolean   := Is_Ignored_Ghost_Entity (Res);
      Res_Is_IGN : constant Boolean   := Is_Ignored_Ghost_Node   (Res);
      Res_Scope  : constant Entity_Id := Scope (Res);

   begin
      Copy_Node (CW_Typ, Res);

      --  Restore the relevant attributes of the class-wide subtype

      Set_Chars                   (Res, Res_Chars);
      Set_Is_Checked_Ghost_Entity (Res, Res_Is_CGE);
      Set_Is_Ignored_Ghost_Entity (Res, Res_Is_IGE);
      Set_Is_Ignored_Ghost_Node   (Res, Res_Is_IGN);
      Set_Scope                   (Res, Res_Scope);

      --  Decorate the class-wide subtype

      Set_Associated_Node_For_Itype (Res, N);
      Set_Comes_From_Source         (Res, False);
      Mutate_Ekind                  (Res, E_Class_Wide_Subtype);
      Set_Etype                     (Res, Base_Type (CW_Typ));
      Set_Freeze_Node               (Res, Empty);
      Set_Is_Frozen                 (Res, False);
      Set_Is_Itype                  (Res);
      Set_Is_Public                 (Res, False);
      Set_Next_Entity               (Res, Empty);
      Set_Prev_Entity               (Res, Empty);
      Set_Sloc                      (Res, Sloc (N));

      Set_Public_Status (Res);

      return Res;
   end New_Class_Wide_Subtype;

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

   function Possible_Bit_Aligned_Component
     (N         : Node_Id;
      For_Slice : Boolean := False) return Boolean
   is
   begin
      --  Do not process an unanalyzed node because it is not yet decorated and
      --  most checks performed below will fail.

      if not Analyzed (N) then
         return False;
      end if;

      --  There are never alignment issues in CodePeer mode

      if CodePeer_Mode then
         return False;
      end if;

      case Nkind (N) is

         --  Case of indexed component

         when N_Indexed_Component =>
            declare
               P    : constant Node_Id   := Prefix (N);
               Ptyp : constant Entity_Id := Etype (P);

            begin
               --  If we know the component size and it is not larger than the
               --  maximum integer size, then we are OK. The back end does the
               --  assignment of small misaligned objects correctly.

               if Known_Static_Component_Size (Ptyp)
                 and then Component_Size (Ptyp) <= System_Max_Integer_Size
               then
                  return False;

               --  Otherwise, we need to test the prefix, to see if we are
               --  indexing from a possibly unaligned component.

               else
                  return Possible_Bit_Aligned_Component (P, For_Slice);
               end if;
            end;

         --  Case of selected component

         when N_Selected_Component =>
            declare
               P    : constant Node_Id   := Prefix (N);
               Comp : constant Entity_Id := Entity (Selector_Name (N));

            begin
               --  This is the crucial test: if the component itself causes
               --  trouble, then we can stop and return True.

               if Component_May_Be_Bit_Aligned (Comp, For_Slice) then
                  return True;

               --  Otherwise, we need to test the prefix, to see if we are
               --  selecting from a possibly unaligned component.

               else
                  return Possible_Bit_Aligned_Component (P, For_Slice);
               end if;
            end;

         --  For a slice, test the prefix, if that is possibly misaligned,
         --  then for sure the slice is.

         when N_Slice =>
            return Possible_Bit_Aligned_Component (Prefix (N), True);

         --  For an unchecked conversion, check whether the expression may
         --  be bit aligned.

         when N_Unchecked_Type_Conversion =>
            return Possible_Bit_Aligned_Component (Expression (N), For_Slice);

         --  If we have none of the above, it means that we have fallen off the
         --  top testing prefixes recursively, and we now have a stand alone
         --  object, where we don't have a problem, unless this is a renaming,
         --  in which case we need to look into the renamed object.

         when others =>
            return Is_Entity_Name (N)
              and then Is_Object (Entity (N))
              and then Present (Renamed_Object (Entity (N)))
              and then Possible_Bit_Aligned_Component
                         (Renamed_Object (Entity (N)), For_Slice);
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

         Add_Block_Identifier (Block_Nod, Block_Id, Scop);

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

            if Nkind (N) in N_Elsif_Part | N_If_Statement
              and then not Is_Empty_List (Then_Statements (N))
              and then not Are_Wrapped (Then_Statements (N))
              and then Requires_Cleanup_Actions
                         (L                 => Then_Statements (N),
                          Lib_Level         => False,
                          Nested_Constructs => False)
            then
               Block := Wrap_Statements_In_Block (Then_Statements (N));
               Set_Then_Statements (N, New_List (Block));

               Analyze (Block);
            end if;

            --  Check the "else statements" for conditional entry calls, if
            --  statements and selective accepts.

            if Nkind (N) in
                 N_Conditional_Entry_Call | N_If_Statement | N_Selective_Accept
              and then not Is_Empty_List (Else_Statements (N))
              and then not Are_Wrapped (Else_Statements (N))
              and then Requires_Cleanup_Actions
                         (L                 => Else_Statements (N),
                          Lib_Level         => False,
                          Nested_Constructs => False)
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
              and then Requires_Cleanup_Actions
                         (L                 => Statements (N),
                          Lib_Level         => False,
                          Nested_Constructs => False)
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

         --  Could be e.g. a loop that was transformed into a block or null
         --  statement. Do nothing for terminate alternatives.

         when N_Block_Statement
            | N_Null_Statement
            | N_Terminate_Alternative
         =>
            null;

         when others =>
            raise Program_Error;
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

   --  Start of processing for Remove_Init_Call

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
         --  If restrictions have forbidden Aborts, the initialization call
         --  for objects that require deep initialization has not been wrapped
         --  into the following block (see Exp_Ch3, Default_Initialize_Object)
         --  so if present remove it as well, and include the IP call in it,
         --  in the rare case the caller may need to simply displace the
         --  initialization, as is done for a later address specification.

         if Nkind (Next (Init_Call)) = N_Block_Statement
           and then Is_Initialization_Block (Next (Init_Call))
         then
            declare
               IP_Call : constant Node_Id := Init_Call;
            begin
               Init_Call := Next (IP_Call);
               Remove (IP_Call);
               Prepend (IP_Call,
                 Statements (Handled_Statement_Sequence (Init_Call)));
            end;
         end if;

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
      Discr_Number       : Int       := 0;
      Check_Side_Effects : Boolean   := True)
   is
      function Build_Temporary
        (Loc         : Source_Ptr;
         Id          : Character;
         Related_Nod : Node_Id := Empty) return Entity_Id;
      --  Create an external symbol of the form xxx_FIRST/_LAST if Related_Nod
      --  is present (xxx is taken from the Chars field of Related_Nod),
      --  otherwise it generates an internal temporary. The created temporary
      --  entity is marked as internal.

      function Possible_Side_Effect_In_SPARK (Exp : Node_Id) return Boolean;
      --  Computes whether a side effect is possible in SPARK, which should
      --  be handled by removing it from the expression for GNATprove. Note
      --  that other side effects related to volatile variables are handled
      --  separately.

      ---------------------
      -- Build_Temporary --
      ---------------------

      function Build_Temporary
        (Loc         : Source_Ptr;
         Id          : Character;
         Related_Nod : Node_Id := Empty) return Entity_Id
      is
         Temp_Id  : Entity_Id;
         Temp_Nam : Name_Id;
         Should_Set_Related_Expression : Boolean := False;

      begin
         --  The context requires an external symbol : expression is
         --  the bound of an array, or a discriminant value. We create
         --  a unique string using the related entity and an appropriate
         --  suffix, rather than a numeric serial number (used for internal
         --  entities) that may vary depending on compilation options, in
         --  particular on the Assertions_Enabled mode. This avoids spurious
         --  link errors.

         if Present (Related_Id) then
            if Is_Low_Bound then
               Temp_Nam := New_External_Name (Chars (Related_Id), "_FIRST");

            elsif Is_High_Bound then
               Temp_Nam := New_External_Name (Chars (Related_Id), "_LAST");

            else
               pragma Assert (Discr_Number > 0);

               --  We don't have any intelligible way of printing T_DISCR in
               --  CodePeer. Thus, set a related expression in this case.

               Should_Set_Related_Expression := True;

               --  Use fully qualified name to avoid ambiguities.

               Temp_Nam :=
                  New_External_Name
                   (Get_Qualified_Name (Related_Id), "_DISCR", Discr_Number);
            end if;

            Temp_Id := Make_Defining_Identifier (Loc, Temp_Nam);

            if Should_Set_Related_Expression then
               Set_Related_Expression (Temp_Id, Related_Nod);
            end if;

         --  Otherwise generate an internal temporary

         else
            Temp_Id := Make_Temporary (Loc, Id, Related_Nod);
         end if;

         Set_Is_Internal (Temp_Id);

         return Temp_Id;
      end Build_Temporary;

      -----------------------------------
      -- Possible_Side_Effect_In_SPARK --
      -----------------------------------

      function Possible_Side_Effect_In_SPARK (Exp : Node_Id) return Boolean is
      begin
         --  Side-effect removal in SPARK should only occur when not inside a
         --  generic and not doing a preanalysis, inside an object renaming or
         --  a type declaration or a for-loop iteration scheme.

         if not Inside_A_Generic
           and then Full_Analysis
         then

            case Nkind (Enclosing_Declaration (Exp)) is
               when N_Component_Declaration
                  | N_Full_Type_Declaration
                  | N_Iterator_Specification
                  | N_Loop_Parameter_Specification
                  | N_Object_Renaming_Declaration
               =>
                  return True;

               --  If the expression belongs to an itype declaration, then
               --  check if side effects are allowed in the original
               --  associated node.

               when N_Subtype_Declaration =>
                  declare
                     Subt : constant Entity_Id :=
                       Defining_Identifier (Enclosing_Declaration (Exp));
                  begin
                     if Is_Itype (Subt) then

                        --  When this routine is called while the itype
                        --  is being created, the entity might not yet be
                        --  decorated with the associated node, but will
                        --  have the related expression.

                        if Present (Associated_Node_For_Itype (Subt)) then
                           return
                             Possible_Side_Effect_In_SPARK
                               (Associated_Node_For_Itype (Subt));

                        else
                           return
                             Possible_Side_Effect_In_SPARK
                               (Related_Expression (Subt));
                        end if;
                     else
                        return True;
                     end if;
                  end;

               when others =>
                  return False;
            end case;
         else
            return False;
         end if;
      end Possible_Side_Effect_In_SPARK;

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
      --  renamings.

      if not Expander_Active
        and then not
          (GNATprove_Mode and then Possible_Side_Effect_In_SPARK (Exp))
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

      --  No action needed for side-effect-free expressions

      elsif Check_Side_Effects
        and then Side_Effect_Free (Exp, Name_Req, Variable_Ref)
      then
         return;
      end if;

      --  The remaining processing is done with all checks suppressed

      --  Note: from now on, don't use return statements, instead do a goto
      --  Leave, to ensure that we properly restore Scope_Suppress.Suppress.

      Scope_Suppress.Suppress := (others => True);

      --  If this is a side-effect-free attribute reference whose expressions
      --  are also side-effect-free and whose prefix is not a name, remove the
      --  side effects of the prefix. A copy of the prefix is required in this
      --  case and it is better not to make an additional one for the attribute
      --  itself, because the return type of many of them is universal integer,
      --  which is a very large type for a temporary.
      --  The prefix of an attribute reference Reduce may be syntactically an
      --  aggregate, but will be expanded into a loop, so no need to remove
      --  side effects.

      if Nkind (Exp) = N_Attribute_Reference
        and then Side_Effect_Free_Attribute (Attribute_Name (Exp))
        and then Side_Effect_Free (Expressions (Exp), Name_Req, Variable_Ref)
        and then (Attribute_Name (Exp) /= Name_Reduce
                   or else Nkind (Prefix (Exp)) /= N_Aggregate)
        and then not Is_Name_Reference (Prefix (Exp))
      then
         Remove_Side_Effects (Prefix (Exp), Name_Req, Variable_Ref);
         goto Leave;

      --  If this is an elementary or a small not-by-reference record type, and
      --  we need to capture the value, just make a constant; this is cheap and
      --  objects of both kinds of types can be bit aligned, so it might not be
      --  possible to generate a reference to them. Likewise if this is not a
      --  name reference, except for a type conversion, because we would enter
      --  an infinite recursion with Checks.Apply_Predicate_Check if the target
      --  type has predicates (and type conversions need a specific treatment
      --  anyway, see below). Also do it if we have a volatile reference and
      --  Name_Req is not set (see comments for Side_Effect_Free).

      elsif (Is_Elementary_Type (Exp_Type)
              or else (Is_Record_Type (Exp_Type)
                        and then Known_Static_RM_Size (Exp_Type)
                        and then RM_Size (Exp_Type) <= System_Max_Integer_Size
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

         --  In GNATprove mode, we prefer to use renamings for intermediate
         --  variables to definition of constants, due to the implicit move
         --  operation that such a constant definition causes as part of the
         --  support in GNATprove for ownership pointers. Hence, we generate
         --  a renaming for a reference to an object of a nonscalar type.

         if Renaming_Req
           or else (GNATprove_Mode
                     and then Is_Object_Reference (Exp)
                     and then not Is_Scalar_Type (Exp_Type))
         then
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
      --  side effects in the expression, unless it is of universal integer,
      --  which is a very large type for a temporary. This is important in
      --  several circumstances: for change of representations and also when
      --  this is a view conversion to a smaller object, where gigi can end
      --  up creating its own temporary of the wrong size.

      elsif Nkind (Exp) = N_Type_Conversion
        and then Etype (Expression (Exp)) /= Universal_Integer
      then
         Remove_Side_Effects (Expression (Exp), Name_Req, Variable_Ref);
         goto Leave;

      --  If this is an unchecked conversion that Gigi can't handle, make
      --  a copy or a use a renaming to capture the value.

      elsif Nkind (Exp) = N_Unchecked_Type_Conversion
        and then not Safe_Unchecked_Type_Conversion (Exp)
      then
         if CW_Or_Needs_Finalization (Exp_Type) then

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

      --  If this is a packed array component or a selected component with a
      --  nonstandard representation, we cannot generate a reference because
      --  the component may be unaligned, so we must use a renaming and this
      --  renaming is handled by the front end, as the back end may balk at
      --  the nonstandard representation (see Evaluation_Required in Exp_Ch8).

      elsif (Nkind (Exp) in N_Indexed_Component | N_Selected_Component
              and then Has_Non_Standard_Rep (Etype (Prefix (Exp))))

        --  For an expression that denotes a name, we can use a renaming
        --  scheme. This is needed for correctness in the case of a volatile
        --  object of a nonvolatile type because the Make_Reference call of the
        --  "default" approach would generate an illegal access value (an
        --  access value cannot designate such an object - see
        --  Analyze_Reference).

        or else (Is_Name_Reference (Exp)

          --  We skip using this scheme if we have an object of a volatile
          --  type and we do not have Name_Req set true (see comments for
          --  Side_Effect_Free).

          and then (Name_Req or else not Treat_As_Volatile (Exp_Type)))
      then
         Def_Id := Build_Temporary (Loc, 'R', Exp);
         Res := New_Occurrence_Of (Def_Id, Loc);

         Insert_Action (Exp,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Subtype_Mark        => New_Occurrence_Of (Exp_Type, Loc),
             Name                => Relocate_Node (Exp)));

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
         --  Special processing for function calls that return a limited type.
         --  We need to build a declaration that will enable build-in-place
         --  expansion of the call. This is not done if the context is already
         --  an object declaration, to prevent infinite recursion.

         --  This is relevant only in Ada 2005 mode. In Ada 95 programs we have
         --  to accommodate functions returning limited objects by reference.

         if Ada_Version >= Ada_2005
           and then Nkind (Exp) = N_Function_Call
           and then Is_Inherently_Limited_Type (Etype (Exp))
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

         if GNATprove_Mode then
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

            --  Do not generate a 'reference in SPARK mode since the access
            --  type is not created in the first place.

            if GNATprove_Mode then
               New_Exp := E;

            --  Otherwise generate reference, marking the value as non-null
            --  since we know it cannot be null and we don't want a check.

            else
               --  Make_Reference assumes that the referenced
               --  object satisfies the constraints of the designated
               --  subtype of the access type. Ensure that this assumption
               --  holds by introducing a qualified expression if needed.

               if not Analyzed (Exp)
                 and then Nkind (Exp) = N_Aggregate
                 and then (Is_Array_Type (Exp_Type)
                           or else Has_Discriminants (Exp_Type))
                 and then Is_Constrained (Exp_Type)
               then
                  --  Do not suppress checks associated with the qualified
                  --  expression we are about to introduce (unless those
                  --  checks were already suppressed when Remove_Side_Effects
                  --  was called).

                  if Is_Array_Type (Exp_Type) then
                     Scope_Suppress.Suppress (Length_Check) :=
                       Svg_Suppress.Suppress (Length_Check);
                  else
                     Scope_Suppress.Suppress (Discriminant_Check) :=
                       Svg_Suppress.Suppress (Discriminant_Check);
                  end if;

                  E := Make_Qualified_Expression (Loc,
                         Subtype_Mark => New_Occurrence_Of (Exp_Type, Loc),
                         Expression => E);
               end if;

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
             Object_Definition   => New_Occurrence_Of (Ref_Type, Loc),
             Constant_Present    => True,
             Expression          => New_Exp));
      end if;

      --  Preserve the Assignment_OK flag in all copies, since at least one
      --  copy may be used in a context where this flag must be set (otherwise
      --  why would the flag be set in the first place).

      Set_Assignment_OK (Res, Assignment_OK (Exp));

      --  Preserve the Do_Range_Check flag in all copies

      Set_Do_Range_Check (Res, Do_Range_Check (Exp));

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

         Context : constant Node_Id :=
           (if No (Ref) then Empty else Parent (Ref));

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

               if Nkind (Context) in N_Subprogram_Call
                 and then No (Type_Map.Get (Entity (Name (Context))))
               then
                  declare
                     --  We need to use the Original_Node of the callee, in
                     --  case it was already modified. Note that we are using
                     --  Traverse_Proc to walk the tree, and it is defined to
                     --  walk subtrees in an arbitrary order.

                     Callee : constant Entity_Id :=
                       Entity (Original_Node (Name (Context)));
                  begin
                     if No (Type_Map.Get (Callee)) then
                        New_Ref :=
                          Convert_To
                            (Type_Of_Formal (Context, Old_Ref), New_Ref);

                        --  Do not process the generated type conversion
                        --  because both the parent type and the derived type
                        --  are in the Type_Map table. This will clobber the
                        --  type conversion by resetting its subtype mark.

                        Result := Skip;
                     end if;
                  end;
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

         A := First_Actual (Call);
         F := First_Formal (Entity (Name (Call)));
         while Present (A) and then Present (F) loop
            if A = Actual then
               return Etype (F);
            end if;

            Next_Actual (A);
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
         --  further down. This is done so that routines which examine
         --  properties of the Original_Node have some semantic information.

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
          and then Nkind (N) in N_Package_Body | N_Package_Specification;
      --  N is at the library level if the top-most context is a package and
      --  the path taken to reach N does not include nonpackage constructs.

   begin
      case Nkind (N) is
         when N_Accept_Statement
            | N_Block_Statement
            | N_Entry_Body
            | N_Package_Body
            | N_Subprogram_Body
            | N_Task_Body
         =>
            return
                Requires_Cleanup_Actions
                  (L                 => Declarations (N),
                   Lib_Level         => At_Lib_Level,
                   Nested_Constructs => True)
              or else
                (Present (Handled_Statement_Sequence (N))
                  and then
                    Requires_Cleanup_Actions
                      (L                 =>
                         Statements (Handled_Statement_Sequence (N)),
                       Lib_Level         => At_Lib_Level,
                       Nested_Constructs => True));

         --  Extended return statements are the same as the above, except that
         --  there is no Declarations field. We do not want to clean up the
         --  Return_Object_Declarations.

         when N_Extended_Return_Statement =>
            return
              Present (Handled_Statement_Sequence (N))
                and then Requires_Cleanup_Actions
                           (L                 =>
                              Statements (Handled_Statement_Sequence (N)),
                            Lib_Level         => At_Lib_Level,
                            Nested_Constructs => True);

         when N_Package_Specification =>
            return
                Requires_Cleanup_Actions
                  (L                 => Visible_Declarations (N),
                   Lib_Level         => At_Lib_Level,
                   Nested_Constructs => True)
              or else
                Requires_Cleanup_Actions
                  (L                 => Private_Declarations (N),
                   Lib_Level         => At_Lib_Level,
                   Nested_Constructs => True);

         when others =>
            raise Program_Error;
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
              and then not Is_Abstract_Type (Typ)
              and then not No_Run_Time_Mode
              and then not Restriction_Active (No_Tagged_Type_Registration)
              and then RTE_Available (RE_Unregister_Tag)
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

            --  Finalization of transient objects is treated separately in
            --  order to handle sensitive cases. These include:

            --    * Conditional expressions
            --    * Expressions with actions
            --    * Transient scopes

            elsif Is_Finalized_Transient (Obj_Id) then
               null;

            --  Finalization of specific objects is also treated separately

            elsif Is_Ignored_For_Finalization (Obj_Id) then
               null;

            --  Conversely, if one of the above cases created a Master_Node,
            --  finalization actions are required for the associated object.

            elsif Ekind (Obj_Id) = E_Variable
              and then Is_RTE (Obj_Typ, RE_Master_Node)
            then
               return True;

            --  Ignored Ghost objects do not need any cleanup actions because
            --  they will not appear in the final tree.

            elsif Is_Ignored_Ghost_Entity (Obj_Id) then
               null;

            --  The object is of the form:
            --    Obj : [constant] Typ [:= Expr];
            --
            --  Do not process the incomplete view of a deferred constant.
            --  Note that an object initialized by means of a BIP function
            --  call may appear as a deferred constant after expansion
            --  activities. These kinds of objects must be finalized.

            elsif not Is_Imported (Obj_Id)
              and then Needs_Finalization (Obj_Typ)
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

            --  Simple protected objects which use the type System.Tasking.
            --  Protected_Objects.Protection to manage their locks should be
            --  treated as controlled since they require manual cleanup, but
            --  not for restricted run-time libraries (Ravenscar), see also
            --  Cleanup_Protected_Object in Exp_Ch7.

            --  The only exception is illustrated in the following example:

            --     package Pkg is
            --        type Ctrl is new Controlled ...
            --        procedure Finalize (Obj : in out Ctrl);
            --        Lib_Obj : Ctrl;
            --     end Pkg;

            --     package body Pkg is
            --        protected Prot is
            --           procedure Do_Something (Obj : in out Ctrl);
            --        end Prot;

            --        protected body Prot is
            --           procedure Do_Something (Obj : in out Ctrl) is ...
            --        end Prot;

            --        procedure Finalize (Obj : in out Ctrl) is
            --        begin
            --           Prot.Do_Something (Obj);
            --        end Finalize;
            --     end Pkg;

            --  Since for the most part entities in package bodies depend on
            --  those in package specs, Prot's lock should be cleaned up
            --  first. The subsequent cleanup of the spec finalizes Lib_Obj.
            --  This act however attempts to invoke Do_Something and fails
            --  because the lock has disappeared.

            elsif Ekind (Obj_Id) = E_Variable
              and then not In_Library_Level_Package_Body (Obj_Id)
              and then Has_Simple_Protected_Object (Obj_Typ)
              and then not Restricted_Profile
            then
               return True;
            end if;

         --  Inspect the freeze node of an access-to-controlled type and look
         --  for a delayed finalization collection. This case arises when the
         --  freeze actions are inserted at a later time than the expansion of
         --  the context. Since Build_Finalizer is never called on a single
         --  construct twice, the collection would be ultimately left out and
         --  never finalized. This is also needed for the freeze actions of
         --  designated types themselves, since in some cases the finalization
         --  collection is associated with a designated type's freeze node
         --  rather than that of the access type (see handling for freeze
         --  actions in Build_Finalization_Collection).

         elsif Nkind (Decl) = N_Freeze_Entity
           and then Present (Actions (Decl))
         then
            Typ := Entity (Decl);

            --  Freeze nodes for ignored Ghost types do not need cleanup
            --  actions because they will never appear in the final tree.

            if Is_Ignored_Ghost_Entity (Typ) then
               null;

            elsif ((Is_Access_Object_Type (Typ)
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
        or else Nkind (Pexp)
                  in N_Object_Declaration | N_Object_Renaming_Declaration
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
         return No (Etype (Pexp))
           or else not Is_Type (Etype (Pexp))
           or else not Has_Discriminants (Etype (Pexp))
           or else Is_Constrained (Etype (Pexp));
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

      if Present (Ialign) and then Ialign > Maximum_Alignment then
         return True;

      elsif Present (Ialign)
        and then Present (Oalign)
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
               --  inner one). Really we should record both, but our data
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

         elsif Nkind (Cond) in N_Type_Conversion
                             | N_Qualified_Expression
                             | N_Expression_With_Actions
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

            --  Mark the assignment statement as elaboration code. This allows
            --  the early call region mechanism (see Sem_Elab) to properly
            --  ignore such assignments even though they are nonpreelaborable
            --  code.

            Set_Is_Elaboration_Code (Asn);

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
                Ekind (Scope (Spec_Id)) in E_Block | E_Procedure | E_Function
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
      --  if the construct is side-effect-free (not considering side effects in
      --  other than the prefix which are to be tested by the caller).

      function Within_In_Parameter (N : Node_Id) return Boolean;
      --  Determines if N is a subcomponent of a composite in-parameter. If so,
      --  N is not side-effect-free when the actual is global and modifiable
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
         --  If prefix is not side-effect-free, definitely not safe

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
            --  of side effects.

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

         elsif Parent_Kind (Parent (N)) = N_Loop_Parameter_Specification
           and then Within_In_Parameter (Prefix (N))
           and then Variable_Ref
         then
            return False;

         --  All other cases are side-effect-free

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

         elsif Nkind (N) in N_Indexed_Component | N_Selected_Component then
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
      --  them to be side-effect-free, then we get some awkward expansions
      --  in -gnato mode, resulting in code insertions at a point where we
      --  do not have a clear model for performing the insertions.

      --  Special handling for entity names

      if Is_Entity_Name (N) then

         --  A type reference is always side-effect-free

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

      --  A value known at compile time is always side-effect-free

      elsif Compile_Time_Known_Value (N) then
         return True;

      --  A variable renaming is not side-effect-free, because the renaming
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

            if Nkind (RO) in N_Indexed_Component | N_Explicit_Dereference then
               return False;

            --  A selected component must have a safe prefix

            elsif Nkind (RO) = N_Selected_Component then
               return Safe_Prefixed_Reference (RO);

            --  In all other cases, designated object cannot be changed so
            --  we are side-effect-free.

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
      end if;

      --  For other than entity names and compile time known values,
      --  check the node kind for special processing.

      case Nkind (N) is

         --  An attribute reference is side-effect-free if its expressions
         --  are side-effect-free and its prefix is side-effect-free or is
         --  an entity reference.

         when N_Attribute_Reference =>
            return Side_Effect_Free_Attribute (Attribute_Name (N))
                     and then
                   Side_Effect_Free (Expressions (N), Name_Req, Variable_Ref)
                     and then
                   (Is_Entity_Name (Prefix (N))
                      or else
                    Side_Effect_Free (Prefix (N), Name_Req, Variable_Ref));

         --  A binary operator is side-effect-free if and both operands are
         --  side-effect-free. For this purpose binary operators include
         --  short circuit forms.

         when N_Binary_Op
            | N_Short_Circuit
         =>
            return Side_Effect_Free (Left_Opnd  (N), Name_Req, Variable_Ref)
                     and then
                   Side_Effect_Free (Right_Opnd (N), Name_Req, Variable_Ref);

         --  Membership tests may have either Right_Opnd or Alternatives set

         when N_Membership_Test =>
            return Side_Effect_Free (Left_Opnd (N), Name_Req, Variable_Ref)
                     and then
                   (if Present (Right_Opnd (N))
                    then Side_Effect_Free
                           (Right_Opnd (N), Name_Req, Variable_Ref)
                    else Side_Effect_Free
                           (Alternatives (N), Name_Req, Variable_Ref));

         --  An explicit dereference is side-effect-free only if it is
         --  a side-effect-free prefixed reference.

         when N_Explicit_Dereference =>
            return Safe_Prefixed_Reference (N);

         --  An expression with action is side-effect-free if its expression
         --  is side-effect-free and it has no actions.

         when N_Expression_With_Actions =>
            return
              Is_Empty_List (Actions (N))
                and then Side_Effect_Free
                           (Expression (N), Name_Req, Variable_Ref);

         --  A call to _rep_to_pos is side-effect-free, since we generate
         --  this pure function call ourselves. Moreover it is critically
         --  important to make this exception, since otherwise we can have
         --  discriminants in array components which don't look side-effect
         --  free in the case of an array whose index type is an enumeration
         --  type with an enumeration rep clause.

         --  All other function calls are not side-effect-free

         when N_Function_Call =>
            return
              Nkind (Name (N)) = N_Identifier
                and then Is_TSS (Name (N), TSS_Rep_To_Pos)
                and then Side_Effect_Free
                           (First (Parameter_Associations (N)),
                            Name_Req, Variable_Ref);

         --  An IF expression is side-effect-free if it's of a scalar type, and
         --  all its components are all side-effect-free (conditions and then
         --  actions and else actions). We restrict to scalar types, since it
         --  is annoying to deal with things like (if A then B else C)'First
         --  where the type involved is a string type.

         when N_If_Expression =>
            return
              Is_Scalar_Type (Typ)
                and then Side_Effect_Free
                           (Expressions (N), Name_Req, Variable_Ref);

         --  An indexed component is side-effect-free if it is a side
         --  effect free prefixed reference and all the indexing
         --  expressions are side-effect-free.

         when N_Indexed_Component =>
            return
              Side_Effect_Free (Expressions (N), Name_Req, Variable_Ref)
                and then Safe_Prefixed_Reference (N);

         --  A type qualification, type conversion, or unchecked expression is
         --  side-effect-free if the expression is side-effect-free.

         when N_Qualified_Expression
            | N_Type_Conversion
            | N_Unchecked_Expression
         =>
            return Side_Effect_Free (Expression (N), Name_Req, Variable_Ref);

         --  A selected component is side-effect-free only if it is a side
         --  effect free prefixed reference.

         when N_Selected_Component =>
            return Safe_Prefixed_Reference (N);

         --  A range is side-effect-free if the bounds are side-effect-free

         when N_Range =>
            return Side_Effect_Free (Low_Bound (N),  Name_Req, Variable_Ref)
                     and then
                   Side_Effect_Free (High_Bound (N), Name_Req, Variable_Ref);

         --  A slice is side-effect-free if it is a side-effect-free
         --  prefixed reference and the bounds are side-effect-free.

         when N_Slice =>
            return
               Side_Effect_Free (Discrete_Range (N), Name_Req, Variable_Ref)
                 and then Safe_Prefixed_Reference (N);

         --  A unary operator is side-effect-free if the operand
         --  is side-effect-free.

         when N_Unary_Op =>
            return Side_Effect_Free (Right_Opnd (N), Name_Req, Variable_Ref);

         --  An unchecked type conversion is side-effect-free only if it
         --  is safe and its argument is side-effect-free.

         when N_Unchecked_Type_Conversion =>
            return
              Safe_Unchecked_Type_Conversion (N)
                and then Side_Effect_Free
                           (Expression (N), Name_Req, Variable_Ref);

         --  A literal is side-effect-free

         when N_Character_Literal
            | N_Integer_Literal
            | N_Real_Literal
            | N_String_Literal
         =>
            return True;

         --  An aggregate is side-effect-free if all its values are compile
         --  time known.

         when N_Aggregate =>
            return Compile_Time_Known_Aggregate (N);

         --  We consider that anything else has side effects. This is a bit
         --  crude, but we are pretty close for most common cases, and we
         --  are certainly correct (i.e. we never return True when the
         --  answer should be False).

         when others =>
            return False;
      end case;
   end Side_Effect_Free;

   --  A list is side-effect-free if all elements of the list are side
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

   --------------------------------
   -- Side_Effect_Free_Attribute --
   --------------------------------

   function Side_Effect_Free_Attribute (Name : Name_Id) return Boolean is
   begin
      case Name is
         when Name_Input =>
            return False;

         when Name_Image
            | Name_Img
            | Name_Wide_Image
            | Name_Wide_Wide_Image
         =>
            --  CodePeer doesn't want to see replicated copies of 'Image calls

            return not CodePeer_Mode;

         when others =>
            return True;
      end case;
   end Side_Effect_Free_Attribute;

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

   procedure Silly_Boolean_Array_Xor_Test
     (N : Node_Id;
      R : Node_Id;
      T : Entity_Id)
   is
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
              Left_Opnd  =>
                Make_And_Then (Loc,
                  Left_Opnd  =>
                    Convert_To (Standard_Boolean,
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (CT, Loc),
                        Attribute_Name => Name_First)),

                  Right_Opnd =>
                    Convert_To (Standard_Boolean,
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (CT, Loc),
                        Attribute_Name => Name_Last))),

              Right_Opnd => Make_Non_Empty_Check (Loc, R)),
          Reason    => CE_Range_Check_Failed));
   end Silly_Boolean_Array_Xor_Test;

   ----------------------------
   -- Small_Integer_Type_For --
   ----------------------------

   function Small_Integer_Type_For (S : Uint; Uns : Boolean) return Entity_Id
   is
   begin
      --  The only difference between this and Integer_Type_For is that this
      --  can return small (8- or 16-bit) types.

      if S <= Standard_Short_Short_Integer_Size then
         if Uns then
            return Standard_Short_Short_Unsigned;
         else
            return Standard_Short_Short_Integer;
         end if;

      elsif S <= Standard_Short_Integer_Size then
         if Uns then
            return Standard_Short_Unsigned;
         else
            return Standard_Short_Integer;
         end if;

      else
         return Integer_Type_For (S, Uns);
      end if;
   end Small_Integer_Type_For;

   ------------------
   -- Thunk_Target --
   ------------------

   function Thunk_Target (Thunk : Entity_Id) return Entity_Id is
      Target : Entity_Id := Thunk;

   begin
      pragma Assert (Is_Thunk (Thunk));

      while Is_Thunk (Target) loop
         Target := Thunk_Entity (Target);
      end loop;

      return Target;
   end Thunk_Target;

   -----------------------
   -- Try_Inline_Always --
   -----------------------

   function Try_Inline_Always (Subp : Entity_Id) return Boolean is
     ((Is_Expression_Function (Subp) or else Is_Finalizer (Subp))
       and then not Debug_Flag_Dot_8);
   --  We want to inline expression functions and finalizers as much as
   --  practical unless -gnatd.8.

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
               --  This is the crucial test: if the component itself causes
               --  trouble, then we can stop and return True.

               if Component_May_Be_Bit_Aligned (E) then
                  return True;
               end if;

               --  Otherwise, we need to test its type, to see if it may
               --  itself contain a troublesome component.

               if Type_May_Have_Bit_Aligned_Components (Etype (E)) then
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
      Parent_Type  : constant Entity_Id := Find_Dispatching_Type (Inher_Id);
      Derived_Type : constant Entity_Id := Find_Dispatching_Type (Subp_Id);

   begin
      pragma Assert (Parent_Type /= Derived_Type);
      Map_Types (Parent_Type, Derived_Type);
   end Update_Primitives_Mapping;

   ----------------------------------
   -- Within_Case_Or_If_Expression --
   ----------------------------------

   function Within_Case_Or_If_Expression (N : Node_Id) return Boolean is
      Nod : Node_Id;
      Par : Node_Id;

   begin
      --  Locate an enclosing case or if expression. Note that these constructs
      --  can be expanded into Expression_With_Actions, hence the test of the
      --  original node.

      Nod := N;
      Par := Parent (Nod);

      while Present (Par) loop
         if Nkind (Original_Node (Par)) = N_Case_Expression
           and then Nod /= Expression (Original_Node (Par))
         then
            return True;

         elsif Nkind (Original_Node (Par)) = N_If_Expression
           and then Nod /= First (Expressions (Original_Node (Par)))
         then
            return True;

         --  Stop at contexts where temporaries may be contained

         elsif Nkind (Par) in N_Aggregate
                            | N_Delta_Aggregate
                            | N_Extension_Aggregate
                            | N_Block_Statement
                            | N_Loop_Statement
         then
            return False;

         --  Prevent the search from going too far

         elsif Is_Body_Or_Package_Declaration (Par) then
            return False;
         end if;

         Nod := Par;
         Par := Parent (Nod);
      end loop;

      return False;
   end Within_Case_Or_If_Expression;

   ------------------------------
   -- Predicate_Check_In_Scope --
   ------------------------------

   function Predicate_Check_In_Scope (N : Node_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then not Is_Subprogram (S) loop
         S := Scope (S);
      end loop;

      if Present (S) then

         --  Predicate checks should only be enabled in init procs for
         --  expressions coming from source.

         if Is_Init_Proc (S) then
            return Comes_From_Source (N);

         elsif Get_TSS_Name (S) /= TSS_Null
           and then not Is_Predicate_Function (S)
         then
            return False;
         end if;
      end if;

      return True;
   end Predicate_Check_In_Scope;

end Exp_Util;
