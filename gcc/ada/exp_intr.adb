------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I N T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch11; use Exp_Ch11;
with Exp_Code; use Exp_Code;
with Exp_Fixd; use Exp_Fixd;
with Exp_Util; use Exp_Util;
with Itypes;   use Itypes;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Intr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Is_Negative (N : Node_Id);
   --  Expand a call to the intrinsic Is_Negative function

   procedure Expand_Exception_Call (N : Node_Id; Ent : RE_Id);
   --  Expand a call to Exception_Information/Message/Name. The first
   --  parameter, N, is the node for the function call, and Ent is the
   --  entity for the corresponding routine in the Ada.Exceptions package.

   procedure Expand_Import_Call (N : Node_Id);
   --  Expand a call to Import_Address/Longest_Integer/Value. The parameter
   --  N is the node for the function call.

   procedure Expand_Shift (N : Node_Id; E : Entity_Id; K : Node_Kind);
   --  Expand an intrinsic shift operation, N and E are from the call to
   --  Expand_Intrinsic_Call (call node and subprogram spec entity) and
   --  K is the kind for the shift node

   procedure Expand_Unc_Conversion (N : Node_Id; E : Entity_Id);
   --  Expand a call to an instantiation of Unchecked_Convertion into a node
   --  N_Unchecked_Type_Conversion.

   procedure Expand_Unc_Deallocation (N : Node_Id);
   --  Expand a call to an instantiation of Unchecked_Deallocation into a node
   --  N_Free_Statement and appropriate context.

   procedure Expand_To_Address (N : Node_Id);
   procedure Expand_To_Pointer (N : Node_Id);
   --  Expand a call to corresponding function, declared in an instance of
   --  System.Addess_To_Access_Conversions.

   procedure Expand_Source_Info (N : Node_Id; Nam : Name_Id);
   --  Rewrite the node by the appropriate string or positive constant.
   --  Nam can be one of the following:
   --    Name_File             - expand string that is the name of source file
   --    Name_Line             - expand integer line number
   --    Name_Source_Location  - expand string of form file:line
   --    Name_Enclosing_Entity - expand string  with name of enclosing entity

   ---------------------------
   -- Expand_Exception_Call --
   ---------------------------

   --  If the function call is not within an exception handler, then the
   --  call is replaced by a null string. Otherwise the appropriate routine
   --  in Ada.Exceptions is called passing the choice parameter specification
   --  from the enclosing handler. If the enclosing handler lacks a choice
   --  parameter, then one is supplied.

   procedure Expand_Exception_Call (N : Node_Id; Ent : RE_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      P   : Node_Id;
      E   : Entity_Id;

   begin
      --  Climb up parents to see if we are in exception handler

      P := Parent (N);
      loop
         --  Case of not in exception handler, replace by null string

         if No (P) then
            Rewrite (N,
              Make_String_Literal (Loc,
                Strval => ""));
            exit;

         --  Case of in exception handler

         elsif Nkind (P) = N_Exception_Handler then
            if No (Choice_Parameter (P)) then

               --  If no choice parameter present, then put one there. Note
               --  that we do not need to put it on the entity chain, since
               --  no one will be referencing it by normal visibility methods.

               E := Make_Defining_Identifier (Loc, New_Internal_Name ('E'));
               Set_Choice_Parameter (P, E);
               Set_Ekind (E, E_Variable);
               Set_Etype (E, RTE (RE_Exception_Occurrence));
               Set_Scope (E, Current_Scope);
            end if;

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (RTE (Ent), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Choice_Parameter (P), Loc))));
            exit;

         --  Keep climbing!

         else
            P := Parent (P);
         end if;
      end loop;

      Analyze_And_Resolve (N, Standard_String);
   end Expand_Exception_Call;

   ------------------------
   -- Expand_Import_Call --
   ------------------------

   --  The function call must have a static string as its argument. We create
   --  a dummy variable which uses this string as the external name in an
   --  Import pragma. The result is then obtained as the address of this
   --  dummy variable, converted to the appropriate target type.

   procedure Expand_Import_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Entity_Id  := Entity (Name (N));
      Str : constant Node_Id    := First_Actual (N);
      Dum : Entity_Id;

   begin
      Dum := Make_Defining_Identifier (Loc, New_Internal_Name ('D'));

      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Dum,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Character, Loc)),

        Make_Pragma (Loc,
          Chars => Name_Import,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Name_Ada)),

            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Chars (Dum))),

            Make_Pragma_Argument_Association (Loc,
              Chars => Name_Link_Name,
              Expression => Relocate_Node (Str))))));

      Rewrite (N,
        Unchecked_Convert_To (Etype (Ent),
          Make_Attribute_Reference (Loc,
            Attribute_Name => Name_Address,
            Prefix => Make_Identifier (Loc, Chars (Dum)))));

      Analyze_And_Resolve (N, Etype (Ent));
   end Expand_Import_Call;

   ---------------------------
   -- Expand_Intrinsic_Call --
   ---------------------------

   procedure Expand_Intrinsic_Call (N : Node_Id; E : Entity_Id) is
      Nam : Name_Id;

   begin
      --  If the intrinsic subprogram is generic, gets its original name

      if Present (Parent (E))
        and then Present (Generic_Parent (Parent (E)))
      then
         Nam := Chars (Generic_Parent (Parent (E)));
      else
         Nam := Chars (E);
      end if;

      if Nam = Name_Asm then
         Expand_Asm_Call (N);

      elsif Nam = Name_Divide then
         Expand_Decimal_Divide_Call (N);

      elsif Nam = Name_Exception_Information then
         Expand_Exception_Call (N, RE_Exception_Information);

      elsif Nam = Name_Exception_Message then
         Expand_Exception_Call (N, RE_Exception_Message);

      elsif Nam = Name_Exception_Name then
         Expand_Exception_Call (N, RE_Exception_Name_Simple);

      elsif Nam = Name_Import_Address
              or else
            Nam = Name_Import_Largest_Value
              or else
            Nam = Name_Import_Value
      then
         Expand_Import_Call (N);

      elsif Nam = Name_Is_Negative then
         Expand_Is_Negative (N);

      elsif Nam = Name_Rotate_Left then
         Expand_Shift (N, E, N_Op_Rotate_Left);

      elsif Nam = Name_Rotate_Right then
         Expand_Shift (N, E, N_Op_Rotate_Right);

      elsif Nam = Name_Shift_Left then
         Expand_Shift (N, E, N_Op_Shift_Left);

      elsif Nam = Name_Shift_Right then
         Expand_Shift (N, E, N_Op_Shift_Right);

      elsif Nam = Name_Shift_Right_Arithmetic then
         Expand_Shift (N, E, N_Op_Shift_Right_Arithmetic);

      elsif Nam = Name_Unchecked_Conversion then
         Expand_Unc_Conversion (N, E);

      elsif Nam = Name_Unchecked_Deallocation then
         Expand_Unc_Deallocation (N);

      elsif Nam = Name_To_Address then
         Expand_To_Address (N);

      elsif Nam = Name_To_Pointer then
         Expand_To_Pointer (N);

      elsif Nam = Name_File
        or else Nam = Name_Line
        or else Nam = Name_Source_Location
        or else Nam = Name_Enclosing_Entity
      then
         Expand_Source_Info (N, Nam);

         --  If we have a renaming, expand the call to the original operation,
         --  which must itself be intrinsic, since renaming requires matching
         --  conventions and this has already been checked.

      elsif Present (Alias (E)) then
         Expand_Intrinsic_Call (N,  Alias (E));

         --  The only other case is where an external name was specified,
         --  since this is the only way that an otherwise unrecognized
         --  name could escape the checking in Sem_Prag. Nothing needs
         --  to be done in such a case, since we pass such a call to the
         --  back end unchanged.

      else
         null;
      end if;
   end Expand_Intrinsic_Call;

   ------------------------
   -- Expand_Is_Negative --
   ------------------------

   procedure Expand_Is_Negative (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Opnd  : constant Node_Id    := Relocate_Node (First_Actual (N));

   begin

      --  We replace the function call by the following expression

      --    if Opnd < 0.0 then
      --       True
      --    else
      --       if Opnd > 0.0 then
      --          False;
      --       else
      --          Float_Unsigned!(Float (Opnd)) /= 0
      --       end if;
      --    end if;

      Rewrite (N,
        Make_Conditional_Expression (Loc,
          Expressions => New_List (
            Make_Op_Lt (Loc,
              Left_Opnd  => Duplicate_Subexpr (Opnd),
              Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),

            New_Occurrence_Of (Standard_True, Loc),

            Make_Conditional_Expression (Loc,
             Expressions => New_List (
               Make_Op_Gt (Loc,
                 Left_Opnd  => Duplicate_Subexpr_No_Checks (Opnd),
                 Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),

               New_Occurrence_Of (Standard_False, Loc),

                Make_Op_Ne (Loc,
                  Left_Opnd =>
                    Unchecked_Convert_To
                      (RTE (RE_Float_Unsigned),
                       Convert_To
                         (Standard_Float,
                          Duplicate_Subexpr_No_Checks (Opnd))),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, 0)))))));

      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_Is_Negative;

   ------------------
   -- Expand_Shift --
   ------------------

   --  This procedure is used to convert a call to a shift function to the
   --  corresponding operator node. This conversion is not done by the usual
   --  circuit for converting calls to operator functions (e.g. "+"(1,2)) to
   --  operator nodes, because shifts are not predefined operators.

   --  As a result, whenever a shift is used in the source program, it will
   --  remain as a call until converted by this routine to the operator node
   --  form which Gigi is expecting to see.

   --  Note: it is possible for the expander to generate shift operator nodes
   --  directly, which will be analyzed in the normal manner by calling Analyze
   --  and Resolve. Such shift operator nodes will not be seen by Expand_Shift.

   procedure Expand_Shift (N : Node_Id; E : Entity_Id; K : Node_Kind) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Left  : constant Node_Id    := First_Actual (N);
      Right : constant Node_Id    := Next_Actual (Left);
      Ltyp  : constant Node_Id    := Etype (Left);
      Rtyp  : constant Node_Id    := Etype (Right);
      Snode : Node_Id;

   begin
      Snode := New_Node (K, Loc);
      Set_Left_Opnd  (Snode, Relocate_Node (Left));
      Set_Right_Opnd (Snode, Relocate_Node (Right));
      Set_Chars      (Snode, Chars (E));
      Set_Etype      (Snode, Base_Type (Typ));
      Set_Entity     (Snode, E);

      if Compile_Time_Known_Value (Type_High_Bound (Rtyp))
        and then Expr_Value (Type_High_Bound (Rtyp)) < Esize (Ltyp)
      then
         Set_Shift_Count_OK (Snode, True);
      end if;

      --  Do the rewrite. Note that we don't call Analyze and Resolve on
      --  this node, because it already got analyzed and resolved when
      --  it was a function call!

      Rewrite (N, Snode);
      Set_Analyzed (N);
   end Expand_Shift;

   ------------------------
   -- Expand_Source_Info --
   ------------------------

   procedure Expand_Source_Info (N : Node_Id; Nam : Name_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : Entity_Id;

   begin
      --  Integer cases

      if Nam = Name_Line then
         Rewrite (N,
           Make_Integer_Literal (Loc,
             Intval => UI_From_Int (Int (Get_Logical_Line_Number (Loc)))));
         Analyze_And_Resolve (N, Standard_Positive);

      --  String cases

      else
         case Nam is
            when Name_File =>
               Get_Decoded_Name_String
                 (Reference_Name (Get_Source_File_Index (Loc)));

            when Name_Source_Location =>
               Build_Location_String (Loc);

            when Name_Enclosing_Entity =>
               Name_Len := 0;

               Ent := Current_Scope;

               --  Skip enclosing blocks to reach enclosing unit.

               while Present (Ent) loop
                  exit when Ekind (Ent) /= E_Block
                    and then Ekind (Ent) /= E_Loop;
                  Ent := Scope (Ent);
               end loop;

               --  Ent now points to the relevant defining entity

               declare
                  SDef : Source_Ptr := Sloc (Ent);
                  TDef : Source_Buffer_Ptr;

               begin
                  TDef := Source_Text (Get_Source_File_Index (SDef));
                  Name_Len := 0;

                  while TDef (SDef) in '0' .. '9'
                    or else TDef (SDef) >= 'A'
                    or else TDef (SDef) = ASCII.ESC
                  loop
                     Add_Char_To_Name_Buffer (TDef (SDef));
                     SDef := SDef + 1;
                  end loop;
               end;

            when others =>
               raise Program_Error;
         end case;

         Rewrite (N,
           Make_String_Literal (Loc, Strval => String_From_Name_Buffer));
         Analyze_And_Resolve (N, Standard_String);
      end if;

      Set_Is_Static_Expression (N);
   end Expand_Source_Info;

   ---------------------------
   -- Expand_Unc_Conversion --
   ---------------------------

   procedure Expand_Unc_Conversion (N : Node_Id; E : Entity_Id) is
      Func : constant Entity_Id  := Entity (Name (N));
      Conv : Node_Id;
      Ftyp : Entity_Id;

   begin
      --  Rewrite as unchecked conversion node. Note that we must convert
      --  the operand to the formal type of the input parameter of the
      --  function, so that the resulting N_Unchecked_Type_Conversion
      --  call indicates the correct types for Gigi.

      --  Right now, we only do this if a scalar type is involved. It is
      --  not clear if it is needed in other cases. If we do attempt to
      --  do the conversion unconditionally, it crashes 3411-018. To be
      --  investigated further ???

      Conv := Relocate_Node (First_Actual (N));
      Ftyp := Etype (First_Formal (Func));

      if Is_Scalar_Type (Ftyp) then
         Conv := Convert_To (Ftyp, Conv);
         Set_Parent (Conv, N);
         Analyze_And_Resolve (Conv);
      end if;

      --  We do the analysis here, because we do not want the compiler
      --  to try to optimize or otherwise reorganize the unchecked
      --  conversion node.

      Rewrite (N, Unchecked_Convert_To (Etype (E), Conv));
      Set_Etype (N, Etype (E));
      Set_Analyzed (N);

      if Nkind (N) = N_Unchecked_Type_Conversion then
         Expand_N_Unchecked_Type_Conversion (N);
      end if;
   end Expand_Unc_Conversion;

   -----------------------------
   -- Expand_Unc_Deallocation --
   -----------------------------

   --  Generate the following Code :

   --    if Arg /= null then
   --     <Finalize_Call> (.., T'Class(Arg.all), ..);  -- for controlled types
   --       Free (Arg);
   --       Arg := Null;
   --    end if;

   --  For a task, we also generate a call to Free_Task to ensure that the
   --  task itself is freed if it is terminated, ditto for a simple protected
   --  object, with a call to Finalize_Protection. For composite types that
   --  have tasks or simple protected objects as components, we traverse the
   --  structures to find and terminate those components.

   procedure Expand_Unc_Deallocation (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Arg   : constant Node_Id    := First_Actual (N);
      Typ   : constant Entity_Id  := Etype (Arg);
      Stmts : constant List_Id    := New_List;
      Rtyp  : constant Entity_Id  := Underlying_Type (Root_Type (Typ));
      Pool  : constant Entity_Id  := Associated_Storage_Pool (Rtyp);

      Desig_T   : constant Entity_Id  := Designated_Type (Typ);
      Gen_Code  : Node_Id;
      Free_Node : Node_Id;
      Deref     : Node_Id;
      Free_Arg  : Node_Id;
      Free_Cod  : List_Id;
      Blk       : Node_Id;

   begin
      if No_Pool_Assigned (Rtyp) then
         Error_Msg_N ("?deallocation from empty storage pool", N);
      end if;

      if Controlled_Type (Desig_T) then
         Deref :=
           Make_Explicit_Dereference (Loc,
             Prefix => Duplicate_Subexpr_No_Checks (Arg));

         --  If the type is tagged, then we must force dispatching on the
         --  finalization call because the designated type may not be the
         --  actual type of the object

         if Is_Tagged_Type (Desig_T)
           and then not Is_Class_Wide_Type (Desig_T)
         then
            Deref := Unchecked_Convert_To (Class_Wide_Type (Desig_T), Deref);
         end if;

         Free_Cod :=
           Make_Final_Call
            (Ref         => Deref,
             Typ         => Desig_T,
             With_Detach => New_Reference_To (Standard_True, Loc));

         if Abort_Allowed then
            Prepend_To (Free_Cod,
              Build_Runtime_Call (Loc, RE_Abort_Defer));

            Blk :=
              Make_Block_Statement (Loc, Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements  => Free_Cod,
                  At_End_Proc =>
                    New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc)));

            --  We now expand the exception (at end) handler. We set a
            --  temporary parent pointer since we have not attached Blk
            --  to the tree yet.

            Set_Parent (Blk, N);
            Analyze (Blk);
            Expand_At_End_Handler
              (Handled_Statement_Sequence (Blk), Entity (Identifier (Blk)));
            Append (Blk, Stmts);

         else
            Append_List_To (Stmts, Free_Cod);
         end if;
      end if;

      --  For a task type, call Free_Task before freeing the ATCB

      if Is_Task_Type (Desig_T) then
         declare
            Stat : Node_Id := Prev (N);
            Nam1 : Node_Id;
            Nam2 : Node_Id;

         begin
            --  An Abort followed by a Free will not do what the user
            --  expects, because the abort is not immediate. This is
            --  worth a friendly warning.

            while Present (Stat)
              and then not Comes_From_Source (Original_Node (Stat))
            loop
               Prev (Stat);
            end loop;

            if Present (Stat)
              and then Nkind (Original_Node (Stat)) = N_Abort_Statement
            then
               Stat := Original_Node (Stat);
               Nam1 := First (Names (Stat));
               Nam2 := Original_Node (First (Parameter_Associations (N)));

               if Nkind (Nam1) = N_Explicit_Dereference
                 and then Is_Entity_Name (Prefix (Nam1))
                 and then Is_Entity_Name (Nam2)
                 and then Entity (Prefix (Nam1)) = Entity (Nam2)
               then
                  Error_Msg_N ("Abort may take time to complete?", N);
                  Error_Msg_N ("\deallocation might have no effect?", N);
                  Error_Msg_N ("\safer to wait for termination.?", N);
               end if;
            end if;
         end;

         Append_To
           (Stmts, Cleanup_Task (N, Duplicate_Subexpr_No_Checks (Arg)));

      --  For composite types that contain tasks, recurse over the structure
      --  to build the selectors for the task subcomponents.

      elsif Has_Task (Desig_T) then
         if Is_Record_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Record (N, Arg, Desig_T));

         elsif Is_Array_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Array (N, Arg, Desig_T));
         end if;
      end if;

      --  Same for simple protected types. Eventually call Finalize_Protection
      --  before freeing the PO for each protected component.

      if Is_Simple_Protected_Type (Desig_T) then
         Append_To (Stmts,
           Cleanup_Protected_Object (N, Duplicate_Subexpr_No_Checks (Arg)));

      elsif Has_Simple_Protected_Object (Desig_T) then
         if Is_Record_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Record (N, Arg, Desig_T));
         elsif Is_Array_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Array (N, Arg, Desig_T));
         end if;
      end if;

      --  Normal processing for non-controlled types

      Free_Arg := Duplicate_Subexpr_No_Checks (Arg);
      Free_Node := Make_Free_Statement (Loc, Empty);
      Append_To (Stmts, Free_Node);
      Set_Storage_Pool (Free_Node, Pool);

      --  Make implicit if statement. We omit this if we are the then part
      --  of a test of the form:

      --    if not (Arg = null) then

      --  i.e. if the test is explicit in the source. Arg must be a simple
      --  identifier for the purposes of this special test. Note that the
      --  use of /= in the source is always transformed into the above form.

      declare
         Test_Needed : Boolean := True;
         P           : constant Node_Id := Parent (N);
         C           : Node_Id;

      begin
         if Nkind (Arg) = N_Identifier
           and then Nkind (P) =  N_If_Statement
           and then First (Then_Statements (P)) = N
         then
            if Nkind (Condition (P)) = N_Op_Not then
               C := Right_Opnd (Condition (P));

               if Nkind (C) = N_Op_Eq
                 and then Nkind (Left_Opnd (C)) = N_Identifier
                 and then Chars (Arg) = Chars (Left_Opnd (C))
                 and then Nkind (Right_Opnd (C)) = N_Null
               then
                  Test_Needed := False;
               end if;
            end if;
         end if;

         --  Generate If_Statement if needed

         if Test_Needed then
            Gen_Code :=
              Make_Implicit_If_Statement (N,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Arg),
                    Right_Opnd => Make_Null (Loc)),
                Then_Statements => Stmts);

         else
            Gen_Code :=
              Make_Block_Statement (Loc,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Stmts));
         end if;
      end;

      --  Deal with storage pool

      if Present (Pool) then

         --  Freeing the secondary stack is meaningless

         if Is_RTE (Pool, RE_SS_Pool) then
            null;

         elsif Is_Class_Wide_Type (Etype (Pool)) then
            Set_Procedure_To_Call (Free_Node,
              RTE (RE_Deallocate_Any));
         else
            Set_Procedure_To_Call (Free_Node,
              Find_Prim_Op (Etype (Pool), Name_Deallocate));

            --  If the type is class wide, we generate an implicit type
            --  with the right dynamic size, so that the deallocate call
            --  gets the right size parameter computed by gigi

            if Is_Class_Wide_Type (Desig_T) then
               declare
                  Acc_Type : constant Entity_Id :=
                               Create_Itype (E_Access_Type, N);
                  Deref    : constant Node_Id :=
                               Make_Explicit_Dereference (Loc,
                                 Duplicate_Subexpr_No_Checks (Arg));

               begin
                  Set_Etype  (Deref, Typ);
                  Set_Parent (Deref, Free_Node);

                  Set_Etype     (Acc_Type, Acc_Type);
                  Set_Size_Info (Acc_Type, Typ);
                  Set_Directly_Designated_Type
                                (Acc_Type, Entity (Make_Subtype_From_Expr
                                                    (Deref, Desig_T)));

                  Free_Arg := Unchecked_Convert_To (Acc_Type, Free_Arg);
               end;
            end if;
         end if;
      end if;

      Set_Expression (Free_Node, Free_Arg);

      declare
         Lhs : constant Node_Id := Duplicate_Subexpr_No_Checks (Arg);

      begin
         Set_Assignment_OK (Lhs);
         Append_To (Stmts,
           Make_Assignment_Statement (Loc,
             Name       => Lhs,
             Expression => Make_Null (Loc)));
      end;

      Rewrite (N, Gen_Code);
      Analyze (N);
   end Expand_Unc_Deallocation;

   -----------------------
   -- Expand_To_Address --
   -----------------------

   procedure Expand_To_Address (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Arg : constant Node_Id := First_Actual (N);
      Obj : Node_Id;

   begin
      Remove_Side_Effects (Arg);

      Obj := Make_Explicit_Dereference (Loc, Relocate_Node (Arg));

      Rewrite (N,
        Make_Conditional_Expression (Loc,
          Expressions => New_List (
            Make_Op_Eq (Loc,
              Left_Opnd => New_Copy_Tree (Arg),
              Right_Opnd => Make_Null (Loc)),
            New_Occurrence_Of (RTE (RE_Null_Address), Loc),
            Make_Attribute_Reference (Loc,
              Attribute_Name => Name_Address,
              Prefix => Obj))));

      Analyze_And_Resolve (N, RTE (RE_Address));
   end Expand_To_Address;

   -----------------------
   -- Expand_To_Pointer --
   -----------------------

   procedure Expand_To_Pointer (N : Node_Id) is
      Arg : constant Node_Id := First_Actual (N);

   begin
      Rewrite (N, Unchecked_Convert_To (Etype (N), Arg));
      Analyze (N);
   end Expand_To_Pointer;

end Exp_Intr;
