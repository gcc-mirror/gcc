------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Smem; use Exp_Smem;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Exp_VFpt; use Exp_VFpt;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch2 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Current_Value (N : Node_Id);
   --  Given a node N for a variable whose Current_Value field is set.
   --  If the node is for a discrete type, replaces the node with a
   --  copy of the referenced value. This provides a limited form of
   --  value propagation for variables which are initialized or assigned
   --  not been further modified at the time of reference. The call has
   --  no effect if the Current_Value refers to a conditional with a
   --  condition other than equality.

   procedure Expand_Discriminant (N : Node_Id);
   --  An occurrence of a discriminant within a discriminated type is replaced
   --  with the corresponding discriminal, that is to say the formal parameter
   --  of the initialization procedure for the type that is associated with
   --  that particular discriminant. This replacement is not performed for
   --  discriminants of records that appear in constraints of component of the
   --  record, because Gigi uses the discriminant name to retrieve its value.
   --  In the other hand, it has to be performed for default expressions of
   --  components because they are used in the record init procedure. See
   --  Einfo for more details, and Exp_Ch3, Exp_Ch9 for examples of use.
   --  For discriminants of tasks and protected types, the transformation is
   --  more complex when it occurs within a default expression for an entry
   --  or protected operation. The corresponding default_expression_function
   --  has an additional parameter which is the target of an entry call, and
   --  the discriminant of the task must be replaced with a reference to the
   --  discriminant of that formal parameter.

   procedure Expand_Entity_Reference (N : Node_Id);
   --  Common processing for expansion of identifiers and expanded names

   procedure Expand_Entry_Index_Parameter (N : Node_Id);
   --  A reference to the identifier in the entry index specification
   --  of a protected entry body is modified to a reference to a constant
   --  definintion equal to the index of the entry family member being
   --  called. This constant is calculated as part of the elaboration
   --  of the expanded code for the body, and is calculated from the
   --  object-wide entry index returned by Next_Entry_Call.

   procedure Expand_Entry_Parameter (N : Node_Id);
   --  A reference to an entry parameter is modified to be a reference to
   --  the corresponding component of the entry parameter record that is
   --  passed by the runtime to the accept body procedure

   procedure Expand_Formal (N : Node_Id);
   --  A reference to a formal parameter of a protected subprogram is
   --  expanded to the corresponding formal of the unprotected procedure
   --  used to represent the protected subprogram within the protected object.

   procedure Expand_Protected_Private (N : Node_Id);
   --  A reference to a private object of a protected type is expanded
   --  to a component selected from the record used to implement
   --  the protected object. Such a record is passed to all operations
   --  on a protected object in a parameter named _object. Such an object
   --  is a constant within a function, and a variable otherwise.

   procedure Expand_Renaming (N : Node_Id);
   --  For renamings, just replace the identifier by the corresponding
   --  name expression. Note that this has been evaluated (see routine
   --  Exp_Ch8.Expand_N_Object_Renaming.Evaluate_Name) so this gives
   --  the correct renaming semantics.

   --------------------------
   -- Expand_Current_Value --
   --------------------------

   procedure Expand_Current_Value (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      E   : constant Entity_Id  := Entity (N);
      CV  : constant Node_Id    := Current_Value (E);
      T   : constant Entity_Id  := Etype (N);
      Val : Node_Id;
      Op  : Node_Kind;

      function In_Appropriate_Scope return Boolean;
      --  Returns true if the current scope is the scope of E, or is a nested
      --  (to any level) package declaration, package body, or block of this
      --  scope. The idea is that such references are in the sequential
      --  execution sequence of statements executed after E is elaborated.

      --------------------------
      -- In_Appropriate_Scope --
      --------------------------

      function In_Appropriate_Scope return Boolean is
         ES : constant Entity_Id := Scope (E);
         CS : Entity_Id;

      begin
         CS := Current_Scope;

         loop
            --  If we are in right scope, replacement is safe

            if CS = ES then
               return True;

            --  Packages do not affect the determination of safety

            elsif Ekind (CS) = E_Package then
               CS := Scope (CS);
               exit when CS = Standard_Standard;

            --  Blocks do not affect the determination of safety

            elsif Ekind (CS) = E_Block then
               CS := Scope (CS);

            --  Otherwise, the reference is dubious, and we cannot be
            --  sure that it is safe to do the replacement.

            else
               exit;
            end if;
         end loop;

         return False;
      end In_Appropriate_Scope;

   --  Start of processing for Expand_Current_Value

   begin
      if True

         --  No replacement if value raises constraint error

         and then Nkind (CV) /= N_Raise_Constraint_Error

         --  Do this only for discrete types

         and then Is_Discrete_Type (T)

         --  Do not replace biased types, since it is problematic to
         --  consistently generate a sensible constant value in this case.

         and then not Has_Biased_Representation (T)

         --  Do not replace lvalues

         and then not Is_Lvalue (N)

         --  Do not replace occurrences that are not in the current scope,
         --  because in a nested subprogram we know absolutely nothing about
         --  the sequence of execution.

         and then In_Appropriate_Scope

         --  Do not replace statically allocated objects, because they may
         --  be modified outside the current scope.

         and then not Is_Statically_Allocated (E)

         --  Do not replace aliased or volatile objects, since we don't know
         --  what else might change the value

         and then not Is_Aliased (E) and then not Treat_As_Volatile (E)

         --  Debug flag -gnatdM disconnects this optimization

         and then not Debug_Flag_MM

         --  Do not replace occurrences in pragmas (where names typically
         --  appear not as values, but as simply names. If there are cases
         --  where values are required, it is only a very minor efficiency
         --  issue that they do not get replaced when they could be).

         and then Nkind (Parent (N)) /= N_Pragma_Argument_Association

         --  Same for Asm_Input and Asm_Output attribute references

         and then not (Nkind (Parent (N)) = N_Attribute_Reference
                         and then
                           (Attribute_Name (Parent (N)) = Name_Asm_Input
                              or else
                            Attribute_Name (Parent (N)) = Name_Asm_Output))
      then
         --  Case of Current_Value is a compile time known value

         if Nkind (CV) in N_Subexpr then
            Val := CV;

         --  Case of Current_Value is a conditional expression reference

         else
            Get_Current_Value_Condition (N, Op, Val);

            if Op /= N_Op_Eq then
               return;
            end if;
         end if;

         --  If constant value is an occurrence of an enumeration literal,
         --  then we just make another occurence of the same literal.

         if Is_Entity_Name (Val)
           and then Ekind (Entity (Val)) = E_Enumeration_Literal
         then
            Rewrite (N,
              Unchecked_Convert_To (T,
                New_Occurrence_Of (Entity (Val), Loc)));

         --  Otherwise get the value, and convert to appropriate type

         else
            Rewrite (N,
              Unchecked_Convert_To (T,
                Make_Integer_Literal (Loc,
                  Intval => Expr_Rep_Value (Val))));
         end if;

         Analyze_And_Resolve (N, T);
         Set_Is_Static_Expression (N, False);
      end if;
   end Expand_Current_Value;

   -------------------------
   -- Expand_Discriminant --
   -------------------------

   procedure Expand_Discriminant (N : Node_Id) is
      Scop     : constant Entity_Id := Scope (Entity (N));
      P        : Node_Id := N;
      Parent_P : Node_Id := Parent (P);
      In_Entry : Boolean := False;

   begin
      --  The Incomplete_Or_Private_Kind happens while resolving the
      --  discriminant constraint involved in a derived full type,
      --  such as:

      --    type D is private;
      --    type D(C : ...) is new T(C);

      if Ekind (Scop) = E_Record_Type
        or Ekind (Scop) in Incomplete_Or_Private_Kind
      then
         --  Find the origin by walking up the tree till the component
         --  declaration

         while Present (Parent_P)
           and then Nkind (Parent_P) /= N_Component_Declaration
         loop
            P := Parent_P;
            Parent_P := Parent (P);
         end loop;

         --  If the discriminant reference was part of the default expression
         --  it has to be "discriminalized"

         if Present (Parent_P) and then P = Expression (Parent_P) then
            Set_Entity (N, Discriminal (Entity (N)));
         end if;

      elsif Is_Concurrent_Type (Scop) then
         while Present (Parent_P)
           and then Nkind (Parent_P) /= N_Subprogram_Body
         loop
            P := Parent_P;

            if Nkind (P) = N_Entry_Declaration then
               In_Entry := True;
            end if;

            Parent_P := Parent (Parent_P);
         end loop;

         --  If the discriminant occurs within the default expression for
         --  a formal of an entry or protected operation, create a default
         --  function for it, and replace the discriminant with a reference
         --  to the discriminant of the formal of the default function.
         --  The discriminant entity is the one defined in the corresponding
         --  record.

         if Present (Parent_P)
           and then Present (Corresponding_Spec (Parent_P))
         then
            declare
               Loc    : constant Source_Ptr := Sloc (N);
               D_Fun  : constant Entity_Id := Corresponding_Spec  (Parent_P);
               Formal : constant Entity_Id := First_Formal (D_Fun);
               New_N  : Node_Id;
               Disc   : Entity_Id;

            begin
               --  Verify that we are within a default function: the type of
               --  its formal parameter is the same task or protected type.

               if Present (Formal)
                 and then Etype (Formal) = Scope (Entity (N))
               then
                  Disc := CR_Discriminant (Entity (N));

                  New_N :=
                    Make_Selected_Component (Loc,
                      Prefix => New_Occurrence_Of (Formal, Loc),
                      Selector_Name => New_Occurrence_Of (Disc, Loc));

                  Set_Etype (New_N, Etype (N));
                  Rewrite (N, New_N);

               else
                  Set_Entity (N, Discriminal (Entity (N)));
               end if;
            end;

         elsif Nkind (Parent (N)) = N_Range
           and then In_Entry
         then
            Set_Entity (N, CR_Discriminant (Entity (N)));
         else
            Set_Entity (N, Discriminal (Entity (N)));
         end if;

      else
         Set_Entity (N, Discriminal (Entity (N)));
      end if;
   end Expand_Discriminant;

   -----------------------------
   -- Expand_Entity_Reference --
   -----------------------------

   procedure Expand_Entity_Reference (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      --  Defend against errors

      if No (E) and then Total_Errors_Detected /= 0 then
         return;
      end if;

      if Ekind (E) = E_Discriminant then
         Expand_Discriminant (N);

      elsif Is_Entry_Formal (E) then
         Expand_Entry_Parameter (N);

      elsif Ekind (E) = E_Component
        and then Is_Protected_Private (E)
      then
         --  Protect against junk use of tasking in no run time mode

         if No_Run_Time_Mode then
            return;
         end if;

         Expand_Protected_Private (N);

      elsif Ekind (E) = E_Entry_Index_Parameter then
         Expand_Entry_Index_Parameter (N);

      elsif Is_Formal (E) then
         Expand_Formal (N);

      elsif Is_Renaming_Of_Object (E) then
         Expand_Renaming (N);

      elsif Ekind (E) = E_Variable
        and then Is_Shared_Passive (E)
      then
         Expand_Shared_Passive_Variable (N);

      elsif (Ekind (E) = E_Variable
               or else
             Ekind (E) = E_In_Out_Parameter
               or else
             Ekind (E) = E_Out_Parameter)
        and then Present (Current_Value (E))
      then
         Expand_Current_Value (N);

         --  We do want to warn for the case of a boolean variable (not
         --  a boolean constant) whose value is known at compile time.

         if Is_Boolean_Type (Etype (N)) then
            Warn_On_Known_Condition (N);
         end if;
      end if;
   end Expand_Entity_Reference;

   ----------------------------------
   -- Expand_Entry_Index_Parameter --
   ----------------------------------

   procedure Expand_Entry_Index_Parameter (N : Node_Id) is
   begin
      Set_Entity (N, Entry_Index_Constant (Entity (N)));
   end Expand_Entry_Index_Parameter;

   ----------------------------
   -- Expand_Entry_Parameter --
   ----------------------------

   procedure Expand_Entry_Parameter (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Ent_Formal : constant Entity_Id  := Entity (N);
      Ent_Spec   : constant Entity_Id  := Scope (Ent_Formal);
      Parm_Type  : constant Entity_Id  := Entry_Parameters_Type (Ent_Spec);
      Acc_Stack  : constant Elist_Id   := Accept_Address (Ent_Spec);
      Addr_Ent   : constant Entity_Id  := Node (Last_Elmt (Acc_Stack));
      P_Comp_Ref : Entity_Id;

      function In_Assignment_Context (N : Node_Id) return Boolean;
      --  Check whether this is a context in which the entry formal may
      --  be assigned to.

      ---------------------------
      -- In_Assignment_Context --
      ---------------------------

      function In_Assignment_Context (N : Node_Id) return Boolean is
      begin
         if Nkind (Parent (N)) = N_Procedure_Call_Statement
           or else Nkind (Parent (N)) = N_Entry_Call_Statement
           or else
             (Nkind (Parent (N)) = N_Assignment_Statement
                 and then N = Name (Parent (N)))
         then
            return True;

         elsif Nkind (Parent (N)) = N_Parameter_Association then
            return In_Assignment_Context (Parent (N));

         elsif (Nkind (Parent (N)) = N_Selected_Component
                 or else Nkind (Parent (N)) = N_Indexed_Component
                 or else Nkind (Parent (N)) = N_Slice)
           and then In_Assignment_Context (Parent (N))
         then
            return True;
         else
            return False;
         end if;
      end In_Assignment_Context;

   --  Start of processing for Expand_Entry_Parameter

   begin
      if Is_Task_Type (Scope (Ent_Spec))
        and then Comes_From_Source (Ent_Formal)
      then
         --  Before replacing the formal with the local renaming that is
         --  used in the accept block, note if this is an assignment
         --  context, and note the modification to avoid spurious warnings,
         --  because the original entity is not used further.
         --  If the formal is unconstrained, we also generate an extra
         --  parameter to hold the Constrained attribute of the actual. No
         --  renaming is generated for this flag.

         if Ekind (Entity (N)) /= E_In_Parameter
           and then In_Assignment_Context (N)
         then
            Note_Possible_Modification (N);
         end if;

         Rewrite (N, New_Occurrence_Of (Renamed_Object (Entity (N)), Loc));
         return;
      end if;

      --  What we need is a reference to the corresponding component of the
      --  parameter record object. The Accept_Address field of the entry
      --  entity references the address variable that contains the address
      --  of the accept parameters record. We first have to do an unchecked
      --  conversion to turn this into a pointer to the parameter record and
      --  then we select the required parameter field.

      P_Comp_Ref :=
        Make_Selected_Component (Loc,
          Prefix =>
            Make_Explicit_Dereference (Loc,
              Unchecked_Convert_To (Parm_Type,
                New_Reference_To (Addr_Ent, Loc))),
          Selector_Name =>
            New_Reference_To (Entry_Component (Ent_Formal), Loc));

      --  For all types of parameters, the constructed parameter record
      --  object contains a pointer to the parameter. Thus we must
      --  dereference them to access them (this will often be redundant,
      --  since the needed deference is implicit, but no harm is done by
      --  making it explicit).

      Rewrite (N,
        Make_Explicit_Dereference (Loc, P_Comp_Ref));

      Analyze (N);
   end Expand_Entry_Parameter;

   -------------------
   -- Expand_Formal --
   -------------------

   procedure Expand_Formal (N : Node_Id) is
      E    : constant Entity_Id  := Entity (N);
      Subp : constant Entity_Id  := Scope (E);

   begin
      if Is_Protected_Type (Scope (Subp))
        and then not Is_Init_Proc (Subp)
        and then Present (Protected_Formal (E))
      then
         Set_Entity (N, Protected_Formal (E));
      end if;
   end Expand_Formal;

   ----------------------------
   -- Expand_N_Expanded_Name --
   ----------------------------

   procedure Expand_N_Expanded_Name (N : Node_Id) is
   begin
      Expand_Entity_Reference (N);
   end Expand_N_Expanded_Name;

   -------------------------
   -- Expand_N_Identifier --
   -------------------------

   procedure Expand_N_Identifier (N : Node_Id) is
   begin
      Expand_Entity_Reference (N);
   end Expand_N_Identifier;

   ---------------------------
   -- Expand_N_Real_Literal --
   ---------------------------

   procedure Expand_N_Real_Literal (N : Node_Id) is
   begin
      if Vax_Float (Etype (N)) then
         Expand_Vax_Real_Literal (N);
      end if;
   end Expand_N_Real_Literal;

   ------------------------------
   -- Expand_Protected_Private --
   ------------------------------

   procedure Expand_Protected_Private (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      E        : constant Entity_Id  := Entity (N);
      Op       : constant Node_Id    := Protected_Operation (E);
      Scop     : Entity_Id;
      Lo       : Node_Id;
      Hi       : Node_Id;
      D_Range  : Node_Id;

   begin
      if Nkind (Op) /= N_Subprogram_Body
        or else Nkind (Specification (Op)) /= N_Function_Specification
      then
         Set_Ekind (Prival (E), E_Variable);
      else
         Set_Ekind (Prival (E), E_Constant);
      end if;

      --  If the private component appears in an assignment (either lhs or
      --  rhs) and is a one-dimensional array constrained by a discriminant,
      --  rewrite as  P (Lo .. Hi) with an explicit range, so that discriminal
      --  is directly visible. This solves delicate visibility problems.

      if Comes_From_Source (N)
        and then Is_Array_Type (Etype (E))
        and then Number_Dimensions (Etype (E)) = 1
        and then not Within_Init_Proc
      then
         Lo := Type_Low_Bound  (Etype (First_Index (Etype (E))));
         Hi := Type_High_Bound (Etype (First_Index (Etype (E))));

         if Nkind (Parent (N)) = N_Assignment_Statement
           and then ((Is_Entity_Name (Lo)
                          and then Ekind (Entity (Lo)) = E_In_Parameter)
                       or else (Is_Entity_Name (Hi)
                                  and then
                                    Ekind (Entity (Hi)) = E_In_Parameter))
         then
            D_Range := New_Node (N_Range, Loc);

            if Is_Entity_Name (Lo)
              and then Ekind (Entity (Lo)) = E_In_Parameter
            then
               Set_Low_Bound (D_Range,
                 Make_Identifier (Loc, Chars (Entity (Lo))));
            else
               Set_Low_Bound (D_Range, Duplicate_Subexpr (Lo));
            end if;

            if Is_Entity_Name (Hi)
              and then Ekind (Entity (Hi)) = E_In_Parameter
            then
               Set_High_Bound (D_Range,
                 Make_Identifier (Loc, Chars (Entity (Hi))));
            else
               Set_High_Bound (D_Range, Duplicate_Subexpr (Hi));
            end if;

            Rewrite (N,
              Make_Slice (Loc,
                Prefix => New_Occurrence_Of (E, Loc),
                Discrete_Range => D_Range));

            Analyze_And_Resolve (N, Etype (E));
            return;
         end if;
      end if;

      --  The type of the reference is the type of the prival, which may
      --  differ from that of the original component if it is an itype.

      Set_Entity (N, Prival (E));
      Set_Etype  (N, Etype (Prival (E)));
      Scop := Current_Scope;

      --  Find entity for protected operation, which must be on scope stack

      while not Is_Protected_Type (Scope (Scop)) loop
         Scop := Scope (Scop);
      end loop;

      Append_Elmt (N, Privals_Chain (Scop));
   end Expand_Protected_Private;

   ---------------------
   -- Expand_Renaming --
   ---------------------

   procedure Expand_Renaming (N : Node_Id) is
      E : constant Entity_Id := Entity (N);
      T : constant Entity_Id := Etype (N);

   begin
      Rewrite (N, New_Copy_Tree (Renamed_Object (E)));

      --  We mark the copy as unanalyzed, so that it is sure to be
      --  reanalyzed at the top level. This is needed in the packed
      --  case since we specifically avoided expanding packed array
      --  references when the renaming declaration was analyzed.

      Reset_Analyzed_Flags (N);
      Analyze_And_Resolve (N, T);
   end Expand_Renaming;

   ------------------
   -- Param_Entity --
   ------------------

   --  This would be trivial, simply a test for an identifier that was a
   --  reference to a formal, if it were not for the fact that a previous
   --  call to Expand_Entry_Parameter will have modified the reference
   --  to the identifier. A formal of a protected entity is rewritten as

   --    typ!(recobj).rec.all'Constrained

   --  where rec is a selector whose Entry_Formal link points to the formal
   --  For a formal of a task entity, the formal is rewritten as a local
   --  renaming.

   --  In addition, a formal that is marked volatile because it is aliased
   --  through an address clause is rewritten as dereference as well.

   function Param_Entity (N : Node_Id) return Entity_Id is
   begin
      --  Simple reference case

      if Nkind (N) = N_Identifier or else Nkind (N) = N_Expanded_Name then
         if Is_Formal (Entity (N)) then
            return Entity (N);

         elsif Nkind (Parent (Entity (N))) = N_Object_Renaming_Declaration
           and then Nkind (Parent (Parent (Entity (N)))) = N_Accept_Statement
         then
            return Entity (N);
         end if;

      else
         if Nkind (N) = N_Explicit_Dereference then
            declare
               P : constant Node_Id := Prefix (N);
               S : Node_Id;

            begin
               if Nkind (P) = N_Selected_Component then
                  S := Selector_Name (P);

                  if Present (Entry_Formal (Entity (S))) then
                     return Entry_Formal (Entity (S));
                  end if;

               elsif Nkind (Original_Node (N)) = N_Identifier then
                  return Param_Entity (Original_Node (N));
               end if;
            end;
         end if;
      end if;

      return (Empty);
   end Param_Entity;

end Exp_Ch2;
