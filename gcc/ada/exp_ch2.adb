------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Smem; use Exp_Smem;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch2 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Current_Value (N : Node_Id);
   --  N is a node for a variable whose Current_Value field is set. If N is
   --  node is for a discrete type, replaces node with a copy of the referenced
   --  value. This provides a limited form of value propagation for variables
   --  which are initialized or assigned not been further modified at the time
   --  of reference. The call has no effect if the Current_Value refers to a
   --  conditional with condition other than equality.

   procedure Expand_Discriminant (N : Node_Id);
   --  An occurrence of a discriminant within a discriminated type is replaced
   --  with the corresponding discriminal, that is to say the formal parameter
   --  of the initialization procedure for the type that is associated with
   --  that particular discriminant. This replacement is not performed for
   --  discriminants of records that appear in constraints of component of the
   --  record, because Gigi uses the discriminant name to retrieve its value.
   --  In the other hand, it has to be performed for default expressions of
   --  components because they are used in the record init procedure. See Einfo
   --  for more details, and Exp_Ch3, Exp_Ch9 for examples of use. For
   --  discriminants of tasks and protected types, the transformation is more
   --  complex when it occurs within a default expression for an entry or
   --  protected operation. The corresponding default_expression_function has
   --  an additional parameter which is the target of an entry call, and the
   --  discriminant of the task must be replaced with a reference to the
   --  discriminant of that formal parameter.

   procedure Expand_Entity_Reference (N : Node_Id);
   --  Common processing for expansion of identifiers and expanded names
   --  Dispatches to specific expansion procedures.

   procedure Expand_Entry_Index_Parameter (N : Node_Id);
   --  A reference to the identifier in the entry index specification of an
   --  entry body is modified to a reference to a constant definition equal to
   --  the index of the entry family member being called. This constant is
   --  calculated as part of the elaboration of the expanded code for the body,
   --  and is calculated from the object-wide entry index returned by Next_
   --  Entry_Call.

   procedure Expand_Entry_Parameter (N : Node_Id);
   --  A reference to an entry parameter is modified to be a reference to the
   --  corresponding component of the entry parameter record that is passed by
   --  the runtime to the accept body procedure.

   procedure Expand_Formal (N : Node_Id);
   --  A reference to a formal parameter of a protected subprogram is expanded
   --  into the corresponding formal of the unprotected procedure used to
   --  represent the operation within the protected object. In other cases
   --  Expand_Formal is a no-op.

   procedure Expand_Protected_Component (N : Node_Id);
   --  A reference to a private component of a protected type is expanded into
   --  a reference to the corresponding prival in the current protected entry
   --  or subprogram.

   procedure Expand_Renaming (N : Node_Id);
   --  For renamings, just replace the identifier by the corresponding
   --  named expression. Note that this has been evaluated (see routine
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

         and then not May_Be_Lvalue (N)

         --  Check that entity is suitable for replacement

         and then OK_To_Do_Constant_Replacement (E)

         --  Do not replace occurrences in pragmas (where names typically
         --  appear not as values, but as simply names. If there are cases
         --  where values are required, it is only a very minor efficiency
         --  issue that they do not get replaced when they could be).

         and then Nkind (Parent (N)) /= N_Pragma_Argument_Association

         --  Do not replace the prefixes of attribute references, since this
         --  causes trouble with cases like 4'Size. Also for Name_Asm_Input and
         --  Name_Asm_Output, don't do replacement anywhere, since we can have
         --  lvalue references in the arguments.

         and then not (Nkind (Parent (N)) = N_Attribute_Reference
                         and then
                           (Attribute_Name (Parent (N)) = Name_Asm_Input
                              or else
                            Attribute_Name (Parent (N)) = Name_Asm_Output
                              or else
                            Prefix (Parent (N)) = N))

      then
         --  Case of Current_Value is a compile time known value

         if Nkind (CV) in N_Subexpr then
            Val := CV;

         --  Case of Current_Value is an if expression reference

         else
            Get_Current_Value_Condition (N, Op, Val);

            if Op /= N_Op_Eq then
               return;
            end if;
         end if;

         --  If constant value is an occurrence of an enumeration literal,
         --  then we just make another occurrence of the same literal.

         if Is_Entity_Name (Val)
           and then Ekind (Entity (Val)) = E_Enumeration_Literal
         then
            Rewrite (N,
              Unchecked_Convert_To (T,
                New_Occurrence_Of (Entity (Val), Loc)));

         --  If constant is of an integer type, just make an appropriately
         --  integer literal, which will get the proper type.

         elsif Is_Integer_Type (T) then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Intval => Expr_Rep_Value (Val)));

         --  Otherwise do unchecked conversion of value to right type

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

         --  If the discriminant occurs within the default expression for a
         --  formal of an entry or protected operation, replace it with a
         --  reference to the discriminant of the formal of the enclosing
         --  operation.

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
               --  Verify that we are within the body of an entry or protected
               --  operation. Its first formal parameter is the synchronized
               --  type itself.

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

            --  Finally, if the entity is the discriminant of the original
            --  type declaration, and we are within the initialization
            --  procedure for a task, the designated entity is the
            --  discriminal of the task body. This can happen when the
            --  argument of pragma Task_Name mentions a discriminant,
            --  because the pragma is analyzed in the task declaration
            --  but is expanded in the call to Create_Task in the init_proc.

         elsif Within_Init_Proc then
            Set_Entity (N, Discriminal (CR_Discriminant (Entity (N))));
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

      if No (E) then
         Check_Error_Detected;
         return;
      end if;

      if Ekind (E) = E_Discriminant then
         Expand_Discriminant (N);

      elsif Is_Entry_Formal (E) then
         Expand_Entry_Parameter (N);

      elsif Is_Protected_Component (E) then
         if No_Run_Time_Mode then
            return;
         else
            Expand_Protected_Component (N);
         end if;

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
      end if;

      --  Test code for implementing the pragma Reviewable requirement of
      --  classifying reads of scalars as referencing potentially uninitialized
      --  objects or not.

      if Debug_Flag_XX
        and then Is_Scalar_Type (Etype (N))
        and then (Is_Assignable (E) or else Is_Constant_Object (E))
        and then Comes_From_Source (N)
        and then not Is_LHS (N)
        and then not Is_Actual_Out_Parameter (N)
        and then (Nkind (Parent (N)) /= N_Attribute_Reference
                   or else Attribute_Name (Parent (N)) /= Name_Valid)
      then
         Write_Location (Sloc (N));
         Write_Str (": Read from scalar """);
         Write_Name (Chars (N));
         Write_Str ("""");

         if Is_Known_Valid (E) then
            Write_Str (", Is_Known_Valid");
         end if;

         Write_Eol;
      end if;

      --  Set Atomic_Sync_Required if necessary for atomic variable

      if Nkind_In (N, N_Identifier, N_Expanded_Name)
        and then Ekind (E) = E_Variable
        and then (Is_Atomic (E) or else Is_Atomic (Etype (E)))
      then
         declare
            Set  : Boolean;

         begin
            --  If variable is atomic, but type is not, setting depends on
            --  disable/enable state for the variable.

            if Is_Atomic (E) and then not Is_Atomic (Etype (E)) then
               Set := not Atomic_Synchronization_Disabled (E);

            --  If variable is not atomic, but its type is atomic, setting
            --  depends on disable/enable state for the type.

            elsif not Is_Atomic (E) and then Is_Atomic (Etype (E)) then
               Set := not Atomic_Synchronization_Disabled (Etype (E));

            --  Else both variable and type are atomic (see outer if), and we
            --  disable if either variable or its type have sync disabled.

            else
               Set := (not Atomic_Synchronization_Disabled (E))
                        and then
                      (not Atomic_Synchronization_Disabled (Etype (E)));
            end if;

            --  Set flag if required

            if Set then
               Activate_Atomic_Synchronization (N);
            end if;
         end;
      end if;

      --  Interpret possible Current_Value for variable case

      if Is_Assignable (E)
        and then Present (Current_Value (E))
      then
         Expand_Current_Value (N);

         --  We do want to warn for the case of a boolean variable (not a
         --  boolean constant) whose value is known at compile time.

         if Is_Boolean_Type (Etype (N)) then
            Warn_On_Known_Condition (N);
         end if;

      --  Don't mess with Current_Value for compile time known values. Not
      --  only is it unnecessary, but we could disturb an indication of a
      --  static value, which could cause semantic trouble.

      elsif Compile_Time_Known_Value (N) then
         null;

      --  Interpret possible Current_Value for constant case

      elsif Is_Constant_Object (E)
        and then Present (Current_Value (E))
      then
         Expand_Current_Value (N);
      end if;
   end Expand_Entity_Reference;

   ----------------------------------
   -- Expand_Entry_Index_Parameter --
   ----------------------------------

   procedure Expand_Entry_Index_Parameter (N : Node_Id) is
      Index_Con : constant Entity_Id := Entry_Index_Constant (Entity (N));
   begin
      Set_Entity (N, Index_Con);
      Set_Etype  (N, Etype (Index_Con));
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
      --  Check whether this is a context in which the entry formal may be
      --  assigned to.

      ---------------------------
      -- In_Assignment_Context --
      ---------------------------

      function In_Assignment_Context (N : Node_Id) return Boolean is
      begin
         --  Case of use in a call

         --  ??? passing a formal as actual for a mode IN formal is
         --  considered as an assignment?

         if Nkind_In (Parent (N), N_Procedure_Call_Statement,
                                  N_Entry_Call_Statement)
           or else (Nkind (Parent (N)) = N_Assignment_Statement
                      and then N = Name (Parent (N)))
         then
            return True;

         --  Case of a parameter association: climb up to enclosing call

         elsif Nkind (Parent (N)) = N_Parameter_Association then
            return In_Assignment_Context (Parent (N));

         --  Case of a selected component, indexed component or slice prefix:
         --  climb up the tree, unless the prefix is of an access type (in
         --  which case there is an implicit dereference, and the formal itself
         --  is not being assigned to).

         elsif Nkind_In (Parent (N), N_Selected_Component,
                                     N_Indexed_Component,
                                     N_Slice)
           and then N = Prefix (Parent (N))
           and then not Is_Access_Type (Etype (N))
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
         --  Before replacing the formal with the local renaming that is used
         --  in the accept block, note if this is an assignment context, and
         --  note the modification to avoid spurious warnings, because the
         --  original entity is not used further. If formal is unconstrained,
         --  we also generate an extra parameter to hold the Constrained
         --  attribute of the actual. No renaming is generated for this flag.

         --  Calling Note_Possible_Modification in the expander is dubious,
         --  because this generates a cross-reference entry, and should be
         --  done during semantic processing so it is called in -gnatc mode???

         if Ekind (Entity (N)) /= E_In_Parameter
           and then In_Assignment_Context (N)
         then
            Note_Possible_Modification (N, Sure => True);
         end if;
      end if;

      --  What we need is a reference to the corresponding component of the
      --  parameter record object. The Accept_Address field of the entry entity
      --  references the address variable that contains the address of the
      --  accept parameters record. We first have to do an unchecked conversion
      --  to turn this into a pointer to the parameter record and then we
      --  select the required parameter field.

      --  The same processing applies to protected entries, where the Accept_
      --  Address is also the address of the Parameters record.

      P_Comp_Ref :=
        Make_Selected_Component (Loc,
          Prefix =>
            Make_Explicit_Dereference (Loc,
              Unchecked_Convert_To (Parm_Type,
                New_Reference_To (Addr_Ent, Loc))),
          Selector_Name =>
            New_Reference_To (Entry_Component (Ent_Formal), Loc));

      --  For all types of parameters, the constructed parameter record object
      --  contains a pointer to the parameter. Thus we must dereference them to
      --  access them (this will often be redundant, since the dereference is
      --  implicit, but no harm is done by making it explicit).

      Rewrite (N,
        Make_Explicit_Dereference (Loc, P_Comp_Ref));

      Analyze (N);
   end Expand_Entry_Parameter;

   -------------------
   -- Expand_Formal --
   -------------------

   procedure Expand_Formal (N : Node_Id) is
      E    : constant Entity_Id  := Entity (N);
      Scop : constant Entity_Id  := Scope (E);

   begin
      --  Check whether the subprogram of which this is a formal is
      --  a protected operation. The initialization procedure for
      --  the corresponding record type is not itself a protected operation.

      if Is_Protected_Type (Scope (Scop))
        and then not Is_Init_Proc (Scop)
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
      pragma Unreferenced (N);

   begin
      --  Historically, this routine existed because there were expansion
      --  requirements for Vax real literals, but now Vax real literals
      --  are now handled by gigi, so this routine no longer does anything.

      null;
   end Expand_N_Real_Literal;

   --------------------------------
   -- Expand_Protected_Component --
   --------------------------------

   procedure Expand_Protected_Component (N : Node_Id) is

      function Inside_Eliminated_Body return Boolean;
      --  Determine whether the current entity is inside a subprogram or an
      --  entry which has been marked as eliminated.

      ----------------------------
      -- Inside_Eliminated_Body --
      ----------------------------

      function Inside_Eliminated_Body return Boolean is
         S : Entity_Id := Current_Scope;

      begin
         while Present (S) loop
            if (Ekind (S) = E_Entry
                  or else Ekind (S) = E_Entry_Family
                  or else Ekind (S) = E_Function
                  or else Ekind (S) = E_Procedure)
              and then Is_Eliminated (S)
            then
               return True;
            end if;

            S := Scope (S);
         end loop;

         return False;
      end Inside_Eliminated_Body;

   --  Start of processing for Expand_Protected_Component

   begin
      --  Eliminated bodies are not expanded and thus do not need privals

      if not Inside_Eliminated_Body then
         declare
            Priv : constant Entity_Id := Prival (Entity (N));
         begin
            Set_Entity (N, Priv);
            Set_Etype  (N, Etype (Priv));
         end;
      end if;
   end Expand_Protected_Component;

   ---------------------
   -- Expand_Renaming --
   ---------------------

   procedure Expand_Renaming (N : Node_Id) is
      E : constant Entity_Id := Entity (N);
      T : constant Entity_Id := Etype (N);

   begin
      Rewrite (N, New_Copy_Tree (Renamed_Object (E)));

      --  We mark the copy as unanalyzed, so that it is sure to be reanalyzed
      --  at the top level. This is needed in the packed case since we
      --  specifically avoided expanding packed array references when the
      --  renaming declaration was analyzed.

      Reset_Analyzed_Flags (N);
      Analyze_And_Resolve (N, T);
   end Expand_Renaming;

   ------------------
   -- Param_Entity --
   ------------------

   --  This would be trivial, simply a test for an identifier that was a
   --  reference to a formal, if it were not for the fact that a previous call
   --  to Expand_Entry_Parameter will have modified the reference to the
   --  identifier. A formal of a protected entity is rewritten as

   --    typ!(recobj).rec.all'Constrained

   --  where rec is a selector whose Entry_Formal link points to the formal

   --  If the type of the entry parameter has a representation clause, then an
   --  extra temp is involved (see below).

   --  For a formal of a task entity, the formal is rewritten as a local
   --  renaming.

   --  In addition, a formal that is marked volatile because it is aliased
   --  through an address clause is rewritten as dereference as well.

   function Param_Entity (N : Node_Id) return Entity_Id is
      Renamed_Obj : Node_Id;

   begin
      --  Simple reference case

      if Nkind_In (N, N_Identifier, N_Expanded_Name) then
         if Is_Formal (Entity (N)) then
            return Entity (N);

         --  Handle renamings of formal parameters and formals of tasks that
         --  are rewritten as renamings.

         elsif Nkind (Parent (Entity (N))) = N_Object_Renaming_Declaration then
            Renamed_Obj := Get_Referenced_Object (Renamed_Object (Entity (N)));

            if Is_Entity_Name (Renamed_Obj)
              and then Is_Formal (Entity (Renamed_Obj))
            then
               return Entity (Renamed_Obj);

            elsif
              Nkind (Parent (Parent (Entity (N)))) = N_Accept_Statement
            then
               return Entity (N);
            end if;
         end if;

      else
         if Nkind (N) = N_Explicit_Dereference then
            declare
               P    : Node_Id := Prefix (N);
               S    : Node_Id;
               E    : Entity_Id;
               Decl : Node_Id;

            begin
               --  If the type of an entry parameter has a representation
               --  clause, then the prefix is not a selected component, but
               --  instead a reference to a temp pointing at the selected
               --  component. In this case, set P to be the initial value of
               --  that temp.

               if Nkind (P) = N_Identifier then
                  E := Entity (P);

                  if Ekind (E) = E_Constant then
                     Decl := Parent (E);

                     if Nkind (Decl) = N_Object_Declaration then
                        P := Expression (Decl);
                     end if;
                  end if;
               end if;

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
