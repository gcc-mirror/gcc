------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
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
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Exp_Smem;       use Exp_Smem;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Output;         use Output;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Eval;       use Sem_Eval;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sem_Warn;       use Sem_Warn;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stand;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Uintp;           use Uintp;

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

         and then not Known_To_Be_Assigned (N)

         --  Check that entity is suitable for replacement

         and then OK_To_Do_Constant_Replacement (E)

         --  Do not replace the prefixes of attribute references, since this
         --  causes trouble with cases like 4'Size. Also for Name_Asm_Input and
         --  Name_Asm_Output, don't do replacement anywhere, since we can have
         --  lvalue references in the arguments.

         and then not (Nkind (Parent (N)) = N_Attribute_Reference
                        and then
                          (Attribute_Name (Parent (N)) in Name_Asm_Input
                                                        | Name_Asm_Output
                            or else Prefix (Parent (N)) = N))
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

         --  If constant is of a character type, just make an appropriate
         --  character literal, which will get the proper type.

         elsif Is_Character_Type (T) then
            Rewrite (N,
              Make_Character_Literal (Loc,
                Chars => Chars (Val),
                Char_Literal_Value => Expr_Rep_Value (Val)));

         --  If constant is of an integer type, just make an appropriate
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

      function Is_Object_Renaming_Name (N : Node_Id) return Boolean;
      --  Indicates that N occurs (after accounting for qualified expressions
      --  and type conversions) as the name of an object renaming declaration.
      --  We don't want to fold values in that case.

      -----------------------------
      -- Is_Object_Renaming_Name --
      -----------------------------

      function Is_Object_Renaming_Name (N : Node_Id) return Boolean is
         Trailer : Node_Id := N;
         Rover   : Node_Id;
      begin
         loop
            Rover := Parent (Trailer);
            case Nkind (Rover) is
               when N_Qualified_Expression | N_Type_Conversion =>
                  --  Conservative for type conversions; only necessary if
                  --  conversion does not introduce a new object (as opposed
                  --  to a new view of an existing object).
                  null;
               when N_Object_Renaming_Declaration =>
                  return Trailer = Name (Rover);
               when others =>
                  return False; -- the usual case
            end case;
            Trailer := Rover;
         end loop;
      end Is_Object_Renaming_Name;

      --  Local variables

      E : constant Entity_Id := Entity (N);

   --  Start of processing for Expand_Entity_Reference

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
        and then not Known_To_Be_Assigned (N)
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

      --  Set Atomic_Sync_Required if necessary for atomic variable. Note that
      --  this processing does NOT apply to Volatile_Full_Access variables.

      if Nkind (N) in N_Identifier | N_Expanded_Name
        and then Ekind (E) = E_Variable
        and then (Is_Atomic (E) or else Is_Atomic (Etype (E)))
      then
         declare
            Set : Boolean;

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
               Set := not Atomic_Synchronization_Disabled (E)
                        and then
                      not Atomic_Synchronization_Disabled (Etype (E));
            end if;

            --  Set flag if required

            if Set then
               Activate_Atomic_Synchronization (N);
            end if;
         end;
      end if;

      --  Interpret possible Current_Value for variable case. The
      --  Is_Object_Renaming_Name test is needed for cases such as
      --    X : Integer := 1;
      --    Y : Integer renames Integer'(X);
      --  where the value of Y is changed by any subsequent assignments to X.
      --  In cases like this, we do not want to use Current_Value even though
      --  it is available.

      if Is_Assignable (E)
        and then Present (Current_Value (E))
        and then not Is_Object_Renaming_Name (N)
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
           and then Known_To_Be_Assigned (N)
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
                New_Occurrence_Of (Addr_Ent, Loc))),
          Selector_Name =>
            New_Occurrence_Of (Entry_Component (Ent_Formal), Loc));

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

   ------------------------------------------
   -- Expand_N_Interpolated_String_Literal --
   ------------------------------------------

   procedure Expand_N_Interpolated_String_Literal (N : Node_Id) is

      procedure Apply_Static_Length_Check (Typ : Entity_Id);
      --  Tries to determine statically whether the length of the interpolated
      --  string N exceeds the length of the target subtype Typ. If it can be
      --  determined at compile time then an N_Raise_Constraint_Error node
      --  replaces the interpolated string N, and a warning message is issued.

      function Build_Interpolated_String_Image (N : Node_Id) return Node_Id;
      --  Build the following Expression_With_Actions node:
      --     do
      --        Sink : Buffer;
      --        [ Set_Trim_Leading_Spaces (Sink); ]
      --        Type'Put_Image (Sink, X);
      --        { [ Set_Trim_Leading_Spaces (Sink); ]
      --          Type'Put_Image (Sink, X); }
      --        Result : constant String := Get (Sink);
      --        Destroy (Sink);
      --     in Result end

      -------------------------------
      -- Apply_Static_Length_Check --
      -------------------------------

      procedure Apply_Static_Length_Check (Typ : Entity_Id) is
         HB         : constant Node_Id := High_Bound (First_Index (Typ));
         LB         : constant Node_Id := Low_Bound (First_Index (Typ));
         Str_Elem   : Node_Id;
         Str_Length : Nat;
         Typ_Length : Nat;

      begin
         if Compile_Time_Known_Value (LB)
           and then Compile_Time_Known_Value (HB)
         then
            Typ_Length := UI_To_Int (Expr_Value (HB) - Expr_Value (LB) + 1);

            --  Compute the minimum length of the interpolated string: the
            --  length of the concatenation of the string literals composing
            --  the interpolated string.

            Str_Length := 0;
            Str_Elem   := First (Expressions (N));
            while Present (Str_Elem) loop
               if Nkind (Str_Elem) = N_String_Literal then
                  Str_Length := Str_Length + String_Length (Strval (Str_Elem));
               end if;

               Next (Str_Elem);
            end loop;

            if Str_Length > Typ_Length then
               Apply_Compile_Time_Constraint_Error
                 (N, "wrong length for interpolated string of}??",
                  CE_Length_Check_Failed,
                  Ent => Typ,
                  Typ => Typ);
            end if;
         end if;
      end Apply_Static_Length_Check;

      -------------------------------------
      -- Build_Interpolated_String_Image --
      -------------------------------------

      function Build_Interpolated_String_Image (N : Node_Id) return Node_Id
      is
         Loc           : constant Source_Ptr := Sloc (N);
         Sink_Entity   : constant Entity_Id  := Make_Temporary (Loc, 'S');
         Sink_Decl     : constant Node_Id :=
                           Make_Object_Declaration (Loc,
                             Defining_Identifier => Sink_Entity,
                             Object_Definition =>
                               New_Occurrence_Of (RTE (RE_Buffer_Type), Loc));

         B_Type        : constant Entity_Id := Base_Type (Etype (N));
         Get_Id        : constant RE_Id :=
                           (if B_Type = Stand.Standard_String then
                               RE_Get
                            elsif B_Type = Stand.Standard_Wide_String then
                               RE_Wide_Get
                            else
                               RE_Wide_Wide_Get);

         Result_Entity : constant Entity_Id := Make_Temporary (Loc, 'R');
         Result_Decl   : constant Node_Id :=
                           Make_Object_Declaration (Loc,
                             Defining_Identifier => Result_Entity,
                             Object_Definition =>
                               New_Occurrence_Of (B_Type, Loc),
                             Expression =>
                               Make_Function_Call (Loc,
                                 Name => New_Occurrence_Of (RTE (Get_Id), Loc),
                                 Parameter_Associations => New_List (
                                   New_Occurrence_Of (Sink_Entity, Loc))));

         Actions  : constant List_Id := New_List;
         U_Type   : constant Entity_Id := Underlying_Type (Etype (N));
         Elem_Typ : Entity_Id;
         Str_Elem : Node_Id;

      begin
         pragma Assert (Etype (N) /= Stand.Any_String);

         Append_To (Actions, Sink_Decl);

         Str_Elem := First (Expressions (N));
         while Present (Str_Elem) loop
            Elem_Typ := Etype (Str_Elem);

            --  If the type is numeric or has a specified Integer_Literal or
            --  Real_Literal aspect, then prior to invoking Put_Image, the
            --  Trim_Leading_Spaces flag is set on the text buffer.

            if Is_Numeric_Type (Underlying_Type (Elem_Typ))
              or else Has_Aspect (Elem_Typ, Aspect_Integer_Literal)
              or else Has_Aspect (Elem_Typ, Aspect_Real_Literal)
            then
               Append_To (Actions,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of
                       (RTE (RE_Set_Trim_Leading_Spaces), Loc),
                   Parameter_Associations => New_List (
                     Convert_To (RTE (RE_Root_Buffer_Type),
                       New_Occurrence_Of (Sink_Entity, Loc)),
                     New_Occurrence_Of (Stand.Standard_True, Loc))));
            end if;

            Append_To (Actions,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Elem_Typ, Loc),
                Attribute_Name => Name_Put_Image,
                Expressions    => New_List (
                  New_Occurrence_Of (Sink_Entity, Loc),
                  Duplicate_Subexpr (Str_Elem))));

            Next (Str_Elem);
         end loop;

         --  Add a type conversion to the result object declaration of custom
         --  string types.

         if not Is_Standard_String_Type (U_Type)
           and then (not RTU_Loaded (Interfaces_C)
                       or else Enclosing_Lib_Unit_Entity (U_Type)
                                 /= RTU_Entity (Interfaces_C))
         then
            Set_Expression (Result_Decl,
              Convert_To (Etype (N),
                Relocate_Node (Expression (Result_Decl))));
         end if;

         Append_To (Actions, Result_Decl);

         return Make_Expression_With_Actions (Loc,
           Actions    => Actions,
           Expression => New_Occurrence_Of (Result_Entity, Loc));
      end Build_Interpolated_String_Image;

      --  Local variables

      Typ : constant Entity_Id := Etype (N);

   --  Start of processing for Expand_N_Interpolated_String_Literal

   begin
      --  If the type imposed by the context is constrained then check that
      --  the statically known length of the interpolated string does not
      --  exceed the length of its type.

      if Is_Constrained (Typ) then
         Apply_Static_Length_Check (Typ);

         if Nkind (N) = N_Raise_Constraint_Error then
            return;
         end if;
      end if;

      Rewrite (N, Build_Interpolated_String_Image (N));
      Analyze_And_Resolve (N, Typ);

      if Is_Constrained (Typ) then
         Apply_Length_Check (Expression (N), Typ);
      end if;
   end Expand_N_Interpolated_String_Literal;

end Exp_Ch2;
