------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 9                               --
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

with Accessibility;  use Accessibility;
with Aspects;        use Aspects;
with Atree;          use Atree;
with Checks;         use Checks;
with Contracts;      use Contracts;
with Debug;          use Debug;
with Diagnostics.Constructors; use Diagnostics.Constructors;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Exp_Ch9;        use Exp_Ch9;
with Elists;         use Elists;
with Freeze;         use Freeze;
with Layout;         use Layout;
with Lib;            use Lib;
with Lib.Xref;       use Lib.Xref;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch5;        use Sem_Ch5;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Elab;       use Sem_Elab;
with Sem_Eval;       use Sem_Eval;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sem_Warn;       use Sem_Warn;
with Snames;         use Snames;
with Stand;          use Stand;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Style;
with Targparm;       use Targparm;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;
package body Sem_Ch9 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Allows_Lock_Free_Implementation
     (N               : Node_Id;
      Lock_Free_Given : Boolean := False) return Boolean;
   --  This routine returns True iff N satisfies the following list of lock-
   --  free restrictions for protected type declaration and protected body:
   --
   --    1) Protected type declaration
   --         May not contain entries
   --         Protected subprogram declarations may not have non-elementary
   --           parameters.
   --
   --    2) Protected Body
   --         Each protected subprogram body within N must satisfy:
   --            May reference only one protected component
   --            May not reference non-constant entities outside the protected
   --              subprogram scope.
   --            May not contain address representation items, allocators and
   --              quantified expressions.
   --            May not contain delay, goto, loop and procedure call
   --              statements.
   --            May not contain exported and imported entities
   --            May not dereference access values
   --            Function calls and attribute references must be static
   --
   --  If Lock_Free_Given is True, an error message is issued when False is
   --  returned.

   procedure Check_Max_Entries (D : Node_Id; R : All_Parameter_Restrictions);
   --  Given either a protected definition or a task definition in D, check
   --  the corresponding restriction parameter identifier R, and if it is set,
   --  count the entries (checking the static requirement), and compare with
   --  the given maximum.

   procedure Check_Interfaces (N : Node_Id; T : Entity_Id);
   --  N is an N_Protected_Type_Declaration or N_Task_Type_Declaration node.
   --  Complete decoration of T and check legality of the covered interfaces.

   procedure Check_Triggering_Statement
     (Trigger        : Node_Id;
      Error_Node     : Node_Id;
      Is_Dispatching : out Boolean);
   --  Examine the triggering statement of a select statement, conditional or
   --  timed entry call. If Trigger is a dispatching call, return its status
   --  in Is_Dispatching and check whether the primitive belongs to a limited
   --  interface. If it does not, emit an error at Error_Node.

   function Find_Concurrent_Spec (Body_Id : Entity_Id) return Entity_Id;
   --  Find entity in corresponding task or protected declaration. Use full
   --  view if first declaration was for an incomplete type.

   -------------------------------------
   -- Allows_Lock_Free_Implementation --
   -------------------------------------

   function Allows_Lock_Free_Implementation
     (N               : Node_Id;
      Lock_Free_Given : Boolean := False) return Boolean
   is
      Errors_Count : Nat := 0;
      --  Errors_Count is a count of errors detected by the compiler so far
      --  when Lock_Free_Given is True.

   begin
      pragma Assert
        (Nkind (N) in N_Protected_Type_Declaration | N_Protected_Body);

      --  Get the number of errors detected by the compiler so far

      if Lock_Free_Given then
         Errors_Count := Serious_Errors_Detected;
      end if;

      --  Protected type declaration case

      if Nkind (N) = N_Protected_Type_Declaration then
         declare
            Pdef       : constant Node_Id := Protected_Definition (N);
            Priv_Decls : constant List_Id := Private_Declarations (Pdef);
            Vis_Decls  : constant List_Id := Visible_Declarations (Pdef);
            Decl       : Node_Id;

         begin
            --  Examine the visible and the private declarations

            Decl := First (Vis_Decls);
            while Present (Decl) loop

               --  Entries and entry families are not allowed by the lock-free
               --  restrictions.

               if Nkind (Decl) = N_Entry_Declaration then
                  if Lock_Free_Given then
                     Error_Msg_N
                       ("entry not allowed when Lock_Free given", Decl);
                  else
                     return False;
                  end if;

               --  Non-elementary parameters in protected procedure are not
               --  allowed by the lock-free restrictions.

               elsif Nkind (Decl) = N_Subprogram_Declaration
                 and then
                   Nkind (Specification (Decl)) = N_Procedure_Specification
               then
                  declare
                     Par_Specs : constant List_Id   :=
                                   Parameter_Specifications
                                     (Specification (Decl));

                     Par : Node_Id;

                  begin
                     Par := First (Par_Specs);
                     while Present (Par) loop
                        if not Is_Elementary_Type
                                 (Etype (Defining_Identifier (Par)))
                        then
                           if Lock_Free_Given then
                              Error_Msg_NE
                                ("non-elementary parameter& not allowed "
                                 & "when Lock_Free given",
                                 Par, Defining_Identifier (Par));
                           else
                              return False;
                           end if;
                        end if;

                        Next (Par);
                     end loop;
                  end;

               elsif Nkind (Decl) = N_Subprogram_Declaration
                 and then
                   Nkind (Specification (Decl)) = N_Function_Specification
                 and then
                   Nkind (Result_Definition (Specification (Decl)))
                     in N_Has_Entity
                 and then
                   Needs_Secondary_Stack
                     (Entity (Result_Definition (Specification (Decl))))
               then
                  if Lock_Free_Given then
                     --  Message text is imprecise; "unconstrained" is
                     --  similar to "needs secondary stack" but not identical.
                     Error_Msg_N
                       ("unconstrained function result subtype not allowed "
                        & "when Lock_Free given",
                        Decl);
                  else
                     return False;
                  end if;
               end if;

               --  Examine private declarations after visible declarations

               if No (Next (Decl))
                 and then List_Containing (Decl) = Vis_Decls
               then
                  Decl := First (Priv_Decls);
               else
                  Next (Decl);
               end if;
            end loop;
         end;

      --  Protected body case

      else
         Protected_Body_Case : declare
            Decls         : constant List_Id   := Declarations (N);
            Pid           : constant Entity_Id := Corresponding_Spec (N);
            Prot_Typ_Decl : constant Node_Id   := Parent (Pid);
            Prot_Def      : constant Node_Id   :=
                              Protected_Definition (Prot_Typ_Decl);
            Priv_Decls    : constant List_Id   :=
                              Private_Declarations (Prot_Def);
            Decl          : Node_Id;

            function Satisfies_Lock_Free_Requirements
              (Sub_Body : Node_Id) return Boolean;
            --  Return True if protected subprogram body Sub_Body satisfies all
            --  requirements of a lock-free implementation.

            --------------------------------------
            -- Satisfies_Lock_Free_Requirements --
            --------------------------------------

            function Satisfies_Lock_Free_Requirements
              (Sub_Body : Node_Id) return Boolean
            is
               Comp : Entity_Id := Empty;
               --  Track the current component which the body references

               Errors_Count : Nat := 0;
               --  Errors_Count is a count of errors detected by the compiler
               --  so far when Lock_Free_Given is True.

               function Check_Node (N : Node_Id) return Traverse_Result;
               --  Check that node N meets the lock free restrictions

               ----------------
               -- Check_Node --
               ----------------

               function Check_Node (N : Node_Id) return Traverse_Result is
                  Kind : constant Node_Kind := Nkind (N);

                  --  The following function belongs in sem_eval ???

                  function Is_Static_Function (Attr : Node_Id) return Boolean;
                  --  Given an attribute reference node Attr, return True if
                  --  Attr denotes a static function according to the rules in
                  --  (RM 4.9 (22)).

                  ------------------------
                  -- Is_Static_Function --
                  ------------------------

                  function Is_Static_Function
                    (Attr : Node_Id) return Boolean
                  is
                     Para : Node_Id;

                  begin
                     pragma Assert (Nkind (Attr) = N_Attribute_Reference);

                     case Attribute_Name (Attr) is
                        when Name_Max
                           | Name_Min
                           | Name_Pred
                           | Name_Succ
                           | Name_Value
                           | Name_Wide_Value
                           | Name_Wide_Wide_Value
                        =>
                           --  A language-defined attribute denotes a static
                           --  function if the prefix denotes a static scalar
                           --  subtype, and if the parameter and result types
                           --  are scalar (RM 4.9 (22)).

                           if Is_Scalar_Type (Etype (Attr))
                             and then Is_Scalar_Type (Etype (Prefix (Attr)))
                             and then
                               Is_OK_Static_Subtype (Etype (Prefix (Attr)))
                           then
                              Para := First (Expressions (Attr));

                              while Present (Para) loop
                                 if not Is_Scalar_Type (Etype (Para)) then
                                    return False;
                                 end if;

                                 Next (Para);
                              end loop;

                              return True;

                           else
                              return False;
                           end if;

                        when others =>
                           return False;
                     end case;
                  end Is_Static_Function;

               --  Start of processing for Check_Node

               begin
                  --  Allocators restricted

                  if Kind = N_Allocator then
                     if Lock_Free_Given then
                        Error_Msg_N ("allocator not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Aspects Address, Export and Import restricted

                  elsif Kind = N_Aspect_Specification then
                     declare
                        Asp_Name : constant Name_Id   :=
                                     Chars (Identifier (N));
                        Asp_Id   : constant Aspect_Id :=
                                     Get_Aspect_Id (Asp_Name);

                     begin
                        if Asp_Id = Aspect_Address or else
                           Asp_Id = Aspect_Export  or else
                           Asp_Id = Aspect_Import
                        then
                           Error_Msg_Name_1 := Asp_Name;

                           if Lock_Free_Given then
                              Error_Msg_N ("aspect% not allowed", N);
                              return Skip;
                           end if;

                           return Abandon;
                        end if;
                     end;

                  --  Address attribute definition clause restricted

                  elsif Kind = N_Attribute_Definition_Clause
                    and then Get_Attribute_Id (Chars (N)) =
                               Attribute_Address
                  then
                     Error_Msg_Name_1 := Chars (N);

                     if Lock_Free_Given then
                        if From_Aspect_Specification (N) then
                           Error_Msg_N ("aspect% not allowed", N);
                        else
                           Error_Msg_N ("% clause not allowed", N);
                        end if;

                        return Skip;
                     end if;

                     return Abandon;

                  --  Non-static Attribute references that don't denote a
                  --  static function restricted.

                  elsif Kind = N_Attribute_Reference
                    and then not Is_OK_Static_Expression (N)
                    and then not Is_Static_Function (N)
                  then
                     if Lock_Free_Given then
                        Error_Msg_N
                          ("non-static attribute reference not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Delay statements restricted

                  elsif Kind in N_Delay_Statement then
                     if Lock_Free_Given then
                        Error_Msg_N ("delay not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Dereferences of access values restricted

                  elsif Kind = N_Explicit_Dereference
                    or else (Kind = N_Selected_Component
                              and then Is_Access_Type (Etype (Prefix (N))))
                  then
                     if Lock_Free_Given then
                        Error_Msg_N
                          ("dereference of access value not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Non-static function calls restricted

                  elsif Kind = N_Function_Call
                    and then not Is_OK_Static_Expression (N)
                  then
                     if Lock_Free_Given then
                        Error_Msg_N
                          ("non-static function call not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Goto statements restricted

                  elsif Kind in N_Goto_Statement | N_Goto_When_Statement then
                     if Lock_Free_Given then
                        Error_Msg_N ("goto statement not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  References

                  elsif Kind in N_Identifier | N_Expanded_Name
                    and then Present (Entity (N))
                  then
                     declare
                        Id     : constant Entity_Id := Entity (N);
                        Sub_Id : constant Entity_Id :=
                                   Corresponding_Spec (Sub_Body);

                     begin
                        --  Prohibit references to non-constant entities
                        --  outside the protected subprogram scope.
                        --
                        --  References to variables in System.Scalar_Values
                        --  generated because of pragma Initialize_Scalars are
                        --  allowed, because once those variables are
                        --  initialized by the binder-generated code, they
                        --  behave like constants.

                        if Is_Assignable (Id)
                          and then not
                            Scope_Within_Or_Same (Scope (Id), Sub_Id)
                          and then not
                            Scope_Within_Or_Same
                              (Scope (Id),
                               Protected_Body_Subprogram (Sub_Id))
                          and then not
                            (Is_RTU (Scope (Id), System_Scalar_Values)
                               and then not Comes_From_Source (N))
                        then
                           if Lock_Free_Given then
                              Error_Msg_NE
                                ("reference to global variable& not allowed",
                                 N, Id);
                              return Skip;
                           end if;

                           return Abandon;
                        end if;
                     end;

                  --  Loop statements restricted

                  elsif Kind = N_Loop_Statement then
                     if Lock_Free_Given then
                        Error_Msg_N ("loop not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Pragmas Export and Import restricted

                  elsif Kind = N_Pragma then
                     declare
                        Prag_Name : constant Name_Id   :=
                          Pragma_Name (N);
                        Prag_Id   : constant Pragma_Id :=
                          Get_Pragma_Id (Prag_Name);

                     begin
                        if Prag_Id = Pragma_Export
                          or else Prag_Id = Pragma_Import
                        then
                           Error_Msg_Name_1 := Prag_Name;

                           if Lock_Free_Given then
                              if From_Aspect_Specification (N) then
                                 Error_Msg_N ("aspect% not allowed", N);
                              else
                                 Error_Msg_N ("pragma% not allowed", N);
                              end if;

                              return Skip;
                           end if;

                           return Abandon;
                        end if;
                     end;

                  --  Procedure call statements restricted

                  elsif Kind = N_Procedure_Call_Statement then
                     if Lock_Free_Given then
                        Error_Msg_N ("procedure call not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;

                  --  Quantified expression restricted. Note that we have
                  --  to check the original node as well, since at this
                  --  stage, it may have been rewritten.

                  elsif Kind = N_Quantified_Expression
                    or else
                      Nkind (Original_Node (N)) = N_Quantified_Expression
                  then
                     if Lock_Free_Given then
                        Error_Msg_N
                          ("quantified expression not allowed", N);
                        return Skip;
                     end if;

                     return Abandon;
                  end if;

                  --  A protected subprogram (function or procedure) may
                  --  reference only one component of the protected type, plus
                  --  the type of the component must support atomic operation.

                  if Kind in N_Identifier | N_Expanded_Name
                    and then Present (Entity (N))
                  then
                     declare
                        Id        : constant Entity_Id := Entity (N);
                        Comp_Decl : Node_Id;
                        Comp_Id   : Entity_Id := Empty;
                        Comp_Type : Entity_Id;

                     begin
                        if Ekind (Id) = E_Component then
                           Comp_Id := Id;

                        elsif Ekind (Id) in E_Constant | E_Variable
                          and then Present (Prival_Link (Id))
                        then
                           Comp_Id := Prival_Link (Id);
                        end if;

                        if Present (Comp_Id) then
                           Comp_Decl := Parent (Comp_Id);
                           Comp_Type := Etype (Comp_Id);

                           if Nkind (Comp_Decl) = N_Component_Declaration
                             and then Is_List_Member (Comp_Decl)
                             and then List_Containing (Comp_Decl) = Priv_Decls
                           then
                              --  Skip generic types since, in that case, we
                              --  will not build a body anyway (in the generic
                              --  template), and the size in the template may
                              --  have a fake value.

                              if not Is_Generic_Type (Comp_Type) then

                                 --  Make sure the protected component type has
                                 --  size and alignment fields set at this
                                 --  point whenever this is possible.

                                 Layout_Type (Comp_Type);

                                 if not
                                   Support_Atomic_Primitives (Comp_Type)
                                 then
                                    if Lock_Free_Given then
                                       Error_Msg_NE
                                         ("type of& must support atomic " &
                                          "operations",
                                          N, Comp_Id);
                                       return Skip;
                                    end if;

                                    return Abandon;
                                 end if;
                              end if;

                              --  Check if another protected component has
                              --  already been accessed by the subprogram body.

                              if No (Comp) then
                                 Comp := Comp_Id;

                              elsif Comp /= Comp_Id then
                                 if Lock_Free_Given then
                                    Error_Msg_N
                                      ("only one protected component allowed",
                                       N);
                                    return Skip;
                                 end if;

                                 return Abandon;
                              end if;
                           end if;
                        end if;
                     end;
                  end if;

                  return OK;
               end Check_Node;

               function Check_All_Nodes is new Traverse_Func (Check_Node);

            --  Start of processing for Satisfies_Lock_Free_Requirements

            begin
               if not Support_Atomic_Primitives_On_Target then
                  if Lock_Free_Given then
                     Error_Msg_N
                       ("Lock_Free aspect requires target support for "
                          & "atomic primitives", N);
                  end if;
                  return False;
               end if;

               --  Deal with case where Ceiling_Locking locking policy is
               --  in effect.

               if Locking_Policy = 'C' then
                  if Lock_Free_Given then
                     --  Explicit Lock_Free aspect spec overrides
                     --  Ceiling_Locking so we generate a warning.

                     Error_Msg_N
                       ("Lock_Free aspect specification overrides "
                          & "Ceiling_Locking locking policy??", N);
                  else
                     --  If Ceiling_Locking locking policy is in effect, then
                     --  Lock_Free can be explicitly specified but it is
                     --  never the default.

                     return False;
                  end if;
               end if;

               --  Get the number of errors detected by the compiler so far

               if Lock_Free_Given then
                  Errors_Count := Serious_Errors_Detected;
               end if;

               if Check_All_Nodes (Sub_Body) = OK
                 and then (not Lock_Free_Given
                            or else Errors_Count = Serious_Errors_Detected)
               then
                  --  Establish a relation between the subprogram body and the
                  --  unique protected component it references.

                  if Present (Comp) then
                     Lock_Free_Subprogram_Table.Append
                       (Lock_Free_Subprogram'(Sub_Body, Comp));
                  end if;

                  return True;
               else
                  return False;
               end if;
            end Satisfies_Lock_Free_Requirements;

         --  Start of processing for Protected_Body_Case

         begin
            Decl := First (Decls);
            while Present (Decl) loop
               if Nkind (Decl) = N_Subprogram_Body
                 and then not Satisfies_Lock_Free_Requirements (Decl)
               then
                  if Lock_Free_Given then
                     Error_Msg_N
                       ("illegal body when Lock_Free given", Decl);
                  else
                     return False;
                  end if;
               end if;

               Next (Decl);
            end loop;
         end Protected_Body_Case;
      end if;

      --  When Lock_Free is given, check if no error has been detected during
      --  the process.

      if Lock_Free_Given
        and then Errors_Count /= Serious_Errors_Detected
      then
         return False;
      end if;

      return True;
   end Allows_Lock_Free_Implementation;

   -----------------------------
   -- Analyze_Abort_Statement --
   -----------------------------

   procedure Analyze_Abort_Statement (N : Node_Id) is
      T_Name : Node_Id;

   begin
      Tasking_Used := True;

      T_Name := First (Names (N));
      while Present (T_Name) loop
         Analyze (T_Name);

         if Is_Task_Type (Etype (T_Name))
           or else (Ada_Version >= Ada_2005
                      and then Ekind (Etype (T_Name)) = E_Class_Wide_Type
                      and then Is_Interface (Etype (T_Name))
                      and then Is_Task_Interface (Etype (T_Name)))
         then
            Resolve (T_Name);
         else
            if Ada_Version >= Ada_2005 then
               Error_Msg_N ("expect task name or task interface class-wide "
                            & "object for ABORT", T_Name);
            else
               Error_Msg_N ("expect task name for ABORT", T_Name);
            end if;

            return;
         end if;

         Next (T_Name);
      end loop;

      Check_Restriction (No_Abort_Statements, N);
      Check_Potentially_Blocking_Operation (N);
   end Analyze_Abort_Statement;

   --------------------------------
   -- Analyze_Accept_Alternative --
   --------------------------------

   procedure Analyze_Accept_Alternative (N : Node_Id) is
   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Any_Boolean);
      end if;

      Analyze (Accept_Statement (N));

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Accept_Alternative;

   ------------------------------
   -- Analyze_Accept_Statement --
   ------------------------------

   procedure Analyze_Accept_Statement (N : Node_Id) is
      Nam       : constant Entity_Id := Entry_Direct_Name (N);
      Formals   : constant List_Id   := Parameter_Specifications (N);
      Index     : constant Node_Id   := Entry_Index (N);
      Stats     : constant Node_Id   := Handled_Statement_Sequence (N);
      Accept_Id : Entity_Id;
      Entry_Nam : Entity_Id;
      E         : Entity_Id;
      Kind      : Entity_Kind;
      Task_Nam  : Entity_Id := Empty;  -- initialize to prevent warning

   begin
      Tasking_Used := True;

      --  Entry name is initialized to Any_Id. It should get reset to the
      --  matching entry entity. An error is signalled if it is not reset.

      Entry_Nam := Any_Id;

      for J in reverse 0 .. Scope_Stack.Last loop
         Task_Nam := Scope_Stack.Table (J).Entity;
         exit when Ekind (Etype (Task_Nam)) = E_Task_Type;
         Kind := Ekind (Task_Nam);

         if Kind /= E_Block and then Kind /= E_Loop
           and then not Is_Entry (Task_Nam)
         then
            Error_Msg_N ("enclosing body of ACCEPT must be a task", N);
            return;
         end if;
      end loop;

      if Ekind (Etype (Task_Nam)) /= E_Task_Type then
         Error_Msg_N ("invalid context for ACCEPT statement",  N);
         return;
      end if;

      --  In order to process the parameters, we create a defining identifier
      --  that can be used as the name of the scope. The name of the accept
      --  statement itself is not a defining identifier, and we cannot use
      --  its name directly because the task may have any number of accept
      --  statements for the same entry.

      if Present (Index) then
         Accept_Id := New_Internal_Entity
           (E_Entry_Family, Current_Scope, Sloc (N), 'E');
      else
         Accept_Id := New_Internal_Entity
           (E_Entry, Current_Scope, Sloc (N), 'E');
      end if;

      Set_Etype          (Accept_Id, Standard_Void_Type);
      Set_Accept_Address (Accept_Id, New_Elmt_List);

      if Present (Formals) then
         Push_Scope (Accept_Id);
         Process_Formals (Formals, N);
         Create_Extra_Formals (Accept_Id);
         End_Scope;
      end if;

      --  We set the default expressions processed flag because we don't need
      --  default expression functions. This is really more like body entity
      --  than a spec entity anyway.

      Set_Default_Expressions_Processed (Accept_Id);

      E := First_Entity (Etype (Task_Nam));
      while Present (E) loop
         if Chars (E) = Chars (Nam)
           and then Ekind (E) = Ekind (Accept_Id)
           and then Type_Conformant (Accept_Id, E)
         then
            Entry_Nam := E;
            exit;
         end if;

         Next_Entity (E);
      end loop;

      if Entry_Nam = Any_Id then
         Error_Msg_N ("no entry declaration matches ACCEPT statement",  N);
         return;
      else
         Set_Entity (Nam, Entry_Nam);
         Generate_Reference (Entry_Nam, Nam, 'b', Set_Ref => False);
         Style.Check_Identifier (Nam, Entry_Nam);
      end if;

      --  Verify that the entry is not hidden by a procedure declared in the
      --  current block (pathological but possible).

      if Current_Scope /= Task_Nam then
         declare
            E1 : Entity_Id;

         begin
            E1 := First_Entity (Current_Scope);
            while Present (E1) loop
               if Ekind (E1) = E_Procedure
                 and then Chars (E1) = Chars (Entry_Nam)
                 and then Type_Conformant (E1, Entry_Nam)
               then
                  Error_Msg_N ("entry name is not visible", N);
               end if;

               Next_Entity (E1);
            end loop;
         end;
      end if;

      Set_Convention (Accept_Id, Convention (Entry_Nam));
      Check_Fully_Conformant (Accept_Id, Entry_Nam, N);

      for J in reverse 0 .. Scope_Stack.Last loop
         exit when Task_Nam = Scope_Stack.Table (J).Entity;

         if Entry_Nam = Scope_Stack.Table (J).Entity then
            Error_Msg_N
              ("duplicate ACCEPT statement for same entry (RM 9.5.2 (15))", N);

            --  Do not continue analysis of accept statement, to prevent
            --  cascaded errors.

            return;
         end if;
      end loop;

      declare
         P : Node_Id := N;
      begin
         loop
            P := Parent (P);
            case Nkind (P) is
               when N_Compilation_Unit
                  | N_Task_Body
               =>
                  exit;

               when N_Asynchronous_Select =>
                  Error_Msg_N
                    ("ACCEPT statement not allowed within an "
                     & "asynchronous SELECT inner to the enclosing task body",
                     N);
                  exit;

               when others =>
                  null;
            end case;
         end loop;
      end;

      if Ekind (Entry_Nam) = E_Entry_Family then
         if No (Index) then
            Error_Msg_N ("missing entry index in accept for entry family", N);
         else
            Analyze_And_Resolve (Index, Entry_Index_Type (Entry_Nam));
            Apply_Scalar_Range_Check (Index, Entry_Index_Type (Entry_Nam));
         end if;

      elsif Present (Index) then
         Error_Msg_N ("invalid entry index in accept for simple entry", N);
      end if;

      --  If label declarations present, analyze them. They are declared in the
      --  enclosing task, but their enclosing scope is the entry itself, so
      --  that goto's to the label are recognized as local to the accept.

      if Present (Declarations (N)) then
         declare
            Decl : Node_Id;
            Id   : Entity_Id;

         begin
            Decl := First (Declarations (N));
            while Present (Decl) loop
               Analyze (Decl);

               pragma Assert
                 (Nkind (Decl) = N_Implicit_Label_Declaration);

               Id := Defining_Identifier (Decl);
               Set_Enclosing_Scope (Id, Entry_Nam);
               Next (Decl);
            end loop;
         end;
      end if;

      --  If statements are present, they must be analyzed in the context of
      --  the entry, so that references to formals are correctly resolved. We
      --  also have to add the declarations that are required by the expansion
      --  of the accept statement in this case if expansion active.

      --  In the case of a select alternative of a selective accept, the
      --  expander references the address declaration even if there is no
      --  statement list.

      --  We also need to create the renaming declarations for the local
      --  variables that will replace references to the formals within the
      --  accept statement.

      Exp_Ch9.Expand_Accept_Declarations (N, Entry_Nam);

      --  Set Never_Set_In_Source and clear Is_True_Constant/Current_Value
      --  fields on all entry formals (this loop ignores all other entities).
      --  Reset Referenced, Referenced_As_xxx and Has_Pragma_Unreferenced as
      --  well, so that we can post accurate warnings on each accept statement
      --  for the same entry.

      E := First_Entity (Entry_Nam);
      while Present (E) loop
         if Is_Formal (E) then
            Set_Never_Set_In_Source         (E, True);
            Set_Is_True_Constant            (E, False);
            Set_Current_Value               (E, Empty);
            Set_Referenced                  (E, False);
            Set_Referenced_As_LHS           (E, False);
            Set_Referenced_As_Out_Parameter (E, False);
            Set_Has_Pragma_Unreferenced     (E, False);
         end if;

         Next_Entity (E);
      end loop;

      --  Analyze statements if present

      if Present (Stats) then
         Push_Scope (Entry_Nam);
         Install_Declarations (Entry_Nam);

         Set_Actual_Subtypes (N, Current_Scope);

         Analyze (Stats);
         Process_End_Label (Handled_Statement_Sequence (N), 't', Entry_Nam);
         End_Scope;
      end if;

      --  Some warning checks

      Check_Potentially_Blocking_Operation (N);
      Check_References (Entry_Nam, N);
      Set_Entry_Accepted (Entry_Nam);
   end Analyze_Accept_Statement;

   ---------------------------------
   -- Analyze_Asynchronous_Select --
   ---------------------------------

   procedure Analyze_Asynchronous_Select (N : Node_Id) is
      Is_Disp_Select : Boolean := False;
      Trigger        : Node_Id;

   begin
      Tasking_Used := True;
      Check_Restriction (Max_Asynchronous_Select_Nesting, N);
      Check_Restriction (No_Select_Statements, N);

      if Ada_Version >= Ada_2005 then
         Trigger := Triggering_Statement (Triggering_Alternative (N));

         Analyze (Trigger);

         --  Ada 2005 (AI-345): Check for a potential dispatching select

         Check_Triggering_Statement (Trigger, N, Is_Disp_Select);
      end if;

      --  Ada 2005 (AI-345): The expansion of the dispatching asynchronous
      --  select will have to duplicate the triggering statements. Postpone
      --  the analysis of the statements till expansion. Analyze only if the
      --  expander is disabled in order to catch any semantic errors.

      if Is_Disp_Select then
         if not Expander_Active then
            Analyze_Statements (Statements (Abortable_Part (N)));
            Analyze (Triggering_Alternative (N));
         end if;

      --  Analyze the statements. We analyze statements in the abortable part,
      --  because this is the section that is executed first, and that way our
      --  remembering of saved values and checks is accurate.

      else
         Analyze_Statements (Statements (Abortable_Part (N)));
         Analyze (Triggering_Alternative (N));
      end if;
   end Analyze_Asynchronous_Select;

   ------------------------------------
   -- Analyze_Conditional_Entry_Call --
   ------------------------------------

   procedure Analyze_Conditional_Entry_Call (N : Node_Id) is
      Trigger        : constant Node_Id :=
                         Entry_Call_Statement (Entry_Call_Alternative (N));
      Is_Disp_Select : Boolean := False;

   begin
      Tasking_Used := True;
      Check_Restriction (No_Select_Statements, N);

      --  Ada 2005 (AI-345): The trigger may be a dispatching call

      if Ada_Version >= Ada_2005 then
         Analyze (Trigger);
         Check_Triggering_Statement (Trigger, N, Is_Disp_Select);
      end if;

      if List_Length (Else_Statements (N)) = 1
        and then Nkind (First (Else_Statements (N))) in N_Delay_Statement
      then
         Error_Msg_N
           ("suspicious form of conditional entry call??!", N);
         Error_Msg_N
           ("\`SELECT OR` may be intended rather than `SELECT ELSE`??!", N);
      end if;

      --  Postpone the analysis of the statements till expansion. Analyze only
      --  if the expander is disabled in order to catch any semantic errors.

      if Is_Disp_Select then
         if not Expander_Active then
            Analyze (Entry_Call_Alternative (N));
            Analyze_Statements (Else_Statements (N));
         end if;

      --  Regular select analysis

      else
         Analyze (Entry_Call_Alternative (N));
         Analyze_Statements (Else_Statements (N));
      end if;
   end Analyze_Conditional_Entry_Call;

   --------------------------------
   -- Analyze_Delay_Alternative  --
   --------------------------------

   procedure Analyze_Delay_Alternative (N : Node_Id) is
      Expr : Node_Id;
      Typ  : Entity_Id;

   begin
      Tasking_Used := True;
      Check_Restriction (No_Delay, N);

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      if Nkind (Parent (N)) in N_Selective_Accept | N_Timed_Entry_Call then
         Expr := Expression (Delay_Statement (N));

         --  Defer full analysis until the statement is expanded, to insure
         --  that generated code does not move past the guard. The delay
         --  expression is only evaluated if the guard is open.

         if Nkind (Delay_Statement (N)) = N_Delay_Relative_Statement then
            Preanalyze_And_Resolve (Expr, Standard_Duration);
         else
            Preanalyze_And_Resolve (Expr);
         end if;

         Typ := First_Subtype (Etype (Expr));

         if Nkind (Delay_Statement (N)) = N_Delay_Until_Statement
           and then not Is_RTE (Typ, RO_CA_Time)
           and then not Is_RTE (Typ, RO_RT_Time)
         then
            Error_Msg_N ("expect Time types for `DELAY UNTIL`", Expr);
         end if;

         Check_Restriction (No_Fixed_Point, Expr);

      else
         Analyze (Delay_Statement (N));
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Any_Boolean);
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Delay_Alternative;

   ----------------------------
   -- Analyze_Delay_Relative --
   ----------------------------

   procedure Analyze_Delay_Relative (N : Node_Id) is
      E : constant Node_Id := Expression (N);

   begin
      Tasking_Used := True;
      Check_Restriction (No_Relative_Delay, N);
      Check_Restriction (No_Delay, N);
      Check_Potentially_Blocking_Operation (N);
      Analyze_And_Resolve (E, Standard_Duration);
      Check_Restriction (No_Fixed_Point, E);

      --  In SPARK mode the relative delay statement introduces an implicit
      --  dependency on the Ada.Real_Time.Clock_Time abstract state, so we must
      --  force the loading of the Ada.Real_Time package.

      if GNATprove_Mode then
         SPARK_Implicit_Load (RO_RT_Time);
      end if;
   end Analyze_Delay_Relative;

   -------------------------
   -- Analyze_Delay_Until --
   -------------------------

   procedure Analyze_Delay_Until (N : Node_Id) is
      E   : constant Node_Id := Expression (N);
      Typ : Entity_Id;

   begin
      Tasking_Used := True;
      Check_Restriction (No_Delay, N);
      Check_Potentially_Blocking_Operation (N);
      Analyze_And_Resolve (E);
      Typ := First_Subtype (Etype (E));

      if not Is_RTE (Typ, RO_CA_Time) and then
         not Is_RTE (Typ, RO_RT_Time)
      then
         Error_Msg_N ("expect Time types for `DELAY UNTIL`", E);
      end if;
   end Analyze_Delay_Until;

   ------------------------
   -- Analyze_Entry_Body --
   ------------------------

   procedure Analyze_Entry_Body (N : Node_Id) is
      Id         : constant Entity_Id := Defining_Identifier (N);
      Decls      : constant List_Id   := Declarations (N);
      Stats      : constant Node_Id   := Handled_Statement_Sequence (N);
      Formals    : constant Node_Id   := Entry_Body_Formal_Part (N);
      P_Type     : constant Entity_Id := Current_Scope;
      E          : Entity_Id;
      Entry_Name : Entity_Id;

   begin
      --  An entry body freezes the contract of the nearest enclosing package
      --  body and all other contracts encountered in the same declarative part
      --  up to and excluding the entry body. This ensures that any annotations
      --  referenced by the contract of an entry or subprogram body declared
      --  within the current protected body are available.

      Freeze_Previous_Contracts (N);

      Tasking_Used := True;

      --  Entry_Name is initialized to Any_Id. It should get reset to the
      --  matching entry entity. An error is signalled if it is not reset.

      Entry_Name := Any_Id;

      Analyze (Formals);

      if Present (Entry_Index_Specification (Formals)) then
         Mutate_Ekind (Id, E_Entry_Family);
      else
         Mutate_Ekind (Id, E_Entry);
      end if;

      Set_Etype          (Id, Standard_Void_Type);
      Set_Scope          (Id, Current_Scope);
      Set_Accept_Address (Id, New_Elmt_List);

      --  Set the SPARK_Mode from the current context (may be overwritten later
      --  with an explicit pragma).

      Set_SPARK_Pragma           (Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (Id);

      --  Analyze any aspect specifications that appear on the entry body

      Analyze_Aspects_On_Subprogram_Body_Or_Stub (N);

      E := First_Entity (P_Type);
      while Present (E) loop
         if Chars (E) = Chars (Id)
           and then Ekind (E) = Ekind (Id)
           and then Type_Conformant (Id, E)
         then
            Entry_Name := E;
            Set_Convention (Id, Convention (E));
            Set_Corresponding_Body (Parent (E), Id);
            Set_Corresponding_Spec (N, E);
            Check_Fully_Conformant (Id, E, N);

            if Ekind (Id) = E_Entry_Family then
               if not Fully_Conformant_Discrete_Subtypes (
                  Discrete_Subtype_Definition (Parent (E)),
                  Discrete_Subtype_Definition
                    (Entry_Index_Specification (Formals)))
               then
                  Error_Msg_N
                    ("index not fully conformant with previous declaration",
                      Discrete_Subtype_Definition
                       (Entry_Index_Specification (Formals)));

               else
                  --  The elaboration of the entry body does not recompute the
                  --  bounds of the index, which may have side effects. Inherit
                  --  the bounds from the entry declaration. This is critical
                  --  if the entry has a per-object constraint. If a bound is
                  --  given by a discriminant, it must be reanalyzed in order
                  --  to capture the discriminal of the current entry, rather
                  --  than that of the protected type.

                  declare
                     Index_Spec : constant Node_Id :=
                                    Entry_Index_Specification (Formals);

                     Def : constant Node_Id :=
                             New_Copy_Tree
                               (Discrete_Subtype_Definition (Parent (E)));

                  begin
                     if Nkind
                       (Original_Node
                         (Discrete_Subtype_Definition (Index_Spec))) = N_Range
                     then
                        Set_Etype (Def, Empty);
                        Set_Analyzed (Def, False);

                        --  Keep the original subtree to ensure a properly
                        --  formed tree.

                        Rewrite
                          (Discrete_Subtype_Definition (Index_Spec), Def);

                        Set_Analyzed (Low_Bound (Def), False);
                        Set_Analyzed (High_Bound (Def), False);

                        if Denotes_Discriminant (Low_Bound (Def)) then
                           Set_Entity (Low_Bound (Def), Empty);
                        end if;

                        if Denotes_Discriminant (High_Bound (Def)) then
                           Set_Entity (High_Bound (Def), Empty);
                        end if;

                        Analyze (Def);
                        Make_Index (Def, Index_Spec);
                        Set_Etype
                          (Defining_Identifier (Index_Spec), Etype (Def));
                     end if;
                  end;
               end if;
            end if;

            exit;
         end if;

         Next_Entity (E);
      end loop;

      if Entry_Name = Any_Id then
         Error_Msg_N ("no entry declaration matches entry body",  N);
         return;

      elsif Has_Completion (Entry_Name) then
         Error_Msg_N ("duplicate entry body", N);
         return;

      else
         Set_Has_Completion (Entry_Name);
         Generate_Reference (Entry_Name, Id, 'b', Set_Ref => False);
         Style.Check_Identifier (Id, Entry_Name);
      end if;

      Exp_Ch9.Expand_Entry_Barrier (N, Entry_Name);
      Push_Scope (Entry_Name);

      Install_Declarations (Entry_Name);
      Set_Actual_Subtypes (N, Current_Scope);

      --  The entity for the protected subprogram corresponding to the entry
      --  has been created. We retain the name of this entity in the entry
      --  body, for use when the corresponding subprogram body is created.
      --  Note that entry bodies have no Corresponding_Spec, and there is no
      --  easy link back in the tree between the entry body and the entity for
      --  the entry itself, which is why we must propagate some attributes
      --  explicitly from spec to body.

      Set_Protected_Body_Subprogram
        (Id, Protected_Body_Subprogram (Entry_Name));

      Set_Entry_Parameters_Type
        (Id, Entry_Parameters_Type (Entry_Name));

      --  Add a declaration for the Protection object, renaming declarations
      --  for the discriminals and privals and finally a declaration for the
      --  entry family index (if applicable).

      if Expander_Active
        and then Is_Protected_Type (P_Type)
      then
         Install_Private_Data_Declarations
           (Sloc (N), Entry_Name, P_Type, N, Decls);
      end if;

      if Present (Decls) then
         Analyze_Declarations (Decls);
         Inspect_Deferred_Constant_Completion (Decls);
      end if;

      --  Process the contract of the subprogram body after all declarations
      --  have been analyzed. This ensures that any contract-related pragmas
      --  are available through the N_Contract node of the body.

      Analyze_Entry_Or_Subprogram_Body_Contract (Id);

      if Present (Stats) then
         Analyze (Stats);
      end if;

      --  Check for unreferenced variables etc. Before the Check_References
      --  call, we transfer Never_Set_In_Source and Referenced flags from
      --  parameters in the spec to the corresponding entities in the body,
      --  since we want the warnings on the body entities. Note that we do not
      --  have to transfer Referenced_As_LHS, since that flag can only be set
      --  for simple variables, but we include Has_Pragma_Unreferenced,
      --  which may have been specified for a formal in the body.

      --  At the same time, we set the flags on the spec entities to suppress
      --  any warnings on the spec formals, since we also scan the spec.
      --  Finally, we propagate the Entry_Component attribute to the body
      --  formals, for use in the renaming declarations created later for the
      --  formals (see exp_ch9.Add_Formal_Renamings).

      declare
         E1 : Entity_Id;
         E2 : Entity_Id;

      begin
         E1 := First_Entity (Entry_Name);
         while Present (E1) loop
            E2 := First_Entity (Id);
            while Present (E2) loop
               exit when Chars (E1) = Chars (E2);
               Next_Entity (E2);
            end loop;

            --  If no matching body entity, then we already had a detected
            --  error of some kind, so just don't worry about these warnings.

            if No (E2) then
               goto Continue;
            end if;

            if Ekind (E1) = E_Out_Parameter then
               Set_Never_Set_In_Source (E2, Never_Set_In_Source (E1));
               Set_Never_Set_In_Source (E1, False);
            end if;

            Set_Referenced (E2, Referenced (E1));
            Set_Referenced (E1);
            Set_Has_Pragma_Unreferenced (E2, Has_Pragma_Unreferenced (E1));
            Set_Entry_Component (E2, Entry_Component (E1));

         <<Continue>>
            Next_Entity (E1);
         end loop;

         Check_References (Id);
      end;

      --  We still need to check references for the spec, since objects
      --  declared in the body are chained (in the First_Entity sense) to
      --  the spec rather than the body in the case of entries.

      Check_References (Entry_Name);

      --  Process the end label, and terminate the scope

      Process_End_Label (Handled_Statement_Sequence (N), 't', Entry_Name);
      Update_Use_Clause_Chain;
      End_Scope;

      --  If this is an entry family, remove the loop created to provide
      --  a scope for the entry index.

      if Ekind (Id) = E_Entry_Family
        and then Present (Entry_Index_Specification (Formals))
      then
         End_Scope;
      end if;
   end Analyze_Entry_Body;

   ------------------------------------
   -- Analyze_Entry_Body_Formal_Part --
   ------------------------------------

   procedure Analyze_Entry_Body_Formal_Part (N : Node_Id) is
      Id      : constant Entity_Id := Defining_Identifier (Parent (N));
      Index   : constant Node_Id   := Entry_Index_Specification (N);
      Formals : constant List_Id   := Parameter_Specifications (N);

   begin
      Tasking_Used := True;

      if Present (Index) then
         Analyze (Index);

         --  The entry index functions like a loop variable, thus it is known
         --  to have a valid value.

         Set_Is_Known_Valid (Defining_Identifier (Index));
      end if;

      if Present (Formals) then
         Set_Scope (Id, Current_Scope);
         Push_Scope (Id);
         Process_Formals (Formals, Parent (N));
         End_Scope;
      end if;
   end Analyze_Entry_Body_Formal_Part;

   ------------------------------------
   -- Analyze_Entry_Call_Alternative --
   ------------------------------------

   procedure Analyze_Entry_Call_Alternative (N : Node_Id) is
      Call : constant Node_Id := Entry_Call_Statement (N);

   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      if Nkind (Call) = N_Attribute_Reference then

         --  Possibly a stream attribute, but definitely illegal. Other
         --  illegalities, such as procedure calls, are diagnosed after
         --  resolution.

         Error_Msg_N ("entry call alternative requires an entry call", Call);
         return;
      end if;

      Analyze (Call);

      --  An indirect call in this context is illegal. A procedure call that
      --  does not involve a renaming of an entry is illegal as well, but this
      --  and other semantic errors are caught during resolution.

      if Nkind (Call) = N_Explicit_Dereference then
         Error_Msg_N
           ("entry call or dispatching primitive of interface required", N);
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Entry_Call_Alternative;

   -------------------------------
   -- Analyze_Entry_Declaration --
   -------------------------------

   procedure Analyze_Entry_Declaration (N : Node_Id) is
      D_Sdef  : constant Node_Id   := Discrete_Subtype_Definition (N);
      Def_Id  : constant Entity_Id := Defining_Identifier (N);
      Formals : constant List_Id   := Parameter_Specifications (N);

   begin
      Generate_Definition (Def_Id);

      Tasking_Used := True;

      --  Case of no discrete subtype definition

      if No (D_Sdef) then
         Mutate_Ekind (Def_Id, E_Entry);

      --  Processing for discrete subtype definition present

      else
         Enter_Name (Def_Id);
         Mutate_Ekind (Def_Id, E_Entry_Family);
         Analyze (D_Sdef);
         Make_Index (D_Sdef, N, Def_Id);

         --  Check subtype with predicate in entry family

         Bad_Predicated_Subtype_Use
           ("subtype& has predicate, not allowed in entry family",
            D_Sdef, Etype (D_Sdef));

         --  Check entry family static bounds outside allowed limits

         --  Note: originally this check was not performed here, but in that
         --  case the check happens deep in the expander, and the message is
         --  posted at the wrong location, and omitted in -gnatc mode.
         --  If the type of the entry index is a generic formal, no check
         --  is possible. In an instance, the check is not static and a run-
         --  time exception will be raised if the bounds are unreasonable.

         declare
            PEI : constant Entity_Id := RTE (RE_Protected_Entry_Index);
            LB  : constant Uint      := Expr_Value (Type_Low_Bound (PEI));
            UB  : constant Uint      := Expr_Value (Type_High_Bound (PEI));

            LBR : Node_Id;
            UBR : Node_Id;

         begin

            --  No bounds checking if the type is generic or if previous error.
            --  In an instance the check is dynamic.

            if Is_Generic_Type (Etype (D_Sdef))
              or else In_Instance
              or else Error_Posted (D_Sdef)
            then
               goto Skip_LB;

            elsif Nkind (D_Sdef) = N_Range then
               LBR := Low_Bound (D_Sdef);

            elsif Is_Entity_Name (D_Sdef)
              and then Is_Type (Entity (D_Sdef))
            then
               LBR := Type_Low_Bound (Entity (D_Sdef));

            else
               goto Skip_LB;
            end if;

            if Is_OK_Static_Expression (LBR)
              and then Expr_Value (LBR) < LB
            then
               Error_Msg_Uint_1 := LB;
               Error_Msg_N ("entry family low bound must be '>'= ^!", D_Sdef);
            end if;

         <<Skip_LB>>
            if Is_Generic_Type (Etype (D_Sdef))
              or else In_Instance
              or else Error_Posted (D_Sdef)
            then
               goto Skip_UB;

            elsif Nkind (D_Sdef) = N_Range then
               UBR := High_Bound (D_Sdef);

            elsif Is_Entity_Name (D_Sdef)
              and then Is_Type (Entity (D_Sdef))
            then
               UBR := Type_High_Bound (Entity (D_Sdef));

            else
               goto Skip_UB;
            end if;

            if Is_OK_Static_Expression (UBR)
              and then Expr_Value (UBR) > UB
            then
               Error_Msg_Uint_1 := UB;
               Error_Msg_N ("entry family high bound must be '<'= ^!", D_Sdef);
            end if;

         <<Skip_UB>>
            null;
         end;
      end if;

      --  Decorate Def_Id

      Set_Etype          (Def_Id, Standard_Void_Type);
      Set_Convention     (Def_Id, Convention_Entry);
      Set_Accept_Address (Def_Id, New_Elmt_List);

      --  Set the SPARK_Mode from the current context (may be overwritten later
      --  with an explicit pragma). Task entries are excluded because they are
      --  not completed by entry bodies.

      if Ekind (Current_Scope) = E_Protected_Type then
         Set_SPARK_Pragma           (Def_Id, SPARK_Mode_Pragma);
         Set_SPARK_Pragma_Inherited (Def_Id);
      end if;

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => Def_Id,
         Checks   => True,
         Warnings => True);

      --  Process formals

      if Present (Formals) then
         Set_Scope (Def_Id, Current_Scope);
         Push_Scope (Def_Id);
         Process_Formals (Formals, N);
         Create_Extra_Formals (Def_Id);
         End_Scope;
      end if;

      if Ekind (Def_Id) = E_Entry then
         New_Overloaded_Entity (Def_Id);
      end if;

      Generate_Reference_To_Formals (Def_Id);

      Analyze_Aspect_Specifications (N, Def_Id);
   end Analyze_Entry_Declaration;

   ---------------------------------------
   -- Analyze_Entry_Index_Specification --
   ---------------------------------------

   --  The Defining_Identifier of the entry index specification is local to the
   --  entry body, but it must be available in the entry barrier which is
   --  evaluated outside of the entry body. The index is eventually renamed as
   --  a run-time object, so its visibility is strictly a front-end concern. In
   --  order to make it available to the barrier, we create an additional
   --  scope, as for a loop, whose only declaration is the index name. This
   --  loop is not attached to the tree and does not appear as an entity local
   --  to the protected type, so its existence need only be known to routines
   --  that process entry families.

   procedure Analyze_Entry_Index_Specification (N : Node_Id) is
      Iden    : constant Node_Id   := Defining_Identifier (N);
      Def     : constant Node_Id   := Discrete_Subtype_Definition (N);
      Loop_Id : constant Entity_Id := Make_Temporary (Sloc (N), 'L');

   begin
      Tasking_Used := True;
      Analyze (Def);

      --  There is no elaboration of the entry index specification. Therefore,
      --  if the index is a range, it is not resolved and expanded, but the
      --  bounds are inherited from the entry declaration, and reanalyzed.
      --  See Analyze_Entry_Body.

      if Nkind (Def) /= N_Range then
         Make_Index (Def, N);
      end if;

      Mutate_Ekind (Loop_Id, E_Loop);
      Set_Scope (Loop_Id, Current_Scope);
      Push_Scope (Loop_Id);
      Enter_Name (Iden);
      Mutate_Ekind (Iden, E_Entry_Index_Parameter);
      Set_Etype (Iden, Etype (Def));
   end Analyze_Entry_Index_Specification;

   ----------------------------
   -- Analyze_Protected_Body --
   ----------------------------

   procedure Analyze_Protected_Body (N : Node_Id) is
      Body_Id : constant Entity_Id := Defining_Identifier (N);
      Last_E  : Entity_Id;

      Spec_Id : Entity_Id;
      --  This is initially the entity of the protected object or protected
      --  type involved, but is replaced by the protected type always in the
      --  case of a single protected declaration, since this is the proper
      --  scope to be used.

      Ref_Id : Entity_Id;
      --  This is the entity of the protected object or protected type
      --  involved, and is the entity used for cross-reference purposes (it
      --  differs from Spec_Id in the case of a single protected object, since
      --  Spec_Id is set to the protected type in this case).

      function Lock_Free_Disabled return Boolean;
      --  This routine returns False if the protected object has a Lock_Free
      --  aspect specification or a Lock_Free pragma that turns off the
      --  lock-free implementation (e.g. whose expression is False).

      ------------------------
      -- Lock_Free_Disabled --
      ------------------------

      function Lock_Free_Disabled return Boolean is
         Ritem : constant Node_Id :=
                   Get_Rep_Item
                     (Spec_Id, Name_Lock_Free, Check_Parents => False);

      begin
         if Present (Ritem) then

            --  Pragma with one argument

            if Nkind (Ritem) = N_Pragma
              and then Present (Pragma_Argument_Associations (Ritem))
            then
               return
                 Is_False
                   (Static_Boolean
                     (Expression
                       (First (Pragma_Argument_Associations (Ritem)))));

            --  Aspect Specification with expression present

            elsif Nkind (Ritem) = N_Aspect_Specification
              and then Present (Expression (Ritem))
            then
               return Is_False (Static_Boolean (Expression (Ritem)));

            --  Otherwise, return False

            else
               return False;
            end if;
         end if;

         return False;
      end Lock_Free_Disabled;

   --  Start of processing for Analyze_Protected_Body

   begin
      --  A protected body freezes the contract of the nearest enclosing
      --  package body and all other contracts encountered in the same
      --  declarative part up to and excluding the protected body. This
      --  ensures that any annotations referenced by the contract of an
      --  entry or subprogram body declared within the current protected
      --  body are available.

      Freeze_Previous_Contracts (N);

      Tasking_Used := True;
      Mutate_Ekind (Body_Id, E_Protected_Body);
      Set_Etype (Body_Id, Standard_Void_Type);
      Spec_Id := Find_Concurrent_Spec (Body_Id);

      if Present (Spec_Id) and then Ekind (Spec_Id) = E_Protected_Type then
         null;

      elsif Present (Spec_Id)
        and then Ekind (Etype (Spec_Id)) = E_Protected_Type
        and then not Comes_From_Source (Etype (Spec_Id))
      then
         null;

      else
         Error_Msg_N ("missing specification for protected body", Body_Id);
         return;
      end if;

      Ref_Id := Spec_Id;
      Generate_Reference (Ref_Id, Body_Id, 'b', Set_Ref => False);
      Style.Check_Identifier (Body_Id, Spec_Id);

      --  The declarations are always attached to the type

      if Ekind (Spec_Id) /= E_Protected_Type then
         Spec_Id := Etype (Spec_Id);
      end if;

      Analyze_Aspect_Specifications (N, Body_Id);

      Push_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Parent (Spec_Id), Body_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);
      Expand_Protected_Body_Declarations (N, Spec_Id);
      Last_E := Last_Entity (Spec_Id);

      Analyze_Declarations (Declarations (N));

      --  For visibility purposes, all entities in the body are private. Set
      --  First_Private_Entity accordingly, if there was no private part in the
      --  protected declaration.

      if No (First_Private_Entity (Spec_Id)) then
         if Present (Last_E) then
            Set_First_Private_Entity (Spec_Id, Next_Entity (Last_E));
         else
            Set_First_Private_Entity (Spec_Id, First_Entity (Spec_Id));
         end if;
      end if;

      Check_Completion (Body_Id);
      Check_References (Spec_Id);
      Process_End_Label (N, 't', Ref_Id);
      Update_Use_Clause_Chain;
      End_Scope;

      --  When a Lock_Free aspect specification/pragma forces the lock-free
      --  implementation, verify the protected body meets all the restrictions,
      --  otherwise Allows_Lock_Free_Implementation issues an error message.

      if Uses_Lock_Free (Spec_Id) then
         if not Allows_Lock_Free_Implementation (N, True) then
            return;
         end if;

      --  In other cases, if there is no aspect specification/pragma that
      --  disables the lock-free implementation, check both the protected
      --  declaration and body satisfy the lock-free restrictions.

      elsif not Lock_Free_Disabled
        and then Allows_Lock_Free_Implementation (Parent (Spec_Id))
        and then Allows_Lock_Free_Implementation (N)
      then
         Set_Uses_Lock_Free (Spec_Id);
      end if;
   end Analyze_Protected_Body;

   ----------------------------------
   -- Analyze_Protected_Definition --
   ----------------------------------

   procedure Analyze_Protected_Definition (N : Node_Id) is
      procedure Undelay_Itypes (T : Entity_Id);
      --  Itypes created for the private components of a protected type
      --  do not receive freeze nodes, because there is no scope in which
      --  they can be elaborated, and they can depend on discriminants of
      --  the enclosed protected type. Given that the components can be
      --  composite types with inner components, we traverse recursively
      --  the private components of the protected type, and indicate that
      --  all itypes within are frozen. This ensures that no freeze nodes
      --  will be generated for them. In the case of itypes that are access
      --  types we need to complete their representation by calling layout,
      --  which would otherwise be invoked when freezing a type.
      --
      --  On the other hand, components of the corresponding record are
      --  frozen (or receive itype references) as for other records.

      --------------------
      -- Undelay_Itypes --
      --------------------

      procedure Undelay_Itypes (T : Entity_Id) is
         Comp : Entity_Id;

      begin
         if Is_Protected_Type (T) then
            Comp := First_Private_Entity (T);
         elsif Is_Record_Type (T) then
            Comp := First_Entity (T);
         else
            return;
         end if;

         while Present (Comp) loop
            if Is_Type (Comp) and then Is_Itype (Comp) then
               Set_Has_Delayed_Freeze (Comp, False);
               Set_Is_Frozen (Comp);

               if Is_Access_Type (Comp) then
                  Layout_Type (Comp);
               end if;

               if Is_Record_Type (Comp) or else Is_Protected_Type (Comp) then
                  Undelay_Itypes (Comp);
               end if;
            end if;

            Next_Entity (Comp);
         end loop;
      end Undelay_Itypes;

      --  Local variables

      Prot_Typ : constant Entity_Id := Current_Scope;
      Item_Id  : Entity_Id;
      Last_Id  : Entity_Id;

   --  Start of processing for Analyze_Protected_Definition

   begin
      Tasking_Used := True;
      Analyze_Declarations (Visible_Declarations (N));

      if not Is_Empty_List (Private_Declarations (N)) then
         Last_Id := Last_Entity (Prot_Typ);
         Analyze_Declarations (Private_Declarations (N));

         if Present (Last_Id) then
            Set_First_Private_Entity (Prot_Typ, Next_Entity (Last_Id));
         else
            Set_First_Private_Entity (Prot_Typ, First_Entity (Prot_Typ));
         end if;
      end if;

      Item_Id := First_Entity (Prot_Typ);
      while Present (Item_Id) loop
         if Ekind (Item_Id) in E_Function | E_Procedure then
            Set_Convention (Item_Id, Convention_Protected);
         else
            Propagate_Concurrent_Flags (Prot_Typ, Etype (Item_Id));

            if Has_Controlled_Component (Etype (Item_Id))
              or else (Chars (Item_Id) /= Name_uParent
                        and then Is_Controlled (Etype (Item_Id)))
            then
               Set_Has_Controlled_Component (Prot_Typ);
            end if;
         end if;

         Next_Entity (Item_Id);
      end loop;

      Undelay_Itypes (Prot_Typ);

      Check_Max_Entries (N, Max_Protected_Entries);
      Process_End_Label (N, 'e', Prot_Typ);
   end Analyze_Protected_Definition;

   ----------------------------------------
   -- Analyze_Protected_Type_Declaration --
   ----------------------------------------

   procedure Analyze_Protected_Type_Declaration (N : Node_Id) is
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      E      : Entity_Id;
      T      : Entity_Id;

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("protected type", N);

         Analyze_Aspect_Specifications (N, Def_Id);

         return;
      end if;

      Tasking_Used := True;
      Check_Restriction (No_Protected_Types, N);

      T := Find_Type_Name (N);

      --  In the case of an incomplete type, use the full view, unless it's not
      --  present (as can occur for an incomplete view from a limited with).

      if Ekind (T) = E_Incomplete_Type and then Present (Full_View (T)) then
         T := Full_View (T);
         Set_Completion_Referenced (T);
      end if;

      Mutate_Ekind           (T, E_Protected_Type);
      Set_Is_Not_Self_Hidden (T);
      Set_Is_First_Subtype   (T);
      Reinit_Size_Align      (T);
      Set_Etype              (T, T);
      Set_Has_Delayed_Freeze (T);
      Set_Stored_Constraint  (T, No_Elist);

      --  Initialize type's primitive operations list, for possible use when
      --  the extension of prefixed call notation for untagged types is enabled
      --  (such as by use of -gnatX).

      Set_Direct_Primitive_Operations (T, New_Elmt_List);

      --  Mark this type as a protected type for the sake of restrictions,
      --  unless the protected type is declared in a private part of a package
      --  of the runtime. With this exception, the Suspension_Object from
      --  Ada.Synchronous_Task_Control can be implemented using a protected
      --  object without triggering violations of No_Local_Protected_Objects
      --  when the user locally declares such an object. This may look like a
      --  trick, but the user doesn't have to know how Suspension_Object is
      --  implemented.

      if In_Private_Part (Current_Scope)
        and then Is_Internal_Unit (Current_Sem_Unit)
      then
         Set_Has_Protected (T, False);
      else
         Set_Has_Protected (T);
      end if;

      --  Set the SPARK_Mode from the current context (may be overwritten later
      --  with an explicit pragma).

      Set_SPARK_Pragma               (T, SPARK_Mode_Pragma);
      Set_SPARK_Aux_Pragma           (T, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited     (T);
      Set_SPARK_Aux_Pragma_Inherited (T);

      Push_Scope (T);

      if Ada_Version >= Ada_2005 then
         Check_Interfaces (N, T);
      end if;

      if Present (Discriminant_Specifications (N)) then
         if Has_Discriminants (T) then

            --  Install discriminants. Also, verify conformance of
            --  discriminants of previous and current view. ???

            Install_Declarations (T);
         else
            Process_Discriminants (N);
         end if;
      end if;

      Set_Is_Constrained (T, not Has_Discriminants (T));

      --  If aspects are present, analyze them now. They can make references to
      --  the discriminants of the type, but not to any components.

      --  The protected type is the full view of a private type. Analyze the
      --  aspects with the entity of the private type to ensure that after
      --  both views are exchanged, the aspect are actually associated with
      --  the full view.

      if T /= Def_Id and then Is_Private_Type (Def_Id) then
         Analyze_Aspect_Specifications (N, T);
      else
         Analyze_Aspect_Specifications (N, Def_Id);
      end if;

      Analyze (Protected_Definition (N));

      --  In the case where the protected type is declared at a nested level
      --  and the No_Local_Protected_Objects restriction applies, issue a
      --  warning that objects of the type will violate the restriction.

      if Restriction_Check_Required (No_Local_Protected_Objects)
        and then not Is_Library_Level_Entity (T)
        and then Comes_From_Source (T)
      then
         Error_Msg_Sloc := Restrictions_Loc (No_Local_Protected_Objects);

         if Error_Msg_Sloc = No_Location then
            Error_Msg_N
              ("objects of this type will violate " &
               "`No_Local_Protected_Objects`??", N);
         else
            Error_Msg_N
              ("objects of this type will violate " &
               "`No_Local_Protected_Objects`#??", N);
         end if;
      end if;

      --  Protected types with entries are controlled (because of the
      --  Protection component if nothing else), same for any protected type
      --  with interrupt handlers. Note that we need to analyze the protected
      --  definition to set Has_Entries and such.

      if (Abort_Allowed or else Restriction_Active (No_Entry_Queue) = False
           or else Number_Entries (T) > 1)
        and then not Restricted_Profile
        and then
          (Has_Entries (T)
            or else Has_Interrupt_Handler (T)
            or else Has_Attach_Handler (T))
      then
         Set_Has_Controlled_Component (T);
      end if;

      --  The Ekind of components is E_Void during analysis for historical
      --  reasons. Now it can be set correctly.

      E := First_Entity (Current_Scope);
      while Present (E) loop
         if Ekind (E) = E_Void then
            if not Is_Itype (E) then
               Mutate_Ekind (E, E_Component);
               Reinit_Component_Location (E);
            end if;
         end if;

         Next_Entity (E);
      end loop;

      End_Scope;

      --  When a Lock_Free aspect forces the lock-free implementation, check N
      --  meets all the lock-free restrictions. Otherwise, an error message is
      --  issued by Allows_Lock_Free_Implementation.

      if Uses_Lock_Free (Defining_Identifier (N)) then

         --  Complain when there is an explicit aspect/pragma Priority (or
         --  Interrupt_Priority) while the lock-free implementation is forced
         --  by an aspect/pragma.

         declare
            Id : constant Entity_Id := Defining_Identifier (Original_Node (N));
            --  The warning must be issued on the original identifier in order
            --  to deal properly with the case of a single protected object.

            Prio_Item : constant Node_Id :=
                          Get_Rep_Item (Def_Id, Name_Priority, False);

         begin
            if Present (Prio_Item) then

               --  Aspect case

               if Nkind (Prio_Item) = N_Aspect_Specification
                 or else From_Aspect_Specification (Prio_Item)
               then
                  Error_Msg_Name_1 := Chars (Identifier (Prio_Item));
                  Error_Msg_NE
                    ("aspect% for & has no effect when Lock_Free given??",
                     Prio_Item, Id);

               --  Pragma case

               else
                  if Debug_Flag_Underscore_DD then
                     Record_Pragma_No_Effect_With_Lock_Free_Warning
                       (Pragma_Node     => Prio_Item,
                        Pragma_Name     => Pragma_Name (Prio_Item),
                        Lock_Free_Node  => Id,
                        Lock_Free_Range => Parent (Id));
                  else
                     Error_Msg_Name_1 := Pragma_Name (Prio_Item);
                     Error_Msg_NE
                       ("pragma% for & has no effect when Lock_Free given??",
                        Prio_Item, Id);
                  end if;
               end if;
            end if;
         end;

         if not Allows_Lock_Free_Implementation (N, Lock_Free_Given => True)
         then
            return;
         end if;
      end if;

      --  If the Attach_Handler aspect is specified or the Interrupt_Handler
      --  aspect is True, then the initial ceiling priority must be in the
      --  range of System.Interrupt_Priority. It is therefore recommanded
      --  to use the Interrupt_Priority aspect instead of the Priority aspect.

      if Has_Interrupt_Handler (T) or else Has_Attach_Handler (T) then
         declare
            Prio_Item : constant Node_Id :=
                          Get_Rep_Item (Def_Id, Name_Priority, False);

         begin
            if Present (Prio_Item) then

               --  Aspect case

               if (Nkind (Prio_Item) = N_Aspect_Specification
                    or else From_Aspect_Specification (Prio_Item))
                 and then Chars (Identifier (Prio_Item)) = Name_Priority
               then
                  Error_Msg_N
                    ("aspect Interrupt_Priority is preferred in presence of "
                     & "handlers??", Prio_Item);

               --  Pragma case

               elsif Nkind (Prio_Item) = N_Pragma
                 and then Pragma_Name (Prio_Item) = Name_Priority
               then
                  Error_Msg_N
                    ("pragma Interrupt_Priority is preferred in presence of "
                     & "handlers??", Prio_Item);
               end if;
            end if;
         end;
      end if;

      --  Case of a completion of a private declaration

      if T /= Def_Id and then Is_Private_Type (Def_Id) then

         --  Deal with preelaborable initialization. Note that this processing
         --  is done by Process_Full_View, but as can be seen below, in this
         --  case the call to Process_Full_View is skipped if any serious
         --  errors have occurred, and we don't want to lose this check.

         if Known_To_Have_Preelab_Init (Def_Id) then
            Set_Must_Have_Preelab_Init (T);
         end if;

         --  Propagate Default_Initial_Condition-related attributes from the
         --  private type to the protected type.

         Propagate_DIC_Attributes (T, From_Typ => Def_Id);

         --  Propagate invariant-related attributes from the private type to
         --  the protected type.

         Propagate_Invariant_Attributes (T, From_Typ => Def_Id);

         --  Propagate predicate-related attributes from the private type to
         --  the protected type.

         Propagate_Predicate_Attributes (T, From_Typ => Def_Id);

         --  Create corresponding record now, because some private dependents
         --  may be subtypes of the partial view.

         --  Skip if errors are present, to prevent cascaded messages

         if Serious_Errors_Detected = 0

           --  Also skip if expander is not active

           and then Expander_Active
         then
            Expand_N_Protected_Type_Declaration (N);
            Process_Full_View (N, T, Def_Id);
         end if;
      end if;

      --  In GNATprove mode, force the loading of a Interrupt_Priority, which
      --  is required for the ceiling priority protocol checks triggered by
      --  calls originating from protected subprograms and entries.

      if GNATprove_Mode then
         SPARK_Implicit_Load (RE_Interrupt_Priority);
      end if;
   end Analyze_Protected_Type_Declaration;

   ---------------------
   -- Analyze_Requeue --
   ---------------------

   procedure Analyze_Requeue (N : Node_Id) is

      procedure Check_Wrong_Attribute_In_Postconditions
        (Entry_Id   : Entity_Id;
         Error_Node : Node_Id);
      --  Check that the requeue target Entry_Id does not have an specific or
      --  class-wide postcondition that references an Old or Index attribute.

      ---------------------------------------------
      -- Check_Wrong_Attribute_In_Postconditions --
      ---------------------------------------------

      procedure Check_Wrong_Attribute_In_Postconditions
        (Entry_Id   : Entity_Id;
         Error_Node : Node_Id)
      is
         function Check_Node (N : Node_Id) return Traverse_Result;
         --  Check that N is not a reference to attribute Index or Old; report
         --  an error otherwise.

         ----------------
         -- Check_Node --
         ----------------

         function Check_Node (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Attribute_Reference
              and then Attribute_Name (N) in Name_Index
                                           | Name_Old
            then
               Error_Msg_Name_1 := Attribute_Name (N);
               Error_Msg_N
                 ("target of requeue must not have references to attribute % "
                  & "in postcondition",
                  Error_Node);
            end if;

            return OK;
         end Check_Node;

         procedure Check_Attr_Refs is new Traverse_Proc (Check_Node);

         --  Local variables

         Prag : Node_Id;
      begin
         Prag := Pre_Post_Conditions (Contract (Entry_Id));

         while Present (Prag) loop
            if Pragma_Name (Prag) = Name_Postcondition then
               Check_Attr_Refs (First (Pragma_Argument_Associations (Prag)));
            end if;

            Prag := Next_Pragma (Prag);
         end loop;
      end Check_Wrong_Attribute_In_Postconditions;

      --  Local variables

      Count       : Natural := 0;
      Entry_Name  : Node_Id := Name (N);
      Entry_Id    : Entity_Id;
      I           : Interp_Index;
      Is_Disp_Req : Boolean;
      It          : Interp;
      Enclosing   : Entity_Id;
      Target_Obj  : Node_Id := Empty;
      Req_Scope   : Entity_Id;
      Outer_Ent   : Entity_Id;
      Synch_Type  : Entity_Id := Empty;

   --  Start of processing for Analyze_Requeue

   begin
      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => N,
         Checks   => True,
         Modes    => True,
         Warnings => True);

      Tasking_Used := True;
      Check_Restriction (No_Requeue_Statements, N);
      Check_Unreachable_Code (N);

      Enclosing := Empty;
      for J in reverse 0 .. Scope_Stack.Last loop
         Enclosing := Scope_Stack.Table (J).Entity;
         exit when Is_Entry (Enclosing);

         if Ekind (Enclosing) not in E_Block | E_Loop then
            Error_Msg_N ("requeue must appear within accept or entry body", N);
            return;
         end if;
      end loop;

      Analyze (Entry_Name);

      if Etype (Entry_Name) = Any_Type then
         return;
      end if;

      if Nkind (Entry_Name) = N_Selected_Component then
         Target_Obj := Prefix (Entry_Name);
         Entry_Name := Selector_Name (Entry_Name);
      end if;

      --  Ada 2012 (9.5.4(5.6/4): "If the target is a procedure, the name
      --  shall denote a renaming of an entry or ...". We support this
      --  language rule replacing the target procedure with the renamed
      --  entry. Thus, reanalyzing the resulting requeue statement we
      --  reuse all the Ada 2005 machinery to perform the analysis.

      if Nkind (Entry_Name) in N_Has_Entity then
         declare
            Target_E : constant Entity_Id := Entity (Entry_Name);

         begin
            if Ada_Version >= Ada_2012
              and then Ekind (Target_E) = E_Procedure
              and then Convention (Target_E) = Convention_Entry
              and then Nkind (Original_Node (Parent (Parent (Target_E))))
                         = N_Subprogram_Renaming_Declaration
            then
               Set_Name (N,
                 New_Copy_Tree
                   (Name (Original_Node (Parent (Parent (Target_E))))));
               Analyze_Requeue (N);
               return;
            end if;
         end;
      end if;

      --  If an explicit target object is given then we have to check the
      --  restrictions of 9.5.4(6).

      if Present (Target_Obj) then

         --  Locate containing concurrent unit and determine enclosing entry
         --  body or outermost enclosing accept statement within the unit.

         Outer_Ent := Empty;
         for S in reverse 0 .. Scope_Stack.Last loop
            Req_Scope := Scope_Stack.Table (S).Entity;

            exit when Is_Concurrent_Type (Req_Scope);

            if Is_Entry (Req_Scope) then
               Outer_Ent := Req_Scope;
            end if;
         end loop;

         pragma Assert (Present (Outer_Ent));

         --  Check that the accessibility level of the target object is not
         --  greater or equal to the outermost enclosing accept statement (or
         --  entry body) unless it is a parameter of the innermost enclosing
         --  accept statement (or entry body).

         if Static_Accessibility_Level (Target_Obj, Zero_On_Dynamic_Level)
              >= Scope_Depth (Outer_Ent)
           and then
             (not Is_Entity_Name (Target_Obj)
               or else not Is_Formal (Entity (Target_Obj))
               or else Enclosing /= Scope (Entity (Target_Obj)))
         then
            Error_Msg_N
              ("target object has invalid level for requeue", Target_Obj);
         end if;
      end if;

      --  Overloaded case, find right interpretation

      if Is_Overloaded (Entry_Name) then
         Entry_Id := Empty;

         --  Loop over candidate interpretations and filter out any that are
         --  not parameterless, are not type conformant, are not entries, or
         --  do not come from source.

         Get_First_Interp (Entry_Name, I, It);
         while Present (It.Nam) loop

            --  Note: we test type conformance here, not subtype conformance.
            --  Subtype conformance will be tested later on, but it is better
            --  for error output in some cases not to do that here.

            if (No (First_Formal (It.Nam))
                 or else Type_Conformant (Enclosing, It.Nam))
              and then Ekind (It.Nam) = E_Entry
            then
               --  Ada 2005 (AI-345): Since protected and task types have
               --  primitive entry wrappers, we only consider source entries.

               if Comes_From_Source (It.Nam) then
                  Count := Count + 1;
                  Entry_Id := It.Nam;
               else
                  Remove_Interp (I);
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Count = 0 then
            Error_Msg_N ("no entry matches context", N);
            return;

         elsif Count > 1 then
            Error_Msg_N ("ambiguous entry name in requeue", N);
            return;

         else
            Set_Is_Overloaded (Entry_Name, False);
            Set_Entity (Entry_Name, Entry_Id);
         end if;

      --  Non-overloaded cases

      --  For the case of a reference to an element of an entry family, the
      --  Entry_Name is an indexed component.

      elsif Nkind (Entry_Name) = N_Indexed_Component then

         --  Requeue to an entry out of the body

         if Nkind (Prefix (Entry_Name)) = N_Selected_Component then
            Entry_Id := Entity (Selector_Name (Prefix (Entry_Name)));

         --  Requeue from within the body itself

         elsif Nkind (Prefix (Entry_Name)) = N_Identifier then
            Entry_Id := Entity (Prefix (Entry_Name));

         else
            Error_Msg_N ("invalid entry_name specified",  N);
            return;
         end if;

      --  If we had a requeue of the form REQUEUE A (B), then the parser
      --  accepted it (because it could have been a requeue on an entry index.
      --  If A turns out not to be an entry family, then the analysis of A (B)
      --  turned it into a function call.

      elsif Nkind (Entry_Name) = N_Function_Call then
         Error_Msg_N
           ("arguments not allowed in requeue statement",
            First (Parameter_Associations (Entry_Name)));
         return;

      --  Normal case of no entry family, no argument

      else
         Entry_Id := Entity (Entry_Name);
      end if;

      --  Ada 2012 (AI05-0030): Potential dispatching requeue statement. The
      --  target type must be a concurrent interface class-wide type and the
      --  target must be a procedure, flagged by pragma Implemented. The
      --  target may be an access to class-wide type, in which case it must
      --  be dereferenced.

      if Present (Target_Obj) then
         Synch_Type := Etype (Target_Obj);

         if Is_Access_Type (Synch_Type) then
            Synch_Type := Designated_Type (Synch_Type);
         end if;
      end if;

      Is_Disp_Req :=
        Ada_Version >= Ada_2012
          and then Present (Target_Obj)
          and then Is_Class_Wide_Type (Synch_Type)
          and then Is_Concurrent_Interface (Synch_Type)
          and then Ekind (Entry_Id) = E_Procedure
          and then Has_Rep_Pragma (Entry_Id, Name_Implemented);

      --  Resolve entry, and check that it is subtype conformant with the
      --  enclosing construct if this construct has formals (RM 9.5.4(5)).
      --  Ada 2005 (AI05-0030): Do not emit an error for this specific case.

      if not Is_Entry (Entry_Id)
        and then not Is_Disp_Req
      then
         Error_Msg_N ("expect entry name in requeue statement", Name (N));

      elsif Ekind (Entry_Id) = E_Entry_Family
        and then Nkind (Entry_Name) /= N_Indexed_Component
      then
         Error_Msg_N ("missing index for entry family component", Name (N));

      else
         Resolve_Entry (Name (N));
         Generate_Reference (Entry_Id, Entry_Name);

         if Present (First_Formal (Entry_Id)) then

            --  Ada 2012 (AI05-0030): Perform type conformance after skipping
            --  the first parameter of Entry_Id since it is the interface
            --  controlling formal.

            if Ada_Version >= Ada_2012 and then Is_Disp_Req then
               declare
                  Enclosing_Formal : Entity_Id;
                  Target_Formal    : Entity_Id;

               begin
                  Enclosing_Formal := First_Formal (Enclosing);
                  Target_Formal := Next_Formal (First_Formal (Entry_Id));
                  while Present (Enclosing_Formal)
                    and then Present (Target_Formal)
                  loop
                     if not Conforming_Types
                              (T1    => Etype (Enclosing_Formal),
                               T2    => Etype (Target_Formal),
                               Ctype => Subtype_Conformant)
                     then
                        Error_Msg_Node_2 := Target_Formal;
                        Error_Msg_NE
                          ("formal & is not subtype conformant with &" &
                           "in dispatching requeue", N, Enclosing_Formal);
                     end if;

                     Next_Formal (Enclosing_Formal);
                     Next_Formal (Target_Formal);
                  end loop;
               end;
            else
               Check_Subtype_Conformant (Enclosing, Entry_Id, Name (N));
            end if;

            --  Processing for parameters accessed by the requeue

            declare
               Ent : Entity_Id;

            begin
               Ent := First_Formal (Enclosing);
               while Present (Ent) loop

                  --  For OUT or IN OUT parameter, the effect of the requeue is
                  --  to assign the parameter a value on exit from the requeued
                  --  body, so we can set it as source assigned. We also clear
                  --  the Is_True_Constant indication. We do not need to clear
                  --  Current_Value, since the effect of the requeue is to
                  --  perform an unconditional goto so that any further
                  --  references will not occur anyway.

                  if Ekind (Ent) in E_Out_Parameter | E_In_Out_Parameter then
                     Set_Never_Set_In_Source (Ent, False);
                     Set_Is_True_Constant    (Ent, False);
                  end if;

                  --  For all parameters, the requeue acts as a reference,
                  --  since the value of the parameter is passed to the new
                  --  entry, so we want to suppress unreferenced warnings.

                  Set_Referenced (Ent);
                  Next_Formal (Ent);
               end loop;
            end;
         end if;
      end if;

      --  AI05-0225: the target protected object of a requeue must be a
      --  variable. This is a binding interpretation that applies to all
      --  versions of the language. Note that the subprogram does not have
      --  to be a protected operation: it can be an primitive implemented
      --  by entry with a formal that is a protected interface.

      if Present (Target_Obj)
        and then not Is_Variable (Target_Obj)
      then
         Error_Msg_N
           ("target protected object of requeue must be a variable", N);
      end if;

      --  Ada 2022 (AI12-0143): The requeue target shall not have an
      --  applicable specific or class-wide postcondition which includes
      --  an Old or Index attribute reference.

      if Ekind (Entry_Id) = E_Entry_Family
        and then Present (Contract (Entry_Id))
      then
         Check_Wrong_Attribute_In_Postconditions
           (Entry_Id   => Entry_Id,
            Error_Node => Entry_Name);
      end if;

      --  A requeue statement is treated as a call for purposes of ABE checks
      --  and diagnostics. Annotate the tree by creating a call marker in case
      --  the requeue statement is transformed by expansion.

      Build_Call_Marker (N);
   end Analyze_Requeue;

   ------------------------------
   -- Analyze_Selective_Accept --
   ------------------------------

   procedure Analyze_Selective_Accept (N : Node_Id) is
      Alts : constant List_Id := Select_Alternatives (N);
      Alt  : Node_Id;

      Accept_Present    : Boolean := False;
      Terminate_Present : Boolean := False;
      Delay_Present     : Boolean := False;
      Relative_Present  : Boolean := False;
      Alt_Count         : Uint    := Uint_0;

   begin
      Tasking_Used := True;
      Check_Restriction (No_Select_Statements, N);

      --  Loop to analyze alternatives

      Alt := First (Alts);
      while Present (Alt) loop
         Alt_Count := Alt_Count + 1;
         Analyze (Alt);

         if Nkind (Alt) = N_Delay_Alternative then
            if Delay_Present then

               if Relative_Present /=
                   (Nkind (Delay_Statement (Alt)) = N_Delay_Relative_Statement)
               then
                  Error_Msg_N
                    ("delay_until and delay_relative alternatives", Alt);
                  Error_Msg_N
                    ("\cannot appear in the same selective_wait", Alt);
               end if;

            else
               Delay_Present := True;
               Relative_Present :=
                 Nkind (Delay_Statement (Alt)) = N_Delay_Relative_Statement;
            end if;

         elsif Nkind (Alt) = N_Terminate_Alternative then
            if Terminate_Present then
               Error_Msg_N ("only one terminate alternative allowed", N);
            else
               Terminate_Present := True;
               Check_Restriction (No_Terminate_Alternatives, N);
            end if;

         elsif Nkind (Alt) = N_Accept_Alternative then
            Accept_Present := True;

            --  Check for duplicate accept

            declare
               Alt1 : Node_Id;
               Stm  : constant Node_Id := Accept_Statement (Alt);
               EDN  : constant Node_Id := Entry_Direct_Name (Stm);
               Ent  : Entity_Id;

            begin
               if Nkind (EDN) = N_Identifier
                 and then No (Condition (Alt))
                 and then Present (Entity (EDN)) -- defend against junk
                 and then Ekind (Entity (EDN)) = E_Entry
               then
                  Ent := Entity (EDN);

                  Alt1 := First (Alts);
                  while Alt1 /= Alt loop
                     if Nkind (Alt1) = N_Accept_Alternative
                       and then No (Condition (Alt1))
                     then
                        declare
                           Stm1 : constant Node_Id := Accept_Statement (Alt1);
                           EDN1 : constant Node_Id := Entry_Direct_Name (Stm1);

                        begin
                           if Nkind (EDN1) = N_Identifier then
                              if Entity (EDN1) = Ent then
                                 Error_Msg_Sloc := Sloc (Stm1);
                                 Error_Msg_N
                                   ("ACCEPT duplicates one on line#??", Stm);
                                 exit;
                              end if;
                           end if;
                        end;
                     end if;

                     Next (Alt1);
                  end loop;
               end if;
            end;
         end if;

         Next (Alt);
      end loop;

      Check_Restriction (Max_Select_Alternatives, N, Alt_Count);
      Check_Potentially_Blocking_Operation (N);

      if Terminate_Present and Delay_Present then
         Error_Msg_N ("at most one of TERMINATE or DELAY alternative", N);

      elsif not Accept_Present then
         Error_Msg_N
           ("SELECT must contain at least one ACCEPT alternative", N);
      end if;

      if Present (Else_Statements (N)) then
         if Terminate_Present or Delay_Present then
            Error_Msg_N ("ELSE part not allowed with other alternatives", N);
         end if;

         Analyze_Statements (Else_Statements (N));
      end if;
   end Analyze_Selective_Accept;

   ------------------------------------------
   -- Analyze_Single_Protected_Declaration --
   ------------------------------------------

   procedure Analyze_Single_Protected_Declaration (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Obj_Id   : constant Node_Id    := Defining_Identifier (N);
      Obj_Decl : Node_Id;
      Typ      : Entity_Id;

   begin
      Generate_Definition (Obj_Id);
      Tasking_Used := True;

      --  A single protected declaration is transformed into a pair of an
      --  anonymous protected type and an object of that type. Generate:

      --    protected type Typ is ...;

      Typ :=
        Make_Defining_Identifier (Sloc (Obj_Id),
          Chars => New_External_Name (Chars (Obj_Id), 'T'));

      Rewrite (N,
        Make_Protected_Type_Declaration (Loc,
         Defining_Identifier => Typ,
         Protected_Definition => Relocate_Node (Protected_Definition (N)),
         Interface_List       => Interface_List (N)));

      --  Use the original defining identifier of the single protected
      --  declaration in the generated object declaration to allow for debug
      --  information to be attached to it when compiling with -gnatD. The
      --  parent of the entity is the new object declaration. The single
      --  protected declaration is not used in semantics or code generation,
      --  but is scanned when generating debug information, and therefore needs
      --  the updated Sloc information from the entity (see Sprint). Generate:

      --    Obj : Typ;

      --  Keep the aspects from the original node

      Move_Aspects (Original_Node (N), N);

      Obj_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Obj_Id,
          Object_Definition   => New_Occurrence_Of (Typ, Loc));

      Insert_After (N, Obj_Decl);
      Mark_Rewrite_Insertion (Obj_Decl);

      --  Relocate aspect Part_Of from the original single protected
      --  declaration to the anonymous object declaration. This emulates the
      --  placement of an equivalent source pragma.

      Move_Or_Merge_Aspects (N, To => Obj_Decl);

      --  Relocate pragma Part_Of from the visible declarations of the original
      --  single protected declaration to the anonymous object declaration. The
      --  new placement better reflects the role of the pragma.

      Relocate_Pragmas_To_Anonymous_Object (N, Obj_Decl);

      --  Enter the names of the anonymous protected type and the object before
      --  analysis takes places, because the name of the object may be used in
      --  its own body.

      Enter_Name (Typ);
      Mutate_Ekind         (Typ, E_Protected_Type);
      Set_Etype            (Typ, Typ);
      Set_Anonymous_Object (Typ, Obj_Id);

      Enter_Name (Obj_Id);
      Mutate_Ekind               (Obj_Id, E_Variable);
      Set_Is_Not_Self_Hidden     (Obj_Id);
      Set_Etype                  (Obj_Id, Typ);
      Set_SPARK_Pragma           (Obj_Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (Obj_Id);

      --  Instead of calling Analyze on the new node, call the proper analysis
      --  procedure directly. Otherwise the node would be expanded twice, with
      --  disastrous result.

      Analyze_Protected_Type_Declaration (N);

      Analyze_Aspect_Specifications (N, Obj_Id);
   end Analyze_Single_Protected_Declaration;

   -------------------------------------
   -- Analyze_Single_Task_Declaration --
   -------------------------------------

   procedure Analyze_Single_Task_Declaration (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Obj_Id   : constant Node_Id    := Defining_Identifier (N);
      Obj_Decl : Node_Id;
      Typ      : Entity_Id;

   begin
      Generate_Definition (Obj_Id);
      Tasking_Used := True;

      --  A single task declaration is transformed into a pair of an anonymous
      --  task type and an object of that type. Generate:

      --    task type Typ is ...;

      Typ :=
        Make_Defining_Identifier (Sloc (Obj_Id),
          Chars => New_External_Name (Chars (Obj_Id), Suffix => "TK"));

      Rewrite (N,
        Make_Task_Type_Declaration (Loc,
          Defining_Identifier => Typ,
          Task_Definition     => Relocate_Node (Task_Definition (N)),
          Interface_List      => Interface_List (N)));

      --  Use the original defining identifier of the single task declaration
      --  in the generated object declaration to allow for debug information
      --  to be attached to it when compiling with -gnatD. The parent of the
      --  entity is the new object declaration. The single task declaration
      --  is not used in semantics or code generation, but is scanned when
      --  generating debug information, and therefore needs the updated Sloc
      --  information from the entity (see Sprint). Generate:

      --    Obj : Typ;

      --  Keep the aspects from the original node

      Move_Aspects (Original_Node (N), N);

      Obj_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Obj_Id,
          Object_Definition   => New_Occurrence_Of (Typ, Loc));

      Insert_After (N, Obj_Decl);
      Mark_Rewrite_Insertion (Obj_Decl);

      --  Relocate aspects Depends, Global and Part_Of from the original single
      --  task declaration to the anonymous object declaration. This emulates
      --  the placement of an equivalent source pragma.

      Move_Or_Merge_Aspects (N, To => Obj_Decl);

      --  Relocate pragmas Depends, Global and Part_Of from the visible
      --  declarations of the original single protected declaration to the
      --  anonymous object declaration. The new placement better reflects the
      --  role of the pragmas.

      Relocate_Pragmas_To_Anonymous_Object (N, Obj_Decl);

      --  Enter the names of the anonymous task type and the object before
      --  analysis takes places, because the name of the object may be used
      --  in its own body.

      Enter_Name (Typ);
      Mutate_Ekind         (Typ, E_Task_Type);
      Set_Etype            (Typ, Typ);
      Set_Anonymous_Object (Typ, Obj_Id);

      Enter_Name (Obj_Id);
      Mutate_Ekind               (Obj_Id, E_Variable);
      Set_Is_Not_Self_Hidden     (Obj_Id);
      Set_Etype                  (Obj_Id, Typ);
      Set_SPARK_Pragma           (Obj_Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (Obj_Id);

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => Obj_Id,
         Checks   => True,
         Warnings => True);

      --  Instead of calling Analyze on the new node, call the proper analysis
      --  procedure directly. Otherwise the node would be expanded twice, with
      --  disastrous result.

      Analyze_Task_Type_Declaration (N);

      Analyze_Aspect_Specifications (N, Obj_Id);
   end Analyze_Single_Task_Declaration;

   -----------------------
   -- Analyze_Task_Body --
   -----------------------

   procedure Analyze_Task_Body (N : Node_Id) is
      Body_Id : constant Entity_Id := Defining_Identifier (N);
      Decls   : constant List_Id   := Declarations (N);
      HSS     : constant Node_Id   := Handled_Statement_Sequence (N);
      Last_E  : Entity_Id;

      Spec_Id : Entity_Id;
      --  This is initially the entity of the task or task type involved, but
      --  is replaced by the task type always in the case of a single task
      --  declaration, since this is the proper scope to be used.

      Ref_Id : Entity_Id;
      --  This is the entity of the task or task type, and is the entity used
      --  for cross-reference purposes (it differs from Spec_Id in the case of
      --  a single task, since Spec_Id is set to the task type).

   begin
      --  A task body freezes the contract of the nearest enclosing package
      --  body and all other contracts encountered in the same declarative part
      --  up to and excluding the task body. This ensures that annotations
      --  referenced by the contract of an entry or subprogram body declared
      --  within the current protected body are available.

      Freeze_Previous_Contracts (N);

      Tasking_Used := True;
      Set_Scope (Body_Id, Current_Scope);
      Mutate_Ekind (Body_Id, E_Task_Body);
      Set_Etype (Body_Id, Standard_Void_Type);
      Spec_Id := Find_Concurrent_Spec (Body_Id);

      --  The spec is either a task type declaration, or a single task
      --  declaration for which we have created an anonymous type.

      if Present (Spec_Id) and then Ekind (Spec_Id) = E_Task_Type then
         null;

      elsif Present (Spec_Id)
        and then Ekind (Etype (Spec_Id)) = E_Task_Type
        and then not Comes_From_Source (Etype (Spec_Id))
      then
         null;

      else
         Error_Msg_N ("missing specification for task body", Body_Id);
         return;
      end if;

      if Has_Completion (Spec_Id)
        and then Present (Corresponding_Body (Parent (Spec_Id)))
      then
         if Nkind (Parent (Spec_Id)) = N_Task_Type_Declaration then
            Error_Msg_NE ("duplicate body for task type&", N, Spec_Id);
         else
            Error_Msg_NE ("duplicate body for task&", N, Spec_Id);
         end if;
      end if;

      Ref_Id := Spec_Id;
      Generate_Reference (Ref_Id, Body_Id, 'b', Set_Ref => False);
      Style.Check_Identifier (Body_Id, Spec_Id);

      --  Deal with case of body of single task (anonymous type was created)

      if Ekind (Spec_Id) = E_Variable then
         Spec_Id := Etype (Spec_Id);
      end if;

      --  Set the SPARK_Mode from the current context (may be overwritten later
      --  with an explicit pragma).

      Set_SPARK_Pragma           (Body_Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (Body_Id);

      Analyze_Aspect_Specifications (N, Body_Id);

      Push_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Parent (Spec_Id), Body_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);
      Last_E := Last_Entity (Spec_Id);

      Analyze_Declarations (Decls);
      Inspect_Deferred_Constant_Completion (Decls);

      --  For visibility purposes, all entities in the body are private. Set
      --  First_Private_Entity accordingly, if there was no private part in the
      --  protected declaration.

      if No (First_Private_Entity (Spec_Id)) then
         if Present (Last_E) then
            Set_First_Private_Entity (Spec_Id, Next_Entity (Last_E));
         else
            Set_First_Private_Entity (Spec_Id, First_Entity (Spec_Id));
         end if;

         --  The entity list of the current scope now includes entities in
         --  the spec as well as the body. Their declarations will become
         --  part of the statement sequence of the task body procedure that
         --  is built during expansion. Indicate that aspect specifications
         --  for these entities need not be rechecked. The guards on
         --  Check_Aspect_At_End_Of_Declarations are not sufficient to
         --  suppress these checks, because the declarations come from source.

         declare
            Priv : Entity_Id := First_Private_Entity (Spec_Id);

         begin
            while Present (Priv) loop
               Set_Has_Delayed_Aspects (Priv, False);
               Next_Entity (Priv);
            end loop;
         end;
      end if;

      --  Mark all handlers as not suitable for local raise optimization,
      --  since this optimization causes difficulties in a task context.

      if Present (Exception_Handlers (HSS)) then
         declare
            Handlr : Node_Id;
         begin
            Handlr := First (Exception_Handlers (HSS));
            while Present (Handlr) loop
               Set_Local_Raise_Not_OK (Handlr);
               Next (Handlr);
            end loop;
         end;
      end if;

      --  Now go ahead and complete analysis of the task body

      Analyze (HSS);
      Check_Completion (Body_Id);
      Check_References (Body_Id);
      Check_References (Spec_Id);

      --  Check for entries with no corresponding accept

      declare
         Ent : Entity_Id;

      begin
         Ent := First_Entity (Spec_Id);
         while Present (Ent) loop
            if Is_Entry (Ent)
              and then not Entry_Accepted (Ent)
              and then Comes_From_Source (Ent)
            then
               Error_Msg_NE ("no accept for entry &??", N, Ent);
            end if;

            Next_Entity (Ent);
         end loop;
      end;

      Process_End_Label (HSS, 't', Ref_Id);
      Update_Use_Clause_Chain;
      End_Scope;
   end Analyze_Task_Body;

   -----------------------------
   -- Analyze_Task_Definition --
   -----------------------------

   procedure Analyze_Task_Definition (N : Node_Id) is
      L : Entity_Id;

   begin
      Tasking_Used := True;

      if Present (Visible_Declarations (N)) then
         Analyze_Declarations (Visible_Declarations (N));
      end if;

      if Present (Private_Declarations (N)) then
         L := Last_Entity (Current_Scope);
         Analyze_Declarations (Private_Declarations (N));

         if Present (L) then
            Set_First_Private_Entity
              (Current_Scope, Next_Entity (L));
         else
            Set_First_Private_Entity
              (Current_Scope, First_Entity (Current_Scope));
         end if;
      end if;

      Check_Max_Entries (N, Max_Task_Entries);
      Process_End_Label (N, 'e', Current_Scope);
   end Analyze_Task_Definition;

   -----------------------------------
   -- Analyze_Task_Type_Declaration --
   -----------------------------------

   procedure Analyze_Task_Type_Declaration (N : Node_Id) is
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      T      : Entity_Id;

   begin
      --  Attempt to use tasking in no run time mode is not allowe. Issue hard
      --  error message to disable expansion which leads to crashes.

      if Opt.No_Run_Time_Mode then
         Error_Msg_N ("tasking not allowed in No_Run_Time mode", N);

      --  Otherwise soft check for no tasking restriction

      else
         Check_Restriction (No_Tasking, N);
      end if;

      --  Proceed ahead with analysis of task type declaration

      Tasking_Used := True;

      --  The sequential partition elaboration policy is supported only in the
      --  restricted profile.

      if Partition_Elaboration_Policy = 'S'
        and then not Restricted_Profile
      then
         Error_Msg_N
           ("sequential elaboration supported only in restricted profile", N);
      end if;

      T := Find_Type_Name (N);
      Generate_Definition (T);

      --  In the case of an incomplete type, use the full view, unless it's not
      --  present (as can occur for an incomplete view from a limited with).
      --  Initialize the Corresponding_Record_Type (which overlays the Private
      --  Dependents field of the incomplete view).

      if Ekind (T) = E_Incomplete_Type then
         if Present (Full_View (T)) then
            T := Full_View (T);
            Set_Completion_Referenced (T);

         else
            Mutate_Ekind (T, E_Task_Type);
            Set_Corresponding_Record_Type (T, Empty);
         end if;
      end if;

      Mutate_Ekind           (T, E_Task_Type);
      Set_Is_Not_Self_Hidden (T);
      Set_Is_First_Subtype   (T, True);
      Set_Has_Task           (T, True);
      Reinit_Size_Align      (T);
      Set_Etype              (T, T);
      Set_Has_Delayed_Freeze (T, True);
      Set_Stored_Constraint  (T, No_Elist);

      --  Initialize type's primitive operations list, for possible use when
      --  the extension of prefixed call notation for untagged types is enabled
      --  (such as by use of -gnatX).

      Set_Direct_Primitive_Operations (T, New_Elmt_List);

      --  Set the SPARK_Mode from the current context (may be overwritten later
      --  with an explicit pragma).

      Set_SPARK_Pragma               (T, SPARK_Mode_Pragma);
      Set_SPARK_Aux_Pragma           (T, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited     (T);
      Set_SPARK_Aux_Pragma_Inherited (T);

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => T,
         Checks   => True,
         Warnings => True);

      Push_Scope (T);

      if Ada_Version >= Ada_2005 then
         Check_Interfaces (N, T);
      end if;

      if Present (Discriminant_Specifications (N)) then
         if Ada_Version = Ada_83 and then Comes_From_Source (N) then
            Error_Msg_N ("(Ada 83) task discriminant not allowed!", N);
         end if;

         if Has_Discriminants (T) then

            --  Install discriminants. Also, verify conformance of
            --  discriminants of previous and current view. ???

            Install_Declarations (T);
         else
            Process_Discriminants (N);
         end if;
      end if;

      Set_Is_Constrained (T, not Has_Discriminants (T));

      --  The task type is the full view of a private type. Analyze the
      --  aspects with the entity of the private type to ensure that after
      --  both views are exchanged, the aspect are actually associated with
      --  the full view.

      if T /= Def_Id and then Is_Private_Type (Def_Id) then
         Analyze_Aspect_Specifications (N, T);
      else
         Analyze_Aspect_Specifications (N, Def_Id);
      end if;

      if Present (Task_Definition (N)) then
         Analyze_Task_Definition (Task_Definition (N));
      end if;

      --  In the case where the task type is declared at a nested level and the
      --  No_Task_Hierarchy restriction applies, issue a warning that objects
      --  of the type will violate the restriction.

      if Restriction_Check_Required (No_Task_Hierarchy)
        and then not Is_Library_Level_Entity (T)
        and then Comes_From_Source (T)
        and then not CodePeer_Mode
      then
         Error_Msg_Sloc := Restrictions_Loc (No_Task_Hierarchy);

         if Error_Msg_Sloc = No_Location then
            Error_Msg_N
              ("objects of this type will violate `No_Task_Hierarchy`??", N);
         else
            Error_Msg_N
              ("objects of this type will violate `No_Task_Hierarchy`#??", N);
         end if;
      end if;

      End_Scope;

      --  Case of a completion of a private declaration

      if T /= Def_Id and then Is_Private_Type (Def_Id) then

         --  Deal with preelaborable initialization. Note that this processing
         --  is done by Process_Full_View, but as can be seen below, in this
         --  case the call to Process_Full_View is skipped if any serious
         --  errors have occurred, and we don't want to lose this check.

         if Known_To_Have_Preelab_Init (Def_Id) then
            Set_Must_Have_Preelab_Init (T);
         end if;

         --  Propagate Default_Initial_Condition-related attributes from the
         --  private type to the task type.

         Propagate_DIC_Attributes (T, From_Typ => Def_Id);

         --  Propagate invariant-related attributes from the private type to
         --  task type.

         Propagate_Invariant_Attributes (T, From_Typ => Def_Id);

         --  Propagate predicate-related attributes from the private type to
         --  task type.

         Propagate_Predicate_Attributes (T, From_Typ => Def_Id);

         --  Create corresponding record now, because some private dependents
         --  may be subtypes of the partial view.

         --  Skip if errors are present, to prevent cascaded messages

         if Serious_Errors_Detected = 0

           --  Also skip if expander is not active

           and then Expander_Active
         then
            Expand_N_Task_Type_Declaration (N);
            Process_Full_View (N, T, Def_Id);
         end if;
      end if;

      --  In GNATprove mode, force the loading of a Interrupt_Priority, which
      --  is required for the ceiling priority protocol checks triggered by
      --  calls originating from tasks.

      if GNATprove_Mode then
         SPARK_Implicit_Load (RE_Interrupt_Priority);
      end if;
   end Analyze_Task_Type_Declaration;

   -----------------------------------
   -- Analyze_Terminate_Alternative --
   -----------------------------------

   procedure Analyze_Terminate_Alternative (N : Node_Id) is
   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Any_Boolean);
      end if;
   end Analyze_Terminate_Alternative;

   ------------------------------
   -- Analyze_Timed_Entry_Call --
   ------------------------------

   procedure Analyze_Timed_Entry_Call (N : Node_Id) is
      Trigger        : constant Node_Id :=
                         Entry_Call_Statement (Entry_Call_Alternative (N));
      Is_Disp_Select : Boolean := False;

   begin
      Tasking_Used := True;
      Check_Restriction (No_Select_Statements, N);

      --  Ada 2005 (AI-345): The trigger may be a dispatching call

      if Ada_Version >= Ada_2005 then
         Analyze (Trigger);
         Check_Triggering_Statement (Trigger, N, Is_Disp_Select);
      end if;

      --  Postpone the analysis of the statements till expansion. Analyze only
      --  if the expander is disabled in order to catch any semantic errors.

      if Is_Disp_Select then
         if not Expander_Active then
            Analyze (Entry_Call_Alternative (N));
            Analyze (Delay_Alternative (N));
         end if;

      --  Regular select analysis

      else
         Analyze (Entry_Call_Alternative (N));
         Analyze (Delay_Alternative (N));
      end if;
   end Analyze_Timed_Entry_Call;

   ------------------------------------
   -- Analyze_Triggering_Alternative --
   ------------------------------------

   procedure Analyze_Triggering_Alternative (N : Node_Id) is
      Trigger : constant Node_Id := Triggering_Statement (N);

   begin
      Tasking_Used := True;

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      Analyze (Trigger);

      if Comes_From_Source (Trigger)
        and then Nkind (Trigger) not in N_Delay_Statement
        and then Nkind (Trigger) /= N_Entry_Call_Statement
      then
         if Ada_Version < Ada_2005 then
            Error_Msg_N
             ("triggering statement must be delay or entry call", Trigger);

         --  Ada 2005 (AI-345): If a procedure_call_statement is used for a
         --  procedure_or_entry_call, the procedure_name or procedure_prefix
         --  of the procedure_call_statement shall denote an entry renamed by a
         --  procedure, or (a view of) a primitive subprogram of a limited
         --  interface whose first parameter is a controlling parameter.

         elsif Nkind (Trigger) = N_Procedure_Call_Statement
           and then not Is_Renamed_Entry (Entity (Name (Trigger)))
           and then not Is_Controlling_Limited_Procedure
                          (Entity (Name (Trigger)))
         then
            Error_Msg_N
              ("triggering statement must be procedure or entry call " &
               "or delay statement", Trigger);
         end if;
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Triggering_Alternative;

   -----------------------
   -- Check_Max_Entries --
   -----------------------

   procedure Check_Max_Entries (D : Node_Id; R : All_Parameter_Restrictions) is
      Ecount : Uint;

      procedure Count (L : List_Id);
      --  Count entries in given declaration list

      -----------
      -- Count --
      -----------

      procedure Count (L : List_Id) is
         D : Node_Id;

      begin
         if No (L) then
            return;
         end if;

         D := First (L);
         while Present (D) loop
            if Nkind (D) = N_Entry_Declaration then
               declare
                  DSD : constant Node_Id :=
                          Discrete_Subtype_Definition (D);

               begin
                  --  If not an entry family, then just one entry

                  if No (DSD) then
                     Ecount := Ecount + 1;

                  --  If entry family with static bounds, count entries

                  elsif Is_OK_Static_Subtype (Etype (DSD)) then
                     declare
                        Lo : constant Uint :=
                               Expr_Value
                                 (Type_Low_Bound (Etype (DSD)));
                        Hi : constant Uint :=
                               Expr_Value
                                 (Type_High_Bound (Etype (DSD)));

                     begin
                        if Hi >= Lo then
                           Ecount := Ecount + Hi - Lo + 1;
                        end if;
                     end;

                  --  Entry family with non-static bounds

                  else
                     --  Record an unknown count restriction, and if the
                     --  restriction is active, post a message or warning.

                     Check_Restriction (R, D);
                  end if;
               end;
            end if;

            Next (D);
         end loop;
      end Count;

   --  Start of processing for Check_Max_Entries

   begin
      Ecount := Uint_0;
      Count (Visible_Declarations (D));
      Count (Private_Declarations (D));

      if Ecount > 0 then
         Check_Restriction (R, D, Ecount);
      end if;
   end Check_Max_Entries;

   ----------------------
   -- Check_Interfaces --
   ----------------------

   procedure Check_Interfaces (N : Node_Id; T : Entity_Id) is
      Iface     : Node_Id;
      Iface_Typ : Entity_Id;

   begin
      pragma Assert
        (Nkind (N) in N_Protected_Type_Declaration | N_Task_Type_Declaration);

      if Present (Interface_List (N)) then
         Set_Is_Tagged_Type (T);

         --  The primitive operations of a tagged synchronized type are placed
         --  on the Corresponding_Record for proper dispatching, but are
         --  attached to the synchronized type itself when expansion is
         --  disabled.

         Set_Direct_Primitive_Operations (T, New_Elmt_List);

         Iface := First (Interface_List (N));
         while Present (Iface) loop
            Iface_Typ := Find_Type_Of_Subtype_Indic (Iface);

            if not Is_Interface (Iface_Typ) then
               Error_Msg_NE
                 ("(Ada 2005) & must be an interface", Iface, Iface_Typ);

            else
               --  Ada 2005 (AI-251): "The declaration of a specific descendant
               --  of an interface type freezes the interface type" RM 13.14.

               Freeze_Before (N, Etype (Iface));

               --  Implicit inheritance of attribute

               if not Has_First_Controlling_Parameter_Aspect (T)
                 and then Has_First_Controlling_Parameter_Aspect (Iface_Typ)
               then
                  Set_Has_First_Controlling_Parameter_Aspect (T);
               end if;

               if Nkind (N) = N_Protected_Type_Declaration then

                  --  Ada 2005 (AI-345): Protected types can only implement
                  --  limited, synchronized, or protected interfaces (note that
                  --  the predicate Is_Limited_Interface includes synchronized
                  --  and protected interfaces).

                  if Is_Task_Interface (Iface_Typ) then
                     Error_Msg_N ("(Ada 2005) protected type cannot implement "
                       & "a task interface", Iface);

                  elsif not Is_Limited_Interface (Iface_Typ) then
                     Error_Msg_N ("(Ada 2005) protected type cannot implement "
                       & "a non-limited interface", Iface);
                  end if;

               else pragma Assert (Nkind (N) = N_Task_Type_Declaration);

                  --  Ada 2005 (AI-345): Task types can only implement limited,
                  --  synchronized, or task interfaces (note that the predicate
                  --  Is_Limited_Interface includes synchronized and task
                  --  interfaces).

                  if Is_Protected_Interface (Iface_Typ) then
                     Error_Msg_N ("(Ada 2005) task type cannot implement a " &
                       "protected interface", Iface);

                  elsif not Is_Limited_Interface (Iface_Typ) then
                     Error_Msg_N ("(Ada 2005) task type cannot implement a " &
                       "non-limited interface", Iface);
                  end if;
               end if;
            end if;

            Next (Iface);
         end loop;

         --  Check consistency of any nonoverridable aspects that are
         --  inherited from multiple sources.

         Check_Inherited_Nonoverridable_Aspects
           (Inheritor      => N,
            Interface_List => Interface_List (N),
            Parent_Type    => Empty);
      end if;

      if not Has_Private_Declaration (T) then
         return;
      end if;

      --  Additional checks on full-types associated with private type
      --  declarations. Search for the private type declaration.

      declare
         Full_T_Ifaces : Elist_Id := No_Elist;
         Iface         : Node_Id;
         Priv_T        : Entity_Id;
         Priv_T_Ifaces : Elist_Id := No_Elist;

      begin
         Priv_T := First_Entity (Scope (T));
         loop
            pragma Assert (Present (Priv_T));

            if Is_Type (Priv_T) and then Present (Full_View (Priv_T)) then
               exit when Full_View (Priv_T) = T;
            end if;

            Next_Entity (Priv_T);
         end loop;

         --  In case of synchronized types covering interfaces the private type
         --  declaration must be limited.

         if Present (Interface_List (N))
           and then not Is_Limited_Type (Priv_T)
         then
            Error_Msg_Sloc := Sloc (Priv_T);
            Error_Msg_N ("(Ada 2005) limited type declaration expected for " &
                         "private type#", T);
         end if;

         --  RM 7.3 (7.1/2): If the full view has a partial view that is
         --  tagged then check RM 7.3 subsidiary rules.

         if Is_Tagged_Type (Priv_T)
           and then not Error_Posted (N)
         then
            --  RM 7.3 (7.2/2): The partial view shall be a synchronized tagged
            --  type if and only if the full type is a synchronized tagged type

            if Is_Synchronized_Tagged_Type (Priv_T)
              and then not Is_Synchronized_Tagged_Type (T)
            then
               Error_Msg_N
                 ("(Ada 2005) full view must be a synchronized tagged " &
                  "type (RM 7.3 (7.2/2))", Priv_T);

            elsif Is_Synchronized_Tagged_Type (T)
              and then not Is_Synchronized_Tagged_Type (Priv_T)
            then
               Error_Msg_N
                 ("(Ada 2005) partial view must be a synchronized tagged " &
                  "type (RM 7.3 (7.2/2))", T);
            end if;

            --  RM 7.3 (7.3/2): The partial view shall be a descendant of an
            --  interface type if and only if the full type is descendant of
            --  the interface type.

            if Present (Interface_List (N))
              or else (Is_Tagged_Type (Priv_T)
                         and then Has_Interfaces
                                   (Priv_T, Use_Full_View => False))
            then
               if Is_Tagged_Type (Priv_T) then
                  Collect_Interfaces
                    (Priv_T, Priv_T_Ifaces, Use_Full_View => False);
               end if;

               if Is_Tagged_Type (T) then
                  Collect_Interfaces (T, Full_T_Ifaces);
               end if;

               Iface := Find_Hidden_Interface (Priv_T_Ifaces, Full_T_Ifaces);

               if Present (Iface) then
                  Error_Msg_NE
                    ("interface in partial view& not implemented by full "
                     & "type (RM-2005 7.3 (7.3/2))", T, Iface);
               end if;

               Iface := Find_Hidden_Interface (Full_T_Ifaces, Priv_T_Ifaces);

               if Present (Iface) then
                  Error_Msg_NE
                    ("interface & not implemented by partial " &
                     "view (RM-2005 7.3 (7.3/2))", T, Iface);
               end if;
            end if;
         end if;
      end;
   end Check_Interfaces;

   --------------------------------
   -- Check_Triggering_Statement --
   --------------------------------

   procedure Check_Triggering_Statement
     (Trigger        : Node_Id;
      Error_Node     : Node_Id;
      Is_Dispatching : out Boolean)
   is
      Param : Node_Id;

   begin
      Is_Dispatching := False;

      --  It is not possible to have a dispatching trigger if we are not in
      --  Ada 2005 mode.

      if Ada_Version >= Ada_2005
        and then Nkind (Trigger) = N_Procedure_Call_Statement
        and then Present (Parameter_Associations (Trigger))
      then
         Param := First (Parameter_Associations (Trigger));

         if Is_Controlling_Actual (Param)
           and then Is_Interface (Etype (Param))
         then
            if Is_Limited_Record (Etype (Param)) then
               Is_Dispatching := True;
            else
               Error_Msg_N
                 ("dispatching operation of limited or synchronized " &
                  "interface required (RM 9.7.2(3))!", Error_Node);
            end if;

         elsif Nkind (Trigger) = N_Explicit_Dereference then
            Error_Msg_N
              ("entry call or dispatching primitive of interface required",
                Trigger);
         end if;
      end if;
   end Check_Triggering_Statement;

   --------------------------
   -- Find_Concurrent_Spec --
   --------------------------

   function Find_Concurrent_Spec (Body_Id : Entity_Id) return Entity_Id is
      Spec_Id : Entity_Id := Current_Entity_In_Scope (Body_Id);

   begin
      --  The type may have been given by an incomplete type declaration.
      --  Find full view now.

      if Present (Spec_Id) and then Ekind (Spec_Id) = E_Incomplete_Type then
         Spec_Id := Full_View (Spec_Id);
      end if;

      return Spec_Id;
   end Find_Concurrent_Spec;

   --------------------------
   -- Install_Declarations --
   --------------------------

   procedure Install_Declarations (Spec : Entity_Id) is
      E    : Entity_Id;
      Prev : Entity_Id;
   begin
      E := First_Entity (Spec);
      while Present (E) loop
         Prev := Current_Entity (E);
         Set_Current_Entity (E);
         Set_Is_Immediately_Visible (E);
         Set_Homonym (E, Prev);
         Next_Entity (E);
      end loop;
   end Install_Declarations;

end Sem_Ch9;
