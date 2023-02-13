------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Checks;         use Checks;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
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
with Sem_Ch5;        use Sem_Ch5;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sem_Warn;       use Sem_Warn;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Stand;          use Stand;
with Warnsw;         use Warnsw;

package body Sem_Ch11 is

   -----------------------------------
   -- Analyze_Exception_Declaration --
   -----------------------------------

   procedure Analyze_Exception_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      PF : constant Boolean   := Is_Pure (Current_Scope);

   begin
      Generate_Definition         (Id);
      Enter_Name                  (Id);
      Mutate_Ekind                (Id, E_Exception);
      Set_Etype                   (Id, Standard_Exception_Type);
      Set_Is_Statically_Allocated (Id);
      Set_Is_Pure                 (Id, PF);

      if Has_Aspects (N) then
         Analyze_Aspect_Specifications (N, Id);
      end if;
   end Analyze_Exception_Declaration;

   --------------------------------
   -- Analyze_Exception_Handlers --
   --------------------------------

   procedure Analyze_Exception_Handlers (L : List_Id) is
      Handler : Node_Id;
      Choice  : Entity_Id;
      Id      : Node_Id;
      H_Scope : Entity_Id := Empty;

      procedure Check_Duplication (Id : Node_Id);
      --  Iterate through the identifiers in each handler to find duplicates

      function Others_Present return Boolean;
      --  Returns True if others handler is present

      -----------------------
      -- Check_Duplication --
      -----------------------

      procedure Check_Duplication (Id : Node_Id) is
         Handler   : Node_Id;
         Id1       : Node_Id;
         Id_Entity : Entity_Id := Entity (Id);

      begin
         if Present (Renamed_Entity (Id_Entity)) then
            Id_Entity := Renamed_Entity (Id_Entity);
         end if;

         Handler := First_Non_Pragma (L);
         while Present (Handler) loop
            Id1 := First (Exception_Choices (Handler));
            while Present (Id1) loop

               --  Only check against the exception choices which precede
               --  Id in the handler, since the ones that follow Id have not
               --  been analyzed yet and will be checked in a subsequent call.

               if Id = Id1 then
                  return;

               elsif Nkind (Id1) /= N_Others_Choice
                 and then
                   (Id_Entity = Entity (Id1)
                     or else (Id_Entity = Renamed_Entity (Entity (Id1))))
               then
                  if Handler /= Parent (Id) then
                     Error_Msg_Sloc := Sloc (Id1);
                     Error_Msg_NE ("exception choice duplicates &#", Id, Id1);

                  else
                     if Ada_Version = Ada_83
                       and then Comes_From_Source (Id)
                     then
                        Error_Msg_N
                          ("(Ada 83) duplicate exception choice&", Id);
                     end if;
                  end if;
               end if;

               Next_Non_Pragma (Id1);
            end loop;

            Next (Handler);
         end loop;
      end Check_Duplication;

      --------------------
      -- Others_Present --
      --------------------

      function Others_Present return Boolean is
         H : Node_Id;

      begin
         H := First (L);
         while Present (H) loop
            if Nkind (H) /= N_Pragma
              and then Nkind (First (Exception_Choices (H))) = N_Others_Choice
            then
               return True;
            end if;

            Next (H);
         end loop;

         return False;
      end Others_Present;

   --  Start of processing for Analyze_Exception_Handlers

   begin
      Handler := First (L);

      --  Pragma Restriction_Warnings has more related semantics than pragma
      --  Restrictions in that it flags exception handlers as violators. Note
      --  that the compiler must still generate handlers for certain critical
      --  scenarios such as finalization. As a result, these handlers should
      --  not be subjected to the restriction check when in warnings mode.

      if not Comes_From_Source (Handler)
        and then (Restriction_Warnings (No_Exception_Handlers)
                   or else Restriction_Warnings (No_Exception_Propagation)
                   or else Restriction_Warnings (No_Exceptions))
      then
         null;

      else
         Check_Restriction (No_Exceptions, Handler);
         Check_Restriction (No_Exception_Handlers, Handler);
      end if;

      --  Kill current remembered values, since we don't know where we were
      --  when the exception was raised.

      Kill_Current_Values;

      --  Loop through handlers (which can include pragmas)

      while Present (Handler) loop

         --  If pragma just analyze it

         if Nkind (Handler) = N_Pragma then
            Analyze (Handler);

         --  Otherwise we have a real exception handler

         else
            --  Deal with choice parameter. The exception handler is a
            --  declarative part for the choice parameter, so it constitutes a
            --  scope for visibility purposes. We create an entity to denote
            --  the whole exception part, and use it as the scope of all the
            --  choices, which may even have the same name without conflict.
            --  This scope plays no other role in expansion or code generation.

            Choice := Choice_Parameter (Handler);

            if Present (Choice) then
               Set_Local_Raise_Not_OK (Handler);

               if Comes_From_Source (Choice) then
                  Check_Restriction (No_Exception_Propagation, Choice);
                  Set_Debug_Info_Needed (Choice);
               end if;

               if No (H_Scope) then
                  H_Scope :=
                    New_Internal_Entity
                     (E_Block, Current_Scope, Sloc (Choice), 'E');
                  Set_Is_Exception_Handler (H_Scope);
               end if;

               Push_Scope (H_Scope);
               Set_Etype (H_Scope, Standard_Void_Type);

               Enter_Name (Choice);
               Mutate_Ekind (Choice, E_Variable);

               if RTE_Available (RE_Exception_Occurrence) then
                  Set_Etype (Choice, RTE (RE_Exception_Occurrence));
               end if;

               Generate_Definition (Choice);

               --  Indicate that choice has an initial value, since in effect
               --  this field is assigned an initial value by the exception.
               --  We also consider that it is modified in the source.

               Set_Has_Initial_Value (Choice, True);
               Set_Never_Set_In_Source (Choice, False);
            end if;

            Id := First (Exception_Choices (Handler));
            while Present (Id) loop
               if Nkind (Id) = N_Others_Choice then
                  if Present (Next (Id))
                    or else Present (Next (Handler))
                    or else Present (Prev (Id))
                  then
                     Error_Msg_N ("OTHERS must appear alone and last", Id);
                  end if;

               else
                  Analyze (Id);

                  --  In most cases the choice has already been analyzed in
                  --  Analyze_Handled_Statement_Sequence, in order to expand
                  --  local handlers. This advance analysis does not take into
                  --  account the case in which a choice has the same name as
                  --  the choice parameter of the handler, which may hide an
                  --  outer exception. This pathological case appears in ACATS
                  --  B80001_3.adb, and requires an explicit check to verify
                  --  that the id is not hidden.

                  if not Is_Entity_Name (Id)
                    or else Ekind (Entity (Id)) /= E_Exception
                    or else
                      (Nkind (Id) = N_Identifier
                        and then Chars (Id) = Chars (Choice))
                  then
                     Error_Msg_N ("exception name expected", Id);

                  else
                     --  Emit a warning at the declaration level when a local
                     --  exception is never raised explicitly.

                     if Warn_On_Redundant_Constructs
                       and then not Is_Raised (Entity (Id))
                       and then Scope (Entity (Id)) = Current_Scope
                     then
                        Error_Msg_NE
                          ("exception & is never raised?r?", Entity (Id), Id);
                     end if;

                     if Present (Renamed_Entity (Entity (Id))) then
                        if Entity (Id) = Standard_Numeric_Error then
                           Check_Restriction (No_Obsolescent_Features, Id);

                           if Warn_On_Obsolescent_Feature then
                              Error_Msg_N
                                ("Numeric_Error is an " &
                                 "obsolescent feature (RM J.6(1))?j?", Id);
                              Error_Msg_N
                                ("\use Constraint_Error instead?j?", Id);
                           end if;
                        end if;
                     end if;

                     Check_Duplication (Id);

                     --  Check for exception declared within generic formal
                     --  package (which is illegal, see RM 11.2(8))

                     declare
                        Ent  : Entity_Id := Entity (Id);
                        Scop : Entity_Id;

                     begin
                        if Present (Renamed_Entity (Ent)) then
                           Ent := Renamed_Entity (Ent);
                        end if;

                        Scop := Scope (Ent);
                        while Scop /= Standard_Standard
                          and then Ekind (Scop) = E_Package
                        loop
                           if Nkind (Declaration_Node (Scop)) =
                                           N_Package_Specification
                             and then
                               Nkind (Original_Node (Parent
                                 (Declaration_Node (Scop)))) =
                                           N_Formal_Package_Declaration
                           then
                              Error_Msg_NE
                                ("exception& is declared in generic formal "
                                 & "package", Id, Ent);
                              Error_Msg_N
                                ("\and therefore cannot appear in handler "
                                 & "(RM 11.2(8))", Id);
                              exit;

                           --  If the exception is declared in an inner
                           --  instance, nothing else to check.

                           elsif Is_Generic_Instance (Scop) then
                              exit;
                           end if;

                           Scop := Scope (Scop);
                        end loop;
                     end;
                  end if;
               end if;

               Next (Id);
            end loop;

            --  Check for redundant handler (has only raise statement) and is
            --  either an others handler, or is a specific handler when no
            --  others handler is present.

            if Warn_On_Redundant_Constructs
              and then List_Length (Statements (Handler)) = 1
              and then Nkind (First (Statements (Handler))) = N_Raise_Statement
              and then No (Name (First (Statements (Handler))))
              and then (not Others_Present
                         or else Nkind (First (Exception_Choices (Handler))) =
                                              N_Others_Choice)
            then
               Error_Msg_N
                 ("useless handler contains only a reraise statement?r?",
                  Handler);
            end if;

            --  Now analyze the statements of this handler

            Analyze_Statements (Statements (Handler));

            --  If a choice was present, we created a special scope for it, so
            --  this is where we pop that special scope to get rid of it.

            if Present (Choice) then
               End_Scope;
            end if;
         end if;

         Next (Handler);
      end loop;
   end Analyze_Exception_Handlers;

   --------------------------------
   -- Analyze_Handled_Statements --
   --------------------------------

   procedure Analyze_Handled_Statements (N : Node_Id) is
      Handlers : constant List_Id := Exception_Handlers (N);
      Handler  : Node_Id;
      Choice   : Node_Id;

   begin
      if Present (Handlers) then
         Kill_All_Checks;
      end if;

      --  We are now going to analyze the statements and then the exception
      --  handlers. We certainly need to do things in this order to get the
      --  proper sequential semantics for various warnings.

      --  However, there is a glitch. When we process raise statements, an
      --  optimization is to look for local handlers and specialize the code
      --  in this case.

      --  In order to detect if a handler is matching, we must have at least
      --  analyzed the choices in the proper scope so that proper visibility
      --  analysis is performed. Hence we analyze just the choices first,
      --  before we analyze the statement sequence.

      Handler := First_Non_Pragma (Handlers);
      while Present (Handler) loop
         Choice := First_Non_Pragma (Exception_Choices (Handler));
         while Present (Choice) loop
            Analyze (Choice);
            Next_Non_Pragma (Choice);
         end loop;

         Next_Non_Pragma (Handler);
      end loop;

      --  Analyze statements in sequence

      Analyze_Statements (Statements (N));

      --  If the current scope is a subprogram, entry or task body or declare
      --  block then this is the right place to check for hanging useless
      --  assignments from the statement sequence.

      if Is_Subprogram_Or_Entry (Current_Scope)
        or else Ekind (Current_Scope) in E_Block | E_Task_Type
      then
         Warn_On_Useless_Assignments (Current_Scope);
      end if;

      --  Deal with handlers or AT END proc

      if Present (Handlers) then
         Analyze_Exception_Handlers (Handlers);
      elsif Present (At_End_Proc (N)) then
         Analyze (At_End_Proc (N));
      end if;
   end Analyze_Handled_Statements;

   ------------------------------
   -- Analyze_Raise_Expression --
   ------------------------------

   procedure Analyze_Raise_Expression (N : Node_Id) is
      Exception_Id   : constant Node_Id := Name (N);
      Exception_Name : Entity_Id        := Empty;

   begin
      --  Check exception restrictions on the original source

      if Comes_From_Source (N) then
         Check_Restriction (No_Exceptions, N);
      end if;

      Analyze (Exception_Id);

      if Is_Entity_Name (Exception_Id) then
         Exception_Name := Entity (Exception_Id);
      end if;

      if No (Exception_Name)
        or else Ekind (Exception_Name) /= E_Exception
      then
         Error_Msg_N
           ("exception name expected in raise statement", Exception_Id);
      else
         Set_Is_Raised (Exception_Name);
      end if;

      --  Deal with RAISE WITH case

      if Present (Expression (N)) then
         Analyze_And_Resolve (Expression (N), Standard_String);
      end if;

      --  Check obsolescent use of Numeric_Error

      if Exception_Name = Standard_Numeric_Error then
         Check_Restriction (No_Obsolescent_Features, Exception_Id);
      end if;

      --  Kill last assignment indication

      Kill_Current_Values (Last_Assignment_Only => True);

      --  Raise_Type is compatible with all other types so that the raise
      --  expression is legal in any expression context. It will be eventually
      --  replaced by the concrete type imposed by the context.

      Set_Etype (N, Raise_Type);
   end Analyze_Raise_Expression;

   -----------------------------
   -- Analyze_Raise_Statement --
   -----------------------------

   procedure Analyze_Raise_Statement (N : Node_Id) is
      Exception_Id   : constant Node_Id := Name (N);
      Exception_Name : Entity_Id        := Empty;
      P              : Node_Id;
      Par            : Node_Id;

   begin
      Check_Unreachable_Code (N);

      --  Check exception restrictions on the original source

      if Comes_From_Source (N) then
         Check_Restriction (No_Exceptions, N);
      end if;

      --  Check for useless assignment to OUT or IN OUT scalar preceding the
      --  raise. Right now only look at assignment statements, could do more???

      if Is_List_Member (N) then
         declare
            P : Node_Id;
            L : Node_Id;

         begin
            P := Prev (N);

            --  Skip past null statements and pragmas

            while Present (P)
              and then Nkind (P) in N_Null_Statement | N_Pragma
            loop
               P := Prev (P);
            end loop;

            --  See if preceding statement is an assignment

            if Present (P) and then Nkind (P) = N_Assignment_Statement then
               L := Name (P);

               --  Give warning for assignment to scalar formal

               if Is_Scalar_Type (Etype (L))
                 and then Is_Entity_Name (L)
                 and then Is_Formal (Entity (L))

                 --  Do this only for parameters to the current subprogram.
                 --  This avoids some false positives for the nested case.

                 and then Nearest_Dynamic_Scope (Current_Scope) =
                                                        Scope (Entity (L))

               then
                  --  Don't give warning if we are covered by an exception
                  --  handler, since this may result in false positives, since
                  --  the handler may handle the exception and return normally.

                  --  First find the enclosing handled sequence of statements
                  --  (note, we could also look for a handler in an outer block
                  --  but currently we don't, and in that case we'll emit the
                  --  warning).

                  Par := N;
                  loop
                     Par := Parent (Par);
                     exit when Nkind (Par) = N_Handled_Sequence_Of_Statements;
                  end loop;

                  --  See if there is a handler, give message if not

                  if No (Exception_Handlers (Par)) then
                     Error_Msg_N
                       ("assignment to pass-by-copy formal "
                        & "may have no effect??", P);
                     Error_Msg_N
                       ("\RAISE statement may result in abnormal return "
                        & "(RM 6.4.1(17))??", P);
                  end if;
               end if;
            end if;
         end;
      end if;

      --  Reraise statement

      if No (Exception_Id) then
         P := Parent (N);
         while Nkind (P) not in
                 N_Exception_Handler | N_Subprogram_Body | N_Package_Body |
                 N_Task_Body         | N_Entry_Body
         loop
            P := Parent (P);
         end loop;

         if Nkind (P) /= N_Exception_Handler then
            Error_Msg_N
              ("reraise statement must appear directly in a handler", N);

         --  If a handler has a reraise, it cannot be the target of a local
         --  raise (goto optimization is impossible), and if the no exception
         --  propagation restriction is set, this is a violation.

         else
            Set_Local_Raise_Not_OK (P);
            Check_Restriction (No_Exception_Propagation, N);
         end if;

      --  Normal case with exception id present

      else
         Analyze (Exception_Id);

         if Is_Entity_Name (Exception_Id) then
            Exception_Name := Entity (Exception_Id);
         end if;

         if No (Exception_Name)
           or else Ekind (Exception_Name) /= E_Exception
         then
            Error_Msg_N
              ("exception name expected in raise statement", Exception_Id);
         else
            Set_Is_Raised (Exception_Name);
         end if;

         --  Deal with RAISE WITH case

         if Present (Expression (N)) then
            Analyze_And_Resolve (Expression (N), Standard_String);
         end if;
      end if;

      --  Check obsolescent use of Numeric_Error

      if Exception_Name = Standard_Numeric_Error then
         Check_Restriction (No_Obsolescent_Features, Exception_Id);
      end if;

      --  Kill last assignment indication

      Kill_Current_Values (Last_Assignment_Only => True);
   end Analyze_Raise_Statement;

   ----------------------------------
   -- Analyze_Raise_When_Statement --
   ----------------------------------

   procedure Analyze_Raise_When_Statement (N : Node_Id) is
   begin
      --  Verify the condition is a Boolean expression

      Analyze_And_Resolve (Condition (N), Any_Boolean);
      Check_Unset_Reference (Condition (N));
   end Analyze_Raise_When_Statement;

   -----------------------------
   -- Analyze_Raise_xxx_Error --
   -----------------------------

   --  Normally, the Etype is already set (when this node is used within
   --  an expression, since it is copied from the node which it rewrites).
   --  If this node is used in a statement context, then we set the type
   --  Standard_Void_Type. This is used both by Gigi and by the front end
   --  to distinguish the statement use and the subexpression use.

   --  The only other required processing is to take care of the Condition
   --  field if one is present.

   procedure Analyze_Raise_xxx_Error (N : Node_Id) is

      function Same_Expression (C1, C2 : Node_Id) return Boolean;
      --  It often occurs that two identical raise statements are generated in
      --  succession (for example when dynamic elaboration checks take place on
      --  separate expressions in a call). If the two statements are identical
      --  according to the simple criterion that follows, the raise is
      --  converted into a null statement.

      ---------------------
      -- Same_Expression --
      ---------------------

      function Same_Expression (C1, C2 : Node_Id) return Boolean is
      begin
         if No (C1) and then No (C2) then
            return True;

         elsif Is_Entity_Name (C1) and then Is_Entity_Name (C2) then
            return Entity (C1) = Entity (C2);

         elsif Nkind (C1) /= Nkind (C2) then
            return False;

         elsif Nkind (C1) in N_Unary_Op then
            return Same_Expression (Right_Opnd (C1), Right_Opnd (C2));

         elsif Nkind (C1) in N_Binary_Op then
            return Same_Expression (Left_Opnd (C1),  Left_Opnd (C2))
                     and then
                   Same_Expression (Right_Opnd (C1), Right_Opnd (C2));

         elsif Nkind (C1) = N_Null then
            return True;

         else
            return False;
         end if;
      end Same_Expression;

   --  Start of processing for Analyze_Raise_xxx_Error

   begin
      if No (Etype (N)) then
         Set_Etype (N, Standard_Void_Type);
      end if;

      if Present (Condition (N)) then
         Analyze_And_Resolve (Condition (N), Standard_Boolean);
      end if;

      --  Deal with static cases in obvious manner

      if Nkind (Condition (N)) = N_Identifier then
         if Entity (Condition (N)) = Standard_True then
            Set_Condition (N, Empty);

         elsif Entity (Condition (N)) = Standard_False then
            Rewrite (N, Make_Null_Statement (Sloc (N)));
         end if;
      end if;

      --  Remove duplicate raise statements. Note that the previous one may
      --  already have been removed as well.

      if not Comes_From_Source (N)
        and then Nkind (N) /= N_Null_Statement
        and then Is_List_Member (N)
        and then Present (Prev (N))
        and then Nkind (N) = Nkind (Original_Node (Prev (N)))
        and then Same_Expression
                   (Condition (N), Condition (Original_Node (Prev (N))))
      then
         Rewrite (N, Make_Null_Statement (Sloc (N)));
      end if;
   end Analyze_Raise_xxx_Error;

end Sem_Ch11;
