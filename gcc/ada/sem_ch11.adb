------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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
with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp;    use Uintp;

package body Sem_Ch11 is

   -----------------------------------
   -- Analyze_Exception_Declaration --
   -----------------------------------

   procedure Analyze_Exception_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      PF : constant Boolean   := Is_Pure (Current_Scope);

   begin
      Generate_Definition (Id);
      Enter_Name          (Id);
      Set_Ekind           (Id, E_Exception);
      Set_Exception_Code  (Id, Uint_0);
      Set_Etype           (Id, Standard_Exception_Type);

      Set_Is_Statically_Allocated (Id);
      Set_Is_Pure (Id, PF);
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
                     Error_Msg_NE
                       ("exception choice duplicates &#", Id, Id1);

                  else
                     if Ada_Version = Ada_83
                       and then Comes_From_Source (Id)
                     then
                        Error_Msg_N
                          ("(Ada 83): duplicate exception choice&", Id);
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

   --  Start processing for Analyze_Exception_Handlers

   begin
      Handler := First (L);
      Check_Restriction (No_Exceptions, Handler);
      Check_Restriction (No_Exception_Handlers, Handler);

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
            --  Deal with choice parameter. The exception handler is
            --  a declarative part for it, so it constitutes a scope
            --  for visibility purposes. We create an entity to denote
            --  the whole exception part, and use it as the scope of all
            --  the choices, which may even have the same name without
            --  conflict. This scope plays no other role in expansion or
            --  or code generation.

            Choice := Choice_Parameter (Handler);

            if Present (Choice) then
               if No (H_Scope) then
                  H_Scope := New_Internal_Entity
                    (E_Block, Current_Scope, Sloc (Choice), 'E');
               end if;

               New_Scope (H_Scope);
               Set_Etype (H_Scope, Standard_Void_Type);

               --  Set the Finalization Chain entity to Error means that it
               --  should not be used at that level but the parent one
               --  should be used instead.

               --  ??? this usage needs documenting in Einfo/Exp_Ch7 ???
               --  ??? using Error for this non-error condition is nasty ???

               Set_Finalization_Chain_Entity (H_Scope, Error);

               Enter_Name (Choice);
               Set_Ekind (Choice, E_Variable);
               Set_Etype (Choice, RTE (RE_Exception_Occurrence));
               Generate_Definition (Choice);

               --  Set source assigned flag, since in effect this field
               --  is always assigned an initial value by the exception.

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

                  if not Is_Entity_Name (Id)
                    or else Ekind (Entity (Id)) /= E_Exception
                  then
                     Error_Msg_N ("exception name expected", Id);

                  else
                     if Present (Renamed_Entity (Entity (Id))) then
                        if Entity (Id) = Standard_Numeric_Error then
                           Check_Restriction (No_Obsolescent_Features, Id);

                           if Warn_On_Obsolescent_Feature then
                              Error_Msg_N
                                ("Numeric_Error is an " &
                                 "obsolescent feature ('R'M 'J.6(1))?", Id);
                              Error_Msg_N
                                ("\use Constraint_Error instead?", Id);
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
                           --  If the exception is declared in an inner
                           --  instance, nothing else to check.

                           if Is_Generic_Instance (Scop) then
                              exit;

                           elsif Nkind (Declaration_Node (Scop)) =
                                           N_Package_Specification
                             and then
                               Nkind (Original_Node (Parent
                                 (Declaration_Node (Scop)))) =
                                           N_Formal_Package_Declaration
                           then
                              Error_Msg_NE
                                ("exception& is declared in "  &
                                 "generic formal package", Id, Ent);
                              Error_Msg_N
                                ("\and therefore cannot appear in " &
                                 "handler ('R'M 11.2(8))", Id);
                              exit;
                           end if;

                           Scop := Scope (Scop);
                        end loop;
                     end;
                  end if;
               end if;

               Next (Id);
            end loop;

            --  Check for redundant handler (has only raise statement) and
            --  is either an others handler, or is a specific handler when
            --  no others handler is present.

            if Warn_On_Redundant_Constructs
              and then List_Length (Statements (Handler)) = 1
              and then Nkind (First (Statements (Handler))) = N_Raise_Statement
              and then No (Name (First (Statements (Handler))))
              and then (not Others_Present
                          or else Nkind (First (Exception_Choices (Handler))) =
                                              N_Others_Choice)
            then
               Error_Msg_N
                 ("useless handler contains only a reraise statement?",
                  Handler);
            end if;

            --  Now analyze the statements of this handler

            Analyze_Statements (Statements (Handler));

            --  If a choice was present, we created a special scope for it,
            --  so this is where we pop that special scope to get rid of it.

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

   begin
      if Present (Handlers) then
         Kill_All_Checks;
      end if;

      Analyze_Statements (Statements (N));

      if Present (Handlers) then
         Analyze_Exception_Handlers (Handlers);

      elsif Present (At_End_Proc (N)) then
         Analyze (At_End_Proc (N));
      end if;
   end Analyze_Handled_Statements;

   -----------------------------
   -- Analyze_Raise_Statement --
   -----------------------------

   procedure Analyze_Raise_Statement (N : Node_Id) is
      Exception_Id   : constant Node_Id := Name (N);
      Exception_Name : Entity_Id        := Empty;
      P              : Node_Id;
      Nkind_P        : Node_Kind;

   begin
      Check_Unreachable_Code (N);

      --  Check exception restrictions on the original source

      if Comes_From_Source (N) then
         Check_Restriction (No_Exceptions, N);
      end if;

      --  Check for useless assignment to OUT or IN OUT scalar
      --  immediately preceding the raise. Right now we only look
      --  at assignment statements, we could do more.

      if Is_List_Member (N) then
         declare
            P : Node_Id;
            L : Node_Id;

         begin
            P := Prev (N);

            if Present (P)
              and then Nkind (P) = N_Assignment_Statement
            then
               L := Name (P);

               if Is_Scalar_Type (Etype (L))
                 and then Is_Entity_Name (L)
                 and then Is_Formal (Entity (L))
               then
                  Error_Msg_N
                    ("?assignment to pass-by-copy formal may have no effect",
                      P);
                  Error_Msg_N
                    ("\?RAISE statement may result in abnormal return" &
                     " ('R'M 6.4.1(17))", P);
               end if;
            end if;
         end;
      end if;

      --  Reraise statement

      if No (Exception_Id) then

         P := Parent (N);
         Nkind_P := Nkind (P);

         while Nkind_P /= N_Exception_Handler
           and then Nkind_P /= N_Subprogram_Body
           and then Nkind_P /= N_Package_Body
           and then Nkind_P /= N_Task_Body
           and then Nkind_P /= N_Entry_Body
         loop
            P := Parent (P);
            Nkind_P := Nkind (P);
         end loop;

         if Nkind (P) /= N_Exception_Handler then
            Error_Msg_N
              ("reraise statement must appear directly in a handler", N);
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
         end if;

         if Present (Expression (N)) then
            Analyze_And_Resolve (Expression (N), Standard_String);
         end if;
      end if;
   end Analyze_Raise_Statement;

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
   end Analyze_Raise_xxx_Error;

   -----------------------------
   -- Analyze_Subprogram_Info --
   -----------------------------

   procedure Analyze_Subprogram_Info (N : Node_Id) is
   begin
      Set_Etype (N, RTE (RE_Code_Loc));
   end Analyze_Subprogram_Info;

end Sem_Ch11;
