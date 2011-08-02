------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 9                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order by RM
--  section rather than alphabetical.

separate (Par)
package body Ch9 is

   --  Local subprograms, used only in this chapter

   function P_Accept_Alternative                   return Node_Id;
   function P_Delay_Alternative                    return Node_Id;
   function P_Delay_Relative_Statement             return Node_Id;
   function P_Delay_Until_Statement                return Node_Id;
   function P_Entry_Barrier                        return Node_Id;
   function P_Entry_Body_Formal_Part               return Node_Id;
   function P_Entry_Declaration                    return Node_Id;
   function P_Entry_Index_Specification            return Node_Id;
   function P_Protected_Definition                 return Node_Id;
   function P_Protected_Operation_Declaration_Opt  return Node_Id;
   function P_Protected_Operation_Items            return List_Id;
   function P_Task_Items                           return List_Id;
   function P_Task_Definition return Node_Id;

   -----------------------------
   -- 9.1  Task (also 10.1.3) --
   -----------------------------

   --  TASK_TYPE_DECLARATION ::=
   --    task type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
   --      [ASPECT_SPECIFICATIONS]
   --      [is [new INTERFACE_LIST with] TASK_DEFINITION];

   --  SINGLE_TASK_DECLARATION ::=
   --    task DEFINING_IDENTIFIER
   --      [ASPECT_SPECIFICATIONS]
   --      [is [new INTERFACE_LIST with] TASK_DEFINITION];

   --  TASK_BODY ::=
   --    task body DEFINING_IDENTIFIER is
   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [task_IDENTIFIER]

   --  TASK_BODY_STUB ::=
   --    task body DEFINING_IDENTIFIER is separate;

   --  This routine scans out a task declaration, task body, or task stub

   --  The caller has checked that the initial token is TASK and scanned
   --  past it, so that Token is set to the token after TASK

   --  Error recovery: cannot raise Error_Resync

   function P_Task return Node_Id is
      Name_Node  : Node_Id;
      Task_Node  : Node_Id;
      Task_Sloc  : Source_Ptr;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Lreq := False;
      Task_Sloc := Prev_Token_Ptr;

      if Token = Tok_Body then
         Scan; -- past BODY
         Name_Node := P_Defining_Identifier (C_Is);
         Scope.Table (Scope.Last).Labl := Name_Node;

         if Token = Tok_Left_Paren then
            Error_Msg_SC ("discriminant part not allowed in task body");
            Discard_Junk_List (P_Known_Discriminant_Part_Opt);
         end if;

         TF_Is;

         --  Task stub

         if Token = Tok_Separate then
            Scan; -- past SEPARATE
            Task_Node := New_Node (N_Task_Body_Stub, Task_Sloc);
            Set_Defining_Identifier (Task_Node, Name_Node);
            TF_Semicolon;
            Pop_Scope_Stack; -- remove unused entry

         --  Task body

         else
            Task_Node := New_Node (N_Task_Body, Task_Sloc);
            Set_Defining_Identifier (Task_Node, Name_Node);
            Parse_Decls_Begin_End (Task_Node);
         end if;

         return Task_Node;

      --  Otherwise we must have a task declaration

      else
         if Token = Tok_Type then
            Scan; -- past TYPE
            Task_Node := New_Node (N_Task_Type_Declaration, Task_Sloc);
            Name_Node := P_Defining_Identifier;
            Set_Defining_Identifier (Task_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;
            Set_Discriminant_Specifications
              (Task_Node, P_Known_Discriminant_Part_Opt);

         else
            Task_Node := New_Node (N_Single_Task_Declaration, Task_Sloc);
            Name_Node := P_Defining_Identifier (C_Is);
            Set_Defining_Identifier (Task_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;

            if Token = Tok_Left_Paren then
               Error_Msg_SC ("discriminant part not allowed for single task");
               Discard_Junk_List (P_Known_Discriminant_Part_Opt);
            end if;
         end if;

         --  Scan aspect specifications, don't eat the semicolon, since it
         --  might not be there if we have an IS.

         P_Aspect_Specifications (Task_Node, Semicolon => False);

         --  Parse optional task definition. Note that P_Task_Definition scans
         --  out the semicolon and possible aspect specifications as well as
         --  the task definition itself.

         if Token = Tok_Semicolon then

            --  A little check, if the next token after semicolon is Entry,
            --  then surely the semicolon should really be IS

            Scan; -- past semicolon

            if Token = Tok_Entry then
               Error_Msg_SP -- CODEFIX
                 ("|"";"" should be IS");
               Set_Task_Definition (Task_Node, P_Task_Definition);
            else
               Pop_Scope_Stack; -- Remove unused entry
            end if;

         --  Here we have a task definition

         else
            TF_Is; -- must have IS if no semicolon

            --  Ada 2005 (AI-345)

            if Token = Tok_New then
               Scan; --  past NEW

               if Ada_Version < Ada_2005 then
                  Error_Msg_SP ("task interface is an Ada 2005 extension");
                  Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
               end if;

               Set_Interface_List (Task_Node, New_List);

               loop
                  Append (P_Qualified_Simple_Name, Interface_List (Task_Node));
                  exit when Token /= Tok_And;
                  Scan; --  past AND
               end loop;

               if Token /= Tok_With then
                  Error_Msg_SC -- CODEFIX
                    ("WITH expected");
               end if;

               Scan; -- past WITH

               if Token = Tok_Private then
                  Error_Msg_SP -- CODEFIX
                    ("PRIVATE not allowed in task type declaration");
               end if;
            end if;

            Set_Task_Definition (Task_Node, P_Task_Definition);
         end if;

         return Task_Node;
      end if;
   end P_Task;

   --------------------------------
   -- 9.1  Task Type Declaration --
   --------------------------------

   --  Parsed by P_Task (9.1)

   ----------------------------------
   -- 9.1  Single Task Declaration --
   ----------------------------------

   --  Parsed by P_Task (9.1)

   --------------------------
   -- 9.1  Task Definition --
   --------------------------

   --  TASK_DEFINITION ::=
   --      {TASK_ITEM}
   --    [private
   --      {TASK_ITEM}]
   --    end [task_IDENTIFIER];

   --  The caller has already made the scope stack entry

   --  Note: there is a small deviation from official syntax here in that we
   --  regard the semicolon after end as part of the Task_Definition, and in
   --  the official syntax, it's part of the enclosing declaration. The reason
   --  for this deviation is that otherwise the end processing would have to
   --  be special cased, which would be a nuisance!

   --  Error recovery:  cannot raise Error_Resync

   function P_Task_Definition return Node_Id is
      Def_Node  : Node_Id;

   begin
      Def_Node := New_Node (N_Task_Definition, Token_Ptr);
      Set_Visible_Declarations (Def_Node, P_Task_Items);

      if Token = Tok_Private then
         Scan; -- past PRIVATE
         Set_Private_Declarations (Def_Node, P_Task_Items);

         --  Deal gracefully with multiple PRIVATE parts

         while Token = Tok_Private loop
            Error_Msg_SC ("only one private part allowed per task");
            Scan; -- past PRIVATE
            Append_List (P_Task_Items, Private_Declarations (Def_Node));
         end loop;
      end if;

      End_Statements (Def_Node);
      return Def_Node;
   end P_Task_Definition;

   --------------------
   -- 9.1  Task Item --
   --------------------

   --  TASK_ITEM ::= ENTRY_DECLARATION | REPRESENTATION_CLAUSE

   --  This subprogram scans a (possibly empty) list of task items and pragmas

   --  Error recovery:  cannot raise Error_Resync

   --  Note: a pragma can also be returned in this position

   function P_Task_Items return List_Id is
      Items      : List_Id;
      Item_Node  : Node_Id;
      Decl_Sloc  : Source_Ptr;

   begin
      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;

      --  Loop to scan out task items

      Items := New_List;

      Decl_Loop : loop
         Decl_Sloc := Token_Ptr;

         if Token = Tok_Pragma then
            Append (P_Pragma, Items);

         --  Ada 2005 (AI-397): Reserved words NOT and OVERRIDING
         --  may begin an entry declaration.

         elsif Token = Tok_Entry
           or else Token = Tok_Not
           or else Token = Tok_Overriding
         then
            Append (P_Entry_Declaration, Items);

         elsif Token = Tok_For then
            --  Representation clause in task declaration. The only rep
            --  clause which is legal in a protected is an address clause,
            --  so that is what we try to scan out.

            Item_Node := P_Representation_Clause;

            if Nkind (Item_Node) = N_At_Clause then
               Append (Item_Node, Items);

            elsif Nkind (Item_Node) = N_Attribute_Definition_Clause
              and then Chars (Item_Node) = Name_Address
            then
               Append (Item_Node, Items);

            else
               Error_Msg
                 ("the only representation clause " &
                  "allowed here is an address clause!", Decl_Sloc);
            end if;

         elsif Token = Tok_Identifier
           or else Token in Token_Class_Declk
         then
            Error_Msg_SC ("illegal declaration in task definition");
            Resync_Past_Semicolon;

         else
            exit Decl_Loop;
         end if;
      end loop Decl_Loop;

      return Items;
   end P_Task_Items;

   --------------------
   -- 9.1  Task Body --
   --------------------

   --  Parsed by P_Task (9.1)

   ----------------------------------
   -- 9.4  Protected (also 10.1.3) --
   ----------------------------------

   --  PROTECTED_TYPE_DECLARATION ::=
   --    protected type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
   --      [ASPECT_SPECIFICATIONS]
   --    is [new INTERFACE_LIST with] PROTECTED_DEFINITION;

   --  SINGLE_PROTECTED_DECLARATION ::=
   --    protected DEFINING_IDENTIFIER
   --      [ASPECT_SPECIFICATIONS]
   --    is [new INTERFACE_LIST with] PROTECTED_DEFINITION;

   --  PROTECTED_BODY ::=
   --    protected body DEFINING_IDENTIFIER is
   --      {PROTECTED_OPERATION_ITEM}
   --    end [protected_IDENTIFIER];

   --  PROTECTED_BODY_STUB ::=
   --    protected body DEFINING_IDENTIFIER is separate;

   --  This routine scans out a protected declaration, protected body
   --  or a protected stub.

   --  The caller has checked that the initial token is PROTECTED and
   --  scanned past it, so Token is set to the following token.

   --  Error recovery: cannot raise Error_Resync

   function P_Protected return Node_Id is
      Name_Node      : Node_Id;
      Protected_Node : Node_Id;
      Protected_Sloc : Source_Ptr;
      Scan_State     : Saved_Scan_State;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Protected_Sloc := Prev_Token_Ptr;

      if Token = Tok_Body then
         Scan; -- past BODY
         Name_Node := P_Defining_Identifier (C_Is);
         Scope.Table (Scope.Last).Labl := Name_Node;

         if Token = Tok_Left_Paren then
            Error_Msg_SC ("discriminant part not allowed in protected body");
            Discard_Junk_List (P_Known_Discriminant_Part_Opt);
         end if;

         TF_Is;

         --  Protected stub

         if Token = Tok_Separate then
            Scan; -- past SEPARATE
            Protected_Node := New_Node (N_Protected_Body_Stub, Protected_Sloc);
            Set_Defining_Identifier (Protected_Node, Name_Node);
            TF_Semicolon;
            Pop_Scope_Stack; -- remove unused entry

         --  Protected body

         else
            Protected_Node := New_Node (N_Protected_Body, Protected_Sloc);
            Set_Defining_Identifier (Protected_Node, Name_Node);
            Set_Declarations (Protected_Node, P_Protected_Operation_Items);
            End_Statements (Protected_Node);
         end if;

         return Protected_Node;

      --  Otherwise we must have a protected declaration

      else
         if Token = Tok_Type then
            Scan; -- past TYPE
            Protected_Node :=
              New_Node (N_Protected_Type_Declaration, Protected_Sloc);
            Name_Node := P_Defining_Identifier (C_Is);
            Set_Defining_Identifier (Protected_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;
            Set_Discriminant_Specifications
              (Protected_Node, P_Known_Discriminant_Part_Opt);

         else
            Protected_Node :=
              New_Node (N_Single_Protected_Declaration, Protected_Sloc);
            Name_Node := P_Defining_Identifier (C_Is);
            Set_Defining_Identifier (Protected_Node, Name_Node);

            if Token = Tok_Left_Paren then
               Error_Msg_SC
                 ("discriminant part not allowed for single protected");
               Discard_Junk_List (P_Known_Discriminant_Part_Opt);
            end if;

            Scope.Table (Scope.Last).Labl := Name_Node;
         end if;

         P_Aspect_Specifications (Protected_Node, Semicolon => False);

         --  Check for semicolon not followed by IS, this is something like

         --    protected type r;

         --  where we want

         --    protected type r IS END;

         if Token = Tok_Semicolon then
            Save_Scan_State (Scan_State); -- at semicolon
            Scan; -- past semicolon

            if Token /= Tok_Is then
               Restore_Scan_State (Scan_State);
               Error_Msg_SC -- CODEFIX
                 ("missing IS");
               Set_Protected_Definition (Protected_Node,
                 Make_Protected_Definition (Token_Ptr,
                   Visible_Declarations => Empty_List,
                   End_Label           => Empty));

               SIS_Entry_Active := False;
               End_Statements
                 (Protected_Definition (Protected_Node), Protected_Node);
               return Protected_Node;
            end if;

            Error_Msg_SP -- CODEFIX
              ("|extra ""("" ignored");
         end if;

         T_Is;

         --  Ada 2005 (AI-345)

         if Token = Tok_New then
            Scan; --  past NEW

            if Ada_Version < Ada_2005 then
               Error_Msg_SP ("protected interface is an Ada 2005 extension");
               Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
            end if;

            Set_Interface_List (Protected_Node, New_List);

            loop
               Append (P_Qualified_Simple_Name,
                 Interface_List (Protected_Node));

               exit when Token /= Tok_And;
               Scan; --  past AND
            end loop;

            if Token /= Tok_With then
               Error_Msg_SC -- CODEFIX
                 ("WITH expected");
            end if;

            Scan; -- past WITH
         end if;

         Set_Protected_Definition (Protected_Node, P_Protected_Definition);
         return Protected_Node;
      end if;
   end P_Protected;

   -------------------------------------
   -- 9.4  Protected Type Declaration --
   -------------------------------------

   --  Parsed by P_Protected (9.4)

   ---------------------------------------
   -- 9.4  Single Protected Declaration --
   ---------------------------------------

   --  Parsed by P_Protected (9.4)

   -------------------------------
   -- 9.4  Protected Definition --
   -------------------------------

   --  PROTECTED_DEFINITION ::=
   --      {PROTECTED_OPERATION_DECLARATION}
   --    [private
   --      {PROTECTED_ELEMENT_DECLARATION}]
   --    end [protected_IDENTIFIER]

   --  PROTECTED_ELEMENT_DECLARATION ::=
   --    PROTECTED_OPERATION_DECLARATION
   --  | COMPONENT_DECLARATION

   --  The caller has already established the scope stack entry

   --  Error recovery: cannot raise Error_Resync

   function P_Protected_Definition return Node_Id is
      Def_Node  : Node_Id;
      Item_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Protected_Definition, Token_Ptr);

      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;

      --  Loop to scan visible declarations (protected operation declarations)

      Set_Visible_Declarations (Def_Node, New_List);

      loop
         Item_Node := P_Protected_Operation_Declaration_Opt;
         exit when No (Item_Node);
         Append (Item_Node, Visible_Declarations (Def_Node));
      end loop;

      --  Deal with PRIVATE part (including graceful handling of multiple
      --  PRIVATE parts).

      Private_Loop : while Token = Tok_Private loop
         if No (Private_Declarations (Def_Node)) then
            Set_Private_Declarations (Def_Node, New_List);
         else
            Error_Msg_SC ("duplicate private part");
         end if;

         Scan; -- past PRIVATE

         Declaration_Loop : loop
            if Token = Tok_Identifier then
               P_Component_Items (Private_Declarations (Def_Node));
            else
               Item_Node := P_Protected_Operation_Declaration_Opt;
               exit Declaration_Loop when No (Item_Node);
               Append (Item_Node, Private_Declarations (Def_Node));
            end if;
         end loop Declaration_Loop;
      end loop Private_Loop;

      End_Statements (Def_Node);
      return Def_Node;
   end P_Protected_Definition;

   ------------------------------------------
   -- 9.4  Protected Operation Declaration --
   ------------------------------------------

   --  PROTECTED_OPERATION_DECLARATION ::=
   --    SUBPROGRAM_DECLARATION
   --  | ENTRY_DECLARATION
   --  | REPRESENTATION_CLAUSE

   --  Error recovery: cannot raise Error_Resync

   --  Note: a pragma can also be returned in this position

   --  We are not currently permitting representation clauses to appear as
   --  protected operation declarations, do we have to rethink this???

   function P_Protected_Operation_Declaration_Opt return Node_Id is
      L : List_Id;
      P : Source_Ptr;

      function P_Entry_Or_Subprogram_With_Indicator return Node_Id;
      --  Ada 2005 (AI-397): Parse an entry or a subprogram with an overriding
      --  indicator. The caller has checked that the initial token is NOT or
      --  OVERRIDING.

      ------------------------------------------
      -- P_Entry_Or_Subprogram_With_Indicator --
      ------------------------------------------

      function P_Entry_Or_Subprogram_With_Indicator return Node_Id is
         Decl           : Node_Id := Error;
         Is_Overriding  : Boolean := False;
         Not_Overriding : Boolean := False;

      begin
         if Token = Tok_Not then
            Scan;  -- past NOT

            if Token = Tok_Overriding then
               Scan;  -- past OVERRIDING
               Not_Overriding := True;
            else
               Error_Msg_SC -- CODEFIX
                 ("OVERRIDING expected!");
            end if;

         else
            Scan;  -- past OVERRIDING
            Is_Overriding := True;
         end if;

         if Is_Overriding or else Not_Overriding then
            if Ada_Version < Ada_2005 then
               Error_Msg_SP ("overriding indicator is an Ada 2005 extension");
               Error_Msg_SP ("\unit must be compiled with -gnat05 switch");

            elsif Token = Tok_Entry then
               Decl := P_Entry_Declaration;

               Set_Must_Override     (Decl, Is_Overriding);
               Set_Must_Not_Override (Decl, Not_Overriding);

            elsif Token = Tok_Function or else Token = Tok_Procedure then
               Decl := P_Subprogram (Pf_Decl_Pexp);

               Set_Must_Override     (Specification (Decl), Is_Overriding);
               Set_Must_Not_Override (Specification (Decl), Not_Overriding);

            else
               Error_Msg_SC -- CODEFIX
                 ("ENTRY, FUNCTION or PROCEDURE expected!");
            end if;
         end if;

         return Decl;
      end P_Entry_Or_Subprogram_With_Indicator;

   --  Start of processing for P_Protected_Operation_Declaration_Opt

   begin
      --  This loop runs more than once only when a junk declaration
      --  is skipped.

      loop
         if Token = Tok_Pragma then
            return P_Pragma;

         elsif Token = Tok_Not or else Token = Tok_Overriding then
            return P_Entry_Or_Subprogram_With_Indicator;

         elsif Token = Tok_Entry then
            return P_Entry_Declaration;

         elsif Token = Tok_Function or else Token = Tok_Procedure then
            return P_Subprogram (Pf_Decl_Pexp);

         elsif Token = Tok_Identifier then
            L := New_List;
            P := Token_Ptr;
            Skip_Declaration (L);

            if Nkind (First (L)) = N_Object_Declaration then
               Error_Msg
                 ("component must be declared in private part of " &
                  "protected type", P);
            else
               Error_Msg
                 ("illegal declaration in protected definition", P);
            end if;

         elsif Token in Token_Class_Declk then
            Error_Msg_SC ("illegal declaration in protected definition");
            Resync_Past_Semicolon;

            --  Return now to avoid cascaded messages if next declaration
            --  is a valid component declaration.

            return Error;

         elsif Token = Tok_For then
            Error_Msg_SC
              ("representation clause not allowed in protected definition");
            Resync_Past_Semicolon;

         else
            return Empty;
         end if;
      end loop;
   end P_Protected_Operation_Declaration_Opt;

   -----------------------------------
   -- 9.4  Protected Operation Item --
   -----------------------------------

   --  PROTECTED_OPERATION_ITEM ::=
   --    SUBPROGRAM_DECLARATION
   --  | SUBPROGRAM_BODY
   --  | ENTRY_BODY
   --  | REPRESENTATION_CLAUSE

   --  This procedure parses and returns a list of protected operation items

   --  We are not currently permitting representation clauses to appear
   --  as protected operation items, do we have to rethink this???

   function P_Protected_Operation_Items return List_Id is
      Item_List : List_Id;

   begin
      Item_List := New_List;

      loop
         if Token = Tok_Entry or else Bad_Spelling_Of (Tok_Entry) then
            Append (P_Entry_Body, Item_List);

         --  If the operation starts with procedure, function, or an overriding
         --  indicator ("overriding" or "not overriding"), parse a subprogram.

         elsif Token = Tok_Function or else Bad_Spelling_Of (Tok_Function)
                 or else
               Token = Tok_Procedure or else Bad_Spelling_Of (Tok_Procedure)
                 or else
               Token = Tok_Overriding or else Bad_Spelling_Of (Tok_Overriding)
                 or else
               Token = Tok_Not or else Bad_Spelling_Of (Tok_Not)
         then
            Append (P_Subprogram (Pf_Decl_Pbod_Pexp), Item_List);

         elsif Token = Tok_Pragma or else Bad_Spelling_Of (Tok_Pragma) then
            P_Pragmas_Opt (Item_List);

         elsif Token = Tok_Private or else Bad_Spelling_Of (Tok_Private) then
            Error_Msg_SC ("PRIVATE not allowed in protected body");
            Scan; -- past PRIVATE

         elsif Token = Tok_Identifier then
            Error_Msg_SC ("all components must be declared in spec!");
            Resync_Past_Semicolon;

         elsif Token in Token_Class_Declk then
            Error_Msg_SC ("this declaration not allowed in protected body");
            Resync_Past_Semicolon;

         else
            exit;
         end if;
      end loop;

      return Item_List;
   end P_Protected_Operation_Items;

   ------------------------------
   -- 9.5.2  Entry Declaration --
   ------------------------------

   --  ENTRY_DECLARATION ::=
   --    [OVERRIDING_INDICATOR]
   --    entry DEFINING_IDENTIFIER [(DISCRETE_SUBTYPE_DEFINITION)]
   --      PARAMETER_PROFILE;
   --        [ASPECT_SPECIFICATIONS];

   --  The caller has checked that the initial token is ENTRY, NOT or
   --  OVERRIDING.

   --  Error recovery: cannot raise Error_Resync

   function P_Entry_Declaration return Node_Id is
      Decl_Node  : Node_Id;
      Scan_State : Saved_Scan_State;

      --  Flags for optional overriding indication. Two flags are needed,
      --  to distinguish positive and negative overriding indicators from
      --  the absence of any indicator.

      Is_Overriding  : Boolean := False;
      Not_Overriding : Boolean := False;

   begin
      --  Ada 2005 (AI-397): Scan leading overriding indicator

      if Token = Tok_Not then
         Scan;  -- past NOT

         if Token = Tok_Overriding then
            Scan;  -- part OVERRIDING
            Not_Overriding := True;
         else
            Error_Msg_SC -- CODEFIX
              ("OVERRIDING expected!");
         end if;

      elsif Token = Tok_Overriding then
         Scan;  -- part OVERRIDING
         Is_Overriding := True;
      end if;

      if Is_Overriding or else Not_Overriding then
         if Ada_Version < Ada_2005 then
            Error_Msg_SP ("overriding indicator is an Ada 2005 extension");
            Error_Msg_SP ("\unit must be compiled with -gnat05 switch");

         elsif Token /= Tok_Entry then
            Error_Msg_SC -- CODEFIX
              ("ENTRY expected!");
         end if;
      end if;

      Decl_Node := New_Node (N_Entry_Declaration, Token_Ptr);
      Scan; -- past ENTRY

      Set_Defining_Identifier
        (Decl_Node, P_Defining_Identifier (C_Left_Paren_Semicolon));

      --  If left paren, could be (Discrete_Subtype_Definition) or Formal_Part

      if Token = Tok_Left_Paren then
         Scan; -- past (

         --  If identifier after left paren, could still be either

         if Token = Tok_Identifier then
            Save_Scan_State (Scan_State); -- at Id
            Scan; -- past Id

            --  If comma or colon after Id, must be Formal_Part

            if Token = Tok_Comma or else Token = Tok_Colon then
               Restore_Scan_State (Scan_State); -- to Id
               Set_Parameter_Specifications (Decl_Node, P_Formal_Part);

            --  Else if Id without comma or colon, must be discrete subtype
            --  defn

            else
               Restore_Scan_State (Scan_State); -- to Id
               Set_Discrete_Subtype_Definition
                 (Decl_Node, P_Discrete_Subtype_Definition);
               T_Right_Paren;
               Set_Parameter_Specifications (Decl_Node, P_Parameter_Profile);
            end if;

         --  If no Id, must be discrete subtype definition

         else
            Set_Discrete_Subtype_Definition
              (Decl_Node, P_Discrete_Subtype_Definition);
            T_Right_Paren;
            Set_Parameter_Specifications (Decl_Node, P_Parameter_Profile);
         end if;
      end if;

      if Is_Overriding then
         Set_Must_Override (Decl_Node);
      elsif Not_Overriding then
         Set_Must_Not_Override (Decl_Node);
      end if;

      --  Error recovery check for illegal return

      if Token = Tok_Return then
         Error_Msg_SC ("entry cannot have return value!");
         Scan;
         Discard_Junk_Node (P_Subtype_Indication);
      end if;

      --  Error recovery check for improper use of entry barrier in spec

      if Token = Tok_When then
         Error_Msg_SC ("barrier not allowed here (belongs in body)");
         Scan; -- past WHEN;
         Discard_Junk_Node (P_Expression_No_Right_Paren);
      end if;

      P_Aspect_Specifications (Decl_Node);
      return Decl_Node;

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         return Error;
   end P_Entry_Declaration;

   -----------------------------
   -- 9.5.2  Accept Statement --
   -----------------------------

   --  ACCEPT_STATEMENT ::=
   --    accept entry_DIRECT_NAME
   --      [(ENTRY_INDEX)] PARAMETER_PROFILE [do
   --        HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [entry_IDENTIFIER]];

   --  The caller has checked that the initial token is ACCEPT

   --  Error recovery: cannot raise Error_Resync. If an error occurs, the
   --  scan is resynchronized past the next semicolon and control returns.

   function P_Accept_Statement return Node_Id is
      Scan_State  : Saved_Scan_State;
      Accept_Node : Node_Id;
      Hand_Seq    : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Ecol := Start_Column;

      Accept_Node := New_Node (N_Accept_Statement, Token_Ptr);
      Scan; -- past ACCEPT
      Scope.Table (Scope.Last).Labl := Token_Node;

      Set_Entry_Direct_Name (Accept_Node, P_Identifier (C_Do));

      --  Left paren could be (Entry_Index) or Formal_Part, determine which

      if Token = Tok_Left_Paren then
         Save_Scan_State (Scan_State); -- at left paren
         Scan; -- past left paren

         --  If first token after left paren not identifier, then Entry_Index

         if Token /= Tok_Identifier then
            Set_Entry_Index (Accept_Node, P_Expression);
            T_Right_Paren;
            Set_Parameter_Specifications (Accept_Node, P_Parameter_Profile);

         --  First token after left paren is identifier, could be either case

         else -- Token = Tok_Identifier
            Scan; -- past identifier

            --  If identifier followed by comma or colon, must be Formal_Part

            if Token = Tok_Comma or else Token = Tok_Colon then
               Restore_Scan_State (Scan_State); -- to left paren
               Set_Parameter_Specifications (Accept_Node, P_Parameter_Profile);

            --  If identifier not followed by comma/colon, must be entry index

            else
               Restore_Scan_State (Scan_State); -- to left paren
               Scan; -- past left paren (again!)
               Set_Entry_Index (Accept_Node, P_Expression);
               T_Right_Paren;
               Set_Parameter_Specifications (Accept_Node, P_Parameter_Profile);
            end if;
         end if;
      end if;

      --  Scan out DO if present

      if Token = Tok_Do then
         Scope.Table (Scope.Last).Etyp := E_Name;
         Scope.Table (Scope.Last).Lreq := False;
         Scan; -- past DO
         Hand_Seq := P_Handled_Sequence_Of_Statements;
         Set_Handled_Statement_Sequence (Accept_Node, Hand_Seq);
         End_Statements (Handled_Statement_Sequence (Accept_Node));

         --  Exception handlers not allowed in Ada 95 node

         if Present (Exception_Handlers (Hand_Seq)) then
            if Ada_Version = Ada_83 then
               Error_Msg_N
                 ("(Ada 83) exception handlers in accept not allowed",
                  First_Non_Pragma (Exception_Handlers (Hand_Seq)));
            end if;
         end if;

      else
         Pop_Scope_Stack; -- discard unused entry
         TF_Semicolon;
      end if;

      return Accept_Node;

   --  If error, resynchronize past semicolon

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         Pop_Scope_Stack; -- discard unused entry
         return Error;

   end P_Accept_Statement;

   ------------------------
   -- 9.5.2  Entry Index --
   ------------------------

   --  Parsed by P_Expression (4.4)

   -----------------------
   -- 9.5.2  Entry Body --
   -----------------------

   --  ENTRY_BODY ::=
   --    entry DEFINING_IDENTIFIER ENTRY_BODY_FORMAL_PART ENTRY_BARRIER is
   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [entry_IDENTIFIER];

   --  The caller has checked that the initial token is ENTRY

   --  Error_Recovery: cannot raise Error_Resync

   function P_Entry_Body return Node_Id is
      Entry_Node       : Node_Id;
      Formal_Part_Node : Node_Id;
      Name_Node        : Node_Id;

   begin
      Push_Scope_Stack;
      Entry_Node := New_Node (N_Entry_Body, Token_Ptr);
      Scan; -- past ENTRY

      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;

      Name_Node := P_Defining_Identifier;
      Set_Defining_Identifier (Entry_Node, Name_Node);
      Scope.Table (Scope.Last).Labl := Name_Node;

      Formal_Part_Node := P_Entry_Body_Formal_Part;
      Set_Entry_Body_Formal_Part (Entry_Node, Formal_Part_Node);

      Set_Condition (Formal_Part_Node, P_Entry_Barrier);
      Parse_Decls_Begin_End (Entry_Node);
      return Entry_Node;
   end P_Entry_Body;

   -----------------------------------
   -- 9.5.2  Entry Body Formal Part --
   -----------------------------------

   --  ENTRY_BODY_FORMAL_PART ::=
   --    [(ENTRY_INDEX_SPECIFICATION)] [PARAMETER_PART]

   --  Error_Recovery: cannot raise Error_Resync

   function P_Entry_Body_Formal_Part return Node_Id is
      Fpart_Node : Node_Id;
      Scan_State : Saved_Scan_State;

   begin
      Fpart_Node := New_Node (N_Entry_Body_Formal_Part, Token_Ptr);

      --  See if entry index specification present, and if so parse it

      if Token = Tok_Left_Paren then
         Save_Scan_State (Scan_State); -- at left paren
         Scan; -- past left paren

         if Token = Tok_For then
            Set_Entry_Index_Specification
              (Fpart_Node, P_Entry_Index_Specification);
            T_Right_Paren;
         else
            Restore_Scan_State (Scan_State); -- to left paren
         end if;

      --  Check for (common?) case of left paren omitted before FOR. This
      --  is a tricky case, because the corresponding missing left paren
      --  can cause real havoc if a formal part is present which gets
      --  treated as part of the discrete subtype definition of the
      --  entry index specification, so just give error and resynchronize

      elsif Token = Tok_For then
         T_Left_Paren; -- to give error message
         Resync_To_When;
      end if;

      Set_Parameter_Specifications (Fpart_Node, P_Parameter_Profile);
      return Fpart_Node;
   end P_Entry_Body_Formal_Part;

   --------------------------
   -- 9.5.2  Entry Barrier --
   --------------------------

   --  ENTRY_BARRIER ::= when CONDITION

   --  Error_Recovery: cannot raise Error_Resync

   function P_Entry_Barrier return Node_Id is
      Bnode : Node_Id;

   begin
      if Token = Tok_When then
         Scan; -- past WHEN;
         Bnode := P_Expression_No_Right_Paren;

         if Token = Tok_Colon_Equal then
            Error_Msg_SC -- CODEFIX
              ("|"":="" should be ""=""");
            Scan;
            Bnode := P_Expression_No_Right_Paren;
         end if;

      else
         T_When; -- to give error message
         Bnode := Error;
      end if;

      TF_Is;
      return Bnode;
   end P_Entry_Barrier;

   --------------------------------------
   -- 9.5.2  Entry Index Specification --
   --------------------------------------

   --  ENTRY_INDEX_SPECIFICATION ::=
   --    for DEFINING_IDENTIFIER in DISCRETE_SUBTYPE_DEFINITION

   --  Error recovery: can raise Error_Resync

   function P_Entry_Index_Specification return Node_Id is
      Iterator_Node : Node_Id;

   begin
      Iterator_Node := New_Node (N_Entry_Index_Specification, Token_Ptr);
      T_For; -- past FOR
      Set_Defining_Identifier (Iterator_Node, P_Defining_Identifier (C_In));
      T_In;
      Set_Discrete_Subtype_Definition
        (Iterator_Node, P_Discrete_Subtype_Definition);
      return Iterator_Node;
   end P_Entry_Index_Specification;

   ---------------------------------
   -- 9.5.3  Entry Call Statement --
   ---------------------------------

   --  Parsed by P_Name (4.1). Within a select, an entry call is parsed
   --  by P_Select_Statement (9.7)

   ------------------------------
   -- 9.5.4  Requeue Statement --
   ------------------------------

   --  REQUEUE_STATEMENT ::= requeue entry_NAME [with abort];

   --  The caller has checked that the initial token is requeue

   --  Error recovery: can raise Error_Resync

   function P_Requeue_Statement return Node_Id is
      Requeue_Node : Node_Id;

   begin
      Requeue_Node := New_Node (N_Requeue_Statement, Token_Ptr);
      Scan; -- past REQUEUE
      Set_Name (Requeue_Node, P_Name);

      if Token = Tok_With then
         Scan; -- past WITH
         T_Abort;
         Set_Abort_Present (Requeue_Node, True);
      end if;

      TF_Semicolon;
      return Requeue_Node;
   end P_Requeue_Statement;

   --------------------------
   -- 9.6  Delay Statement --
   --------------------------

   --  DELAY_STATEMENT ::=
   --    DELAY_UNTIL_STATEMENT
   --  | DELAY_RELATIVE_STATEMENT

   --  The caller has checked that the initial token is DELAY

   --  Error recovery: cannot raise Error_Resync

   function P_Delay_Statement return Node_Id is
   begin
      Scan; -- past DELAY

      --  The following check for delay until misused in Ada 83 doesn't catch
      --  all cases, but it's good enough to catch most of them!

      if Token_Name = Name_Until then
         Check_95_Keyword (Tok_Until, Tok_Left_Paren);
         Check_95_Keyword (Tok_Until, Tok_Identifier);
      end if;

      if Token = Tok_Until then
         return P_Delay_Until_Statement;
      else
         return P_Delay_Relative_Statement;
      end if;
   end P_Delay_Statement;

   --------------------------------
   -- 9.6  Delay Until Statement --
   --------------------------------

   --  DELAY_UNTIL_STATEMENT ::= delay until delay_EXPRESSION;

   --  The caller has checked that the initial token is DELAY, scanned it
   --  out and checked that the current token is UNTIL

   --  Error recovery: cannot raise Error_Resync

   function P_Delay_Until_Statement return Node_Id is
      Delay_Node : Node_Id;

   begin
      Delay_Node := New_Node (N_Delay_Until_Statement, Prev_Token_Ptr);
      Scan; -- past UNTIL
      Set_Expression (Delay_Node, P_Expression_No_Right_Paren);
      TF_Semicolon;
      return Delay_Node;
   end P_Delay_Until_Statement;

   -----------------------------------
   -- 9.6  Delay Relative Statement --
   -----------------------------------

   --  DELAY_RELATIVE_STATEMENT ::= delay delay_EXPRESSION;

   --  The caller has checked that the initial token is DELAY, scanned it
   --  out and determined that the current token is not UNTIL

   --  Error recovery: cannot raise Error_Resync

   function P_Delay_Relative_Statement return Node_Id is
      Delay_Node : Node_Id;

   begin
      Delay_Node := New_Node (N_Delay_Relative_Statement, Prev_Token_Ptr);
      Set_Expression (Delay_Node, P_Expression_No_Right_Paren);
      Check_Simple_Expression_In_Ada_83 (Expression (Delay_Node));
      TF_Semicolon;
      return Delay_Node;
   end P_Delay_Relative_Statement;

   ---------------------------
   -- 9.7  Select Statement --
   ---------------------------

   --  SELECT_STATEMENT ::=
   --    SELECTIVE_ACCEPT
   --  | TIMED_ENTRY_CALL
   --  | CONDITIONAL_ENTRY_CALL
   --  | ASYNCHRONOUS_SELECT

   --  SELECTIVE_ACCEPT ::=
   --    select
   --      [GUARD]
   --        SELECT_ALTERNATIVE
   --    {or
   --      [GUARD]
   --        SELECT_ALTERNATIVE
   --    [else
   --      SEQUENCE_OF_STATEMENTS]
   --    end select;

   --  GUARD ::= when CONDITION =>

   --  Note: the guard preceding a select alternative is included as part
   --  of the node generated for a selective accept alternative.

   --  SELECT_ALTERNATIVE ::=
   --    ACCEPT_ALTERNATIVE
   --  | DELAY_ALTERNATIVE
   --  | TERMINATE_ALTERNATIVE

   --  TIMED_ENTRY_CALL ::=
   --    select
   --      ENTRY_CALL_ALTERNATIVE
   --    or
   --      DELAY_ALTERNATIVE
   --    end select;

   --  CONDITIONAL_ENTRY_CALL ::=
   --    select
   --      ENTRY_CALL_ALTERNATIVE
   --    else
   --      SEQUENCE_OF_STATEMENTS
   --    end select;

   --  ENTRY_CALL_ALTERNATIVE ::=
   --    ENTRY_CALL_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  ASYNCHRONOUS_SELECT ::=
   --    select
   --      TRIGGERING_ALTERNATIVE
   --    then abort
   --      ABORTABLE_PART
   --    end select;

   --  TRIGGERING_ALTERNATIVE ::=
   --    TRIGGERING_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  TRIGGERING_STATEMENT ::= ENTRY_CALL_STATEMENT | DELAY_STATEMENT

   --  The caller has checked that the initial token is SELECT

   --  Error recovery: can raise Error_Resync

   function P_Select_Statement return Node_Id is
      Select_Node    : Node_Id;
      Select_Sloc    : Source_Ptr;
      Stmnt_Sloc     : Source_Ptr;
      Ecall_Node     : Node_Id;
      Alternative    : Node_Id;
      Select_Pragmas : List_Id;
      Alt_Pragmas    : List_Id;
      Statement_List : List_Id;
      Alt_List       : List_Id;
      Cond_Expr      : Node_Id;
      Delay_Stmnt    : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Select;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Labl := Error;

      Select_Sloc := Token_Ptr;
      Scan; -- past SELECT
      Stmnt_Sloc := Token_Ptr;
      Select_Pragmas := P_Pragmas_Opt;

      --  If first token after select is designator, then we have an entry
      --  call, which must be the start of a conditional entry call, timed
      --  entry call or asynchronous select

      if Token in Token_Class_Desig then

         --  Scan entry call statement

         begin
            Ecall_Node := P_Name;

            --  ??  The following two clauses exactly parallel code in ch5
            --      and should be combined sometime

            if Nkind (Ecall_Node) = N_Indexed_Component then
               declare
                  Prefix_Node : constant Node_Id := Prefix (Ecall_Node);
                  Exprs_Node  : constant List_Id := Expressions (Ecall_Node);

               begin
                  Change_Node (Ecall_Node, N_Procedure_Call_Statement);
                  Set_Name (Ecall_Node, Prefix_Node);
                  Set_Parameter_Associations (Ecall_Node, Exprs_Node);
               end;

            elsif Nkind (Ecall_Node) = N_Function_Call then
               declare
                  Fname_Node  : constant Node_Id := Name (Ecall_Node);
                  Params_List : constant List_Id :=
                                  Parameter_Associations (Ecall_Node);

               begin
                  Change_Node (Ecall_Node, N_Procedure_Call_Statement);
                  Set_Name (Ecall_Node, Fname_Node);
                  Set_Parameter_Associations (Ecall_Node, Params_List);
               end;

            elsif Nkind (Ecall_Node) = N_Identifier
              or else Nkind (Ecall_Node) = N_Selected_Component
            then
               --  Case of a call to a parameterless entry

               declare
                  C_Node : constant Node_Id :=
                         New_Node (N_Procedure_Call_Statement, Stmnt_Sloc);
               begin
                  Set_Name (C_Node, Ecall_Node);
                  Set_Parameter_Associations (C_Node, No_List);
                  Ecall_Node := C_Node;
               end;
            end if;

            TF_Semicolon;

         exception
            when Error_Resync =>
               Resync_Past_Semicolon;
               return Error;
         end;

         Statement_List := P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm);

         --  OR follows, we have a timed entry call

         if Token = Tok_Or then
            Scan; -- past OR
            Alt_Pragmas := P_Pragmas_Opt;

            Select_Node := New_Node (N_Timed_Entry_Call, Select_Sloc);
            Set_Entry_Call_Alternative (Select_Node,
              Make_Entry_Call_Alternative (Stmnt_Sloc,
                Entry_Call_Statement => Ecall_Node,
                Pragmas_Before       => Select_Pragmas,
                Statements           => Statement_List));

            --  Only possibility is delay alternative. If we have anything
            --  else, give message, and treat as conditional entry call.

            if Token /= Tok_Delay then
               Error_Msg_SC
                 ("only allowed alternative in timed entry call is delay!");
               Discard_Junk_List (P_Sequence_Of_Statements (SS_Sreq));
               Set_Delay_Alternative (Select_Node, Error);

            else
               Set_Delay_Alternative (Select_Node, P_Delay_Alternative);
               Set_Pragmas_Before
                 (Delay_Alternative (Select_Node), Alt_Pragmas);
            end if;

         --  ELSE follows, we have a conditional entry call

         elsif Token = Tok_Else then
            Scan; -- past ELSE
            Select_Node := New_Node (N_Conditional_Entry_Call, Select_Sloc);

            Set_Entry_Call_Alternative (Select_Node,
              Make_Entry_Call_Alternative (Stmnt_Sloc,
                Entry_Call_Statement => Ecall_Node,
                Pragmas_Before       => Select_Pragmas,
                Statements           => Statement_List));

            Set_Else_Statements
              (Select_Node, P_Sequence_Of_Statements (SS_Sreq));

         --  Only remaining case is THEN ABORT (asynchronous select)

         elsif Token = Tok_Abort then
            Select_Node :=
              Make_Asynchronous_Select (Select_Sloc,
                Triggering_Alternative =>
                  Make_Triggering_Alternative (Stmnt_Sloc,
                    Triggering_Statement => Ecall_Node,
                    Pragmas_Before       => Select_Pragmas,
                    Statements           => Statement_List),
                Abortable_Part => P_Abortable_Part);

         --  Else error

         else
            if Ada_Version = Ada_83 then
               Error_Msg_BC ("OR or ELSE expected");
            else
               Error_Msg_BC ("OR or ELSE or THEN ABORT expected");
            end if;

            Select_Node := Error;
         end if;

         End_Statements;

      --  Here we have a selective accept or an asynchronous select (first
      --  token after SELECT is other than a designator token).

      else
         --  If we have delay with no guard, could be asynchronous select

         if Token = Tok_Delay then
            Delay_Stmnt := P_Delay_Statement;
            Statement_List := P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm);

            --  Asynchronous select

            if Token = Tok_Abort then
               Select_Node :=
                 Make_Asynchronous_Select (Select_Sloc,
                   Triggering_Alternative =>
                     Make_Triggering_Alternative (Stmnt_Sloc,
                       Triggering_Statement => Delay_Stmnt,
                       Pragmas_Before       => Select_Pragmas,
                       Statements           => Statement_List),
                     Abortable_Part => P_Abortable_Part);

               End_Statements;
               return Select_Node;

            --  Delay which was not an asynchronous select. Must be a selective
            --  accept, and since at least one accept statement is required,
            --  we must have at least one OR phrase present.

            else
               Alt_List := New_List (
                 Make_Delay_Alternative (Stmnt_Sloc,
                   Delay_Statement => Delay_Stmnt,
                   Pragmas_Before  => Select_Pragmas,
                   Statements      => Statement_List));
               T_Or;
               Alt_Pragmas := P_Pragmas_Opt;
            end if;

         --  If not a delay statement, then must be another possibility for
         --  a selective accept alternative, or perhaps a guard is present

         else
            Alt_List := New_List;
            Alt_Pragmas := Select_Pragmas;
         end if;

         Select_Node := New_Node (N_Selective_Accept, Select_Sloc);
         Set_Select_Alternatives (Select_Node, Alt_List);

         --  Scan out selective accept alternatives. On entry to this loop,
         --  we are just past a SELECT or OR token, and any pragmas that
         --  immediately follow the SELECT or OR are in Alt_Pragmas.

         loop
            if Token = Tok_When then

               if Present (Alt_Pragmas) then
                  Error_Msg_SC ("pragmas may not precede guard");
               end if;

               Scan; --  past WHEN
               Cond_Expr := P_Expression_No_Right_Paren;
               T_Arrow;
               Alt_Pragmas := P_Pragmas_Opt;

            else
               Cond_Expr := Empty;
            end if;

            if Token = Tok_Accept then
               Alternative := P_Accept_Alternative;

               --  Check for junk attempt at asynchronous select using
               --  an Accept alternative as the triggering statement

               if Token = Tok_Abort
                 and then Is_Empty_List (Alt_List)
                 and then No (Cond_Expr)
               then
                  Error_Msg
                    ("triggering statement must be entry call or delay",
                     Sloc (Alternative));
                  Scan; -- past junk ABORT
                  Discard_Junk_List (P_Sequence_Of_Statements (SS_Sreq));
                  End_Statements;
                  return Error;
               end if;

            elsif Token = Tok_Delay then
               Alternative := P_Delay_Alternative;

            elsif Token = Tok_Terminate then
               Alternative := P_Terminate_Alternative;

            else
               Error_Msg_SC
                 ("select alternative (ACCEPT, ABORT, DELAY) expected");
               Alternative := Error;

               if Token = Tok_Semicolon then
                  Scan; -- past junk semicolon
               end if;
            end if;

            --  THEN ABORT at this stage is just junk

            if Token = Tok_Abort then
               Error_Msg_SP ("misplaced `THEN ABORT`");
               Scan; -- past junk ABORT
               Discard_Junk_List (P_Sequence_Of_Statements (SS_Sreq));
               End_Statements;
               return Error;

            else
               if Alternative /= Error then
                  Set_Condition (Alternative, Cond_Expr);
                  Set_Pragmas_Before (Alternative, Alt_Pragmas);
                  Append (Alternative, Alt_List);
               end if;

               exit when Token /= Tok_Or;
            end if;

            T_Or;
            Alt_Pragmas := P_Pragmas_Opt;
         end loop;

         if Token = Tok_Else then
            Scan; -- past ELSE
            Set_Else_Statements
              (Select_Node, P_Sequence_Of_Statements (SS_Ortm_Sreq));

            if Token = Tok_Or then
               Error_Msg_SC ("select alternative cannot follow else part!");
            end if;
         end if;

         End_Statements;
      end if;

      return Select_Node;
   end P_Select_Statement;

   -----------------------------
   -- 9.7.1  Selective Accept --
   -----------------------------

   --  Parsed by P_Select_Statement (9.7)

   ------------------
   -- 9.7.1  Guard --
   ------------------

   --  Parsed by P_Select_Statement (9.7)

   -------------------------------
   -- 9.7.1  Select Alternative --
   -------------------------------

   --  SELECT_ALTERNATIVE ::=
   --    ACCEPT_ALTERNATIVE
   --  | DELAY_ALTERNATIVE
   --  | TERMINATE_ALTERNATIVE

   --  Note: the guard preceding a select alternative is included as part
   --  of the node generated for a selective accept alternative.

   --  Error recovery: cannot raise Error_Resync

   -------------------------------
   -- 9.7.1  Accept Alternative --
   -------------------------------

   --  ACCEPT_ALTERNATIVE ::=
   --    ACCEPT_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  Error_Recovery: Cannot raise Error_Resync

   --  Note: the caller is responsible for setting the Pragmas_Before
   --  field of the returned N_Terminate_Alternative node.

   function P_Accept_Alternative return Node_Id is
      Accept_Alt_Node : Node_Id;

   begin
      Accept_Alt_Node := New_Node (N_Accept_Alternative, Token_Ptr);
      Set_Accept_Statement (Accept_Alt_Node, P_Accept_Statement);

      --  Note: the reason that we accept THEN ABORT as a terminator for
      --  the sequence of statements is for error recovery which allows
      --  for misuse of an accept statement as a triggering statement.

      Set_Statements
        (Accept_Alt_Node, P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm));
      return Accept_Alt_Node;
   end P_Accept_Alternative;

   ------------------------------
   -- 9.7.1  Delay Alternative --
   ------------------------------

   --  DELAY_ALTERNATIVE ::=
   --    DELAY_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  Error_Recovery: Cannot raise Error_Resync

   --  Note: the caller is responsible for setting the Pragmas_Before
   --  field of the returned N_Terminate_Alternative node.

   function P_Delay_Alternative return Node_Id is
      Delay_Alt_Node : Node_Id;

   begin
      Delay_Alt_Node := New_Node (N_Delay_Alternative, Token_Ptr);
      Set_Delay_Statement (Delay_Alt_Node, P_Delay_Statement);

      --  Note: the reason that we accept THEN ABORT as a terminator for
      --  the sequence of statements is for error recovery which allows
      --  for misuse of an accept statement as a triggering statement.

      Set_Statements
        (Delay_Alt_Node, P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm));
      return Delay_Alt_Node;
   end P_Delay_Alternative;

   ----------------------------------
   -- 9.7.1  Terminate Alternative --
   ----------------------------------

   --  TERMINATE_ALTERNATIVE ::= terminate;

   --  Error_Recovery: Cannot raise Error_Resync

   --  Note: the caller is responsible for setting the Pragmas_Before
   --  field of the returned N_Terminate_Alternative node.

   function P_Terminate_Alternative return Node_Id is
      Terminate_Alt_Node : Node_Id;

   begin
      Terminate_Alt_Node := New_Node (N_Terminate_Alternative, Token_Ptr);
      Scan; -- past TERMINATE
      TF_Semicolon;

      --  For all other select alternatives, the sequence of statements
      --  after the alternative statement will swallow up any pragmas
      --  coming in this position. But the terminate alternative has no
      --  sequence of statements, so the pragmas here must be treated
      --  specially.

      Set_Pragmas_After (Terminate_Alt_Node, P_Pragmas_Opt);
      return Terminate_Alt_Node;
   end P_Terminate_Alternative;

   -----------------------------
   -- 9.7.2  Timed Entry Call --
   -----------------------------

   --  Parsed by P_Select_Statement (9.7)

   -----------------------------------
   -- 9.7.2  Entry Call Alternative --
   -----------------------------------

   --  Parsed by P_Select_Statement (9.7)

   -----------------------------------
   -- 9.7.3  Conditional Entry Call --
   -----------------------------------

   --  Parsed by P_Select_Statement (9.7)

   --------------------------------
   -- 9.7.4  Asynchronous Select --
   --------------------------------

   --  Parsed by P_Select_Statement (9.7)

   -----------------------------------
   -- 9.7.4  Triggering Alternative --
   -----------------------------------

   --  Parsed by P_Select_Statement (9.7)

   ---------------------------------
   -- 9.7.4  Triggering Statement --
   ---------------------------------

   --  Parsed by P_Select_Statement (9.7)

   ---------------------------
   -- 9.7.4  Abortable Part --
   ---------------------------

   --  ABORTABLE_PART ::= SEQUENCE_OF_STATEMENTS

   --  The caller has verified that THEN ABORT is present, and Token is
   --  pointing to the ABORT on entry (or if not, then we have an error)

   --  Error recovery: cannot raise Error_Resync

   function P_Abortable_Part return Node_Id is
      Abortable_Part_Node : Node_Id;

   begin
      Abortable_Part_Node := New_Node (N_Abortable_Part, Token_Ptr);
      T_Abort; -- scan past ABORT

      if Ada_Version = Ada_83 then
         Error_Msg_SP ("(Ada 83) asynchronous select not allowed!");
      end if;

      Set_Statements (Abortable_Part_Node, P_Sequence_Of_Statements (SS_Sreq));
      return Abortable_Part_Node;
   end P_Abortable_Part;

   --------------------------
   -- 9.8  Abort Statement --
   --------------------------

   --  ABORT_STATEMENT ::= abort task_NAME {, task_NAME};

   --  The caller has checked that the initial token is ABORT

   --  Error recovery: cannot raise Error_Resync

   function P_Abort_Statement return Node_Id is
      Abort_Node : Node_Id;

   begin
      Abort_Node := New_Node (N_Abort_Statement, Token_Ptr);
      Scan; -- past ABORT
      Set_Names (Abort_Node, New_List);

      loop
         Append (P_Name, Names (Abort_Node));
         exit when Token /= Tok_Comma;
         Scan; -- past comma
      end loop;

      TF_Semicolon;
      return Abort_Node;
   end P_Abort_Statement;

end Ch9;
