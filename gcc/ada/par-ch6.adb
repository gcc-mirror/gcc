------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order
--  by RM section rather than alphabetical

with Sinfo.CN; use Sinfo.CN;

separate (Par)
package body Ch6 is

   --  Local subprograms, used only in this chapter

   function P_Defining_Designator        return Node_Id;
   function P_Defining_Operator_Symbol   return Node_Id;

   procedure Check_Junk_Semicolon_Before_Return;
   --  Check for common error of junk semicolon before RETURN keyword of
   --  function specification. If present, skip over it with appropriate
   --  error message, leaving Scan_Ptr pointing to the RETURN after. This
   --  routine also deals with a possibly misspelled version of Return.

   ----------------------------------------
   -- Check_Junk_Semicolon_Before_Return --
   ----------------------------------------

   procedure Check_Junk_Semicolon_Before_Return is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Semicolon then
         Save_Scan_State (Scan_State);
         Scan; -- past the semicolon

         if Token = Tok_Return then
            Restore_Scan_State (Scan_State);
            Error_Msg_SC ("Unexpected semicolon ignored");
            Scan; -- rescan past junk semicolon

         else
            Restore_Scan_State (Scan_State);
         end if;

      elsif Bad_Spelling_Of (Tok_Return) then
         null;
      end if;
   end Check_Junk_Semicolon_Before_Return;

   -----------------------------------------------------
   -- 6.1  Subprogram (Also 6.3, 8.5.4, 10.1.3, 12.3) --
   -----------------------------------------------------

   --  This routine scans out a subprogram declaration, subprogram body,
   --  subprogram renaming declaration or subprogram generic instantiation.

   --  SUBPROGRAM_DECLARATION ::= SUBPROGRAM_SPECIFICATION;

   --  ABSTRACT_SUBPROGRAM_DECLARATION ::=
   --    SUBPROGRAM_SPECIFICATION is abstract;

   --  SUBPROGRAM_SPECIFICATION ::=
   --      procedure DEFINING_PROGRAM_UNIT_NAME PARAMETER_PROFILE
   --    | function DEFINING_DESIGNATOR PARAMETER_AND_RESULT_PROFILE

   --  PARAMETER_PROFILE ::= [FORMAL_PART]

   --  PARAMETER_AND_RESULT_PROFILE ::= [FORMAL_PART] return SUBTYPE_MARK

   --  SUBPROGRAM_BODY ::=
   --    SUBPROGRAM_SPECIFICATION is
   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [DESIGNATOR];

   --  SUBPROGRAM_RENAMING_DECLARATION ::=
   --    SUBPROGRAM_SPECIFICATION renames callable_entity_NAME;

   --  SUBPROGRAM_BODY_STUB ::=
   --    SUBPROGRAM_SPECIFICATION is separate;

   --  GENERIC_INSTANTIATION ::=
   --    procedure DEFINING_PROGRAM_UNIT_NAME is
   --      new generic_procedure_NAME [GENERIC_ACTUAL_PART];
   --  | function DEFINING_DESIGNATOR is
   --      new generic_function_NAME [GENERIC_ACTUAL_PART];

   --  The value in Pf_Flags indicates which of these possible declarations
   --  is acceptable to the caller:

   --    Pf_Flags.Decl                 Set if declaration OK
   --    Pf_Flags.Gins                 Set if generic instantiation OK
   --    Pf_Flags.Pbod                 Set if proper body OK
   --    Pf_Flags.Rnam                 Set if renaming declaration OK
   --    Pf_Flags.Stub                 Set if body stub OK

   --  If an inappropriate form is encountered, it is scanned out but an
   --  error message indicating that it is appearing in an inappropriate
   --  context is issued. The only possible values for Pf_Flags are those
   --  defined as constants in the Par package.

   --  The caller has checked that the initial token is FUNCTION or PROCEDURE

   --  Error recovery: cannot raise Error_Resync

   function P_Subprogram (Pf_Flags : Pf_Rec) return Node_Id is
      Specification_Node : Node_Id;
      Name_Node   : Node_Id;
      Fpart_List  : List_Id;
      Fpart_Sloc  : Source_Ptr;
      Return_Node : Node_Id;
      Inst_Node   : Node_Id;
      Body_Node   : Node_Id;
      Decl_Node   : Node_Id;
      Rename_Node : Node_Id;
      Absdec_Node : Node_Id;
      Stub_Node   : Node_Id;
      Fproc_Sloc  : Source_Ptr;
      Func        : Boolean;
      Scan_State  : Saved_Scan_State;

   begin
      --  Set up scope stack entry. Note that the Labl field will be set later

      SIS_Entry_Active := False;
      SIS_Missing_Semicolon_Message := No_Error_Msg;
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;

      Func := (Token = Tok_Function);
      Fproc_Sloc := Token_Ptr;
      Scan; -- past FUNCTION or PROCEDURE
      Ignore (Tok_Type);
      Ignore (Tok_Body);

      if Func then
         Name_Node := P_Defining_Designator;

         if Nkind (Name_Node) = N_Defining_Operator_Symbol
           and then Scope.Last = 1
         then
            Error_Msg_SP ("operator symbol not allowed at library level");
            Name_Node := New_Entity (N_Defining_Identifier, Sloc (Name_Node));

            --  Set name from file name, we need some junk name, and that's
            --  as good as anything. This is only approximate, since we do
            --  not do anything with non-standard name translations.

            Get_Name_String (File_Name (Current_Source_File));

            for J in 1 .. Name_Len loop
               if Name_Buffer (J) = '.' then
                  Name_Len := J - 1;
                  exit;
               end if;
            end loop;

            Set_Chars (Name_Node, Name_Find);
            Set_Error_Posted (Name_Node);
         end if;

      else
         Name_Node := P_Defining_Program_Unit_Name;
      end if;

      Scope.Table (Scope.Last).Labl := Name_Node;

      if Token = Tok_Colon then
         Error_Msg_SC ("redundant colon ignored");
         Scan; -- past colon
      end if;

      --  Deal with generic instantiation, the one case in which we do not
      --  have a subprogram specification as part of whatever we are parsing

      if Token = Tok_Is then
         Save_Scan_State (Scan_State); -- at the IS
         T_Is; -- checks for redundant IS's

         if Token = Tok_New then
            if not Pf_Flags.Gins then
               Error_Msg_SC ("generic instantation not allowed here!");
            end if;

            Scan; -- past NEW

            if Func then
               Inst_Node := New_Node (N_Function_Instantiation, Fproc_Sloc);
               Set_Name (Inst_Node, P_Function_Name);
            else
               Inst_Node := New_Node (N_Procedure_Instantiation, Fproc_Sloc);
               Set_Name (Inst_Node, P_Qualified_Simple_Name);
            end if;

            Set_Defining_Unit_Name (Inst_Node, Name_Node);
            Set_Generic_Associations (Inst_Node, P_Generic_Actual_Part_Opt);
            TF_Semicolon;
            Pop_Scope_Stack; -- Don't need scope stack entry in this case
            return Inst_Node;

         else
            Restore_Scan_State (Scan_State); -- to the IS
         end if;
      end if;

      --  If not a generic instantiation, then we definitely have a subprogram
      --  specification (all possibilities at this stage include one here)

      Fpart_Sloc := Token_Ptr;

      Check_Misspelling_Of (Tok_Return);

      --  Scan formal part. First a special error check. If we have an
      --  identifier here, then we have a definite error. If this identifier
      --  is on the same line as the designator, then we assume it is the
      --  first formal after a missing left parenthesis

      if Token = Tok_Identifier
        and then not Token_Is_At_Start_Of_Line
      then
            T_Left_Paren; -- to generate message
            Fpart_List := P_Formal_Part;

      --  Otherwise scan out an optional formal part in the usual manner

      else
         Fpart_List := P_Parameter_Profile;
      end if;

      --  We treat what we have as a function specification if FUNCTION was
      --  used, or if a RETURN is present. This gives better error recovery
      --  since later RETURN statements will be valid in either case.

      Check_Junk_Semicolon_Before_Return;
      Return_Node := Error;

      if Token = Tok_Return then
         if not Func then
            Error_Msg ("PROCEDURE should be FUNCTION", Fproc_Sloc);
            Func := True;
         end if;

         Scan; -- past RETURN
         Return_Node := P_Subtype_Mark;
         No_Constraint;

      else
         if Func then
            Ignore (Tok_Right_Paren);
            TF_Return;
         end if;
      end if;

      if Func then
         Specification_Node :=
           New_Node (N_Function_Specification, Fproc_Sloc);
         Set_Subtype_Mark (Specification_Node, Return_Node);

      else
         Specification_Node :=
           New_Node (N_Procedure_Specification, Fproc_Sloc);
      end if;

      Set_Defining_Unit_Name (Specification_Node, Name_Node);
      Set_Parameter_Specifications (Specification_Node, Fpart_List);

      --  Error check: barriers not allowed on protected functions/procedures

      if Token = Tok_When then
         if Func then
            Error_Msg_SC ("barrier not allowed on function, only on entry");
         else
            Error_Msg_SC ("barrier not allowed on procedure, only on entry");
         end if;

         Scan; -- past WHEN
         Discard_Junk_Node (P_Expression);
      end if;

      --  Deal with case of semicolon ending a subprogram declaration

      if Token = Tok_Semicolon then
         if not Pf_Flags.Decl then
            T_Is;
         end if;

         Scan; -- past semicolon

         --  If semicolon is immediately followed by IS, then ignore the
         --  semicolon, and go process the body.

         if Token = Tok_Is then
            Error_Msg_SP ("unexpected semicolon ignored");
            T_Is; -- ignroe redundant IS's
            goto Subprogram_Body;

         --  If BEGIN follows in an appropriate column, we immediately
         --  commence the error action of assuming that the previous
         --  subprogram declaration should have been a subprogram body,
         --  i.e. that the terminating semicolon should have been IS.

         elsif Token = Tok_Begin
            and then Start_Column >= Scope.Table (Scope.Last).Ecol
         then
            Error_Msg_SP (""";"" should be IS!");
            goto Subprogram_Body;

         else
            goto Subprogram_Declaration;
         end if;

      --  Case of not followed by semicolon

      else
         --  Subprogram renaming declaration case

         Check_Misspelling_Of (Tok_Renames);

         if Token = Tok_Renames then
            if not Pf_Flags.Rnam then
               Error_Msg_SC ("renaming declaration not allowed here!");
            end if;

            Rename_Node :=
              New_Node (N_Subprogram_Renaming_Declaration, Token_Ptr);
            Scan; -- past RENAMES
            Set_Name (Rename_Node, P_Name);
            Set_Specification (Rename_Node, Specification_Node);
            TF_Semicolon;
            Pop_Scope_Stack;
            return Rename_Node;

         --  Case of IS following subprogram specification

         elsif Token = Tok_Is then
            T_Is; -- ignore redundant Is's

            if Token_Name = Name_Abstract then
               Check_95_Keyword (Tok_Abstract, Tok_Semicolon);
            end if;

            --  Deal nicely with (now obsolete) use of <> in place of abstract

            if Token = Tok_Box then
               Error_Msg_SC ("ABSTRACT expected");
               Token := Tok_Abstract;
            end if;

            --  Abstract subprogram declaration case

            if Token = Tok_Abstract then
               Absdec_Node :=
                 New_Node (N_Abstract_Subprogram_Declaration, Token_Ptr);
               Set_Specification (Absdec_Node, Specification_Node);
               Pop_Scope_Stack; -- discard unneeded entry
               Scan; -- past ABSTRACT
               TF_Semicolon;
               return Absdec_Node;

            --  Check for IS NEW with Formal_Part present and handle nicely

            elsif Token = Tok_New then
               Error_Msg
                 ("formal part not allowed in instantiation", Fpart_Sloc);
               Scan; -- past NEW

               if Func then
                  Inst_Node := New_Node (N_Function_Instantiation, Fproc_Sloc);
               else
                  Inst_Node :=
                    New_Node (N_Procedure_Instantiation, Fproc_Sloc);
               end if;

               Set_Defining_Unit_Name (Inst_Node, Name_Node);
               Set_Name (Inst_Node, P_Name);
               Set_Generic_Associations (Inst_Node, P_Generic_Actual_Part_Opt);
               TF_Semicolon;
               Pop_Scope_Stack; -- Don't need scope stack entry in this case
               return Inst_Node;

            else
               goto Subprogram_Body;
            end if;

         --  Here we have a missing IS or missing semicolon, we always guess
         --  a missing semicolon, since we are pretty good at fixing up a
         --  semicolon which should really be an IS

         else
            Error_Msg_AP ("missing "";""");
            SIS_Missing_Semicolon_Message := Get_Msg_Id;
            goto Subprogram_Declaration;
         end if;
      end if;

      --  Processing for subprogram body

      <<Subprogram_Body>>
         if not Pf_Flags.Pbod then
            Error_Msg_SP ("subprogram body not allowed here!");
         end if;

         --  Subprogram body stub case

         if Separate_Present then
            if not Pf_Flags.Stub then
               Error_Msg_SC ("body stub not allowed here!");
            end if;

            if Nkind (Name_Node) = N_Defining_Operator_Symbol then
               Error_Msg
                 ("operator symbol cannot be used as subunit name",
                  Sloc (Name_Node));
            end if;

            Stub_Node :=
              New_Node (N_Subprogram_Body_Stub, Sloc (Specification_Node));
            Set_Specification (Stub_Node, Specification_Node);
            Scan; -- past SEPARATE
            Pop_Scope_Stack;
            TF_Semicolon;
            return Stub_Node;

         --  Subprogram body case

         else
            --  Here is the test for a suspicious IS (i.e. one that looks
            --  like it might more properly be a semicolon). See separate
            --  section discussing use of IS instead of semicolon in
            --  package Parse.

            if (Token in Token_Class_Declk
                  or else
                Token = Tok_Identifier)
              and then Start_Column <= Scope.Table (Scope.Last).Ecol
              and then Scope.Last /= 1
            then
               Scope.Table (Scope.Last).Etyp := E_Suspicious_Is;
               Scope.Table (Scope.Last).S_Is := Prev_Token_Ptr;
            end if;

            Body_Node :=
              New_Node (N_Subprogram_Body, Sloc (Specification_Node));
            Set_Specification (Body_Node, Specification_Node);
            Parse_Decls_Begin_End (Body_Node);
            return Body_Node;
         end if;

      --  Processing for subprogram declaration

      <<Subprogram_Declaration>>
         Decl_Node :=
           New_Node (N_Subprogram_Declaration, Sloc (Specification_Node));
         Set_Specification (Decl_Node, Specification_Node);

         --  If this is a context in which a subprogram body is permitted,
         --  set active SIS entry in case (see section titled "Handling
         --  Semicolon Used in Place of IS" in body of Parser package)
         --  Note that SIS_Missing_Semicolon_Message is already set properly.

         if Pf_Flags.Pbod then
            SIS_Labl := Scope.Table (Scope.Last).Labl;
            SIS_Sloc := Scope.Table (Scope.Last).Sloc;
            SIS_Ecol := Scope.Table (Scope.Last).Ecol;
            SIS_Declaration_Node := Decl_Node;
            SIS_Semicolon_Sloc := Prev_Token_Ptr;
            SIS_Entry_Active := True;
         end if;

         Pop_Scope_Stack;
         return Decl_Node;

   end P_Subprogram;

   ---------------------------------
   -- 6.1  Subprogram Declaration --
   ---------------------------------

   --  Parsed by P_Subprogram (6.1)

   ------------------------------------------
   -- 6.1  Abstract Subprogram Declaration --
   ------------------------------------------

   --  Parsed by P_Subprogram (6.1)

   -----------------------------------
   -- 6.1  Subprogram Specification --
   -----------------------------------

   --  SUBPROGRAM_SPECIFICATION ::=
   --      procedure DEFINING_PROGRAM_UNIT_NAME PARAMETER_PROFILE
   --    | function DEFINING_DESIGNATOR PARAMETER_AND_RESULT_PROFILE

   --  PARAMETER_PROFILE ::= [FORMAL_PART]

   --  PARAMETER_AND_RESULT_PROFILE ::= [FORMAL_PART] return SUBTYPE_MARK

   --  Subprogram specifications that appear in subprogram declarations
   --  are parsed by P_Subprogram (6.1). This routine is used in other
   --  contexts where subprogram specifications occur.

   --  Note: this routine does not affect the scope stack in any way

   --  Error recovery: can raise Error_Resync

   function P_Subprogram_Specification return Node_Id is
      Specification_Node : Node_Id;

   begin
      if Token = Tok_Function then
         Specification_Node := New_Node (N_Function_Specification, Token_Ptr);
         Scan; -- past FUNCTION
         Ignore (Tok_Body);
         Set_Defining_Unit_Name (Specification_Node, P_Defining_Designator);
         Set_Parameter_Specifications
           (Specification_Node, P_Parameter_Profile);
         Check_Junk_Semicolon_Before_Return;
         TF_Return;
         Set_Subtype_Mark (Specification_Node, P_Subtype_Mark);
         No_Constraint;
         return Specification_Node;

      elsif Token = Tok_Procedure then
         Specification_Node := New_Node (N_Procedure_Specification, Token_Ptr);
         Scan; -- past PROCEDURE
         Ignore (Tok_Body);
         Set_Defining_Unit_Name
           (Specification_Node, P_Defining_Program_Unit_Name);
         Set_Parameter_Specifications
           (Specification_Node, P_Parameter_Profile);
         return Specification_Node;

      else
         Error_Msg_SC ("subprogram specification expected");
         raise Error_Resync;
      end if;
   end P_Subprogram_Specification;

   ---------------------
   -- 6.1  Designator --
   ---------------------

   --  DESIGNATOR ::=
   --    [PARENT_UNIT_NAME .] IDENTIFIER | OPERATOR_SYMBOL

   --  The caller has checked that the initial token is an identifier,
   --  operator symbol, or string literal. Note that we don't bother to
   --  do much error diagnosis in this routine, since it is only used for
   --  the label on END lines, and the routines in package Par.Endh will
   --  check that the label is appropriate.

   --  Error recovery: cannot raise Error_Resync

   function P_Designator return Node_Id is
      Ident_Node  : Node_Id;
      Name_Node   : Node_Id;
      Prefix_Node : Node_Id;

      function Real_Dot return Boolean;
      --  Tests if a current token is an interesting period, i.e. is followed
      --  by an identifier or operator symbol or string literal. If not, it is
      --  probably just incorrect punctuation to be caught by our caller. Note
      --  that the case of an operator symbol or string literal is also an
      --  error, but that is an error that we catch here. If the result is
      --  True, a real dot has been scanned and we are positioned past it,
      --  if the result is False, the scan position is unchanged.

      --------------
      -- Real_Dot --
      --------------

      function Real_Dot return Boolean is
         Scan_State  : Saved_Scan_State;

      begin
         if Token /= Tok_Dot then
            return False;

         else
            Save_Scan_State (Scan_State);
            Scan; -- past dot

            if Token = Tok_Identifier
              or else Token = Tok_Operator_Symbol
              or else Token = Tok_String_Literal
            then
               return True;

            else
               Restore_Scan_State (Scan_State);
               return False;
            end if;
         end if;
      end Real_Dot;

   --  Start of processing for P_Designator

   begin
      Ident_Node := Token_Node;
      Scan; -- past initial token

      if Prev_Token = Tok_Operator_Symbol
        or else Prev_Token = Tok_String_Literal
        or else not Real_Dot
      then
         return Ident_Node;

      --  Child name case

      else
         Prefix_Node := Ident_Node;

         --  Loop through child names, on entry to this loop, Prefix contains
         --  the name scanned so far, and Ident_Node is the last identifier.

         loop
            Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Name_Node, Prefix_Node);
            Ident_Node := P_Identifier;
            Set_Selector_Name (Name_Node, Ident_Node);
            Prefix_Node := Name_Node;
            exit when not Real_Dot;
         end loop;

         --  On exit from the loop, Ident_Node is the last identifier scanned,
         --  i.e. the defining identifier, and Prefix_Node is a node for the
         --  entire name, structured (incorrectly!) as a selected component.

         Name_Node := Prefix (Prefix_Node);
         Change_Node (Prefix_Node, N_Designator);
         Set_Name (Prefix_Node, Name_Node);
         Set_Identifier (Prefix_Node, Ident_Node);
         return Prefix_Node;
      end if;

   exception
      when Error_Resync =>
         while Token = Tok_Dot or else Token = Tok_Identifier loop
            Scan;
         end loop;

         return Error;
   end P_Designator;

   ------------------------------
   -- 6.1  Defining Designator --
   ------------------------------

   --  DEFINING_DESIGNATOR ::=
   --    DEFINING_PROGRAM_UNIT_NAME | DEFINING_OPERATOR_SYMBOL

   --  Error recovery: cannot raise Error_Resync

   function P_Defining_Designator return Node_Id is
   begin
      if Token = Tok_Operator_Symbol then
         return P_Defining_Operator_Symbol;

      elsif Token = Tok_String_Literal then
         Error_Msg_SC ("invalid operator name");
         Scan; -- past junk string
         return Error;

      else
         return P_Defining_Program_Unit_Name;
      end if;
   end P_Defining_Designator;

   -------------------------------------
   -- 6.1  Defining Program Unit Name --
   -------------------------------------

   --  DEFINING_PROGRAM_UNIT_NAME ::=
   --    [PARENT_UNIT_NAME .] DEFINING_IDENTIFIER

   --  Note: PARENT_UNIT_NAME may be present only in 95 mode at the outer level

   --  Error recovery: cannot raise Error_Resync

   function P_Defining_Program_Unit_Name return Node_Id is
      Ident_Node  : Node_Id;
      Name_Node   : Node_Id;
      Prefix_Node : Node_Id;

   begin
      --  Set identifier casing if not already set and scan initial identifier

      if Token = Tok_Identifier
        and then Identifier_Casing (Current_Source_File) = Unknown
      then
         Set_Identifier_Casing (Current_Source_File, Determine_Token_Casing);
      end if;

      Ident_Node := P_Identifier (C_Dot);
      Merge_Identifier (Ident_Node, Tok_Return);

      --  Normal case (not child library unit name)

      if Token /= Tok_Dot then
         Change_Identifier_To_Defining_Identifier (Ident_Node);
         return Ident_Node;

      --  Child library unit name case

      else
         if Scope.Last > 1 then
            Error_Msg_SP ("child unit allowed only at library level");
            raise Error_Resync;

         elsif Ada_83 then
            Error_Msg_SP ("(Ada 83) child unit not allowed!");

         end if;

         Prefix_Node := Ident_Node;

         --  Loop through child names, on entry to this loop, Prefix contains
         --  the name scanned so far, and Ident_Node is the last identifier.

         loop
            exit when Token /= Tok_Dot;
            Name_Node := New_Node (N_Selected_Component, Token_Ptr);
            Scan; -- past period
            Set_Prefix (Name_Node, Prefix_Node);
            Ident_Node := P_Identifier (C_Dot);
            Set_Selector_Name (Name_Node, Ident_Node);
            Prefix_Node := Name_Node;
         end loop;

         --  On exit from the loop, Ident_Node is the last identifier scanned,
         --  i.e. the defining identifier, and Prefix_Node is a node for the
         --  entire name, structured (incorrectly!) as a selected component.

         Name_Node := Prefix (Prefix_Node);
         Change_Node (Prefix_Node, N_Defining_Program_Unit_Name);
         Set_Name (Prefix_Node, Name_Node);
         Change_Identifier_To_Defining_Identifier (Ident_Node);
         Set_Defining_Identifier (Prefix_Node, Ident_Node);

         --  All set with unit name parsed

         return Prefix_Node;
      end if;

   exception
      when Error_Resync =>
         while Token = Tok_Dot or else Token = Tok_Identifier loop
            Scan;
         end loop;

         return Error;
   end P_Defining_Program_Unit_Name;

   --------------------------
   -- 6.1  Operator Symbol --
   --------------------------

   --  OPERATOR_SYMBOL ::= STRING_LITERAL

   --  Operator symbol is returned by the scanner as Tok_Operator_Symbol

   -----------------------------------
   -- 6.1  Defining Operator Symbol --
   -----------------------------------

   --  DEFINING_OPERATOR_SYMBOL ::= OPERATOR_SYMBOL

   --  The caller has checked that the initial symbol is an operator symbol

   function P_Defining_Operator_Symbol return Node_Id is
      Op_Node : Node_Id;

   begin
      Op_Node := Token_Node;
      Change_Operator_Symbol_To_Defining_Operator_Symbol (Op_Node);
      Scan; -- past operator symbol
      return Op_Node;
   end P_Defining_Operator_Symbol;

   ----------------------------
   -- 6.1  Parameter_Profile --
   ----------------------------

   --  PARAMETER_PROFILE ::= [FORMAL_PART]

   --  Empty is returned if no formal part is present

   --  Error recovery: cannot raise Error_Resync

   function P_Parameter_Profile return List_Id is
   begin
      if Token = Tok_Left_Paren then
         Scan; -- part left paren
         return P_Formal_Part;
      else
         return No_List;
      end if;
   end P_Parameter_Profile;

   ---------------------------------------
   -- 6.1  Parameter And Result Profile --
   ---------------------------------------

   --  Parsed by its parent construct, which uses P_Parameter_Profile to
   --  parse the parameters, and P_Subtype_Mark to parse the return type.

   ----------------------
   -- 6.1  Formal part --
   ----------------------

   --  FORMAL_PART ::= (PARAMETER_SPECIFICATION {; PARAMETER_SPECIFICATION})

   --  PARAMETER_SPECIFICATION ::=
   --    DEFINING_IDENTIFIER_LIST : MODE SUBTYPE_MARK
   --      [:= DEFAULT_EXPRESSION]
   --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
   --      [:= DEFAULT_EXPRESSION]

   --  This scans the construct Formal_Part. The caller has already checked
   --  that the initial token is a left parenthesis, and skipped past it, so
   --  that on entry Token is the first token following the left parenthesis.

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Part return List_Id is
      Specification_List : List_Id;
      Specification_Node : Node_Id;
      Scan_State         : Saved_Scan_State;
      Num_Idents         : Nat;
      Ident              : Nat;
      Ident_Sloc         : Source_Ptr;

      Idents : array (Int range 1 .. 4096) of Entity_Id;
      --  This array holds the list of defining identifiers. The upper bound
      --  of 4096 is intended to be essentially infinite, and we do not even
      --  bother to check for it being exceeded.

   begin
      Specification_List := New_List;

      Specification_Loop : loop
         begin
            if Token = Tok_Pragma then
               P_Pragmas_Misplaced;
            end if;

            Ignore (Tok_Left_Paren);
            Ident_Sloc := Token_Ptr;
            Idents (1) := P_Defining_Identifier (C_Comma_Colon);
            Num_Idents := 1;

            Ident_Loop : loop
               exit Ident_Loop when Token = Tok_Colon;

               --  The only valid tokens are colon and comma, so if we have
               --  neither do a bit of investigation to see which is the
               --  better choice for insertion.

               if Token /= Tok_Comma then

                  --  Assume colon if IN or OUT keyword found

                  exit Ident_Loop when Token = Tok_In or else Token = Tok_Out;

                  --  Otherwise scan ahead

                  Save_Scan_State (Scan_State);
                  Look_Ahead : loop

                     --  If we run into a semicolon, then assume that a
                     --  colon was missing, e.g.  Parms (X Y; ...). Also
                     --  assume missing colon on EOF (a real disaster!)
                     --  and on a right paren, e.g. Parms (X Y), and also
                     --  on an assignment symbol, e.g. Parms (X Y := ..)

                     if Token = Tok_Semicolon
                       or else Token = Tok_Right_Paren
                       or else Token = Tok_EOF
                       or else Token = Tok_Colon_Equal
                     then
                        Restore_Scan_State (Scan_State);
                        exit Ident_Loop;

                     --  If we run into a colon, assume that we had a missing
                     --  comma, e.g. Parms (A B : ...). Also assume a missing
                     --  comma if we hit another comma, e.g. Parms (A B, C ..)

                     elsif Token = Tok_Colon
                       or else Token = Tok_Comma
                     then
                        Restore_Scan_State (Scan_State);
                        exit Look_Ahead;
                     end if;

                     Scan;
                  end loop Look_Ahead;
               end if;

               --  Here if a comma is present, or to be assumed

               T_Comma;
               Num_Idents := Num_Idents + 1;
               Idents (Num_Idents) := P_Defining_Identifier (C_Comma_Colon);
            end loop Ident_Loop;

            --  Fall through the loop on encountering a colon, or deciding
            --  that there is a missing colon.

            T_Colon;

            --  If there are multiple identifiers, we repeatedly scan the
            --  type and initialization expression information by resetting
            --  the scan pointer (so that we get completely separate trees
            --  for each occurrence).

            if Num_Idents > 1 then
               Save_Scan_State (Scan_State);
            end if;

            --  Loop through defining identifiers in list

            Ident := 1;

            Ident_List_Loop : loop
               Specification_Node :=
                 New_Node (N_Parameter_Specification, Ident_Sloc);
               Set_Defining_Identifier (Specification_Node, Idents (Ident));

               if Token = Tok_Access then
                  if Ada_83 then
                     Error_Msg_SC ("(Ada 83) access parameters not allowed");
                  end if;

                  Set_Parameter_Type
                    (Specification_Node, P_Access_Definition);

               else
                  P_Mode (Specification_Node);

                  if Token = Tok_Procedure
                       or else
                     Token = Tok_Function
                  then
                     Error_Msg_SC ("formal subprogram parameter not allowed");
                     Scan;

                     if Token = Tok_Left_Paren then
                        Discard_Junk_List (P_Formal_Part);
                     end if;

                     if Token = Tok_Return then
                        Scan;
                        Discard_Junk_Node (P_Subtype_Mark);
                     end if;

                     Set_Parameter_Type (Specification_Node, Error);

                  else
                     Set_Parameter_Type (Specification_Node, P_Subtype_Mark);
                     No_Constraint;
                  end if;
               end if;

               Set_Expression (Specification_Node, Init_Expr_Opt (True));

               if Ident > 1 then
                  Set_Prev_Ids (Specification_Node, True);
               end if;

               if Ident < Num_Idents then
                  Set_More_Ids (Specification_Node, True);
               end if;

               Append (Specification_Node, Specification_List);
               exit Ident_List_Loop when Ident = Num_Idents;
               Ident := Ident + 1;
               Restore_Scan_State (Scan_State);
            end loop Ident_List_Loop;

         exception
            when Error_Resync =>
               Resync_Semicolon_List;
         end;

         if Token = Tok_Semicolon then
            Save_Scan_State (Scan_State);
            Scan; -- past semicolon

            --  If we have RETURN or IS after the semicolon, then assume
            --  that semicolon should have been a right parenthesis and exit

            if Token = Tok_Is or else Token = Tok_Return then
               Error_Msg_SP ("expected "")"" in place of "";""");
               exit Specification_Loop;
            end if;

            --  If we have a declaration keyword after the semicolon, then
            --  assume we had a missing right parenthesis and terminate list

            if Token in Token_Class_Declk then
               Error_Msg_AP ("missing "")""");
               Restore_Scan_State (Scan_State);
               exit Specification_Loop;
            end if;

         elsif Token = Tok_Right_Paren then
            Scan; -- past right paren
            exit Specification_Loop;

         --  Special check for common error of using comma instead of semicolon

         elsif Token = Tok_Comma then
            T_Semicolon;
            Scan; -- past comma

         --  Special check for omitted separator

         elsif Token = Tok_Identifier then
            T_Semicolon;

         --  If nothing sensible, skip to next semicolon or right paren

         else
            T_Semicolon;
            Resync_Semicolon_List;

            if Token = Tok_Semicolon then
               Scan; -- past semicolon
            else
               T_Right_Paren;
               exit Specification_Loop;
            end if;
         end if;
      end loop Specification_Loop;

      return Specification_List;
   end P_Formal_Part;

   ----------------------------------
   -- 6.1  Parameter Specification --
   ----------------------------------

   --  Parsed by P_Formal_Part (6.1)

   ---------------
   -- 6.1  Mode --
   ---------------

   --  MODE ::= [in] | in out | out

   --  There is no explicit node in the tree for the Mode. Instead the
   --  In_Present and Out_Present flags are set in the parent node to
   --  record the presence of keywords specifying the mode.

   --  Error_Recovery: cannot raise Error_Resync

   procedure P_Mode (Node : Node_Id) is
   begin
      if Token = Tok_In then
         Scan; -- past IN
         Set_In_Present (Node, True);
      end if;

      if Token = Tok_Out then
         Scan; -- past OUT
         Set_Out_Present (Node, True);
      end if;

      if Token = Tok_In then
         Error_Msg_SC ("IN must preceed OUT in parameter mode");
         Scan; -- past IN
         Set_In_Present (Node, True);
      end if;
   end P_Mode;

   --------------------------
   -- 6.3  Subprogram Body --
   --------------------------

   --  Parsed by P_Subprogram (6.1)

   -----------------------------------
   -- 6.4  Procedure Call Statement --
   -----------------------------------

   --  Parsed by P_Sequence_Of_Statements (5.1)

   ------------------------
   -- 6.4  Function Call --
   ------------------------

   --  Parsed by P_Call_Or_Name (4.1)

   --------------------------------
   -- 6.4  Actual Parameter Part --
   --------------------------------

   --  Parsed by P_Call_Or_Name (4.1)

   --------------------------------
   -- 6.4  Parameter Association --
   --------------------------------

   --  Parsed by P_Call_Or_Name (4.1)

   ------------------------------------
   -- 6.4  Explicit Actual Parameter --
   ------------------------------------

   --  Parsed by P_Call_Or_Name (4.1)

   ---------------------------
   -- 6.5  Return Statement --
   ---------------------------

   --  RETURN_STATEMENT ::= return [EXPRESSION];

   --  The caller has checked that the initial token is RETURN

   --  Error recovery: can raise Error_Resync

   function P_Return_Statement return Node_Id is
      Return_Node : Node_Id;

   begin
      Return_Node := New_Node (N_Return_Statement, Token_Ptr);

      --  Sloc points to RETURN
      --  Expression (Op3)

      Scan; -- past RETURN

      if Token /= Tok_Semicolon then

         --  If no semicolon, then scan an expression, except that
         --  we avoid trying to scan an expression if we are at an
         --  expression terminator since in that case the best error
         --  message is probably that we have a missing semicolon.

         if Token not in Token_Class_Eterm then
            Set_Expression (Return_Node, P_Expression_No_Right_Paren);
         end if;
      end if;

      TF_Semicolon;
      return Return_Node;
   end P_Return_Statement;

end Ch6;
