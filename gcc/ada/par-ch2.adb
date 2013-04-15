------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
--  Turn off subprogram body ordering check. Subprograms are in order
--  by RM section rather than alphabetical

separate (Par)
package body Ch2 is

   --  Local functions, used only in this chapter

   procedure Scan_Pragma_Argument_Association
     (Identifier_Seen : in out Boolean;
      Association     : out Node_Id);
   --  Scans out a pragma argument association. Identifier_Seen is true on
   --  entry if a previous association had an identifier, and gets set True if
   --  the scanned association has an identifier (this is used to check the
   --  rule that no associations without identifiers can follow an association
   --  which has an identifier). The result is returned in Association.
   --
   --  Note: We allow attribute forms Pre'Class, Post'Class, Invariant'Class,
   --  Type_Invariant'Class in place of a pragma argument identifier. Rather
   --  than handle this case specially, we replace such references with
   --  one of the special internal identifiers _Pre, _Post, _Invariant, or
   --  _Type_Invariant, and this procedure is where this replacement occurs.

   ---------------------
   -- 2.3  Identifier --
   ---------------------

   --  IDENTIFIER ::= LETTER {[UNDERLINE] LETTER_OR_DIGIT}

   --  LETTER_OR_DIGIT ::= IDENTIFIER_LETTER | DIGIT

   --  An IDENTIFIER shall not be a reserved word

   --  Error recovery: can raise Error_Resync (cannot return Error)

   function P_Identifier (C : Id_Check := None) return Node_Id is
      Ident_Node : Node_Id;

   begin
      --  All set if we do indeed have an identifier

      --  Code duplication, see Par_Ch3.P_Defining_Identifier???

      if Token = Tok_Identifier then
         Check_Future_Keyword;
         Ident_Node := Token_Node;
         Scan; -- past Identifier
         return Ident_Node;

      --  If we have a reserved identifier, manufacture an identifier with
      --  a corresponding name after posting an appropriate error message

      elsif Is_Reserved_Identifier (C) then
         Scan_Reserved_Identifier (Force_Msg => False);
         Ident_Node := Token_Node;
         Scan; -- past the node
         return Ident_Node;

      --  Otherwise we have junk that cannot be interpreted as an identifier

      else
         T_Identifier; -- to give message
         raise Error_Resync;
      end if;
   end P_Identifier;

   --------------------------
   -- 2.3  Letter Or Digit --
   --------------------------

   --  Parsed by P_Identifier (2.3)

   --------------------------
   -- 2.4  Numeric Literal --
   --------------------------

   --  NUMERIC_LITERAL ::= DECIMAL_LITERAL | BASED_LITERAL

   --  Numeric literal is returned by the scanner as either
   --  Tok_Integer_Literal or Tok_Real_Literal

   ----------------------------
   -- 2.4.1  Decimal Literal --
   ----------------------------

   --  DECIMAL_LITERAL ::= NUMERAL [.NUMERAL] [EXPONENT]

   --  Handled by scanner as part of numeric literal handing (see 2.4)

   --------------------
   -- 2.4.1  Numeral --
   --------------------

   --  NUMERAL ::= DIGIT {[UNDERLINE] DIGIT}

   --  Handled by scanner as part of numeric literal handling (see 2.4)

   ---------------------
   -- 2.4.1  Exponent --
   ---------------------

   --  EXPONENT ::= E [+] NUMERAL | E - NUMERAL

   --  Handled by scanner as part of numeric literal handling (see 2.4)

   --------------------------
   -- 2.4.2  Based Literal --
   --------------------------

   --  BASED_LITERAL ::=
   --   BASE # BASED_NUMERAL [.BASED_NUMERAL] # [EXPONENT]

   --  Handled by scanner as part of numeric literal handling (see 2.4)

   -----------------
   -- 2.4.2  Base --
   -----------------

   --  BASE ::= NUMERAL

   --  Handled by scanner as part of numeric literal handling (see 2.4)

   --------------------------
   -- 2.4.2  Based Numeral --
   --------------------------

   --  BASED_NUMERAL ::=
   --    EXTENDED_DIGIT {[UNDERLINE] EXTENDED_DIGIT}

   --  Handled by scanner as part of numeric literal handling (see 2.4)

   ---------------------------
   -- 2.4.2  Extended Digit --
   ---------------------------

   --  EXTENDED_DIGIT ::= DIGIT | A | B | C | D | E | F

   --  Handled by scanner as part of numeric literal handling (see 2.4)

   ----------------------------
   -- 2.5  Character Literal --
   ----------------------------

   --  CHARACTER_LITERAL ::= ' GRAPHIC_CHARACTER '

   --  Handled by the scanner and returned as Tok_Char_Literal

   -------------------------
   -- 2.6  String Literal --
   -------------------------

   --  STRING LITERAL ::= "{STRING_ELEMENT}"

   --  Handled by the scanner and returned as Tok_String_Literal
   --  or if the string looks like an operator as Tok_Operator_Symbol.

   -------------------------
   -- 2.6  String Element --
   -------------------------

   --  STRING_ELEMENT ::= "" | non-quotation_mark_GRAPHIC_CHARACTER

   --  A STRING_ELEMENT is either a pair of quotation marks ("),
   --  or a single GRAPHIC_CHARACTER other than a quotation mark.

   --  Handled by scanner as part of string literal handling (see 2.4)

   ------------------
   -- 2.7  Comment --
   ------------------

   --  A COMMENT starts with two adjacent hyphens and extends up to the
   --  end of the line. A COMMENT may appear on any line of a program.

   --  Handled by the scanner which simply skips past encountered comments

   -----------------
   -- 2.8  Pragma --
   -----------------

   --  PRAGMA ::= pragma IDENTIFIER
   --    [(PRAGMA_ARGUMENT_ASSOCIATION {, PRAGMA_ARGUMENT_ASSOCIATION})];

   --  The caller has checked that the initial token is PRAGMA

   --  Error recovery: cannot raise Error_Resync

   --  One special piece of processing is needed in this routine. As described
   --  in the section on "Handling semicolon used in place of IS" in module
   --  Parse, the parser detects the case of missing subprogram bodies to
   --  allow recovery from this syntactic error. Pragma INTERFACE (and, for
   --  Ada 95, pragma IMPORT) can appear in place of the body. The parser must
   --  recognize the use of these two pragmas in this context, otherwise it
   --  will think there are missing bodies, and try to change ; to IS, when
   --  in fact the bodies ARE present, supplied by these pragmas.

   function P_Pragma (Skipping : Boolean := False) return Node_Id is
      Interface_Check_Required : Boolean := False;
      --  Set True if check of pragma INTERFACE is required

      Import_Check_Required : Boolean := False;
      --  Set True if check of pragma IMPORT is required

      Arg_Count : Int := 0;
      --  Number of argument associations processed

      Identifier_Seen : Boolean := False;
      --  Set True if an identifier is encountered for a pragma argument. Used
      --  to check that there are no more arguments without identifiers.

      Prag_Node     : Node_Id;
      Prag_Name     : Name_Id;
      Semicolon_Loc : Source_Ptr;
      Ident_Node    : Node_Id;
      Assoc_Node    : Node_Id;
      Result        : Node_Id;

      procedure Skip_Pragma_Semicolon;
      --  Skip past semicolon at end of pragma

      ---------------------------
      -- Skip_Pragma_Semicolon --
      ---------------------------

      procedure Skip_Pragma_Semicolon is
      begin
         if Token /= Tok_Semicolon then

            --  If skipping the pragma, ignore a missing semicolon

            if Skipping then
               null;

            --  Otherwise demand a semicolon

            else
               T_Semicolon;
            end if;

         --  Scan past semicolon if present

         else
            Scan;
         end if;
      end Skip_Pragma_Semicolon;

   --  Start of processing for P_Pragma

   begin
      Prag_Node := New_Node (N_Pragma, Token_Ptr);
      Scan; -- past PRAGMA
      Prag_Name := Token_Name;

      if Style_Check then
         Style.Check_Pragma_Name;
      end if;

      --  Ada 2005 (AI-284): INTERFACE is a new reserved word but it is
      --  allowed as a pragma name.

      if Ada_Version >= Ada_2005
        and then Token = Tok_Interface
      then
         Prag_Name := Name_Interface;
         Ident_Node  := Make_Identifier (Token_Ptr, Name_Interface);
         Scan; -- past INTERFACE
      else
         Ident_Node := P_Identifier;
      end if;

      Set_Pragma_Identifier (Prag_Node, Ident_Node);

      --  See if special INTERFACE/IMPORT check is required

      if SIS_Entry_Active then
         Interface_Check_Required := (Prag_Name = Name_Interface);
         Import_Check_Required    := (Prag_Name = Name_Import);
      else
         Interface_Check_Required := False;
         Import_Check_Required    := False;
      end if;

      --  Scan arguments. We assume that arguments are present if there is
      --  a left paren, or if a semicolon is missing and there is another
      --  token on the same line as the pragma name.

      if Token = Tok_Left_Paren
        or else (Token /= Tok_Semicolon
                   and then not Token_Is_At_Start_Of_Line)
      then
         Set_Pragma_Argument_Associations (Prag_Node, New_List);
         T_Left_Paren;

         loop
            Arg_Count := Arg_Count + 1;
            Scan_Pragma_Argument_Association (Identifier_Seen, Assoc_Node);

            if Arg_Count = 2
              and then (Interface_Check_Required or else Import_Check_Required)
            then
               --  Here is where we cancel the SIS active status if this pragma
               --  supplies a body for the currently active subprogram spec.

               if Nkind (Expression (Assoc_Node)) in N_Direct_Name
                 and then Chars (Expression (Assoc_Node)) = Chars (SIS_Labl)
               then
                  SIS_Entry_Active := False;
               end if;
            end if;

            Append (Assoc_Node, Pragma_Argument_Associations (Prag_Node));
            exit when Token /= Tok_Comma;
            Scan; -- past comma
         end loop;

         --  If we have := for pragma Debug, it is worth special casing the
         --  error message (it is easy to think of pragma Debug as taking a
         --  statement, and an assignment statement is the most likely
         --  candidate for this error)

         if Token = Tok_Colon_Equal and then Prag_Name = Name_Debug then
            Error_Msg_SC ("argument for pragma Debug must be procedure call");
            Resync_To_Semicolon;

         --  Normal case, we expect a right paren here

         else
            T_Right_Paren;
         end if;
      end if;

      Semicolon_Loc := Token_Ptr;

      --  Now we have two tasks left, we need to scan out the semicolon
      --  following the pragma, and we have to call Par.Prag to process
      --  the pragma. Normally we do them in this order, however, there
      --  is one exception namely pragma Style_Checks where we like to
      --  skip the semicolon after processing the pragma, since that way
      --  the style checks for the scanning of the semicolon follow the
      --  settings of the pragma.

      --  You might think we could just unconditionally do things in
      --  the opposite order, but there are other pragmas, notably the
      --  case of pragma Source_File_Name, which assume the semicolon
      --  is already scanned out.

      if Prag_Name = Name_Style_Checks then
         Result := Par.Prag (Prag_Node, Semicolon_Loc);
         Skip_Pragma_Semicolon;
         return Result;
      else
         Skip_Pragma_Semicolon;
         return Par.Prag (Prag_Node, Semicolon_Loc);
      end if;

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         return Error;

   end P_Pragma;

   --  This routine is called if a pragma is encountered in an inappropriate
   --  position, the pragma is scanned out and control returns to continue.

   --  The caller has checked that the initial token is pragma

   --  Error recovery: cannot raise Error_Resync

   procedure P_Pragmas_Misplaced is
   begin
      while Token = Tok_Pragma loop
         Error_Msg_SC ("pragma not allowed here");
         Discard_Junk_Node (P_Pragma (Skipping => True));
      end loop;
   end P_Pragmas_Misplaced;

   --  This function is called to scan out an optional sequence of pragmas.
   --  If no pragmas are found, then No_List is returned.

   --  Error recovery: Cannot raise Error_Resync

   function P_Pragmas_Opt return List_Id is
      L : List_Id;

   begin
      if Token = Tok_Pragma then
         L := New_List;
         P_Pragmas_Opt (L);
         return L;

      else
         return No_List;
      end if;
   end P_Pragmas_Opt;

   --  This procedure is called to scan out an optional sequence of pragmas.
   --  Any pragmas found are appended to the list provided as an argument.

   --  Error recovery: Cannot raise Error_Resync

   procedure P_Pragmas_Opt (List : List_Id) is
      P     : Node_Id;

   begin
      while Token = Tok_Pragma loop
         P := P_Pragma;

         if Nkind (P) /= N_Error
           and then Nam_In (Pragma_Name (P), Name_Assert, Name_Debug)
         then
            Error_Msg_Name_1 := Pragma_Name (P);
            Error_Msg_N
              ("pragma% must be in declaration/statement context", P);
         else
            Append (P, List);
         end if;
      end loop;
   end P_Pragmas_Opt;

   --------------------------------------
   -- 2.8  Pragma_Argument Association --
   --------------------------------------

   --  PRAGMA_ARGUMENT_ASSOCIATION ::=
   --    [pragma_argument_IDENTIFIER =>] NAME
   --  | [pragma_argument_IDENTIFIER =>] EXPRESSION

   --  In Ada 2012, there are two more possibilities:

   --  PRAGMA_ARGUMENT_ASSOCIATION ::=
   --    [pragma_argument_ASPECT_MARK =>] NAME
   --  | [pragma_argument_ASPECT_MARK =>] EXPRESSION

   --  where the interesting allowed cases (which do not fit the syntax of the
   --  first alternative above are

   --  ASPECT_MARK ::=
   --    Pre'Class | Post'Class | Invariant'Class | Type_Invariant'Class

   --  We allow this special usage in all Ada modes, but it would be a pain to
   --  allow these aspects to pervade the pragma syntax, and the representation
   --  of pragma nodes internally. So what we do is to replace these
   --  ASPECT_MARK forms with identifiers whose name is one of the special
   --  internal names _Pre, _Post, _Invariant, or _Type_Invariant.

   --  Error recovery: cannot raise Error_Resync

   procedure Scan_Pragma_Argument_Association
     (Identifier_Seen : in out Boolean;
      Association     : out Node_Id)
   is
      Scan_State      : Saved_Scan_State;
      Identifier_Node : Node_Id;
      Id_Present      : Boolean;

   begin
      Association := New_Node (N_Pragma_Argument_Association, Token_Ptr);
      Set_Chars (Association, No_Name);
      Id_Present := False;

      --  Argument starts with identifier

      if Token = Tok_Identifier then
         Identifier_Node := Token_Node;
         Save_Scan_State (Scan_State); -- at Identifier
         Scan; -- past Identifier

         if Token = Tok_Arrow then
            Scan; -- past arrow
            Id_Present := True;

         --  Case of one of the special aspect forms

         elsif Token = Tok_Apostrophe then
            Scan; -- past apostrophe

            --  We have apostrophe, so check for identifier'Class

            if Token /= Tok_Identifier or else Token_Name /= Name_Class then
               null;

            --  We have identifier'Class, check for arrow

            else
               Scan; -- Past Class

               if Token /= Tok_Arrow then
                  null;

               --  Here we have scanned identifier'Class =>

               else
                  Id_Present := True;
                  Scan; -- past arrow

                  case Chars (Identifier_Node) is
                     when Name_Pre =>
                        Set_Chars (Identifier_Node, Name_uPre);

                     when Name_Post =>
                        Set_Chars (Identifier_Node, Name_uPost);

                     when Name_Type_Invariant =>
                        Set_Chars (Identifier_Node, Name_uType_Invariant);

                     when Name_Invariant =>
                        Set_Chars (Identifier_Node, Name_uInvariant);

                     --  If it is X'Class => for some invalid X, we will give
                     --  an error, and forget that 'Class was present, which
                     --  will give better error recovery. We could do a spell
                     --  check here, but it seems too much work.

                     when others =>
                        Error_Msg_SC ("invalid aspect id for pragma");
                  end case;
               end if;
            end if;
         end if;

         --  Identifier was present

         if Id_Present then
            Set_Chars (Association, Chars (Identifier_Node));
            Identifier_Seen := True;

         --  Identifier not present after all

         else
            Restore_Scan_State (Scan_State); -- to Identifier
         end if;
      end if;

      --  Diagnose error of "positional" argument for pragma appearing after
      --  a "named" argument (quotes here are because that's not quite accurate
      --  Ada RM terminology).

      --  Since older GNAT versions did not generate this error, disable this
      --  message in Relaxed_RM_Semantics mode to help legacy code using e.g.
      --  codepeer.

      if Identifier_Seen and not Id_Present and not Relaxed_RM_Semantics then
         Error_Msg_SC ("|pragma argument identifier required here");
         Error_Msg_SC ("\since previous argument had identifier (RM 2.8(4))");
      end if;

      if Id_Present then
         Set_Expression (Association, P_Expression);
      else
         Set_Expression (Association, P_Expression_If_OK);
      end if;
   end Scan_Pragma_Argument_Association;

end Ch2;
