------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 2                               --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order
--  by RM section rather than alphabetical

separate (Par)
package body Ch2 is

   --  Local functions, used only in this chapter

   procedure Scan_Pragma_Argument_Association
     (Identifier_Seen   : in out Boolean;
      Association       : out Node_Id;
      Reserved_Words_OK : Boolean := False);
   --  Scans out a pragma argument association. Identifier_Seen is True on
   --  entry if a previous association had an identifier, and gets set True
   --  if the scanned association has an identifier (this is used to check the
   --  rule that no associations without identifiers can follow an association
   --  which has an identifier). The result is returned in Association. Flag
   --  For_Pragma_Restrictions should be set when arguments are being parsed
   --  for pragma Restrictions.
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

   function P_Identifier
     (C         : Id_Check := None;
      Force_Msg : Boolean  := False)
     return Node_Id
   is
      Ident_Node : Node_Id;

   begin
      --  All set if we do indeed have an identifier

      if Token = Tok_Identifier then
         Check_Future_Keyword;

      --  If we have a reserved identifier, manufacture an identifier with
      --  a corresponding name after posting an appropriate error message

      elsif Is_Reserved_Identifier (C) then
         Scan_Reserved_Identifier (Force_Msg => Force_Msg);

      --  Otherwise we have junk that cannot be interpreted as an identifier

      else
         T_Identifier; -- to give message
         raise Error_Resync;
      end if;

      if Style_Check then
         Style.Check_Defining_Identifier_Casing;
      end if;

      Ident_Node := Token_Node;
      Scan; -- past the identifier

      return Ident_Node;
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

   ---------------------------------------
   --  2.6  Interpolated String Literal --
   ---------------------------------------

   --  INTERPOLATED_STRING_LITERAL ::=
   --    'f' "{INTERPOLATED_STRING_ELEMENT}" {
   --        "{INTERPOLATED_STRING_ELEMENT}" }

   --  INTERPOLATED_STRING_ELEMENT ::=
   --     ESCAPED_CHARACTER | INTERPOLATED_EXPRESSION
   --   | non_quotation_mark_non_left_brace_GRAPHIC_CHARACTER

   --  ESCAPED_CHARACTER ::= '\GRAPHIC_CHARACTER'

   --  INTERPOLATED_EXPRESSION ::= '{' EXPRESSION '}'

   --  Interpolated string element and escaped character rules are handled by
   --  scanner as part of string literal handling.

   -----------------------------------
   -- P_Interpolated_String_Literal --
   -----------------------------------

   function P_Interpolated_String_Literal return Node_Id is
      Elements_List : constant List_Id := New_List;
      NL_Node       : Node_Id;
      String_Node   : Node_Id;

   begin
      String_Node := New_Node (N_Interpolated_String_Literal, Token_Ptr);
      Inside_Interpolated_String_Literal := True;

      Scan;   --  past 'f'

      if Token /= Tok_String_Literal then
         Error_Msg_SC ("string literal expected");

      else
         Append_To (Elements_List, Token_Node);
         Scan;  --  past string_literal

         while Token in Tok_Left_Curly_Bracket | Tok_String_Literal loop

            --  Interpolated expression

            if Token = Tok_Left_Curly_Bracket then
               Scan; --  past '{'
               Append_To (Elements_List, P_Expression);
               T_Right_Curly_Bracket;
            else
               if Prev_Token = Tok_String_Literal then
                  NL_Node := New_Node (N_String_Literal, Token_Ptr);
                  Set_Has_Wide_Character (NL_Node, False);
                  Set_Has_Wide_Wide_Character (NL_Node, False);

                  Start_String;
                  Store_String_Char (Get_Char_Code (ASCII.LF));
                  Set_Strval (NL_Node, End_String);
                  Append_To (Elements_List, NL_Node);
               end if;

               Append_To (Elements_List, Token_Node);
               Scan; --  past string_literal
            end if;
         end loop;
      end if;

      Inside_Interpolated_String_Literal := False;
      Set_Expressions (String_Node, Elements_List);

      return String_Node;
   end P_Interpolated_String_Literal;

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
      procedure Skip_Pragma_Semicolon;
      --  Skip past semicolon at end of pragma

      ---------------------------
      -- Skip_Pragma_Semicolon --
      ---------------------------

      procedure Skip_Pragma_Semicolon is
      begin
         --  If skipping the pragma, ignore a missing semicolon

         if Token /= Tok_Semicolon and then Skipping then
            null;

         --  Otherwise demand a semicolon

         else
            T_Semicolon;
         end if;
      end Skip_Pragma_Semicolon;

      --  Local variables

      Import_Check_Required : Boolean := False;
      --  Set True if check of pragma IMPORT or INTERFACE is required

      Arg_Count : Nat := 0;
      --  Number of argument associations processed

      Identifier_Seen : Boolean := False;
      --  Set True if an identifier is encountered for a pragma argument. Used
      --  to check that there are no more arguments without identifiers.

      Assoc_Node    : Node_Id;
      Ident_Node    : Node_Id;
      Prag_Name     : Name_Id;
      Prag_Node     : Node_Id;
      Result        : Node_Id;
      Semicolon_Loc : Source_Ptr;

   --  Start of processing for P_Pragma

   begin
      Inside_Pragma := True;
      Prag_Node := New_Node (N_Pragma, Token_Ptr);
      Scan; -- past PRAGMA
      Prag_Name := Token_Name;

      if Style_Check then
         Style.Check_Pragma_Name;
      end if;

      --  Ada 2005 (AI-284): INTERFACE is a new reserved word but it is
      --  allowed as a pragma name.

      if Is_Reserved_Keyword (Token) then
         Prag_Name  := Keyword_Name (Token);
         Ident_Node := Make_Identifier (Token_Ptr, Prag_Name);
         Scan; -- past the keyword
      else
         Ident_Node := P_Identifier;
      end if;

      Set_Pragma_Identifier (Prag_Node, Ident_Node);

      --  See if special INTERFACE/IMPORT check is required

      if SIS_Entry_Active then
         Import_Check_Required :=
           (Prag_Name = Name_Import) or else (Prag_Name = Name_Interface);
      else
         Import_Check_Required := False;
      end if;

      --  Set global to indicate if we are within a Depends pragma

      if Chars (Ident_Node) = Name_Depends
        or else Chars (Ident_Node) = Name_Refined_Depends
      then
         Inside_Depends := True;
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

            Scan_Pragma_Argument_Association
              (Identifier_Seen   => Identifier_Seen,
               Association       => Assoc_Node,
               Reserved_Words_OK =>
                 Prag_Name in Name_Restriction_Warnings | Name_Restrictions);

            if Arg_Count = 2 and then Import_Check_Required then
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

      --  Cancel indication of being within a pragma or in particular a Depends
      --  pragma.

      Inside_Depends := False;
      Inside_Pragma  := False;

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
         Inside_Depends := False;
         Inside_Pragma  := False;
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
      P : Node_Id;

   begin
      while Token = Tok_Pragma loop
         P := P_Pragma;

         if Nkind (P) /= N_Error
           and then Pragma_Name_Unmapped (P) in Name_Assert | Name_Debug
         then
            Error_Msg_Name_1 := Pragma_Name_Unmapped (P);
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
   --  first alternative above) are

   --  ASPECT_MARK ::=
   --    Pre'Class | Post'Class | Invariant'Class | Type_Invariant'Class

   --  We allow this special usage in all Ada modes, but it would be a pain to
   --  allow these aspects to pervade the pragma syntax, and the representation
   --  of pragma nodes internally. So what we do is to replace these
   --  ASPECT_MARK forms with identifiers whose name is one of the special
   --  internal names _Pre, _Post, _Invariant, or _Type_Invariant.

   --  Error recovery: cannot raise Error_Resync

   procedure Scan_Pragma_Argument_Association
     (Identifier_Seen   : in out Boolean;
      Association       : out Node_Id;
      Reserved_Words_OK : Boolean := False)
   is
      function P_Expression_Or_Reserved_Word return Node_Id;
      --  Parse an expression or, if the token is one of the following reserved
      --  words, construct an identifier with proper Chars field.
      --    Access
      --    Delta
      --    Digits
      --    Mod
      --    Range

      -----------------------------------
      -- P_Expression_Or_Reserved_Word --
      -----------------------------------

      function P_Expression_Or_Reserved_Word return Node_Id is
         Word    : Node_Id;
         Word_Id : Name_Id;

      begin
         Word_Id := No_Name;

         if Token = Tok_Access then
            Word_Id := Name_Access;
            Scan; -- past ACCESS

         elsif Token = Tok_Delta then
            Word_Id := Name_Delta;
            Scan; -- past DELTA

         elsif Token = Tok_Digits then
            Word_Id := Name_Digits;
            Scan; -- past DIGITS

         elsif Token = Tok_Mod then
            Word_Id := Name_Mod;
            Scan; -- past MOD

         elsif Token = Tok_Range then
            Word_Id := Name_Range;
            Scan; -- post RANGE
         end if;

         if Word_Id = No_Name then
            return P_Expression;
         else
            Word := New_Node (N_Identifier, Token_Ptr);
            Set_Chars (Word, Word_Id);
            return Word;
         end if;
      end P_Expression_Or_Reserved_Word;

      --  Local variables

      Expression_Node : Node_Id;
      Identifier_Node : Node_Id;
      Identifier_OK   : Boolean;
      Scan_State      : Saved_Scan_State;

   --  Start of processing for Scan_Pragma_Argument_Association

   begin
      Association := New_Node (N_Pragma_Argument_Association, Token_Ptr);
      Set_Chars (Association, No_Name);
      Identifier_OK := False;

      --  Argument starts with identifier

      if Token = Tok_Identifier then
         Identifier_Node := Token_Node;
         Save_Scan_State (Scan_State); -- at Identifier
         Scan; -- past Identifier

         if Token = Tok_Arrow then
            Scan; -- past arrow
            Identifier_OK := True;

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
                  Identifier_OK := True;
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

         if Identifier_OK then
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

      if Identifier_Seen
        and not Identifier_OK
        and not Relaxed_RM_Semantics
      then
         Error_Msg_SC ("|pragma argument identifier required here");
         Error_Msg_SC ("\since previous argument had identifier (RM 2.8(4))");
      end if;

      if Identifier_OK then

         --  Certain pragmas such as Restriction_Warnings and Restrictions
         --  allow reserved words to appear as expressions when checking for
         --  prohibited uses of attributes.

         if Reserved_Words_OK
           and then Chars (Identifier_Node) = Name_No_Use_Of_Attribute
         then
            Expression_Node := P_Expression_Or_Reserved_Word;
         else
            Expression_Node := P_Expression;
         end if;
      else
         Expression_Node := P_Expression_If_OK;
      end if;

      Set_Expression (Association, Expression_Node);
   end Scan_Pragma_Argument_Association;

end Ch2;
