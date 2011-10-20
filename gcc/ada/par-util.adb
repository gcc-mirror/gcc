------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

with Csets;    use Csets;
with Namet.Sp; use Namet.Sp;
with Stylesw;  use Stylesw;
with Uintp;    use Uintp;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

separate (Par)
package body Util is

   ---------------------
   -- Bad_Spelling_Of --
   ---------------------

   function Bad_Spelling_Of (T : Token_Type) return Boolean is
      Tname : constant String := Token_Type'Image (T);
      --  Characters of token name

      S : String (1 .. Tname'Last - 4);
      --  Characters of token name folded to lower case, omitting TOK_ at start

      M1 : String (1 .. 42) := "incorrect spelling of keyword ************";
      M2 : String (1 .. 44) := "illegal abbreviation of keyword ************";
      --  Buffers used to construct error message

      P1 : constant := 30;
      P2 : constant := 32;
      --  Starting subscripts in M1, M2 for keyword name

      SL : constant Natural := S'Length;
      --  Length of expected token name excluding TOK_ at start

   begin
      if Token /= Tok_Identifier then
         return False;
      end if;

      for J in S'Range loop
         S (J) := Fold_Lower (Tname (J + 4));
      end loop;

      Get_Name_String (Token_Name);

      --  A special check for case of PROGRAM used for PROCEDURE

      if T = Tok_Procedure
        and then Name_Len = 7
        and then Name_Buffer (1 .. 7) = "program"
      then
         Error_Msg_SC -- CODEFIX
           ("PROCEDURE expected");
         Token := T;
         return True;

      --  A special check for an illegal abbreviation

      elsif Name_Len < S'Length
        and then Name_Len >= 4
        and then Name_Buffer (1 .. Name_Len) = S (1 .. Name_Len)
      then
         for J in 1 .. S'Last loop
            M2 (P2 + J - 1) := Fold_Upper (S (J));
         end loop;

         Error_Msg_SC (M2 (1 .. P2 - 1 + S'Last));
         Token := T;
         return True;
      end if;

      --  Now we go into the full circuit to check for a misspelling

      --  Never consider something a misspelling if either the actual or
      --  expected string is less than 3 characters (before this check we
      --  used to consider i to be a misspelled if in some cases!)

      if SL < 3 or else Name_Len < 3 then
         return False;

      --  Special case: prefix matches, i.e. the leading characters of the
      --  token that we have exactly match the required keyword. If there
      --  are at least two characters left over, assume that we have a case
      --  of two keywords joined together which should not be joined.

      elsif Name_Len > SL + 1
        and then S = Name_Buffer (1 .. SL)
      then
         Scan_Ptr := Token_Ptr + S'Length;
         Error_Msg_S ("|missing space");
         Token := T;
         return True;
      end if;

      if Is_Bad_Spelling_Of (Name_Buffer (1 .. Name_Len), S) then
         for J in 1 .. S'Last loop
            M1 (P1 + J - 1) := Fold_Upper (S (J));
         end loop;

         Error_Msg_SC -- CODFIX
           (M1 (1 .. P1 - 1 + S'Last));
         Token := T;
         return True;

      else
         return False;
      end if;
   end Bad_Spelling_Of;

   ----------------------
   -- Check_95_Keyword --
   ----------------------

   --  On entry, the caller has checked that current token is an identifier
   --  whose name matches the name of the 95 keyword New_Tok.

   procedure Check_95_Keyword (Token_95, Next : Token_Type) is
      Scan_State : Saved_Scan_State;

   begin
      Save_Scan_State (Scan_State); -- at identifier/keyword
      Scan; -- past identifier/keyword

      if Token = Next then
         Restore_Scan_State (Scan_State); -- to identifier
         Error_Msg_Name_1 := Token_Name;
         Error_Msg_SC ("(Ada 83) keyword* cannot be used!");
         Token := Token_95;
      else
         Restore_Scan_State (Scan_State); -- to identifier
      end if;
   end Check_95_Keyword;

   ----------------------
   -- Check_Bad_Layout --
   ----------------------

   procedure Check_Bad_Layout is
   begin
      if RM_Column_Check and then Token_Is_At_Start_Of_Line
        and then Start_Column <= Scope.Table (Scope.Last).Ecol
      then
         Error_Msg_BC -- CODEFIX
           ("(style) incorrect layout");
      end if;
   end Check_Bad_Layout;

   --------------------------
   -- Check_Future_Keyword --
   --------------------------

   procedure Check_Future_Keyword is
   begin
      --  Ada 2005 (AI-284): Compiling in Ada95 mode we warn that INTERFACE,
      --  OVERRIDING, and SYNCHRONIZED are new reserved words.

      if Ada_Version = Ada_95
        and then Warn_On_Ada_2005_Compatibility
      then
         if Token_Name = Name_Overriding
           or else Token_Name = Name_Synchronized
           or else (Token_Name = Name_Interface
                     and then Prev_Token /= Tok_Pragma)
         then
            Error_Msg_N ("& is a reserved word in Ada 2005?", Token_Node);
         end if;
      end if;

      --  Similarly, warn about Ada 2012 reserved words

      if Ada_Version in Ada_95 .. Ada_2005
        and then Warn_On_Ada_2012_Compatibility
      then
         if Token_Name = Name_Some then
            Error_Msg_N ("& is a reserved word in Ada 2012?", Token_Node);
         end if;
      end if;

      --  Note: we deliberately do not emit these warnings when operating in
      --  Ada 83 mode because in that case we assume the user is building
      --  legacy code anyway and is not interested in updating Ada versions.

   end Check_Future_Keyword;

   --------------------------
   -- Check_Misspelling_Of --
   --------------------------

   procedure Check_Misspelling_Of (T : Token_Type) is
   begin
      if Bad_Spelling_Of (T) then
         null;
      end if;
   end Check_Misspelling_Of;

   -----------------------------
   -- Check_Simple_Expression --
   -----------------------------

   procedure Check_Simple_Expression (E : Node_Id) is
   begin
      if Expr_Form = EF_Non_Simple then
         Error_Msg_N ("this expression must be parenthesized", E);
      end if;
   end Check_Simple_Expression;

   ---------------------------------------
   -- Check_Simple_Expression_In_Ada_83 --
   ---------------------------------------

   procedure Check_Simple_Expression_In_Ada_83 (E : Node_Id) is
   begin
      if Expr_Form = EF_Non_Simple then
         if Ada_Version = Ada_83 then
            Error_Msg_N ("(Ada 83) this expression must be parenthesized!", E);
         end if;
      end if;
   end Check_Simple_Expression_In_Ada_83;

   ------------------------
   -- Check_Subtype_Mark --
   ------------------------

   function Check_Subtype_Mark (Mark : Node_Id) return Node_Id is
   begin
      if Nkind (Mark) = N_Identifier
        or else Nkind (Mark) = N_Selected_Component
        or else (Nkind (Mark) = N_Attribute_Reference
                  and then Is_Type_Attribute_Name (Attribute_Name (Mark)))
        or else Mark = Error
      then
         return Mark;
      else
         Error_Msg ("subtype mark expected", Sloc (Mark));
         return Error;
      end if;
   end Check_Subtype_Mark;

   -------------------
   -- Comma_Present --
   -------------------

   function Comma_Present return Boolean is
      Scan_State  : Saved_Scan_State;
      Paren_Count : Nat;

   begin
      --  First check, if a comma is present, then a comma is present!

      if Token = Tok_Comma then
         T_Comma;
         return True;

      --  If we have a right paren, then that is taken as ending the list
      --  i.e. no comma is present.

      elsif Token = Tok_Right_Paren then
         return False;

      --  If pragmas, then get rid of them and make a recursive call
      --  to process what follows these pragmas.

      elsif Token = Tok_Pragma then
         P_Pragmas_Misplaced;
         return Comma_Present;

      --  At this stage we have an error, and the goal is to decide on whether
      --  or not we should diagnose an error and report a (non-existent)
      --  comma as being present, or simply to report no comma is present

      --  If we are a semicolon, then the question is whether we have a missing
      --  right paren, or whether the semicolon should have been a comma. To
      --  guess the right answer, we scan ahead keeping track of the paren
      --  level, looking for a clue that helps us make the right decision.

      --  This approach is highly accurate in the single error case, and does
      --  not make bad mistakes in the multiple error case (indeed we can't
      --  really make a very bad decision at this point in any case).

      elsif Token = Tok_Semicolon then
         Save_Scan_State (Scan_State);
         Scan; -- past semicolon

         --  Check for being followed by identifier => which almost certainly
         --  means we are still in a parameter list and the comma should have
         --  been a semicolon (such a sequence could not follow a semicolon)

         if Token = Tok_Identifier then
            Scan;

            if Token = Tok_Arrow then
               goto Assume_Comma;
            end if;
         end if;

         --  If that test didn't work, loop ahead looking for a comma or
         --  semicolon at the same parenthesis level. Always remember that
         --  we can't go badly wrong in an error situation like this!

         Paren_Count := 0;

         --  Here is the look ahead loop, Paren_Count tells us whether the
         --  token we are looking at is at the same paren level as the
         --  suspicious semicolon that we are trying to figure out.

         loop

            --  If we hit another semicolon or an end of file, and we have
            --  not seen a right paren or another comma on the way, then
            --  probably the semicolon did end the list. Indeed that is
            --  certainly the only single error correction possible here.

            if Token = Tok_Semicolon or else Token = Tok_EOF then
               Restore_Scan_State (Scan_State);
               return False;

            --  A comma at the same paren level as the semicolon is a strong
            --  indicator that the semicolon should have been a comma, indeed
            --  again this is the only possible single error correction.

            elsif Token = Tok_Comma then
               exit when Paren_Count = 0;

            --  A left paren just bumps the paren count

            elsif Token = Tok_Left_Paren then
               Paren_Count := Paren_Count + 1;

            --  A right paren that is at the same paren level as the semicolon
            --  also means that the only possible single error correction is
            --  to assume that the semicolon should have been a comma. If we
            --  are not at the same paren level, then adjust the paren level.

            elsif Token = Tok_Right_Paren then
               exit when Paren_Count = 0;
               Paren_Count := Paren_Count - 1;
            end if;

            --  Keep going, we haven't made a decision yet

            Scan;
         end loop;

         --  If we fall through the loop, it means that we found a terminating
         --  right paren or another comma. In either case it is reasonable to
         --  assume that the semicolon was really intended to be a comma. Also
         --  come here for the identifier arrow case.

         <<Assume_Comma>>
            Restore_Scan_State (Scan_State);
            Error_Msg_SC -- CODEFIX
              ("|"";"" should be "",""");
            Scan; -- past the semicolon
            return True;

      --  If we are not at semicolon or a right paren, then we base the
      --  decision on whether or not the next token can be part of an
      --  expression. If not, then decide that no comma is present (the
      --  caller will eventually generate a missing right parent message)

      elsif Token in Token_Class_Eterm then
         return False;

      --  Otherwise we assume a comma is present, even if none is present,
      --  since the next token must be part of an expression, so if we were
      --  at the end of the list, then there is more than one error present.

      else
         T_Comma; -- to give error
         return True;
      end if;
   end Comma_Present;

   -----------------------
   -- Discard_Junk_List --
   -----------------------

   procedure Discard_Junk_List (L : List_Id) is
      pragma Warnings (Off, L);
   begin
      null;
   end Discard_Junk_List;

   -----------------------
   -- Discard_Junk_Node --
   -----------------------

   procedure Discard_Junk_Node (N : Node_Id) is
      pragma Warnings (Off, N);
   begin
      null;
   end Discard_Junk_Node;

   ------------
   -- Ignore --
   ------------

   procedure Ignore (T : Token_Type) is
   begin
      while Token = T loop
         if T = Tok_Comma then
            Error_Msg_SC -- CODEFIX
              ("|extra "","" ignored");

         elsif T = Tok_Left_Paren then
            Error_Msg_SC -- CODEFIX
              ("|extra ""("" ignored");

         elsif T = Tok_Right_Paren then
            Error_Msg_SC -- CODEFIX
              ("|extra "")"" ignored");

         elsif T = Tok_Semicolon then
            Error_Msg_SC -- CODEFIX
              ("|extra "";"" ignored");

         elsif T = Tok_Colon then
            Error_Msg_SC -- CODEFIX
              ("|extra "":"" ignored");

         else
            declare
               Tname : constant String := Token_Type'Image (Token);
            begin
               Error_Msg_SC ("|extra " & Tname (5 .. Tname'Last) & "ignored");
            end;
         end if;

         Scan; -- Scan past ignored token
      end loop;
   end Ignore;

   ----------------------------
   -- Is_Reserved_Identifier --
   ----------------------------

   function Is_Reserved_Identifier (C : Id_Check := None) return Boolean is
   begin
      if not Is_Reserved_Keyword (Token) then
         return False;

      else
         declare
            Ident_Casing : constant Casing_Type :=
                             Identifier_Casing (Current_Source_File);
            Key_Casing   : constant Casing_Type :=
                             Keyword_Casing (Current_Source_File);

         begin
            --  If the casing of identifiers and keywords is different in
            --  this source file, and the casing of this token matches the
            --  keyword casing, then we return False, since it is pretty
            --  clearly intended to be a keyword.

            if Ident_Casing = Unknown
              or else Key_Casing = Unknown
              or else Ident_Casing = Key_Casing
              or else Determine_Token_Casing /= Key_Casing
            then
               return True;

            --  Here we have a keyword written clearly with keyword casing.
            --  In default mode, we would not be willing to consider this as
            --  a reserved identifier, but if C is set, we may still accept it

            elsif C /= None then
               declare
                  Scan_State  : Saved_Scan_State;
                  OK_Next_Tok : Boolean;

               begin
                  Save_Scan_State (Scan_State);
                  Scan;

                  if Token_Is_At_Start_Of_Line then
                     return False;
                  end if;

                  case C is
                     when None =>
                        raise Program_Error;

                     when C_Comma_Right_Paren =>
                        OK_Next_Tok :=
                          Token = Tok_Comma or else Token = Tok_Right_Paren;

                     when C_Comma_Colon =>
                        OK_Next_Tok :=
                          Token = Tok_Comma or else Token = Tok_Colon;

                     when C_Do =>
                        OK_Next_Tok :=
                          Token = Tok_Do;

                     when C_Dot =>
                        OK_Next_Tok :=
                          Token = Tok_Dot;

                     when C_Greater_Greater =>
                        OK_Next_Tok :=
                          Token = Tok_Greater_Greater;

                     when C_In =>
                        OK_Next_Tok :=
                          Token = Tok_In;

                     when C_Is =>
                        OK_Next_Tok :=
                          Token = Tok_Is;

                     when C_Left_Paren_Semicolon =>
                        OK_Next_Tok :=
                          Token = Tok_Left_Paren or else Token = Tok_Semicolon;

                     when C_Use =>
                        OK_Next_Tok :=
                          Token = Tok_Use;

                     when C_Vertical_Bar_Arrow =>
                        OK_Next_Tok :=
                          Token = Tok_Vertical_Bar or else Token = Tok_Arrow;
                  end case;

                  Restore_Scan_State (Scan_State);

                  if OK_Next_Tok then
                     return True;
                  end if;
               end;
            end if;
         end;
      end if;

      --  If we fall through it is not a reserved identifier

      return False;
   end Is_Reserved_Identifier;

   ----------------------
   -- Merge_Identifier --
   ----------------------

   procedure Merge_Identifier (Prev : Node_Id; Nxt : Token_Type) is
   begin
      if Token /= Tok_Identifier then
         return;
      end if;

      declare
         S : Saved_Scan_State;
         T : Token_Type;

      begin
         Save_Scan_State (S);
         Scan;
         T := Token;
         Restore_Scan_State (S);

         if T /= Nxt then
            return;
         end if;
      end;

      --  Check exactly one space between identifiers

      if Source (Token_Ptr - 1) /= ' '
        or else Int (Token_Ptr) /=
                  Int (Prev_Token_Ptr) + Length_Of_Name (Chars (Prev)) + 1
      then
         return;
      end if;

      --  Do the merge

      Get_Name_String (Chars (Token_Node));

      declare
         Buf : constant String (1 .. Name_Len) :=
                 Name_Buffer (1 .. Name_Len);

      begin
         Get_Name_String (Chars (Prev));
         Add_Char_To_Name_Buffer ('_');
         Add_Str_To_Name_Buffer (Buf);
         Set_Chars (Prev, Name_Find);
      end;

      Error_Msg_Node_1 := Prev;
      Error_Msg_SC ("unexpected identifier, possibly & was meant here");
      Scan;
   end Merge_Identifier;

   -------------------
   -- Next_Token_Is --
   -------------------

   function Next_Token_Is (Tok : Token_Type) return Boolean is
      Scan_State : Saved_Scan_State;
      Result     : Boolean;
   begin
      Save_Scan_State (Scan_State);
      Scan;
      Result := (Token = Tok);
      Restore_Scan_State (Scan_State);
      return Result;
   end Next_Token_Is;

   -------------------
   -- No_Constraint --
   -------------------

   procedure No_Constraint is
   begin
      if Token in Token_Class_Consk then
         Error_Msg_SC ("constraint not allowed here");
         Discard_Junk_Node (P_Constraint_Opt);
      end if;
   end No_Constraint;

   ---------------------
   -- Pop_Scope_Stack --
   ---------------------

   procedure Pop_Scope_Stack is
   begin
      pragma Assert (Scope.Last > 0);
      Scope.Decrement_Last;

      if Debug_Flag_P then
         Error_Msg_Uint_1 := UI_From_Int (Scope.Last);
         Error_Msg_SC ("decrement scope stack ptr, new value = ^!");
      end if;
   end Pop_Scope_Stack;

   ----------------------
   -- Push_Scope_Stack --
   ----------------------

   procedure Push_Scope_Stack is
   begin
      Scope.Increment_Last;

      if Style_Check_Max_Nesting_Level
        and then Scope.Last = Style_Max_Nesting_Level + 1
      then
         Error_Msg
           ("(style) maximum nesting level exceeded",
            First_Non_Blank_Location);
      end if;

      Scope.Table (Scope.Last).Junk := False;
      Scope.Table (Scope.Last).Node := Empty;

      if Debug_Flag_P then
         Error_Msg_Uint_1 := UI_From_Int (Scope.Last);
         Error_Msg_SC ("increment scope stack ptr, new value = ^!");
      end if;
   end Push_Scope_Stack;

   ----------------------
   -- Separate_Present --
   ----------------------

   function Separate_Present return Boolean is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Separate then
         return True;

      elsif Token /= Tok_Identifier then
         return False;

      else
         Save_Scan_State (Scan_State);
         Scan; -- past identifier

         if Token = Tok_Semicolon then
            Restore_Scan_State (Scan_State);
            return Bad_Spelling_Of (Tok_Separate);

         else
            Restore_Scan_State (Scan_State);
            return False;
         end if;
      end if;
   end Separate_Present;

   --------------------------
   -- Signal_Bad_Attribute --
   --------------------------

   procedure Signal_Bad_Attribute is
   begin
      Error_Msg_N ("unrecognized attribute&", Token_Node);

      --  Check for possible misspelling

      Error_Msg_Name_1 := First_Attribute_Name;
      while Error_Msg_Name_1 <= Last_Attribute_Name loop
         if Is_Bad_Spelling_Of (Token_Name, Error_Msg_Name_1) then
            Error_Msg_N -- CODEFIX
              ("\possible misspelling of %", Token_Node);
            exit;
         end if;

         Error_Msg_Name_1 := Error_Msg_Name_1 + 1;
      end loop;
   end Signal_Bad_Attribute;

   -----------------------------
   -- Token_Is_At_End_Of_Line --
   -----------------------------

   function Token_Is_At_End_Of_Line return Boolean is
      S : Source_Ptr;

   begin
      --  Skip past blanks and horizontal tabs

      S := Scan_Ptr;
      while Source (S) = ' ' or else Source (S) = ASCII.HT loop
         S := S + 1;
      end loop;

      --  We are at end of line if at a control character (CR/LF/VT/FF/EOF)
      --  or if we are at the start of an end of line comment sequence.

      return Source (S) < ' '
        or else (Source (S) = '-' and then Source (S + 1) = '-');
   end Token_Is_At_End_Of_Line;

   -------------------------------
   -- Token_Is_At_Start_Of_Line --
   -------------------------------

   function Token_Is_At_Start_Of_Line return Boolean is
   begin
      return (Token_Ptr = First_Non_Blank_Location or else Token = Tok_EOF);
   end Token_Is_At_Start_Of_Line;

end Util;
