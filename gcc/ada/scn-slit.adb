------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S C N . S L I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

with Stringt; use Stringt;

separate (Scn)
procedure Slit is

   Delimiter : Character;
   --  Delimiter (first character of string)

   C : Character;
   --  Current source program character

   Code : Char_Code;
   --  Current character code value

   Err : Boolean;
   --  Error flag for Scan_Wide call

   String_Literal_Id : String_Id;
   --  Id for currently scanned string value

   Wide_Character_Found : Boolean := False;
   --  Set True if wide character found

   procedure Error_Bad_String_Char;
   --  Signal bad character in string/character literal. On entry Scan_Ptr
   --  points to the improper character encountered during the scan. Scan_Ptr
   --  is not modified, so it still points to the bad character on return.

   procedure Error_Unterminated_String;
   --  Procedure called if a line terminator character is encountered during
   --  scanning a string, meaning that the string is not properly terminated.

   procedure Set_String;
   --  Procedure used to distinguish between string and operator symbol.
   --  On entry the string has been scanned out, and its characters start
   --  at Token_Ptr and end one character before Scan_Ptr. On exit Token
   --  is set to Tok_String_Literal or Tok_Operator_Symbol as appropriate,
   --  and Token_Node is appropriately initialized. In addition, in the
   --  operator symbol case, Token_Name is appropriately set.

   ---------------------------
   -- Error_Bad_String_Char --
   ---------------------------

   procedure Error_Bad_String_Char is
      C : constant Character := Source (Scan_Ptr);

   begin
      if C = HT then
         Error_Msg_S ("horizontal tab not allowed in string");

      elsif C = VT or else C = FF then
         Error_Msg_S ("format effector not allowed in string");

      elsif C in Upper_Half_Character then
         Error_Msg_S ("(Ada 83) upper half character not allowed");

      else
         Error_Msg_S ("control character not allowed in string");
      end if;
   end Error_Bad_String_Char;

   -------------------------------
   -- Error_Unterminated_String --
   -------------------------------

   procedure Error_Unterminated_String is
   begin
      --  An interesting little refinement. Consider the following examples:

      --     A := "this is an unterminated string;
      --     A := "this is an unterminated string &
      --     P(A, "this is a parameter that didn't get terminated);

      --  We fiddle a little to do slightly better placement in these cases
      --  also if there is white space at the end of the line we place the
      --  flag at the start of this white space, not at the end. Note that
      --  we only have to test for blanks, since tabs aren't allowed in
      --  strings in the first place and would have caused an error message.

      --  Two more cases that we treat specially are:

      --     A := "this string uses the wrong terminator'
      --     A := "this string uses the wrong terminator' &

      --  In these cases we give a different error message as well

      --  We actually reposition the scan pointer to the point where we
      --  place the flag in these cases, since it seems a better bet on
      --  the original intention.

      while Source (Scan_Ptr - 1) = ' '
        or else Source (Scan_Ptr - 1) = '&'
      loop
         Scan_Ptr := Scan_Ptr - 1;
         Unstore_String_Char;
      end loop;

      --  Check for case of incorrect string terminator, but single quote is
      --  not considered incorrect if the opening terminator misused a single
      --  quote (error message already given).

      if Delimiter /= '''
        and then Source (Scan_Ptr - 1) = '''
      then
         Unstore_String_Char;
         Error_Msg ("incorrect string terminator character", Scan_Ptr - 1);
         return;
      end if;

      if Source (Scan_Ptr - 1) = ';' then
         Scan_Ptr := Scan_Ptr - 1;
         Unstore_String_Char;

         if Source (Scan_Ptr - 1) = ')' then
            Scan_Ptr := Scan_Ptr - 1;
            Unstore_String_Char;
         end if;
      end if;

      Error_Msg_S ("missing string quote");
   end Error_Unterminated_String;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String is
      Slen : Int := Int (Scan_Ptr - Token_Ptr - 2);
      C1   : Character;
      C2   : Character;
      C3   : Character;

   begin
      --  Token_Name is currently set to Error_Name. The following section of
      --  code resets Token_Name to the proper Name_Op_xx value if the string
      --  is a valid operator symbol, otherwise it is left set to Error_Name.

      if Slen = 1 then
         C1 := Source (Token_Ptr + 1);

         case C1 is
            when '=' =>
               Token_Name := Name_Op_Eq;

            when '>' =>
               Token_Name := Name_Op_Gt;

            when '<' =>
               Token_Name := Name_Op_Lt;

            when '+' =>
               Token_Name := Name_Op_Add;

            when '-' =>
               Token_Name := Name_Op_Subtract;

            when '&' =>
               Token_Name := Name_Op_Concat;

            when '*' =>
               Token_Name := Name_Op_Multiply;

            when '/' =>
               Token_Name := Name_Op_Divide;

            when others =>
               null;
         end case;

      elsif Slen = 2 then
         C1 := Source (Token_Ptr + 1);
         C2 := Source (Token_Ptr + 2);

         if C1 = '*' and then C2 = '*' then
            Token_Name := Name_Op_Expon;

         elsif C2 = '=' then

            if C1 = '/' then
               Token_Name := Name_Op_Ne;
            elsif C1 = '<' then
               Token_Name := Name_Op_Le;
            elsif C1 = '>' then
               Token_Name := Name_Op_Ge;
            end if;

         elsif (C1 = 'O' or else C1 = 'o') and then    -- OR
               (C2 = 'R' or else C2 = 'r')
         then
            Token_Name := Name_Op_Or;
         end if;

      elsif Slen = 3 then
         C1 := Source (Token_Ptr + 1);
         C2 := Source (Token_Ptr + 2);
         C3 := Source (Token_Ptr + 3);

         if (C1 = 'A' or else C1 = 'a') and then       -- AND
            (C2 = 'N' or else C2 = 'n') and then
            (C3 = 'D' or else C3 = 'd')
         then
            Token_Name := Name_Op_And;

         elsif (C1 = 'A' or else C1 = 'a') and then    -- ABS
               (C2 = 'B' or else C2 = 'b') and then
               (C3 = 'S' or else C3 = 's')
         then
            Token_Name := Name_Op_Abs;

         elsif (C1 = 'M' or else C1 = 'm') and then    -- MOD
               (C2 = 'O' or else C2 = 'o') and then
               (C3 = 'D' or else C3 = 'd')
         then
            Token_Name := Name_Op_Mod;

         elsif (C1 = 'N' or else C1 = 'n') and then    -- NOT
               (C2 = 'O' or else C2 = 'o') and then
               (C3 = 'T' or else C3 = 't')
         then
            Token_Name := Name_Op_Not;

         elsif (C1 = 'R' or else C1 = 'r') and then    -- REM
               (C2 = 'E' or else C2 = 'e') and then
               (C3 = 'M' or else C3 = 'm')
         then
            Token_Name := Name_Op_Rem;

         elsif (C1 = 'X' or else C1 = 'x') and then    -- XOR
               (C2 = 'O' or else C2 = 'o') and then
               (C3 = 'R' or else C3 = 'r')
         then
            Token_Name := Name_Op_Xor;
         end if;

      end if;

      --  If it is an operator symbol, then Token_Name is set. If it is some
      --  other string value, then Token_Name still contains Error_Name.

      if Token_Name = Error_Name then
         Token := Tok_String_Literal;
         Token_Node := New_Node (N_String_Literal, Token_Ptr);
         Set_Has_Wide_Character (Token_Node, Wide_Character_Found);

      else
         Token := Tok_Operator_Symbol;
         Token_Node := New_Node (N_Operator_Symbol, Token_Ptr);
         Set_Chars (Token_Node, Token_Name);
      end if;

      Set_Strval (Token_Node, String_Literal_Id);

   end Set_String;

----------
-- Slit --
----------

begin
   --  On entry, Scan_Ptr points to the opening character of the string which
   --  is either a percent, double quote, or apostrophe (single quote). The
   --  latter case is an error detected by the character literal circuit.

   Delimiter := Source (Scan_Ptr);
   Accumulate_Checksum (Delimiter);
   Start_String;
   Scan_Ptr := Scan_Ptr + 1;

   --  Loop to scan out characters of string literal

   loop
      C := Source (Scan_Ptr);

      if C = Delimiter then
         Accumulate_Checksum (C);
         Scan_Ptr := Scan_Ptr + 1;
         exit when Source (Scan_Ptr) /= Delimiter;
         Code := Get_Char_Code (C);
         Accumulate_Checksum (C);
         Scan_Ptr := Scan_Ptr + 1;

      else
         if C = '"' and then Delimiter = '%' then
            Error_Msg_S ("quote not allowed in percent delimited string");
            Code := Get_Char_Code (C);
            Scan_Ptr := Scan_Ptr + 1;

         elsif (C = ESC
                 and then
                Wide_Character_Encoding_Method in WC_ESC_Encoding_Method)
           or else
               (C in Upper_Half_Character
                 and then
                Upper_Half_Encoding)
           or else
               (C = '['
                 and then
                Source (Scan_Ptr + 1) = '"'
                 and then
                Identifier_Char (Source (Scan_Ptr + 2)))
         then
            Scan_Wide (Source, Scan_Ptr, Code, Err);
            Accumulate_Checksum (Code);

            if Err then
               Error_Illegal_Wide_Character;
               Code := Get_Char_Code (' ');
            end if;

         else
            Accumulate_Checksum (C);

            if C not in Graphic_Character then
               if C in Line_Terminator then
                  Error_Unterminated_String;
                  exit;

               elsif C in Upper_Half_Character then
                  if Ada_83 then
                     Error_Bad_String_Char;
                  end if;

               else
                  Error_Bad_String_Char;
               end if;
            end if;

            Code := Get_Char_Code (C);
            Scan_Ptr := Scan_Ptr + 1;
         end if;
      end if;

      Store_String_Char (Code);

      if not In_Character_Range (Code) then
         Wide_Character_Found := True;
      end if;
   end loop;

   String_Literal_Id := End_String;
   Set_String;
   return;

end Slit;
