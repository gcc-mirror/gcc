------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                             X G N A T U G N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2008, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  This utility is used to process the source of gnat_ugn.texi to make a
--  version suitable for running through standard Texinfo processor. It is
--  invoked as follows:

--  xgnatugn <target> <in-file> <word-list> [ <out-file> [ <warnings> ] ]

--  1. <target> is the target type of the manual, which is one of:

--     unw       Unix and Windows platforms
--     vms       OpenVMS

--  2. <in-file> is the file name of the Texinfo file to be
--  preprocessed.

--  3. <word-list> is the name of the word list file. This file is used for
--  rewriting the VMS edition. Each line contains a word mapping: The source
--  word in the first column, the target word in the second column. The
--  columns are separated by a '^' character. When preprocessing for VMS, the
--  first word is replaced with the second. (Words consist of letters,
--  digits, and the four characters "?-_~". A sequence of multiple words can
--  be replaced if they are listed in the first column, separated by a single
--  space character. If multiple words are to be replaced, there must be a
--  replacement for each prefix.)

--  4. <out-file> (optional) is the name of the output file. It defaults to
--  gnat_ugn_unw.texi or gnat_ugn_vms.texi, depending on the target.

--  5. <warnings> (optional, and allowed only if <out-file> is explicit)
--  can be any string. If present, it indicates that warning messages are
--  to be output to Standard_Error. If absent, no warning messages are
--  generated.

--  The following steps are performed:

--     In VMS mode

--       Any occurrences of ^alpha^beta^ are replaced by beta. The sequence
--       must fit on a single line, and there can only be one occurrence on a
--       line.

--       Any occurrences of a word in the Ug_Words list are replaced by the
--       appropriate vms equivalents. Note that replacements do not occur
--       within ^alpha^beta^ sequences.

--       Any occurrence of [filename].extension, where extension one of the
--       following:

--           "o", "ads", "adb", "ali", "ada", "atb", "ats", "adc", "c"

--       replaced by the appropriate VMS names (all upper case with .o
--       replaced .OBJ). Note that replacements do not occur within
--       ^alpha^beta^ sequences.

--     In UNW mode

--       Any occurrences of ^alpha^beta^ are replaced by alpha. The sequence
--       must fit on a single line.

--     In both modes

--       The sequence ^^^ is replaced by a single ^. This escape sequence
--       must be used if the literal character ^ is to appear in the
--       output. A line containing this escape sequence may not also contain
--       a ^alpha^beta^ sequence.

--       Process @ifset and @ifclear for the target flags (unw, vms);
--       this is because we have menu problems if we let makeinfo handle
--       these ifset/ifclear pairs.
--       Note: @ifset/@ifclear commands for the edition flags (FSFEDITION,
--       PROEDITION, GPLEDITION) are passed through unchanged

with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.Spitbol;               use GNAT.Spitbol;
with GNAT.Spitbol.Table_VString; use GNAT.Spitbol.Table_VString;

procedure Xgnatugn is

   procedure Usage;
   --  Print usage information. Invoked if an invalid command line is
   --  encountered.

   subtype Sfile is Ada.Streams.Stream_IO.File_Type;

   Output_File : Sfile;
   --  The preprocessed output is written to this file

   type Input_File is record
      Name : VString;
      Data : Ada.Text_IO.File_Type;
      Line : Natural := 0;
   end record;
   --  Records information on an input file. Name and Line are used
   --  in error messages, Line is updated automatically by Get_Line.

   function Get_Line (Input : access Input_File) return String;
   --  Returns a line from Input and performs the necessary
   --  line-oriented checks (length, character set, trailing spaces).

   procedure Put_Line (F : Sfile; S : String);
   procedure Put_Line (F : Sfile; S : VString);
   --  Local version of Put_Line ensures Unix style line endings

   Number_Of_Warnings : Natural := 0;
   Number_Of_Errors   : Natural := 0;
   Warnings_Enabled   : Boolean;

   procedure Error
     (Input        : Input_File;
      At_Character : Natural;
      Message      : String);
   procedure Error
     (Input        : Input_File;
      Message      : String);
   --  Prints a message reporting an error on line Input.Line. If
   --  At_Character is not 0, indicate the exact character at which
   --  the error occurs.

   procedure Warning
     (Input        : Input_File;
      At_Character : Natural;
      Message      : String);
   procedure Warning
     (Input        : Input_File;
      Message      : String);
   --  Like Error, but just print a warning message

   Dictionary_File : aliased Input_File;
   procedure Read_Dictionary_File;
   --  Dictionary_File is opened using the name given on the command
   --  line. It contains the replacements for the Ug_Words list.
   --  Read_Dictionary_File reads Dictionary_File and fills the
   --  Ug_Words table.

   Source_File : aliased Input_File;
   procedure Process_Source_File;
   --  Source_File is opened using the name given on the command line.
   --  It contains the Texinfo source code. Process_Source_File
   --  performs the necessary replacements.

   type Flag_Type is (UNW, VMS, FSFEDITION, PROEDITION, GPLEDITION);
   --  The flags permitted in @ifset or @ifclear commands:
   --
   --  Targets for preprocessing
   --    UNW (Unix and Windows) or VMS
   --
   --  Editions of the manual
   --    FSFEDITION, PROEDITION, or GPLEDITION
   --
   --  Conditional commands for target are processed by xgnatugn
   --
   --  Conditional commands for edition are passed through unchanged

   subtype Target_Type is Flag_Type range UNW .. VMS;
   subtype Edition_Type is Flag_Type range FSFEDITION .. GPLEDITION;

   Target : Target_Type;
   --  The Target variable is initialized using the command line

   Valid_Characters : constant Character_Set :=
                        To_Set (Span => (' ',  '~'));
   --  This array controls which characters are permitted in the input
   --  file (after line breaks have been removed). Valid characters
   --  are all printable ASCII characters and the space character.

   Word_Characters : constant Character_Set :=
                       (To_Set (Ranges =>
                                  (('0', '9'), ('a', 'z'), ('A', 'Z')))
                        or To_Set ("?-_~"));
   --  The characters which are permitted in words. Other (valid)
   --  characters are assumed to be delimiters between words. Note that
   --  this set has to include all characters of the source words of the
   --  Ug_Words dictionary.

   Reject_Trailing_Spaces : constant Boolean := True;
   --  Controls whether Xgnatug rejects superfluous space characters
   --  at the end of lines.

   Maximum_Line_Length     : constant Positive := 79;
   Fatal_Line_Length_Limit : constant Positive := 5000;
   Fatal_Line_Length       : exception;
   --  If Maximum_Line_Length is exceeded in an input file, an error
   --  message is printed. If Fatal_Line_Length is exceeded,
   --  execution terminates with a Fatal_Line_Length exception.

   VMS_Escape_Character : constant Character := '^';
   --  The character used to mark VMS alternatives (^alpha^beta^)

   Extensions : GNAT.Spitbol.Table_VString.Table (20);
   procedure Initialize_Extensions;
   --  This table records extensions and their replacement for
   --  rewriting filenames in the VMS version of the manual.

   function Is_Extension (Extension : String) return Boolean;
   function Get_Replacement_Extension (Extension : String) return String;
   --  These functions query the replacement table. Is_Extension
   --  checks if the given string is a known extension.
   --  Get_Replacement returns the replacement extension.

   Ug_Words : GNAT.Spitbol.Table_VString.Table (200);
   function Is_Known_Word (Word : String) return Boolean;
   function Get_Replacement_Word (Word : String) return String;
   --  The Ug_Words table lists replacement words for the VMS version
   --  of the manual. Is_Known_Word and Get_Replacement_Word query
   --  this table. The table is filled using Read_Dictionary_File.

   function Rewrite_Source_Line (Line : String) return String;
   --  This subprogram takes a line and rewrites it according to Target.
   --  It relies on information in Source_File to generate error messages.

   type Conditional is (Set, Clear);
   procedure Push_Conditional (Cond : Conditional; Flag : Target_Type);
   procedure Pop_Conditional  (Cond : Conditional);
   --  These subprograms deal with conditional processing (@ifset/@ifclear).
   --  They rely on information in Source_File to generate error messages.

   function Currently_Excluding return Boolean;
   --  Returns true if conditional processing directives imply that the
   --  current line should not be included in the output.

   function VMS_Context_Determined return Boolean;
   --  Returns true if, in the current conditional preprocessing context, we
   --  always have a VMS or a non-VMS version, regardless of the value of
   --  Target.

   function In_VMS_Section return Boolean;
   --  Returns True if in an "@ifset vms" section

   procedure Check_No_Pending_Conditional;
   --  Checks that all preprocessing directives have been properly matched by
   --  their @end counterpart. If this is not the case, print an error
   --  message.

   --  The following definitions implement a stack to track the conditional
   --  preprocessing context.

   type Conditional_Context is record
      Starting_Line : Positive;
      Cond          : Conditional;
      Flag          : Flag_Type;
      Excluding     : Boolean;
   end record;

   Conditional_Stack_Depth : constant := 3;

   Conditional_Stack :
     array (1 .. Conditional_Stack_Depth) of Conditional_Context;

   Conditional_TOS : Natural := 0;
   --  Pointer to the Top Of Stack for Conditional_Stack

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line (Standard_Error,
            "usage: xgnatugn TARGET SOURCE DICTIONARY [OUTFILE [WARNINGS]]");
      New_Line;
      Put_Line (Standard_Error, "TARGET is one of:");

      for T in Target_Type'Range loop
         Put_Line (Standard_Error, "  " & Target_Type'Image (T));
      end loop;

      New_Line;
      Put_Line (Standard_Error, "SOURCE is the source file to process.");
      New_Line;
      Put_Line (Standard_Error, "DICTIONARY is the name of a file "
                & "that contains word replacements");
      Put_Line (Standard_Error, "for the VMS version.");
      New_Line;
      Put_Line (Standard_Error,
                "OUT-FILE, if present, is the output file to be created;");
      Put_Line (Standard_Error,
                "If OUT-FILE is absent, the output file is either " &
                "gnat_ugn_unw.texi, ");
      Put_Line (Standard_Error,
                "or gnat_ugn_vms.texi, depending on TARGET.");
      New_Line;
      Put_Line (Standard_Error,
                "WARNINGS, if present, is any string;");
      Put_Line (Standard_Error,
                "it will result in warning messages (e.g., line too long))");
      Put_Line (Standard_Error,
                "being output to Standard_Error.");
   end Usage;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Input : access Input_File) return String is
      Line_Buffer : String (1 .. Fatal_Line_Length_Limit);
      Last        : Natural;

   begin
      Input.Line := Input.Line + 1;
      Get_Line (Input.Data, Line_Buffer, Last);

      if Last = Line_Buffer'Last then
         Error (Input.all, "line exceeds fatal line length limit");
         raise Fatal_Line_Length;
      end if;

      declare
         Line : String renames Line_Buffer (Line_Buffer'First .. Last);

      begin
         for J in Line'Range loop
            if not Is_In (Line (J), Valid_Characters) then
               Error (Input.all, J, "invalid character");
               exit;
            end if;
         end loop;

         if Line'Length > Maximum_Line_Length then
            Warning (Input.all, Maximum_Line_Length + 1, "line too long");
         end if;

         if Reject_Trailing_Spaces
           and then Line'Length > 0
           and then Line (Line'Last) = ' '
         then
            Error (Input.all, Line'Last, "trailing space character");
         end if;

         return Trim (Line, Right);
      end;
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (F : Sfile; S : String) is
   begin
      String'Write (Stream (F), S);
      Character'Write (Stream (F), ASCII.LF);
   end Put_Line;

   procedure Put_Line (F : Sfile; S : VString) is
   begin
      Put_Line (F, To_String (S));
   end Put_Line;

   -----------
   -- Error --
   -----------

   procedure Error
     (Input   : Input_File;
      Message : String)
   is
   begin
      Error (Input, 0, Message);
   end Error;

   procedure Error
     (Input        : Input_File;
      At_Character : Natural;
      Message      : String)
   is
      Line_Image         : constant String := Integer'Image (Input.Line);
      At_Character_Image : constant String := Integer'Image (At_Character);
      --  These variables are required because we have to drop the leading
      --  space character.

   begin
      Number_Of_Errors := Number_Of_Errors + 1;

      if At_Character > 0 then
         Put_Line (Standard_Error,
                   S (Input.Name) & ':'
                   & Line_Image (Line_Image'First + 1 .. Line_Image'Last) & ':'
                   & At_Character_Image (At_Character_Image'First + 1
                                         .. At_Character_Image'Last)
                   & ": "
                   & Message);
      else
         Put_Line (Standard_Error,
                   S (Input.Name) & ':'
                   & Line_Image (Line_Image'First + 1 .. Line_Image'Last)
                   & ": "
                   & Message);
      end if;
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Input   : Input_File;
      Message : String)
   is
   begin
      if Warnings_Enabled then
         Warning (Input, 0, Message);
      end if;
   end Warning;

   procedure Warning
     (Input        : Input_File;
      At_Character : Natural;
      Message      : String)
   is
      Line_Image         : constant String := Integer'Image (Input.Line);
      At_Character_Image : constant String := Integer'Image (At_Character);
      --  These variables are required because we have to drop the leading
      --  space character.

   begin
      if not Warnings_Enabled then
         return;
      end if;

      Number_Of_Warnings := Number_Of_Warnings + 1;

      if At_Character > 0 then
         Put_Line (Standard_Error,
                   S (Input.Name) & ':'
                   & Line_Image (Line_Image'First + 1 .. Line_Image'Last) & ':'
                   & At_Character_Image (At_Character_Image'First + 1
                                         .. At_Character_Image'Last)
                   & ": warning: "
                   & Message);
      else
         Put_Line (Standard_Error,
                   S (Input.Name) & ':'
                   & Line_Image (Line_Image'First + 1 .. Line_Image'Last)
                   & ": warning: "
                   & Message);
      end if;
   end Warning;

   --------------------------
   -- Read_Dictionary_File --
   --------------------------

   procedure Read_Dictionary_File is
   begin
      while not End_Of_File (Dictionary_File.Data) loop
         declare
            Line  : constant String :=
                      Get_Line (Dictionary_File'Access);
            Split : constant Natural :=
                      Index (Line, (1 => VMS_Escape_Character));

         begin
            if Line'Length = 0 then
               Error (Dictionary_File, "empty line in dictionary file");

            elsif Line (Line'First) = ' ' then
               Error (Dictionary_File, 1, "line starts with space character");

            elsif Split = 0 then
               Error (Dictionary_File, "line does not contain "
                      & VMS_Escape_Character & " character");
            else
               declare
                  Source : constant String :=
                             Trim (Line (1 .. Split - 1), Both);
                  Target : constant String :=
                             Trim (Line (Split + 1 .. Line'Last), Both);
                  Two_Spaces : constant Natural :=
                                 Index (Source, "  ");
                  Non_Word_Character : constant Natural :=
                                         Index (Source,
                                                Word_Characters or
                                                  To_Set (" ."),
                                                Outside);

               begin
                  if Two_Spaces /= 0 then
                     Error (Dictionary_File, Two_Spaces,
                            "multiple space characters in source word");
                  end if;

                  if Non_Word_Character /= 0 then
                     Error (Dictionary_File, Non_Word_Character,
                            "illegal character in source word");
                  end if;

                  if Source'Length = 0 then
                     Error (Dictionary_File, "source is empty");

                  elsif Target'Length = 0 then
                     Error (Dictionary_File, "target is empty");

                  else
                     Set (Ug_Words, Source, V (Target));

                     --  Ensure that if Source is a sequence of words
                     --  "WORD1 WORD2 ...", we already have a mapping for
                     --  "WORD1".

                     for J in Source'Range loop
                        if Source (J) = ' ' then
                           declare
                              Prefix : String renames
                                         Source (Source'First .. J - 1);

                           begin
                              if not Is_Known_Word (Prefix) then
                                 Error (Dictionary_File,
                                        "prefix '" & Prefix
                                        & "' not known at this point");
                              end if;
                           end;
                        end if;
                     end loop;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Read_Dictionary_File;

   -------------------------
   -- Rewrite_Source_Line --
   -------------------------

   function Rewrite_Source_Line (Line : String) return String is

      --  We use a simple lexer to split the line into tokens:

      --    Word             consisting entirely of Word_Characters
      --    VMS_Alternative  ^alpha^beta^ replacement (but not ^^^)
      --    Space            a space character
      --    Other            everything else (sequence of non-word characters)
      --    VMS_Error        incomplete VMS alternative
      --    End_Of_Line      no more characters on this line

      --   A sequence of three VMS_Escape_Characters is automatically
      --   collapsed to an Other token.

      type Token_Span is record
         First, Last : Positive;
      end record;
      --  The character range covered by a token in Line

      type Token_Kind is (End_Of_Line, Word, Other,
                          VMS_Alternative, VMS_Error);
      type Token_Record (Kind : Token_Kind := End_Of_Line) is record
         First : Positive;
         case Kind is
            when Word | Other =>
               Span : Token_Span;
            when VMS_Alternative =>
               Non_VMS, VMS : Token_Span;
            when VMS_Error | End_Of_Line =>
               null;
         end case;
      end record;

      Input_Position : Positive := Line'First;
      Token : Token_Record;
      --  The position of the next character to be processed by Next_Token

      procedure Next_Token;
      --  Returns the next token in Line, starting at Input_Position

      Rewritten_Line : VString;
      --  Collects the line as it is rewritten

      procedure Rewrite_Word;
      --  The current token is assumed to be a Word. When processing the VMS
      --  version of the manual, additional tokens are gathered to check if
      --  we have a file name or a sequence of known words.

      procedure Maybe_Rewrite_Extension;
      --  The current token is assumed to be Other. When processing the VMS
      --  version of the manual and the token represents a single dot ".",
      --  the following word is rewritten according to the rules for
      --  extensions.

      VMS_Token_Seen : Boolean := False;
      --  This is set to true if a VMS_Alternative has been encountered, or a
      --  ^^^ token.

      ----------------
      -- Next_Token --
      ----------------

      procedure Next_Token is
         Remaining_Line : String renames Line (Input_Position .. Line'Last);
         Last_Character : Natural;

      begin
         if Remaining_Line'Length = 0 then
            Token := (End_Of_Line, Remaining_Line'First);
            return;
         end if;

         --  ^alpha^beta^, the VMS_Alternative case

         if Remaining_Line (Remaining_Line'First) = VMS_Escape_Character then
            declare
               VMS_Second_Character, VMS_Third_Character : Natural;

            begin
               if VMS_Token_Seen then
                  Error (Source_File, Remaining_Line'First,
                         "multiple " & VMS_Escape_Character
                         & " characters on a single line");
               else
                  VMS_Token_Seen := True;
               end if;

               --  Find the second and third escape character. If one of
               --  them is not present, generate an error token.

               VMS_Second_Character :=
                 Index (Remaining_Line (Remaining_Line'First + 1
                                           .. Remaining_Line'Last),
                        (1 => VMS_Escape_Character));

               if VMS_Second_Character = 0 then
                  Input_Position := Remaining_Line'Last + 1;
                  Token := (VMS_Error, Remaining_Line'First);
                  return;
               end if;

               VMS_Third_Character :=
                 Index (Remaining_Line (VMS_Second_Character + 1
                                           .. Remaining_Line'Last),
                        (1 => VMS_Escape_Character));

               if VMS_Third_Character = 0 then
                  Input_Position := Remaining_Line'Last + 1;
                  Token := (VMS_Error, Remaining_Line'First);
                  return;
               end if;

               --  Consume all the characters we are about to include in
               --  the token.

               Input_Position := VMS_Third_Character + 1;

               --  Check if we are in a ^^^ situation, and return an Other
               --  token in this case.

               if Remaining_Line'First + 1 = VMS_Second_Character
                 and then Remaining_Line'First + 2 = VMS_Third_Character
               then
                  Token := (Other, Remaining_Line'First,
                            (Remaining_Line'First, Remaining_Line'First));
                  return;
               end if;

               Token := (VMS_Alternative, Remaining_Line'First,
                         (Remaining_Line'First + 1, VMS_Second_Character - 1),
                         (VMS_Second_Character + 1, VMS_Third_Character - 1));
               return;
            end;
         end if;                        --  VMS_Alternative

         --  The Word case. Search for characters not in Word_Characters.
         --  We have found a word if the first non-word character is not
         --  the first character in Remaining_Line, i.e. if Remaining_Line
         --  starts with a word character.

         Last_Character := Index (Remaining_Line, Word_Characters, Outside);
         if Last_Character /= Remaining_Line'First then

            --  If we haven't found a character which is not in
            --  Word_Characters, all remaining characters are part of the
            --  current Word token.

            if Last_Character = 0 then
               Last_Character := Remaining_Line'Last + 1;
            end if;

            Input_Position := Last_Character;
            Token := (Word, Remaining_Line'First,
                      (Remaining_Line'First, Last_Character - 1));
            return;
         end if;

         --  Remaining characters are in the Other category. To speed
         --  up processing, we collect them together if there are several
         --  of them.

         Input_Position := Last_Character + 1;
         Token := (Other,
                   Remaining_Line'First,
                   (Remaining_Line'First, Last_Character));
      end Next_Token;

      ------------------
      -- Rewrite_Word --
      ------------------

      procedure Rewrite_Word is
         First_Word : String
           renames Line (Token.Span.First .. Token.Span.Last);

      begin
         --  We do not perform any error checking below, so we can just skip
         --  all processing for the non-VMS version.

         if Target /= VMS then
            Append (Rewritten_Line, First_Word);
            Next_Token;
            return;
         end if;

         if Is_Known_Word (First_Word) then

            --  If we have a word from the dictionary, we look for the
            --  longest possible sequence we can rewrite.

            declare
               Seq : Token_Span := Token.Span;
               Lost_Space : Boolean := False;

            begin
               Next_Token;
               loop
                  if Token.Kind = Other
                    and then Line (Token.Span.First .. Token.Span.Last) = " "
                  then
                     Next_Token;
                     if Token.Kind /= Word
                       or else not Is_Known_Word (Line (Seq.First
                                                        .. Token.Span.Last))
                     then
                        --  When we reach this point, the following
                        --  conditions are true:
                        --
                        --  Seq is a known word.
                        --  The previous token was a space character.
                        --  Seq extended to the current token is not a
                        --  known word.

                        Lost_Space := True;
                        exit;

                     else

                        --  Extend Seq to cover the current (known) word

                        Seq.Last := Token.Span.Last;
                        Next_Token;
                     end if;

                  else
                     --  When we reach this point, the following conditions
                     --  are true:
                     --
                     --  Seq is a known word.
                     --  The previous token was a word.
                     --  The current token is not a space character.

                     exit;
                  end if;
               end loop;

               --  Rewrite Seq, and add the lost space if necessary

               Append (Rewritten_Line,
                       Get_Replacement_Word (Line (Seq.First .. Seq.Last)));
               if Lost_Space then
                  Append (Rewritten_Line, ' ');
               end if;

               --  The unknown token will be processed during the
               --  next iteration of the main loop.
               return;
            end;
         end if;

         Next_Token;

         if Token.Kind = Other
           and then Line (Token.Span.First .. Token.Span.Last) = "."
         then
            --  Deal with extensions

            Next_Token;
            if Token.Kind = Word
              and then Is_Extension (Line (Token.Span.First
                                           .. Token.Span.Last))
            then
               --  We have discovered a file extension. Convert the file
               --  name to upper case.

               Append (Rewritten_Line,
                       Translate (First_Word, Upper_Case_Map) & '.');
               Append (Rewritten_Line,
                       Get_Replacement_Extension
                       (Line (Token.Span.First .. Token.Span.Last)));
               Next_Token;
            else
               --  We already have: Word ".", followed by an unknown token

               Append (Rewritten_Line, First_Word & '.');

               --  The unknown token will be processed during the next
               --  iteration of the main loop.
            end if;

         else
            --  We have an unknown Word, followed by an unknown token.
            --  The unknown token will be processed by the outer loop.

            Append (Rewritten_Line, First_Word);
         end if;
      end Rewrite_Word;

      -----------------------------
      -- Maybe_Rewrite_Extension --
      -----------------------------

      procedure Maybe_Rewrite_Extension is
      begin
         --  Again, we need no special processing in the non-VMS case

         if Target = VMS
           and then Line (Token.Span.First .. Token.Span.Last) = "."
         then
            --  This extension is not preceded by a word, otherwise
            --  Rewrite_Word would have handled it.

            Next_Token;
            if Token.Kind = Word
              and then Is_Extension (Line (Token.Span.First
                                           .. Token.Span.Last))
            then
               Append (Rewritten_Line, '.' & Get_Replacement_Extension
                       (Line (Token.Span.First .. Token.Span.Last)));
               Next_Token;
            else
               Append (Rewritten_Line, '.');
            end if;
         else
            Append (Rewritten_Line, Line (Token.Span.First
                                          .. Token.Span.Last));
            Next_Token;
         end if;
      end Maybe_Rewrite_Extension;

   --  Start of processing for Process_Source_Line

   begin
      --  The following parser recognizes the following special token
      --  sequences:

      --     Word "." Word    rewrite as file name if second word is extension
      --     Word " " Word    rewrite as a single word using Ug_Words table

      Next_Token;
      loop
         case Token.Kind is
            when End_Of_Line =>
               exit;

            when Word  =>
               Rewrite_Word;

            when Other =>
               Maybe_Rewrite_Extension;

            when VMS_Alternative =>
               if VMS_Context_Determined then
                  if (not In_VMS_Section)
                    or else
                    Line (Token.VMS.First .. Token.VMS.Last) /=
                    Line (Token.Non_VMS.First .. Token.Non_VMS.Last)
                  then
                     Warning (Source_File, Token.First,
                              "VMS alternative already determined "
                                & "by conditionals");
                  end if;
               end if;
               if Target = VMS then
                  Append (Rewritten_Line, Line (Token.VMS.First
                                                .. Token.VMS.Last));
               else
                  Append (Rewritten_Line, Line (Token.Non_VMS.First
                                                .. Token.Non_VMS.Last));
               end if;
               Next_Token;

            when VMS_Error =>
               Error (Source_File, Token.First, "invalid VMS alternative");
               Next_Token;
         end case;
      end loop;

      return S (Rewritten_Line);
   end Rewrite_Source_Line;

   -------------------------
   -- Process_Source_File --
   -------------------------

   procedure Process_Source_File is
      Ifset       : constant String := "@ifset ";
      Ifclear     : constant String := "@ifclear ";
      Endsetclear : constant String := "@end ";
      --  Strings to be recognized for conditional processing

   begin
      while not End_Of_File (Source_File.Data) loop
         declare
            Line      : constant String := Get_Line (Source_File'Access);
            Rewritten : constant String := Rewrite_Source_Line (Line);
            --  We unconditionally rewrite the line so that we can check the
            --  syntax of all lines, and not only those which are actually
            --  included in the output.

            Have_Conditional : Boolean := False;
            --  True if we have encountered a conditional preprocessing
            --  directive.

            Cond : Conditional;
            --  The kind of the directive

            Flag : Flag_Type;
            --  Its flag

         begin
            --  If the line starts with @ifset or @ifclear, we try to convert
            --  the following flag to one of our flag types. If we fail,
            --  Have_Conditional remains False.

            if Line'Length >= Ifset'Length
              and then Line (1 .. Ifset'Length) = Ifset
            then
               Cond := Set;

               declare
                  Arg : constant String :=
                          Trim (Line (Ifset'Length + 1 .. Line'Last), Both);

               begin
                  Flag := Flag_Type'Value (Arg);
                  Have_Conditional := True;

                  case Flag is
                     when Target_Type =>
                        if Translate (Target_Type'Image (Flag),
                                      Lower_Case_Map)
                                                      /= Arg
                        then
                           Error (Source_File, "flag has to be lowercase");
                        end if;

                     when Edition_Type =>
                        null;
                  end case;
               exception
                  when Constraint_Error =>
                     Error (Source_File, "unknown flag for '@ifset'");
               end;

            elsif Line'Length >= Ifclear'Length
              and then Line (1 .. Ifclear'Length) = Ifclear
            then
               Cond := Clear;

               declare
                  Arg : constant String :=
                          Trim (Line (Ifclear'Length + 1 .. Line'Last), Both);

               begin
                  Flag := Flag_Type'Value (Arg);
                  Have_Conditional := True;

                  case Flag is
                     when Target_Type =>
                        if Translate (Target_Type'Image (Flag),
                                      Lower_Case_Map)
                                                      /= Arg
                        then
                           Error (Source_File, "flag has to be lowercase");
                        end if;

                     when Edition_Type =>
                        null;
                  end case;
               exception
                  when Constraint_Error =>
                     Error (Source_File, "unknown flag for '@ifclear'");
               end;
            end if;

            if Have_Conditional and (Flag in Target_Type) then

               --  We create a new conditional context and suppress the
               --  directive in the output.

               Push_Conditional (Cond, Flag);

            elsif Line'Length >= Endsetclear'Length
              and then Line (1 .. Endsetclear'Length) = Endsetclear
              and then (Flag in Target_Type)
            then
               --  The '@end ifset'/'@end ifclear' case is handled here. We
               --  have to pop the conditional context.

               declare
                  First, Last : Natural;

               begin
                  Find_Token (Source => Line (Endsetclear'Length + 1
                                              .. Line'Length),
                              Set    => Letter_Set,
                              Test   => Inside,
                              First  => First,
                              Last   => Last);

                  if Last = 0 then
                     Error (Source_File, "'@end' without argument");
                  else
                     if Line (First .. Last) = "ifset" then
                        Have_Conditional := True;
                        Cond := Set;
                     elsif Line (First .. Last) = "ifclear" then
                        Have_Conditional := True;
                        Cond := Clear;
                     end if;

                     if Have_Conditional then
                        Pop_Conditional (Cond);
                     end if;

                     --  We fall through to the ordinary case for other @end
                     --  directives.

                  end if;               --  @end without argument
               end;
            end if;                     --  Have_Conditional

            if (not Have_Conditional) or (Flag in Edition_Type) then

               --  The ordinary case

               if not Currently_Excluding then
                  Put_Line (Output_File, Rewritten);
               end if;
            end if;
         end;
      end loop;

      Check_No_Pending_Conditional;
   end Process_Source_File;

   ---------------------------
   -- Initialize_Extensions --
   ---------------------------

   procedure Initialize_Extensions is

      procedure Add (Extension : String);
      --  Adds an extension which is replaced with itself (in upper
      --  case).

      procedure Add (Extension, Replacement : String);
      --  Adds an extension with a custom replacement

      ---------
      -- Add --
      ---------

      procedure Add (Extension : String) is
      begin
         Add (Extension, Translate (Extension, Upper_Case_Map));
      end Add;

      procedure Add (Extension, Replacement : String) is
      begin
         Set (Extensions, Extension, V (Replacement));
      end Add;

   --  Start of processing for Initialize_Extensions

   begin
      --  To avoid performance degradation, increase the constant in the
      --  definition of Extensions above if you add more extensions here.

      Add ("o", "OBJ");
      Add ("ads");
      Add ("adb");
      Add ("ali");
      Add ("ada");
      Add ("atb");
      Add ("ats");
      Add ("adc");
      Add ("c");
   end Initialize_Extensions;

   ------------------
   -- Is_Extension --
   ------------------

   function Is_Extension (Extension : String) return Boolean is
   begin
      return Present (Extensions, Extension);
   end Is_Extension;

   -------------------------------
   -- Get_Replacement_Extension --
   -------------------------------

   function Get_Replacement_Extension (Extension : String) return String is
   begin
      return S (Get (Extensions, Extension));
   end Get_Replacement_Extension;

   -------------------
   -- Is_Known_Word --
   -------------------

   function Is_Known_Word (Word : String) return Boolean is
   begin
      return Present (Ug_Words, Word);
   end Is_Known_Word;

   --------------------------
   -- Get_Replacement_Word --
   --------------------------

   function Get_Replacement_Word (Word : String) return String is
   begin
      return S (Get (Ug_Words, Word));
   end Get_Replacement_Word;

   ----------------------
   -- Push_Conditional --
   ----------------------

   procedure Push_Conditional (Cond : Conditional; Flag : Target_Type) is
      Will_Exclude : Boolean;

   begin
      --  If we are already in an excluding context, inherit this property,
      --  otherwise calculate it from scratch.

      if Conditional_TOS > 0
        and then Conditional_Stack (Conditional_TOS).Excluding
      then
         Will_Exclude := True;
      else
         case Cond is
            when Set =>
               Will_Exclude := Flag /= Target;
            when Clear =>
               Will_Exclude := Flag = Target;
         end case;
      end if;

      --  Check if the current directive is pointless because of a previous,
      --  enclosing directive.

      for J in 1 .. Conditional_TOS loop
         if Conditional_Stack (J).Flag = Flag then
            Warning (Source_File, "directive without effect because of line"
                     & Integer'Image (Conditional_Stack (J).Starting_Line));
         end if;
      end loop;

      Conditional_TOS := Conditional_TOS + 1;
      Conditional_Stack (Conditional_TOS) :=
        (Starting_Line => Source_File.Line,
         Cond          => Cond,
         Flag          => Flag,
         Excluding     => Will_Exclude);
   end Push_Conditional;

   ---------------------
   -- Pop_Conditional --
   ---------------------

   procedure Pop_Conditional (Cond : Conditional) is
   begin
      if Conditional_TOS > 0 then
         case Cond is
            when Set =>
               if Conditional_Stack (Conditional_TOS).Cond /= Set then
                  Error (Source_File,
                         "'@end ifset' does not match '@ifclear' at line"
                         & Integer'Image (Conditional_Stack
                                          (Conditional_TOS).Starting_Line));
               end if;

            when Clear =>
               if Conditional_Stack (Conditional_TOS).Cond /= Clear then
                  Error (Source_File,
                         "'@end ifclear' does not match '@ifset' at line"
                         & Integer'Image (Conditional_Stack
                                          (Conditional_TOS).Starting_Line));
               end if;
         end case;

         Conditional_TOS := Conditional_TOS - 1;

      else
         case Cond is
            when Set =>
               Error (Source_File,
                      "'@end ifset' without corresponding '@ifset'");

            when Clear =>
               Error (Source_File,
                      "'@end ifclear' without corresponding '@ifclear'");
         end case;
      end if;
   end Pop_Conditional;

   -------------------------
   -- Currently_Excluding --
   -------------------------

   function Currently_Excluding return Boolean is
   begin
      return Conditional_TOS > 0
        and then Conditional_Stack (Conditional_TOS).Excluding;
   end Currently_Excluding;

   ----------------------------
   -- VMS_Context_Determined --
   ----------------------------

   function VMS_Context_Determined return Boolean is
   begin
      for J in 1 .. Conditional_TOS loop
         if Conditional_Stack (J).Flag = VMS then
            return True;
         end if;
      end loop;

      return False;
   end VMS_Context_Determined;

   --------------------
   -- In_VMS_Section --
   --------------------

   function In_VMS_Section return Boolean is
   begin
      for J in 1 .. Conditional_TOS loop
         if Conditional_Stack (J).Flag = VMS then
            return Conditional_Stack (J).Cond = Set;
         end if;
      end loop;

      return False;
   end In_VMS_Section;

   ----------------------------------
   -- Check_No_Pending_Conditional --
   ----------------------------------

   procedure Check_No_Pending_Conditional is
   begin
      for J in 1 .. Conditional_TOS loop
         case Conditional_Stack (J).Cond is
            when Set =>
               Error (Source_File, "Missing '@end ifset' for '@ifset' at line"
                      & Integer'Image (Conditional_Stack (J).Starting_Line));

            when Clear =>
               Error (Source_File,
                      "Missing '@end ifclear' for '@ifclear' at line"
                      & Integer'Image (Conditional_Stack (J).Starting_Line));
         end case;
      end loop;
   end Check_No_Pending_Conditional;

--  Start of processing for Xgnatugn

   Valid_Command_Line : Boolean;
   Output_File_Name   : VString;

begin
   Initialize_Extensions;
   Valid_Command_Line := Argument_Count in 3 .. 5;

   --  First argument: Target

   if Valid_Command_Line then
      begin
         Target := Flag_Type'Value (Argument (1));

         if not Target'Valid then
            Valid_Command_Line := False;
         end if;

      exception
         when Constraint_Error =>
            Valid_Command_Line := False;
      end;
   end if;

   --  Second argument: Source_File

   if Valid_Command_Line then
      begin
         Source_File.Name := V (Argument (2));
         Open (Source_File.Data, In_File, Argument (2));

      exception
         when Ada.Text_IO.Name_Error =>
            Valid_Command_Line := False;
      end;
   end if;

   --  Third argument: Dictionary_File

   if Valid_Command_Line then
      begin
         Dictionary_File.Name := V (Argument (3));
         Open (Dictionary_File.Data, In_File, Argument (3));

      exception
         when Ada.Text_IO.Name_Error =>
            Valid_Command_Line := False;
      end;
   end if;

   --  Fourth argument: Output_File

   if Valid_Command_Line then
      if Argument_Count in 4 .. 5 then
         Output_File_Name := V (Argument (4));
      else
         case Target is
            when UNW =>
               Output_File_Name := V ("gnat_ugn_unw.texi");
            when VMS =>
               Output_File_Name := V ("gnat_ugn_vms.texi");
         end case;
      end if;

      Warnings_Enabled := Argument_Count = 5;

      begin
         Create (Output_File, Out_File, S (Output_File_Name));

      exception
         when Ada.Text_IO.Name_Error | Ada.Text_IO.Use_Error =>
            Valid_Command_Line := False;
      end;
   end if;

   if not Valid_Command_Line then
      Usage;
      Set_Exit_Status (Failure);

   else
      Read_Dictionary_File;
      Close (Dictionary_File.Data);

      --  Main processing starts here

      Process_Source_File;
      Close (Output_File);
      Close (Source_File.Data);

      New_Line (Standard_Error);

      if Number_Of_Warnings = 0 then
         Put_Line (Standard_Error, " NO Warnings");

      else
         Put (Standard_Error, Integer'Image (Number_Of_Warnings));
         Put (Standard_Error, " Warning");

         if Number_Of_Warnings > 1 then
            Put (Standard_Error, "s");
         end if;

         New_Line (Standard_Error);
      end if;

      if Number_Of_Errors = 0 then
         Put_Line (Standard_Error, " NO Errors");

      else
         Put (Standard_Error, Integer'Image (Number_Of_Errors));
         Put (Standard_Error, " Error");

         if Number_Of_Errors > 1 then
            Put (Standard_Error, "s");
         end if;

         New_Line (Standard_Error);
      end if;

      if Number_Of_Errors /= 0  then
         Set_Exit_Status (Failure);
      else
         Set_Exit_Status (Success);
      end if;
   end if;
end Xgnatugn;
