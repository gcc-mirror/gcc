------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T P R E P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1996-2002, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Heap_Sort_G;
with GNAT.Command_Line;

with Gnatvsn;

procedure GNATprep is

   type Strptr is access String;

   Usage_Error : exception;
   --  Raised if a usage error is detected, causes termination of processing
   --  with an appropriate error message and error exit status set.

   Fatal_Error : exception;
   --  Exception raised if fatal error detected

   Expression_Error : exception;
   --  Exception raised when an invalid boolean expression is found
   --  on a preprocessor line

   ------------------------
   -- Argument Line Data --
   ------------------------

   Outfile_Name : Strptr;
   Deffile_Name : Strptr;
   --  Names of files

   type Input;
   type Input_Ptr is access Input;
   type Input is record
      File     : File_Type;
      Next     : Input_Ptr;
      Prev     : Input_Ptr;
      Name     : Strptr;
      Line_Num : Natural := 0;
   end record;
   --  Data for the current input file (main input file or included file
   --  or definition file).

   Infile  : Input_Ptr := new Input;
   Outfile : File_Type;
   Deffile : File_Type;

   Opt_Comment_Deleted_Lines : Boolean := False;  -- Set if -c switch set
   Blank_Deleted_Lines       : Boolean := False;  -- Set if -b switch set
   List_Symbols              : Boolean := False;  -- Set if -s switch set
   Source_Ref_Pragma         : Boolean := False;  -- Set if -r switch set
   Undefined_Is_False        : Boolean := False;  -- Set if -u switch set
   --  Record command line options

   ---------------------------
   -- Definitions File Data --
   ---------------------------

   Num_Syms : Natural := 0;
   --  Number of symbols defined in definitions file

   Symbols : array (0 .. 10_000) of Strptr;
   Values  : array (0 .. 10_000) of Strptr;
   --  Symbol names and values. Note that the zero'th element is used only
   --  during the call to Sort (to hold a temporary value, as required by
   --  the GNAT.Heap_Sort_G interface).

   ---------------------
   -- Input File Data --
   ---------------------

   Current_File_Name : Strptr;
   --  Holds name of file being read (definitions file or input file)

   Line_Buffer : String (1 .. 20_000);
   --  Hold one line

   Line_Length : Natural;
   --  Length of line in Line_Buffer

   Ptr : Natural;
   --  Input scan pointer for line in Line_Buffer

   type Keyword is (K_Not, K_Then, K_If, K_Else, K_End, K_Elsif,
                    K_And, K_Or, K_Open_Paren, K_Close_Paren,
                    K_Defined, K_Andthen, K_Orelse, K_Equal, K_Include,
                    K_None);
   --  Keywords that are recognized on preprocessor lines. K_None indicates
   --  that no keyword was present.

   K : Keyword;
   --  Scanned keyword

   Start_Sym, End_Sym : Natural;
   --  First and last positions of scanned symbol

   Num_Errors : Natural := 0;
   --  Number of errors detected

   -----------------------
   -- Preprocessor Data --
   -----------------------

   --  The following record represents the state of an #if structure:

   type PP_Rec is record
      If_Line : Positive;
      --  Line number for #if line

      If_Name : Strptr;
      --  File name of #if line

      Else_Line : Natural;
      --  Line number for #else line, zero = no else seen yet

      Deleting : Boolean;
      --  True if lines currently being deleted

      Match_Seen : Boolean;
      --  True if either the #if condition or one of the previously seen
      --  #elsif lines was true, meaning that any future #elsif sections
      --  or the #else section, is to be deleted.

   end record;

   PP_Depth : Natural;
   --  Preprocessor #if nesting level. A value of zero means that we are
   --  outside any #if structure.

   PP : array (0 .. 100) of PP_Rec;
   --  Stack of records showing state of #if structures. PP (1) is the
   --  outer level entry, and PP (PP_Depth) is the active entry. PP (0)
   --  contains a dummy entry whose Deleting flag is always set to False.

   -----------------
   -- Subprograms --
   -----------------

   function At_End_Of_Line return Boolean;
   --  First advances Ptr using Skip_Spaces. Then returns True if Ptr is
   --  either at the end of the line, or at a -- comment sequence.

   procedure Error (Msg : String);
   --  Post error message with given text. The line number is taken from
   --  Infile.Line_Num, and the column number from Ptr.

   function Eval_Condition
     (Parenthesis : Natural := 0;
      Do_Eval     : Boolean := True)
      return        Boolean;
   --  Eval the condition found in the current Line. The condition can
   --  include any of the 'and', 'or', 'not', and parenthesis subexpressions.
   --  If Line is an invalid expression, then Expression_Error is raised,
   --  after an error message has been printed. Line can include 'then'
   --  followed by a comment, which is automatically ignored. If Do_Eval
   --  is False, then the expression is not evaluated at all, and symbols
   --  are just skipped.

   function Eval_Symbol (Do_Eval : Boolean) return Boolean;
   --  Read and evaluate the next symbol or expression (A,  A'Defined,  A=...)
   --  If it is followed by 'Defined or an equality test, read as many symbols
   --  as needed. Do_Eval has the same meaning as in Eval_Condition

   procedure Help_Page;
   --  Print a help page to summarize the usage of gnatprep

   function Image (N : Natural) return String;
   --  Returns Natural'Image (N) without the initial space

   function Is_Preprocessor_Line return Boolean;
   --  Tests if current line is a preprocessor line, i.e. that its first
   --  non-blank character is a # character. If so, then a result of True
   --  is returned, and Ptr is set to point to the character following the
   --  # character. If not, False is returned and Ptr is undefined.

   procedure No_Junk;
   --  Make sure no junk is present on a preprocessor line. Ptr points past
   --  the scanned preprocessor syntax.

   function OK_Identifier (S : String) return Boolean;
   --  Tests if given referenced string is valid Ada identifier

   function Matching_Strings (S1, S2 : String) return Boolean;
   --  Check if S1 and S2 are the same string (this is a case independent
   --  comparison, lower and upper case letters are considered to match).
   --  Duplicate quotes in S2 are considered as a single quote ("" => ")

   procedure Parse_Def_File;
   --  Parse the deffile given by the user

   function Scan_Keyword return Keyword;
   --  Advances Ptr to end of line or next non-blank using Skip_Spaces. Then
   --  attempts to scan out a recognized keyword. if a recognized keyword is
   --  found, sets Ptr past it, and returns the code for the keyword, if not,
   --  then Ptr is left unchanged pointing to a non-blank character or to the
   --  end of the line.

   function Symbol_Scanned return Boolean;
   --  On entry, Start_Sym is set to the first character of an identifier
   --  symbol to be scanned out. On return, End_Sym is set to the last
   --  character of the identifier, and the result indicates if the scanned
   --  symbol is a valid identifier (True = valid). Ptr is not changed.

   procedure Skip_Spaces;
   --  Skips Ptr past tabs and spaces to next non-blank, or one character
   --  past the end of line.

   function Variable_Index (Name : String) return Natural;
   --  Returns the index of the variable in the table. If the variable is not
   --  found, returns Natural'Last

   --------------------
   -- At_End_Of_Line --
   --------------------

   function At_End_Of_Line return Boolean is
   begin
      Skip_Spaces;

      return Ptr > Line_Length
        or else
          (Ptr < Line_Length and then Line_Buffer (Ptr .. Ptr + 1) = "--");
   end At_End_Of_Line;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
      L : constant String := Natural'Image (Infile.Line_Num);
      C : constant String := Natural'Image (Ptr);

   begin
      Put (Standard_Error, Current_File_Name.all);
      Put (Standard_Error, ':');
      Put (Standard_Error, L (2 .. L'Length));
      Put (Standard_Error, ':');
      Put (Standard_Error, C (2 .. C'Length));
      Put (Standard_Error, ": ");

      Put_Line (Standard_Error, Msg);
      Num_Errors := Num_Errors + 1;
   end Error;

   --------------------
   -- Eval_Condition --
   --------------------

   function Eval_Condition
     (Parenthesis : Natural := 0;
      Do_Eval     : Boolean := True)
      return        Boolean
   is
      Symbol_Is_True : Boolean := False; -- init to avoid warning
      K              : Keyword;

   begin
      --  Find the next subexpression

      K := Scan_Keyword;

      case K is
         when K_None =>
            Symbol_Is_True := Eval_Symbol (Do_Eval);

         when K_Not =>

            --  Not applies to the next subexpression (either a simple
            --  evaluation like  A or A'Defined, or a parenthesis expression)

            K := Scan_Keyword;

            if K = K_Open_Paren then
               Symbol_Is_True := not Eval_Condition (Parenthesis + 1, Do_Eval);

            elsif K = K_None then
               Symbol_Is_True := not Eval_Symbol (Do_Eval);

            else
               Ptr := Start_Sym;  --  Puts the keyword back
            end if;

         when K_Open_Paren =>
            Symbol_Is_True := Eval_Condition (Parenthesis + 1, Do_Eval);

         when others =>
            Ptr := Start_Sym;
            Error ("invalid syntax in preprocessor line");
            raise Expression_Error;
      end case;

      --  Do we have a compound expression with AND, OR, ...

      K := Scan_Keyword;
      case K is
         when K_None =>
            if not At_End_Of_Line then
               Error ("Invalid Syntax at end of line");
               raise Expression_Error;
            end if;

            if Parenthesis /= 0 then
               Error ("Unmatched opening parenthesis");
               raise Expression_Error;
            end if;

            return Symbol_Is_True;

         when K_Then =>
            if Parenthesis /= 0 then
               Error ("Unmatched opening parenthesis");
               raise Expression_Error;
            end if;

            return Symbol_Is_True;

         when K_Close_Paren =>
            if Parenthesis = 0 then
               Error ("Unmatched closing parenthesis");
               raise Expression_Error;
            end if;

            return Symbol_Is_True;

         when K_And =>
            return Symbol_Is_True and Eval_Condition (Parenthesis, Do_Eval);

         when K_Andthen =>
            if not Symbol_Is_True then

               --  Just skip the symbols for the remaining part

               Symbol_Is_True := Eval_Condition (Parenthesis, False);
               return False;

            else
               return Eval_Condition (Parenthesis, Do_Eval);
            end if;

         when K_Or =>
            return Symbol_Is_True or Eval_Condition (Parenthesis, Do_Eval);

         when K_Orelse =>
            if Symbol_Is_True then

               --  Just skip the symbols for the remaining part

               Symbol_Is_True := Eval_Condition (Parenthesis, False);
               return True;

            else
               return Eval_Condition (Parenthesis, Do_Eval);
            end if;

         when others =>
            Error ("invalid syntax in preprocessor line");
            raise Expression_Error;
      end case;

   end Eval_Condition;

   -----------------
   -- Eval_Symbol --
   -----------------

   function Eval_Symbol (Do_Eval : Boolean) return Boolean is
      Sym            : constant String := Line_Buffer (Start_Sym .. End_Sym);
      K              : Keyword;
      Index          : Natural;
      Symbol_Defined : Boolean := False;
      Symbol_Is_True : Boolean := False;

   begin
      --  Read the symbol

      Skip_Spaces;
      Start_Sym := Ptr;

      if not Symbol_Scanned then
         Error ("invalid symbol name");
         raise Expression_Error;
      end if;

      Ptr := End_Sym + 1;

      --  Test if we have a simple test (A) or a more complicated one
      --  (A'Defined)

      K := Scan_Keyword;

      if K /= K_Defined and then K /= K_Equal then
         Ptr := Start_Sym;  --  Puts the keyword back
      end if;

      Index := Variable_Index (Sym);

      case K is
         when K_Defined =>
            Symbol_Defined := Index /= Natural'Last;
            Symbol_Is_True := Symbol_Defined;

         when K_Equal =>

            --  Read the second part of the statement

            Skip_Spaces;
            Start_Sym := Ptr;

            if not Symbol_Scanned
              and then End_Sym < Start_Sym
            then
               Error ("No right part for the equality test");
               raise Expression_Error;
            end if;

            Ptr := End_Sym + 1;

            --  If the variable was not found

            if Do_Eval then
               if Index = Natural'Last then
                  if not Undefined_Is_False then
                     Error ("symbol name """ & Sym &
                            """ is not defined in definitions file");
                  end if;

               else
                  declare
                     Right : constant String
                       := Line_Buffer (Start_Sym .. End_Sym);
                     Index_R : Natural;
                  begin
                     if Right (Right'First) = '"' then
                        Symbol_Is_True :=
                          Matching_Strings
                          (Values (Index).all,
                           Right (Right'First + 1 .. Right'Last - 1));
                     else
                        Index_R := Variable_Index (Right);
                        if Index_R = Natural'Last then
                           Error ("Variable " & Right & " in test is "
                                  & "not defined");
                           raise Expression_Error;
                        else
                           Symbol_Is_True :=
                             Matching_Strings (Values (Index).all,
                                               Values (Index_R).all);
                        end if;
                     end if;
                  end;
               end if;
            end if;

         when others =>

            if Index = Natural'Last then

               Symbol_Defined := False;
               if Do_Eval and then not Symbol_Defined then
                  if Undefined_Is_False then
                     Symbol_Defined := True;
                     Symbol_Is_True := False;

                  else
                     Error
                       ("symbol name """ & Sym &
                        """ is not defined in definitions file");
                  end if;
               end if;

            elsif not Do_Eval then
               Symbol_Is_True := True;

            elsif Matching_Strings (Values (Index).all, "True") then
               Symbol_Is_True := True;

            elsif Matching_Strings (Values (Index).all, "False") then
               Symbol_Is_True := False;

            else
               Error ("symbol value is not True or False");
               Symbol_Is_True := False;
            end if;

      end case;

      return Symbol_Is_True;
   end Eval_Symbol;

   ---------------
   -- Help_Page --
   ---------------

   procedure Help_Page is
   begin
      Put_Line (Standard_Error,
                "GNAT Preprocessor " &
                Gnatvsn.Gnat_Version_String &
                " Copyright 1996-2002 Free Software Foundation, Inc.");
      Put_Line (Standard_Error,
                "Usage: gnatprep [-bcrsu] [-Dsymbol=value] infile " &
                "outfile [deffile]");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "  infile     Name of the input file");
      Put_Line (Standard_Error, "  outfile    Name of the output file");
      Put_Line (Standard_Error, "  deffile    Name of the definition file");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "gnatprep switches:");
      Put_Line (Standard_Error, "   -b  Replace preprocessor lines by " &
                "blank lines");
      Put_Line (Standard_Error, "   -c  Keep preprocessor lines as comments");
      Put_Line (Standard_Error, "   -D  Associate symbol with value");
      Put_Line (Standard_Error, "   -r  Generate Source_Reference pragma");
      Put_Line (Standard_Error, "   -s  Print a sorted list of symbol names " &
                "and values");
      Put_Line (Standard_Error, "   -u  Treat undefined symbols as FALSE");
      New_Line (Standard_Error);
   end Help_Page;

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
      Result : constant String := Natural'Image (N);
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Image;

   --------------------------
   -- Is_Preprocessor_Line --
   --------------------------

   function Is_Preprocessor_Line return Boolean is
   begin
      Ptr := 1;

      while Ptr <= Line_Length loop
         if Line_Buffer (Ptr) = '#' then
            Ptr := Ptr + 1;
            return True;

         elsif Line_Buffer (Ptr) > ' ' then
            return False;

         else
            Ptr := Ptr + 1;
         end if;
      end loop;

      return False;
   end Is_Preprocessor_Line;

   ----------------------
   -- Matching_Strings --
   ----------------------

   function Matching_Strings (S1, S2 : String) return Boolean is
      S2_Index : Integer := S2'First;

   begin
      for S1_Index in S1'Range loop

         if To_Upper (S1 (S1_Index)) /= To_Upper (S2 (S2_Index)) then
            return False;

         else
            if S2 (S2_Index) = '"'
              and then S2_Index < S2'Last
              and then S2 (S2_Index + 1) = '"'
            then
               S2_Index := S2_Index + 2;
            else
               S2_Index := S2_Index + 1;
            end if;

            --  If S2 was too short then

            if S2_Index > S2'Last and then S1_Index < S1'Last then
               return False;
            end if;
         end if;
      end loop;

      return S2_Index = S2'Last + 1;
   end Matching_Strings;

   -------------
   -- No_Junk --
   -------------

   procedure No_Junk is
   begin
      Skip_Spaces;

      if Ptr = Line_Length
        or else (Ptr < Line_Length
                   and then Line_Buffer (Ptr .. Ptr + 1) /= "--")
      then
         Error ("extraneous text on preprocessor line ignored");
      end if;
   end No_Junk;

   -------------------
   -- OK_Identifier --
   -------------------

   function OK_Identifier (S : String) return Boolean is
      P : Natural := S'First;

   begin
      if S'Length /= 0 and then S (P) = Character'Val (39) then -- '''
         P := P + 1;
      end if;

      if S'Length = 0
        or else not Is_Letter (S (P))
      then
         return False;

      else
         while P <= S'Last loop
            if Is_Letter (S (P)) or Is_Digit (S (P)) then
               null;

            elsif S (P) = '_'
              and then P < S'Last
              and then S (P + 1) /= '_'
            then
               null;

            else
               return False;
            end if;

            P := P + 1;
         end loop;

         return True;
      end if;
   end OK_Identifier;

   --------------------
   -- Parse_Def_File --
   --------------------

   procedure Parse_Def_File is
   begin
      Open (Deffile, In_File, Deffile_Name.all);

      --  Initialize data for procedure Error

      Infile.Line_Num := 0;
      Current_File_Name := Deffile_Name;

      --  Loop through lines in symbol definitions file

      while not End_Of_File (Deffile) loop
         Get_Line (Deffile, Line_Buffer, Line_Length);
         Infile.Line_Num := Infile.Line_Num + 1;

         Ptr := 1;
         Skip_Spaces;

         if Ptr > Line_Length
           or else (Ptr < Line_Length
                    and then
                    Line_Buffer (Ptr .. Ptr + 1) = "--")
         then
            goto Continue;
         end if;

         Start_Sym := Ptr;

         if not Symbol_Scanned then
            Error ("invalid symbol identifier """ &
                   Line_Buffer (Start_Sym .. End_Sym) &
                   '"');
            goto Continue;
         end if;

         Ptr := End_Sym + 1;
         Skip_Spaces;

         if Ptr >= Line_Length
           or else Line_Buffer (Ptr .. Ptr + 1) /= ":="
         then
            Error ("missing "":="" in symbol definition line");
            goto Continue;
         end if;

         Ptr := Ptr + 2;
         Skip_Spaces;

         Num_Syms := Num_Syms + 1;
         Symbols (Num_Syms) := new String'(Line_Buffer (Start_Sym .. End_Sym));

         Start_Sym := Ptr;
         End_Sym := Ptr - 1;

         if At_End_Of_Line then
            null;

         elsif Line_Buffer (Start_Sym) = '"' then
            End_Sym := End_Sym + 1;
            loop
               End_Sym := End_Sym + 1;

               if End_Sym > Line_Length then
                  Error ("no closing quote for string constant");
                  goto Continue;

               elsif End_Sym < Line_Length
                 and then Line_Buffer (End_Sym .. End_Sym + 1) = """"""
               then
                  End_Sym := End_Sym + 1;

               elsif Line_Buffer (End_Sym) = '"' then
                  exit;
               end if;
            end loop;

         else
            End_Sym := Ptr - 1;

            while End_Sym < Line_Length
              and then (Is_Alphanumeric (Line_Buffer (End_Sym + 1))
                        or else
                        Line_Buffer (End_Sym + 1) = '_'
                        or else
                        Line_Buffer (End_Sym + 1) = '.')
            loop
               End_Sym := End_Sym + 1;
            end loop;

            Ptr := End_Sym + 1;

            if not At_End_Of_Line then
               Error ("incorrect symbol value syntax");
               goto Continue;
            end if;
         end if;

         Values (Num_Syms) := new String'(Line_Buffer (Start_Sym .. End_Sym));

         <<Continue>>
         null;
      end loop;

   exception
      --  Could not open the file

      when Name_Error =>
         Put_Line (Standard_Error, "cannot open " & Deffile_Name.all);
         raise Fatal_Error;
   end Parse_Def_File;

   ------------------
   -- Scan_Keyword --
   ------------------

   function Scan_Keyword return Keyword is
      Kptr : constant Natural := Ptr;

   begin
      Skip_Spaces;
      Start_Sym := Ptr;

      if Symbol_Scanned then

         --  If the symbol was the last thing on the line, End_Sym will
         --  point too far in Line_Buffer

         if End_Sym > Line_Length then
            End_Sym := Line_Length;
         end if;

         Ptr  := End_Sym + 1;

         declare
            Sym : constant String := Line_Buffer (Start_Sym .. End_Sym);

         begin
            if    Matching_Strings (Sym, "not") then
               return K_Not;

            elsif Matching_Strings (Sym, "then") then
               return K_Then;

            elsif Matching_Strings (Sym, "if") then
               return K_If;

            elsif Matching_Strings (Sym, "else") then
               return K_Else;

            elsif Matching_Strings (Sym, "end") then
               return K_End;

            elsif Matching_Strings (Sym, "elsif") then
               return K_Elsif;

            elsif Matching_Strings (Sym, "and") then
               if Scan_Keyword = K_Then then
                  Start_Sym := Kptr;
                  return K_Andthen;
               else
                  Ptr := Start_Sym;  --  Put back the last keyword read
                  Start_Sym := Kptr;
                  return K_And;
               end if;

            elsif Matching_Strings (Sym, "or") then
               if Scan_Keyword = K_Else then
                  Start_Sym := Kptr;
                  return K_Orelse;
               else
                  Ptr := Start_Sym;  --  Put back the last keyword read
                  Start_Sym := Kptr;
                  return K_Or;
               end if;

            elsif Matching_Strings (Sym, "'defined") then
               return K_Defined;

            elsif Matching_Strings (Sym, "include") then
               return K_Include;

            elsif Sym = "(" then
               return K_Open_Paren;

            elsif Sym = ")" then
               return K_Close_Paren;

            elsif Sym = "=" then
               return K_Equal;
            end if;
         end;
      end if;

      Ptr := Kptr;
      return K_None;
   end Scan_Keyword;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      while Ptr <= Line_Length loop
         if Line_Buffer (Ptr) /= ' '
           and then Line_Buffer (Ptr) /= ASCII.HT
         then
            return;
         else
            Ptr := Ptr + 1;
         end if;
      end loop;
   end Skip_Spaces;

   --------------------
   -- Symbol_Scanned --
   --------------------

   function Symbol_Scanned return Boolean is
   begin
      End_Sym := Start_Sym - 1;

      case Line_Buffer (End_Sym + 1) is

         when '(' | ')' | '=' =>
            End_Sym := End_Sym + 1;
            return True;

         when '"' =>
            End_Sym := End_Sym + 1;
            while End_Sym < Line_Length loop

               if Line_Buffer (End_Sym + 1) = '"' then

                  if End_Sym + 2 < Line_Length
                    and then Line_Buffer (End_Sym + 2) = '"'
                  then
                     End_Sym := End_Sym + 2;
                  else
                     exit;
                  end if;
               else
                  End_Sym := End_Sym + 1;
               end if;
            end loop;

            if End_Sym >= Line_Length then
               Error ("Invalid string ");
               raise Expression_Error;
            end if;

            End_Sym := End_Sym + 1;
            return False;

         when ''' =>
            End_Sym := End_Sym + 1;

         when others =>
            null;
      end case;

      while End_Sym < Line_Length
        and then (Is_Alphanumeric (Line_Buffer (End_Sym + 1))
                   or else Line_Buffer (End_Sym + 1) = '_')
      loop
         End_Sym := End_Sym + 1;
      end loop;

      return OK_Identifier (Line_Buffer (Start_Sym .. End_Sym));
   end Symbol_Scanned;

   --------------------
   -- Variable_Index --
   --------------------

   function Variable_Index (Name : String) return Natural is
   begin
      for J in 1 .. Num_Syms loop
         if Matching_Strings (Symbols (J).all, Name) then
            return J;
         end if;
      end loop;

      return Natural'Last;
   end Variable_Index;

--  Start of processing for GNATprep

begin

   --  Parse the switches

   loop
      case GNAT.Command_Line.Getopt ("D: b c r s u") is
         when ASCII.NUL =>
            exit;

         when 'D' =>
            declare
               S : String := GNAT.Command_Line.Parameter;
               Index : Natural;

            begin
               Index := Ada.Strings.Fixed.Index (S, "=");

               if Index = 0 then
                  Num_Syms := Num_Syms + 1;
                  Symbols (Num_Syms) := new String'(S);
                  Values (Num_Syms) := new String'("True");

               else
                  Num_Syms := Num_Syms + 1;
                  Symbols (Num_Syms) := new String'(S (S'First .. Index - 1));
                  Values (Num_Syms) := new String'(S (Index + 1 .. S'Last));
               end if;
            end;

         when 'b' =>
            Blank_Deleted_Lines := True;

         when 'c' =>
            Opt_Comment_Deleted_Lines := True;

         when 'r' =>
            Source_Ref_Pragma := True;

         when 's' =>
            List_Symbols := True;

         when 'u' =>
            Undefined_Is_False := True;

         when others =>
            raise Usage_Error;
      end case;
   end loop;

   --  Get the file names

   loop
      declare
         S : constant String := GNAT.Command_Line.Get_Argument;

      begin
         exit when S'Length = 0;

         if Infile.Name = null then
            Infile.Name := new String'(S);
         elsif Outfile_Name = null then
            Outfile_Name := new String'(S);
         elsif Deffile_Name = null then
            Deffile_Name := new String'(S);
         else
            raise Usage_Error;
         end if;
      end;
   end loop;

   --  Test we had all the arguments needed

   if Infile.Name = null
     or else Outfile_Name = null
   then
      raise Usage_Error;
   end if;

   if Source_Ref_Pragma and (not Opt_Comment_Deleted_Lines) then
      Blank_Deleted_Lines := True;
   end if;

   --  Get symbol definitions

   if Deffile_Name /= null then
      Parse_Def_File;
   end if;

   if Num_Errors > 0 then
      raise Fatal_Error;

   elsif List_Symbols and then Num_Syms > 0 then
      List_Symbols_Case : declare

         function Lt (Op1, Op2 : Natural) return Boolean;
         --  Comparison routine for sort call

         procedure Move (From : Natural; To : Natural);
         --  Move routine for sort call

         function Lt (Op1, Op2 : Natural) return Boolean is
            L1   : constant Natural := Symbols (Op1)'Length;
            L2   : constant Natural := Symbols (Op2)'Length;
            MinL : constant Natural := Natural'Min (L1, L2);

            C1, C2 : Character;

         begin
            for J in 0 .. MinL - 1 loop
               C1 := To_Upper (Symbols (Op1).all (Symbols (Op1)'First + J));
               C2 := To_Upper (Symbols (Op2).all (Symbols (Op2)'First + J));

               if C1 < C2 then
                  return True;

               elsif C1 > C2 then
                  return False;
               end if;
            end loop;

            return L1 < L2;
         end Lt;

         procedure Move (From : Natural; To : Natural) is
         begin
            Symbols (To) := Symbols (From);
            Values  (To) := Values  (From);
         end Move;

         package Sort_Syms is new GNAT.Heap_Sort_G (Move, Lt);

         Max_L : Natural;
         --  Maximum length of any symbol

      --  Start of processing for List_Symbols_Case

      begin
         Sort_Syms.Sort (Num_Syms);

         Max_L := 7;
         for J in 1 .. Num_Syms loop
            Max_L := Natural'Max (Max_L, Symbols (J)'Length);
         end loop;

         New_Line;
         Put ("Symbol");

         for J in 1 .. Max_L - 5 loop
            Put (' ');
         end loop;

         Put_Line ("Value");

         Put ("------");

         for J in 1 .. Max_L - 5 loop
            Put (' ');
         end loop;

         Put_Line ("------");

         for J in 1 .. Num_Syms loop
            Put (Symbols (J).all);

            for K in 1 .. Max_L - Symbols (J)'Length + 1 loop
               Put (' ');
            end loop;

            Put_Line (Values (J).all);
         end loop;

         New_Line;
      end List_Symbols_Case;
   end if;

   --  Open files and initialize preprocessing

   begin
      Open (Infile.File,  In_File,  Infile.Name.all);

   exception
      when Name_Error =>
         Put_Line (Standard_Error, "cannot open " & Infile.Name.all);
         raise Fatal_Error;
   end;

   begin
      Create (Outfile, Out_File, Outfile_Name.all);

   exception
      when Name_Error =>
         Put_Line (Standard_Error, "cannot create " & Outfile_Name.all);
         raise Fatal_Error;
   end;

   Infile.Line_Num := 0;
   Current_File_Name := Infile.Name;

   PP_Depth := 0;
   PP (0).Deleting := False;

   --  We return here after we start reading an include file and after
   --  we have finished reading an include file.

   <<Read_In_File>>

   --  If we generate Source_Reference pragmas, then generate one
   --  either with line number 1 for a newly included file, or
   --  with the number of the next line when we have returned to the
   --  including file.

   if Source_Ref_Pragma then
      Put_Line
        (Outfile, "pragma Source_Reference (" &
           Image (Infile.Line_Num + 1) &
           ", """ & Infile.Name.all & """);");
   end if;

   --  Loop through lines in input file

   while not End_Of_File (Infile.File) loop
      Get_Line (Infile.File, Line_Buffer, Line_Length);
      Infile.Line_Num := Infile.Line_Num + 1;

      --  Handle preprocessor line

      if Is_Preprocessor_Line then
         K := Scan_Keyword;

         case K is

            --  Include file

            when K_Include =>
               --  Ignore if Deleting is True

               if PP (PP_Depth).Deleting then
                  goto Output;
               end if;

               Skip_Spaces;

               if Ptr >= Line_Length then
                  Error ("no file to include");

               elsif Line_Buffer (Ptr) /= '"' then
                  Error
                    ("file to include must be specified as a literal string");

               else
                  declare
                     Start_File : constant Positive := Ptr + 1;

                  begin
                     Ptr := Line_Length;

                     while Line_Buffer (Ptr) = ' '
                       or else Line_Buffer (Ptr) = ASCII.HT
                     loop
                        Ptr := Ptr - 1;
                     end loop;

                     if Ptr <= Start_File
                       or else Line_Buffer (Ptr) /= '"'
                     then
                        Error ("no string literal for included file");

                     else
                        if Infile.Next = null then
                           Infile.Next := new Input;
                           Infile.Next.Prev := Infile;
                        end if;

                        Infile := Infile.Next;
                        Infile.Name :=
                          new String'(Line_Buffer (Start_File .. Ptr - 1));

                        --  Check for circularity: an file including itself,
                        --  either directly or indirectly.

                        declare
                           File : Input_Ptr := Infile.Prev;

                        begin
                           while File /= null
                             and then File.Name.all /= Infile.Name.all
                           loop
                              File := File.Prev;
                           end loop;

                           if File /= null then
                              Infile := Infile.Prev;
                              Error ("circularity in included files");

                              while File.Prev /= null loop
                                 File := File.Prev;
                              end loop;

                              while File /= Infile.Next loop
                                 Error ('"' & File.Name.all &
                                          """ includes """ &
                                          File.Next.Name.all & '"');
                                 File := File.Next;
                              end loop;

                           else
                              --  We have a file name and no circularity.
                              --  Open the file and record an error if the
                              --  file cannot be opened.

                              begin
                                 Open (Infile.File, In_File, Infile.Name.all);
                                 Current_File_Name := Infile.Name;
                                 Infile.Line_Num := 0;

                                 --  If we use Source_Reference pragma,
                                 --  we need to output one for this new file.
                                 goto Read_In_File;

                              exception
                                 when Name_Error =>

                                    --  We need to set the input file to
                                    --  the including file, so that the
                                    --  line number is correct when reporting
                                    --  the error.

                                    Infile := Infile.Prev;
                                    Error ("cannot open """ &
                                             Infile.Next.Name.all & '"');
                              end;
                           end if;
                        end;
                     end if;
                  end;
               end if;

            --  If/Elsif processing

            when K_If | K_Elsif =>

               --  If differs from elsif only in that an initial stack entry
               --  must be made for the new if range. We set the match seen
               --  entry to a copy of the deleting status in the range above
               --  us. If we are deleting in the range above us, then we want
               --  all the branches of the nested #if to delete.

               if K = K_If then
                  PP_Depth := PP_Depth + 1;
                  PP (PP_Depth) :=
                    (If_Line    => Infile.Line_Num,
                     If_Name    => Infile.Name,
                     Else_Line  => 0,
                     Deleting   => False,
                     Match_Seen => PP (PP_Depth - 1).Deleting);

               elsif PP_Depth = 0 then
                  Error ("no matching #if for this #elsif");
                  goto Output;

               end if;

               PP (PP_Depth).Deleting := True;

               if not PP (PP_Depth).Match_Seen
                 and then Eval_Condition = True
               then

                  --  Case of match and no match yet in this #if

                  PP (PP_Depth).Deleting := False;
                  PP (PP_Depth).Match_Seen := True;
                  No_Junk;
               end if;

            --  Processing for #else

            when K_Else =>

               if PP_Depth = 0 then
                  Error ("no matching #if for this #else");

               elsif PP (PP_Depth).Else_Line /= 0 then
                  Error ("duplicate #else line (previous was on line" &
                          Natural'Image (PP (PP_Depth).Else_Line)     &
                          ")");

               else
                  PP (PP_Depth).Else_Line := Infile.Line_Num;
                  PP (PP_Depth).Deleting := PP (PP_Depth).Match_Seen;
               end if;

               No_Junk;

            --  Process for #end

            when K_End =>

               if PP_Depth = 0 then
                  Error ("no matching #if for this #end");

               else
                  Skip_Spaces;

                  if Scan_Keyword /= K_If then
                     Error ("expected if after #end");
                     Ptr := Line_Length + 1;
                  end if;

                  Skip_Spaces;

                  if Ptr > Line_Length
                    or else Line_Buffer (Ptr) /= ';'
                  then
                     Error ("missing semicolon after #end if");
                  else
                     Ptr := Ptr + 1;
                  end if;

                  No_Junk;

                  PP_Depth := PP_Depth - 1;
               end if;

            when others =>
               Error ("invalid preprocessor keyword syntax");

         end case;

      --  Handle symbol substitution

      --  Substitution is not allowed in string (which we simply skip),
      --  but is allowed inside character constants. The last case is
      --  because there is no way to know whether the user want to
      --  substitute the name of an attribute ('Min or 'Max for instance)
      --  or actually meant to substitue a character ('$name' is probably
      --  a character constant, but my_type'$name'Min is probably an
      --  attribute, with $name=Base)

      else
         Ptr := 1;

         while Ptr < Line_Length loop
            exit when At_End_Of_Line;

            case Line_Buffer (Ptr) is

               when ''' =>

                  --  Two special cases here:
                  --  '"' => we don't want the " sign to appear as belonging
                  --     to a string.
                  --  '$' => this is obviously not a substitution, just skip it

                  if Ptr < Line_Length - 1
                    and then Line_Buffer (Ptr + 1) = '"'
                  then
                     Ptr := Ptr + 2;
                  elsif Ptr < Line_Length - 2
                    and then Line_Buffer (Ptr + 1 .. Ptr + 2) = "$'"
                  then
                     Ptr := Ptr + 2;
                  end if;

               when '"' =>

                  --  The special case of "" inside the string is easy to
                  --  handle: just ignore them. The second one will be seen
                  --  as the beginning of a second string

                  Ptr := Ptr + 1;
                  while Ptr < Line_Length
                    and then Line_Buffer (Ptr) /= '"'
                  loop
                     Ptr := Ptr + 1;
                  end loop;

               when '$' =>

                  --  $ found, so scan out possible following symbol

                  Start_Sym := Ptr + 1;

                  if Symbol_Scanned then

                     --  Look up symbol in table and if found do replacement

                     for J in 1 .. Num_Syms loop
                        if Matching_Strings
                          (Symbols (J).all, Line_Buffer (Start_Sym .. End_Sym))
                        then
                           declare
                              OldL : constant Positive :=
                                       End_Sym - Start_Sym + 2;
                              NewL : constant Positive := Values (J)'Length;
                              AdjL : constant Integer  := NewL - OldL;
                              NewP : constant Positive := Ptr + NewL - 1;

                           begin
                              Line_Buffer (NewP + 1 .. Line_Length + AdjL) :=
                                Line_Buffer (End_Sym + 1 .. Line_Length);
                              Line_Buffer (Ptr .. NewP) := Values (J).all;

                              Ptr := NewP;
                              Line_Length := Line_Length + AdjL;
                           end;

                           exit;
                        end if;
                     end loop;
                  end if;

               when others =>
                  null;

            end case;
            Ptr := Ptr + 1;
         end loop;
      end if;

      --  Here after dealing with preprocessor line, output current line

      <<Output>>

      if Is_Preprocessor_Line or else PP (PP_Depth).Deleting then
         if Blank_Deleted_Lines then
            New_Line (Outfile);

         elsif Opt_Comment_Deleted_Lines then
            if Line_Length = 0 then
               Put_Line (Outfile, "--!");
            else
               Put (Outfile, "--! ");
               Put_Line (Outfile, Line_Buffer (1 .. Line_Length));
            end if;
         end if;

      else
         Put_Line (Outfile, Line_Buffer (1 .. Line_Length));
      end if;
   end loop;

   --  If we have finished reading an included file, close it and continue
   --  with the next line of the including file.

   if Infile.Prev /= null then
      Close (Infile.File);
      Infile := Infile.Prev;
      Current_File_Name := Infile.Name;
      goto Read_In_File;
   end if;

   for J in 1 .. PP_Depth loop
      if PP (J).If_Name = Infile.Name then
         Error ("no matching #end for #if at line" &
                Natural'Image (PP (J).If_Line));
      else
         Error ("no matching #end for #if at line" &
                Natural'Image (PP (J).If_Line) &
                " of file """ & PP (J).If_Name.all & '"');
      end if;
   end loop;

   if Num_Errors = 0 then
      Close (Outfile);
      Set_Exit_Status (0);
   else
      Delete (Outfile);
      Set_Exit_Status (1);
   end if;

exception
   when Usage_Error =>
      Help_Page;
      Set_Exit_Status (1);

   when GNAT.Command_Line.Invalid_Parameter =>
      Put_Line (Standard_Error, "No parameter given for -"
                & GNAT.Command_Line.Full_Switch);
      Help_Page;
      Set_Exit_Status (1);

   when  GNAT.Command_Line.Invalid_Switch =>
      Put_Line (Standard_Error, "Invalid Switch: -"
                & GNAT.Command_Line.Full_Switch);
      Help_Page;
      Set_Exit_Status (1);

   when Fatal_Error =>
      Set_Exit_Status (1);

   when Expression_Error =>
      Set_Exit_Status (1);

end GNATprep;
