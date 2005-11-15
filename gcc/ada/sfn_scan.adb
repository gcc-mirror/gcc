------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S F N _ S C A N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;

package body SFN_Scan is

   use ASCII;
   --  Allow easy access to control character definitions

   EOF : constant Character := ASCII.SUB;
   --  The character SUB (16#1A#) is used in DOS and other systems derived
   --  from DOS (OS/2, NT etc) to signal the end of a text file. If this
   --  character appears as the last character of a file scanned by a call
   --  to Scan_SFN_Pragmas, then it is ignored, otherwise it is treated as
   --  an illegal character.

   type String_Ptr is access String;

   S : String_Ptr;
   --  Points to the gnat.adc input file

   P : Natural;
   --  Subscript of next character to process in S

   Line_Num : Natural;
   --  Current line number

   Start_Of_Line : Natural;
   --  Subscript of first character at start of current line

   ----------------------
   -- Local Procedures --
   ----------------------

   function Acquire_Integer return Natural;
   --  This function skips white space, and then scans and returns
   --  an unsigned integer. Raises Error if no integer is present
   --  or if the integer is greater than 999.

   function Acquire_String (B : Natural; E : Natural) return String;
   --  This function takes a string scanned out by Scan_String, strips
   --  the enclosing quote characters and any internal doubled quote
   --  characters, and returns the result as a String. The arguments
   --  B and E are as returned from a call to Scan_String. The lower
   --  bound of the string returned is always 1.

   function Acquire_Unit_Name return String;
   --  Skips white space, and then scans and returns a unit name. The
   --  unit name is cased exactly as it appears in the source file.
   --  The terminating character must be white space, or a comma or
   --  a right parenthesis or end of file.

   function At_EOF return Boolean;
   pragma Inline (At_EOF);
   --  Returns True if at end of file, False if not. Note that this
   --  function does NOT skip white space, so P is always unchanged.

   procedure Check_Not_At_EOF;
   pragma Inline (Check_Not_At_EOF);
   --  Skips past white space if any, and then raises Error if at
   --  end of file. Otherwise returns with P skipped past whitespace.

   function Check_File_Type return Character;
   --  Skips white space if any, and then looks for any of the tokens
   --  Spec_File_Name, Body_File_Name, or Subunit_File_Name. If one
   --  of these is found then the value returned is 's', 'b' or 'u'
   --  respectively, and P is bumped past the token. If none of
   --  these tokens is found, then P is unchanged (except for
   --  possible skip of white space), and a space is returned.

   function Check_Token (T : String) return Boolean;
   --  Skips white space if any, and then checks if the string at the
   --  current location matches the given string T, and the character
   --  immediately following is non-alphabetic, non-numeric. If so,
   --  P is stepped past the token, and True is returned. If not,
   --  P is unchanged (except for possibly skipping past whitespace),
   --  and False is returned. S may contain only lower-case letters
   --  ('a' .. 'z').

   procedure Error (Err : String);
   --  Called if an error is detected. Raises Syntax_Error_In_GNAT_ADC
   --  with a message of the form gnat.adc:line:col: xxx, where xxx is
   --  the string Err passed as a parameter.

   procedure Require_Token (T : String);
   --  Skips white space if any, and then requires the given string
   --  to be present. If it is, the P is stepped past it, otherwise
   --  Error is raised, since this is a syntax error. Require_Token
   --  is used only for sequences of special characters, so there
   --  is no issue of terminators, or casing of letters.

   procedure Scan_String (B : out Natural; E : out Natural);
   --  Skips white space if any, then requires that a double quote
   --  or percent be present (start of string). Raises error if
   --  neither of these two characters is found. Otherwise scans
   --  out the string, and returns with P pointing past the
   --  closing quote and S (B .. E) contains the characters of the
   --  string (including the enclosing quotes, with internal quotes
   --  still doubled). Raises Error if the string is malformed.

   procedure Skip_WS;
   --  Skips P past any white space characters (end of line
   --  characters, spaces, comments, horizontal tab characters).

   ---------------------
   -- Acquire_Integer --
   ---------------------

   function Acquire_Integer return Natural is
      N : Natural := 0;

   begin
      Skip_WS;

      if S (P) not in '0' .. '9' then
         Error ("missing index parameter");
      end if;

      while S (P) in '0' .. '9' loop
         N := N * 10 + Character'Pos (S (P)) - Character'Pos ('0');

         if N > 999 then
            Error ("index value greater than 999");
         end if;

         P := P + 1;
      end loop;

      return N;
   end Acquire_Integer;

   --------------------
   -- Acquire_String --
   --------------------

   function Acquire_String (B : Natural; E : Natural) return String is
      Str : String (1 .. E - B - 1);
      Q   : constant Character := S (B);
      J   : Natural;
      Ptr : Natural;

   begin
      Ptr := B + 1;
      J := 0;
      while Ptr < E loop
         J := J + 1;
         Str (J) := S (Ptr);

         if S (Ptr) = Q and then S (Ptr + 1) = Q then
            Ptr := Ptr + 2;
         else
            Ptr := Ptr + 1;
         end if;
      end loop;

      return Str (1 .. J);
   end Acquire_String;

   -----------------------
   -- Acquire_Unit_Name --
   -----------------------

   function Acquire_Unit_Name return String is
      B : Natural;

   begin
      Check_Not_At_EOF;
      B := P;

      while not At_EOF loop
         exit when S (P) not in '0' .. '9'
           and then S (P) /= '.'
           and then S (P) /= '_'
           and then not (S (P) = '[' and then S (P + 1) = '"')
           and then not (S (P) = '"' and then S (P - 1) = '[')
           and then not (S (P) = '"' and then S (P + 1) = ']')
           and then not (S (P) = ']' and then S (P - 1) = '"')
           and then S (P) < 'A';
         P := P + 1;
      end loop;

      if P = B then
         Error ("null unit name");
      end if;

      return S (B .. P - 1);
   end Acquire_Unit_Name;

   ------------
   -- At_EOF --
   ------------

   function At_EOF return Boolean is
   begin
      --  Immediate return (False) if before last character of file

      if P < S'Last then
         return False;

      --  Special case: DOS EOF character as last character of file is
      --  allowed and treated as an end of file.

      elsif P = S'Last then
         return S (P) = EOF;

      --  If beyond last character of file, then definitely at EOF

      else
         return True;
      end if;
   end At_EOF;

   ---------------------
   -- Check_File_Type --
   ---------------------

   function Check_File_Type return Character is
   begin
      if Check_Token ("spec_file_name") then
         return 's';
      elsif Check_Token ("body_file_name") then
         return 'b';
      elsif Check_Token ("subunit_file_name") then
         return 'u';
      else
         return ' ';
      end if;
   end Check_File_Type;

   ----------------------
   -- Check_Not_At_EOF --
   ----------------------

   procedure Check_Not_At_EOF is
   begin
      Skip_WS;

      if At_EOF then
         Error ("unexpected end of file");
      end if;

      return;
   end Check_Not_At_EOF;

   -----------------
   -- Check_Token --
   -----------------

   function Check_Token (T : String) return Boolean is
      Save_P : Natural;
      C : Character;

   begin
      Skip_WS;
      Save_P := P;

      for K in T'Range loop
         if At_EOF then
            P := Save_P;
            return False;
         end if;

         C := S (P);

         if C in 'A' .. 'Z' then
            C := Character'Val (Character'Pos (C) +
                                 (Character'Pos ('a') - Character'Pos ('A')));
         end if;

         if C /= T (K) then
            P := Save_P;
            return False;
         end if;

         P := P + 1;
      end loop;

      if At_EOF then
         return True;
      end if;

      C := S (P);

      if C in '0' .. '9'
        or else C in 'a' .. 'z'
        or else C in 'A' .. 'Z'
        or else C > Character'Val (127)
      then
         P := Save_P;
         return False;

      else
         return True;
      end if;
   end Check_Token;

   -----------
   -- Error --
   -----------

   procedure Error (Err : String) is
      C : Natural := 0;
      --  Column number

      M : String (1 .. 80);
      --  Buffer used to build resulting error msg

      LM : Natural := 0;
      --  Pointer to last set location in M

      procedure Add_Nat (N : Natural);
      --  Add chars of integer to error msg buffer

      -------------
      -- Add_Nat --
      -------------

      procedure Add_Nat (N : Natural) is
      begin
         if N > 9 then
            Add_Nat (N / 10);
         end if;

         LM := LM + 1;
         M (LM) := Character'Val (N mod 10 + Character'Pos ('0'));
      end Add_Nat;

   --  Start of processing for Error

   begin
      M (1 .. 9) := "gnat.adc:";
      LM := 9;
      Add_Nat (Line_Num);
      LM := LM + 1;
      M (LM) := ':';

      --  Determine column number

      for X in Start_Of_Line .. P loop
         C := C + 1;

         if S (X) = HT then
            C := (C + 7) / 8 * 8;
         end if;
      end loop;

      Add_Nat (C);
      M (LM + 1) := ':';
      LM := LM + 1;
      M (LM + 1) := ' ';
      LM := LM + 1;

      M (LM + 1 .. LM + Err'Length) := Err;
      LM := LM + Err'Length;

      Raise_Exception (Syntax_Error_In_GNAT_ADC'Identity, M (1 .. LM));
   end Error;

   -------------------
   -- Require_Token --
   -------------------

   procedure Require_Token (T : String) is
      SaveP : Natural;

   begin
      Skip_WS;
      SaveP := P;

      for J in T'Range loop

         if At_EOF or else S (P) /= T (J) then
            declare
               S : String (1 .. T'Length + 10);

            begin
               S (1 .. 9) := "missing """;
               S (10 .. T'Length + 9) := T;
               S (T'Length + 10) := '"';
               P := SaveP;
               Error (S);
            end;

         else
            P := P + 1;
         end if;
      end loop;
   end Require_Token;

   ----------------------
   -- Scan_SFN_Pragmas --
   ----------------------

   procedure Scan_SFN_Pragmas
     (Source   : String;
      SFN_Ptr  : Set_File_Name_Ptr;
      SFNP_Ptr : Set_File_Name_Pattern_Ptr)
   is
      B, E : Natural;
      Typ  : Character;
      Cas  : Character;

   begin
      Line_Num := 1;
      S := Source'Unrestricted_Access;
      P := Source'First;
      Start_Of_Line := P;

      --  Loop through pragmas in file

      Main_Scan_Loop : loop
         Skip_WS;
         exit Main_Scan_Loop when At_EOF;

         --  Error if something other than pragma

         if not Check_Token ("pragma") then
            Error ("non pragma encountered");
         end if;

         --  Source_File_Name pragma case

         if Check_Token ("source_file_name")
              or else
             Check_Token ("source_file_name_project")
         then
            Require_Token ("(");

            Typ := Check_File_Type;

            --  First format, with unit name first

            if Typ = ' ' then
               if Check_Token ("unit_name") then
                  Require_Token ("=>");
               end if;

               declare
                  U : constant String := Acquire_Unit_Name;

               begin
                  Require_Token (",");
                  Typ := Check_File_Type;

                  if Typ /= 's' and then Typ /= 'b' then
                     Error ("bad pragma");
                  end if;

                  Require_Token ("=>");
                  Scan_String (B, E);

                  declare
                     F : constant String := Acquire_String (B, E);
                     X : Natural;

                  begin
                     --  Scan Index parameter if present

                     if Check_Token (",") then
                        if Check_Token ("index") then
                           Require_Token ("=>");
                        end if;

                        X := Acquire_Integer;
                     else
                        X := 0;
                     end if;

                     Require_Token (")");
                     Require_Token (";");
                     SFN_Ptr.all (Typ, U, F, X);
                  end;
               end;

            --  Second format with pattern string

            else
               Require_Token ("=>");
               Scan_String (B, E);

               declare
                  Pat : constant String := Acquire_String (B, E);
                  Nas : Natural := 0;

               begin
                  --  Check exactly one asterisk

                  for J in Pat'Range loop
                     if Pat (J) = '*' then
                        Nas := Nas + 1;
                     end if;
                  end loop;

                  if Nas /= 1 then
                     Error ("** not allowed");
                  end if;

                  B := 0;
                  E := 0;
                  Cas := ' ';

                  --  Loop to scan out Casing or Dot_Replacement parameters

                  loop
                     Check_Not_At_EOF;
                     exit when S (P) = ')';
                     Require_Token (",");

                     if Check_Token ("casing") then
                        Require_Token ("=>");

                        if Cas /= ' ' then
                           Error ("duplicate casing argument");
                        elsif Check_Token ("lowercase") then
                           Cas := 'l';
                        elsif Check_Token ("uppercase") then
                           Cas := 'u';
                        elsif Check_Token ("mixedcase") then
                           Cas := 'm';
                        else
                           Error ("invalid casing argument");
                        end if;

                     elsif Check_Token ("dot_replacement") then
                        Require_Token ("=>");

                        if E /= 0 then
                           Error ("duplicate dot_replacement");
                        else
                           Scan_String (B, E);
                        end if;

                     else
                        Error ("invalid argument");
                     end if;
                  end loop;

                  Require_Token (")");
                  Require_Token (";");

                  if Cas = ' ' then
                     Cas := 'l';
                  end if;

                  if E = 0 then
                     SFNP_Ptr.all (Pat, Typ, ".", Cas);

                  else
                     declare
                        Dot : constant String := Acquire_String (B, E);

                     begin
                        SFNP_Ptr.all (Pat, Typ, Dot, Cas);
                     end;
                  end if;
               end;
            end if;

         --  Some other pragma, scan to semicolon at end of pragma

         else
            Skip_Loop : loop
               exit Main_Scan_Loop when At_EOF;
               exit Skip_Loop when S (P) = ';';

               if S (P) = '"' or else S (P) = '%' then
                  Scan_String (B, E);
               else
                  P := P + 1;
               end if;
            end loop Skip_Loop;

            --  We successfuly skipped to semicolon, so skip past it

            P := P + 1;
         end if;
      end loop Main_Scan_Loop;

   exception
      when others =>
         Cursor := P - S'First + 1;
         raise;
   end Scan_SFN_Pragmas;

   -----------------
   -- Scan_String --
   -----------------

   procedure Scan_String (B : out Natural; E : out Natural) is
      Q : Character;

   begin
      Check_Not_At_EOF;

      if S (P) = '"' then
         Q := '"';
      elsif S (P) = '%' then
         Q := '%';
      else
         Error ("bad string");
         Q := '"';
      end if;

      --  Scan out the string, B points to first char

      B := P;
      P := P + 1;

      loop
         if At_EOF or else S (P) = LF or else S (P) = CR then
            Error ("missing string quote");

         elsif S (P) = HT then
            Error ("tab character in string");

         elsif S (P) /= Q then
            P := P + 1;

         --  We have a quote

         else
            P := P + 1;

            --  Check for doubled quote

            if not At_EOF and then S (P) = Q then
               P := P + 1;

            --  Otherwise this is the terminating quote

            else
               E := P - 1;
               return;
            end if;
         end if;
      end loop;
   end Scan_String;

   -------------
   -- Skip_WS --
   -------------

   procedure Skip_WS is
   begin
      WS_Scan : while not At_EOF loop
         case S (P) is

            --  End of physical line

            when CR | LF =>
               Line_Num := Line_Num + 1;
               P := P + 1;

               while not At_EOF
                 and then (S (P) = CR or else S (P) = LF)
               loop
                  Line_Num := Line_Num + 1;
                  P := P + 1;
               end loop;

               Start_Of_Line := P;

            --  All other cases of white space characters

            when ' ' | FF | VT | HT =>
               P := P + 1;

            --  Comment

            when '-' =>
               P := P + 1;

               if At_EOF then
                  Error ("bad comment");

               elsif S (P) = '-' then
                  P := P + 1;

                  while not At_EOF loop
                     case S (P) is
                        when CR | LF | FF | VT =>
                           exit;
                        when others =>
                           P := P + 1;
                     end case;
                  end loop;

               else
                  P := P - 1;
                  exit WS_Scan;
               end if;

            when others =>
               exit WS_Scan;

         end case;
      end loop WS_Scan;
   end Skip_WS;

end SFN_Scan;
