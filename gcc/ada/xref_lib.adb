------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R E F _ L I B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2009, Free Software Foundation, Inc.         --
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

with Osint;
with Output; use Output;
with Types;  use Types;

with Unchecked_Deallocation;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.IO_Aux;       use GNAT.IO_Aux;

package body Xref_Lib is

   Type_Position : constant := 50;
   --  Column for label identifying type of entity

   ---------------------
   -- Local Variables --
   ---------------------

   Pipe : constant Character := '|';
   --  First character on xref lines in the .ali file

   No_Xref_Information : exception;
   --  Exception raised when there is no cross-referencing information in
   --  the .ali files.

   procedure Parse_EOL
     (Source                 : not null access String;
      Ptr                    : in out Positive;
      Skip_Continuation_Line : Boolean := False);
   --  On return Source (Ptr) is the first character of the next line
   --  or EOF. Source.all must be terminated by EOF.
   --
   --  If Skip_Continuation_Line is True, this subprogram skips as many
   --  lines as required when the second or more lines starts with '.'
   --  (continuation lines in ALI files).

   function Current_Xref_File (File : ALI_File) return File_Reference;
   --  Return the file matching the last 'X' line we found while parsing
   --  the ALI file.

   function File_Name (File : ALI_File; Num : Positive) return File_Reference;
   --  Returns the dependency file name number Num

   function Get_Full_Type (Decl : Declaration_Reference) return String;
   --  Returns the full type corresponding to a type letter as found in
   --  the .ali files.

   procedure Open
     (Name         : String;
      File         : out ALI_File;
      Dependencies : Boolean := False);
   --  Open a new ALI file. If Dependencies is True, the insert every library
   --  file 'with'ed in the files database (used for gnatxref)

   procedure Parse_Identifier_Info
     (Pattern       : Search_Pattern;
      File          : in out ALI_File;
      Local_Symbols : Boolean;
      Der_Info      : Boolean := False;
      Type_Tree     : Boolean := False;
      Wide_Search   : Boolean := True;
      Labels_As_Ref : Boolean := True);
   --  Output the file and the line where the identifier was referenced,
   --  If Local_Symbols is False then only the publicly visible symbols
   --  will be processed.
   --
   --  If Labels_As_Ref is true, then the references to the entities after
   --  the end statements ("end Foo") will be counted as actual references.
   --  The entity will never be reported as unreferenced by gnatxref -u

   procedure Parse_Token
     (Source    : not null access String;
      Ptr       : in out Positive;
      Token_Ptr : out Positive);
   --  Skips any separators and stores the start of the token in Token_Ptr.
   --  Then stores the position of the next separator in Ptr. On return
   --  Source (Token_Ptr .. Ptr - 1) is the token. Separators are space
   --  and ASCII.HT. Parse_Token will never skip to the next line.

   procedure Parse_Number
     (Source : not null access String;
      Ptr    : in out Positive;
      Number : out Natural);
   --  Skips any separators and parses Source up to the first character that
   --  is not a decimal digit. Returns value of parsed digits or 0 if none.

   procedure Parse_X_Filename (File : in out ALI_File);
   --  Reads and processes "X..." lines in the ALI file
   --  and updates the File.X_File information.

   procedure Skip_To_First_X_Line
     (File    : in out ALI_File;
      D_Lines : Boolean;
      W_Lines : Boolean);
   --  Skip the lines in the ALI file until the first cross-reference line
   --  (^X...) is found. Search is started from the beginning of the file.
   --  If not such line is found, No_Xref_Information is raised.
   --  If W_Lines is false, then the lines "^W" are not parsed.
   --  If D_Lines is false, then the lines "^D" are not parsed.

   ----------------
   -- Add_Entity --
   ----------------

   procedure Add_Entity
     (Pattern : in out Search_Pattern;
      Entity  : String;
      Glob    : Boolean := False)
   is
      File_Start : Natural;
      Line_Start : Natural;
      Col_Start  : Natural;
      Line_Num   : Natural := 0;
      Col_Num    : Natural := 0;

      File_Ref : File_Reference := Empty_File;
      pragma Warnings (Off, File_Ref);

   begin
      --  Find the end of the first item in Entity (pattern or file?)
      --  If there is no ':', we only have a pattern

      File_Start := Index (Entity, ":");

      --  If the regular expression is invalid, just consider it as a string

      if File_Start = 0 then
         begin
            Pattern.Entity := Compile (Entity, Glob, False);
            Pattern.Initialized := True;

         exception
            when Error_In_Regexp =>

               --  The basic idea is to insert a \ before every character

               declare
                  Tmp_Regexp : String (1 .. 2 * Entity'Length);
                  Index      : Positive := 1;

               begin
                  for J in Entity'Range loop
                     Tmp_Regexp (Index) := '\';
                     Tmp_Regexp (Index + 1) := Entity (J);
                     Index := Index + 2;
                  end loop;

                  Pattern.Entity := Compile (Tmp_Regexp, True, False);
                  Pattern.Initialized := True;
               end;
         end;

         Set_Default_Match (True);
         return;
      end if;

      --  If there is a dot in the pattern, then it is a file name

      if (Glob and then
           Index (Entity (Entity'First .. File_Start - 1), ".") /= 0)
             or else
              (not Glob
                 and then Index (Entity (Entity'First .. File_Start - 1),
                                   "\.") /= 0)
      then
         Pattern.Entity      := Compile (".*", False);
         Pattern.Initialized := True;
         File_Start          := Entity'First;

      else
         --  If the regular expression is invalid, just consider it as a string

         begin
            Pattern.Entity :=
              Compile (Entity (Entity'First .. File_Start - 1), Glob, False);
            Pattern.Initialized := True;

         exception
            when Error_In_Regexp =>

               --  The basic idea is to insert a \ before every character

               declare
                  Tmp_Regexp : String (1 .. 2 * (File_Start - Entity'First));
                  Index      : Positive := 1;

               begin
                  for J in Entity'First .. File_Start - 1 loop
                     Tmp_Regexp (Index) := '\';
                     Tmp_Regexp (Index + 1) := Entity (J);
                     Index := Index + 2;
                  end loop;

                  Pattern.Entity := Compile (Tmp_Regexp, True, False);
                  Pattern.Initialized := True;
               end;
         end;

         File_Start := File_Start + 1;
      end if;

      --  Parse the file name

      Line_Start := Index (Entity (File_Start .. Entity'Last), ":");

      --  Check if it was a disk:\directory item (for NT and OS/2)

      if File_Start = Line_Start - 1
        and then Line_Start < Entity'Last
        and then Entity (Line_Start + 1) = '\'
      then
         Line_Start := Index (Entity (Line_Start + 1 .. Entity'Last), ":");
      end if;

      if Line_Start = 0 then
         Line_Start := Entity'Length + 1;

      elsif Line_Start /= Entity'Last then
         Col_Start := Index (Entity (Line_Start + 1 .. Entity'Last), ":");

         if Col_Start = 0 then
            Col_Start := Entity'Last + 1;
         end if;

         if Col_Start > Line_Start + 1 then
            begin
               Line_Num := Natural'Value
                 (Entity (Line_Start + 1 .. Col_Start - 1));

            exception
               when Constraint_Error =>
                  raise Invalid_Argument;
            end;
         end if;

         if Col_Start < Entity'Last then
            begin
               Col_Num := Natural'Value (Entity
                                         (Col_Start + 1 .. Entity'Last));

            exception
               when Constraint_Error => raise Invalid_Argument;
            end;
         end if;
      end if;

      File_Ref :=
        Add_To_Xref_File
          (Entity (File_Start .. Line_Start - 1), Visited => True);
      Pattern.File_Ref := File_Ref;

      Add_Line (Pattern.File_Ref, Line_Num, Col_Num);

      File_Ref :=
        Add_To_Xref_File
          (ALI_File_Name (Entity (File_Start .. Line_Start - 1)),
           Visited      => False,
           Emit_Warning => True);
   end Add_Entity;

   -------------------
   -- Add_Xref_File --
   -------------------

   procedure Add_Xref_File (File : String) is
      File_Ref : File_Reference := Empty_File;
      pragma Unreferenced (File_Ref);

      Iterator : Expansion_Iterator;

      procedure Add_Xref_File_Internal (File : String);
      --  Do the actual addition of the file

      ----------------------------
      -- Add_Xref_File_Internal --
      ----------------------------

      procedure Add_Xref_File_Internal (File : String) is
      begin
         --  Case where we have an ALI file, accept it even though this is
         --  not official usage, since the intention is obvious

         if Tail (File, 4) = ".ali" then
            File_Ref := Add_To_Xref_File
                          (File, Visited => False, Emit_Warning => True);

         --  Normal non-ali file case

         else
            File_Ref := Add_To_Xref_File (File, Visited => True);

            File_Ref := Add_To_Xref_File
                         (ALI_File_Name (File),
                          Visited => False, Emit_Warning => True);
         end if;
      end Add_Xref_File_Internal;

   --  Start of processing for Add_Xref_File

   begin
      --  Check if we need to do the expansion

      if Ada.Strings.Fixed.Index (File, "*") /= 0
        or else Ada.Strings.Fixed.Index (File, "?") /= 0
      then
         Start_Expansion (Iterator, File);

         loop
            declare
               S : constant String := Expansion (Iterator);

            begin
               exit when S'Length = 0;
               Add_Xref_File_Internal (S);
            end;
         end loop;

      else
         Add_Xref_File_Internal (File);
      end if;
   end Add_Xref_File;

   -----------------------
   -- Current_Xref_File --
   -----------------------

   function Current_Xref_File (File : ALI_File) return File_Reference is
   begin
      return File.X_File;
   end Current_Xref_File;

   --------------------------
   -- Default_Project_File --
   --------------------------

   function Default_Project_File (Dir_Name : String) return String is
      My_Dir  : Dir_Type;
      Dir_Ent : File_Name_String;
      Last    : Natural;

   begin
      Open (My_Dir, Dir_Name);

      loop
         Read (My_Dir, Dir_Ent, Last);
         exit when Last = 0;

         if Tail (Dir_Ent (1 .. Last), 4) = ".adp" then

            --  The first project file found is the good one

            Close (My_Dir);
            return Dir_Ent (1 .. Last);
         end if;
      end loop;

      Close (My_Dir);
      return String'(1 .. 0 => ' ');

   exception
      when Directory_Error => return String'(1 .. 0 => ' ');
   end Default_Project_File;

   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (File : ALI_File;
      Num  : Positive) return File_Reference
   is
   begin
      return File.Dep.Table (Num);
   end File_Name;

   --------------------
   -- Find_ALI_Files --
   --------------------

   procedure Find_ALI_Files is
      My_Dir  : Rec_DIR;
      Dir_Ent : File_Name_String;
      Last    : Natural;

      File_Ref : File_Reference;
      pragma Unreferenced (File_Ref);

      function Open_Next_Dir return Boolean;
      --  Tries to open the next object directory, and return False if
      --  the directory cannot be opened.

      -------------------
      -- Open_Next_Dir --
      -------------------

      function Open_Next_Dir return Boolean is
      begin
         --  Until we are able to open a new directory

         loop
            declare
               Obj_Dir : constant String := Next_Obj_Dir;

            begin
               --  Case of no more Obj_Dir lines

               if Obj_Dir'Length = 0 then
                  return False;
               end if;

               Open (My_Dir.Dir, Obj_Dir);
               exit;

            exception

               --  Could not open the directory

               when Directory_Error => null;
            end;
         end loop;

         return True;
      end Open_Next_Dir;

   --  Start of processing for Find_ALI_Files

   begin
      Reset_Obj_Dir;

      if Open_Next_Dir then
         loop
            Read (My_Dir.Dir, Dir_Ent, Last);

            if Last = 0 then
               Close (My_Dir.Dir);

               if not Open_Next_Dir then
                  return;
               end if;

            elsif Last > 4 and then Dir_Ent (Last - 3 .. Last) = ".ali" then
               File_Ref :=
                 Add_To_Xref_File (Dir_Ent (1 .. Last), Visited => False);
            end if;
         end loop;
      end if;
   end Find_ALI_Files;

   -------------------
   -- Get_Full_Type --
   -------------------

   function Get_Full_Type (Decl : Declaration_Reference) return String is

      function Param_String return String;
      --  Return the string to display depending on whether Decl is a parameter

      ------------------
      -- Param_String --
      ------------------

      function Param_String return String is
      begin
         if Is_Parameter (Decl) then
            return "parameter ";
         else
            return "";
         end if;
      end Param_String;

   --  Start of processing for Get_Full_Type

   begin
      case Get_Type (Decl) is
         when 'A' => return "array type";
         when 'B' => return "boolean type";
         when 'C' => return "class-wide type";
         when 'D' => return "decimal type";
         when 'E' => return "enumeration type";
         when 'F' => return "float type";
         when 'I' => return "integer type";
         when 'M' => return "modular type";
         when 'O' => return "fixed type";
         when 'P' => return "access type";
         when 'R' => return "record type";
         when 'S' => return "string type";
         when 'T' => return "task type";
         when 'W' => return "protected type";

         when 'a' => return "array type";
         when 'b' => return Param_String & "boolean object";
         when 'c' => return Param_String & "class-wide object";
         when 'd' => return Param_String & "decimal object";
         when 'e' => return Param_String & "enumeration object";
         when 'f' => return Param_String & "float object";
         when 'h' => return "interface";
         when 'i' => return Param_String & "integer object";
         when 'm' => return Param_String & "modular object";
         when 'o' => return Param_String & "fixed object";
         when 'p' => return Param_String & "access object";
         when 'r' => return Param_String & "record object";
         when 's' => return Param_String & "string object";
         when 't' => return Param_String & "task object";
         when 'w' => return Param_String & "protected object";
         when 'x' => return Param_String & "abstract procedure";
         when 'y' => return Param_String & "abstract function";

         when 'K' => return "package";
         when 'k' => return "generic package";
         when 'L' => return "statement label";
         when 'l' => return "loop label";
         when 'N' => return "named number";
         when 'n' => return "enumeration literal";
         when 'q' => return "block label";
         when 'U' => return "procedure";
         when 'u' => return "generic procedure";
         when 'V' => return "function";
         when 'v' => return "generic function";
         when 'X' => return "exception";
         when 'Y' => return "entry";

         when '+' => return "private type";

         --  The above should be the only possibilities, but for this kind
         --  of informational output, we don't want to bomb if we find
         --  something else, so just return three question marks when we
         --  have an unknown Abbrev value

         when others =>
            return "??? (" & Get_Type (Decl) & ")";
      end case;
   end Get_Full_Type;

   --------------------------
   -- Skip_To_First_X_Line --
   --------------------------

   procedure Skip_To_First_X_Line
     (File    : in out ALI_File;
      D_Lines : Boolean;
      W_Lines : Boolean)
   is
      Ali              : String_Access renames File.Buffer;
      Token            : Positive;
      Ptr              : Positive := Ali'First;
      Num_Dependencies : Natural  := 0;
      File_Start       : Positive;
      File_End         : Positive;
      Gnatchop_Offset  : Integer;
      Gnatchop_Name    : Positive;

      File_Ref : File_Reference;
      pragma Unreferenced (File_Ref);

   begin
      --  Read all the lines possibly processing with-clauses and dependency
      --  information and exit on finding the first Xref line.
      --  A fall-through of the loop means that there is no xref information
      --  which is an error condition.

      while Ali (Ptr) /= EOF loop
         if D_Lines and then Ali (Ptr) = 'D' then

            --  Found dependency information. Format looks like:
            --  D src-nam time-stmp checksum [subunit-name] [line:file-name]

            --  Skip the D and parse the filenam

            Ptr := Ptr + 1;
            Parse_Token (Ali, Ptr, Token);
            File_Start := Token;
            File_End := Ptr - 1;

            Num_Dependencies := Num_Dependencies + 1;
            Set_Last (File.Dep, Num_Dependencies);

            Parse_Token (Ali, Ptr, Token); --  Skip time-stamp
            Parse_Token (Ali, Ptr, Token); --  Skip checksum
            Parse_Token (Ali, Ptr, Token); --  Read next entity on the line

            if not (Ali (Token) in '0' .. '9') then
               Parse_Token (Ali, Ptr, Token); --  Was a subunit name
            end if;

            --  Did we have a gnatchop-ed file with a pragma Source_Reference ?

            Gnatchop_Offset := 0;

            if Ali (Token) in '0' .. '9' then
               Gnatchop_Name := Token;
               while Ali (Gnatchop_Name) /= ':' loop
                  Gnatchop_Name := Gnatchop_Name + 1;
               end loop;

               Gnatchop_Offset :=
                 2 - Natural'Value (Ali (Token .. Gnatchop_Name - 1));
               Token := Gnatchop_Name + 1;
            end if;

            File.Dep.Table (Num_Dependencies) := Add_To_Xref_File
              (Ali (File_Start .. File_End),
               Gnatchop_File => Ali (Token .. Ptr - 1),
               Gnatchop_Offset => Gnatchop_Offset);

         elsif W_Lines and then Ali (Ptr) = 'W' then

            --  Found with-clause information. Format looks like:
            --     "W debug%s               debug.adb               debug.ali"

            --  Skip the W and parse the .ali filename (3rd token)

            Parse_Token (Ali, Ptr, Token);
            Parse_Token (Ali, Ptr, Token);
            Parse_Token (Ali, Ptr, Token);

            File_Ref :=
              Add_To_Xref_File (Ali (Token .. Ptr - 1), Visited => False);

         elsif Ali (Ptr) = 'X' then

            --  Found a cross-referencing line - stop processing

            File.Current_Line := Ptr;
            File.Xref_Line    := Ptr;
            return;
         end if;

         Parse_EOL (Ali, Ptr);
      end loop;

      raise No_Xref_Information;
   end Skip_To_First_X_Line;

   ----------
   -- Open --
   ----------

   procedure Open
     (Name         : String;
      File         : out ALI_File;
      Dependencies : Boolean := False)
   is
      Ali : String_Access renames File.Buffer;
      pragma Warnings (Off, Ali);

   begin
      if File.Buffer /= null then
         Free (File.Buffer);
      end if;

      Init (File.Dep);

      begin
         Read_File (Name, Ali);

      exception
         when Ada.Text_IO.Name_Error | Ada.Text_IO.End_Error =>
            raise No_Xref_Information;
      end;

      Skip_To_First_X_Line (File, D_Lines => True, W_Lines => Dependencies);
   end Open;

   ---------------
   -- Parse_EOL --
   ---------------

   procedure Parse_EOL
     (Source                 : not null access String;
      Ptr                    : in out Positive;
      Skip_Continuation_Line : Boolean := False)
   is
   begin
      loop
         --  Skip to end of line

         while Source (Ptr) /= ASCII.CR and then Source (Ptr) /= ASCII.LF
           and then Source (Ptr) /= EOF
         loop
            Ptr := Ptr + 1;
         end loop;

         --  Skip CR or LF if not at end of file

         if Source (Ptr) /= EOF then
            Ptr := Ptr + 1;
         end if;

         --  Skip past CR/LF or LF/CR combination

         if (Source (Ptr) = ASCII.CR or else Source (Ptr) = ASCII.LF)
           and then Source (Ptr) /= Source (Ptr - 1)
         then
            Ptr := Ptr + 1;
         end if;

         exit when not Skip_Continuation_Line or else Source (Ptr) /= '.';
      end loop;
   end Parse_EOL;

   ---------------------------
   -- Parse_Identifier_Info --
   ---------------------------

   procedure Parse_Identifier_Info
     (Pattern       : Search_Pattern;
      File          : in out ALI_File;
      Local_Symbols : Boolean;
      Der_Info      : Boolean := False;
      Type_Tree     : Boolean := False;
      Wide_Search   : Boolean := True;
      Labels_As_Ref : Boolean := True)
   is
      Ptr      : Positive renames File.Current_Line;
      Ali      : String_Access renames File.Buffer;

      E_Line   : Natural;   --  Line number of current entity
      E_Col    : Natural;   --  Column number of current entity
      E_Type   : Character; --  Type of current entity
      E_Name   : Positive;  --  Pointer to begin of entity name
      E_Global : Boolean;   --  True iff entity is global

      R_Line   : Natural;   --  Line number of current reference
      R_Col    : Natural;   --  Column number of current reference
      R_Type   : Character; --  Type of current reference

      Decl_Ref : Declaration_Reference;
      File_Ref : File_Reference := Current_Xref_File (File);

      function Get_Symbol_Name (Eun, Line, Col : Natural) return String;
      --  Returns the symbol name for the entity defined at the specified
      --  line and column in the dependent unit number Eun. For this we need
      --  to parse the ali file again because the parent entity is not in
      --  the declaration table if it did not match the search pattern.

      procedure Skip_To_Matching_Closing_Bracket;
      --  When Ptr points to an opening square bracket, moves it to the
      --  character following the matching closing bracket

      ---------------------
      -- Get_Symbol_Name --
      ---------------------

      function Get_Symbol_Name (Eun, Line, Col : Natural) return String is
         Ptr    : Positive := 1;
         E_Eun  : Positive;   --  Unit number of current entity
         E_Line : Natural;    --  Line number of current entity
         E_Col  : Natural;    --  Column number of current entity
         E_Name : Positive;   --  Pointer to begin of entity name

      begin
         --  Look for the X lines corresponding to unit Eun

         loop
            if Ali (Ptr) = 'X' then
               Ptr := Ptr + 1;
               Parse_Number (Ali, Ptr, E_Eun);
               exit when E_Eun = Eun;
            end if;

            Parse_EOL (Ali, Ptr, Skip_Continuation_Line => True);
         end loop;

         --  Here we are in the right Ali section, we now look for the entity
         --  declared at position (Line, Col).

         loop
            Parse_Number (Ali, Ptr, E_Line);
            exit when Ali (Ptr) = EOF;
            Ptr := Ptr + 1;
            Parse_Number (Ali, Ptr, E_Col);
            exit when Ali (Ptr) = EOF;
            Ptr := Ptr + 1;

            if Line = E_Line and then Col = E_Col then
               Parse_Token (Ali, Ptr, E_Name);
               return Ali (E_Name .. Ptr - 1);
            end if;

            Parse_EOL (Ali, Ptr, Skip_Continuation_Line => True);
            exit when Ali (Ptr) = EOF;
         end loop;

         --  We were not able to find the symbol, this should not happen but
         --  since we don't want to stop here we return a string of three
         --  question marks as the symbol name.

         return "???";
      end Get_Symbol_Name;

      --------------------------------------
      -- Skip_To_Matching_Closing_Bracket --
      --------------------------------------

      procedure Skip_To_Matching_Closing_Bracket is
         Num_Brackets : Natural;

      begin
         Num_Brackets := 1;
         while Num_Brackets /= 0 loop
            Ptr := Ptr + 1;
            if Ali (Ptr) = '[' then
               Num_Brackets := Num_Brackets + 1;
            elsif Ali (Ptr) = ']' then
               Num_Brackets := Num_Brackets - 1;
            end if;
         end loop;

         Ptr := Ptr + 1;
      end Skip_To_Matching_Closing_Bracket;

   --  Start of processing for Parse_Identifier_Info

   begin
      --  The identifier info looks like:
      --     "38U9*Debug 12|36r6 36r19"

      --  Extract the line, column and entity name information

      Parse_Number (Ali, Ptr, E_Line);

      if Ali (Ptr) > ' ' then
         E_Type := Ali (Ptr);
         Ptr := Ptr + 1;
      end if;

      --  Ignore some of the entities (labels,...)

      case E_Type is
         when 'l' | 'L' | 'q' =>
            Parse_EOL (Ali, Ptr, Skip_Continuation_Line => True);
            return;

         when others =>
            null;
      end case;

      Parse_Number (Ali, Ptr, E_Col);

      E_Global := False;
      if Ali (Ptr) >= ' ' then
         E_Global := (Ali (Ptr) = '*');
         Ptr := Ptr + 1;
      end if;

      Parse_Token (Ali, Ptr, E_Name);

      --  Exit if the symbol does not match
      --  or if we have a local symbol and we do not want it

      if (not Local_Symbols and not E_Global)
        or else (Pattern.Initialized
                  and then not Match (Ali (E_Name .. Ptr - 1), Pattern.Entity))
        or else (E_Name >= Ptr)
      then
         Decl_Ref := Add_Declaration
           (File.X_File, Ali (E_Name .. Ptr - 1), E_Line, E_Col, E_Type,
            Remove_Only => True);
         Parse_EOL (Ali, Ptr, Skip_Continuation_Line => True);
         return;
      end if;

      --  Insert the declaration in the table

      Decl_Ref := Add_Declaration
        (File.X_File, Ali (E_Name .. Ptr - 1), E_Line, E_Col, E_Type);

      if Ali (Ptr) = '[' then
         Skip_To_Matching_Closing_Bracket;
      end if;

      --  Skip any renaming indication

      if Ali (Ptr) = '=' then
         declare
            P_Line, P_Column : Natural;
            pragma Warnings (Off, P_Line);
            pragma Warnings (Off, P_Column);
         begin
            Ptr := Ptr + 1;
            Parse_Number (Ali, Ptr, P_Line);
            Ptr := Ptr + 1;
            Parse_Number (Ali, Ptr, P_Column);
         end;
      end if;

      if Ali (Ptr) = '<'
        or else Ali (Ptr) = '('
        or else Ali (Ptr) = '{'
      then
         --  Here we have a type derivation information. The format is
         --  <3|12I45> which means that the current entity is derived from the
         --  type defined in unit number 3, line 12 column 45. The pipe and
         --  unit number is optional. It is specified only if the parent type
         --  is not defined in the current unit.

         --  We also have the format for generic instantiations, as in
         --  7a5*Uid(3|5I8[4|2]) 2|4r74

         --  We could also have something like
         --  16I9*I<integer>
         --  that indicates that I derives from the predefined type integer.

         Ptr := Ptr + 1;

         if Ali (Ptr) in '0' .. '9' then
            Parse_Derived_Info : declare
               P_Line   : Natural;          --  parent entity line
               P_Column : Natural;          --  parent entity column
               P_Eun    : Positive;         --  parent entity file number

            begin
               Parse_Number (Ali, Ptr, P_Line);

               --  If we have a pipe then the first number was the unit number

               if Ali (Ptr) = '|' then
                  P_Eun := P_Line;
                  Ptr := Ptr + 1;

                  --  Now we have the line number

                  Parse_Number (Ali, Ptr, P_Line);

               else
                  --  We don't have a unit number specified, so we set P_Eun to
                  --  the current unit.

                  for K in Dependencies_Tables.First .. Last (File.Dep) loop
                     P_Eun := K;
                     exit when File.Dep.Table (K) = File_Ref;
                  end loop;
               end if;

               --  Then parse the type and column number

               Ptr := Ptr + 1;
               Parse_Number (Ali, Ptr, P_Column);

               --  Skip the information for generics instantiations

               if Ali (Ptr) = '[' then
                  Skip_To_Matching_Closing_Bracket;
               end if;

               --  Skip '>', or ')' or '>'

               Ptr := Ptr + 1;

               --  The derived info is needed only is the derived info mode is
               --  on or if we want to output the type hierarchy

               if Der_Info or else Type_Tree then
                  declare
                     Symbol : constant String :=
                                Get_Symbol_Name (P_Eun, P_Line, P_Column);
                  begin
                     if Symbol /= "???" then
                        Add_Parent
                          (Decl_Ref,
                           Symbol,
                           P_Line,
                           P_Column,
                           File.Dep.Table (P_Eun));
                     end if;
                  end;
               end if;

               if Type_Tree
                 and then (Pattern.File_Ref = Empty_File
                             or else
                           Pattern.File_Ref = Current_Xref_File (File))
               then
                  Search_Parent_Tree : declare
                     Pattern         : Search_Pattern;  --  Parent type pattern
                     File_Pos_Backup : Positive;

                  begin
                     Add_Entity
                       (Pattern,
                        Get_Symbol_Name (P_Eun, P_Line, P_Column)
                        & ':' & Get_Gnatchop_File (File.Dep.Table (P_Eun))
                        & ':' & Get_Line (Get_Parent (Decl_Ref))
                        & ':' & Get_Column (Get_Parent (Decl_Ref)),
                        False);

                     --  No default match is needed to look for the parent type
                     --  since we are using the fully qualified symbol name:
                     --  symbol:file:line:column

                     Set_Default_Match (False);

                     --  The parent hierarchy is defined in the same unit as
                     --  the derived type. So we want to revisit the unit.

                     File_Pos_Backup   := File.Current_Line;

                     Skip_To_First_X_Line
                       (File, D_Lines => False, W_Lines => False);

                     while File.Buffer (File.Current_Line) /= EOF loop
                        Parse_X_Filename (File);
                        Parse_Identifier_Info
                          (Pattern       => Pattern,
                           File          => File,
                           Local_Symbols => False,
                           Der_Info      => Der_Info,
                           Type_Tree     => True,
                           Wide_Search   => False,
                           Labels_As_Ref => Labels_As_Ref);
                     end loop;

                     File.Current_Line := File_Pos_Backup;
                  end Search_Parent_Tree;
               end if;
            end Parse_Derived_Info;

         else
            while Ali (Ptr) /= '>'
              and then Ali (Ptr) /= ')'
              and then Ali (Ptr) /= '}'
            loop
               Ptr := Ptr + 1;
            end loop;
            Ptr := Ptr + 1;
         end if;
      end if;

      --  To find the body, we will have to parse the file too

      if Wide_Search then
         declare
            File_Ref : File_Reference;
            pragma Unreferenced (File_Ref);
            File_Name : constant String := Get_Gnatchop_File (File.X_File);
         begin
            File_Ref := Add_To_Xref_File (ALI_File_Name (File_Name), False);
         end;
      end if;

      --  Parse references to this entity.
      --  Ptr points to next reference with leading blanks

      loop
         --  Process references on current line

         while Ali (Ptr) = ' ' or else Ali (Ptr) = ASCII.HT loop

            --  For every reference read the line, type and column,
            --  optionally preceded by a file number and a pipe symbol.

            Parse_Number (Ali, Ptr, R_Line);

            if Ali (Ptr) = Pipe then
               Ptr := Ptr + 1;
               File_Ref := File_Name (File, R_Line);

               Parse_Number (Ali, Ptr, R_Line);
            end if;

            if Ali (Ptr) > ' ' then
               R_Type := Ali (Ptr);
               Ptr := Ptr + 1;
            end if;

            --  Imported entities might special indication as to their external
            --  name:
            --    5U14*Foo2 5>20 6b<c,myfoo2>22

            if R_Type = 'b'
              and then Ali (Ptr) = '<'
            then
               while Ptr <= Ali'Last
                 and then Ali (Ptr) /= '>'
               loop
                  Ptr := Ptr + 1;
               end loop;
               Ptr := Ptr + 1;
            end if;

            Parse_Number (Ali, Ptr, R_Col);

            --  Insert the reference or body in the table

            Add_Reference
              (Decl_Ref, File_Ref, R_Line, R_Col, R_Type, Labels_As_Ref);

            --  Skip generic information, if any

            if Ali (Ptr) = '[' then
               declare
                  Num_Nested : Integer := 1;

               begin
                  Ptr := Ptr + 1;
                  while Num_Nested /= 0 loop
                     if Ali (Ptr) = ']' then
                        Num_Nested := Num_Nested - 1;
                     elsif Ali (Ptr) = '[' then
                        Num_Nested := Num_Nested + 1;
                     end if;

                     Ptr := Ptr + 1;
                  end loop;
               end;
            end if;

         end loop;

         Parse_EOL (Ali, Ptr);

         --   Loop until new line is no continuation line

         exit when Ali (Ptr) /= '.';
         Ptr := Ptr + 1;
      end loop;
   end Parse_Identifier_Info;

   ------------------
   -- Parse_Number --
   ------------------

   procedure Parse_Number
     (Source : not null access String;
      Ptr    : in out Positive;
      Number : out Natural)
   is
   begin
      --  Skip separators

      while Source (Ptr) = ' ' or else Source (Ptr) = ASCII.HT loop
         Ptr := Ptr + 1;
      end loop;

      Number := 0;
      while Source (Ptr) in '0' .. '9' loop
         Number :=
           10 * Number + (Character'Pos (Source (Ptr)) - Character'Pos ('0'));
         Ptr := Ptr + 1;
      end loop;
   end Parse_Number;

   -----------------
   -- Parse_Token --
   -----------------

   procedure Parse_Token
     (Source    : not null access String;
      Ptr       : in out Positive;
      Token_Ptr : out Positive)
   is
      In_Quotes : Character := ASCII.NUL;

   begin
      --  Skip separators

      while Source (Ptr) = ' ' or else Source (Ptr) = ASCII.HT loop
         Ptr := Ptr + 1;
      end loop;

      Token_Ptr := Ptr;

      --  Find end-of-token

      while (In_Quotes /= ASCII.NUL or else
               not (Source (Ptr) = ' '
                     or else Source (Ptr) = ASCII.HT
                     or else Source (Ptr) = '<'
                     or else Source (Ptr) = '{'
                     or else Source (Ptr) = '['
                     or else Source (Ptr) = '='
                     or else Source (Ptr) = '('))
        and then Source (Ptr) >= ' '
      loop
         --  Double-quotes are used for operators
         --  Simple-quotes are used for character constants, for instance when
         --  they are found in an enumeration type "type A is ('+', '-');"

         case Source (Ptr) is
            when '"' | ''' =>
               if In_Quotes = Source (Ptr) then
                  In_Quotes := ASCII.NUL;
               elsif In_Quotes = ASCII.NUL then
                  In_Quotes := Source (Ptr);
               end if;

            when others =>
               null;
         end case;

         Ptr := Ptr + 1;
      end loop;
   end Parse_Token;

   ----------------------
   -- Parse_X_Filename --
   ----------------------

   procedure Parse_X_Filename (File : in out ALI_File) is
      Ali     : String_Access renames File.Buffer;
      Ptr     : Positive renames File.Current_Line;
      File_Nr : Natural;

   begin
      while Ali (Ptr) = 'X' loop

         --  The current line is the start of a new Xref file section,
         --  whose format looks like:

         --     " X 1 debug.ads"

         --  Skip the X and read the file number for the new X_File

         Ptr := Ptr + 1;
         Parse_Number (Ali, Ptr, File_Nr);

         if File_Nr > 0 then
            File.X_File := File.Dep.Table (File_Nr);
         end if;

         Parse_EOL (Ali, Ptr);
      end loop;
   end Parse_X_Filename;

   --------------------
   -- Print_Gnatfind --
   --------------------

   procedure Print_Gnatfind
     (References     : Boolean;
      Full_Path_Name : Boolean)
   is
      Decls : constant Declaration_Array_Access := Get_Declarations;
      Decl  : Declaration_Reference;
      Arr   : Reference_Array_Access;

      procedure Print_Ref
        (Ref : Reference;
         Msg : String := "      ");
      --  Print a reference, according to the extended tag of the output

      ---------------
      -- Print_Ref --
      ---------------

      procedure Print_Ref
        (Ref : Reference;
         Msg : String := "      ")
      is
         F : String_Access :=
               Osint.To_Host_File_Spec
                (Get_Gnatchop_File (Ref, Full_Path_Name));

         Buffer : constant String :=
                    F.all &
                    ":" & Get_Line (Ref)   &
                    ":" & Get_Column (Ref) &
                    ": ";

         Num_Blanks : Integer := Longest_File_Name + 10 - Buffer'Length;

      begin
         Free (F);
         Num_Blanks := Integer'Max (0, Num_Blanks);
         Write_Line
           (Buffer
            & String'(1 .. Num_Blanks => ' ')
            & Msg & " " & Get_Symbol (Decl));

         if Get_Source_Line (Ref)'Length /= 0 then
            Write_Line ("   " & Get_Source_Line (Ref));
         end if;
      end Print_Ref;

   --  Start of processing for Print_Gnatfind

   begin
      for D in Decls'Range loop
         Decl := Decls (D);

         if Match (Decl) then

            --  Output the declaration

            declare
               Parent : constant Declaration_Reference := Get_Parent (Decl);

               F : String_Access :=
                     Osint.To_Host_File_Spec
                      (Get_Gnatchop_File (Decl, Full_Path_Name));

               Buffer : constant String :=
                          F.all &
                          ":" & Get_Line (Decl)   &
                          ":" & Get_Column (Decl) &
                          ": ";

               Num_Blanks : Integer := Longest_File_Name + 10 - Buffer'Length;

            begin
               Free (F);
               Num_Blanks := Integer'Max (0, Num_Blanks);
               Write_Line
                 (Buffer & String'(1 .. Num_Blanks => ' ')
                  & "(spec) " & Get_Symbol (Decl));

               if Parent /= Empty_Declaration then
                  F := Osint.To_Host_File_Spec (Get_Gnatchop_File (Parent));
                  Write_Line
                    (Buffer & String'(1 .. Num_Blanks => ' ')
                     & "   derived from " & Get_Symbol (Parent)
                     & " ("
                     & F.all
                     & ':' & Get_Line (Parent)
                     & ':' & Get_Column (Parent) & ')');
                  Free (F);
               end if;
            end;

            if Get_Source_Line (Decl)'Length /= 0 then
               Write_Line ("   " & Get_Source_Line (Decl));
            end if;

            --  Output the body (sorted)

            Arr := Get_References (Decl, Get_Bodies => True);

            for R in Arr'Range loop
               Print_Ref (Arr (R), "(body)");
            end loop;

            Free (Arr);

            if References then
               Arr := Get_References
                 (Decl, Get_Writes => True, Get_Reads => True);

               for R in Arr'Range loop
                  Print_Ref (Arr (R));
               end loop;

               Free (Arr);
            end if;
         end if;
      end loop;
   end Print_Gnatfind;

   ------------------
   -- Print_Unused --
   ------------------

   procedure Print_Unused (Full_Path_Name : Boolean) is
      Decls : constant Declaration_Array_Access := Get_Declarations;
      Decl  : Declaration_Reference;
      Arr   : Reference_Array_Access;
      F     : String_Access;

   begin
      for D in Decls'Range loop
         Decl := Decls (D);

         if References_Count
             (Decl, Get_Reads => True, Get_Writes => True) = 0
         then
            F := Osint.To_Host_File_Spec
              (Get_Gnatchop_File (Decl, Full_Path_Name));
            Write_Str (Get_Symbol (Decl)
                        & " ("
                        & Get_Full_Type (Decl)
                        & ") "
                        & F.all
                        & ':'
                        & Get_Line (Decl)
                        & ':'
                        & Get_Column (Decl));
            Free (F);

            --  Print the body if any

            Arr := Get_References (Decl, Get_Bodies => True);

            for R in Arr'Range loop
               F := Osint.To_Host_File_Spec
                      (Get_Gnatchop_File (Arr (R), Full_Path_Name));
               Write_Str (' '
                           & F.all
                           & ':' & Get_Line (Arr (R))
                           & ':' & Get_Column (Arr (R)));
               Free (F);
            end loop;

            Write_Eol;
            Free (Arr);
         end if;
      end loop;
   end Print_Unused;

   --------------
   -- Print_Vi --
   --------------

   procedure Print_Vi (Full_Path_Name : Boolean) is
      Tab   : constant Character := ASCII.HT;
      Decls : constant Declaration_Array_Access :=
                Get_Declarations (Sorted => False);
      Decl  : Declaration_Reference;
      Arr   : Reference_Array_Access;
      F     : String_Access;

   begin
      for D in Decls'Range loop
         Decl := Decls (D);

         F := Osint.To_Host_File_Spec (Get_File (Decl, Full_Path_Name));
         Write_Line (Get_Symbol (Decl) & Tab & F.all & Tab & Get_Line (Decl));
         Free (F);

         --  Print the body if any

         Arr := Get_References (Decl, Get_Bodies => True);

         for R in Arr'Range loop
            F := Osint.To_Host_File_Spec (Get_File (Arr (R), Full_Path_Name));
            Write_Line
              (Get_Symbol (Decl) & Tab & F.all & Tab  & Get_Line (Arr (R)));
            Free (F);
         end loop;

         Free (Arr);

         --  Print the modifications

         Arr := Get_References (Decl, Get_Writes => True, Get_Reads => True);

         for R in Arr'Range loop
            F := Osint.To_Host_File_Spec (Get_File (Arr (R), Full_Path_Name));
            Write_Line
              (Get_Symbol (Decl) & Tab & F.all & Tab & Get_Line (Arr (R)));
            Free (F);
         end loop;

         Free (Arr);
      end loop;
   end Print_Vi;

   ----------------
   -- Print_Xref --
   ----------------

   procedure Print_Xref (Full_Path_Name : Boolean) is
      Decls : constant Declaration_Array_Access := Get_Declarations;
      Decl : Declaration_Reference;

      Margin : constant := 10;
      --  Column where file names start

      procedure New_Line80;
      --  Go to start of new line

      procedure Print80 (S : String);
      --  Print the text, respecting the 80 columns rule

      procedure Print_Ref (Line, Column : String);
      --  The beginning of the output is aligned on a column multiple of 9

      procedure Print_List
        (Decl       : Declaration_Reference;
         Msg        : String;
         Get_Reads  : Boolean := False;
         Get_Writes : Boolean := False;
         Get_Bodies : Boolean := False);
      --  Print a list of references. If the list is not empty, Msg will
      --  be printed prior to the list.

      ----------------
      -- New_Line80 --
      ----------------

      procedure New_Line80 is
      begin
         Write_Eol;
         Write_Str (String'(1 .. Margin - 1 => ' '));
      end New_Line80;

      -------------
      -- Print80 --
      -------------

      procedure Print80 (S : String) is
         Align : Natural := Margin - (Integer (Column) mod Margin);

      begin
         if Align = Margin then
            Align := 0;
         end if;

         Write_Str (String'(1 .. Align => ' ') & S);
      end Print80;

      ---------------
      -- Print_Ref --
      ---------------

      procedure Print_Ref (Line, Column : String) is
         Line_Align : constant Integer := 4 - Line'Length;

         S : constant String := String'(1 .. Line_Align => ' ')
                                  & Line & ':' & Column;

         Align : Natural := Margin - (Integer (Output.Column) mod Margin);

      begin
         if Align = Margin then
            Align := 0;
         end if;

         if Integer (Output.Column) + Align + S'Length > 79 then
            New_Line80;
            Align := 0;
         end if;

         Write_Str (String'(1 .. Align => ' ') & S);
      end Print_Ref;

      ----------------
      -- Print_List --
      ----------------

      procedure Print_List
        (Decl       : Declaration_Reference;
         Msg        : String;
         Get_Reads  : Boolean := False;
         Get_Writes : Boolean := False;
         Get_Bodies : Boolean := False)
      is
         Arr : Reference_Array_Access :=
                 Get_References
                   (Decl,
                    Get_Writes => Get_Writes,
                    Get_Reads  => Get_Reads,
                    Get_Bodies => Get_Bodies);
         File : File_Reference := Empty_File;
         F    : String_Access;

      begin
         if Arr'Length /= 0 then
            Write_Eol;
            Write_Str (Msg);
         end if;

         for R in Arr'Range loop
            if Get_File_Ref (Arr (R)) /= File then
               if File /= Empty_File then
                  New_Line80;
               end if;

               File := Get_File_Ref (Arr (R));
               F := Osint.To_Host_File_Spec
                 (Get_Gnatchop_File (Arr (R), Full_Path_Name));
               Write_Str (F.all & ' ');
               Free (F);
            end if;

            Print_Ref (Get_Line (Arr (R)), Get_Column (Arr (R)));
         end loop;

         Free (Arr);
      end Print_List;

      F : String_Access;

   --  Start of processing for Print_Xref

   begin
      for D in Decls'Range loop
         Decl := Decls (D);

         Write_Str (Get_Symbol (Decl));

         while Column < Type_Position loop
            Write_Char (' ');
         end loop;

         Write_Line (Get_Full_Type (Decl));

         Write_Parent_Info : declare
            Parent : constant Declaration_Reference := Get_Parent (Decl);

         begin
            if Parent /= Empty_Declaration then
               Write_Str ("  Ptype: ");
               F := Osint.To_Host_File_Spec (Get_Gnatchop_File (Parent));
               Print80 (F.all);
               Free (F);
               Print_Ref (Get_Line (Parent), Get_Column (Parent));
               Print80 ("  " & Get_Symbol (Parent));
               Write_Eol;
            end if;
         end Write_Parent_Info;

         Write_Str ("  Decl:  ");
         F := Osint.To_Host_File_Spec
               (Get_Gnatchop_File (Decl, Full_Path_Name));
         Print80 (F.all & ' ');
         Free (F);
         Print_Ref (Get_Line (Decl), Get_Column (Decl));

         Print_List
           (Decl, "  Body:  ", Get_Bodies => True);
         Print_List
           (Decl, "  Modi:  ", Get_Writes => True);
         Print_List
           (Decl, "  Ref:   ", Get_Reads => True);
         Write_Eol;
      end loop;
   end Print_Xref;

   ------------
   -- Search --
   ------------

   procedure Search
     (Pattern       : Search_Pattern;
      Local_Symbols : Boolean;
      Wide_Search   : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean;
      Type_Tree     : Boolean)
   is
      type String_Access is access String;
      procedure Free is new Unchecked_Deallocation (String, String_Access);

      ALIfile   : ALI_File;
      File_Ref  : File_Reference;
      Strip_Num : Natural := 0;
      Ali_Name  : String_Access;

   begin
      --  If we want all the .ali files, then find them

      if Wide_Search then
         Find_ALI_Files;
      end if;

      loop
         --  Get the next unread ali file

         File_Ref := Next_Unvisited_File;

         exit when File_Ref = Empty_File;

         --  Find the ALI file to use. Most of the time, it will be the unit
         --  name, with a different extension. However, when dealing with
         --  separates the ALI file is in fact the parent's ALI file (and this
         --  is recursive, in case the parent itself is a separate).

         Strip_Num := 0;
         loop
            Free (Ali_Name);
            Ali_Name := new String'
              (Get_File (File_Ref, With_Dir => True, Strip => Strip_Num));

            --  Stripped too many things...

            if Ali_Name.all = "" then
               if Get_Emit_Warning (File_Ref) then
                  Set_Standard_Error;
                  Write_Line
                    ("warning : file " & Get_File (File_Ref, With_Dir => True)
                     & " not found");
                  Set_Standard_Output;
               end if;
               Free (Ali_Name);
               exit;

            --  If not found, try the parent's ALI file (this is needed for
            --  separate units and subprograms).

            --  Reset the cached directory first, in case the separate's
            --  ALI file is not in the same directory.

            elsif not File_Exists (Ali_Name.all) then
               Strip_Num := Strip_Num + 1;
               Reset_Directory (File_Ref);

            --  Else we finally found it

            else
               exit;
            end if;
         end loop;

         --  If we had to get the parent's ALI, insert it in the list as usual.
         --  This is to avoid parsing it twice in case it has already been
         --  parsed.

         if Ali_Name /= null and then Strip_Num /= 0 then
            File_Ref := Add_To_Xref_File
              (File_Name => Ali_Name.all,
               Visited   => False);

         --  Now that we have a file name, parse it to find any reference to
         --  the entity.

         elsif Ali_Name /= null
           and then (Read_Only or else Is_Writable_File (Ali_Name.all))
         then
            begin
               Open (Ali_Name.all, ALIfile);
               while ALIfile.Buffer (ALIfile.Current_Line) /= EOF loop
                  Parse_X_Filename (ALIfile);
                  Parse_Identifier_Info
                    (Pattern, ALIfile, Local_Symbols,
                     Der_Info, Type_Tree, Wide_Search, Labels_As_Ref => True);
               end loop;

            exception
               when No_Xref_Information   =>
                  if Get_Emit_Warning (File_Ref) then
                     Set_Standard_Error;
                     Write_Line
                       ("warning : No cross-referencing information in  "
                        & Ali_Name.all);
                     Set_Standard_Output;
                  end if;
            end;
         end if;
      end loop;

      Free (Ali_Name);
   end Search;

   -----------------
   -- Search_Xref --
   -----------------

   procedure Search_Xref
     (Local_Symbols : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean)
   is
      ALIfile      : ALI_File;
      File_Ref     : File_Reference;
      Null_Pattern : Search_Pattern;

   begin
      Null_Pattern.Initialized := False;

      loop
         --  Find the next unvisited file

         File_Ref := Next_Unvisited_File;
         exit when File_Ref = Empty_File;

         --  Search the object directories for the .ali file

         declare
            F : constant String := Get_File (File_Ref, With_Dir => True);

         begin
            if Read_Only or else Is_Writable_File (F) then
               Open (F, ALIfile, True);

               while ALIfile.Buffer (ALIfile.Current_Line) /= EOF loop
                  Parse_X_Filename (ALIfile);
                  Parse_Identifier_Info
                    (Null_Pattern, ALIfile, Local_Symbols, Der_Info,
                     Labels_As_Ref => False);
               end loop;
            end if;

         exception
            when No_Xref_Information =>  null;
         end;
      end loop;
   end Search_Xref;

end Xref_Lib;
