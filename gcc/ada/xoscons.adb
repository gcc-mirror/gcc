------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                              X O S C O N S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2009, Free Software Foundation, Inc.         --
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

--  This program generates the spec of System.OS_Constants (s-oscons.ads)

--  It works in conjunction with a C template file which must be pre-processed
--  and compiled using the cross compiler. Two input files are used:
--    - the preprocessed C file: s-oscons-tmplt.i
--    - the generated assembly file: s-oscons-tmplt.s

--  The contents of s-oscons.ads is written on standard output

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;

pragma Warnings (Off);
--  System.Unsigned_Types is an internal GNAT unit
with System.Unsigned_Types;   use System.Unsigned_Types;
pragma Warnings (On);

with GNAT.Table;

with XUtil;                   use XUtil;

procedure XOSCons is

   use ASCII;
   use Ada.Strings;

   Unit_Name : constant String := "s-oscons";
   Tmpl_Name : constant String := Unit_Name & "-tmplt";

   -------------------------------------------------
   -- Information retrieved from assembly listing --
   -------------------------------------------------

   type String_Access is access all String;
   --  Note: we can't use GNAT.Strings for this definition, since that unit
   --  is not available in older base compilers.

   --  We need to deal with integer values that can be signed or unsigned, so
   --  we need to accomodate the maximum range of both cases.

   type Int_Value_Type is record
      Positive  : Boolean;
      Abs_Value : Long_Unsigned := 0;
   end record;

   type Asm_Info_Kind is
     (CND,     --  Constant (decimal)
      CNS,     --  Constant (freeform string)
      TXT);    --  Literal text
   --  Recognized markers found in assembly file. These markers are produced by
   --  the same-named macros from the C template.

   type Asm_Info (Kind : Asm_Info_Kind := TXT) is record
      Line_Number   : Integer;
      --  Line number in C source file

      Constant_Name : String_Access;
      --  Name of constant to be defined

      Value_Len     : Natural := 0;
      --  Length of text representation of constant's value

      Text_Value    : String_Access;
      --  Value for CNS constant

      Int_Value     : Int_Value_Type;
      --  Value for CND constant

      Comment       : String_Access;
      --  Additional descriptive comment for constant, or free-form text (TXT)
   end record;

   package Asm_Infos is new GNAT.Table
     (Table_Component_Type => Asm_Info,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 10);

   Max_Const_Name_Len     : Natural := 0;
   Max_Constant_Value_Len : Natural := 0;
   --  Lengths of longest name and longest value

   type Language is (Lang_Ada, Lang_C);

   procedure Output_Info
     (Lang       : Language;
      OFile      : Sfile;
      Info_Index : Integer);
   --  Output information from the indicated asm info line

   procedure Parse_Asm_Line (Line : String);
   --  Parse one information line from the assembly source

   function Contains_Template_Name (S : String) return Boolean;
   --  True if S contains Tmpl_Name, possibly with different casing

   function Spaces (Count : Integer) return String;
   --  If Count is positive, return a string of Count spaces, else return an
   --  empty string.

   ----------------------------
   -- Contains_Template_Name --
   ----------------------------

   function Contains_Template_Name (S : String) return Boolean is
   begin
      if Index (Source => To_Lower (S), Pattern => Tmpl_Name) > 0 then
         return True;
      else
         return False;
      end if;
   end Contains_Template_Name;

   -----------------
   -- Output_Info --
   -----------------

   procedure Output_Info
     (Lang       : Language;
      OFile      : Sfile;
      Info_Index : Integer)
   is
      Info : Asm_Info renames Asm_Infos.Table (Info_Index);

      procedure Put (S : String);
      --  Write S to OFile

      ---------
      -- Put --
      ---------

      procedure Put (S : String) is
      begin
         Put (OFile, S);
      end Put;

   begin
      if Info.Kind /= TXT then
         --  TXT case is handled by the common code below

         case Lang is
            when Lang_Ada =>
               Put ("   " & Info.Constant_Name.all);
               Put (Spaces (Max_Const_Name_Len - Info.Constant_Name'Length));

               Put (" : constant := ");

            when Lang_C =>
               Put ("#define " & Info.Constant_Name.all & " ");
               Put (Spaces (Max_Const_Name_Len - Info.Constant_Name'Length));
         end case;

         if Info.Kind = CND then
            if not Info.Int_Value.Positive then
               Put ("-");
            end if;
            Put (Trim (Info.Int_Value.Abs_Value'Img, Side => Left));
         else
            Put (Info.Text_Value.all);
         end if;

         if Lang = Lang_Ada then
            Put (";");

            if Info.Comment'Length > 0 then
               Put (Spaces (Max_Constant_Value_Len - Info.Value_Len));
               Put (" --  ");
            end if;
         end if;
      end if;

      if Lang = Lang_Ada then
         Put (Info.Comment.all);
      end if;

      New_Line (OFile);
   end Output_Info;

   --------------------
   -- Parse_Asm_Line --
   --------------------

   procedure Parse_Asm_Line (Line : String) is
      Index1, Index2 : Integer := Line'First;

      function Field_Alloc return String_Access;
      --  Allocate and return a copy of Line (Index1 .. Index2 - 1)

      procedure Find_Colon (Index : in out Integer);
      --  Increment Index until the next colon in Line

      function Parse_Int (S : String) return Int_Value_Type;
      --  Parse a decimal number, preceded by an optional '$' or '#' character,
      --  and return its value.

      -----------------
      -- Field_Alloc --
      -----------------

      function Field_Alloc return String_Access is
      begin
         return new String'(Line (Index1 .. Index2 - 1));
      end Field_Alloc;

      ----------------
      -- Find_Colon --
      ----------------

      procedure Find_Colon (Index : in out Integer) is
      begin
         loop
            Index := Index + 1;
            exit when Index > Line'Last or else Line (Index) = ':';
         end loop;
      end Find_Colon;

      ---------------
      -- Parse_Int --
      ---------------

      function Parse_Int (S : String) return Int_Value_Type is
         First    : Integer := S'First;
         Positive : Boolean;
      begin
         --  On some platforms, immediate integer values are prefixed with
         --  a $ or # character in assembly output.

         if S (First) = '$' or else S (First) = '#' then
            First := First + 1;
         end if;

         if S (First) = '-' then
            Positive := False;
            First    := First + 1;
         else
            Positive := True;
         end if;

         return (Positive  => Positive,
                 Abs_Value => Long_Unsigned'Value (S (First .. S'Last)));

      exception
         when E : others =>
            Put_Line (Standard_Error, "can't parse decimal value: " & S);
            raise;
      end Parse_Int;

   --  Start of processing for Parse_Asm_Line

   begin
      Find_Colon (Index2);

      declare
         Info : Asm_Info (Kind => Asm_Info_Kind'Value
                                    (Line (Line'First .. Index2 - 1)));
      begin
         Index1 := Index2 + 1;
         Find_Colon (Index2);

         Info.Line_Number :=
           Integer (Parse_Int (Line (Index1 .. Index2 - 1)).Abs_Value);

         case Info.Kind is
            when CND | CNS =>
               Index1 := Index2 + 1;
               Find_Colon (Index2);

               Info.Constant_Name := Field_Alloc;
               if Info.Constant_Name'Length > Max_Const_Name_Len then
                  Max_Const_Name_Len := Info.Constant_Name'Length;
               end if;

               Index1 := Index2 + 1;
               Find_Colon (Index2);

               if Info.Kind = CND then
                  Info.Int_Value := Parse_Int (Line (Index1 .. Index2 - 1));
                  Info.Value_Len := Index2 - Index1 - 1;

               else
                  Info.Text_Value := Field_Alloc;
                  Info.Value_Len  := Info.Text_Value'Length;
               end if;

            when others =>
               null;
         end case;

         Index1 := Index2 + 1;
         Index2 := Line'Last + 1;
         Info.Comment := Field_Alloc;

         if Info.Kind = TXT then
            Info.Text_Value := Info.Comment;

         --  Update Max_Constant_Value_Len, but only if this constant has a
         --  comment (else the value is allowed to be longer).

         elsif Info.Comment'Length > 0 then
            if Info.Value_Len > Max_Constant_Value_Len then
               Max_Constant_Value_Len := Info.Value_Len;
            end if;
         end if;

         Asm_Infos.Append (Info);
      end;
   exception
      when E : others =>
         Put_Line (Standard_Error,
           "can't parse " & Line);
         Put_Line (Standard_Error,
           "exception raised: " & Exception_Information (E));
   end Parse_Asm_Line;

   ------------
   -- Spaces --
   ------------

   function Spaces (Count : Integer) return String is
   begin
      if Count <= 0 then
         return "";
      else
         return (1 .. Count => ' ');
      end if;
   end Spaces;

   --  Local declarations

   --  Input files

   Tmpl_File_Name : constant String := Tmpl_Name & ".i";
   Asm_File_Name  : constant String := Tmpl_Name & ".s";

   --  Output files

   Ada_File_Name  : constant String := Unit_Name & ".ads";
   C_File_Name    : constant String := Unit_Name & ".h";

   Asm_File  : Ada.Text_IO.File_Type;
   Tmpl_File : Ada.Text_IO.File_Type;
   Ada_OFile : Sfile;
   C_OFile   : Sfile;

   Line : String (1 .. 256);
   Last : Integer;
   --  Line being processed

   Current_Line : Integer;
   Current_Info : Integer;
   In_Comment   : Boolean;
   In_Template  : Boolean;

--  Start of processing for XOSCons

begin
   --  Load values from assembly file

   Open (Asm_File, In_File, Asm_File_Name);

   while not End_Of_File (Asm_File) loop
      Get_Line (Asm_File, Line, Last);
      if Last > 2 and then Line (1 .. 2) = "->" then
         Parse_Asm_Line (Line (3 .. Last));
      end if;
   end loop;

   Close (Asm_File);

   --  Load C template and output definitions

   Open   (Tmpl_File, In_File,  Tmpl_File_Name);
   Create (Ada_OFile, Out_File, Ada_File_Name);
   Create (C_OFile,   Out_File, C_File_Name);

   Current_Line := 0;
   Current_Info := Asm_Infos.First;
   In_Comment   := False;

   while not End_Of_File (Tmpl_File) loop
      <<Get_One_Line>>
      Get_Line (Tmpl_File, Line, Last);

      if Last >= 2 and then Line (1 .. 2) = "# " then
         declare
            Index : Integer := 3;
         begin
            while Index <= Last and then Line (Index) in '0' .. '9' loop
               Index := Index + 1;
            end loop;

            if Contains_Template_Name (Line (Index + 1 .. Last)) then
               Current_Line := Integer'Value (Line (3 .. Index - 1));
               In_Template  := True;
               goto Get_One_Line;
            else
               In_Template := False;
            end if;
         end;

      elsif In_Template then
         if In_Comment then
            if Line (1 .. Last) = "*/" then
               Put_Line (C_OFile, Line (1 .. Last));
               In_Comment := False;
            else
               Put_Line (Ada_OFile, Line (1 .. Last));
               Put_Line (C_OFile, Line (1 .. Last));
            end if;

         elsif Line (1 .. Last) = "/*" then
            Put_Line (C_OFile, Line (1 .. Last));
            In_Comment := True;

         elsif Asm_Infos.Table (Current_Info).Line_Number = Current_Line then
            Output_Info (Lang_Ada, Ada_OFile, Current_Info);
            Output_Info (Lang_C,   C_OFile,   Current_Info);
            Current_Info := Current_Info + 1;
         end if;

         Current_Line := Current_Line + 1;
      end if;
   end loop;

   Close (Tmpl_File);

end XOSCons;
