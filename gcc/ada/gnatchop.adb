------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T C H O P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2020, Free Software Foundation, Inc.         --
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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Directories;            use Ada.Directories;
with Ada.Streams.Stream_IO;      use Ada.Streams;
with Ada.Text_IO;                use Ada.Text_IO;
with System.CRTL;                use System; use System.CRTL;

with GNAT.Byte_Order_Mark;       use GNAT.Byte_Order_Mark;
with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Heap_Sort_G;
with GNAT.Table;

with Switch;                     use Switch;
with Types;

procedure Gnatchop is

   Config_File_Name : constant String_Access := new String'("gnat.adc");
   --  The name of the file holding the GNAT configuration pragmas

   Gcc : String_Access := new String'("gcc");
   --  May be modified by switch --GCC=

   Gcc_Set : Boolean := False;
   --  True if a switch --GCC= is used

   Gnat_Cmd : String_Access;
   --  Command to execute the GNAT compiler

   Gnat_Args : Argument_List_Access :=
                 new Argument_List'
                   (new String'("-c"),
                    new String'("-x"),
                    new String'("ada"),
                    new String'("-gnats"),
                    new String'("-gnatu"));
   --  Arguments used in Gnat_Cmd call

   EOF : constant Character := Character'Val (26);
   --  Special character to signal end of file. Not required in input files,
   --  but properly treated if present. Not generated in output files except
   --  as a result of copying input file.

   BOM_Length : Natural := 0;
   --  Reset to non-zero value if BOM detected at start of file

   --------------------
   -- File arguments --
   --------------------

   subtype File_Num is Natural;
   subtype File_Offset is Natural;

   type File_Entry is record
      Name : String_Access;
      --  Name of chop file or directory

      SR_Name : String_Access;
      --  Null unless the chop file starts with a source reference pragma
      --  in which case this field points to the file name from this pragma.
   end record;

   package File is new GNAT.Table
     (Table_Component_Type => File_Entry,
      Table_Index_Type     => File_Num,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100);

   Directory : String_Access;
   --  Record name of directory, or a null string if no directory given

   Compilation_Mode  : Boolean := False;
   Overwrite_Files   : Boolean := False;
   Preserve_Mode     : Boolean := False;
   Quiet_Mode        : Boolean := False;
   Source_References : Boolean := False;
   Verbose_Mode      : Boolean := False;
   Exit_On_Error     : Boolean := False;
   --  Global options

   Write_gnat_adc : Boolean := False;
   --  Gets set true if we append to gnat.adc or create a new gnat.adc.
   --  Used to inhibit complaint about no units generated.

   ---------------
   -- Unit list --
   ---------------

   type Line_Num is new Natural;
   --  Line number (for source reference pragmas)

   type Unit_Count_Type  is new Integer;
   subtype Unit_Num      is Unit_Count_Type range 1 .. Unit_Count_Type'Last;
   --  Used to refer to unit number in unit table

   type SUnit_Num is new Integer;
   --  Used to refer to entry in sorted units table. Note that entry
   --  zero is only for use by Heapsort, and is not otherwise referenced.

   type Unit_Kind is (Unit_Spec, Unit_Body, Config_Pragmas);

   --  Structure to contain all necessary information for one unit.
   --  Entries are also temporarily used to record config pragma sequences.

   type Unit_Info is record
      File_Name : String_Access;
      --  File name from GNAT output line

      Chop_File : File_Num;
      --  File number in chop file sequence

      Start_Line : Line_Num;
      --  Line number from GNAT output line

      Offset : File_Offset;
      --  Offset name from GNAT output line

      SR_Present : Boolean;
      --  Set True if SR parameter present

      Length : File_Offset;
      --  A length of 0 means that the Unit is the last one in the file

      Kind : Unit_Kind;
      --  Indicates kind of unit

      Sorted_Index : SUnit_Num;
      --  Index of unit in sorted unit list

      Bufferg : String_Access;
      --  Pointer to buffer containing configuration pragmas to be prepended.
      --  Null if no pragmas to be prepended.
   end record;

   --  The following table stores the unit offset information

   package Unit is new GNAT.Table
     (Table_Component_Type => Unit_Info,
      Table_Index_Type     => Unit_Count_Type,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100);

   --  The following table is used as a sorted index to the Unit.Table.
   --  The entries in Unit.Table are not moved, instead we just shuffle
   --  the entries in Sorted_Units. Note that the zeroeth entry in this
   --  table is used by GNAT.Heap_Sort_G.

   package Sorted_Units is new GNAT.Table
     (Table_Component_Type => Unit_Num,
      Table_Index_Type     => SUnit_Num,
      Table_Low_Bound      => 0,
      Table_Initial        => 500,
      Table_Increment      => 100);

   function Is_Duplicated (U : SUnit_Num) return Boolean;
   --  Returns true if U is duplicated by a later unit.
   --  Note that this function returns false for the last entry.

   procedure Sort_Units;
   --  Sort units and set up sorted unit table

   ----------------------
   -- File_Descriptors --
   ----------------------

   function dup  (handle   : File_Descriptor) return File_Descriptor;
   function dup2 (from, to : File_Descriptor) return File_Descriptor;

   ---------------------
   -- Local variables --
   ---------------------

   Warning_Count : Natural := 0;
   --  Count of warnings issued so far

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Error_Msg (Message : String; Warning : Boolean := False);
   --  Produce an error message on standard error output

   function Files_Exist return Boolean;
   --  Check Unit.Table for possible file names that already exist
   --  in the file system. Returns true if files exist, False otherwise

   function Get_Maximum_File_Name_Length return Integer;
   pragma Import (C, Get_Maximum_File_Name_Length,
                 "__gnat_get_maximum_file_name_length");
   --  Function to get maximum file name length for system

   Maximum_File_Name_Length : constant Integer := Get_Maximum_File_Name_Length;
   Maximum_File_Name_Length_String : constant String :=
                                       Integer'Image
                                         (Maximum_File_Name_Length);

   function Locate_Executable
     (Program_Name    : String;
      Look_For_Prefix : Boolean := True) return String_Access;
   --  Locate executable for given program name. This takes into account
   --  the target-prefix of the current command, if Look_For_Prefix is True.

   subtype EOL_Length is Natural range 0 .. 2;
   --  Possible lengths of end of line sequence

   type EOL_String (Len : EOL_Length := 0) is record
      Str : String (1 .. Len);
   end record;

   function Get_EOL
     (Source : not null access String;
      Start  : Positive) return EOL_String;
   --  Return the line terminator used in the passed string

   procedure Parse_EOL
     (Source : not null access String;
      Ptr    : in out Positive);
   --  On return Source (Ptr) is the first character of the next line
   --  or EOF. Source.all must be terminated by EOF.

   function Parse_File (Num : File_Num) return Boolean;
   --  Calls the GNAT compiler to parse the given source file and parses the
   --  output using Parse_Offset_Info. Returns True if parse operation
   --  completes, False if some system error (e.g. failure to read the
   --  offset information) occurs.

   procedure Parse_Offset_Info
     (Chop_File : File_Num;
      Source    : not null access String);
   --  Parses the output of the compiler indicating the offsets and names of
   --  the compilation units in Chop_File.

   procedure Parse_Token
     (Source    : not null access String;
      Ptr       : in out Positive;
      Token_Ptr : out Positive);
   --  Skips any separators and stores the start of the token in Token_Ptr.
   --  Then stores the position of the next separator in Ptr. On return
   --  Source (Token_Ptr .. Ptr - 1) is the token.

   procedure Read_File
     (FD       : File_Descriptor;
      Contents : out String_Access;
      Success  : out Boolean);
   --  Reads file associated with FS into the newly allocated string Contents.
   --  Success is true iff the number of bytes read is equal to the file size.

   function Report_Duplicate_Units return Boolean;
   --  Output messages about duplicate units in the input files in Unit.Table
   --  Returns True if any duplicates found, False if no duplicates found.

   function Scan_Arguments return Boolean;
   --  Scan command line options and set global variables accordingly.
   --  Also scan out file and directory arguments. Returns True if scan
   --  was successful, and False if the scan fails for any reason.

   procedure Usage;
   --  Output message on standard output describing syntax of gnatchop command

   procedure Warning_Msg (Message : String);
   --  Output a warning message on standard error and update warning count

   function Write_Chopped_Files (Input : File_Num) return Boolean;
   --  Write all units that result from chopping the Input file

   procedure Write_Config_File (Input : File_Num; U : Unit_Num);
   --  Call to write configuration pragmas (append them to gnat.adc). Input is
   --  the file number for the chop file and U identifies the unit entry for
   --  the configuration pragmas.

   function Get_Config_Pragmas
     (Input : File_Num;
      U     : Unit_Num) return String_Access;
   --  Call to read configuration pragmas from given unit entry, and return a
   --  buffer containing the pragmas to be appended to following units. Input
   --  is the file number for the chop file and U identifies the unit entry for
   --  the configuration pragmas.

   procedure Write_Source_Reference_Pragma
     (Info    : Unit_Info;
      Line    : Line_Num;
      File    : Stream_IO.File_Type;
      EOL     : EOL_String;
      Success : in out Boolean);
   --  If Success is True on entry, writes a source reference pragma using
   --  the chop file from Info, and the given line number. On return Success
   --  indicates whether the write succeeded. If Success is False on entry,
   --  or if the global flag Source_References is False, then the call to
   --  Write_Source_Reference_Pragma has no effect. EOL indicates the end
   --  of line sequence to be written at the end of the pragma.

   procedure Write_Unit
     (Source    : not null access String;
      Num       : Unit_Num;
      TS_Time   : OS_Time;
      Write_BOM : Boolean;
      Success   : out Boolean);
   --  Write one compilation unit of the source to file. Source is the pointer
   --  to the input string, Num is the unit number, TS_Time is the timestamp,
   --  Write_BOM is set True to write a UTF-8 BOM at the start of the file.
   --  Success is set True unless the write attempt fails.

   ---------
   -- dup --
   ---------

   function dup (handle : File_Descriptor) return File_Descriptor is
   begin
      return File_Descriptor (System.CRTL.dup (int (handle)));
   end dup;

   ----------
   -- dup2 --
   ----------

   function dup2 (from, to : File_Descriptor) return File_Descriptor is
   begin
      return File_Descriptor (System.CRTL.dup2 (int (from), int (to)));
   end dup2;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Message : String; Warning : Boolean := False) is
   begin
      Put_Line (Standard_Error, Message);

      if not Warning then
         Set_Exit_Status (Failure);

         if Exit_On_Error then
            raise Types.Terminate_Program;
         end if;
      end if;
   end Error_Msg;

   -----------------
   -- Files_Exist --
   -----------------

   function Files_Exist return Boolean is
      Exists : Boolean := False;

   begin
      for SNum in 1 .. SUnit_Num (Unit.Last) loop

         --  Only check and report for the last instance of duplicated files

         if not Is_Duplicated (SNum) then
            declare
               Info : constant Unit_Info :=
                        Unit.Table (Sorted_Units.Table (SNum));

            begin
               if Is_Writable_File (Info.File_Name.all) then
                  Error_Msg (Info.File_Name.all
                              & " already exists, use -w to overwrite");
                  Exists := True;
               end if;
            end;
         end if;
      end loop;

      return Exists;
   end Files_Exist;

   ------------------------
   -- Get_Config_Pragmas --
   ------------------------

   function Get_Config_Pragmas
     (Input : File_Num;
      U     : Unit_Num) return String_Access
   is
      Info    : Unit_Info renames Unit.Table (U);
      FD      : File_Descriptor;
      Name    : aliased constant String :=
                  File.Table (Input).Name.all & ASCII.NUL;
      Length  : File_Offset;
      Buffer  : String_Access;
      Result  : String_Access;

      Success : Boolean;
      pragma Warnings (Off, Success);

   begin
      FD := Open_Read (Name'Address, Binary);

      if FD = Invalid_FD then
         Error_Msg ("cannot open " & File.Table (Input).Name.all);
         return null;
      end if;

      Read_File (FD, Buffer, Success);

      --  A length of 0 indicates that the rest of the file belongs to
      --  this unit. The actual length must be calculated now. Take into
      --  account that the last character (EOF) must not be written.

      if Info.Length = 0 then
         Length := Buffer'Last - (Buffer'First + Info.Offset);
      else
         Length := Info.Length;
      end if;

      Result := new String'(Buffer (1 .. Length));
      Close (FD);
      return Result;
   end Get_Config_Pragmas;

   -------------
   -- Get_EOL --
   -------------

   function Get_EOL
     (Source : not null access String;
      Start  : Positive) return EOL_String
   is
      Ptr   : Positive := Start;
      First : Positive;
      Last  : Natural;

   begin
      --  Skip to end of line

      while Source (Ptr) /= ASCII.CR and then
            Source (Ptr) /= ASCII.LF and then
            Source (Ptr) /= EOF
      loop
         Ptr := Ptr + 1;
      end loop;

      Last  := Ptr;

      if Source (Ptr) /= EOF then

         --  Found CR or LF

         First := Ptr;

      else
         First := Ptr + 1;
      end if;

      --  Recognize CR/LF

      if Source (Ptr) = ASCII.CR and then Source (Ptr + 1) = ASCII.LF then
         Last := First + 1;
      end if;

      return (Len => Last + 1 - First, Str => Source (First .. Last));
   end Get_EOL;

   -------------------
   -- Is_Duplicated --
   -------------------

   function Is_Duplicated (U : SUnit_Num) return Boolean is
   begin
      return U < SUnit_Num (Unit.Last)
        and then
          Unit.Table (Sorted_Units.Table (U)).File_Name.all =
          Unit.Table (Sorted_Units.Table (U + 1)).File_Name.all;
   end Is_Duplicated;

   -----------------------
   -- Locate_Executable --
   -----------------------

   function Locate_Executable
     (Program_Name    : String;
      Look_For_Prefix : Boolean := True) return String_Access
   is
      Gnatchop_Str    : constant String := "gnatchop";
      Current_Command : constant String := Normalize_Pathname (Command_Name);
      End_Of_Prefix   : Natural;
      Start_Of_Prefix : Positive;
      Start_Of_Suffix : Positive;
      Result          : String_Access;

   begin
      Start_Of_Prefix := Current_Command'First;
      Start_Of_Suffix := Current_Command'Last + 1;
      End_Of_Prefix   := Start_Of_Prefix - 1;

      if Look_For_Prefix then

         --  Find Start_Of_Prefix

         for J in reverse Current_Command'Range loop
            if Current_Command (J) = '/'                 or else
               Current_Command (J) = Directory_Separator or else
               Current_Command (J) = ':'
            then
               Start_Of_Prefix := J + 1;
               exit;
            end if;
         end loop;

         --  Find End_Of_Prefix

         for J in Start_Of_Prefix ..
                  Current_Command'Last - Gnatchop_Str'Length + 1
         loop
            if Current_Command (J .. J + Gnatchop_Str'Length - 1) =
                                                                  Gnatchop_Str
            then
               End_Of_Prefix := J - 1;
               exit;
            end if;
         end loop;
      end if;

      if End_Of_Prefix > Current_Command'First then
         Start_Of_Suffix := End_Of_Prefix + Gnatchop_Str'Length + 1;
      end if;

      declare
         Command : constant String :=
                     Current_Command (Start_Of_Prefix .. End_Of_Prefix)
                       & Program_Name
                       & Current_Command (Start_Of_Suffix ..
                                          Current_Command'Last);
      begin
         Result := Locate_Exec_On_Path (Command);

         if Result = null then
            Error_Msg
              (Command & ": installation problem, executable not found");
         end if;
      end;

      return Result;
   end Locate_Executable;

   ---------------
   -- Parse_EOL --
   ---------------

   procedure Parse_EOL
     (Source : not null access String;
      Ptr    : in out Positive) is
   begin
      --  Skip to end of line

      while Source (Ptr) /= ASCII.CR and then Source (Ptr) /= ASCII.LF
        and then Source (Ptr) /= EOF
      loop
         Ptr := Ptr + 1;
      end loop;

      if Source (Ptr) /= EOF then
         Ptr := Ptr + 1;      -- skip CR or LF
      end if;

      --  Skip past CR/LF or LF/CR combination

      if (Source (Ptr) = ASCII.CR or else Source (Ptr) = ASCII.LF)
         and then Source (Ptr) /= Source (Ptr - 1)
      then
         Ptr := Ptr + 1;
      end if;
   end Parse_EOL;

   ----------------
   -- Parse_File --
   ----------------

   function Parse_File (Num : File_Num) return Boolean is
      Chop_Name   : constant String_Access   := File.Table (Num).Name;
      Save_Stdout : constant File_Descriptor := dup (Standout);
      Offset_Name : Temp_File_Name;
      Offset_FD   : File_Descriptor := Invalid_FD;
      Buffer      : String_Access;
      Success     : Boolean;
      Failure     : exception;

   begin
      --  Display copy of GNAT command if verbose mode

      if Verbose_Mode then
         Put (Gnat_Cmd.all);

         for J in 1 .. Gnat_Args'Length loop
            Put (' ');
            Put (Gnat_Args (J).all);
         end loop;

         Put (' ');
         Put_Line (Chop_Name.all);
      end if;

      --  Create temporary file

      Create_Temp_File (Offset_FD, Offset_Name);

      if Offset_FD = Invalid_FD then
         Error_Msg ("gnatchop: cannot create temporary file");
         Close (Save_Stdout);
         return False;
      end if;

      --  Redirect Stdout to this temporary file in the Unix way

      if dup2 (Offset_FD, Standout) = Invalid_FD then
         Error_Msg ("gnatchop: cannot redirect stdout to temporary file");
         Close (Save_Stdout);
         Close (Offset_FD);
         return False;
      end if;

      --  Call Gnat on the source filename argument with special options
      --  to generate offset information. If this special compilation completes
      --  successfully then we can do the actual gnatchop operation.

      Spawn (Gnat_Cmd.all, Gnat_Args.all & Chop_Name, Success);

      if not Success then
         Error_Msg (Chop_Name.all & ": parse errors detected");
         Error_Msg (Chop_Name.all & ": chop may not be successful");
      end if;

      --  Restore stdout

      if dup2 (Save_Stdout, Standout) = Invalid_FD then
         Error_Msg ("gnatchop: cannot restore stdout");
      end if;

      --  Reopen the file to start reading from the beginning

      Close (Offset_FD);
      Close (Save_Stdout);
      Offset_FD := Open_Read (Offset_Name'Address, Binary);

      if Offset_FD = Invalid_FD then
         Error_Msg ("gnatchop: cannot access offset info");
         raise Failure;
      end if;

      Read_File (Offset_FD, Buffer, Success);

      if not Success then
         Error_Msg ("gnatchop: error reading offset info");
         Close (Offset_FD);
         raise Failure;
      else
         Parse_Offset_Info (Num, Buffer);
      end if;

      --  Close and delete temporary file

      Close (Offset_FD);
      Delete_File (Offset_Name'Address, Success);

      return Success;

   exception
      when Failure | Types.Terminate_Program =>
         if Offset_FD /= Invalid_FD then
            Close (Offset_FD);
         end if;

         Delete_File (Offset_Name'Address, Success);
         return False;
   end Parse_File;

   -----------------------
   -- Parse_Offset_Info --
   -----------------------

   procedure Parse_Offset_Info
     (Chop_File : File_Num;
      Source    : not null access String)
   is
      First_Unit : constant Unit_Num := Unit.Last + 1;
      Bufferg    : String_Access     := null;
      Parse_Ptr  : File_Offset       := Source'First;
      Token_Ptr  : File_Offset;
      Info       : Unit_Info;

      function Match (Literal : String) return Boolean;
      --  Checks if given string appears at the current Token_Ptr location
      --  and if so, bumps Parse_Ptr past the token and returns True. If
      --  the string is not present, sets Parse_Ptr to Token_Ptr and
      --  returns False.

      -----------
      -- Match --
      -----------

      function Match (Literal : String) return Boolean is
      begin
         Parse_Token (Source, Parse_Ptr, Token_Ptr);

         if Source'Last  + 1 - Token_Ptr < Literal'Length
           or else
             Source (Token_Ptr .. Token_Ptr + Literal'Length - 1) /= Literal
         then
            Parse_Ptr := Token_Ptr;
            return False;
         end if;

         Parse_Ptr := Token_Ptr + Literal'Length;
         return True;
      end Match;

   --  Start of processing for Parse_Offset_Info

   begin
      loop
         --  Set default values, should get changed for all
         --  units/pragmas except for the last

         Info.Chop_File := Chop_File;
         Info.Length := 0;

         --  Parse the current line of offset information into Info
         --  and exit the loop if there are any errors or on EOF.

         --  First case, parse a line in the following format:

         --  Unit x (spec) line 7, file offset 142, [SR, ]file name x.ads

         --  Note that the unit name can be an operator name in quotes.
         --  This is of course illegal, but both GNAT and gnatchop handle
         --  the case so that this error does not interfere with chopping.

         --  The SR ir present indicates that a source reference pragma
         --  was processed as part of this unit (and that therefore no
         --  Source_Reference pragma should be generated.

         if Match ("Unit") then
            Parse_Token (Source, Parse_Ptr, Token_Ptr);

            if Match ("(body)") then
               Info.Kind := Unit_Body;
            elsif Match ("(spec)") then
               Info.Kind := Unit_Spec;
            else
               exit;
            end if;

            exit when not Match ("line");
            Parse_Token (Source, Parse_Ptr, Token_Ptr);
            Info.Start_Line := Line_Num'Value
              (Source (Token_Ptr .. Parse_Ptr - 1));

            exit when not Match ("file offset");
            Parse_Token (Source, Parse_Ptr, Token_Ptr);
            Info.Offset := File_Offset'Value
              (Source (Token_Ptr .. Parse_Ptr - 1));

            Info.SR_Present := Match ("SR, ");

            exit when not Match ("file name");
            Parse_Token (Source, Parse_Ptr, Token_Ptr);
            Info.File_Name := new String'
              (Directory.all & Source (Token_Ptr .. Parse_Ptr - 1));
            Parse_EOL (Source, Parse_Ptr);

         --  Second case, parse a line of the following form

         --  Configuration pragmas at line 10, file offset 223

         elsif Match ("Configuration pragmas at") then
            Info.Kind := Config_Pragmas;
            Info.File_Name := Config_File_Name;

            exit when not Match ("line");
            Parse_Token (Source, Parse_Ptr, Token_Ptr);
            Info.Start_Line := Line_Num'Value
              (Source (Token_Ptr .. Parse_Ptr - 1));

            exit when not Match ("file offset");
            Parse_Token (Source, Parse_Ptr, Token_Ptr);
            Info.Offset := File_Offset'Value
              (Source (Token_Ptr .. Parse_Ptr - 1));

            Parse_EOL (Source, Parse_Ptr);

         --  Third case, parse a line of the following form

         --    Source_Reference pragma for file "filename"

         --  This appears at the start of the file only, and indicates
         --  the name to be used on any generated Source_Reference pragmas.

         elsif Match ("Source_Reference pragma for file ") then
            Parse_Token (Source, Parse_Ptr, Token_Ptr);
            File.Table (Chop_File).SR_Name :=
              new String'(Source (Token_Ptr + 1 .. Parse_Ptr - 2));
            Parse_EOL (Source, Parse_Ptr);
            goto Continue;

         --  Unrecognized keyword or end of file

         else
            exit;
         end if;

         --  Store the data in the Info record in the Unit.Table

         Unit.Increment_Last;
         Unit.Table (Unit.Last) := Info;

         --  If this is not the first unit from the file, calculate
         --  the length of the previous unit as difference of the offsets

         if Unit.Last > First_Unit then
            Unit.Table (Unit.Last - 1).Length :=
              Info.Offset - Unit.Table (Unit.Last - 1).Offset;
         end if;

         --  If not in compilation mode combine current unit with any
         --  preceding configuration pragmas.

         if not Compilation_Mode
           and then Unit.Last > First_Unit
           and then Unit.Table (Unit.Last - 1).Kind = Config_Pragmas
         then
            Info.Start_Line := Unit.Table (Unit.Last - 1).Start_Line;
            Info.Offset := Unit.Table (Unit.Last - 1).Offset;

            --  Delete the configuration pragma entry

            Unit.Table (Unit.Last - 1) := Info;
            Unit.Decrement_Last;
         end if;

         --  If in compilation mode, and previous entry is the initial
         --  entry for the file and is for configuration pragmas, then
         --  they are to be appended to every unit in the file.

         if Compilation_Mode
           and then Unit.Last = First_Unit + 1
           and then Unit.Table (First_Unit).Kind = Config_Pragmas
         then
            Bufferg :=
              Get_Config_Pragmas
                (Unit.Table (Unit.Last - 1).Chop_File, First_Unit);
            Unit.Table (Unit.Last - 1) := Info;
            Unit.Decrement_Last;
         end if;

         Unit.Table (Unit.Last).Bufferg := Bufferg;

         --  If in compilation mode, and this is not the first item,
         --  combine configuration pragmas with previous unit, which
         --  will cause an error message to be generated when the unit
         --  is compiled.

         if Compilation_Mode
           and then Unit.Last > First_Unit
           and then Unit.Table (Unit.Last).Kind = Config_Pragmas
         then
            Unit.Decrement_Last;
         end if;

      <<Continue>>
         null;

      end loop;

      --  Find out if the loop was exited prematurely because of
      --  an error or if the EOF marker was found.

      if Source (Parse_Ptr) /= EOF then
         Error_Msg
           (File.Table (Chop_File).Name.all & ": error parsing offset info");
         return;
      end if;

      --  Handle case of a chop file consisting only of config pragmas

      if Unit.Last = First_Unit
        and then Unit.Table (Unit.Last).Kind = Config_Pragmas
      then
         --  In compilation mode, we append such a file to gnat.adc

         if Compilation_Mode then
            Write_Config_File (Unit.Table (Unit.Last).Chop_File, First_Unit);
            Unit.Decrement_Last;

         --  In default (non-compilation) mode, this is invalid

         else
            Error_Msg
              (File.Table (Chop_File).Name.all &
               ": no units found (only pragmas)");
            Unit.Decrement_Last;
         end if;
      end if;

      --  Handle case of a chop file ending with config pragmas. This can
      --  happen only in default non-compilation mode, since in compilation
      --  mode such configuration pragmas are part of the preceding unit.
      --  We simply concatenate such pragmas to the previous file which
      --  will cause a compilation error, which is appropriate.

      if Unit.Last > First_Unit
        and then Unit.Table (Unit.Last).Kind = Config_Pragmas
      then
         Unit.Decrement_Last;
      end if;
   end Parse_Offset_Info;

   -----------------
   -- Parse_Token --
   -----------------

   procedure Parse_Token
     (Source    : not null access String;
      Ptr       : in out Positive;
      Token_Ptr : out Positive)
   is
      In_Quotes : Boolean := False;

   begin
      --  Skip separators

      while Source (Ptr) = ' ' or else Source (Ptr) = ',' loop
         Ptr := Ptr + 1;
      end loop;

      Token_Ptr := Ptr;

      --  Find end-of-token

      while (In_Quotes
              or else not (Source (Ptr) = ' ' or else Source (Ptr) = ','))
        and then Source (Ptr) >= ' '
      loop
         if Source (Ptr) = '"' then
            In_Quotes := not In_Quotes;
         end if;

         Ptr := Ptr + 1;
      end loop;
   end Parse_Token;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (FD       : File_Descriptor;
      Contents : out String_Access;
      Success  : out Boolean)
   is
      Length      : constant File_Offset := File_Offset (File_Length (FD));
      --  Include room for EOF char
      Buffer      : String_Access := new String (1 .. Length + 1);

      This_Read   : Integer;
      Read_Ptr    : File_Offset := 1;

   begin

      loop
         This_Read := Read (FD,
           A => Buffer (Read_Ptr)'Address,
           N => Length + 1 - Read_Ptr);
         Read_Ptr := Read_Ptr + Integer'Max (This_Read, 0);
         exit when This_Read <= 0;
      end loop;

      Buffer (Read_Ptr) := EOF;

      --  Comment needed for the following ???
      --  Under what circumstances can the test fail ???
      --  What is copy doing in that case???

      if Read_Ptr = Length then
         Contents := Buffer;

      else
         Contents := new String (1 .. Read_Ptr);
         Contents.all := Buffer (1 .. Read_Ptr);
         Free (Buffer);
      end if;

      Success := Read_Ptr = Length + 1;
   end Read_File;

   ----------------------------
   -- Report_Duplicate_Units --
   ----------------------------

   function Report_Duplicate_Units return Boolean is
      US : SUnit_Num;
      U  : Unit_Num;

      Duplicates : Boolean  := False;

   begin
      US := 1;
      while US < SUnit_Num (Unit.Last) loop
         U := Sorted_Units.Table (US);

         if Is_Duplicated (US) then
            Duplicates := True;

            --  Move to last two versions of duplicated file to make it clearer
            --  to understand which file is retained in case of overwriting.

            while US + 1 < SUnit_Num (Unit.Last) loop
               exit when not Is_Duplicated (US + 1);
               US := US + 1;
            end loop;

            U := Sorted_Units.Table (US);

            if Overwrite_Files then
               Warning_Msg (Unit.Table (U).File_Name.all
                 & " is duplicated (all but last will be skipped)");

            elsif Unit.Table (U).Chop_File =
                    Unit.Table (Sorted_Units.Table (US + 1)).Chop_File
            then
               Error_Msg (Unit.Table (U).File_Name.all
                 & " is duplicated in "
                 & File.Table (Unit.Table (U).Chop_File).Name.all);

            else
               Error_Msg (Unit.Table (U).File_Name.all
                  & " in "
                  & File.Table (Unit.Table (U).Chop_File).Name.all
                  & " is duplicated in "
                  & File.Table
                      (Unit.Table
                        (Sorted_Units.Table (US + 1)).Chop_File).Name.all);
            end if;
         end if;

         US := US + 1;
      end loop;

      if Duplicates and not Overwrite_Files then
         Put_Line ("use -w to overwrite files and keep last version");
      end if;

      return Duplicates;
   end Report_Duplicate_Units;

   --------------------
   -- Scan_Arguments --
   --------------------

   function Scan_Arguments return Boolean is
      Kset : Boolean := False;
      --  Set true if -k switch found

   begin
      Initialize_Option_Scan;

      --  Scan options first

      loop
         case Getopt ("c gnat? h k? p q r v w x -GCC=!") is
            when ASCII.NUL =>
               exit;

            when '-' =>
               Gcc     := new String'(Parameter);
               Gcc_Set := True;

            when 'c' =>
               Compilation_Mode := True;

            when 'g' =>
               Gnat_Args :=
                 new Argument_List'(Gnat_Args.all &
                                      new String'("-gnat" & Parameter));

            when 'h' =>
               Usage;
               raise Types.Terminate_Program;

            when 'k' =>
               declare
                  Param : String_Access := new String'(Parameter);

               begin
                  if Param.all /= "" then
                     for J in Param'Range loop
                        if Param (J) not in '0' .. '9' then
                           Error_Msg ("-k# requires numeric parameter");
                           return False;
                        end if;
                     end loop;

                  else
                     Param := new String'("8");
                  end if;

                  Gnat_Args :=
                    new Argument_List'(Gnat_Args.all &
                                         new String'("-gnatk" & Param.all));
                  Kset := True;
               end;

            when 'p' =>
               Preserve_Mode := True;

            when 'q' =>
               Quiet_Mode := True;

            when 'r' =>
               Source_References := True;

            when 'v' =>
               Verbose_Mode := True;
               Display_Version ("GNATCHOP", "1998");

            when 'w' =>
               Overwrite_Files := True;

            when 'x' =>
               Exit_On_Error := True;

            when others =>
               null;
         end case;
      end loop;

      if not Kset and then Maximum_File_Name_Length > 0 then

         --  If this system has restricted filename lengths, tell gnat1
         --  about them, removing the leading blank from the image string.

         Gnat_Args :=
           new Argument_List'(Gnat_Args.all
             & new String'("-gnatk"
               & Maximum_File_Name_Length_String
                 (Maximum_File_Name_Length_String'First + 1
                  .. Maximum_File_Name_Length_String'Last)));
      end if;

      --  Scan file names

      loop
         declare
            S : constant String := Get_Argument (Do_Expansion => True);

         begin
            exit when S = "";
            File.Increment_Last;
            File.Table (File.Last).Name    := new String'(S);
            File.Table (File.Last).SR_Name := null;
         end;
      end loop;

      --  Case of more than one file where last file is a directory

      if File.Last > 1
        and then Is_Directory (File.Table (File.Last).Name.all)
      then
         Directory := File.Table (File.Last).Name;
         File.Decrement_Last;

         --  Make sure Directory is terminated with a directory separator,
         --  so we can generate the output by just appending a filename.

         if Directory (Directory'Last) /= Directory_Separator
            and then Directory (Directory'Last) /= '/'
         then
            Directory := new String'(Directory.all & Directory_Separator);
         end if;

      --  At least one filename must be given

      elsif File.Last = 0 then
         if Argument_Count = 0 then
            Usage;
         else
            Try_Help;
         end if;

         return False;

      --  No directory given, set directory to null, so that we can just
      --  concatenate the directory name to the file name unconditionally.

      else
         Directory := new String'("");
      end if;

      --  Finally check all filename arguments

      for File_Num in 1 .. File.Last loop
         declare
            F : constant String := File.Table (File_Num).Name.all;

         begin
            if Is_Directory (F) then
               Error_Msg (F & " is a directory, cannot be chopped");
               return False;

            elsif not Is_Regular_File (F) then
               Error_Msg (F & " not found");
               return False;
            end if;
         end;
      end loop;

      return True;

   exception
      when Invalid_Switch =>
         Error_Msg ("invalid switch " & Full_Switch);
         return False;

      when Invalid_Parameter =>
         Error_Msg ("-k switch requires numeric parameter");
         return False;
   end Scan_Arguments;

   ----------------
   -- Sort_Units --
   ----------------

   procedure Sort_Units is

      procedure Move (From : Natural; To : Natural);
      --  Procedure used to sort the unit list
      --  Unit.Table (To) := Unit_List (From); used by sort

      function Lt (Left, Right : Natural) return Boolean;
      --  Compares Left and Right units based on file name (first),
      --  Chop_File (second) and Offset (third). This ordering is
      --  important to keep the last version in case of duplicate files.

      package Unit_Sort is new GNAT.Heap_Sort_G (Move, Lt);
      --  Used for sorting on filename to detect duplicates

      --------
      -- Lt --
      --------

      function Lt (Left, Right : Natural) return Boolean is
         L : Unit_Info renames
               Unit.Table (Sorted_Units.Table (SUnit_Num (Left)));

         R : Unit_Info renames
               Unit.Table (Sorted_Units.Table (SUnit_Num (Right)));

      begin
         return L.File_Name.all < R.File_Name.all
           or else (L.File_Name.all = R.File_Name.all
                     and then (L.Chop_File < R.Chop_File
                                 or else (L.Chop_File = R.Chop_File
                                            and then L.Offset < R.Offset)));
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         Sorted_Units.Table (SUnit_Num (To)) :=
           Sorted_Units.Table (SUnit_Num (From));
      end Move;

   --  Start of processing for Sort_Units

   begin
      Sorted_Units.Set_Last (SUnit_Num (Unit.Last));

      for J in 1 .. Unit.Last loop
         Sorted_Units.Table (SUnit_Num (J)) := J;
      end loop;

      --  Sort Unit.Table, using Sorted_Units.Table (0) as scratch

      Unit_Sort.Sort (Natural (Unit.Last));

      --  Set the Sorted_Index fields in the unit tables

      for J in 1 .. SUnit_Num (Unit.Last) loop
         Unit.Table (Sorted_Units.Table (J)).Sorted_Index := J;
      end loop;
   end Sort_Units;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line
        ("Usage: gnatchop [-c] [-h] [-k#] " &
         "[-r] [-p] [-q] [-v] [-w] [-x] [--GCC=xx] file [file ...] [dir]");

      New_Line;

      Display_Usage_Version_And_Help;

      Put_Line
        ("  -c       compilation mode, configuration pragmas " &
         "follow RM rules");

      Put_Line
        ("  -gnatxxx passes the -gnatxxx switch to gnat parser");

      Put_Line
        ("  -h       help: output this usage information");

      Put_Line
        ("  -k#      krunch file names of generated files to " &
         "no more than # characters");

      Put_Line
        ("  -k       krunch file names of generated files to " &
         "no more than 8 characters");

      Put_Line
        ("  -p       preserve time stamp, output files will " &
         "have same stamp as input");

      Put_Line
        ("  -q       quiet mode, no output of generated file " &
         "names");

      Put_Line
        ("  -r       generate Source_Reference pragmas refer" &
         "encing original source file");

      Put_Line
        ("  -v       verbose mode, output version and generat" &
         "ed commands");

      Put_Line
        ("  -w       overwrite existing filenames");

      Put_Line
        ("  -x       exit on error");

      Put_Line
        ("  --GCC=xx specify the path of the gnat parser to be used");

      New_Line;
      Put_Line
        ("  file...  list of source files to be chopped");

      Put_Line
        ("  dir      directory location for split files (defa" &
         "ult = current directory)");
   end Usage;

   -----------------
   -- Warning_Msg --
   -----------------

   procedure Warning_Msg (Message : String) is
   begin
      Warning_Count := Warning_Count + 1;
      Put_Line (Standard_Error, "warning: " & Message);
   end Warning_Msg;

   -------------------------
   -- Write_Chopped_Files --
   -------------------------

   function Write_Chopped_Files (Input : File_Num) return Boolean is
      Name    : aliased constant String :=
                  File.Table (Input).Name.all & ASCII.NUL;
      FD      : File_Descriptor;
      Buffer  : String_Access;
      Success : Boolean;
      TS_Time : OS_Time;

      BOM_Present : Boolean;
      BOM         : BOM_Kind;
      --  Record presence of UTF8 BOM in input

   begin
      FD := Open_Read (Name'Address, Binary);
      TS_Time := File_Time_Stamp (FD);

      if FD = Invalid_FD then
         Error_Msg ("cannot open " & File.Table (Input).Name.all);
         return False;
      end if;

      Read_File (FD, Buffer, Success);

      if not Success then
         Error_Msg ("cannot read " & File.Table (Input).Name.all);
         Close (FD);
         return False;
      end if;

      if not Quiet_Mode then
         Put_Line ("splitting " & File.Table (Input).Name.all & " into:");
      end if;

      --  Test for presence of BOM

      Read_BOM (Buffer.all, BOM_Length, BOM, XML_Support => False);
      BOM_Present := BOM /= Unknown;

      --  Only chop those units that come from this file

      for Unit_Number in 1 .. Unit.Last loop
         if Unit.Table (Unit_Number).Chop_File = Input then
            Write_Unit
              (Source    => Buffer,
               Num       => Unit_Number,
               TS_Time   => TS_Time,
               Write_BOM => BOM_Present and then Unit_Number /= 1,
               Success   => Success);
            exit when not Success;
         end if;
      end loop;

      Close (FD);
      return Success;
   end Write_Chopped_Files;

   -----------------------
   -- Write_Config_File --
   -----------------------

   procedure Write_Config_File (Input : File_Num; U : Unit_Num) is
      FD      : File_Descriptor;
      Name    : aliased constant String := "gnat.adc" & ASCII.NUL;
      Buffer  : String_Access;
      Success : Boolean;
      Append  : Boolean;
      Buffera : String_Access;
      Bufferl : Natural;

   begin
      Write_gnat_adc := True;
      FD := Open_Read_Write (Name'Address, Binary);

      if FD = Invalid_FD then
         FD := Create_File (Name'Address, Binary);
         Append := False;

         if not Quiet_Mode then
            Put_Line ("writing configuration pragmas from " &
               File.Table (Input).Name.all & " to gnat.adc");
         end if;

      else
         Append := True;

         if not Quiet_Mode then
            Put_Line
              ("appending configuration pragmas from " &
               File.Table (Input).Name.all & " to gnat.adc");
         end if;
      end if;

      Success := FD /= Invalid_FD;

      if not Success then
         Error_Msg ("cannot create gnat.adc");
         return;
      end if;

      --  In append mode, acquire existing gnat.adc file

      if Append then
         Read_File (FD, Buffera, Success);

         if not Success then
            Error_Msg ("cannot read gnat.adc");
            return;
         end if;

         --  Find location of EOF byte if any to exclude from append

         Bufferl := 1;
         while Bufferl <= Buffera'Last
           and then Buffera (Bufferl) /= EOF
         loop
            Bufferl := Bufferl + 1;
         end loop;

         Bufferl := Bufferl - 1;
         Close (FD);

         --  Write existing gnat.adc to new gnat.adc file

         FD := Create_File (Name'Address, Binary);
         Success := Write (FD, Buffera (1)'Address, Bufferl) = Bufferl;

         if not Success then
            Error_Msg ("error writing gnat.adc");
            return;
         end if;
      end if;

      Buffer := Get_Config_Pragmas  (Input, U);

      if Buffer /= null then
         Success := Write (FD, Buffer.all'Address, Buffer'Length) =
                                 Buffer'Length;

         if not Success then
            Error_Msg ("disk full writing gnat.adc");
            return;
         end if;
      end if;

      Close (FD);
   end Write_Config_File;

   -----------------------------------
   -- Write_Source_Reference_Pragma --
   -----------------------------------

   procedure Write_Source_Reference_Pragma
     (Info    : Unit_Info;
      Line    : Line_Num;
      File    : Stream_IO.File_Type;
      EOL     : EOL_String;
      Success : in out Boolean)
   is
      FTE : File_Entry renames Gnatchop.File.Table (Info.Chop_File);
      Nam : String_Access;

   begin
      if Success and then Source_References and then not Info.SR_Present then
         if FTE.SR_Name /= null then
            Nam := FTE.SR_Name;
         else
            Nam := FTE.Name;
         end if;

         declare
            Reference : String :=
                          "pragma Source_Reference (000000, """
                            & Nam.all & """);" & EOL.Str;

            Pos : Positive := Reference'First;
            Lin : Line_Num := Line;

         begin
            while Reference (Pos + 1) /= ',' loop
               Pos := Pos + 1;
            end loop;

            while Reference (Pos) = '0' loop
               Reference (Pos) := Character'Val
                 (Character'Pos ('0') + Lin mod 10);
               Lin := Lin / 10;
               Pos := Pos - 1;
            end loop;

            --  Assume there are enough zeroes for any program length

            pragma Assert (Lin = 0);

            begin
               String'Write (Stream_IO.Stream (File), Reference);
               Success := True;
            exception
               when others =>
                  Success := False;
            end;
         end;
      end if;
   end Write_Source_Reference_Pragma;

   ----------------
   -- Write_Unit --
   ----------------

   procedure Write_Unit
     (Source    : not null access String;
      Num       : Unit_Num;
      TS_Time   : OS_Time;
      Write_BOM : Boolean;
      Success   : out Boolean)
   is

      procedure OS_Filename
        (Name     : String;
         W_Name   : Wide_String;
         OS_Name  : Address;
         N_Length : access Natural;
         Encoding : Address;
         E_Length : access Natural);
      pragma Import (C, OS_Filename, "__gnat_os_filename");
      --  Returns in OS_Name the proper name for the OS when used with the
      --  returned Encoding value. For example on Windows this will return the
      --  UTF-8 encoded name into OS_Name and set Encoding to encoding=utf8
      --  (the form parameter for Stream_IO).
      --
      --  Name is the filename and W_Name the same filename in Unicode 16 bits
      --  (this corresponds to Win32 Unicode ISO/IEC 10646). N_Length/E_Length
      --  are the length returned in OS_Name/Encoding respectively.

      Info     : Unit_Info renames Unit.Table (Num);
      Name     : aliased constant String := Info.File_Name.all & ASCII.NUL;
      W_Name   : aliased constant Wide_String := To_Wide_String (Name);
      EOL      : constant EOL_String :=
                   Get_EOL (Source, Source'First + Info.Offset);
      OS_Name  : aliased String (1 .. Name'Length * 2);
      O_Length : aliased Natural := OS_Name'Length;
      Encoding : aliased String (1 .. 64);
      E_Length : aliased Natural := Encoding'Length;
      Length   : File_Offset;

   begin
      --  Skip duplicated files

      if Is_Duplicated (Info.Sorted_Index) then
         Put_Line ("   " & Info.File_Name.all & " skipped");
         Success := Overwrite_Files;
         return;
      end if;

      --  Get OS filename

      OS_Filename
        (Name, W_Name,
         OS_Name'Address, O_Length'Access,
         Encoding'Address, E_Length'Access);

      declare
         E_Name      : constant String := OS_Name (1 .. O_Length);
         OS_Encoding : constant String := Encoding (1 .. E_Length);
         File        : Stream_IO.File_Type;

      begin
         begin
            if not Overwrite_Files and then Exists (E_Name) then
               raise Stream_IO.Name_Error;
            else
               Stream_IO.Create
                 (File, Stream_IO.Out_File, E_Name, OS_Encoding);
               Success := True;
            end if;

         exception
            when Stream_IO.Name_Error | Stream_IO.Use_Error =>
               Error_Msg ("cannot create " & Info.File_Name.all);
               return;
         end;

         --  A length of 0 indicates that the rest of the file belongs to
         --  this unit. The actual length must be calculated now. Take into
         --  account that the last character (EOF) must not be written.

         if Info.Length = 0 then
            Length := Source'Last - (Source'First + Info.Offset);
         else
            Length := Info.Length;
         end if;

         --  Write BOM if required

         if Write_BOM then
            String'Write
              (Stream_IO.Stream (File),
               Source.all (Source'First .. Source'First + BOM_Length - 1));
         end if;

         --  Prepend configuration pragmas if necessary

         if Success and then Info.Bufferg /= null then
            Write_Source_Reference_Pragma (Info, 1, File, EOL, Success);
            String'Write (Stream_IO.Stream (File), Info.Bufferg.all);
         end if;

         Write_Source_Reference_Pragma
           (Info, Info.Start_Line, File, EOL, Success);

         if Success then
            begin
               String'Write
                 (Stream_IO.Stream (File),
                  Source (Source'First + Info.Offset ..
                      Source'First + Info.Offset + Length - 1));
            exception
               when Stream_IO.Use_Error | Stream_IO.Device_Error =>
                  Error_Msg ("disk full writing " & Info.File_Name.all);
                  return;
            end;
         end if;

         if not Quiet_Mode then
            Put_Line ("   " & Info.File_Name.all);
         end if;

         Stream_IO.Close (File);

         if Preserve_Mode then
            Set_File_Last_Modify_Time_Stamp (E_Name, TS_Time);
         end if;
      end;
   end Write_Unit;

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

--  Start of processing for gnatchop

begin
   --  Add the directory where gnatchop is invoked in front of the path, if
   --  gnatchop is invoked with directory information.

   declare
      Command : constant String := Command_Name;

   begin
      for Index in reverse Command'Range loop
         if Command (Index) = Directory_Separator then
            declare
               Absolute_Dir : constant String :=
                                Normalize_Pathname
                                  (Command (Command'First .. Index));
               PATH         : constant String :=
                                Absolute_Dir
                                & Path_Separator
                                & Getenv ("PATH").all;
            begin
               Setenv ("PATH", PATH);
            end;

            exit;
         end if;
      end loop;
   end;

   --  Process command line options and initialize global variables

   --  First, scan to detect --version and/or --help

   Check_Version_And_Help ("GNATCHOP", "1998");

   if not Scan_Arguments then
      Set_Exit_Status (Failure);
      return;
   end if;

   --  Check presence of required executables

   Gnat_Cmd := Locate_Executable (Gcc.all, not Gcc_Set);

   if Gnat_Cmd = null then
      goto No_Files_Written;
   end if;

   --  First parse all files and read offset information

   for Num in 1 .. File.Last loop
      if not Parse_File (Num) then
         goto No_Files_Written;
      end if;
   end loop;

   --  Check if any units have been found (assumes non-empty Unit.Table)

   if Unit.Last = 0 then
      if not Write_gnat_adc then
         Error_Msg ("no compilation units found", Warning => True);
      end if;

      goto No_Files_Written;
   end if;

   Sort_Units;

   --  Check if any duplicate files would be created. If so, emit a warning if
   --  Overwrite_Files is true, otherwise generate an error.

   if Report_Duplicate_Units and then not Overwrite_Files then
      goto No_Files_Written;
   end if;

   --  Check if any files exist, if so do not write anything Because all files
   --  have been parsed and checked already, there won't be any duplicates

   if not Overwrite_Files and then Files_Exist then
      goto No_Files_Written;
   end if;

   --  After this point, all source files are read in succession and chopped
   --  into their destination files.

   --  Source_File_Name pragmas are handled as logical file 0 so write it first

   for F in 1 .. File.Last loop
      if not Write_Chopped_Files (F) then
         Set_Exit_Status (Failure);
         return;
      end if;
   end loop;

   if Warning_Count > 0 then
      declare
         Warnings_Msg : constant String := Warning_Count'Img & " warning(s)";
      begin
         Error_Msg (Warnings_Msg (2 .. Warnings_Msg'Last), Warning => True);
      end;
   end if;

   return;

<<No_Files_Written>>

   --  Special error exit for all situations where no files have
   --  been written.

   if not Write_gnat_adc then
      Error_Msg ("no source files written", Warning => True);
   end if;

   return;

exception
   when Types.Terminate_Program =>
      null;

end Gnatchop;
