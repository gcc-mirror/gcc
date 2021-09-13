------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G P R E P                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2021, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Csets;
with Errutil;
with Namet;    use Namet;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prep;     use Prep;
with Scng;
with Sinput.C;
with Snames;
with Stringt;  use Stringt;
with Switch;   use Switch;
with Types;    use Types;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with System.OS_Lib; use System.OS_Lib;

package body GPrep is

   Copyright_Displayed : Boolean := False;
   --  Used to prevent multiple displays of the copyright notice

   ------------------------
   -- Argument Line Data --
   ------------------------

   Unix_Line_Terminators : Boolean := False;
   --  Set to True with option -T

   type String_Array is array (Boolean) of String_Access;
   Yes_No : constant String_Array :=
     (False => new String'("YES"),
      True  => new String'("NO"));

   Infile_Name  : Name_Id := No_Name;
   Outfile_Name : Name_Id := No_Name;
   Deffile_Name : Name_Id := No_Name;

   Output_Directory : Name_Id := No_Name;
   --  Used when the specified output is an existing directory

   Input_Directory : Name_Id := No_Name;
   --  Used when the specified input and output are existing directories

   Source_Ref_Pragma : Boolean := False;
   --  Record command line options (set if -r switch set)

   Text_Outfile : aliased Ada.Text_IO.File_Type;
   Outfile      : constant File_Access := Text_Outfile'Access;

   File_Name_Buffer_Initial_Size : constant := 50;
   File_Name_Buffer : String_Access :=
                        new String (1 .. File_Name_Buffer_Initial_Size);
   --  A buffer to build output file names from input file names

   -----------------
   -- Subprograms --
   -----------------

   procedure Display_Copyright;
   --  Display the copyright notice

   procedure Post_Scan;
   --  Null procedure, needed by instantiation of Scng below

   package Scanner is new Scng
     (Post_Scan,
      Errutil.Error_Msg,
      Errutil.Error_Msg_S,
      Errutil.Error_Msg_SC,
      Errutil.Error_Msg_SP,
      Errutil.Style);
   --  The scanner for the preprocessor

   function Is_ASCII_Letter (C : Character) return Boolean;
   --  True if C is in 'a' .. 'z' or in 'A' .. 'Z'

   procedure Double_File_Name_Buffer;
   --  Double the size of the file name buffer

   procedure Preprocess_Infile_Name;
   --  When the specified output is a directory, preprocess the infile name
   --  for symbol substitution, to get the output file name.

   procedure Process_Files;
   --  Process the single input file or all the files in the directory tree
   --  rooted at the input directory.

   procedure Process_Command_Line_Symbol_Definition (S : String);
   --  Process a -D switch on the command line

   procedure Put_Char_To_Outfile (C : Character);
   --  Output one character to the output file. Used to initialize the
   --  preprocessor.

   procedure New_EOL_To_Outfile;
   --  Output a new line to the output file. Used to initialize the
   --  preprocessor.

   procedure Scan_Command_Line;
   --  Scan the switches and the file names

   procedure Usage;
   --  Display the usage

   -----------------------
   -- Display_Copyright --
   -----------------------

   procedure Display_Copyright is
   begin
      if not Copyright_Displayed then
         Display_Version ("GNAT Preprocessor", "1996");
         Copyright_Displayed := True;
      end if;
   end Display_Copyright;

   -----------------------------
   -- Double_File_Name_Buffer --
   -----------------------------

   procedure Double_File_Name_Buffer is
      New_Buffer : constant String_Access :=
                     new String (1 .. 2 * File_Name_Buffer'Length);
   begin
      New_Buffer (File_Name_Buffer'Range) := File_Name_Buffer.all;
      Free (File_Name_Buffer);
      File_Name_Buffer := New_Buffer;
   end Double_File_Name_Buffer;

   --------------
   -- Gnatprep --
   --------------

   procedure Gnatprep is
   begin
      --  Do some initializations (order is important here)

      Csets.Initialize;
      Snames.Initialize;
      Stringt.Initialize;
      Prep.Initialize;

      --  Initialize the preprocessor

      Prep.Setup_Hooks
        (Error_Msg         => Errutil.Error_Msg'Access,
         Scan              => Scanner.Scan'Access,
         Set_Ignore_Errors => Errutil.Set_Ignore_Errors'Access,
         Put_Char          => Put_Char_To_Outfile'Access,
         New_EOL           => New_EOL_To_Outfile'Access);

      --  Set the scanner characteristics for the preprocessor

      Scanner.Set_Special_Character ('#');
      Scanner.Set_Special_Character ('$');
      Scanner.Set_End_Of_Line_As_Token (True);

      --  Initialize the mapping table of symbols to values

      Prep.Symbol_Table.Init (Prep.Mapping);

      --  Parse the switches and arguments

      Scan_Command_Line;

      if Opt.Verbose_Mode then
         Display_Copyright;
      end if;

      --  Test we had all the arguments needed

      if Infile_Name = No_Name then

         --  No input file specified, just output the usage and exit

         if Argument_Count = 0 then
            Usage;
         else
            GNAT.Command_Line.Try_Help;
         end if;

         return;

      elsif Outfile_Name = No_Name then

         --  No output file specified, exit

         GNAT.Command_Line.Try_Help;
         return;
      end if;

      --  If a pragma Source_File_Name, we need to keep line numbers. So, if
      --  the deleted lines are not put as comment, we must output them as
      --  blank lines.

      if Source_Ref_Pragma and (not Opt.Comment_Deleted_Lines) then
         Opt.Blank_Deleted_Lines := True;
      end if;

      --  If we have a definition file, parse it

      if Deffile_Name /= No_Name then
         declare
            Deffile : Source_File_Index;

         begin
            Errutil.Initialize;
            Deffile := Sinput.C.Load_File (Get_Name_String (Deffile_Name));

            --  Set Main_Source_File to the definition file for the benefit of
            --  Errutil.Finalize.

            Sinput.Main_Source_File := Deffile;

            if Deffile = No_Source_File then
               Fail ("unable to find definition file """
                     & Get_Name_String (Deffile_Name)
                     & """");
            elsif Deffile = No_Access_To_Source_File then
               Fail ("unabled to read definition file """
                     & Get_Name_String (Deffile_Name)
                     & """");
            end if;

            Scanner.Initialize_Scanner (Deffile);

            --  Parse the definition file without "replace in comments"

            declare
               Replace : constant Boolean := Opt.Replace_In_Comments;
            begin
               Opt.Replace_In_Comments := False;
               Prep.Parse_Def_File;
               Opt.Replace_In_Comments := Replace;
            end;
         end;
      end if;

      --  If there are errors in the definition file, output them and exit

      if Total_Errors_Detected > 0 then
         Errutil.Finalize (Source_Type => "definition");
         Fail ("errors in definition file """
               & Get_Name_String (Deffile_Name)
               & """");
      end if;

      --  If -s switch was specified, print a sorted list of symbol names and
      --  values, if any.

      if Opt.List_Preprocessing_Symbols then
         Prep.List_Symbols (Foreword => "");
      end if;

      Output_Directory := No_Name;
      Input_Directory  := No_Name;

      --  Check if the specified output is an existing directory

      if Is_Directory (Get_Name_String (Outfile_Name)) then
         Output_Directory := Outfile_Name;

         --  As the output is an existing directory, check if the input too
         --  is a directory.

         if Is_Directory (Get_Name_String (Infile_Name)) then
            Input_Directory := Infile_Name;
         end if;
      end if;

      --  And process the single input or the files in the directory tree
      --  rooted at the input directory.

      Process_Files;
   end Gnatprep;

   ---------------------
   -- Is_ASCII_Letter --
   ---------------------

   function Is_ASCII_Letter (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z' or else C in 'a' .. 'z';
   end Is_ASCII_Letter;

   ------------------------
   -- New_EOL_To_Outfile --
   ------------------------

   procedure New_EOL_To_Outfile is
   begin
      New_Line (Outfile.all);
   end New_EOL_To_Outfile;

   ---------------
   -- Post_Scan --
   ---------------

   procedure Post_Scan is
   begin
      null;
   end Post_Scan;

   ----------------------------
   -- Preprocess_Infile_Name --
   ----------------------------

   procedure Preprocess_Infile_Name is
      Len    : Natural;
      First  : Positive;
      Last   : Natural;
      Symbol : Name_Id;
      Data   : Symbol_Data;

   begin
      --  Initialize the buffer with the name of the input file

      Get_Name_String (Infile_Name);
      Len := Name_Len;

      while File_Name_Buffer'Length < Len loop
         Double_File_Name_Buffer;
      end loop;

      File_Name_Buffer (1 .. Len) := Name_Buffer (1 .. Len);

      --  Look for possible symbols in the file name

      First := 1;
      while First < Len loop

         --  A symbol starts with a dollar sign followed by a letter

         if File_Name_Buffer (First) = '$' and then
           Is_ASCII_Letter (File_Name_Buffer (First + 1))
         then
            Last := First + 1;

            --  Find the last letter of the symbol

            while Last < Len and then
               Is_ASCII_Letter (File_Name_Buffer (Last + 1))
            loop
               Last := Last + 1;
            end loop;

            --  Get the symbol name id

            Name_Len := Last - First;
            Name_Buffer (1 .. Name_Len) :=
              File_Name_Buffer (First + 1 .. Last);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Symbol := Name_Find;

            --  And look for this symbol name in the symbol table

            for Index in 1 .. Symbol_Table.Last (Mapping) loop
               Data := Mapping.Table (Index);

               if Data.Symbol = Symbol then

                  --  We found the symbol. If its value is not a string,
                  --  replace the symbol in the file name with the value of
                  --  the symbol.

                  if not Data.Is_A_String then
                     String_To_Name_Buffer (Data.Value);

                     declare
                        Sym_Len : constant Positive := Last - First + 1;
                        Offset  : constant Integer := Name_Len - Sym_Len;
                        New_Len : constant Natural := Len + Offset;

                     begin
                        while New_Len > File_Name_Buffer'Length loop
                           Double_File_Name_Buffer;
                        end loop;

                        File_Name_Buffer (Last + 1 + Offset .. New_Len) :=
                          File_Name_Buffer (Last + 1 .. Len);
                        Len := New_Len;
                        Last := Last + Offset;
                        File_Name_Buffer (First .. Last) :=
                          Name_Buffer (1 .. Name_Len);
                     end;
                  end if;

                  exit;
               end if;
            end loop;

            --  Skip over the symbol name or its value: we are not checking
            --  for another symbol name in the value.

            First := Last + 1;

         else
            First := First + 1;
         end if;
      end loop;

      --  We now have the output file name in the buffer. Get the output
      --  path and put it in Outfile_Name.

      Get_Name_String (Output_Directory);
      Add_Char_To_Name_Buffer (Directory_Separator);
      Add_Str_To_Name_Buffer (File_Name_Buffer (1 .. Len));
      Outfile_Name := Name_Find;
   end Preprocess_Infile_Name;

   --------------------------------------------
   -- Process_Command_Line_Symbol_Definition --
   --------------------------------------------

   procedure Process_Command_Line_Symbol_Definition (S : String) is
      Data   : Symbol_Data;
      Symbol : Symbol_Id;

   begin
      --  Check the symbol definition and get the symbol and its value.
      --  Fail if symbol definition is illegal.

      Check_Command_Line_Symbol_Definition (S, Data);

      Symbol := Index_Of (Data.Symbol);

      --  If symbol does not already exist, create a new entry in the mapping
      --  table.

      if Symbol = No_Symbol then
         Symbol_Table.Increment_Last (Mapping);
         Symbol := Symbol_Table.Last (Mapping);
      end if;

      Mapping.Table (Symbol) := Data;
   end Process_Command_Line_Symbol_Definition;

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files is

      procedure Process_One_File;
      --  Process input file Infile_Name and put the result in file
      --  Outfile_Name.

      procedure Recursive_Process (In_Dir : String; Out_Dir : String);
      --  Process recursively files in In_Dir. Results go to Out_Dir

      ----------------------
      -- Process_One_File --
      ----------------------

      procedure Process_One_File is
         Infile : Source_File_Index;

         Modified : Boolean;
         pragma Warnings (Off, Modified);

      begin
         --  Create the output file (fails if this does not work)

         begin
            Create
              (File => Text_Outfile,
               Mode => Out_File,
               Name => Get_Name_String (Outfile_Name),
               Form => "Text_Translation=" &
                       Yes_No (Unix_Line_Terminators).all);

         exception
            when others =>
               Fail
                 ("unable to create output file """
                  & Get_Name_String (Outfile_Name)
                  & """");
         end;

         --  Load the input file

         Infile := Sinput.C.Load_File (Get_Name_String (Infile_Name));

         if Infile = No_Source_File then
            Fail ("unable to find input file """
                  & Get_Name_String (Infile_Name)
                  & """");
         elsif Infile = No_Access_To_Source_File then
            Fail ("unable to read input file """
                  & Get_Name_String (Infile_Name)
                  & """");
         end if;

         --  Set Main_Source_File to the input file for the benefit of
         --  Errutil.Finalize.

         Sinput.Main_Source_File := Infile;

         Scanner.Initialize_Scanner (Infile);

         --  Output the pragma Source_Reference if asked to

         if Source_Ref_Pragma then
            Put_Line
              (Outfile.all,
               "pragma Source_Reference (1, """ &
                 Get_Name_String (Sinput.Full_File_Name (Infile)) & """);");
         end if;

         --  Preprocess the input file

         Prep.Preprocess (Modified);

         --  In verbose mode, if there is no error, report it

         if Opt.Verbose_Mode and then Total_Errors_Detected = 0 then
            Errutil.Finalize (Source_Type => "input");
         end if;

         --  If we had some errors, delete the output file, and report them

         if Total_Errors_Detected > 0 then
            if Outfile /= Standard_Output then
               Delete (Text_Outfile);
            end if;

            Errutil.Finalize (Source_Type => "input");

            OS_Exit (0);

         --  Otherwise, close the output file, and we are done

         elsif Outfile /= Standard_Output then
            Close (Text_Outfile);
         end if;
      end Process_One_File;

      -----------------------
      -- Recursive_Process --
      -----------------------

      procedure Recursive_Process (In_Dir : String; Out_Dir : String) is
         Dir_In : Dir_Type;
         Name : String (1 .. 255);
         Last : Natural;
         In_Dir_Name  : Name_Id;
         Out_Dir_Name : Name_Id;

         procedure Set_Directory_Names;
         --  Establish or reestablish the current input and output directories

         -------------------------
         -- Set_Directory_Names --
         -------------------------

         procedure Set_Directory_Names is
         begin
            Input_Directory := In_Dir_Name;
            Output_Directory := Out_Dir_Name;
         end Set_Directory_Names;

      --  Start of processing for Recursive_Process

      begin
         --  Open the current input directory

         begin
            Open (Dir_In, In_Dir);

         exception
            when Directory_Error =>
               Fail ("could not read directory " & In_Dir);
         end;

         --  Set the new input and output directory names

         Name_Len := In_Dir'Length;
         Name_Buffer (1 .. Name_Len) := In_Dir;
         In_Dir_Name := Name_Find;
         Name_Len := Out_Dir'Length;
         Name_Buffer (1 .. Name_Len) := Out_Dir;
         Out_Dir_Name := Name_Find;

         Set_Directory_Names;

         --  Traverse the input directory
         loop
            Read (Dir_In, Name, Last);
            exit when Last = 0;

            if Name (1 .. Last) /= "." and then Name (1 .. Last) /= ".." then
               declare
                  Input : constant String :=
                            In_Dir & Directory_Separator & Name (1 .. Last);
                  Output : constant String :=
                             Out_Dir & Directory_Separator & Name (1 .. Last);

               begin
                  --  If input is an ordinary file, process it

                  if Is_Regular_File (Input) then
                     --  First get the output file name

                     Name_Len := Last;
                     Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                     Infile_Name := Name_Find;
                     Preprocess_Infile_Name;

                     --  Set the input file name and process the file

                     Name_Len := Input'Length;
                     Name_Buffer (1 .. Name_Len) := Input;
                     Infile_Name := Name_Find;
                     Process_One_File;

                  elsif Is_Directory (Input) then
                     --  Input is a directory. If the corresponding output
                     --  directory does not already exist, create it.

                     if not Is_Directory (Output) then
                        begin
                           Make_Dir (Dir_Name => Output);

                        exception
                           when Directory_Error =>
                              Fail ("could not create directory """
                                    & Output
                                    & """");
                        end;
                     end if;

                     --  And process this new input directory

                     Recursive_Process (Input, Output);

                     --  Reestablish the input and output directory names
                     --  that have been modified by the recursive call.

                     Set_Directory_Names;
                  end if;
               end;
            end if;
         end loop;
      end Recursive_Process;

   --  Start of processing for Process_Files

   begin
      if Output_Directory = No_Name then

         --  If the output is not a directory, fail if the input is
         --  an existing directory, to avoid possible problems.

         if Is_Directory (Get_Name_String (Infile_Name)) then
            Fail ("input file """ & Get_Name_String (Infile_Name) &
                  """ is a directory");
         end if;

         --  Just process the single input file

         Process_One_File;

      elsif Input_Directory = No_Name then

         --  Get the output file name from the input file name, and process
         --  the single input file.

         Preprocess_Infile_Name;
         Process_One_File;

      else
         --  Recursively process files in the directory tree rooted at the
         --  input directory.

         Recursive_Process
           (In_Dir => Get_Name_String (Input_Directory),
            Out_Dir => Get_Name_String (Output_Directory));
      end if;
   end Process_Files;

   -------------------------
   -- Put_Char_To_Outfile --
   -------------------------

   procedure Put_Char_To_Outfile (C : Character) is
   begin
      Put (Outfile.all, C);
   end Put_Char_To_Outfile;

   -----------------------
   -- Scan_Command_Line --
   -----------------------

   procedure Scan_Command_Line is
      Switch : Character;

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

      --  Start of processing for Scan_Command_Line

   begin
      --  First check for --version or --help

      Check_Version_And_Help ("GNATPREP", "1996");

      --  Now scan the other switches

      GNAT.Command_Line.Initialize_Option_Scan;

      loop
         begin
            Switch := GNAT.Command_Line.Getopt ("D: a b c C r s T u v");

            case Switch is
               when ASCII.NUL =>
                  exit;

               when 'D' =>
                  Process_Command_Line_Symbol_Definition
                    (S => GNAT.Command_Line.Parameter);

               when 'a' =>
                  Opt.No_Deletion := True;
                  Opt.Undefined_Symbols_Are_False := True;

               when 'b' =>
                  Opt.Blank_Deleted_Lines := True;

               when 'c' =>
                  Opt.Comment_Deleted_Lines := True;

               when 'C' =>
                  Opt.Replace_In_Comments := True;

               when 'r' =>
                  Source_Ref_Pragma := True;

               when 's' =>
                  Opt.List_Preprocessing_Symbols := True;

               when 'T' =>
                  Unix_Line_Terminators := True;

               when 'u' =>
                  Opt.Undefined_Symbols_Are_False := True;

               when 'v' =>
                  Opt.Verbose_Mode := True;

               when others =>
                  Fail ("Invalid Switch: -" & Switch);
            end case;

         exception
            when GNAT.Command_Line.Invalid_Switch =>
               Write_Str ("Invalid Switch: -");
               Write_Line (GNAT.Command_Line.Full_Switch);
               GNAT.Command_Line.Try_Help;
               OS_Exit (1);
         end;
      end loop;

      --  Get the file names

      loop
         declare
            S : constant String := GNAT.Command_Line.Get_Argument;

         begin
            exit when S'Length = 0;

            Name_Len := S'Length;
            Name_Buffer (1 .. Name_Len) := S;

            if Infile_Name = No_Name then
               Infile_Name := Name_Find;
            elsif Outfile_Name = No_Name then
               Outfile_Name := Name_Find;
            elsif Deffile_Name = No_Name then
               Deffile_Name := Name_Find;
            else
               Fail ("too many arguments specified");
            end if;
         end;
      end loop;
   end Scan_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Display_Copyright;
      Write_Line ("Usage: gnatprep [-bcrsuv] [-Dsymbol=value] " &
                    "infile outfile [deffile]");
      Write_Eol;
      Write_Line ("  infile     Name of the input file");
      Write_Line ("  outfile    Name of the output file");
      Write_Line ("  deffile    Name of the definition file");
      Write_Eol;
      Write_Line ("gnatprep switches:");
      Display_Usage_Version_And_Help;
      Write_Line ("   -b  Replace preprocessor lines by blank lines");
      Write_Line ("   -c  Keep preprocessor lines as comments");
      Write_Line ("   -C  Do symbol replacements within comments");
      Write_Line ("   -D  Associate symbol with value");
      Write_Line ("   -r  Generate Source_Reference pragma");
      Write_Line ("   -s  Print a sorted list of symbol names and values");
      Write_Line ("   -T  Use LF as line terminators");
      Write_Line ("   -u  Treat undefined symbols as FALSE");
      Write_Line ("   -v  Verbose mode");
      Write_Eol;
   end Usage;

end GPrep;
