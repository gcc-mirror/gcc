------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G P R E P                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2004, Free Software Foundation, Inc.         --
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

with Csets;
with Err_Vars; use Err_Vars;
with Errutil;
with Gnatvsn;
with Namet;    use Namet;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prep;     use Prep;
with Scng;
with Sinput.C;
with Snames;
with Stringt;  use Stringt;
with Types;    use Types;

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

package body GPrep is

   Copyright_Displayed : Boolean := False;
   --  Used to prevent multiple displays of the copyright notice

   ------------------------
   -- Argument Line Data --
   ------------------------

   Infile_Name  : String_Access;
   Outfile_Name : String_Access;
   Deffile_Name : String_Access;

   Source_Ref_Pragma : Boolean := False;
   --  Record command line options (set if -r switch set)

   Text_Outfile : aliased Ada.Text_IO.File_Type;
   Outfile      : constant File_Access := Text_Outfile'Access;

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

   procedure Process_Command_Line_Symbol_Definition (S : String);
   --  Process a -D switch on ther command line

   procedure Put_Char_To_Outfile (C : Character);
   --  Output one character to the output file.
   --  Used to initialize the preprocessor.

   procedure New_EOL_To_Outfile;
   --  Output a new line to the output file.
   --  Used to initialize the preprocessor.

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
         Write_Line ("GNAT Preprocessor " &
                     Gnatvsn.Gnat_Version_String &
                     " Copyright 1996-2004 Free Software Foundation, Inc.");
         Copyright_Displayed := True;
      end if;
   end Display_Copyright;

   --------------
   -- Gnatprep --
   --------------

   procedure Gnatprep is
      Infile : Source_File_Index;

   begin
      --  Do some initializations (order is important here!)

      Csets.Initialize;
      Namet.Initialize;
      Snames.Initialize;
      Stringt.Initialize;

      --  Initialize the preprocessor

      Prep.Initialize
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

      if Infile_Name = null then
         --  No input file specified, just output the usage and exit

         Usage;
         return;
      elsif Outfile_Name = null then
         --  No output file specified, just output the usage and exit

         Usage;
         return;
      end if;

      --  If a pragma Source_File_Name, we need to keep line numbers.
      --  So, if the deleted lines are not put as comment, we must output them
      --  as blank lines.

      if Source_Ref_Pragma and (not Opt.Comment_Deleted_Lines) then
         Opt.Blank_Deleted_Lines := True;
      end if;

      --  If we have a definition file, parse it

      if Deffile_Name /= null then
         declare
            Deffile : Source_File_Index;

         begin
            Errutil.Initialize;
            Deffile := Sinput.C.Load_File (Deffile_Name.all);

            --  Set Main_Source_File to the definition file for the benefit of
            --  Errutil.Finalize.

            Sinput.Main_Source_File := Deffile;

            if Deffile = No_Source_File then
               Fail ("unable to find definition file """,
                     Deffile_Name.all,
                     """");
            end if;

            Scanner.Initialize_Scanner (No_Unit, Deffile);

            Prep.Parse_Def_File;
         end;
      end if;

      --  If there are errors in the definition file, output these errors
      --  and exit.

      if Total_Errors_Detected > 0 then
         Errutil.Finalize (Source_Type => "definition");
         Fail ("errors in definition file """, Deffile_Name.all, """");
      end if;

      --  If -s switch was specified, print a sorted list of symbol names and
      --  values, if any.

      if Opt.List_Preprocessing_Symbols then
         Prep.List_Symbols (Foreword => "");
      end if;

      --  Load the input file

      Infile := Sinput.C.Load_File (Infile_Name.all);

      if Infile = No_Source_File then
         Fail ("unable to find input file """, Infile_Name.all, """");
      end if;

      --  Set Main_Source_File to the input file for the benefit of
      --  Errutil.Finalize.

      Sinput.Main_Source_File := Infile;

      Scanner.Initialize_Scanner (No_Unit, Infile);

      --  If an output file were specified, create it; fails if this did not
      --  work.

      if Outfile_Name /= null then
         begin
            Create (Text_Outfile, Out_File, Outfile_Name.all);

         exception
            when others =>
               Fail
                 ("unable to create output file """, Outfile_Name.all, """");
         end;
      end if;

      --  Output the SFN pragma if asked to

      if Source_Ref_Pragma then
         Put_Line (Outfile.all, "pragma Source_Reference (1, """ &
                   Get_Name_String (Sinput.File_Name (Infile)) &
                   """);");
      end if;

      --  Preprocess the input file

      Prep.Preprocess;

      --  In verbose mode, if there is no error, report it

      if Opt.Verbose_Mode and then Err_Vars.Total_Errors_Detected = 0 then
         Errutil.Finalize (Source_Type => "input");
      end if;

      --  If we had some errors, delete the output file, and report the errors,

      if Err_Vars.Total_Errors_Detected > 0 then
         if Outfile /= Standard_Output then
            Delete (Text_Outfile);
         end if;

         Errutil.Finalize (Source_Type => "input");

      --  otherwise, close the output file, and we are done.

      elsif Outfile /= Standard_Output then
         Close (Text_Outfile);
      end if;
   end Gnatprep;

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

      --  If symbol does not alrady exist, create a new entry in the mapping
      --  table.

      if Symbol = No_Symbol then
         Symbol_Table.Increment_Last (Mapping);
         Symbol := Symbol_Table.Last (Mapping);
      end if;

      Mapping.Table (Symbol) := Data;
   end Process_Command_Line_Symbol_Definition;

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

   begin
      --  Parse the switches

      loop
         begin
            Switch := GNAT.Command_Line.Getopt ("D: b c r s u v");
            case Switch is

               when ASCII.NUL =>
                  exit;

               when 'D' =>
                  Process_Command_Line_Symbol_Definition
                    (S => GNAT.Command_Line.Parameter);

               when 'b' =>
                  Opt.Blank_Deleted_Lines := True;

               when 'c' =>
                  Opt.Comment_Deleted_Lines := True;

               when 'r' =>
                  Source_Ref_Pragma := True;

               when 's' =>
                  Opt.List_Preprocessing_Symbols := True;

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
               Usage;
               OS_Exit (1);
         end;
      end loop;

      --  Get the file names

      loop
         declare
            S : constant String := GNAT.Command_Line.Get_Argument;

         begin
            exit when S'Length = 0;

            if Infile_Name = null then
               Infile_Name := new String'(S);
            elsif Outfile_Name = null then
               Outfile_Name := new String'(S);
            elsif Deffile_Name = null then
               Deffile_Name := new String'(S);
            else
               Fail ("too many arguments specifed");
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
      Write_Line ("   -b  Replace preprocessor lines by blank lines");
      Write_Line ("   -c  Keep preprocessor lines as comments");
      Write_Line ("   -D  Associate symbol with value");
      Write_Line ("   -r  Generate Source_Reference pragma");
      Write_Line ("   -s  Print a sorted list of symbol names and values");
      Write_Line ("   -u  Treat undefined symbols as FALSE");
      Write_Line ("   -v  Verbose mode");
      Write_Eol;
   end Usage;

end GPrep;
