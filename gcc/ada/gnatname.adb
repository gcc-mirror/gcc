------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T N A M E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2001-2004 Free Software Foundation, Inc.         --
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

with Gnatvsn;
with Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Makr;
with Table;

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

procedure Gnatname is

   Usage_Output : Boolean := False;
   --  Set to True when usage is output, to avoid multiple output

   Usage_Needed : Boolean := False;
   --  Set to True by -h switch

   Version_Output : Boolean := False;
   --  Set to True when version is output, to avoid multiple output

   Very_Verbose : Boolean := False;
   --  Set to True with -v -v

   Create_Project : Boolean := False;
   --  Set to True with a -P switch

   File_Path : String_Access := new String'("gnat.adc");
   --  Path name of the file specified by -c or -P switch

   File_Set : Boolean := False;
   --  Set to True by -c or -P switch.
   --  Used to detect multiple -c/-P switches.

   package Excluded_Patterns is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Gnatname.Excluded_Patterns");
   --  Table to accumulate the negative patterns

   package Foreign_Patterns is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Gnatname.Foreign_Patterns");
   --  Table to accumulate the foreign patterns

   package Patterns is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Gnatname.Patterns");
   --  Table to accumulate the name patterns

   package Source_Directories is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Gnatname.Source_Directories");
   --  Table to accumulate the source directories specified directly with -d
   --  or indirectly with -D.

   package Preprocessor_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 2,
      Table_Increment      => 50,
      Table_Name           => "Gnatname.Preprocessor_Switches");
   --  Table to store the preprocessor switches to be used in the call
   --  to the compiler.

   procedure Output_Version;
   --  Print name and version

   procedure Usage;
   --  Print usage

   procedure Scan_Args;
   --  Scan the command line arguments

   procedure Add_Source_Directory (S : String);
   --  Add S in the Source_Directories table

   procedure Get_Directories (From_File : String);
   --  Read a source directory text file

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory (S : String) is
   begin
      Source_Directories.Increment_Last;
      Source_Directories.Table (Source_Directories.Last) := new String'(S);
   end Add_Source_Directory;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories (From_File : String) is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 2_000);
      Last : Natural;

   begin
      Open (File, In_File, From_File);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);

         if Last /= 0 then
            Add_Source_Directory (Line (1 .. Last));
         end if;
      end loop;

      Close (File);

   exception
      when Name_Error =>
         Fail ("cannot open source directory """ & From_File & '"');
   end Get_Directories;

   --------------------
   -- Output_Version --
   --------------------

   procedure Output_Version is
   begin
      if not Version_Output then
         Version_Output := True;
         Output.Write_Eol;
         Output.Write_Str ("GNATNAME ");
         Output.Write_Str (Gnatvsn.Gnat_Version_String);
         Output.Write_Line
           (" Copyright 2001-2004 Free Software Foundation, Inc.");
      end if;
   end Output_Version;

   ---------------
   -- Scan_Args --
   ---------------

   procedure Scan_Args is
   begin
      Initialize_Option_Scan;

      --  Scan options first

      loop
         case Getopt ("c: d: gnatep=! gnatep! gnateD! D: h P: v x: f:") is
            when ASCII.NUL =>
               exit;

            when 'c' =>
               if File_Set then
                  Fail ("only one -P or -c switch may be specified");
               end if;

               File_Set := True;
               File_Path := new String'(Parameter);
               Create_Project := False;

            when 'd' =>
               Add_Source_Directory (Parameter);

            when 'D' =>
               Get_Directories (Parameter);

            when 'f' =>
               Foreign_Patterns.Increment_Last;
               Foreign_Patterns.Table (Foreign_Patterns.Last) :=
                 new String'(Parameter);

            when 'g' =>
               Preprocessor_Switches.Increment_Last;
               Preprocessor_Switches.Table (Preprocessor_Switches.Last) :=
                 new String'('-' & Full_Switch & Parameter);

            when 'h' =>
               Usage_Needed := True;

            when 'P' =>
               if File_Set then
                  Fail ("only one -c or -P switch may be specified");
               end if;

               File_Set       := True;
               File_Path      := new String'(Parameter);
               Create_Project := True;

            when 'v' =>
               if Opt.Verbose_Mode then
                  Very_Verbose := True;

               else
                  Opt.Verbose_Mode := True;
               end if;

            when 'x' =>
               Excluded_Patterns.Increment_Last;
               Excluded_Patterns.Table (Excluded_Patterns.Last) :=
                 new String'(Parameter);

            when others =>
               null;
         end case;
      end loop;

      --  Now, get the name patterns, if any

      loop
         declare
            S : String := Get_Argument (Do_Expansion => False);

         begin
            exit when S = "";
            Canonical_Case_File_Name (S);
            Patterns.Increment_Last;
            Patterns.Table (Patterns.Last) := new String'(S);
         end;
      end loop;

   exception
      when Invalid_Switch =>
         Fail ("invalid switch " & Full_Switch);
   end Scan_Args;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Needed := False;
         Usage_Output := True;
         Write_Str ("Usage: ");
         Osint.Write_Program_Name;
         Write_Line (" [switches] naming-pattern [naming-patterns]");
         Write_Eol;
         Write_Line ("switches:");

         Write_Line ("  -cfile       create configuration pragmas file");
         Write_Line ("  -ddir        use dir as one of the source " &
                     "directories");
         Write_Line ("  -Dfile       get source directories from file");
         Write_Line ("  -fpat        foreign pattern");
         Write_Line ("  -gnateDsym=v preprocess with symbol definition");
         Write_Line ("  -gnatep=data preprocess files with data file");
         Write_Line ("  -h           output this help message");
         Write_Line ("  -Pproj       update or create project file proj");
         Write_Line ("  -v           verbose output");
         Write_Line ("  -v -v        very verbose output");
         Write_Line ("  -xpat        exclude pattern pat");
      end if;
   end Usage;

--  Start of processing for Gnatname

begin
   --  Initialize tables

   Excluded_Patterns.Set_Last (0);
   Foreign_Patterns.Set_Last (0);
   Patterns.Set_Last (0);
   Source_Directories.Set_Last (0);
   Preprocessor_Switches.Set_Last (0);

   --  Get the arguments

   Scan_Args;

   if Opt.Verbose_Mode then
      Output_Version;
   end if;

   if Usage_Needed then
      Usage;
   end if;

   --  If no pattern was specified, print the usage and return

   if Patterns.Last = 0 and Foreign_Patterns.Last = 0 then
      Usage;
      return;
   end if;

   --  If no source directory was specified, use the current directory as the
   --  unique directory. Note that if a file was specified with directory
   --  information, the current directory is the directory of the specified
   --  file.

   if Source_Directories.Last = 0 then
      Source_Directories.Increment_Last;
      Source_Directories.Table (Source_Directories.Last) := new String'(".");
   end if;

   declare
      Directories   : Argument_List (1 .. Integer (Source_Directories.Last));
      Name_Patterns : Argument_List (1 .. Integer (Patterns.Last));
      Excl_Patterns : Argument_List (1 .. Integer (Excluded_Patterns.Last));
      Frgn_Patterns : Argument_List (1 .. Integer (Foreign_Patterns.Last));
      Prep_Switches : Argument_List
                        (1 .. Integer (Preprocessor_Switches.Last));

   begin
      --  Build the Directories and Name_Patterns arguments

      for Index in Directories'Range loop
         Directories (Index) := Source_Directories.Table (Index);
      end loop;

      for Index in Name_Patterns'Range loop
         Name_Patterns (Index) := Patterns.Table (Index);
      end loop;

      for Index in Excl_Patterns'Range loop
         Excl_Patterns (Index) := Excluded_Patterns.Table (Index);
      end loop;

      for Index in Frgn_Patterns'Range loop
         Frgn_Patterns (Index) := Foreign_Patterns.Table (Index);
      end loop;

      for Index in Prep_Switches'Range loop
         Prep_Switches (Index) := Preprocessor_Switches.Table (Index);
      end loop;

      --  Call Prj.Makr.Make where the real work is done

      Prj.Makr.Make
        (File_Path         => File_Path.all,
         Project_File      => Create_Project,
         Directories       => Directories,
         Name_Patterns     => Name_Patterns,
         Excluded_Patterns => Excl_Patterns,
         Foreign_Patterns  => Frgn_Patterns,
         Preproc_Switches  => Prep_Switches,
         Very_Verbose      => Very_Verbose);
   end;

   if Opt.Verbose_Mode then
      Write_Eol;
   end if;
end Gnatname;
