------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T F I N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--         Copyright (C) 1998-2001 Free Software Foundation, Inc.           --
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
------------------------------------------------------------------------------

with Xr_Tabls;
with Xref_Lib; use Xref_Lib;
with Ada.Text_IO;
with GNAT.Command_Line;
with Gnatvsn;
with Osint;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

---------------
--  Gnatfind --
---------------

procedure Gnatfind is

   Output_Ref      : Boolean := False;
   Pattern         : Xref_Lib.Search_Pattern;
   Local_Symbols   : Boolean := True;
   Prj_File        : File_Name_String;
   Prj_File_Length : Natural := 0;
   Nb_File         : Natural := 0;
   Usage_Error     : exception;
   Full_Path_Name  : Boolean := False;
   Have_Entity     : Boolean := False;
   Wide_Search     : Boolean := True;
   Glob_Mode       : Boolean := True;
   Der_Info        : Boolean := False;
   Type_Tree       : Boolean := False;
   Read_Only       : Boolean := False;
   Source_Lines    : Boolean := False;

   Has_File_In_Entity : Boolean := False;
   --  Will be true if a file name was specified in the entity

   procedure Parse_Cmd_Line;
   --  Parse every switch on the command line

   procedure Write_Usage;
   --  Print a small help page for program usage

   --------------------
   -- Parse_Cmd_Line --
   --------------------

   procedure Parse_Cmd_Line is
   begin
      loop
         case GNAT.Command_Line.Getopt ("a aI: aO: d e f g h I: p: r s t") is
            when ASCII.NUL =>
               exit;

            when 'a'    =>
               if GNAT.Command_Line.Full_Switch = "a" then
                  Read_Only := True;
               elsif GNAT.Command_Line.Full_Switch = "aI" then
                  Osint.Add_Src_Search_Dir (GNAT.Command_Line.Parameter);
               else
                  Osint.Add_Lib_Search_Dir (GNAT.Command_Line.Parameter);
               end if;

            when 'd'    =>
               Der_Info := True;

            when 'e'    =>
               Glob_Mode := False;

            when 'f'    =>
               Full_Path_Name := True;

            when 'g'    =>
               Local_Symbols := False;

            when 'h'    =>
               Write_Usage;

            when 'I'    =>
               Osint.Add_Src_Search_Dir (GNAT.Command_Line.Parameter);
               Osint.Add_Lib_Search_Dir (GNAT.Command_Line.Parameter);

            when 'p'    =>
               declare
                  S : constant String := GNAT.Command_Line.Parameter;
               begin
                  Prj_File_Length := S'Length;
                  Prj_File (1 .. Prj_File_Length) := S;
               end;

            when 'r'    =>
               Output_Ref := True;

            when 's' =>
               Source_Lines := True;

            when 't' =>
               Type_Tree := True;

            when others =>
               Write_Usage;
         end case;
      end loop;

      --  Get the other arguments

      loop
         declare
            S : constant String := GNAT.Command_Line.Get_Argument;
         begin
            exit when S'Length = 0;

            --  First argument is the pattern

            if not Have_Entity then
               Add_Entity (Pattern, S, Glob_Mode);
               Have_Entity := True;

               if not Has_File_In_Entity
                 and then Index (S, ":") /= 0
               then
                  Has_File_In_Entity := True;
               end if;

            --  Next arguments are the files to search
            else
               Add_File (S);
               Wide_Search := False;
               Nb_File := Nb_File + 1;
            end if;
         end;
      end loop;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Ada.Text_IO.Put_Line ("Invalid switch : "
                               & GNAT.Command_Line.Full_Switch);
         Write_Usage;

      when GNAT.Command_Line.Invalid_Parameter =>
         Ada.Text_IO.Put_Line ("Parameter missing for : "
                               & GNAT.Command_Line.Parameter);
         Write_Usage;

      when Xref_Lib.Invalid_Argument =>
         Ada.Text_IO.Put_Line ("Invalid line or column in the pattern");
         Write_Usage;
   end Parse_Cmd_Line;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
      use Ada.Text_IO;

   begin
      Put_Line ("GNATFIND " & Gnatvsn.Gnat_Version_String
                & " Copyright 1998-2001, Ada Core Technologies Inc.");
      Put_Line ("Usage: gnatfind pattern[:sourcefile[:line[:column]]] "
                & "[file1 file2 ...]");
      New_Line;
      Put_Line ("  pattern     Name of the entity to look for (can have "
                & "wildcards)");
      Put_Line ("  sourcefile  Only find entities referenced from this "
                & "file");
      Put_Line ("  line        Only find entities referenced from this line "
                & "of file");
      Put_Line ("  column      Only find entities referenced from this columns"
                & " of file");
      Put_Line ("  file ...    Set of Ada source files to search for "
                & "references. This parameters are optional");
      New_Line;
      Put_Line ("gnatfind switches:");
      Put_Line ("   -a      Consider all files, even when the ali file is "
                & "readonly");
      Put_Line ("   -aIdir  Specify source files search path");
      Put_Line ("   -aOdir  Specify library/object files search path");
      Put_Line ("   -d      Output derived type information");
      Put_Line ("   -e      Use the full regular expression set for pattern");
      Put_Line ("   -f      Output full path name");
      Put_Line ("   -g      Output information only for global symbols");
      Put_Line ("   -Idir   Like -aIdir -aOdir");
      Put_Line ("   -p file Use file as the default project file");
      Put_Line ("   -r      Find all references (default to find declaration"
                & " only)");
      Put_Line ("   -s      Print source line");
      Put_Line ("   -t      Print type hierarchy");
      New_Line;

      raise Usage_Error;
   end Write_Usage;

begin
   Osint.Initialize (Osint.Compiler);

   Parse_Cmd_Line;

   if not Have_Entity then
      Write_Usage;
   end if;

   --  Special case to speed things up: if the user has a command line of the
   --  form 'gnatfind entity:file', ie has specified a file and only wants the
   --  bodies and specs, then we can restrict the search to the .ali file
   --  associated with 'file'.

   if Has_File_In_Entity
     and then not Output_Ref
   then
      Wide_Search := False;
   end if;

   --  Find the project file

   if Prj_File_Length = 0 then
      Xr_Tabls.Create_Project_File (Default_Project_File ("."));
   else
      Xr_Tabls.Create_Project_File (Prj_File (1 .. Prj_File_Length));
   end if;

   --  Fill up the table

   if Type_Tree and then Nb_File > 1 then
      Ada.Text_IO.Put_Line ("Error: for type hierarchy output you must "
                            & "specify only one file.");
      Ada.Text_IO.New_Line;
      Write_Usage;
   end if;

   Search (Pattern, Local_Symbols, Wide_Search, Read_Only,
           Der_Info, Type_Tree);

   if Source_Lines then
      Xr_Tabls.Grep_Source_Files;
   end if;

   Print_Gnatfind (Output_Ref, Full_Path_Name);

exception
   when Usage_Error =>
      null;
end Gnatfind;
