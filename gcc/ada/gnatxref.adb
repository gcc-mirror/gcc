------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T X R E F                              --
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
with Ada.Strings.Fixed;
with GNAT.Command_Line;
with Gnatvsn;
with Osint;

procedure Gnatxref is

   Search_Unused   : Boolean := False;
   Local_Symbols   : Boolean := True;
   Prj_File        : File_Name_String;
   Prj_File_Length : Natural := 0;
   Usage_Error     : exception;
   Full_Path_Name  : Boolean := False;
   Vi_Mode         : Boolean := False;
   Read_Only       : Boolean := False;
   Have_File       : Boolean := False;
   Der_Info        : Boolean := False;

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
         case GNAT.Command_Line.Getopt ("a aI: aO: d f g h I: p: u v") is
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

            when 'd' =>
               Der_Info := True;

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

            when 'u'    =>
               Search_Unused := True;
               Vi_Mode := False;

            when 'v'    =>
               Vi_Mode := True;
               Search_Unused := False;

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

            if Ada.Strings.Fixed.Index (S, ":") /= 0 then
               Ada.Text_IO.Put_Line
                 ("Only file names are allowed on the command line");
               Write_Usage;
            end if;

            Add_File (S);
            Have_File := True;
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
   end Parse_Cmd_Line;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
      use Ada.Text_IO;

   begin
      Put_Line ("GNATXREF " & Gnatvsn.Gnat_Version_String
                & " Copyright 1998-2001, Ada Core Technologies Inc.");
      Put_Line ("Usage: gnatxref [switches] file1 file2 ...");
      New_Line;
      Put_Line ("  file ... list of source files to xref, " &
                "including with'ed units");
      New_Line;
      Put_Line ("gnatxref switches:");
      Put_Line ("   -a      Consider all files, even when the ali file is"
                & " readonly");
      Put_Line ("   -aIdir  Specify source files search path");
      Put_Line ("   -aOdir  Specify library/object files search path");
      Put_Line ("   -d      Output derived type information");
      Put_Line ("   -f      Output full path name");
      Put_Line ("   -g      Output information only for global symbols");
      Put_Line ("   -Idir   Like -aIdir -aOdir");
      Put_Line ("   -p file Use file as the default project file");
      Put_Line ("   -u      List unused entities");
      Put_Line ("   -v      Print a 'tags' file for vi");
      New_Line;

      raise Usage_Error;
   end Write_Usage;

begin
   Parse_Cmd_Line;

   if not Have_File then
      Write_Usage;
   end if;

   Xr_Tabls.Set_Default_Match (True);

   --  Find the project file

   if Prj_File_Length = 0 then
      Xr_Tabls.Create_Project_File
        (Default_Project_File (Osint.To_Host_Dir_Spec (".", False).all));
   else
      Xr_Tabls.Create_Project_File (Prj_File (1 .. Prj_File_Length));
   end if;

   --  Fill up the table

   Search_Xref (Local_Symbols, Read_Only, Der_Info);

   if Search_Unused then
      Print_Unused (Full_Path_Name);
   elsif Vi_Mode then
      Print_Vi (Full_Path_Name);
   else
      Print_Xref (Full_Path_Name);
   end if;

exception
   when Usage_Error =>
      null;
end Gnatxref;
