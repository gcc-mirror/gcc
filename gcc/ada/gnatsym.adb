------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T S Y M                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003 Free Software Foundation, Inc.               --
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

--  This utility application creates symbol files in a format that is
--  platform-dependent.

--  A symbol file is a text file that lists the symbols to be exported from
--  a shared library. The format of a symbol file depends on the platform;
--  it may be a simple enumeration of the symbol (one per line) or a more
--  elaborate format (on VMS, for example). A symbol file may be used as an
--  input to the platform linker when building a shared library.

--  This utility is not available on all platforms. It is currently supported
--  only on OpenVMS.

--  gnatsym takes as parameters:
--    - the name of the symbol file to create or update
--    - the names of one or more object files where the symbols are found

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gnatvsn; use Gnatvsn;
with Osint;   use Osint;
with Output;  use Output;

with Symbols; use Symbols;
with Table;

procedure Gnatsym is

   Copyright_Displayed : Boolean := False;
   --  A flag to prevent multiple display of the Copyright notice

   Success : Boolean := True;

   Force : Boolean := False;
   --  True when -f switcxh is used

   Verbose : Boolean := False;
   --  True when -v switch is used

   Quiet : Boolean := False;
   --  True when -q switch is used

   Symbol_File_Name : String_Access;
   --  The name of the symbol file

   package Object_Files is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Gnatsymb.Object_Files");
   --  A table to store the object file names

   Object_File : Natural := 0;
   --  An index to traverse the Object_Files table

   procedure Display_Copyright;
   --  Display Copyright notice

   procedure Parse_Cmd_Line;
   --  Parse the command line switches and file names

   procedure Usage;
   --  Display the usage

   -----------------------
   -- Display_Copyright --
   -----------------------

   procedure Display_Copyright is
   begin
      if not Copyright_Displayed then
         Write_Eol;
         Write_Str ("GNATSYMB ");
         Write_Str (Gnat_Version_String);
         Write_Str (" Copyright 2003 Free Software Foundation, Inc");
         Write_Eol;
         Copyright_Displayed := True;
      end if;
   end Display_Copyright;

   --------------------
   -- Parse_Cmd_Line --
   --------------------

   procedure Parse_Cmd_Line is
   begin
      loop
         case GNAT.Command_Line.Getopt ("f q v") is
            when ASCII.NUL =>
               exit;

            when 'f' =>
               Force := True;

            when 'q' =>
               Quiet := True;

            when 'v' =>
               Verbose := True;

            when others =>
               Fail ("invalid switch: ", Full_Switch);
         end case;
      end loop;

      --  Get the file names

      loop
         declare
            S : constant String_Access :=
                           new String'(GNAT.Command_Line.Get_Argument);

         begin
            exit when S'Length = 0;

            if Symbol_File_Name = null then
               Symbol_File_Name := S;

            else
               Object_Files.Increment_Last;
               Object_Files.Table (Object_Files.Last) := S;
            end if;
         end;
      end loop;
   exception
      when Invalid_Switch =>
         Usage;
         Fail ("invalid switch : ", Full_Switch);
   end Parse_Cmd_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Write_Line ("gnatsym [options] sym_file object_file {object_file}");
      Write_Eol;
      Write_Line ("   -f  Force generation of symbol file");
      Write_Line ("   -q  Quiet mode");
      Write_Line ("   -v  Verbose mode");
      Write_Eol;
   end Usage;

--  Start of processing of Gnatsym

begin
   --  Initialize Object_Files table

   Object_Files.Set_Last (0);

   --  Parse the command line

   Parse_Cmd_Line;

   if Verbose then
      Display_Copyright;
   end if;

   --  If there is no symbol file or no object files on the command line,
   --  display the usage and exit with an error status.

   if Object_Files.Last = 0 then
      Usage;
      OS_Exit (1);

   else
      if Verbose then
         Write_Str ("Initializing symbol file """);
         Write_Str (Symbol_File_Name.all);
         Write_Line ("""");
      end if;

      --  Initialize the symbol file

      Symbols.Initialize (Symbol_File_Name.all, Force, Quiet, Success);

      --  Process the object files in order. Stop as soon as there is
      --  something wrong.

      Object_File := 0;

      while Success and then Object_File < Object_Files.Last loop
         Object_File := Object_File + 1;

         if Verbose then
            Write_Str ("Processing object file """);
            Write_Str (Object_Files.Table (Object_File).all);
            Write_Line ("""");
         end if;

         Process (Object_Files.Table (Object_File).all, Success);
      end loop;

      --  Finalize the object file

      if Success then
         if Verbose then
            Write_Str ("Finalizing """);
            Write_Str (Symbol_File_Name.all);
            Write_Line ("""");
         end if;

         Finalize (Quiet, Success);
      end if;

      if not Success then
         Fail ("unable to build symbol file");
      end if;
   end if;
end Gnatsym;
