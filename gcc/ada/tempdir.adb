------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              T E M P D I R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2019, Free Software Foundation, Inc.         --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Opt;      use Opt;
with Output;   use Output;

package body Tempdir is

   Tmpdir_Needs_To_Be_Displayed : Boolean := True;

   Tmpdir   : constant String := "TMPDIR";
   Temp_Dir : String_Access   := new String'("");

   ----------------------
   -- Create_Temp_File --
   ----------------------

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Path_Name_Type)
   is
      File_Name   : String_Access;
      Current_Dir : constant String := Get_Current_Dir;

      function Directory return String;
      --  Returns Temp_Dir.all if not empty, else return current directory

      ---------------
      -- Directory --
      ---------------

      function Directory return String is
      begin
         if Temp_Dir'Length /= 0 then
            return Temp_Dir.all;
         else
            return Current_Dir;
         end if;
      end Directory;

   --  Start of processing for Create_Temp_File

   begin
      if Temp_Dir'Length /= 0 then

         --  In verbose mode, display once the value of TMPDIR, so that
         --  if temp files cannot be created, it is easier to understand
         --  where temp files are supposed to be created.

         if Verbose_Mode and then Tmpdir_Needs_To_Be_Displayed then
            Write_Str ("TMPDIR = """);
            Write_Str (Temp_Dir.all);
            Write_Line ("""");
            Tmpdir_Needs_To_Be_Displayed := False;
         end if;

         --  Change directory to TMPDIR before creating the temp file,
         --  then change back immediately to the previous directory.

         Change_Dir (Temp_Dir.all);
         Create_Temp_File (FD, File_Name);
         Change_Dir (Current_Dir);

      else
         Create_Temp_File (FD, File_Name);
      end if;

      if FD = Invalid_FD then
         Write_Line ("could not create temporary file in " & Directory);
         Name := No_Path;

      else
         declare
            Path_Name : constant String :=
                          Normalize_Pathname
                            (Directory & Directory_Separator & File_Name.all);
         begin
            Name_Len := Path_Name'Length;
            Name_Buffer (1 .. Name_Len) := Path_Name;
            Name := Name_Find;
            Free (File_Name);
         end;
      end if;
   end Create_Temp_File;

   ------------------
   -- Use_Temp_Dir --
   ------------------

   procedure Use_Temp_Dir (Status : Boolean) is
      Dir : String_Access;

   begin
      if Status then
         Dir := Getenv (Tmpdir);
      end if;

      Free (Temp_Dir);

      if Dir /= null
        and then Dir'Length > 0
        and then Is_Absolute_Path (Dir.all)
        and then Is_Directory (Dir.all)
      then
         Temp_Dir := new String'(Normalize_Pathname (Dir.all));
      else
         Temp_Dir := new String'("");
      end if;

      Free (Dir);
   end Use_Temp_Dir;

--  Start of elaboration for package Tempdir

begin
   Use_Temp_Dir (Status => True);
end Tempdir;
