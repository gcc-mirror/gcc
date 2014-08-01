------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                             X G N A T U G N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2014, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  This is a temporary version whose only purpose is to work with
--  Makefile.gnat6
--  Its main previous purpose (to handle VMS-specific wording in
--  gnat_ugn.texi and projects.texi) is not applicable, since there is
--  no longer a VMS-specific version of the User's Guide.

--  The program is invoked as follows:

--  xgnatugn <target> <in-file> <word-list> <out-file>

--  In this temporary version, the program simply copies <in-file>
--  to <out-file> and ignores the <target> and <word-list> arguments

with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Xgnatugn is

   Max_Line_Length : constant := 5000;
   Line            : String (1 .. Max_Line_Length);
   Last            : Natural;
   File1, File2    : File_Type;

begin

   Open (File1, Mode => In_File, Name => Argument (2));
   Create (File2, Mode => Out_File, Name => Argument (4));

   while not End_Of_File (File1) loop
      Get_Line (File1, Line, Last);
      Put_Line (File2, Line (1 .. Last));
   end loop;
end Xgnatugn;
