------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B L D - I O                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--        Copyright (C) 2002 Free Software Foundation, Inc.                 --
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

--  The following private package allows the ouput of text to Makefiles
--  though buffers. It is possible to remove some lines from the buffers
--  without putting them effectively in the Makefile.

private package Bld.IO is

   procedure Create (File_Name : String);
   --  Create a new Makefile

   procedure Flush;
   --  Output all not suppressed lines to the Makefile

   procedure Close;
   --  Close the current Makefile

   procedure Delete_All;
   --  Delete all the Makefiles that have been created

   function Name_Of_File return String;
   --  Return the path name of the current Makefile

   type Position is private;
   --  Identification of a line in the Makefile

   procedure Mark (Pos : out Position);
   --  Record the current line.
   --  No characters should have been already put on this line.

   procedure Release (Pos : Position);
   --  Suppress all line after this one, including this one.

   procedure Suppress (Pos : Position);
   --  Suppress a particular line

   procedure Put (S : String);
   --  Append a string to the current line

   procedure New_Line;
   --  End a line. Go to the next one (initially empty).

private

   type Position is record
      Value : Positive := 1;
   end record;

end Bld.IO;
