------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . D                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2008, Free Software Foundation, Inc.         --
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

--  This child package contains the routines used to write debug source
--  files. These routines are not in Sinput.L, because they are used only
--  by the compiler, while Sinput.L is also used by gnatmake.

package Sinput.D is

   ------------------------------------------------
   -- Subprograms for Writing Debug Source Files --
   ------------------------------------------------

   procedure Create_Debug_Source
     (Source : Source_File_Index;
      Loc    : out Source_Ptr);
   --  Given a source file, creates a new source file table entry to be used
   --  for the debug source file output (Debug_Generated_Code switch set).
   --  Loc is set to the initial Sloc value for the first line. This call
   --  also creates the debug source output file (using Create_Debug_File).

   procedure Write_Debug_Line (Str : String; Loc : in out Source_Ptr);
   --  This procedure is called to write a line to the debug source file
   --  previously created by Create_Debug_Source using Write_Debug_Info.
   --  Str is the source line to be written to the file (it does not include
   --  an end of line character). On entry Loc is the Sloc value previously
   --  returned by Create_Debug_Source or Write_Debug_Line, and on exit,
   --  Sloc is updated to point to the start of the next line to be written,
   --  taking into account the length of the terminator that was written by
   --  Write_Debug_Info.

   procedure Close_Debug_Source;
   --  This procedure completes the source table entry for the debug file
   --  previously created by Create_Debug_Source, and written using the
   --  Write_Debug_Line procedure. It then calls Close_Debug_File to
   --  complete the writing of the file itself.

end Sinput.D;
