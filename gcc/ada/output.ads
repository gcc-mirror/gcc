------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains low level output routines used by the compiler
--  for writing error messages and informational output. It is also used
--  by the debug source file output routines (see Sprintf.Print_Eol).

with Types; use Types;

package Output is
pragma Elaborate_Body (Output);

   -------------------------
   -- Line Buffer Control --
   -------------------------

   --  Note: the following buffer and column position are maintained by
   --  the subprograms defined in this package, and are not normally
   --  directly modified or accessed by a client. However, a client is
   --  permitted to modify these values, using the knowledge that only
   --  Write_Eol actually generates any output.

   Buffer_Max : constant := 8192;
   Buffer     : String (1 .. Buffer_Max + 1);
   --  Buffer used to build output line. We do line buffering because it
   --  is needed for the support of the debug-generated-code option (-gnatD).
   --  Historically it was first added because on VMS, line buffering is
   --  needed with certain file formats. So in any case line buffering must
   --  be retained for this purpose, even if other reasons disappear. Note
   --  any attempt to write more output to a line than can fit in the buffer
   --  will be silently ignored.

   Column : Pos range 1 .. Buffer'Length + 1 := 1;
   --  Column about to be written.

   -----------------
   -- Subprograms --
   -----------------

   procedure Set_Standard_Error;
   --  Sets subsequent output to appear on the standard error file (whatever
   --  that might mean for the host operating system, if anything).

   procedure Set_Standard_Output;
   --  Sets subsequent output to appear on the standard output file (whatever
   --  that might mean for the host operating system, if anything). This is
   --  the default mode before any call to either of the Set procedures.

   procedure Write_Char (C : Character);
   --  Write one character to the standard output file. Note that the
   --  character should not be LF or CR (use Write_Eol for end of line)

   procedure Write_Eol;
   --  Write an end of line (whatever is required by the system in use,
   --  e.g. CR/LF for DOS, or LF for Unix) to the standard output file.
   --  This routine also empties the line buffer, actually writing it
   --  to the file. Note that Write_Eol is the only routine that causes
   --  any actual output to be written.

   procedure Write_Int (Val : Int);
   --  Write an integer value with no leading blanks or zeroes. Negative
   --  values are preceded by a minus sign).

   procedure Write_Str (S : String);
   --  Write a string of characters to the standard output file. Note that
   --  end of line is handled separately using WRITE_EOL, so the string
   --  should not contain either of the characters LF or CR, but it may
   --  contain horizontal tab characters.

   procedure Write_Line (S : String);
   --  Equivalent to Write_Str (S) followed by Write_Eol;

   --------------------------
   -- Debugging Procedures --
   --------------------------

   --  The following procedures are intended only for debugging purposes,
   --  for temporary insertion into the text in environments where a debugger
   --  is not available. They all have non-standard very short lower case
   --  names, precisely to make sure that they are only used for debugging!

   procedure w (C : Character);
   --  Dump quote, character quote, followed by line return

   procedure w (S : String);
   --  Dump string followed by line return

   procedure w (V : Int);
   --  Dump integer followed by line return

   procedure w (B : Boolean);
   --  Dump Boolean followed by line return

   procedure w (L : String; C : Character);
   --  Dump contents of string followed by blank, quote, character, quote

   procedure w (L : String; S : String);
   --  Dump two strings separated by blanks, followed by line return

   procedure w (L : String; V : Int);
   --  Dump contents of string followed by blank, integer, line return

   procedure w (L : String; B : Boolean);
   --  Dump contents of string followed by blank, Boolean, line return

end Output;
