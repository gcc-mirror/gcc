------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 S C N G                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

--  This package contains a generic lexical analyzer. This is used
--  for scanning Ada source files or text files with an Ada-like syntax,
--  such as project files. It is instantiated in Scn and Prj.Err.

with Casing; use Casing;
with Styleg;
with Types;  use Types;

generic
   with procedure Post_Scan;
   --  Procedure called by Scan for the following tokens:
   --  Tok_Char_Literal, Tok_Identifier, Tok_Real_Literal, Tok_Real_Literal,
   --  Tok_Integer_Literal, Tok_String_Literal, Tok_Operator_Symbol.

   with procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr);
   --  Output a message at specified location

   with procedure Error_Msg_S (Msg : String);
   --  Output a message at current scan pointer location

   with procedure Error_Msg_SC (Msg : String);
   --  Output a message at the start of the current token

   with procedure Error_Msg_SP (Msg : String);
   --  Output a message at the start of the previous token

   with procedure Obsolescent_Check (S : Source_Ptr);
   --  Called when one of the obsolescent character replacements is
   --  used with S pointing to the character in question.

   with package Style is new Styleg
     (Error_Msg, Error_Msg_S, Error_Msg_SC, Error_Msg_SP);
   --  Instantiation of Styleg with the same error reporting routines

package Scng is

   procedure Initialize_Scanner
     (Unit  : Unit_Number_Type;
      Index : Source_File_Index);
   --  Initialize lexical scanner for scanning a new file. The caller has
   --  completed the construction of the Units.Table entry for the specified
   --  Unit and Index references the corresponding source file. A special
   --  case is when Unit = No_Unit_Number, and Index corresponds to the
   --  source index for reading the configuration pragma file.
   --  Initialize_Scanner does not call Scan.

   procedure Scan;
   --  Scan scans out the next token, and advances the scan state accordingly
   --  (see package Scan_State for details). If the scan encounters an illegal
   --  token, then an error message is issued pointing to the bad character,
   --  and Scan returns a reasonable substitute token of some kind.
   --  For tokens Char_Literal, Identifier, Real_Literal, Integer_Literal,
   --  String_Literal and Operator_Symbol, Post_Scan is called after scanning.

   function Determine_Token_Casing return Casing_Type;
   pragma Inline (Determine_Token_Casing);
   --  Determines the casing style of the current token, which is
   --  either a keyword or an identifier. See also package Casing.

   procedure Set_Special_Character (C : Character);
   --  Indicate that one of the following character '#', '$', '?', '@', '`',
   --  '\', '^', '_' or '~', when found is a Special token.

   procedure Reset_Special_Characters;
   --  Indicate that there is no characters that are Special tokens., which
   --  is the default.

   procedure Set_End_Of_Line_As_Token (Value : Boolean);
   --  Indicate if End_Of_Line is a token or not.
   --  By default, End_Of_Line is not a token.

   procedure Set_Comment_As_Token (Value : Boolean);
   --  Indicate if a comment is a token or not.
   --  By default, a comment is not a token.

   function Set_Start_Column return Column_Number;
   --  This routine is called with Scan_Ptr pointing to the first character
   --  of a line. On exit, Scan_Ptr is advanced to the first non-blank
   --  character of this line (or to the terminating format effector if the
   --  line contains no non-blank characters), and the returned result is the
   --  column number of this non-blank character (zero origin), which is the
   --  value to be stored in the Start_Column scan variable.

end Scng;
