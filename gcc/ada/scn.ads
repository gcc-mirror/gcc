------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S C N                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  This package contains the lexical analyzer routines. This is used both
--  for scanning Ada source files and also for scanning Ada project files.

with Casing; use Casing;
with Types;  use Types;

package Scn is

   procedure Initialize_Scanner
     (Unit  : Unit_Number_Type;
      Index : Source_File_Index);
   --  Initialize lexical scanner for scanning a new file. The caller has
   --  completed the construction of the Units.Table entry for the specified
   --  Unit and Index references the corresponding source file. A special
   --  case is when Unit = No_Unit_Number, and Index corresponds to the
   --  source index for reading the configuration pragma file.

   procedure Scan;
   --  Scan scans out the next token, and advances the scan state accordingly
   --  (see package Scan_State for details). If the scan encounters an illegal
   --  token, then an error message is issued pointing to the bad character,
   --  and Scan returns a reasonable substitute token of some kind.

   function Scan_First_Char return Source_Ptr;
   --  This routine returns the position in Source of the first non-blank
   --  character on the current line, used for certain error recovery actions.

   procedure Scan_Reserved_Identifier (Force_Msg : Boolean);
   --  This procedure is called to convert the current token, which the caller
   --  has checked is for a reserved word, to an equivalent identifier. This is
   --  of course only used in error situations where the parser can detect that
   --  a reserved word is being used as an identifier. An appropriate error
   --  message, pointing to the token, is also issued if either this is the
   --  first occurrence of misuse of this identifier, or if Force_Msg is True.

   function Determine_Token_Casing return Casing_Type;
   pragma Inline (Determine_Token_Casing);
   --  Determines the casing style of the current token, which is
   --  either a keyword or an identifier. See also package Casing.

end Scn;
