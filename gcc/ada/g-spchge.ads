------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          G N A T . S P E L L I N G _ C H E C K E R _ G E N E R I C       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2007, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  Spelling checker

--  This package provides a utility generic routine for checking for bad
--  spellings. This routine must be instantiated with an appropriate array
--  element type, which must represent a character encoding in which the
--  codes for ASCII characters in the range 16#20#..16#7F# have their normal
--  expected encoding values (e.g. the Pos value 16#31# must be digit 1).

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package GNAT.Spelling_Checker_Generic is
   pragma Pure;

   generic
      type Char_Type is (<>);
      --  See above for restrictions on what types can be used here

      type String_Type is array (Positive range <>) of Char_Type;

   function Is_Bad_Spelling_Of
     (Found  : String_Type;
      Expect : String_Type) return Boolean;
   --  Determines if the string Found is a plausible misspelling of the string
   --  Expect. Returns True for an exact match or a probably misspelling, False
   --  if no near match is detected. This routine is case sensitive, so the
   --  caller should fold both strings to get a case insensitive match if the
   --  character encoding represents upper/lower case.
   --
   --  Note: the spec of this routine is deliberately rather vague. This
   --  routine is the one used by GNAT itself to detect misspelled keywords
   --  and identifiers, and is heuristically adjusted to be appropriate to
   --  this usage. It will work well in any similar case of named entities.

end GNAT.Spelling_Checker_Generic;
