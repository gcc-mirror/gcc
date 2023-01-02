------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       G N A T . W I D E _ W I D E _ S P E L L I N G _ C H E C K E R      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2023, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Spelling checker

--  This package provides a utility routine for checking for bad spellings
--  for the case of Wide_Wide_String arguments.

package GNAT.Wide_Wide_Spelling_Checker is
   pragma Pure;

   function Is_Bad_Spelling_Of
     (Found  : Wide_Wide_String;
      Expect : Wide_Wide_String) return Boolean;
   --  Determines if the string Found is a plausible misspelling of the string
   --  Expect. Returns True for an exact match or a probably misspelling, False
   --  if no near match is detected. This routine is case sensitive, so the
   --  caller should fold both strings to get a case insensitive match.
   --
   --  Note: the spec of this routine is deliberately rather vague. It is used
   --  by GNAT itself to detect misspelled keywords and identifiers, and is
   --  heuristically adjusted to be appropriate to this usage. It will work
   --  well in any similar case of named entities.

end GNAT.Wide_Wide_Spelling_Checker;
