------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ L L I                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

--  This package contains routines for scanning signed Long_Long_Integer
--  values for use in Text_IO.Integer_IO, and the Value attribute.

package System.Val_LLI is
pragma Pure (Val_LLI);

   function Scan_Long_Long_Integer
     (Str  : String;
      Ptr  : access Integer;
      Max  : Integer)
      return Long_Long_Integer;
   --  This function scans the string starting at Str (Ptr.all) for a valid
   --  integer according to the syntax described in (RM 3.5(43)). The substring
   --  scanned extends no further than Str (Max). There are three cases for the
   --  return:
   --
   --  If a valid integer is found after scanning past any initial spaces, then
   --  Ptr.all is updated past the last character of the integer (but trailing
   --  spaces are not scanned out).
   --
   --  If no valid integer is found, then Ptr.all points either to an initial
   --  non-digit character, or to Max + 1 if the field is all spaces and the
   --  exception Constraint_Error is raised.
   --
   --  If a syntactically valid integer is scanned, but the value is out of
   --  range, or, in the based case, the base value is out of range or there
   --  is an out of range digit, then Ptr.all points past the integer, and
   --  Constraint_Error is raised.
   --
   --  Note: these rules correspond to the requirements for leaving the pointer
   --  positioned in Text_Io.Get
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case.

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer;
   --  Used in computing X'Value (Str) where X is a signed integer type whose
   --  base range exceeds the base range of Integer. Str is the string argument
   --  of the attribute. Constraint_Error is raised if the string is malformed,
   --  or if the value is out of range.

end System.Val_LLI;
