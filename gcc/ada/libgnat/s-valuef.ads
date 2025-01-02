------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ F                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2025, Free Software Foundation, Inc.       --
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

--  This package contains the routines for supporting the Value attribute for
--  ordinary fixed point types whose Small is the ratio of two Int values, and
--  also for conversion operations required in Text_IO.Fixed_IO for such types.

generic

   type Int is range <>;

   type Uns is mod <>;

   with procedure Scaled_Divide
          (X, Y, Z : Int;
           Q, R : out Int;
           Round : Boolean);

package System.Value_F is
   pragma Preelaborate;

   function Scan_Fixed
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Num : Int;
      Den : Int) return Int;
   --  This function scans the string starting at Str (Ptr.all) for a valid
   --  real literal according to the syntax described in (RM 3.5(43)). The
   --  substring scanned extends no further than Str (Max). There are three
   --  cases for the return:
   --
   --  If a valid real literal is found after scanning past any initial spaces,
   --  then Ptr.all is updated past the last character of the literal (but
   --  trailing spaces are not scanned out). The value returned is the value
   --  Int'Integer_Value (decimal-literal-value), using the given Num/Den to
   --  determine this value.
   --
   --  If no valid real literal is found, then Ptr.all points either to an
   --  initial non-digit character, or to Max + 1 if the field is all spaces
   --  and the exception Constraint_Error is raised.
   --
   --  If a syntactically valid integer is scanned, but the value is out of
   --  range, or, in the based case, the base value is out of range or there
   --  is an out of range digit, then Ptr.all points past the integer, and
   --  Constraint_Error is raised.
   --
   --  Note: these rules correspond to the requirements for leaving the
   --  pointer positioned in Text_Io.Get
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case.

   function Value_Fixed
     (Str : String;
      Num : Int;
      Den : Int) return Int;
   --  Used in computing X'Value (Str) where X is an ordinary fixed-point type.
   --  Str is the string argument of the attribute. Constraint_Error is raised
   --  if the string is malformed or if the value is out of range of Int (not
   --  the range of the fixed-point type, which must be done by the caller).
   --  Otherwise the value returned is the value Int'Integer_Value
   --  (decimal-literal-value), using Small Num/Den to determine this value.

end System.Value_F;
