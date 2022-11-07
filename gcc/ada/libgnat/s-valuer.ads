------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2022, Free Software Foundation, Inc.       --
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

--  This package contains routines for scanning real values for use in
--  Text_IO.Decimal_IO, Fixed_IO, Float_IO and the Value attribute.

with System.Unsigned_Types; use System.Unsigned_Types;

generic

   type Uns is mod <>;
   --  Modular type used for the value

   Parts : Positive;
   --  Number of Uns parts in the value

   Precision_Limit : Uns;
   --  Precision limit for each part of the value

   Round : Boolean;
   --  If Parts = 1, True if the extra digit must be rounded

package System.Value_R is
   pragma Preelaborate;

   subtype Data_Index is Positive range 1 .. Parts;
   --  The type indexing the value

   type Scale_Array is array (Data_Index) of Integer;
   --  The scale for each part of the value

   type Value_Array is array (Data_Index) of Uns;
   --  The value split into parts

   function Scan_Raw_Real
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Base  : out Unsigned;
      Scale : out Scale_Array;
      Extra : out Unsigned;
      Minus : out Boolean) return Value_Array;
   --  This function scans the string starting at Str (Ptr.all) for a valid
   --  real literal according to the syntax described in (RM 3.5(43)). The
   --  substring scanned extends no further than Str (Max). There are three
   --  cases for the return:
   --
   --  If a valid real is found after scanning past any initial spaces, then
   --  Ptr.all is updated past the last character of the real (but trailing
   --  spaces are not scanned out) and the Base, Scale, Extra and Minus out
   --  parameters are set; if Val is the result of the call, then the real
   --  represented by the literal is equal to
   --
   --    (Val (1) * Base + Extra) * (Base ** (Scale (1) - 1))
   --
   --  when Parts = 1 and
   --
   --    Sum [Val (N) * (Base ** Scale (N)), N in 1 .. Parts]
   --
   --  when Parts > 1, with the negative sign if Minus is true.
   --
   --  If no valid real is found, then Ptr.all points either to an initial
   --  non-blank character, or to Max + 1 if the field is all spaces and the
   --  exception Constraint_Error is raised.
   --
   --  If a syntactically valid real is scanned, but the value is out of
   --  range, or, in the based case, the base value is out of range or there
   --  is an out of range digit, then Ptr.all points past the real literal,
   --  and Constraint_Error is raised.
   --
   --  Note: these rules correspond to the requirements for leaving the
   --  pointer positioned in Text_Io.Get
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case.
   --
   --  Note: this routine should not be called with Str'Last = Positive'Last.
   --  If this occurs Program_Error is raised with a message noting that this
   --  case is not supported. Most such cases are eliminated by the caller.

   function Value_Raw_Real
     (Str   : String;
      Base  : out Unsigned;
      Scale : out Scale_Array;
      Extra : out Unsigned;
      Minus : out Boolean) return Value_Array;
   --  Used in computing X'Value (Str) where X is a real type. Str is the
   --  string argument of the attribute. Constraint_Error is raised if the
   --  string is malformed.

end System.Value_R;
