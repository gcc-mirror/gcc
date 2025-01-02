------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains utility routines for the binder

with Namet; use Namet;
with Types; use Types;

package Butil is

   function Is_Predefined_Unit return Boolean;
   --  Given a unit name stored in Name_Buffer with length in Name_Len,
   --  returns True if this is the name of a predefined unit or a child of
   --  a predefined unit (including the obsolescent renamings). This is used
   --  in the preference selection (see Better_Choice in body of Binde).

   function Is_Internal_Unit return Boolean;
   --  Given a unit name stored in Name_Buffer with length in Name_Len,
   --  returns True if this is the name of an internal unit or a child of
   --  an internal unit. Similar in usage to Is_Predefined_Unit.

   --  Note: the following functions duplicate functionality in Uname, but
   --  we want to avoid bringing Uname into the binder since it generates
   --  to many unnecessary dependencies, and makes the binder too large.

   function Uname_Less (U1, U2 : Unit_Name_Type) return Boolean;
   --  Determines if the unit name U1 is alphabetically before U2

   procedure Write_Unit_Name (U : Unit_Name_Type);
   --  Output unit name with (body) or (spec) after as required. On return
   --  Name_Len is set to the number of characters which were output.

   ---------------
   -- Iterators --
   ---------------

   --  The following type represents an iterator over all units that are
   --  specified in the forced-elaboration-order file supplied by the binder
   --  via switch -f.

   type Forced_Units_Iterator is private;

   function Has_Next (Iter : Forced_Units_Iterator) return Boolean;
   pragma Inline (Has_Next);
   --  Determine whether iterator Iter has more units to examine

   function Iterate_Forced_Units return Forced_Units_Iterator;
   pragma Inline (Iterate_Forced_Units);
   --  Obtain an iterator over all units in the forced-elaboration-order file

   procedure Next
     (Iter      : in out Forced_Units_Iterator;
      Unit_Name : out Unit_Name_Type;
      Unit_Line : out Logical_Line_Number);
   pragma Inline (Next);
   --  Return the current unit referenced by iterator Iter along with the
   --  line number it appears on, and advance to the next available unit.

private
   First_Line_Number : constant Logical_Line_Number := No_Line_Number + 1;

   type Forced_Units_Iterator is record
      Order : String_Ptr := null;
      --  A reference to the contents of the forced-elaboration-order file,
      --  read in as a string.

      Order_Index : Positive := 1;
      --  Index into the order string

      Order_Line : Logical_Line_Number := First_Line_Number;
      --  Logical line number within the order string

      Unit_Line : Logical_Line_Number := No_Line_Number;
      --  The logical line number of the current unit name within the order
      --  string.

      Unit_Name : Unit_Name_Type := No_Unit_Name;
      --  The current unit name parsed from the order string
   end record;

end Butil;
