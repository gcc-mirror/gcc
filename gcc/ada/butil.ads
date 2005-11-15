------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Butil is

--  This package contains utility routines for the binder

   function Is_Predefined_Unit return Boolean;
   --  Given a unit name stored in Name_Buffer with length in Name_Len,
   --  returns True if this is the name of a predefined unit or a child of
   --  a predefined unit (including the obsolescent renamings). This is used
   --  in the preference selection (see Better_Choice in body of Binde).

   function Is_Internal_Unit return Boolean;
   --  Given a unit name stored in Name_Buffer with length in Name_Len,
   --  returns True if this is the name of an internal unit or a child of
   --  an internal. Similar in usage to Is_Predefined_Unit.

   --  Note: the following functions duplicate functionality in Uname, but
   --  we want to avoid bringing Uname into the binder since it generates
   --  to many unnecessary dependencies, and makes the binder too large.

   function Uname_Less (U1, U2 : Unit_Name_Type) return Boolean;
   --  Determines if the unit name U1 is alphabetically before U2

   procedure Write_Unit_Name (U : Unit_Name_Type);
   --  Output unit name with (body) or (spec) after as required. On return
   --  Name_Len is set to the number of characters which were output.

end Butil;
