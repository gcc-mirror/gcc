------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             N A M E T - S P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This child package contains a spell checker for Name_Id values. It is
--  separated off as a child package, because of the extra dependencies,
--  in particular on GNAT.UTF_32_Spelling_Checker. There are a number of
--  packages that use Namet that do not need the spell checking feature,
--  and this separation helps in dealing with older versions of GNAT.

package Namet.Sp is

   function Aspect_Spell_Check (Name : Name_Id) return Boolean;
   --  Returns True, if Name is a misspelling of some aspect name

   function Aspect_Spell_Check (Name : Name_Id) return Name_Id;
   --  Returns a possible correction, if Name is a misspelling of some aspect
   --  name. If not, return No_Name.

   function Attribute_Spell_Check (N : Name_Id) return Boolean;
   --  Returns True, if Name is a misspelling of some attribute name

   function Attribute_Spell_Check (N : Name_Id) return Name_Id;
   --  Returns a possible correction, if Name is a misspelling of some
   --  attribute name. If not, return No_Name.

   function Is_Bad_Spelling_Of (Found, Expect : Name_Id) return Boolean;
   --  Compares two identifier names from the names table, and returns True if
   --  Found is a plausible misspelling of Expect. This function properly deals
   --  with wide and wide wide character encodings in the input names. Note
   --  that an exact match in the names results in False being returned.

end Namet.Sp;
