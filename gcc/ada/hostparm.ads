------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             H O S T P A R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package defines some system dependent parameters for GNAT. These
--  are parameters that are relevant to the host machine on which the
--  compiler is running, and thus this package is part of the compiler.

with Types;

package Hostparm is

   ---------------------
   -- HOST Parameters --
   ---------------------

   Direct_Separator : constant Character;
   pragma Import (C, Direct_Separator, "__gnat_dir_separator");
   Normalized_CWD : constant String := "." & Direct_Separator;
   --  Normalized string to access current directory

   Max_Line_Length : constant :=
     Types.Column_Number'Pred (Types.Column_Number'Last);
   --  Maximum source line length. By default we set it to the maximum
   --  value that can be supported, which is given by the range of the
   --  Column_Number type. We subtract 1 because need to be able to
   --  have a valid Column_Number equal to Max_Line_Length to represent
   --  the location of a "line too long" error.
   --
   --  200 is the minimum value required (RM 2.2(15)). The value set here
   --  can be reduced by the explicit use of the -gnatyM style switch.

   Max_Name_Length : constant := 1024;
   --  Maximum length of unit name (including all dots, and " (spec)") and
   --  of file names in the library, must be at least Max_Line_Length, but
   --  can be larger.

   Tag_Errors : constant Boolean := True;
   --  If set to true, then brief form error messages will be prefaced by
   --  the string "error:". Used as default for Opt.Unique_Error_Tag. Disabled
   --  by gnatd_U.

   Exclude_Missing_Objects : constant Boolean := True;
   --  If set to true, gnatbind will exclude from consideration all
   --  non-existent .o files.

end Hostparm;
