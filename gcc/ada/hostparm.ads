------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             H O S T P A R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

--  This package defines some system dependent parameters for GNAT. These
--  are parameters that are relevant to the host machine on which the
--  compiler is running, and thus this package is part of the compiler.

with Types;

package Hostparm is

   -----------------------
   -- TARGET Parameters --
   -----------------------

   --  ??? The following should really be moved to a Target package

   Java_VM : constant Boolean := False;
   --  Set true when compiling the JGNAT tool chain (compiler, gnatmake, etc)

   ---------------------
   -- HOST Parameters --
   ---------------------

   Gnat_VMSp : Integer;
   pragma Import (C, Gnat_VMSp, "__gnat_vmsp");

   OpenVMS : Boolean := Gnat_VMSp /= 0;
   --  Set True for OpenVMS host. See also OpenVMS target boolean in
   --  5vsystem.ads and OpenVMS_On_Target boolean in Targparm. This is
   --  not a constant, because it can be modified by -gnatdm.

   Normalized_CWD : constant String := "./";
   --  Normalized string to access current directory

   Max_Line_Length : constant := Types.Column_Number'Pred
                       (Types.Column_Number'Last);
   --  Maximum source line length. By default we set it to the maximum
   --  value that can be supported, which is given by the range of the
   --  Column_Number type. We subtract 1 because need to be able to
   --  have a valid Column_Number equal to Max_Line_Length to represent
   --  the location of a "line too long" error.
   --  200 is the minimum value required (RM 2.2(15)). The value set here
   --  can be reduced by the explicit use of the -gnatyM style switch.

   Max_Name_Length : constant := 1024;
   --  Maximum length of unit name (including all dots, and " (spec)") and
   --  of file names in the library, must be at least Max_Line_Length, but
   --  can be larger.

   Max_Instantiations : constant := 4000;
   --  Maximum number of instantiations permitted (to stop runaway cases
   --  of nested instantiations). These situations probably only occur in
   --  specially concocted test cases.

   Tag_Errors : constant Boolean := False;
   --  If set to true, then brief form error messages will be prefaced by
   --  the string "error:". Used as default for Opt.Unique_Error_Tag.

   Exclude_Missing_Objects : constant Boolean := True;
   --  If set to true, gnatbind will exclude from consideration all
   --  non-existent .o files.

   Max_Debug_Name_Length : constant := 256;
   --  If a generated qualified debug name exceeds this length, then it
   --  is automatically compressed, regardless of the setting of the
   --  Compress_Debug_Names switch controlled by -gnatC.

end Hostparm;
