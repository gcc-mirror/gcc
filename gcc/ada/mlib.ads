------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M L I B                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1999-2004, Ada Core Technologies, Inc.           --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the core high level routines used by GNATMLIB
--  and GNATMAKE to build libraries

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Osint;       use Osint;
with Types;       use Types;

package MLib is

   Max_Characters_In_Library_Name : constant := 20;
   --  Maximum number of characters in a library name.
   --  Used by Check_Library_Name below.

   type Fail_Proc is access procedure
     (S1 : String; S2 : String := ""; S3 : String := "");

   Fail : Fail_Proc := Osint.Fail'Access;
   --  This procedure is used in the MLib hierarchy, instead of
   --  directly calling Osint.Fail.
   --  It is redirected to Make.Make_Failed by gnatmake.

   procedure Check_Library_Name (Name : String);
   --  Verify that the name of a library has the following characteristics
   --   - starts with a letter
   --   - includes only letters and digits
   --   - contains not more than Max_Characters_In_Library_Name characters

   procedure Build_Library
     (Ofiles      : Argument_List;
      Afiles      : Argument_List;
      Output_File : String;
      Output_Dir  : String);
   --  Build a static library from a set of object files

   procedure Copy_ALI_Files
     (Files      : Argument_List;
      To         : Name_Id;
      Interfaces : String_List);
   --  Copy all ALI files Files to directory To.
   --  Mark Interfaces ALI files as interfaces, if any.

   function Linker_Library_Path_Option return String_Access;
   --  Linker option to specify to the linker the library directory path.
   --  If non null, the library directory path is to be appended.
   --  Should be deallocated by the caller, when no longer needed.

private

   Preserve : Attribute := Time_Stamps;
   --  Used by Copy_ALI_Files. Changed to None for OpenVMS, because
   --  Copy_Attributes always fails on VMS.

end MLib;
