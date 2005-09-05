------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . M A K R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2001-2005 Free Software Foundation, Inc.       --
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

--  Support for procedure Gnatname.

--  For arbitrary naming schemes, create or update a project file,
--  or create a configuration pragmas file.

package Prj.Makr is

   procedure Make
     (File_Path         : String;
      Project_File      : Boolean;
      Directories       : Argument_List;
      Name_Patterns     : Argument_List;
      Excluded_Patterns : Argument_List;
      Foreign_Patterns  : Argument_List;
      Preproc_Switches  : Argument_List;
      Very_Verbose      : Boolean);
   --  Create a project file or a configuration pragmas file
   --
   --  Project_File is the path name of the project file. If the project
   --  file already exists parse it and keep all the elements that are not
   --  automatically generated.
   --
   --  Directory_List_File is the path name of a text file that
   --  contains on each non empty line the path names of the source
   --  directories for the project file. The source directories
   --  are relative to the directory of the project file.
   --
   --  File_Name_Patterns is a GNAT.Regexp string pattern such as
   --  ".*\.ads|.*\.adb" or any other pattern.
   --
   --  A project file (without any sources) is automatically generated
   --  with the name <project>_naming. It contains a package Naming with
   --  all the specs and bodies for the project.
   --  A file containing the source file names is automatically
   --  generated and used as the Source_File_List for the project file.
   --  It includes all sources that follow the Foreign_Patterns (except those
   --  that follow Excluded_Patterns).

   --  Preproc_switches is a list of optional preprocessor switches -gnatep=
   --  and -gnateD that are used when invoking the compiler to find the
   --  unit name and kind.

end Prj.Makr;
