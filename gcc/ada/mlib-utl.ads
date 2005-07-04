------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . U T L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2001-2005, Ada Core Technologies, Inc           --
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

--  This package provides an easy way of calling various tools such as gcc,
--  ar, etc...

package MLib.Utl is

   procedure Delete_File (Filename : in String);
   --  Delete the file Filename
   --  Why is this different from the standard OS_Lib routine???

   procedure Gcc
     (Output_File : String;
      Objects     : Argument_List;
      Options     : Argument_List;
      Options_2   : Argument_List;
      Driver_Name : Name_Id := No_Name);
   --  Driver_Name indicates the "driver" to invoke; by default, the "driver"
   --  is gcc. This procedure invokes the driver to create a shared library.
   --  Options are passed to gcc before the objects, Options_2 after.
   --  Output_File is the name of the library file to create. Objects are the
   --  names of the object files to put in the library.

   procedure Ar
     (Output_File : String;
      Objects     : Argument_List);
   --  Run ar to move all the binaries inside the archive. If ranlib is on the
   --  path, run it also. Output_File is the path name of the archive to
   --  create. Objects is the list of the path names of the object files to be
   --  put in the archive.

   function Lib_Directory return String;
   --  Return the directory containing libgnat

end MLib.Utl;
