------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . U T L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2008, AdaCore                     --
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

--  This package provides an easy way of calling various tools such as gcc,
--  ar, etc...

package MLib.Utl is

   procedure Delete_File (Filename : String);
   --  Delete the file Filename and output the name of the deleted file in
   --  verbose mode.

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
   --  Run ar to move all the binaries inside the archive. If ranlib is on
   --  the path, run it also. Output_File is the path name of the archive to
   --  create. Objects is the list of the path names of the object files to be
   --  put in the archive. This procedure currently assumes that it is always
   --  called in the context of gnatmake. If other executables start using this
   --  procedure, an additional parameter would need to be added, and calls to
   --  Osint.Program_Name updated accordingly in the body.

   function Lib_Directory return String;
   --  Return the directory containing libgnat

   procedure Specify_Adalib_Dir (Path : String);
   --  Specify the path of the GNAT adalib directory, to be returned by
   --  function Lib_Directory without looking for it. This is used only in
   --  gprlib, because we cannot rely on the search in Lib_Directory, as the
   --  GNAT version may be different for gprbuild/gprlib and the compiler.

end MLib.Utl;
