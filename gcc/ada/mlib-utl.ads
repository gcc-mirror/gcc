------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . U T L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--              Copyright (C) 2001, Ada Core Technologies, Inc.             --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an easy way of calling various tools such as gcc,
--  ar, etc...

package MLib.Utl is

   procedure Delete_File (Filename : in String);
   --  Delete the file Filename.

   procedure Gcc
     (Output_File : String;
      Objects     : Argument_List;
      Options     : Argument_List;
      Base_File   : String := "");
   --  Invoke gcc to create a library.

   procedure Ar
     (Output_File : String;
      Objects     : Argument_List);
   --  Run ar to move all the binaries inside the archive.
   --  If ranlib is on the path, run it also.

   function Lib_Directory return String;
   --  Return the directory containing libgnat.

end MLib.Utl;
