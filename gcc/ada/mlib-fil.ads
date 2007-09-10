------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . F I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2007, AdaCore                     --
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

--  This package provides a set of routines to deal with file extensions

package MLib.Fil is

   function Ext_To
     (Filename : String;
      New_Ext  : String := "") return String;
   --  Return Filename with the extension changed to New_Ext

   function Append_To
     (Filename : String;
      Ext      : String) return String;
   --  Return Filename with the extension Ext

   function Get_Ext (Filename : String) return String;
   --  Return extension of filename

   function Is_Archive (Filename : String) return Boolean;
   --  Test if filename is an archive

   function Is_C (Filename : String) return Boolean;
   --  Test if Filename is a C file

   function Is_Obj (Filename : String) return Boolean;
   --  Test if Filename is an object file

end MLib.Fil;
