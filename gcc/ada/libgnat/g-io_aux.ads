------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . I O _ A U X                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2019, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Auxiliary functions or use with Text_IO

--  This package provides some auxiliary functions for use with Text_IO,
--  including a test for an existing file, and a Get_Line function which
--  returns a string.

with Ada.Text_IO;

package GNAT.IO_Aux is

   function File_Exists (Name : String) return Boolean;
   --  Test for existence of a file named Name

   function Get_Line return String;
   --  Read Ada.Text_IO.Current_Input and return string that includes all
   --  characters from the current character up to the end of the line,
   --  with no limit on its length. Raises Ada.IO_Exceptions.End_Error if
   --  at end of file.

   function Get_Line (File : Ada.Text_IO.File_Type) return String;
   --  Same, but reads from specified file

end GNAT.IO_Aux;
