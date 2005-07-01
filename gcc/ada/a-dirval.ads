------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . D I R E C T O R I E S . V A L I D I T Y              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004 Free Software Foundation, Inc.               --
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

--  This private child package is used in the body of Ada.Directories.
--  It has several bodies, for different platforms.

private package Ada.Directories.Validity is

   function Is_Valid_Simple_Name (Name : String) return Boolean;
   --  Returns True if Name is a valid file name

   function Is_Valid_Path_Name (Name : String) return Boolean;
   --  Returns True if Name is a valid path name

   function Is_Path_Name_Case_Sensitive return Boolean;
   --  Returns True if file and path names are case-sensitive

end Ada.Directories.Validity;
