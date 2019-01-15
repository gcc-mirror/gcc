------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - M                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2019, Free Software Foundation, Inc.         --
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

with Osint;

pragma Elaborate_All (Osint);
--  This pragma is needed because of the call to Set_Program in the
--  elaboration of the package. We cannot rely on the static model
--  of elaboration since the compiler is routinely compiled with
--  checks off (-gnatp), and with older versions of the compiler
--  (up to and including most 5.04 wavefronts), -gnatp suppresses
--  the static elaboration check mechanisms. It could be removed
--  one day, but there really is no need to do so.

package body Osint.M is

   -----------------------
   -- More_Source_Files --
   -----------------------

   function More_Source_Files return Boolean renames More_Files;

   ----------------------
   -- Next_Main_Source --
   ----------------------

   function Next_Main_Source return File_Name_Type renames Next_Main_File;

   ----------------------
   -- Object_File_Name --
   ----------------------

   function Object_File_Name (N : File_Name_Type) return File_Name_Type
     renames Osint.Object_File_Name;

begin
   Set_Program (Make);
end Osint.M;
