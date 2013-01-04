------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  M L I B . T G T . V M S _ C O M M O N                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--        Copyright (C) 2007-2011, Free Software Foundation, Inc.           --
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

--  This is the part of MLib.Tgt.Specific common to both VMS versions

package MLib.Tgt.VMS_Common is
   pragma Elaborate_Body;

   function Init_Proc_Name (Library_Name : String) return String;
   --  Returns, in upper case, Library_Name & "INIT", except when Library_Name
   --  is "ada" (case insensitive), returns "ADA_INIT".

end MLib.Tgt.VMS_Common;
