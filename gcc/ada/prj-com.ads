------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . C O M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2005 Free Software Foundation, Inc.          --
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

--  The following package declares data types for GNAT project.
--  These data types are used in the bodies of the Prj hierarchy.

with Osint;

package Prj.Com is

   type Fail_Proc is access procedure
     (S1 : String;
      S2 : String := "";
      S3 : String := "");

   Fail : Fail_Proc := Osint.Fail'Access;
   --  This procedure is used in the project facility, instead of directly
   --  calling Osint.Fail. It may be specified by tools to do clean up before
   --  calling Osint.Fail, or to simply report an error and return.

end Prj.Com;
