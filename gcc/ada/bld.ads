------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  B L D                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--        Copyright (C) 2002 Free Software Foundation, Inc.                 --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  The following package implements the facilities to build Makefiles
--  for multi-language GNAT project files, so that building a complete
--  multi-language system can be done easily, using GNU make.

package Bld is

   procedure Gpr2make;
   --  Parse a project file and all the other project files it depends on
   --  into a project tree; then from the project tree, produce one Makefile
   --  for each project file in the project tree.

end Bld;
