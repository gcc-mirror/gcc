------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G N A T C L E A N                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003, Free Software Foundation, Inc.              --
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

--  Gnatclean is a utility to delete files produced by the GNAT tools:
--  ALI files, object files, tree files, expanded source files, library
--  files, interface copy files, binder generated files and executable files.

--  Gnatclean may be invoked for one or several executables, for a project
--  file or a tree of project files with the optional specification of
--  one of several executables.

with Clean;

procedure Gnatclean is
begin
   --  The real work is done in Package Clean

   Clean.Gnatclean;
end Gnatclean;
