------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T C M D                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1996 Free Software Foundation, Inc.             --
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

--  This program provides a simple command interface for using GNAT and its
--  associated utilities. The format of switches accepted is intended to
--  be more familiar in style for VMS and DOS users than the standard Unix
--  style switches that are accepted directly.

--    The program is typically called GNAT when it is installed and
--    the two possibile styles of use are:

--  To call gcc:

--    GNAT filename switches

--  To call the tool gnatxxx

--    GNAT xxx filename switches

--  where xxx is the command name (e.g. MAKE for gnatmake). This command name
--  can be abbreviated by giving a prefix (e.g. GNAT MAK) as long as it
--  remains unique.

--  In both cases, filename is in the format appropriate to the operating
--  system in use. The individual commands give more details. In some cases
--  a unit name may be given in place of a file name.

--  The switches start with a slash. Switch names can also be abbreviated
--  where no ambiguity arises. The switches associated with each command
--  are specified by the tables that can be found in the body.

--  Although by convention we use upper case for command names and switches
--  in the documentation, all command and switch names are case insensitive
--  and may be given in upper case or lower case or a mixture.

procedure GNATCmd;
