------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P A R T                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2000-2001 Free Software Foundation, Inc.          --
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
--
--  Implements the parsing of project files into a tree.

with Prj.Tree;  use Prj.Tree;

package Prj.Part is

   procedure Parse
     (Project                : out Project_Node_Id;
      Project_File_Name      : String;
      Always_Errout_Finalize : Boolean);
   --  Parse a project file and all its imported project files
   --  and create a tree.
   --  Return the node for the project (or Empty_Node if parsing failed).
   --  If Always_Errout_Finalize is True, Errout.Finalize is called
   --  in all cases; otherwise, Errout.Finalize is only called if there are
   --  errors (but not if there are only warnings).

end Prj.Part;
