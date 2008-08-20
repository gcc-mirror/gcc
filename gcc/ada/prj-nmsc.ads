------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2008, Free Software Foundation, Inc.         --
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

--  Check the Naming Scheme of a project file, find the source files

private package Prj.Nmsc is

   --  It would be nicer to have a higher level statement of what these
   --  procedures do (related to their names), rather than just an English
   --  language summary of the implementation ???

   procedure Check
     (Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Report_Error    : Put_Line_Access;
      When_No_Sources : Error_Warning;
      Current_Dir     : String);
   --  Check the object directory and the source directories
   --
   --  Check the library attributes, including the library directory if any
   --
   --  Get the set of specification and implementation suffixes, if any
   --
   --  Check the naming scheme for Ada
   --
   --  Find the Ada source files if any
   --
   --  Check the naming scheme for the supported languages (c, c++, ...) other
   --  than Ada. Find the source files if any.
   --
   --  If Report_Error is null , use the standard error reporting mechanism
   --  (Errout). Otherwise, report errors using Report_Error.
   --
   --  Current_Dir is for optimization purposes only, avoiding system calls.
   --
   --  When_No_Sources indicates what should be done when no sources of a
   --  language are found in a project where this language is declared.

end Prj.Nmsc;
