------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2000-2004 Free Software Foundation, Inc.       --
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

--  Check the Naming Scheme of a project file, find the directories
--  and the source files.

private package Prj.Nmsc is

   --  It would be nicer to have a higher level statement of what these
   --  procedures do (related to their names), rather than just an english
   --  language summary of the implementation ???

   procedure Ada_Check
     (Project      : Project_Id;
      Report_Error : Put_Line_Access;
      Trusted_Mode : Boolean);
   --  Call Language_Independent_Check.
   --  Check the naming scheme for Ada.
   --  Find the Ada source files if any.
   --  If Report_Error is null , use the standard error reporting mechanism
   --  (Errout). Otherwise, report errors using Report_Error.
   --  If Trusted_Mode is True, it is assumed that the project doesn't contain
   --  any file duplicated through symbolic links (although the latter are
   --  still valid if they point to a file which is outside of the project),
   --  and that no directory has a name which is a valid source name.

   procedure Language_Independent_Check
     (Project      : Project_Id;
      Report_Error : Put_Line_Access);
   --  Check the object directory and the source directories.
   --  Check the library attributes, including the library directory if any.
   --  Get the set of specification and implementation suffixes, if any.
   --  If Report_Error is null , use the standard error reporting mechanism
   --  (Errout). Otherwise, report errors using Report_Error.

end Prj.Nmsc;
