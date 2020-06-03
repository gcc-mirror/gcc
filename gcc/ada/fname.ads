------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package, together with its child package Fname.UF define the
--  association between source file names and unit names as defined
--  (see package Uname for definition of format of unit names).

with Namet; use Namet;

package Fname is

   --  Note: this package spec does not depend on the Uname spec in the Ada
   --  sense, but the comments and description of the semantics do depend on
   --  the conventions established by Uname.

   ---------------------------
   -- File Name Conventions --
   ---------------------------

   --  GNAT requires that there be a one to one correspondence between source
   --  file names (as used in the Osint package interface) and unit names as
   --  defined by the Uname package. This correspondence is defined by the
   --  two subprograms defined here in the Fname package.

   --   For full rules of file naming, see GNAT User's Guide. Note that the
   --   naming rules are affected by the presence of Source_File_Name pragmas
   --   that have been previously processed.

   --  Note that the file name does *not* include the directory name. The
   --  management of directories is provided by Osint, and full file names
   --  are used only for error message purposes within GNAT itself.

   -----------------
   -- Subprograms --
   -----------------

   function Is_Predefined_File_Name
     (Fname              : String;
      Renamings_Included : Boolean := True) return Boolean;
   function Is_Predefined_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean;
   --  These functions determine if the given file name (which must be a simple
   --  file name with no directory information) is the source or ALI file name
   --  for one of the predefined library units (i.e. part of the Ada, System,
   --  or Interface hierarchies). Note that units in the GNAT hierarchy are not
   --  considered predefined (see Is_Internal_File_Name below).
   --
   --  The Renamings_Included parameter indicates whether annex J renamings
   --  such as Text_IO are to be considered as predefined. If
   --  Renamings_Included is True, then Text_IO will return True, otherwise
   --  only children of Ada, Interfaces and System return True.

   function Is_Predefined_Renaming_File_Name
     (Fname : String) return Boolean;
   function Is_Predefined_Renaming_File_Name
     (Fname : File_Name_Type) return Boolean;
   --  True if Fname is the file name for a predefined renaming (the same file
   --  names that are included if Renamings_Included => True is passed to
   --  Is_Predefined_File_Name).

   function Is_Internal_File_Name
     (Fname              : String;
      Renamings_Included : Boolean := True) return Boolean;
   function Is_Internal_File_Name
     (Fname              : File_Name_Type;
      Renamings_Included : Boolean := True) return Boolean;
   --  Same as Is_Predefined_File_Name, except units in the GNAT hierarchy are
   --  included.

   function Is_GNAT_File_Name (Fname : String) return Boolean;
   function Is_GNAT_File_Name (Fname : File_Name_Type) return Boolean;
   --  True for units in the GNAT hierarchy

end Fname;
