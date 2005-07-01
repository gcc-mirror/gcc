------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
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

--  This child package contains the routines used to actually load a project
--  file and create entries in the source file table. It also contains two
--  routines to save and restore a project scan context.

with Scans; use Scans;
with Types; use Types;

package Sinput.P is

   function Load_Project_File (Path : String) return Source_File_Index;
   --  Load into memory the source of a project source file.
   --  Initialize the Scans state.

   function Source_File_Is_Subunit (X : Source_File_Index) return Boolean;
   --  This function determines if a source file represents a subunit. It
   --  works by scanning for the first compilation unit token, and returning
   --  True if it is the token SEPARATE. It will return False otherwise,
   --  meaning that the file cannot possibly be a legal subunit. This
   --  function does NOT do a complete parse of the file, or build a
   --  tree. It is used in gnatmake to decide if a body without a spec
   --  in a project file needs to be compiled or not.

   type Saved_Project_Scan_State is limited private;
   --  Used to save project scan state in following two routines

   procedure Save_Project_Scan_State
     (Saved_State : out Saved_Project_Scan_State);
   pragma Inline (Save_Project_Scan_State);
   --  Save the Scans state, as well as the values of
   --  Source and Current_Source_File.

   procedure Restore_Project_Scan_State
     (Saved_State : Saved_Project_Scan_State);
   pragma Inline (Restore_Project_Scan_State);
   --  Restore the Scans state and the values of
   --  Source and Current_Source_File.

private

   type Saved_Project_Scan_State is record
      Scan_State          : Saved_Scan_State;
      Source              : Source_Buffer_Ptr;
      Current_Source_File : Source_File_Index;
   end record;

end Sinput.P;
