------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

--  This child package contains the routines used to actually load a source
--  file and create entries in the source file table. It also contains the
--  routines to create virtual entries for instantiations. This is separated
--  off into a child package to avoid a dependence of Sinput on Osint which
--  would cause trouble in the tree read/write routines.

with Types; use Types;

package Sinput.L is

   -------------------------------------------
   --  Subprograms for Loading Source Files --
   -------------------------------------------

   function Load_Source_File (N : File_Name_Type) return Source_File_Index;
   --  Given a source file name, returns the index of the corresponding entry
   --  in the source file table. If the file is not currently loaded, then
   --  this is the call that causes the source file to be read and an entry
   --  made in the table. A new entry in the table has the file name and time
   --  stamp entries set and the Casing entries set to Unknown. Version is set
   --  to all blanks, and the lines table is initialized but only the first
   --  entry is set (and Last_Line is set to 1). If the given source file
   --  cannot be opened, then the value returned is No_Source_File.

   function Load_Config_File (N : File_Name_Type) return Source_File_Index;
   --  Similar to Load_Source_File, except that the file name is always
   --  interpreted in the context of the current working directory.

   procedure Complete_Source_File_Entry;
   --  Called on completing the parsing of a source file. This call completes
   --  the source file table entry for the current source file.

   function Source_File_Is_Subunit (X : Source_File_Index) return Boolean;
   --  This function determines if a source file represents a subunit. It
   --  works by scanning for the first compilation unit token, and returning
   --  True if it is the token SEPARATE. It will return False otherwise,
   --  meaning that the file cannot possibly be a legal subunit. This
   --  function does NOT do a complete parse of the file, or build a
   --  tree. It is used in the main driver in the check for bad bodies.

   -------------------------------------------------
   -- Subprograms for Dealing With Instantiations --
   -------------------------------------------------

   type Sloc_Adjustment is private;
   --  Type returned by Create_Instantiation_Source for use in subsequent
   --  calls to Adjust_Instantiation_Sloc.

   procedure Create_Instantiation_Source
     (Inst_Node   : Entity_Id;
      Template_Id : Entity_Id;
      A           : out Sloc_Adjustment);
   --  This procedure creates the source table entry for an instantiation.
   --  Inst_Node is the instantiation node, and Template_Id is the defining
   --  identifier of the generic declaration or body unit as appropriate.
   --  A is set to an adjustment factor to be used in subsequent calls to
   --  Adjust_Instantiation_Sloc.

   procedure Adjust_Instantiation_Sloc (N : Node_Id; A : Sloc_Adjustment);
   --  The instantiation tree is created by copying the tree of the generic
   --  template (including the original Sloc values), and then applying
   --  Adjust_Instantiation_Sloc to each copied node to adjust the Sloc
   --  to reference the source entry for the instantiation.

   ------------------------------------------------
   -- Subprograms for Writing Debug Source Files --
   ------------------------------------------------

   procedure Create_Debug_Source
     (Source : Source_File_Index;
      Loc    : out Source_Ptr);
   --  Given a source file, creates a new source file table entry to be used
   --  for the debug source file output (Debug_Generated_Code switch set).
   --  Loc is set to the initial Sloc value for the first line. This call
   --  also creates the debug source output file (using Create_Debug_File).

   procedure Write_Debug_Line (Str : String; Loc : in out Source_Ptr);
   --  This procedure is called to write a line to the debug source file
   --  previously created by Create_Debug_Source using Write_Debug_Info.
   --  Str is the source line to be written to the file (it does not include
   --  an end of line character). On entry Loc is the Sloc value previously
   --  returned by Create_Debug_Source or Write_Debug_Line, and on exit,
   --  Sloc is updated to point to the start of the next line to be written,
   --  taking into account the length of the ternminator that was written by
   --  Write_Debug_Info.

   procedure Close_Debug_Source;
   --  This procedure completes the source table entry for the debug file
   --  previously created by Create_Debug_Source, and written using the
   --  Write_Debug_Line procedure. It then calls Close_Debug_File to
   --  complete the writing of the file itself.

private

   type Sloc_Adjustment is record
      Adjust : Source_Ptr;
      --  Adjustment factor. To be added to source location values in the
      --  source table entry for the template to get corresponding sloc
      --  values for the instantiation image of the template. This is not
      --  really a Source_Ptr value, but rather an offset, but it is more
      --  convenient to represent it as a Source_Ptr value and this is a
      --  private type anyway.

      Lo, Hi : Source_Ptr;
      --  Lo and hi values to which adjustment factor can legitimately
      --  be applied, used to ensure that no incorrect adjustments are
      --  made. Really it is a bug if anyone ever tries to adjust outside
      --  this range, but since we are only doing this anyway for getting
      --  better error messages, it is not critical

   end record;

end Sinput.L;
