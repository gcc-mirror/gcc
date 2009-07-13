------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2009, Free Software Foundation, Inc.         --
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

--  Perform various checks on a project and find all its source files

with GNAT.Dynamic_HTables;

private package Prj.Nmsc is

   type Tree_Processing_Data is private;
   --  Temporary data which is needed while parsing a project. It does not need
   --  to be kept in memory once a project has been fully loaded, but is
   --  necessary while performing consistency checks (duplicate sources,...)
   --  This data must be initialized before processing any project, and the
   --  same data is used for processing all projects in the tree.

   procedure Initialize
     (Data                       : out Tree_Processing_Data;
      Tree                       : Project_Tree_Ref;
      Report_Error               : Put_Line_Access;
      When_No_Sources            : Error_Warning;
      Require_Sources_Other_Lang : Boolean := True;
      Allow_Duplicate_Basenames  : Boolean := True;
      Compiler_Driver_Mandatory  : Boolean := False);
   --  Initialize Data
   --  If Allow_Duplicate_Basenames, then files with the same base names are
   --  authorized within a project for source-based languages (never for unit
   --  based languages)
   --  If Compiler_Driver_Mandatory is true, then a Compiler.Driver attribute
   --  for each language must be defined, or we will not look for its source
   --  files.
   --  When_No_Sources indicates what should be done when no sources of a
   --  language are found in a project where this language is declared.
   --  If Require_Sources_Other_Lang is true, then all languages must have at
   --  least one source file, or an error is reported via When_No_Sources. If
   --  it is false, this is only required for Ada (and only if it is a language
   --  of the project).
   --  If Report_Error is null, use the standard error reporting mechanism
   --  (Errout). Otherwise, report errors using Report_Error.

   procedure Free (Data : in out Tree_Processing_Data);
   --  Free the memory occupied by Data

   procedure Check
     (Project        : Project_Id;
      Current_Dir    : String;
      Data           : in out Tree_Processing_Data);
   --  Perform consistency and semantic checks on a project, starting from the
   --  project tree parsed from the .gpr file. This procedure interprets the
   --  various case statements in the project based on the current environment
   --  variables (the "scenario"). After checking the validity of the naming
   --  scheme, it searches for all the source files of the project. The result
   --  of this procedure is a filled-in data structure for Project_Id which
   --  contains all the information about the project. This information is only
   --  valid while the scenario variables are preserved. If the current mode
   --  is Ada_Only, this procedure will only search Ada sources, but in multi-
   --  language mode it will look for sources for all supported languages.
   --
   --  Current_Dir is for optimization purposes only, avoiding system calls to
   --  query it.

private

   package Files_Htable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping from base file names to Source_Id (containing full info about
   --  the source)

   type Tree_Processing_Data is record
      Tree                      : Project_Tree_Ref;
      --  The data applies when parsing this tree

      File_To_Source            : Files_Htable.Instance;

      Require_Sources_Other_Lang : Boolean;
      Report_Error               : Put_Line_Access;
      When_No_Sources            : Error_Warning;
      Allow_Duplicate_Basenames  : Boolean := True;
      Compiler_Driver_Mandatory  : Boolean := False;
      --  See comments for Initialize
   end record;
end Prj.Nmsc;
