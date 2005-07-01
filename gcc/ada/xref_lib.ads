------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              X R E F _ L I B                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 1998-2002 Free Software Foundation, Inc.             --
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

--  Miscellaneous utilities for the cross-referencing tool

with Hostparm;
with Xr_Tabls;  use Xr_Tabls;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Dynamic_Tables;
with GNAT.Regexp;               use GNAT.Regexp;

package Xref_Lib is

   subtype File_Name_String is String (1 .. Hostparm.Max_Name_Length);
   subtype Line_String      is String (1 .. Hostparm.Max_Line_Length);

   type ALI_File is limited private;

   ---------------------
   -- Directory Input --
   ---------------------

   type Rec_DIR is limited private;
   --  This one is used for recursive search of .ali files

   procedure Find_ALI_Files;
   --  Find all the ali files that we will have to parse, and have them to
   --  the file list

   ---------------------
   -- Search patterns --
   ---------------------

   type Search_Pattern is private;
   type Search_Pattern_Ptr is access all Search_Pattern;

   procedure Add_Entity
     (Pattern : in out Search_Pattern;
      Entity  : String;
      Glob    : Boolean := False);
   --  Add a new entity to the search pattern (the entity should have the
   --  form pattern[:file[:line[:column]]], and it is parsed entirely in
   --  this procedure. Glob indicates if we should use the 'globbing
   --  patterns' (True) or the full regular expressions (False)

   procedure Add_Xref_File (File : String);
   --  Add a new file in the list of files to search for references. File
   --  is interpreted as a globbing regular expression, which is expanded.

   Invalid_Argument : exception;
   --  Exception raised when there is a syntax error in the command line

   -----------------------
   -- Output Algorithms --
   -----------------------

   procedure Print_Gnatfind
     (References     : in Boolean;
      Full_Path_Name : in Boolean);
   procedure Print_Unused (Full_Path_Name : in Boolean);
   procedure Print_Vi (Full_Path_Name : in Boolean);
   procedure Print_Xref (Full_Path_Name : in Boolean);
   --  The actual print procedures. These functions step through the symbol
   --  table and print all the symbols if they match the files given on the
   --  command line (they already match the entities if they are in the
   --  symbol table)

   ------------------------
   -- General Algorithms --
   ------------------------

   function Default_Project_File (Dir_Name : in String) return String;
   --  Returns the default Project file name for the directory Dir_Name.

   procedure Search
     (Pattern       : Search_Pattern;
      Local_Symbols : Boolean;
      Wide_Search   : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean;
      Type_Tree     : Boolean);
   --  Search every ALI file for entities matching Pattern, and add
   --  these entities to the internal symbol tables.
   --
   --  If Wide_Search is True, all ALI files found in the object path
   --  are searched.
   --
   --  If Read_Only is True, read-only ALI files will also be parsed,
   --  similar to gnatmake -a.
   --
   --  If Der_Info is true, then the derived type information will be
   --  processed.
   --
   --  If Type_Tree is true, then the type hierarchy wil be searched
   --  going from the pattern to the parent type.

   procedure Search_Xref
     (Local_Symbols : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean);
   --  Search every ali file given in the command line and all their
   --  dependencies. If Read_Only is True, we parse the read-only ali
   --  files too. If Der_Mode is true then the derived type information will
   --  be processed

private
   type Rec_DIR is limited record
      Dir : GNAT.Directory_Operations.Dir_Type;
   end record;

   package Dependencies_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Xr_Tabls.File_Reference,
      Table_Index_Type     => Positive,
      Table_Low_Bound      => 1,
      Table_Initial        => 400,
      Table_Increment      => 100);
   use Dependencies_Tables;

   type Dependencies is new Dependencies_Tables.Instance;

   type ALI_File is limited record
      Buffer         : String_Access := null;
      --  Buffer used to read the whole file at once

      Current_Line   : Positive;
      --  Start of the current line in Buffer

      Xref_Line      : Positive;
      --  Start of the xref lines in Buffer

      X_File         : Xr_Tabls.File_Reference;
      --  Stores the cross-referencing file-name ("X..." lines), as an
      --  index into the dependencies table

      Dep : Dependencies;
      --  Store file name associated with each number ("D..." lines)
   end record;

   --  The following record type stores all the patterns that are searched for

   type Search_Pattern is record
      Entity : GNAT.Regexp.Regexp;
      --  A regular expression matching the entities we are looking for.
      --  File is a list of the places where the declaration of the entities
      --  has to be. When the user enters a file:line:column on the command
      --  line, it is stored as "Entity_Name Declaration_File:line:column"

      File_Ref : Xr_Tabls.File_Reference;
      --  A reference to the source file, if any.

      Initialized : Boolean := False;
      --  Set to True when Entity has been initialized.
   end record;
   --  Stores all the pattern that are search for.
end Xref_Lib;
