------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              X R E F _ L I B                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--       Copyright (C) 1998-1999 Free Software Foundation, Inc.             --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.   --
--                                                                          --
------------------------------------------------------------------------------

with Hostparm;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Dynamic_Tables;

with Xr_Tabls;                  use Xr_Tabls;
with GNAT.Regexp;               use GNAT.Regexp;

--  Misc. utilities for the cross-referencing tool

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

   procedure Add_File (File : String);
   --  Add a new file in the list of files to search for references.
   --  File is considered to be a globbing regular expression, which is thus
   --  expanded

   Invalid_Argument : exception;
   --  Exception raised when there is a syntax error in the command line

   function Match
     (Pattern : Search_Pattern;
      Symbol  : String)
      return    Boolean;
   --  Returns true if Symbol matches one of the entities in the command line

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
   --  Returns the default Project file name

   procedure Search
     (Pattern       : Search_Pattern;
      Local_Symbols : Boolean;
      Wide_Search   : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean;
      Type_Tree     : Boolean);
   --  Search every ali file (following the Readdir rule above), for
   --  each line matching Pattern, and executes Process on these
   --  lines. If World is True, Search will look into every .ali file
   --  in the object search path. If Read_Only is True, we parse the
   --  read-only ali files too. If Der_Mode is true then the derived type
   --  information will be processed. If Type_Tree is true then the type
   --  hierarchy will be search going from pattern to the parent type

   procedure Search_Xref
     (Local_Symbols : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean);
   --  Search every ali file given in the command line and all their
   --  dependencies. If Read_Only is True, we parse the read-only ali
   --  files too. If Der_Mode is true then the derived type information will
   --  be processed

   ---------------
   -- ALI files --
   ---------------

   function Current_Xref_File
     (File : ALI_File)
      return Xr_Tabls.File_Reference;
   --  Returns the name of the file in which the last identifier
   --  is declared

   function File_Name
     (File : ALI_File;
      Num  : Positive)
      return Xr_Tabls.File_Reference;
   --  Returns the dependency file name number Num

   function Get_Full_Type (Abbrev : Character) return String;
   --  Returns the full type corresponding to a type letter as found in
   --  the .ali files.

   procedure Open
     (Name         : in  String;
      File         : out ALI_File;
      Dependencies : in  Boolean := False);
   --  Open a new ALI file
   --  if Dependencies is True, the insert every library file 'with'ed in
   --  the files database (used for gnatxref)


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

      Initialized : Boolean := False;
      --  Set to True when Entity has been initialized.
   end record;
   --  Stores all the pattern that are search for.
end Xref_Lib;
