------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R  _ T A B L S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2014, Free Software Foundation, Inc.         --
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

--  We need comment here saying what this package is???

with GNAT.OS_Lib;

package Xr_Tabls is

   -------------------
   -- Project files --
   -------------------

   function ALI_File_Name (Ada_File_Name : String) return String;
   --  Returns the ali file name corresponding to Ada_File_Name

   procedure Create_Project_File (Name : String);
   --  Open and parse a new project file. If the file Name could not be
   --  opened or is not a valid project file, then a project file associated
   --  with the standard default directories is returned

   function Next_Obj_Dir return String;
   --  Returns the next directory to visit to find related ali files
   --  If there are no more such directories, returns a null string.

   function Current_Obj_Dir return String;
   --  Returns the obj_dir which was returned by the last Next_Obj_Dir call

   procedure Reset_Obj_Dir;
   --  Reset the iterator for Obj_Dir

   ------------
   -- Tables --
   ------------

   type Declaration_Reference is private;
   Empty_Declaration : constant Declaration_Reference;

   type Declaration_Array is array (Natural range <>) of Declaration_Reference;
   type Declaration_Array_Access is access Declaration_Array;

   type File_Reference is private;
   Empty_File : constant File_Reference;

   type Reference is private;
   Empty_Reference : constant Reference;

   type Reference_Array is array (Natural range <>) of Reference;
   type Reference_Array_Access is access Reference_Array;

   procedure Free (Arr : in out Reference_Array_Access);

   function Add_Declaration
     (File_Ref     : File_Reference;
      Symbol       : String;
      Line         : Natural;
      Column       : Natural;
      Decl_Type    : Character;
      Is_Parameter : Boolean := False;
      Remove_Only  : Boolean := False;
      Symbol_Match : Boolean := True)
      return         Declaration_Reference;
   --  Add a new declaration in the table and return the index to it. Decl_Type
   --  is the type of the entity Any previous instance of this entity in the
   --  htable is removed. If Remove_Only is True, then any previous instance is
   --  removed, but the new entity is never inserted. Symbol_Match should be
   --  set to False if the name of the symbol doesn't match the pattern from
   --  the command line. In that case, the entity will not be output by
   --  gnatfind. If Symbol_Match is True, the entity will only be output if
   --  the file name itself matches. Is_Parameter should be set to True if
   --  the entity is known to be a subprogram parameter.

   procedure Add_Parent
     (Declaration : in out Declaration_Reference;
      Symbol      : String;
      Line        : Natural;
      Column      : Natural;
      File_Ref    : File_Reference);
   --  The parent declaration (Symbol in file File_Ref at position Line and
   --  Column) information is added to Declaration.

   function Add_To_Xref_File
     (File_Name       : String;
      Visited         : Boolean := True;
      Emit_Warning    : Boolean := False;
      Gnatchop_File   : String  := "";
      Gnatchop_Offset : Integer := 0)
      return            File_Reference;
   --  Add a new reference to a file in the table. Ref is used to return the
   --  index in the table where this file is stored. Visited is the value which
   --  will be used in the table (if True, the file will not be returned by
   --  Next_Unvisited_File). If Emit_Warning is True and the ali file does
   --  not exist or does not have cross-referencing information, then a
   --  warning will be emitted. Gnatchop_File is the name of the file that
   --  File_Name was extracted from through a call to "gnatchop -r" (using
   --  pragma Source_Reference). Gnatchop_Offset should be the index of the
   --  first line of File_Name within the Gnatchop_File.

   procedure Add_Line
     (File   : File_Reference;
      Line   : Natural;
      Column : Natural);
   --  Add a new reference in a file, which the user has provided on the
   --  command line. This is used for an optimized matching algorithm.

   procedure Add_Reference
     (Declaration   : Declaration_Reference;
      File_Ref      : File_Reference;
      Line          : Natural;
      Column        : Natural;
      Ref_Type      : Character;
      Labels_As_Ref : Boolean);
   --  Add a new reference (Ref_Type = 'r'), body (Ref_Type = 'b') or
   --  modification (Ref_Type = 'm') to an entity. If Labels_As_Ref is True,
   --  then the references to the entity after the end statements ("end Foo")
   --  are counted as actual references. This means that the entity will never
   --  be reported as unreferenced (for instance in the case of gnatxref -u).

   function Get_Declarations
     (Sorted : Boolean := True)
      return   Declaration_Array_Access;
   --  Return a sorted list of all the declarations in the application.
   --  Freeing this array is the responsibility of the caller, however it
   --  shouldn't free the actual contents of the array, which are pointers
   --  to internal data

   function References_Count
     (Decl       : Declaration_Reference;
      Get_Reads  : Boolean := False;
      Get_Writes : Boolean := False;
      Get_Bodies : Boolean := False)
      return       Natural;
   --  Return the number of references in Decl for the categories specified
   --  by the Get_* parameters (read-only accesses, write accesses and bodies)

   function Get_References
     (Decl : Declaration_Reference;
      Get_Reads  : Boolean := False;
      Get_Writes : Boolean := False;
      Get_Bodies : Boolean := False)
      return       Reference_Array_Access;
   --  Return a sorted list of all references to the entity in decl. The
   --  parameters Get_* are used to specify what kind of references should be
   --  merged and returned (read-only accesses, write accesses and bodies).

   function Get_Column (Decl : Declaration_Reference) return String;
   function Get_Column (Ref : Reference) return String;

   function Get_Declaration
     (File_Ref : File_Reference;
      Line     : Natural;
      Column   : Natural)
      return     Declaration_Reference;
   --  Returns reference to the declaration found in file File_Ref at the
   --  given Line and Column

   function Get_Parent
     (Decl : Declaration_Reference)
      return Declaration_Reference;
   --  Returns reference to Decl's parent declaration

   function Get_Emit_Warning (File : File_Reference) return Boolean;
   --  Returns the Emit_Warning field of the structure

   function Get_Gnatchop_File
     (File     : File_Reference;
      With_Dir : Boolean := False)
      return     String;
   function Get_Gnatchop_File
     (Ref      : Reference;
      With_Dir : Boolean := False)
      return     String;
   function Get_Gnatchop_File
     (Decl     : Declaration_Reference;
      With_Dir : Boolean := False)
      return     String;
   --  Return the name of the file that File was extracted from through a
   --  call to "gnatchop -r". The file name for File is returned if File
   --  was not extracted from such a file. The directory will be given only
   --  if With_Dir is True.

   function Get_File
     (Decl     : Declaration_Reference;
      With_Dir : Boolean := False) return String;
   pragma Inline (Get_File);
   --  Extract column number or file name from reference

   function Get_File
     (Ref      : Reference;
      With_Dir : Boolean := False) return String;
   pragma Inline (Get_File);

   function Get_File
     (File     : File_Reference;
      With_Dir : Boolean := False;
      Strip    : Natural := 0) return String;
   --  Returns the file name (and its directory if With_Dir is True or the user
   --  has used the -f switch on the command line. If Strip is not 0, then the
   --  last Strip-th "-..." substrings are removed first. For instance, with
   --  Strip=2, a file name "parent-child1-child2-child3.ali" would be returned
   --  as "parent-child1.ali". This is used when looking for the ALI file to
   --  use for a package, since for separates with have to use the parent's
   --  ALI. The null string is returned if there is no such parent unit.
   --
   --  Note that this version of Get_File is not inlined

   function Get_File_Ref (Ref : Reference)              return File_Reference;
   function Get_Line     (Decl : Declaration_Reference) return String;
   function Get_Line     (Ref : Reference)              return String;
   function Get_Symbol   (Decl : Declaration_Reference) return String;
   function Get_Type     (Decl : Declaration_Reference) return Character;
   function Is_Parameter (Decl : Declaration_Reference) return Boolean;
   --  Functions that return the contents of a declaration

   function Get_Source_Line (Ref : Reference)              return String;
   function Get_Source_Line (Decl : Declaration_Reference) return String;
   --  Return the source line associated with the reference

   procedure Grep_Source_Files;
   --  Parse all the source files which have at least one reference, and grep
   --  the appropriate source lines so that we'll be able to display them. This
   --  function should be called once all the .ali files have been parsed, and
   --  only if the appropriate user switch
   --  has been used (gnatfind -s).
   --
   --  Note: To save memory, the strings for the source lines are shared. Thus
   --  it is no longer possible to free the references, or we would free the
   --  same chunk multiple times. It doesn't matter, though, since this is only
   --  called once, prior to exiting gnatfind.

   function Longest_File_Name return Natural;
   --  Returns the longest file name found

   function Match (Decl : Declaration_Reference) return Boolean;
   --  Return True if the declaration matches

   function Match
     (File   : File_Reference;
      Line   : Natural;
      Column : Natural)
      return   Boolean;
   --  Returns True if File:Line:Column was given on the command line
   --  by the user

   function Next_Unvisited_File return File_Reference;
   --  Returns the next unvisited library file in the list If there is no more
   --  unvisited file, return Empty_File. Two calls to this subprogram will
   --  return different files.

   procedure Set_Default_Match (Value : Boolean);
   --  Set the default value for match in declarations.
   --  This is used so that if no file was provided in the
   --  command line, then every file match

   procedure Reset_Directory (File : File_Reference);
   --  Reset the cached directory for file. Next time Get_File is called, the
   --  directory will be recomputed.

   procedure Set_Unvisited (File_Ref : File_Reference);
   --  Set File_Ref as unvisited. So Next_Unvisited_File will return it

   procedure Read_File
     (File_Name : String;
      Contents  : out GNAT.OS_Lib.String_Access);
   --  Reads File_Name into the newly allocated string Contents. Types.EOF
   --  character will be added to the returned Contents to simplify parsing.
   --  Name_Error is raised if the file was not found. End_Error is raised if
   --  the file could not be read correctly. For most systems correct reading
   --  means that the number of bytes read is equal to the file size.

private
   type Project_File (Src_Dir_Length, Obj_Dir_Length : Natural) is record
      Src_Dir : String (1 .. Src_Dir_Length);
      Src_Dir_Index : Integer;

      Obj_Dir            : String (1 .. Obj_Dir_Length);
      Obj_Dir_Index      : Integer;
      Last_Obj_Dir_Start : Natural;
   end record;

   type Project_File_Ptr is access all Project_File;
   --  This is actually a list of all the directories to be searched,
   --  either for source files or for library files

   type Ref_In_File;
   type Ref_In_File_Ptr is access all Ref_In_File;

   type Ref_In_File is record
      Line   : Natural;
      Column : Natural;
      Next   : Ref_In_File_Ptr := null;
   end record;

   type File_Record;
   type File_Reference is access all File_Record;

   Empty_File : constant File_Reference := null;
   type Cst_String_Access is access constant String;

   procedure Free (Str : in out Cst_String_Access);

   type File_Record is record
      File            : Cst_String_Access;
      Dir             : GNAT.OS_Lib.String_Access;
      Lines           : Ref_In_File_Ptr := null;
      Visited         : Boolean         := False;
      Emit_Warning    : Boolean         := False;
      Gnatchop_File   : GNAT.OS_Lib.String_Access   := null;
      Gnatchop_Offset : Integer         := 0;
      Next            : File_Reference  := null;
   end record;
   --  Holds a reference to a source file, that was referenced in at least one
   --  ALI file. Gnatchop_File will contain the name of the file that File was
   --  extracted From. Gnatchop_Offset contains the index of the first line of
   --  File within Gnatchop_File. These two fields are used to properly support
   --  gnatchop files and pragma Source_Reference.
   --
   --  Lines is used for files that were given on the command line, to
   --  memorize the lines and columns that the user specified.

   type Reference_Record;
   type Reference is access all Reference_Record;

   Empty_Reference : constant Reference := null;

   type Reference_Record is record
      File        : File_Reference;
      Line        : Natural;
      Column      : Natural;
      Source_Line : Cst_String_Access;
      Next        : Reference := null;
   end record;
   --  File is a reference to the Ada source file
   --  Source_Line is the Line as it appears in the source file. This
   --  field is only used when the switch is set on the command line of
   --  gnatfind.

   type Declaration_Record;
   type Declaration_Reference is access all Declaration_Record;

   Empty_Declaration : constant Declaration_Reference := null;

   type Declaration_Record (Symbol_Length : Natural) is record
      Key          : Cst_String_Access;
      Symbol       : String (1 .. Symbol_Length);
      Decl         : Reference;
      Is_Parameter : Boolean := False; -- True if entity is subprog param
      Decl_Type    : Character;
      Body_Ref     : Reference := null;
      Ref_Ref      : Reference := null;
      Modif_Ref    : Reference := null;
      Match        : Boolean := False;
      Par_Symbol   : Declaration_Reference := null;
      Next         : Declaration_Reference := null;
   end record;
   --  The lists of referenced (Body_Ref, Ref_Ref and Modif_Ref) are
   --  kept unsorted until the results needs to be printed. This saves
   --  lots of time while the internal tables are created.

   pragma Inline (Get_Column);
   pragma Inline (Get_Emit_Warning);
   pragma Inline (Get_File_Ref);
   pragma Inline (Get_Line);
   pragma Inline (Get_Symbol);
   pragma Inline (Get_Type);
   pragma Inline (Longest_File_Name);
end Xr_Tabls;
