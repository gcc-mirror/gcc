------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R  _ T A B L S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--         Copyright (C) 1998-2000 Free Software Foundation, Inc.           --
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

with Ada.Strings.Unbounded;

package Xr_Tabls is

   -------------------
   -- Project files --
   -------------------

   function ALI_File_Name (Ada_File_Name : String) return String;
   --  Returns the ali file name corresponding to Ada_File_Name, using the
   --  information provided in gnat.adc if it exists

   procedure Create_Project_File
     (Name           : String);
   --  Open and parse a new project file
   --  If the file Name could not be open or is not a valid project file
   --  then a project file associated with the standard default directories
   --  is returned

   function Find_ALI_File (Short_Name  : String) return String;
   --  Returns the directory name for the file Short_Name
   --  takes into account the obj_dir lines in the project file,
   --  and the default paths for Gnat

   function Find_Source_File (Short_Name  : String) return String;
   --  Returns the directory name for the file Short_Name
   --  takes into account the src_dir lines in the project file,
   --  and the default paths for Gnat

   function Next_Src_Dir return String;
   --  Returns the next directory to visit to find related source files
   --  If there are no more such directory, Length = 0

   function Next_Obj_Dir return String;
   --  Returns the next directory to visit to find related ali files
   --  If there are no more such directory, Length = 0

   function Current_Obj_Dir return String;
   --  Returns the obj_dir which was returned by the last Next_Obj_Dir call

   procedure Parse_Gnatls
     (Gnatls_Src_Cache : out Ada.Strings.Unbounded.Unbounded_String;
      Gnatls_Obj_Cache : out Ada.Strings.Unbounded.Unbounded_String);
   --  Parse the output of Gnatls, to find the standard
   --  directories for source files

   procedure Reset_Src_Dir;
   --  Reset the iterator for Src_Dir

   procedure Reset_Obj_Dir;
   --  Reset the iterator for Obj_Dir

   ------------
   -- Tables --
   ------------

   type Declaration_Reference is private;
   Empty_Declaration : constant Declaration_Reference;

   type File_Reference is private;
   Empty_File : constant File_Reference;

   type Reference is private;
   Empty_Reference : constant Reference;

   type File_Table is limited private;
   type Entity_Table is limited private;

   function Add_Declaration
     (File_Ref  : File_Reference;
      Symbol    : String;
      Line      : Natural;
      Column    : Natural;
      Decl_Type : Character)
      return      Declaration_Reference;
   --  Add a new declaration in the table and return the index to it.
   --  Decl_Type is the type of the entity

   procedure Add_Parent
     (Declaration : in out Declaration_Reference;
      Symbol      : String;
      Line        : Natural;
      Column      : Natural;
      File_Ref    : File_Reference);
   --  The parent declaration (Symbol in file File_Ref at position Line and
   --  Column) information is added to Declaration.

   procedure Add_File
     (File_Name    : String;
      File_Existed : out Boolean;
      Ref          : out File_Reference;
      Visited      : Boolean := True;
      Emit_Warning : Boolean := False;
      Gnatchop_File : String := "";
      Gnatchop_Offset : Integer := 0);
   --  Add a new reference to a file in the table. Ref is used to return
   --  the index in the table where this file is stored On exit,
   --  File_Existed is True if the file was already in the table Visited is
   --  the value which will be used in the table (if True, the file will
   --  not be returned by Next_Unvisited_File). If Emit_Warning is True and
   --  the ali file does not exist or does not have cross-referencing
   --  informations, then a warning will be emitted.
   --  Gnatchop_File is the name of the file that File_Name was extracted from
   --  through a call to "gnatchop -r" (with pragma Source_Reference).
   --  Gnatchop_Offset should be the index of the first line of File_Name
   --  withing Gnatchop_File.

   procedure Add_Line
     (File   : File_Reference;
      Line   : Natural;
      Column : Natural);
   --  Add a new reference in a file, which the user has provided
   --  on the command line. This is used for a optimized matching
   --  algorithm.

   procedure Add_Reference
     (Declaration : Declaration_Reference;
      File_Ref    : File_Reference;
      Line        : Natural;
      Column      : Natural;
      Ref_Type    : Character);
   --  Add a new reference (Ref_Type = 'r'), body (Ref_Type = 'b') or
   --  modification (Ref_Type = 'm') to an entity

   type Compare_Result is (LessThan, Equal, GreaterThan);
   function Compare (Ref1, Ref2 : Reference) return Compare_Result;
   function Compare
     (Decl1 : Declaration_Reference;
      File2 : File_Reference;
      Line2 : Integer;
      Col2  : Integer;
      Symb2 : String)
      return  Compare_Result;
   --  Compare two references

   function First_Body (Decl : Declaration_Reference) return Reference;
   function First_Declaration return Declaration_Reference;
   function First_Modif  (Decl : Declaration_Reference) return Reference;
   function First_Reference (Decl : Declaration_Reference) return Reference;
   --  Initialize the iterators

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
     (File : File_Reference; With_Dir : Boolean := False) return String;
   function Get_Gnatchop_File
     (Ref : Reference; With_Dir : Boolean := False) return String;
   function Get_Gnatchop_File
     (Decl : Declaration_Reference; With_Dir : Boolean := False) return String;
   --  Return the name of the file that File was extracted from through a
   --  call to "gnatchop -r".
   --  The file name for File is returned if File wasn't extracted from such a
   --  file. The directory will be given only if With_Dir is True.


   function Get_File
     (Decl     : Declaration_Reference;
      With_Dir : Boolean := False)
      return     String;
   --  Extract column number or file name from reference

   function Get_File
     (Ref      : Reference;
      With_Dir : Boolean := False)
      return     String;
   pragma Inline (Get_File);

   function Get_File
     (File     : File_Reference;
      With_Dir : Boolean := False;
      Strip    : Natural := 0)
     return     String;
   --  Returns the file name (and its directory if With_Dir is True or
   --  the user as used the -f switch on the command line.
   --  If Strip is not 0, then the last Strip-th "-..." substrings are
   --  removed first. For instance, with Strip=2, a file name
   --  "parent-child1-child2-child3.ali" would be returned as
   --  "parent-child1.ali". This is used when looking for the ALI file to use
   --  for a package, since for separates with have to use the parent's ALI.
   --
   --  "" is returned if there is no such parent unit

   function Get_File_Ref (Ref : Reference) return File_Reference;
   function Get_Line (Decl : Declaration_Reference) return String;
   function Get_Line (Ref : Reference) return String;
   function Get_Symbol (Decl : Declaration_Reference) return String;
   function Get_Type (Decl : Declaration_Reference) return Character;
   --  Functions that return the content of a declaration

   function Get_Source_Line (Ref : Reference) return String;
   function Get_Source_Line (Decl : Declaration_Reference) return String;
   --  Return the source line associated with the reference

   procedure Grep_Source_Files;
   --  Parse all the source files which have at least one reference, and
   --  grep the appropriate lines so that we'll be able to display them.
   --  This function should be called once all the .ali files have been
   --  parsed, and only if the appropriate user switch has been used.

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

   function Next (Decl : Declaration_Reference) return Declaration_Reference;
   function Next (Ref : Reference) return Reference;
   --  Returns the next declaration, or Empty_Declaration

   function Next_Unvisited_File return File_Reference;
   --  Returns the next unvisited library file in the list
   --  If there is no more unvisited file, return Empty_File

   procedure Set_Default_Match (Value : Boolean);
   --  Set the default value for match in declarations.
   --  This is used so that if no file was provided in the
   --  command line, then every file match

   procedure Set_Directory
     (File : File_Reference;
      Dir  : String);
   --  Set the directory for a file

   procedure Set_Unvisited (File_Ref : in File_Reference);
   --  Set File_Ref as unvisited. So Next_Unvisited_File will return it.


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

   type String_Access is access all String;

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

   type File_Record (File_Length : Natural) is record
      File         : String (1 .. File_Length);
      Dir          : String_Access   := null;
      Lines        : Ref_In_File_Ptr := null;
      Visited      : Boolean         := False;
      Emit_Warning : Boolean         := False;
      Gnatchop_File : String_Access  := null;
      Gnatchop_Offset : Integer      := 0;
      Next         : File_Reference  := null;
   end record;
   --  Holds a reference to a source file, that was referenced in at least one
   --  ALI file.
   --  Gnatchop_File will contain the name of the file that File was extracted
   --  From. Gnatchop_Offset contains the index of the first line of File
   --  within Gnatchop_File. These two fields are used to properly support
   --  gnatchop files and pragma Source_Reference.


   type Reference_Record;
   type Reference is access all Reference_Record;

   Empty_Reference : constant Reference := null;

   type Reference_Record is record
      File        : File_Reference;
      Line        : Natural;
      Column      : Natural;
      Source_Line : Ada.Strings.Unbounded.Unbounded_String;
      Next        : Reference := null;
   end record;
   --  File is a reference to the Ada source file
   --  Source_Line is the Line as it appears in the source file. This
   --  field is only used when the switch is set on the command line

   type Declaration_Record;
   type Declaration_Reference is access all Declaration_Record;

   Empty_Declaration : constant Declaration_Reference := null;

   type Declaration_Record (Symbol_Length : Natural) is record
      Symbol     : String (1 .. Symbol_Length);
      Decl       : aliased Reference_Record;
      Decl_Type  : Character;
      Body_Ref   : Reference := null;
      Ref_Ref    : Reference := null;
      Modif_Ref  : Reference := null;
      Match      : Boolean := False;
      Par_Symbol : Declaration_Reference := null;
      Next       : Declaration_Reference := null;
   end record;

   type File_Table is record
      Table        : File_Reference := null;
      Longest_Name : Natural := 0;
   end record;

   type Entity_Table is record
      Table : Declaration_Reference := null;
   end record;

   pragma Inline (First_Body);
   pragma Inline (First_Declaration);
   pragma Inline (First_Modif);
   pragma Inline (First_Reference);
   pragma Inline (Get_Column);
   pragma Inline (Get_Emit_Warning);
   pragma Inline (Get_File);
   pragma Inline (Get_File_Ref);
   pragma Inline (Get_Line);
   pragma Inline (Get_Symbol);
   pragma Inline (Get_Type);
   pragma Inline (Longest_File_Name);
   pragma Inline (Next);

end Xr_Tabls;
