------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

--  This package provides a set of target dependent routines to build static,
--  dynamic and shared libraries. There are several packages providing
--  the actual routines. This package calls them indirectly by means of
--  access-to-subprogram values. Each target-dependent package initializes
--  these values in its elaboration block.

with Prj; use Prj;

package MLib.Tgt is

   function Support_For_Libraries return Library_Support;
   --  Indicates how building libraries by gnatmake is supported by the GNAT
   --  implementation for the platform.

   function Standalone_Library_Auto_Init_Is_Supported return Boolean;
   --  Indicates if when building a dynamic Standalone Library,
   --  automatic initialization is supported. If it is, then it is the default,
   --  unless attribute Library_Auto_Init has the value "false".

   function Archive_Builder return String;
   --  Returns the name of the archive builder program, usually "ar"

   function Archive_Builder_Options return String_List_Access;
   --  A list of options to invoke the Archive_Builder, usually "cr" for "ar"

   function Archive_Builder_Append_Options return String_List_Access;
   --  A list of options to use with the archive builder to append object
   --  files ("q", for example).

   function Archive_Indexer return String;
   --  Returns the name of the program, if any, that generates an index to the
   --  contents of an archive, usually "ranlib". If there is no archive indexer
   --  to be used, returns an empty string.

   function Archive_Indexer_Options return String_List_Access;
   --  A list of options to invoke the Archive_Indexer, usually empty

   function Dynamic_Option return String;
   --  gcc option to create a dynamic library.
   --  For Unix, returns "-shared", for Windows returns "-mdll".

   function Libgnat return String;
   --  System dependent static GNAT library

   function Archive_Ext return  String;
   --  System dependent static library extension, without leading dot.
   --  For Unix and Windows, return "a".

   function Object_Ext return String;
   --  System dependent object extension, without leading dot.
   --  On Unix, returns "o".

   function DLL_Prefix return String;
   --  System dependent dynamic library prefix.
   --  On Windows, returns "". On other platforms, returns "lib".

   function DLL_Ext return String;
   --  System dependent dynamic library extension, without leading dot.
   --  On Windows, returns "dll". On Unix, usually returns "so", but not
   --  always, e.g. on HP-UX the extension for shared libraries is "sl".

   function PIC_Option return String;
   --  Position independent code option

   function Is_Object_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is an object file extension

   function Is_C_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is a C file extension

   function Is_Archive_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is an extension for a library

   function Default_Symbol_File_Name return String;
   --  Returns the name of the symbol file when Library_Symbol_File is not
   --  specified. Return the empty string when symbol files are not supported.

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False);
   --  Build a dynamic/relocatable library
   --
   --  Ofiles is the list of all object files in the library
   --
   --  Options is a list of options to be passed to the tool
   --  (gcc or other) that effectively builds the dynamic library.
   --
   --  Interfaces is the list of ALI files for the interfaces of a SAL.
   --  It is empty if the library is not a SAL.
   --
   --  Lib_Filename is the name of the library, without any prefix or
   --  extension. For example, on Unix, if Lib_Filename is "toto", the
   --  name of the library file will be "libtoto.so".
   --
   --  Lib_Dir is the directory path where the library will be located
   --
   --  For OSes that support symbolic links, Lib_Version, if non null,
   --  is the actual file name of the library. For example on Unix, if
   --  Lib_Filename is "toto" and Lib_Version is "libtoto.so.2.1",
   --  "libtoto.so" will be a symbolic link to "libtoto.so.2.1" which
   --  will be the actual library file.
   --
   --  Symbol_Data is used for some platforms, to generate the symbols to be
   --  exported by the library (not certain if it is currently in use or not).
   --
   --  Note: Depending on the OS, some of the parameters may not be taken into
   --  account. For example, on Linux, Interfaces, Symbol_Data and Auto_Init
   --  are ignored.

   function Library_Exists_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Boolean;
   --  Return True if the library file for a library project already exists.
   --  This function can only be called for library projects.

   function Library_File_Name_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return File_Name_Type;
   --  Returns the file name of the library file of a library project.
   --  This function can only be called for library projects.

   function Library_Major_Minor_Id_Supported return Boolean;
   --  Indicates if major and minor ids are supported for libraries.
   --  If they are supported, then a Library_Version such as libtoto.so.1.2
   --  will have a major id of 1 and a minor id of 2. Then libtoto.so,
   --  libtoto.so.1 and libtoto.so.1.2 will be created, all three designating
   --  the same file.

private
   No_Argument_List : constant Argument_List := (1 .. 0 => null);

   --  Access to subprogram types for indirection

   type String_Function is access function return String;
   type Is_Ext_Function is access function (Ext : String) return Boolean;
   type String_List_Access_Function is access function
     return String_List_Access;

   type Build_Dynamic_Library_Function is access procedure
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False);

   type Library_Exists_For_Function is access function
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Boolean;

   type Library_File_Name_For_Function is access function
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return File_Name_Type;

   type Boolean_Function is access function return Boolean;
   type Library_Support_Function is access function return Library_Support;

   function Archive_Builder_Default return String;
   Archive_Builder_Ptr : String_Function := Archive_Builder_Default'Access;

   function Archive_Builder_Options_Default return String_List_Access;
   Archive_Builder_Options_Ptr : String_List_Access_Function :=
                                   Archive_Builder_Options_Default'Access;

   function Archive_Builder_Append_Options_Default return String_List_Access;
   Archive_Builder_Append_Options_Ptr : String_List_Access_Function :=
                                Archive_Builder_Append_Options_Default'Access;

   function Archive_Ext_Default return String;
   Archive_Ext_Ptr : String_Function := Archive_Ext_Default'Access;

   function Archive_Indexer_Default return String;
   Archive_Indexer_Ptr : String_Function := Archive_Indexer_Default'Access;

   function Archive_Indexer_Options_Default return String_List_Access;
   Archive_Indexer_Options_Ptr : String_List_Access_Function :=
                                   Archive_Indexer_Options_Default'Access;

   function Default_Symbol_File_Name_Default return String;
   Default_Symbol_File_Name_Ptr : String_Function :=
                                    Default_Symbol_File_Name_Default'Access;

   Build_Dynamic_Library_Ptr : Build_Dynamic_Library_Function;

   function DLL_Ext_Default return String;
   DLL_Ext_Ptr : String_Function := DLL_Ext_Default'Access;

   function DLL_Prefix_Default return String;
   DLL_Prefix_Ptr : String_Function := DLL_Prefix_Default'Access;

   function Dynamic_Option_Default return String;
   Dynamic_Option_Ptr : String_Function := Dynamic_Option_Default'Access;

   function Is_Object_Ext_Default (Ext : String) return Boolean;
   Is_Object_Ext_Ptr : Is_Ext_Function := Is_Object_Ext_Default'Access;

   function Is_C_Ext_Default (Ext : String) return Boolean;
   Is_C_Ext_Ptr : Is_Ext_Function := Is_C_Ext_Default'Access;

   function Is_Archive_Ext_Default (Ext : String) return Boolean;
   Is_Archive_Ext_Ptr : Is_Ext_Function := Is_Archive_Ext_Default'Access;

   function Libgnat_Default return String;
   Libgnat_Ptr : String_Function := Libgnat_Default'Access;

   function Library_Exists_For_Default
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Boolean;
   Library_Exists_For_Ptr : Library_Exists_For_Function :=
                              Library_Exists_For_Default'Access;

   function Library_File_Name_For_Default
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return File_Name_Type;
   Library_File_Name_For_Ptr : Library_File_Name_For_Function :=
                                 Library_File_Name_For_Default'Access;

   function Object_Ext_Default return String;
   Object_Ext_Ptr : String_Function := Object_Ext_Default'Access;

   function PIC_Option_Default return String;
   PIC_Option_Ptr : String_Function := PIC_Option_Default'Access;

   function Standalone_Library_Auto_Init_Is_Supported_Default return Boolean;
   Standalone_Library_Auto_Init_Is_Supported_Ptr : Boolean_Function :=
            Standalone_Library_Auto_Init_Is_Supported_Default'Access;

   function Support_For_Libraries_Default return Library_Support;
   Support_For_Libraries_Ptr : Library_Support_Function :=
                                 Support_For_Libraries_Default'Access;

   function Library_Major_Minor_Id_Supported_Default return Boolean;
   Library_Major_Minor_Id_Supported_Ptr : Boolean_Function :=
             Library_Major_Minor_Id_Supported_Default'Access;
end MLib.Tgt;
