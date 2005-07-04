------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2001-2005, Ada Core Technologies, Inc.        --
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

--  This package provides a set of target dependent routines to build
--  static, dynamic and shared libraries.

--  There are several versions for the body of this package

--  In the default version, libraries are not supported, so function
--  Support_For_Libraries return None.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Prj;         use Prj;

package MLib.Tgt is

   type Library_Support is (None, Static_Only, Full);
   --  Support for Library Project File.
   --  - None: Library Project Files are not supported at all
   --  - Static_Only: Library Project Files are only supported for static
   --    libraries.
   --  - Full: Library Project Files are supported for static and dynamic
   --    (shared) libraries.

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
   --  System dependent object extension, without leadien dot.
   --  On Unix, returns "o".

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

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Options_2    : Argument_List;
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
   --  Foreign is the list of non Ada object files (also included in Ofiles)
   --
   --  Afiles is the list of ALI files for the Ada object files
   --
   --  Options and Options_2 are lists of options to be passed to the tool
   --  (gcc or other) that effectively builds the dynamic library. Options
   --  are passed before the object files, Options_2 are passed after the
   --  object files.
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
   --  Symbol_Data is used for some patforms, including VMS, to generate
   --  the symbols to be exported by the library.
   --
   --  Note: Depending on the OS, some of the parameters may not be taken
   --  into account. For example, on Linux, Foreign, Afiles Lib_Address and
   --  Relocatable are ignored.

   function Library_Exists_For
     (Project : Project_Id; In_Tree : Project_Tree_Ref) return Boolean;
   --  Return True if the library file for a library project already exists.
   --  This function can only be called for library projects.

   function Library_File_Name_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Name_Id;
   --  Returns the file name of the library file of a library project.
   --  This function can only be called for library projects.

end MLib.Tgt;
