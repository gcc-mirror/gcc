------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--              Copyright (C) 2001, Ada Core Technologies, Inc.             --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a set of target dependent routines to build
--  static, dynamic and shared libraries.

--  There are several versions for the body of this package.

--  In the default version, libraries are not supported, so function
--  Libraries_Are_Supported returns False.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Types;       use Types;

package MLib.Tgt is

   function Libraries_Are_Supported return Boolean;
   --  Indicates if building libraries by gnatmake and gnatmlib
   --  are supported by the GNAT implementation for the OS.

   function Default_DLL_Address return String;
   --  default address for non relocatable DLL

   function Dynamic_Option return String;
   --  gcc option to create a dynamic library

   function Base_Option return String;

   function Libgnat return String;
   --  System dependent static GNAT library

   function Archive_Ext return  String;
   --  System dependent static library extension

   function Object_Ext return String;
   --  System dependent object extension

   function DLL_Ext return String;
   --  System dependent dynamic library extension

   function PIC_Option return String;
   --  Position independent code option

   function Is_Object_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is an object file extension

   function Is_C_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is a C file extension.

   function Is_Archive_Ext (Ext : String) return Boolean;
   --  Returns True iff Ext is an extension for a library

   procedure Copy_ALI_Files
     (From : Name_Id;
      To   : Name_Id);
   --  Copy all ALI files from directory From to directory To

   function Linker_Library_Path_Option
     (Directory : String)
      return      String_Access;
   --  Linker option to specify the library directory path

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Lib_Address  : String  := "";
      Lib_Version  : String  := "";
      Relocatable  : Boolean := False);
   --  Build a dynamic/relocatable library

end MLib.Tgt;
