------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                            (Windows Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2002-2003, Ada Core Technologies, Inc.           --
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

--  This package provides a set of target dependent routines to build
--  static, dynamic and shared libraries.

--  This is the Windows version of the body.

with Namet;  use Namet;
with Opt;
with Output; use Output;
with Prj.Com;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with MDLL;
with MDLL.Utl;
with MLib.Fil;

package body MLib.Tgt is

   ---------------------
   -- Archive_Builder --
   ---------------------

   function Archive_Builder return String is
   begin
      return "ar";
   end Archive_Builder;

   -----------------------------
   -- Archive_Builder_Options --
   -----------------------------

   function Archive_Builder_Options return String_List_Access is
   begin
      return new String_List'(1 => new String'("cr"));
   end Archive_Builder_Options;

   -----------------
   -- Archive_Ext --
   -----------------

   function Archive_Ext return  String is
   begin
      return "a";
   end Archive_Ext;

   ---------------------
   -- Archive_Indexer --
   ---------------------

   function Archive_Indexer return String is
   begin
      return "ranlib";
   end Archive_Indexer;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Address  : String  := "";
      Lib_Version  : String  := "";
      Relocatable  : Boolean := False;
      Auto_Init    : Boolean := False)
   is
      pragma Unreferenced (Ofiles);
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Driver_Name);
      pragma Unreferenced (Lib_Version);
      pragma Unreferenced (Auto_Init);

      Imp_File : constant String :=
                   "lib" & MLib.Fil.Ext_To (Lib_Filename, Archive_Ext);
      --  Name of the import library

      DLL_File : constant String := MLib.Fil.Ext_To (Lib_Filename, DLL_Ext);
      --  Name of the DLL file

      Lib_File : constant String := Lib_Dir & Directory_Separator & DLL_File;
      --  Full path of the DLL file

      Success : Boolean;

   begin
      if Opt.Verbose_Mode then
         if Relocatable then
            Write_Str ("building relocatable shared library ");
         else
            Write_Str ("building non-relocatable shared library ");
         end if;

         Write_Line (Lib_File);
      end if;

      MDLL.Verbose := Opt.Verbose_Mode;
      MDLL.Quiet   := not MDLL.Verbose;

      MDLL.Utl.Locate;

      MDLL.Build_Dynamic_Library
        (Foreign, Afiles,
         MDLL.Null_Argument_List, MDLL.Null_Argument_List, Options,
         Lib_Filename, Lib_Filename & ".def",
         Lib_Address, True, Relocatable);

      --  Move the DLL and import library in the lib directory

      Copy_File (DLL_File, Lib_Dir, Success, Mode => Overwrite);

      if not Success then
         Fail ("could not copy DLL to library dir");
      end if;

      Copy_File (Imp_File, Lib_Dir, Success, Mode => Overwrite);

      if not Success then
         Fail ("could not copy import library to library dir");
      end if;

      --  Delete files

      Delete_File (DLL_File, Success);

      if not Success then
         Fail ("could not delete DLL from build dir");
      end if;

      Delete_File (Imp_File, Success);

      if not Success then
         Fail ("could not delete import library from build dir");
      end if;
   end Build_Dynamic_Library;

   -------------------------
   -- Default_DLL_Address --
   -------------------------

   function Default_DLL_Address return String is
   begin
      return "0x11000000";
   end Default_DLL_Address;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "dll";
   end DLL_Ext;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return "";
   end Dynamic_Option;

   -------------------
   -- Is_Object_Ext --
   -------------------

   function Is_Object_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".o";
   end Is_Object_Ext;

   --------------
   -- Is_C_Ext --
   --------------

   function Is_C_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".c";
   end Is_C_Ext;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".a";
   end Is_Archive_Ext;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
   begin
      return "libgnat.a";
   end Libgnat;

   ------------------------
   -- Library_Exists_For --
   ------------------------

   function Library_Exists_For (Project : Project_Id) return Boolean is
   begin
      if not Projects.Table (Project).Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_Exists_For called " &
                       "for non library project");
         return False;

      else
         declare
            Lib_Dir : constant String :=
              Get_Name_String (Projects.Table (Project).Library_Dir);
            Lib_Name : constant String :=
              Get_Name_String (Projects.Table (Project).Library_Name);

         begin
            if Projects.Table (Project).Library_Kind = Static then

               --  Static libraries are named : lib<name>.a

               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  MLib.Fil.Ext_To (Lib_Name, Archive_Ext));

            else
               --  Shared libraries are named : <name>.dll

               return Is_Regular_File
                 (Lib_Dir & Directory_Separator &
                  MLib.Fil.Ext_To (Lib_Name, DLL_Ext));
            end if;
         end;
      end if;
   end Library_Exists_For;

   ---------------------------
   -- Library_File_Name_For --
   ---------------------------

   function Library_File_Name_For (Project : Project_Id) return Name_Id is
   begin
      if not Projects.Table (Project).Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_File_Name_For called " &
                       "for non library project");
         return No_Name;

      else
         declare
            Lib_Name : constant String :=
                         Get_Name_String
                           (Projects.Table (Project).Library_Name);

         begin
            if Projects.Table (Project).Library_Kind = Static then

               --  Static libraries are named : lib<name>.a

               Name_Len := 3;
               Name_Buffer (1 .. Name_Len) := "lib";

               Add_Str_To_Name_Buffer (Fil.Ext_To (Lib_Name, Archive_Ext));

            else
               --  Shared libraries are named : <name>.dll

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Fil.Ext_To (Lib_Name, DLL_Ext));
            end if;

            return Name_Find;
         end;
      end if;
   end Library_File_Name_For;

   --------------------------------
   -- Linker_Library_Path_Option --
   --------------------------------

   function Linker_Library_Path_Option return String_Access is
   begin
      return null;
   end Linker_Library_Path_Option;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return "o";
   end Object_Ext;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return "";
   end PIC_Option;

   -----------------------------------------------
   -- Standalone_Library_Auto_Init_Is_Supported --
   -----------------------------------------------

   function Standalone_Library_Auto_Init_Is_Supported return Boolean is
   begin
      return False;
   end Standalone_Library_Auto_Init_Is_Supported;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return Full;
   end Support_For_Libraries;

end MLib.Tgt;
