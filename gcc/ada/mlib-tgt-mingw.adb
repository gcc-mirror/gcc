------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                            (Windows Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2005, Free Software Foundation, Inc.         --
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

--  This is the Windows version of the body. Works only with GCC versions
--  supporting the "-shared" option.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada; use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Namet;  use Namet;
with Opt;
with Output; use Output;
with Prj.Com;

with MLib.Fil;
with MLib.Utl;

package body MLib.Tgt is

   package Files renames MLib.Fil;
   package Tools renames MLib.Utl;

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

   -----------------------------
   -- Archive_Indexer_Options --
   -----------------------------

   function Archive_Indexer_Options return String_List_Access is
   begin
      return new String_List (1 .. 0);
   end Archive_Indexer_Options;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

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
      Auto_Init    : Boolean := False)
   is
      pragma Unreferenced (Foreign);
      pragma Unreferenced (Afiles);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Lib_Version);

      Lib_File : constant String :=
                   Lib_Dir & Directory_Separator &
                   Files.Ext_To (Lib_Filename, DLL_Ext);

   --  Start of processing for Build_Dynamic_Library

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      --  Generate auto-init routine if in Auto_Init mode

      if Auto_Init then
         declare
            Compile_Only  : aliased String := "-c";
            GCC           : constant String_Access :=
                              Locate_Exec_On_Path ("gcc.exe");
            Filename      : constant String := To_Lower (Lib_Filename);
            Autoinit_Spec : constant String := Filename & "_autoinit.ads";
            Autoinit_Body : aliased String  := Filename & "_autoinit.adb";
            Autoinit_Obj  : aliased String  := Filename & "_autoinit.o";
            Autoinit_Ali  : constant String := Filename & "_autoinit.ali";
            Init_Proc     : constant String := Lib_Filename & "init";
            Final_Proc    : constant String := Lib_Filename & "final";
            Autoinit_Opt  : constant Argument_List :=
                              (1 => Autoinit_Obj'Unchecked_Access);
            Arguments     : constant Argument_List (1 .. 2) :=
                              (Compile_Only'Unchecked_Access,
                               Autoinit_Body'Unchecked_Access);
            File          : Text_IO.File_Type;
            Success       : Boolean;

         begin
            if Opt.Verbose_Mode then
               Write_Str ("Creating auto-init Ada file """);
               Write_Str (Autoinit_Spec);
               Write_Str (""" and """);
               Write_Str (Autoinit_Body);
               Write_Line ("""");
            end if;

            --  Create the spec

            Create (File, Out_File, Autoinit_Spec);

            Put_Line (File, "package " & Lib_Filename & "_autoinit is");
            New_Line (File);
            Put_Line (File, "   type HINSTANCE is new Integer;");
            Put_Line (File, "   type DWORD     is new Integer;");
            Put_Line (File, "   type LPVOID    is new Integer;");
            Put_Line (File, "   type BOOL      is new Integer;");
            New_Line (File);
            Put_Line (File, "   function DllMain");
            Put_Line (File, "     (hinstdll    : HINSTANCE;");
            Put_Line (File, "      fdwreason   : DWORD;");
            Put_Line (File, "      lpvreserved : LPVOID)");
            Put_Line (File, "      return BOOL;");
            Put_Line
              (File, "   pragma Export (Stdcall, DllMain, ""DllMain"");");
            New_Line (File);
            Put_Line (File, "end " & Lib_Filename & "_autoinit;");

            Close (File);

            --  Create the body

            Create (File, Out_File, Autoinit_Body);

            Put_Line (File, "package body " & Lib_Filename & "_autoinit is");
            New_Line (File);
            Put_Line (File, "   DLL_PROCESS_DETACH : constant := 0;");
            Put_Line (File, "   DLL_PROCESS_ATTACH : constant := 1;");
            Put_Line (File, "   DLL_THREAD_ATTACH  : constant := 2;");
            Put_Line (File, "   DLL_THREAD_DETACH  : constant := 3;");
            New_Line (File);
            Put_Line (File, "   procedure " & Init_Proc & ";");
            Put      (File, "   pragma Import (C, " & Init_Proc);
            Put_Line (File, ", """ & Init_Proc & """);");
            New_Line (File);
            Put_Line (File, "   procedure " & Final_Proc & ";");
            Put      (File, "   pragma Import (C, " & Final_Proc);
            Put_Line (File, ", """ & Final_Proc & """);");
            New_Line (File);
            Put_Line (File, "   function DllMain");
            Put_Line (File, "     (hinstdll    : HINSTANCE;");
            Put_Line (File, "      fdwreason   : DWORD;");
            Put_Line (File, "      lpvreserved : LPVOID)");
            Put_Line (File, "      return BOOL");
            Put_Line (File, "   is");
            Put_Line (File, "      pragma Unreferenced (hinstDLL);");
            Put_Line (File, "      pragma Unreferenced (lpvReserved);");
            Put_Line (File, "   begin");
            Put_Line (File, "      case fdwReason is");
            Put_Line (File, "         when DLL_PROCESS_ATTACH =>");
            Put_Line (File, "            " & Init_Proc & ";");
            Put_Line (File, "         when DLL_PROCESS_DETACH =>");
            Put_Line (File, "            " & Final_Proc & ";");
            Put_Line (File, "         when DLL_THREAD_ATTACH =>");
            Put_Line (File, "            null;");
            Put_Line (File, "         when DLL_THREAD_DETACH =>");
            Put_Line (File, "            null;");
            Put_Line (File, "         when others =>");
            Put_Line (File, "            null;");
            Put_Line (File, "      end case;");
            Put_Line (File, "      return 1;");
            Put_Line (File, "   exception");
            Put_Line (File, "      when others =>");
            Put_Line (File, "         return 0;");
            Put_Line (File, "   end DllMain;");
            New_Line (File);
            Put_Line (File, "end " & Lib_Filename & "_autoinit;");

            Close (File);

            --  Compile the auto-init file

            Spawn (GCC.all, Arguments, Success);

            if not Success then
               Fail ("unable to compile the auto-init unit for library """,
                     Lib_Filename, """");
            end if;

            --  Build the SAL library

            Tools.Gcc
              (Output_File => Lib_File,
               Objects     => Ofiles,
               Options     => Tools.No_Argument_List,
               Options_2   => Options & Options_2 & Autoinit_Opt,
               Driver_Name => Driver_Name);

            --  Remove generated files

            if Opt.Verbose_Mode then
               Write_Str ("deleting auto-init generated files");
               Write_Eol;
            end if;

            Delete_File (Autoinit_Spec, Success);
            Delete_File (Autoinit_Body, Success);
            Delete_File (Autoinit_Obj, Success);
            Delete_File (Autoinit_Ali, Success);
         end;

      else
         Tools.Gcc
           (Output_File => Lib_File,
            Objects     => Ofiles,
            Options     => Tools.No_Argument_List,
            Options_2   => Options & Options_2,
            Driver_Name => Driver_Name);
      end if;
   end Build_Dynamic_Library;

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
      return "-shared";
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
      return Ext = ".a" or else Ext = ".dll";
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

   function Library_Exists_For
     (Project : Project_Id; In_Tree : Project_Tree_Ref) return Boolean is
   begin
      if not In_Tree.Projects.Table (Project).Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_Exists_For called " &
                       "for non library project");
         return False;

      else
         declare
            Lib_Dir : constant String :=
              Get_Name_String
                (In_Tree.Projects.Table (Project).Library_Dir);
            Lib_Name : constant String :=
              Get_Name_String
                (In_Tree.Projects.Table (Project).Library_Name);

         begin
            if In_Tree.Projects.Table (Project).Library_Kind =
              Static
            then
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  MLib.Fil.Ext_To (Lib_Name, Archive_Ext));

            else
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

   function Library_File_Name_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Name_Id is
   begin
      if not In_Tree.Projects.Table (Project).Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_File_Name_For called " &
                       "for non library project");
         return No_Name;

      else
         declare
            Lib_Name : constant String :=
              Get_Name_String
                (In_Tree.Projects.Table (Project).Library_Name);

         begin
            if In_Tree.Projects.Table (Project).Library_Kind =
              Static
            then
               Name_Len := 3;
               Name_Buffer (1 .. Name_Len) := "lib";
               Add_Str_To_Name_Buffer (Fil.Ext_To (Lib_Name, Archive_Ext));

            else
               Name_Len := 0;
               Add_Str_To_Name_Buffer (Fil.Ext_To (Lib_Name, DLL_Ext));
            end if;

            return Name_Find;
         end;
      end if;
   end Library_File_Name_For;

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
      return True;
   end Standalone_Library_Auto_Init_Is_Supported;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return Full;
   end Support_For_Libraries;

end MLib.Tgt;
