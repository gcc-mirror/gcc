------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                              (AIX Version)                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2003-2005, Ada Core Technologies, Inc.           --
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
--  static, dynamic or relocatable libraries.

--  This is the AIX version of the body.

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with MLib.Fil;
with MLib.Utl;
with Namet;    use Namet;
with Osint;    use Osint;
with Opt;
with Output;   use Output;
with Prj.Com;
with Prj.Util; use Prj.Util;

package body MLib.Tgt is

   No_Arguments        : aliased Argument_List         := (1 .. 0 => null);
   Empty_Argument_List : constant Argument_List_Access := No_Arguments'Access;

   Bexpall : aliased String := "-Wl,-bexpall";
   Bexpall_Option : constant String_Access := Bexpall'Access;
   --  The switch to export all symbols

   Lpthreads : aliased String := "-lpthreads";
   Native_Thread_Options : aliased Argument_List := (1 => Lpthreads'Access);
   --  The switch to use when linking a library against libgnarl when using
   --  Native threads.

   Lgthreads : aliased String := "-lgthreads";
   Lmalloc   : aliased String := "-lmalloc";
   FSU_Thread_Options : aliased Argument_List :=
                          (1 => Lgthreads'Access, 2 => Lmalloc'Access);
   --  The switches to use when linking a library against libgnarl when using
   --  FSU threads.

   Thread_Options : Argument_List_Access := Empty_Argument_List;
   --  Designate the thread switches to used when linking a library against
   --  libgnarl. Depends on the thread library (Native or FSU). Resolved for
   --  the first library linked against libgnarl.

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

   function Archive_Ext return String is
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
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Lib_Version);
      pragma Unreferenced (Auto_Init);

      Lib_File : constant String :=
                   Lib_Dir & Directory_Separator & "lib" &
                   MLib.Fil.Ext_To (Lib_Filename, DLL_Ext);
      --  The file name of the library

      Thread_Opts : Argument_List_Access := Empty_Argument_List;
      --  Set to Thread_Options if -lgnarl is found in the Options

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      --  Look for -lgnarl in Options. If found, set the thread options.

      for J in Options'Range loop
         if Options (J).all = "-lgnarl" then

            --  If Thread_Options is null, read s-osinte.ads to discover the
            --  thread library and set Thread_Options accordingly.

            if Thread_Options = null then
               declare
                  File : Text_File;
                  Line : String (1 .. 100);
                  Last : Natural;

               begin
                  Open
                    (File, Include_Dir_Default_Prefix & "/s-osinte.ads");

                  while not End_Of_File (File) loop
                     Get_Line (File, Line, Last);

                     if Index (Line (1 .. Last), "-lpthreads") /= 0 then
                        Thread_Options := Native_Thread_Options'Access;
                        exit;

                     elsif Index (Line (1 .. Last), "-lgthreads") /= 0 then
                        Thread_Options := FSU_Thread_Options'Access;
                        exit;
                     end if;
                  end loop;

                  Close (File);

                  if Thread_Options = null then
                     Prj.Com.Fail ("cannot find the thread library in use");
                  end if;

               exception
                  when others =>
                     Prj.Com.Fail ("cannot open s-osinte.ads");
               end;
            end if;

            Thread_Opts := Thread_Options;
            exit;
         end if;
      end loop;

      --  Finally, call GCC (or the driver specified) to build the library

      MLib.Utl.Gcc
        (Output_File => Lib_File,
         Objects     => Ofiles,
         Options     => Options & Bexpall_Option,
         Driver_Name => Driver_Name,
         Options_2   => Options_2 & Thread_Opts.all);
   end Build_Dynamic_Library;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "a";
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

   function Library_Exists_For
     (Project : Project_Id; In_Tree : Project_Tree_Ref) return Boolean
   is
   begin
      if not In_Tree.Projects.Table (Project).Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_Exists_For called " &
                       "for non library project");
         return False;

      else
         declare
            Lib_Dir  : constant String :=
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
                  Fil.Ext_To (Lib_Name, Archive_Ext));

            else
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  Fil.Ext_To (Lib_Name, DLL_Ext));
            end if;
         end;
      end if;
   end Library_Exists_For;

   ---------------------------
   -- Library_File_Name_For --
   ---------------------------

   function Library_File_Name_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Name_Id
   is
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
            Name_Len := 3;
            Name_Buffer (1 .. Name_Len) := "lib";

            if In_Tree.Projects.Table (Project).Library_Kind =
              Static
            then
               Add_Str_To_Name_Buffer (Fil.Ext_To (Lib_Name, Archive_Ext));

            else
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
      return "-fPIC";
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
      return Static_Only;
   end Support_For_Libraries;

end MLib.Tgt;
