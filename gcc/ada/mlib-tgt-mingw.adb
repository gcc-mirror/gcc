------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                            (Windows Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2004, Free Software Foundation, Inc.         --
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

--  This is the Windows version of the body. Works only with GCC versions
--  supporting the "-shared" option.

with Namet;  use Namet;
with Opt;
with Output; use Output;
with Prj.Com;

with GNAT.OS_Lib; use GNAT.OS_Lib;

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
      pragma Unreferenced (Foreign);
      pragma Unreferenced (Afiles);
      pragma Unreferenced (Auto_Init);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Lib_Version);

      Strip_Name  : constant String := "strip";
      Strip_Exec  : String_Access;

      procedure Strip_Reloc (Lib_File : String);
      --  Strip .reloc section to build a non relocatable DLL

      -----------------
      -- Strip_Reloc --
      -----------------

      procedure Strip_Reloc (Lib_File : String) is
         Arguments   : Argument_List (1 .. 3);
         Success     : Boolean;
         Line_Length : Natural;

      begin
         --  Look for strip executable

         Strip_Exec := Locate_Exec_On_Path (Strip_Name);

         if Strip_Exec = null then
            Fail (Strip_Name, " not found in path");

         elsif Opt.Verbose_Mode then
            Write_Str  ("found ");
            Write_Line (Strip_Exec.all);
         end if;

         --  Call it: strip -R .reloc <dll>

         Arguments (1) := new String'("-R");
         Arguments (2) := new String'(".reloc");
         Arguments (3) := new String'(Lib_File);

         if not Opt.Quiet_Output then
            Write_Str (Strip_Exec.all);
            Line_Length := Strip_Exec'Length;

            for K in Arguments'Range loop

               --  Make sure the Output buffer does not overflow

               if Line_Length + 1 + Arguments (K)'Length >
                 Integer (Opt.Max_Line_Length)
               then
                  Write_Eol;
                  Line_Length := 0;
               end if;

               Write_Char (' ');
               Write_Str  (Arguments (K).all);
               Line_Length := Line_Length + 1 + Arguments (K)'Length;
            end loop;

            Write_Eol;
         end if;

         Spawn (Strip_Exec.all, Arguments, Success);

         if not Success then
            Fail (Strip_Name, " execution error.");
         end if;

         for K in Arguments'Range loop
            Free (Arguments (K));
         end loop;
      end Strip_Reloc;

      Lib_File : constant String :=
        Lib_Dir & Directory_Separator & "lib" &
        Files.Ext_To (Lib_Filename, DLL_Ext);

      I_Base    : aliased String := "-Wl,--image-base," & Lib_Address;

      Options_2 : Argument_List (1 .. 1);
      O_Index   : Natural := 0;

   --  Start of processing for Build_Dynamic_Library

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building ");

         if not Relocatable then
            Write_Str ("non-");
         end if;

         Write_Str ("relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      if not Relocatable then
         O_Index := O_Index + 1;
         Options_2 (O_Index) := I_Base'Unchecked_Access;
      end if;

      Tools.Gcc
        (Output_File => Lib_File,
         Objects     => Ofiles,
         Options     => Options,
         Driver_Name => Driver_Name,
         Options_2   => Options_2 (1 .. O_Index));

      if not Relocatable then

         --  Strip reloc symbols from the DLL

         Strip_Reloc (Lib_File);
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

   function Library_Exists_For (Project : Project_Id) return Boolean is
   begin
      if not Projects.Table (Project).Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_Exists_For called " &
                       "for non library project");
         return False;

      else
         declare
            Lib_Dir : constant String :=
                        Get_Name_String
                          (Projects.Table (Project).Library_Dir);
            Lib_Name : constant String :=
                         Get_Name_String
                           (Projects.Table (Project).Library_Name);

         begin
            if Projects.Table (Project).Library_Kind = Static then
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  MLib.Fil.Ext_To (Lib_Name, Archive_Ext));

            else
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
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
              Get_Name_String (Projects.Table (Project).Library_Name);

         begin
            Name_Len := 3;
            Name_Buffer (1 .. Name_Len) := "lib";

            if Projects.Table (Project).Library_Kind = Static then
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
