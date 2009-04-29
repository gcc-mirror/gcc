------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2009, AdaCore                     --
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

with MLib.Fil;
with Prj.Com;

with MLib.Tgt.Specific;
pragma Warnings (Off, MLib.Tgt.Specific);
--  MLib.Tgt.Specific is with'ed only for elaboration purposes

package body MLib.Tgt is

   ---------------------
   -- Archive_Builder --
   ---------------------

   function Archive_Builder return String is
   begin
      return Archive_Builder_Ptr.all;
   end Archive_Builder;

   -----------------------------
   -- Archive_Builder_Default --
   -----------------------------

   function Archive_Builder_Default return String is
   begin
      return "ar";
   end Archive_Builder_Default;

   -----------------------------
   -- Archive_Builder_Options --
   -----------------------------

   function Archive_Builder_Options return String_List_Access is
   begin
      return Archive_Builder_Options_Ptr.all;
   end Archive_Builder_Options;

   -------------------------------------
   -- Archive_Builder_Options_Default --
   -------------------------------------

   function Archive_Builder_Options_Default return String_List_Access is
   begin
      return new String_List'(1 => new String'("cr"));
   end Archive_Builder_Options_Default;

   ------------------------------------
   -- Archive_Builder_Append_Options --
   ------------------------------------

   function Archive_Builder_Append_Options return String_List_Access is
   begin
      return Archive_Builder_Append_Options_Ptr.all;
   end Archive_Builder_Append_Options;

   --------------------------------------------
   -- Archive_Builder_Append_Options_Default --
   --------------------------------------------

   function Archive_Builder_Append_Options_Default return String_List_Access is
   begin
      return new String_List'(1 => new String'("q"));
   end Archive_Builder_Append_Options_Default;

   -----------------
   -- Archive_Ext --
   -----------------

   function Archive_Ext return String is
   begin
      return Archive_Ext_Ptr.all;
   end Archive_Ext;

   -------------------------
   -- Archive_Ext_Default --
   -------------------------

   function Archive_Ext_Default return String is
   begin
      return "a";
   end Archive_Ext_Default;

   ---------------------
   -- Archive_Indexer --
   ---------------------

   function Archive_Indexer return String is
   begin
      return Archive_Indexer_Ptr.all;
   end Archive_Indexer;

   -----------------------------
   -- Archive_Indexer_Default --
   -----------------------------

   function Archive_Indexer_Default return String is
   begin
      return "ranlib";
   end Archive_Indexer_Default;

   -----------------------------
   -- Archive_Indexer_Options --
   -----------------------------

   function Archive_Indexer_Options return String_List_Access is
   begin
      return Archive_Indexer_Options_Ptr.all;
   end Archive_Indexer_Options;

   -------------------------------------
   -- Archive_Indexer_Options_Default --
   -------------------------------------

   function Archive_Indexer_Options_Default return String_List_Access is
   begin
      return new String_List (1 .. 0);
   end Archive_Indexer_Options_Default;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False)
   is
   begin
      Build_Dynamic_Library_Ptr
        (Ofiles,
         Options,
         Interfaces,
         Lib_Filename,
         Lib_Dir,
         Symbol_Data,
         Driver_Name,
         Lib_Version,
         Auto_Init);
   end Build_Dynamic_Library;

   ------------------------------
   -- Default_Symbol_File_Name --
   ------------------------------

   function Default_Symbol_File_Name return String is
   begin
      return Default_Symbol_File_Name_Ptr.all;
   end Default_Symbol_File_Name;

   --------------------------------------
   -- Default_Symbol_File_Name_Default --
   --------------------------------------

   function Default_Symbol_File_Name_Default return String is
   begin
      return "";
   end Default_Symbol_File_Name_Default;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return DLL_Ext_Ptr.all;
   end DLL_Ext;

   ---------------------
   -- DLL_Ext_Default --
   ---------------------

   function DLL_Ext_Default return String is
   begin
      return "so";
   end DLL_Ext_Default;

   ----------------
   -- DLL_Prefix --
   ----------------

   function DLL_Prefix return String is
   begin
      return DLL_Prefix_Ptr.all;
   end DLL_Prefix;

   ------------------------
   -- DLL_Prefix_Default --
   ------------------------

   function DLL_Prefix_Default return String is
   begin
      return "lib";
   end DLL_Prefix_Default;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return Dynamic_Option_Ptr.all;
   end Dynamic_Option;

   ----------------------------
   -- Dynamic_Option_Default --
   ----------------------------

   function Dynamic_Option_Default return String is
   begin
      return "-shared";
   end Dynamic_Option_Default;

   -------------------
   -- Is_Object_Ext --
   -------------------

   function Is_Object_Ext (Ext : String) return Boolean is
   begin
      return Is_Object_Ext_Ptr (Ext);
   end Is_Object_Ext;

   ---------------------------
   -- Is_Object_Ext_Default --
   ---------------------------

   function Is_Object_Ext_Default (Ext : String) return Boolean is
   begin
      return Ext = ".o";
   end Is_Object_Ext_Default;

   --------------
   -- Is_C_Ext --
   --------------

   function Is_C_Ext (Ext : String) return Boolean is
   begin
      return Is_C_Ext_Ptr (Ext);
   end Is_C_Ext;

   ----------------------
   -- Is_C_Ext_Default --
   ----------------------

   function Is_C_Ext_Default (Ext : String) return Boolean is
   begin
      return Ext = ".c";
   end Is_C_Ext_Default;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Is_Archive_Ext_Ptr (Ext);
   end Is_Archive_Ext;

   ----------------------------
   -- Is_Archive_Ext_Default --
   ----------------------------

   function Is_Archive_Ext_Default (Ext : String) return Boolean is
   begin
      return Ext = ".a";
   end Is_Archive_Ext_Default;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
   begin
      return Libgnat_Ptr.all;
   end Libgnat;

   ---------------------
   -- Libgnat_Default --
   ---------------------

   function Libgnat_Default return String is
   begin
      return "libgnat.a";
   end Libgnat_Default;

   ------------------------
   -- Library_Exists_For --
   ------------------------

   function Library_Exists_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Boolean
   is
   begin
      return Library_Exists_For_Ptr (Project, In_Tree);
   end Library_Exists_For;

   --------------------------------
   -- Library_Exists_For_Default --
   --------------------------------

   function Library_Exists_For_Default
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return Boolean
   is
      pragma Unreferenced (In_Tree);

   begin
      if not Project.Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_Exists_For called " &
                       "for non library project");
         return False;

      else
         declare
            Lib_Dir  : constant String :=
                         Get_Name_String (Project.Library_Dir.Name);
            Lib_Name : constant String :=
                         Get_Name_String (Project.Library_Name);

         begin
            if Project.Library_Kind = Static then
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  Fil.Append_To (Lib_Name, Archive_Ext));

            else
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & DLL_Prefix &
                  Fil.Append_To (Lib_Name, DLL_Ext));
            end if;
         end;
      end if;
   end Library_Exists_For_Default;

   ---------------------------
   -- Library_File_Name_For --
   ---------------------------

   function Library_File_Name_For
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return File_Name_Type
   is
   begin
      return Library_File_Name_For_Ptr (Project, In_Tree);
   end Library_File_Name_For;

   -----------------------------------
   -- Library_File_Name_For_Default --
   -----------------------------------

   function Library_File_Name_For_Default
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref) return File_Name_Type
   is
      pragma Unreferenced (In_Tree);
   begin
      if not Project.Library then
         Prj.Com.Fail ("INTERNAL ERROR: Library_File_Name_For called " &
                       "for non library project");
         return No_File;

      else
         declare
            Lib_Name : constant String :=
                         Get_Name_String (Project.Library_Name);

         begin
            if Project.Library_Kind =
                 Static
            then
               Name_Len := 3;
               Name_Buffer (1 .. Name_Len) := "lib";
               Add_Str_To_Name_Buffer (Fil.Append_To (Lib_Name, Archive_Ext));
            else
               Name_Len := 0;
               Add_Str_To_Name_Buffer (DLL_Prefix);
               Add_Str_To_Name_Buffer (Fil.Append_To (Lib_Name, DLL_Ext));
            end if;

            return Name_Find;
         end;
      end if;
   end Library_File_Name_For_Default;

   --------------------------------------
   -- Library_Major_Minor_Id_Supported --
   --------------------------------------

   function Library_Major_Minor_Id_Supported return Boolean is
   begin
      return Library_Major_Minor_Id_Supported_Ptr.all;
   end Library_Major_Minor_Id_Supported;

   ----------------------------------------------
   -- Library_Major_Minor_Id_Supported_Default --
   ----------------------------------------------

   function Library_Major_Minor_Id_Supported_Default return Boolean is
   begin
      return True;
   end Library_Major_Minor_Id_Supported_Default;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return Object_Ext_Ptr.all;
   end Object_Ext;

   ------------------------
   -- Object_Ext_Default --
   ------------------------

   function Object_Ext_Default return String is
   begin
      return "o";
   end Object_Ext_Default;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return PIC_Option_Ptr.all;
   end PIC_Option;

   ------------------------
   -- PIC_Option_Default --
   ------------------------

   function PIC_Option_Default return String is
   begin
      return "-fPIC";
   end PIC_Option_Default;

   -----------------------------------------------
   -- Standalone_Library_Auto_Init_Is_Supported --
   -----------------------------------------------

   function Standalone_Library_Auto_Init_Is_Supported return Boolean is
   begin
      return Standalone_Library_Auto_Init_Is_Supported_Ptr.all;
   end Standalone_Library_Auto_Init_Is_Supported;

   -------------------------------------------------------
   -- Standalone_Library_Auto_Init_Is_Supported_Default --
   -------------------------------------------------------

   function Standalone_Library_Auto_Init_Is_Supported_Default return Boolean is
   begin
      return True;
   end Standalone_Library_Auto_Init_Is_Supported_Default;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return Support_For_Libraries_Ptr.all;
   end Support_For_Libraries;

   -----------------------------------
   -- Support_For_Libraries_Default --
   -----------------------------------

   function Support_For_Libraries_Default return Library_Support is
   begin
      return Full;
   end Support_For_Libraries_Default;

end MLib.Tgt;
