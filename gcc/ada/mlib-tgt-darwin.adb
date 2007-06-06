------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     M L I B . T G T . S P E C I F I C                    --
--                             (Darwin Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2007, Free Software Foundation, Inc.         --
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

--  This is the Darwin version of the body

with MLib;     use MLib;
with MLib.Fil;
with MLib.Utl;
with Opt;      use Opt;
with Output;   use Output;

with System;

package body MLib.Tgt.Specific is

   --  Non default subprograms

   function Archive_Indexer_Options return String_List_Access;

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

   function DLL_Ext return String;

   function Dynamic_Option return String;

   function Is_Archive_Ext (Ext : String) return Boolean;

   --  Local objects

   Flat_Namespace : aliased String := "-Wl,-flat_namespace";
   --  Instruct the linker to build the shared library as a flat
   --  namespace image. The default is a two-level namespace image.

   Shared_Libgcc  : aliased String := "-shared-libgcc";

   Shared_Options : constant Argument_List :=
                               (1 => Flat_Namespace'Access,
                                2 => Shared_Libgcc'Access);

   -----------------------------
   -- Archive_Indexer_Options --
   -----------------------------

   function Archive_Indexer_Options return String_List_Access is
   begin
      return new String_List'(1 => new String'("-c"));
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
      pragma Unreferenced (Auto_Init);

      Lib_File : constant String :=
                   Lib_Dir & Directory_Separator & "lib" &
                   Fil.Append_To (Lib_Filename, DLL_Ext);

      Symbolic_Link_Needed : Boolean := False;

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      --  If specified, add automatic elaboration/finalization

      if Lib_Version = "" then
         Utl.Gcc
           (Output_File => Lib_File,
            Objects     => Ofiles,
            Options     => Options & Shared_Options,
            Driver_Name => Driver_Name,
            Options_2   => Options_2);

      else

         if Is_Absolute_Path (Lib_Version) then
            Utl.Gcc
              (Output_File => Lib_Version,
               Objects     => Ofiles,
               Options     => Options & Shared_Options,
               Driver_Name => Driver_Name,
               Options_2   => Options_2);
            Symbolic_Link_Needed := Lib_Version /= Lib_File;

         else
            Utl.Gcc
              (Output_File => Lib_Dir & Directory_Separator & Lib_Version,
               Objects     => Ofiles,
               Options     => Options & Shared_Options,
               Driver_Name => Driver_Name,
               Options_2   => Options_2);
            Symbolic_Link_Needed :=
              Lib_Dir & Directory_Separator & Lib_Version /= Lib_File;
         end if;

         if Symbolic_Link_Needed then
            declare
               Success : Boolean;
               Oldpath : String (1 .. Lib_Version'Length + 1);
               Newpath : String (1 .. Lib_File'Length + 1);

               Result : Integer;
               pragma Unreferenced (Result);

               function Symlink
                 (Oldpath : System.Address;
                  Newpath : System.Address) return Integer;
               pragma Import (C, Symlink, "__gnat_symlink");

            begin
               Oldpath (1 .. Lib_Version'Length) := Lib_Version;
               Oldpath (Oldpath'Last)            := ASCII.NUL;
               Newpath (1 .. Lib_File'Length)    := Lib_File;
               Newpath (Newpath'Last)            := ASCII.NUL;

               Delete_File (Lib_File, Success);

               Result := Symlink (Oldpath'Address, Newpath'Address);
            end;
         end if;
      end if;
   end Build_Dynamic_Library;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "dylib";
   end DLL_Ext;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return "-dynamiclib";
   end Dynamic_Option;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".dylib" or else Ext = ".a";
   end Is_Archive_Ext;

begin
   Archive_Indexer_Options_Ptr := Archive_Indexer_Options'Access;
   Build_Dynamic_Library_Ptr := Build_Dynamic_Library'Access;
   DLL_Ext_Ptr := DLL_Ext'Access;
   Dynamic_Option_Ptr := Dynamic_Option'Access;
   Is_Archive_Ext_Ptr := Is_Archive_Ext'Access;
end MLib.Tgt.Specific;
