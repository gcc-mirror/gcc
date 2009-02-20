------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    M L I B . T G T . S P E C I F I C                     --
--                            (Windows Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2008, Free Software Foundation, Inc.         --
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

--  This is the Windows version of the body. Works only with GCC versions
--  supporting the "-shared" option.

with Opt;
with Output; use Output;

with MLib.Fil;
with MLib.Utl;

package body MLib.Tgt.Specific is

   package Files renames MLib.Fil;
   package Tools renames MLib.Utl;

   --  Non default subprograms

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

   function DLL_Ext return String;

   function DLL_Prefix return String;

   function Is_Archive_Ext (Ext : String) return Boolean;

   function Library_Major_Minor_Id_Supported return Boolean;

   function PIC_Option return String;

   No_Argument_List : constant String_List := (1 .. 0 => null);
   --  Used as value of parameter Options or Options2 in calls to Gcc

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
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Lib_Version);
      pragma Unreferenced (Auto_Init);

      Lib_File : constant String :=
                   Lib_Dir & Directory_Separator &
                   DLL_Prefix & Files.Append_To (Lib_Filename, DLL_Ext);

   --  Start of processing for Build_Dynamic_Library

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      Tools.Gcc
        (Output_File => Lib_File,
         Objects     => Ofiles,
         Options     => No_Argument_List,
         Options_2   => Options,
         Driver_Name => Driver_Name);
   end Build_Dynamic_Library;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "dll";
   end DLL_Ext;

   ----------------
   -- DLL_Prefix --
   ----------------

   function DLL_Prefix return String is
   begin
      return "lib";
   end DLL_Prefix;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".a" or else Ext = ".dll";
   end Is_Archive_Ext;

   --------------------------------------
   -- Library_Major_Minor_Id_Supported --
   --------------------------------------

   function Library_Major_Minor_Id_Supported return Boolean is
   begin
      return False;
   end Library_Major_Minor_Id_Supported;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return "";
   end PIC_Option;

begin
   Build_Dynamic_Library_Ptr := Build_Dynamic_Library'Access;
   DLL_Ext_Ptr               := DLL_Ext'Access;
   DLL_Prefix_Ptr            := DLL_Prefix'Access;
   Is_Archive_Ext_Ptr        := Is_Archive_Ext'Access;
   PIC_Option_Ptr            := PIC_Option'Access;
   Library_Major_Minor_Id_Supported_Ptr :=
                                Library_Major_Minor_Id_Supported'Access;
end MLib.Tgt.Specific;
