------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                           (GNU/Linux Version)                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
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

--  This is the GNU/Linux version of the body.

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with MLib.Fil;
with MLib.Utl;
with Namet;       use Namet;
with Opt;
with Osint;       use Osint;
with Output;      use Output;
with System;

package body MLib.Tgt is

   use GNAT;
   use MLib;

   --  ??? serious lack of comments below, all these declarations need to
   --  be commented, none are:

   package Files renames MLib.Fil;
   package Tools renames MLib.Utl;

   Args : Argument_List_Access := new Argument_List (1 .. 20);
   Last_Arg : Natural := 0;

   Cp      : constant String_Access := Locate_Exec_On_Path ("cp");
   Force   : constant String_Access := new String'("-f");

   procedure Add_Arg (Arg : String);

   -------------
   -- Add_Arg --
   -------------

   procedure Add_Arg (Arg : String) is
   begin
      if Last_Arg = Args'Last then
         declare
            New_Args : constant Argument_List_Access :=
                         new Argument_List (1 .. Args'Last * 2);

         begin
            New_Args (Args'Range) := Args.all;
            Args := New_Args;
         end;
      end if;

      Last_Arg := Last_Arg + 1;
      Args (Last_Arg) := new String'(Arg);
   end Add_Arg;

   -----------------
   -- Archive_Ext --
   -----------------

   function Archive_Ext return  String is
   begin
      return  "a";
   end Archive_Ext;

   -----------------
   -- Base_Option --
   -----------------

   function Base_Option return String is
   begin
      return "";
   end Base_Option;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Lib_Address  : String  := "";
      Lib_Version  : String  := "";
      Relocatable  : Boolean := False)
   is
      Lib_File : constant String :=
        Lib_Dir & Directory_Separator & "lib" &
        Files.Ext_To (Lib_Filename, DLL_Ext);

      use type Argument_List;
      use type String_Access;

      Version_Arg  : String_Access;

      Symbolic_Link_Needed : Boolean := False;

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      if Lib_Version = "" then
         Tools.Gcc
           (Output_File => Lib_File,
            Objects     => Ofiles,
            Options     => Options);

      else
         Version_Arg := new String'("-Wl,-soname," & Lib_Version);

         if Is_Absolute_Path (Lib_Version) then
            Tools.Gcc
              (Output_File => Lib_Version,
               Objects     => Ofiles,
               Options     => Options & Version_Arg);
            Symbolic_Link_Needed := Lib_Version /= Lib_File;

         else
            Tools.Gcc
              (Output_File => Lib_Dir & Directory_Separator & Lib_Version,
               Objects     => Ofiles,
               Options     => Options & Version_Arg);
            Symbolic_Link_Needed :=
              Lib_Dir & Directory_Separator & Lib_Version /= Lib_File;
         end if;

         if Symbolic_Link_Needed then
            declare
               Success : Boolean;
               Oldpath : String (1 .. Lib_Version'Length + 1);
               Newpath : String (1 .. Lib_File'Length + 1);
               Result  : Integer;

               function Symlink
                 (Oldpath : System.Address;
                  Newpath : System.Address)
                  return    Integer;
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

   --------------------
   -- Copy_ALI_Files --
   --------------------

   procedure Copy_ALI_Files
     (From : Name_Id;
      To   : Name_Id)
   is
      Dir      : Dir_Type;
      Name     : String (1 .. 1_000);
      Last     : Natural;
      Success  : Boolean;
      From_Dir : constant String := Get_Name_String (From);
      To_Dir   : constant String_Access :=
                   new String'(Get_Name_String (To));

   begin
      Last_Arg := 0;
      Open (Dir, From_Dir);

      loop
         Read (Dir, Name, Last);
         exit when Last = 0;
         if Last > 4

           and then
           To_Lower (Name (Last - 3 .. Last)) = ".ali"
         then
            Add_Arg (From_Dir & Directory_Separator & Name (1 .. Last));
         end if;
      end loop;

      if Last_Arg /= 0 then
         if not Opt.Quiet_Output then
            Write_Str ("cp -f ");

            for J in 1 .. Last_Arg loop
               Write_Str (Args (J).all);
               Write_Char (' ');
            end loop;

            Write_Line (To_Dir.all);
         end if;

         Spawn (Cp.all,
                Force & Args (1 .. Last_Arg) & To_Dir,
                Success);

         if not Success then
            Fail ("could not copy ALI files to library dir");
         end if;
      end if;
   end Copy_ALI_Files;

   -------------------------
   -- Default_DLL_Address --
   -------------------------

   function Default_DLL_Address return String is
   begin
      return "";
   end Default_DLL_Address;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "so";
   end DLL_Ext;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return  "-shared";
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
      return Ext = ".a" or else Ext = ".so";
   end Is_Archive_Ext;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
   begin
      return "libgnat.a";
   end Libgnat;

   -----------------------------
   -- Libraries_Are_Supported --
   -----------------------------

   function Libraries_Are_Supported return Boolean is
   begin
      return True;
   end Libraries_Are_Supported;

   --------------------------------
   -- Linker_Library_Path_Option --
   --------------------------------

   function Linker_Library_Path_Option
     (Directory : String)
      return      String_Access
   is
   begin
      return new String'("-Wl,-rpath," & Directory);
   end Linker_Library_Path_Option;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return  "o";
   end Object_Ext;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return  "-fPIC";
   end PIC_Option;

end MLib.Tgt;
