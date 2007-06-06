------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    M L I B . T G T . S P E C I F I C                     --
--                           (GNU/Linux Version)                            --
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

--  This is the GNU/Linux version of the body

with MLib.Fil;
with MLib.Utl;
with Opt;
with Output; use Output;
with System;

package body MLib.Tgt.Specific is

   use MLib;

   --  Non default subprograms

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

   function Is_Archive_Ext (Ext : String) return Boolean;

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
      --  Initialization is done through the contructor mechanism

      Lib_File : constant String :=
                   "lib" & Fil.Append_To (Lib_Filename, DLL_Ext);

      Lib_Path : constant String :=
                   Lib_Dir & Directory_Separator & Lib_File;

      Version_Arg          : String_Access;
      Symbolic_Link_Needed : Boolean := False;

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_Path);
      end if;

      if Lib_Version = "" then
         Utl.Gcc
           (Output_File => Lib_Path,
            Objects     => Ofiles,
            Options     => Options,
            Driver_Name => Driver_Name,
            Options_2   => Options_2);

      else
         declare
            Maj_Version : constant String := Lib_Version;
            Last_Maj    : Positive := Maj_Version'Last;
            Last        : Positive;
            Ok_Maj      : Boolean := False;
         begin
            while Last_Maj > Maj_Version'First loop
               if Maj_Version (Last_Maj) in '0' .. '9' then
                  Last_Maj := Last_Maj - 1;

               else
                  Ok_Maj := Last_Maj /= Maj_Version'Last and then
                            Maj_Version (Last_Maj) = '.';

                  if Ok_Maj then
                     Last_Maj := Last_Maj - 1;
                  end if;

                  exit;
               end if;
            end loop;

            if Ok_Maj then
               Last := Last_Maj;

               while Last > Maj_Version'First loop
                  if Maj_Version (Last) in '0' .. '9' then
                     Last := Last - 1;

                  else
                     Ok_Maj := Last /= Last_Maj and then
                               Maj_Version (Last) = '.';

                     if Ok_Maj then
                        Last := Last - 1;

                        Ok_Maj := Maj_Version (1 .. Last) = Lib_File;
                     end if;

                     exit;
                  end if;
               end loop;
            end if;

            if Ok_Maj then
               Version_Arg := new String'("-Wl,-soname," &
                                          Maj_Version (1 .. Last_Maj));

            else
               Version_Arg := new String'("-Wl,-soname," & Lib_Version);
            end if;

            if Is_Absolute_Path (Lib_Version) then
               Utl.Gcc
                 (Output_File => Lib_Version,
                  Objects     => Ofiles,
                  Options     => Options & Version_Arg,
                  Driver_Name => Driver_Name,
                  Options_2   => Options_2);
               Symbolic_Link_Needed := Lib_Version /= Lib_Path;

            else
               Utl.Gcc
                 (Output_File => Lib_Dir & Directory_Separator & Lib_Version,
                  Objects     => Ofiles,
                  Options     => Options & Version_Arg,
                  Driver_Name => Driver_Name,
                  Options_2   => Options_2);
               Symbolic_Link_Needed :=
                 Lib_Dir & Directory_Separator & Lib_Version /= Lib_Path;
            end if;

            if Symbolic_Link_Needed then
               declare
                  Success : Boolean;
                  Oldpath : String (1 .. Lib_Version'Length + 1);
                  Newpath : String (1 .. Lib_Path'Length + 1);

                  Result : Integer;
                  pragma Unreferenced (Result);

                  function Symlink
                    (Oldpath : System.Address;
                     Newpath : System.Address) return Integer;
                  pragma Import (C, Symlink, "__gnat_symlink");

               begin
                  Oldpath (1 .. Lib_Version'Length) := Lib_Version;
                  Oldpath (Oldpath'Last)            := ASCII.NUL;
                  Newpath (1 .. Lib_Path'Length)    := Lib_Path;
                  Newpath (Newpath'Last)            := ASCII.NUL;

                  Delete_File (Lib_Path, Success);

                  Result := Symlink (Oldpath'Address, Newpath'Address);
               end;

               if Ok_Maj then
                  declare
                     Success : Boolean;
                     Oldpath : String (1 .. Lib_Version'Length + 1);
                     Maj_Path : constant String :=
                                  Lib_Dir & Directory_Separator &
                                  Maj_Version (1 .. Last_Maj);
                     Newpath : String (1 .. Maj_Path'Length + 1);

                     Result  : Integer;
                     pragma Unreferenced (Result);

                     function Symlink
                       (Oldpath : System.Address;
                        Newpath : System.Address) return Integer;
                     pragma Import (C, Symlink, "__gnat_symlink");

                  begin
                     Oldpath (1 .. Lib_Version'Length) := Lib_Version;
                     Oldpath (Oldpath'Last)            := ASCII.NUL;
                     Newpath (1 .. Maj_Path'Length)    := Maj_Path;
                     Newpath (Newpath'Last)            := ASCII.NUL;

                     Delete_File (Maj_Path, Success);

                     Result := Symlink (Oldpath'Address, Newpath'Address);
                  end;
               end if;

            end if;
         end;
      end if;
   end Build_Dynamic_Library;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".a" or else Ext = ".so";
   end Is_Archive_Ext;

begin
   Build_Dynamic_Library_Ptr := Build_Dynamic_Library'Access;
   Is_Archive_Ext_Ptr := Is_Archive_Ext'Access;
end MLib.Tgt.Specific;
