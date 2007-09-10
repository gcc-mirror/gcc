------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     M L I B . T G T . S P E C I F I C                    --
--                             (HP-UX Version)                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2007, AdaCore                     --
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

--  This is the HP-UX version of the body

with MLib.Fil;
with MLib.Utl;
with Opt;
with Output; use Output;

package body MLib.Tgt.Specific is

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

   function Is_Archive_Ext (Ext : String) return Boolean;

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
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Auto_Init);

      Lib_File : constant String :=
                   "lib" & Fil.Append_To (Lib_Filename, DLL_Ext);

      Lib_Path : constant String :=
                   Lib_Dir & Directory_Separator & Lib_File;

      Version_Arg          : String_Access;
      Symbolic_Link_Needed : Boolean := False;

      Common_Options : constant Argument_List :=
                         Options & new String'(PIC_Option);
      --  Common set of options to the gcc command performing the link.
      --  On HPUX, this command eventually resorts to collect2, which may
      --  generate a C file and compile it on the fly. This compilation shall
      --  also generate position independant code for the final link to
      --  succeed.
   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_Path);
      end if;

      if Lib_Version = "" then
         MLib.Utl.Gcc
           (Output_File => Lib_Path,
            Objects     => Ofiles,
            Options     => Common_Options,
            Options_2   => No_Argument_List,
            Driver_Name => Driver_Name);

      else
         declare
            Maj_Version : constant String :=
                            Major_Id_Name (Lib_File, Lib_Version);
         begin
            if Maj_Version'Length /= 0 then
               Version_Arg := new String'("-Wl,+h," & Maj_Version);

            else
               Version_Arg := new String'("-Wl,+h," & Lib_Version);
            end if;

            if Is_Absolute_Path (Lib_Version) then
               MLib.Utl.Gcc
                 (Output_File => Lib_Version,
                  Objects     => Ofiles,
                  Options     => Common_Options & Version_Arg,
                  Options_2   => No_Argument_List,
                  Driver_Name => Driver_Name);
               Symbolic_Link_Needed := Lib_Version /= Lib_Path;

            else
               MLib.Utl.Gcc
                 (Output_File => Lib_Dir & Directory_Separator & Lib_Version,
                  Objects     => Ofiles,
                  Options     => Common_Options & Version_Arg,
                  Options_2   => No_Argument_List,
                  Driver_Name => Driver_Name);
               Symbolic_Link_Needed :=
                 Lib_Dir & Directory_Separator & Lib_Version /= Lib_Path;
            end if;

            if Symbolic_Link_Needed then
               Create_Sym_Links
                 (Lib_Path, Lib_Version, Lib_Dir, Maj_Version);
            end if;
         end;
      end if;
   end Build_Dynamic_Library;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "sl";
   end DLL_Ext;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".a" or else Ext = ".so";
   end Is_Archive_Ext;

begin
   Build_Dynamic_Library_Ptr := Build_Dynamic_Library'Access;
   DLL_Ext_Ptr := DLL_Ext'Access;
   Is_Archive_Ext_Ptr := Is_Archive_Ext'Access;
end MLib.Tgt.Specific;
