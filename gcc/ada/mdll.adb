------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M D L L                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  This package provides the core high level routines used by GNATDLL
--  to build Windows DLL

with Ada.Text_IO;

with MDLL.Tools;
with MDLL.Files;

package body MDLL is

   use Ada;
   use GNAT;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles        : Argument_List;
      Afiles        : Argument_List;
      Options       : Argument_List;
      Bargs_Options : Argument_List;
      Largs_Options : Argument_List;
      Lib_Filename  : String;
      Def_Filename  : String;
      Lib_Address   : String  := "";
      Build_Import  : Boolean := False;
      Relocatable   : Boolean := False)
   is

      use type OS_Lib.Argument_List;

      Base_Filename : constant String := MDLL.Files.Ext_To (Lib_Filename);

      Def_File : aliased String := Def_Filename;
      Jnk_File : aliased String := Base_Filename & ".jnk";
      Bas_File : aliased String := Base_Filename & ".base";
      Dll_File : aliased String := Base_Filename & ".dll";
      Exp_File : aliased String := Base_Filename & ".exp";
      Lib_File : aliased String := "lib" & Base_Filename & ".a";

      Bas_Opt  : aliased String := "-Wl,--base-file," & Bas_File;
      Lib_Opt  : aliased String := "-mdll";
      Out_Opt  : aliased String := "-o";

      All_Options : constant Argument_List := Options & Largs_Options;

      procedure Build_Reloc_DLL;
      --  Build a relocatable DLL with only objects file specified.
      --  this use the well known 5 steps build. (see GNAT User's Guide).

      procedure Ada_Build_Reloc_DLL;
      --  Build a relocatable DLL with Ada code.
      --  this use the well known 5 steps build. (see GNAT User's Guide).

      procedure Build_Non_Reloc_DLL;
      --  Build a non relocatable DLL containing no Ada code.

      procedure Ada_Build_Non_Reloc_DLL;
      --  Build a non relocatable DLL with Ada code.

      ---------------------
      -- Build_Reloc_DLL --
      ---------------------

      procedure Build_Reloc_DLL is

         --  Objects plus the export table (.exp) file

         Objects_Exp_File : OS_Lib.Argument_List
           := Exp_File'Unchecked_Access & Ofiles;

      begin
         if not Quiet then
            Text_IO.Put_Line ("building relocatable DLL...");
            Text_IO.Put ("make " & Dll_File);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  1) Build base file with objects files.

         Tools.Gcc (Output_File => Jnk_File,
                    Files       => Ofiles,
                    Options     => All_Options,
                    Base_File   => Bas_File,
                    Build_Lib   => True);

         --  2) Build exp from base file.

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Base_File    => Bas_File,
                        Exp_Table    => Exp_File,
                        Build_Import => False);

         --  3) Build base file with exp file and objects files.

         Tools.Gcc (Output_File => Jnk_File,
                    Files       => Objects_Exp_File,
                    Options     => All_Options,
                    Base_File   => Bas_File,
                    Build_Lib   => True);

         --  4) Build new exp from base file and the lib file (.a)

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Base_File    => Bas_File,
                        Exp_Table    => Exp_File,
                        Build_Import => Build_Import);

         --  5) Build the dynamic library

         Tools.Gcc (Output_File => Dll_File,
                    Files       => Objects_Exp_File,
                    Options     => All_Options,
                    Build_Lib   => True);

         Tools.Delete_File (Exp_File);
         Tools.Delete_File (Bas_File);
         Tools.Delete_File (Jnk_File);

      exception
         when others =>
            Tools.Delete_File (Exp_File);
            Tools.Delete_File (Bas_File);
            Tools.Delete_File (Jnk_File);
            raise;
      end Build_Reloc_DLL;

      -------------------------
      -- Ada_Build_Reloc_DLL --
      -------------------------

      procedure Ada_Build_Reloc_DLL is
      begin
         if not Quiet then
            Text_IO.Put_Line ("Building relocatable DLL...");
            Text_IO.Put ("make " & Dll_File);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  1) Build base file with objects files.

         Tools.Gnatbind (Afiles, Options & Bargs_Options);

         declare
            Params : OS_Lib.Argument_List :=
              Out_Opt'Unchecked_Access & Jnk_File'Unchecked_Access &
              Lib_Opt'Unchecked_Access &
              Bas_Opt'Unchecked_Access & Ofiles & All_Options;
         begin
            Tools.Gnatlink (Afiles (Afiles'Last).all,
                            Params);
         end;

         --  2) Build exp from base file.

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Base_File    => Bas_File,
                        Exp_Table    => Exp_File,
                        Build_Import => False);

         --  3) Build base file with exp file and objects files.

         Tools.Gnatbind (Afiles, Options & Bargs_Options);

         declare
            Params : OS_Lib.Argument_List :=
              Out_Opt'Unchecked_Access & Jnk_File'Unchecked_Access &
              Lib_Opt'Unchecked_Access &
              Bas_Opt'Unchecked_Access &
              Exp_File'Unchecked_Access &
              Ofiles &
              All_Options;
         begin
            Tools.Gnatlink (Afiles (Afiles'Last).all,
                            Params);
         end;

         --  4) Build new exp from base file and the lib file (.a)

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Base_File    => Bas_File,
                        Exp_Table    => Exp_File,
                        Build_Import => Build_Import);

         --  5) Build the dynamic library

         Tools.Gnatbind (Afiles, Options & Bargs_Options);

         declare
            Params : OS_Lib.Argument_List :=
              Out_Opt'Unchecked_Access & Dll_File'Unchecked_Access &
              Lib_Opt'Unchecked_Access &
              Exp_File'Unchecked_Access &
              Ofiles &
              All_Options;
         begin
            Tools.Gnatlink (Afiles (Afiles'Last).all,
                            Params);
         end;

         Tools.Delete_File (Exp_File);
         Tools.Delete_File (Bas_File);
         Tools.Delete_File (Jnk_File);

      exception
         when others =>
            Tools.Delete_File (Exp_File);
            Tools.Delete_File (Bas_File);
            Tools.Delete_File (Jnk_File);
            raise;
      end Ada_Build_Reloc_DLL;

      -------------------------
      -- Build_Non_Reloc_DLL --
      -------------------------

      procedure Build_Non_Reloc_DLL is
      begin
         if not Quiet then
            Text_IO.Put_Line ("building non relocatable DLL...");
            Text_IO.Put ("make " & Dll_File &
                         " using address " & Lib_Address);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  Build exp table and the lib .a file.

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Exp_Table    => Exp_File,
                        Build_Import => Build_Import);

         --  Build the DLL

         Tools.Gcc (Output_File => Dll_File,
                    Files       => Exp_File'Unchecked_Access & Ofiles,
                    Options     => All_Options,
                    Build_Lib   => True);

         Tools.Delete_File (Exp_File);

      exception
         when others =>
            Tools.Delete_File (Exp_File);
            raise;
      end Build_Non_Reloc_DLL;

      -----------------------------
      -- Ada_Build_Non_Reloc_DLL --
      -----------------------------

      --  Build a non relocatable DLL with Ada code.

      procedure Ada_Build_Non_Reloc_DLL is
      begin
         if not Quiet then
            Text_IO.Put_Line ("building non relocatable DLL...");
            Text_IO.Put ("make " & Dll_File &
                         " using address " & Lib_Address);

            if Build_Import then
               Text_IO.Put_Line (" and " & Lib_File);
            else
               Text_IO.New_Line;
            end if;
         end if;

         --  Build exp table and the lib .a file.

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Exp_Table    => Exp_File,
                        Build_Import => Build_Import);

         --  Build the DLL

         Tools.Gnatbind (Afiles, Options & Bargs_Options);

         declare
            Params : OS_Lib.Argument_List :=
              Out_Opt'Unchecked_Access & Dll_File'Unchecked_Access &
              Lib_Opt'Unchecked_Access &
              Exp_File'Unchecked_Access &
              Ofiles &
              All_Options;
         begin
            Tools.Gnatlink (Afiles (Afiles'Last).all,
                            Params);
         end;

         Tools.Delete_File (Exp_File);

      exception
         when others =>
            Tools.Delete_File (Exp_File);
            raise;
      end Ada_Build_Non_Reloc_DLL;

   begin
      case Relocatable is

         when True =>
            if Afiles'Length = 0 then
               Build_Reloc_DLL;
            else
               Ada_Build_Reloc_DLL;
            end if;

         when False =>
            if Afiles'Length = 0 then
               Build_Non_Reloc_DLL;
            else
               Ada_Build_Non_Reloc_DLL;
            end if;

      end case;
   end Build_Dynamic_Library;

   --------------------------
   -- Build_Import_Library --
   --------------------------

   procedure Build_Import_Library
     (Lib_Filename : String;
      Def_Filename : String)
   is

      procedure Build_Import_Library (Def_Base_Filename : String);
      --  Build an import library.
      --  this is to build only a .a library to link against a DLL.

      Base_Filename : constant String := MDLL.Files.Ext_To (Lib_Filename);

      --------------------------
      -- Build_Import_Library --
      --------------------------

      procedure Build_Import_Library (Def_Base_Filename : String) is

         Def_File : String renames Def_Filename;
         Dll_File : constant String := Def_Base_Filename & ".dll";
         Lib_File : constant String := "lib" & Base_Filename & ".a";

      begin

         if not Quiet then
            Text_IO.Put_Line ("Building import library...");
            Text_IO.Put_Line ("make " & Lib_File &
                              " to use dynamic library " & Dll_File);
         end if;

         Tools.Dlltool (Def_File, Dll_File, Lib_File,
                        Build_Import => True);
      end Build_Import_Library;

   begin
      --  If the library has the form lib<name>.a then the def file should
      --  be <name>.def and the DLL to link against <name>.dll
      --  this is a Windows convention and we try as much as possible to
      --  follow the platform convention.

      if Lib_Filename'Length > 3 and then Lib_Filename (1 .. 3) = "lib" then
         Build_Import_Library (Base_Filename (4 .. Base_Filename'Last));
      else
         Build_Import_Library (Base_Filename);
      end if;
   end Build_Import_Library;

end MDLL;
