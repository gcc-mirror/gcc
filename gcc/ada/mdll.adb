------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M D L L                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  This package provides the core high level routines used by GNATDLL
--  to build Windows DLL.

with Ada.Text_IO;

with GNAT.Directory_Operations;
with MDLL.Utl;
with MDLL.Fil;

package body MDLL is

   use Ada;
   use GNAT;

   --  Convention used for the library names on Windows:
   --  DLL:            <name>.dll
   --  Import library: lib<name>.dll

   function Get_Dll_Name (Lib_Filename : String) return String;
   --  Returns <Lib_Filename> if it contains a file extension otherwise it
   --  returns <Lib_Filename>.dll.

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
      Relocatable   : Boolean := False;
      Map_File      : Boolean := False)
   is

      use type OS_Lib.Argument_List;

      Base_Filename : constant String := MDLL.Fil.Ext_To (Lib_Filename);

      Def_File : aliased constant String := Def_Filename;
      Jnk_File : aliased          String := Base_Filename & ".jnk";
      Bas_File : aliased constant String := Base_Filename & ".base";
      Dll_File : aliased          String := Get_Dll_Name (Lib_Filename);
      Exp_File : aliased          String := Base_Filename & ".exp";
      Lib_File : aliased constant String := "lib" & Base_Filename & ".dll.a";

      Bas_Opt  : aliased String := "-Wl,--base-file," & Bas_File;
      Lib_Opt  : aliased String := "-mdll";
      Out_Opt  : aliased String := "-o";
      Adr_Opt  : aliased String :=
                   (if Relocatable
                    then ""
                    else "-Wl,--image-base=" & Lib_Address);
      Map_Opt  : aliased String := "-Wl,-Map," & Lib_Filename & ".map";

      L_Afiles : Argument_List := Afiles;
      --  Local afiles list. This list can be reordered to ensure that the
      --  binder ALI file is not the first entry in this list.

      All_Options : constant Argument_List := Options & Largs_Options;

      procedure Build_DLL;
      --  Build a relocatable DLL with only objects file specified. This uses
      --  the well known five step build (see GNAT User's Guide).

      procedure Ada_Build_DLL;
      --  Build a relocatable DLL with Ada code. This uses the well known five
      --  step build (see GNAT User's Guide).

      ---------------
      -- Build_DLL --
      ---------------

      procedure Build_DLL is

         Objects_Exp_File : constant OS_Lib.Argument_List :=
                              Exp_File'Unchecked_Access & Ofiles;
         --  Objects plus the export table (.exp) file

         Success : Boolean;
         pragma Warnings (Off, Success);

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

         --  1) Build base file with objects files

         Utl.Gcc (Output_File => Jnk_File,
                  Files       => Ofiles,
                  Options     => All_Options,
                  Base_File   => Bas_File,
                  Build_Lib   => True);

         --  2) Build exp from base file

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => False);

         --  3) Build base file with exp file and objects files

         Utl.Gcc (Output_File => Jnk_File,
                  Files       => Objects_Exp_File,
                  Options     => All_Options,
                  Base_File   => Bas_File,
                  Build_Lib   => True);

         --  4) Build new exp from base file and the lib file (.a)

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => Build_Import);

         --  5) Build the dynamic library

         declare
            Params      : constant OS_Lib.Argument_List :=
                            Map_Opt'Unchecked_Access &
                            Adr_Opt'Unchecked_Access & All_Options;
            First_Param : Positive := Params'First + 1;

         begin
            if Map_File then
               First_Param := Params'First;
            end if;

            Utl.Gcc
              (Output_File => Dll_File,
               Files       => Objects_Exp_File,
               Options     => Params (First_Param .. Params'Last),
               Build_Lib   => True);
         end;

         OS_Lib.Delete_File (Exp_File, Success);
         OS_Lib.Delete_File (Bas_File, Success);
         OS_Lib.Delete_File (Jnk_File, Success);

      exception
         when others =>
            OS_Lib.Delete_File (Exp_File, Success);
            OS_Lib.Delete_File (Bas_File, Success);
            OS_Lib.Delete_File (Jnk_File, Success);
            raise;
      end Build_DLL;

      -------------------
      -- Ada_Build_DLL --
      -------------------

      procedure Ada_Build_DLL is
         Success : Boolean;
         pragma Warnings (Off, Success);

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

         --  1) Build base file with objects files

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params : constant OS_Lib.Argument_List :=
                       Out_Opt'Unchecked_Access &
                       Jnk_File'Unchecked_Access &
                       Lib_Opt'Unchecked_Access &
                       Bas_Opt'Unchecked_Access &
                       Ofiles &
                       All_Options;
         begin
            Utl.Gnatlink (L_Afiles (L_Afiles'Last).all, Params);
         end;

         --  2) Build exp from base file

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => False);

         --  3) Build base file with exp file and objects files

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params : constant OS_Lib.Argument_List :=
                       Out_Opt'Unchecked_Access &
                       Jnk_File'Unchecked_Access &
                       Lib_Opt'Unchecked_Access &
                       Bas_Opt'Unchecked_Access &
                       Exp_File'Unchecked_Access &
                       Ofiles &
                       All_Options;
         begin
            Utl.Gnatlink (L_Afiles (L_Afiles'Last).all, Params);
         end;

         --  4) Build new exp from base file and the lib file (.a)

         Utl.Dlltool (Def_File, Dll_File, Lib_File,
                      Base_File    => Bas_File,
                      Exp_Table    => Exp_File,
                      Build_Import => Build_Import);

         --  5) Build the dynamic library

         Utl.Gnatbind (L_Afiles, Options & Bargs_Options);

         declare
            Params      : constant OS_Lib.Argument_List :=
                            Map_Opt'Unchecked_Access &
                            Out_Opt'Unchecked_Access &
                            Dll_File'Unchecked_Access &
                            Lib_Opt'Unchecked_Access &
                            Exp_File'Unchecked_Access &
                            Adr_Opt'Unchecked_Access &
                            Ofiles &
                            All_Options;
            First_Param : Positive := Params'First + 1;

         begin
            if Map_File then
               First_Param := Params'First;
            end if;

            Utl.Gnatlink
              (L_Afiles (L_Afiles'Last).all,
               Params (First_Param .. Params'Last));
         end;

         OS_Lib.Delete_File (Exp_File, Success);
         OS_Lib.Delete_File (Bas_File, Success);
         OS_Lib.Delete_File (Jnk_File, Success);

      exception
         when others =>
            OS_Lib.Delete_File (Exp_File, Success);
            OS_Lib.Delete_File (Bas_File, Success);
            OS_Lib.Delete_File (Jnk_File, Success);
            raise;
      end Ada_Build_DLL;

   --  Start of processing for Build_Dynamic_Library

   begin
      --  On Windows the binder file must not be in the first position in the
      --  list. This is due to the way DLL's are built on Windows. We swap the
      --  first ali with the last one if it is the case.

      if L_Afiles'Length > 1 then
         declare
            Filename : constant String :=
                         Directory_Operations.Base_Name
                           (L_Afiles (L_Afiles'First).all);
            First    : constant Positive := Filename'First;

         begin
            if Filename (First .. First + 1) = "b~" then
               L_Afiles (L_Afiles'Last) := Afiles (Afiles'First);
               L_Afiles (L_Afiles'First) := Afiles (Afiles'Last);
            end if;
         end;
      end if;

      if L_Afiles'Length = 0 then
         Build_DLL;
      else
         Ada_Build_DLL;
      end if;
   end Build_Dynamic_Library;

   --------------------------
   -- Build_Import_Library --
   --------------------------

   procedure Build_Import_Library
     (Lib_Filename : String;
      Def_Filename : String)
   is
      function Strip_Lib_Prefix (Filename : String) return String;
      --  Return Filename without the lib prefix if present

      ----------------------
      -- Strip_Lib_Prefix --
      ----------------------

      function Strip_Lib_Prefix (Filename : String) return String is
      begin
         if Filename (Filename'First .. Filename'First + 2) = "lib" then
            return Filename (Filename'First + 3 .. Filename'Last);
         else
            return Filename;
         end if;
      end Strip_Lib_Prefix;

      --  Local variables

      Def_File      : String renames Def_Filename;
      Dll_File      : constant String := Get_Dll_Name (Lib_Filename);
      Base_Filename : constant String :=
                        MDLL.Fil.Ext_To (Strip_Lib_Prefix (Lib_Filename));
      Lib_File      : constant String := "lib" & Base_Filename & ".dll.a";

   --  Start of processing for Build_Import_Library

   begin
      if not Quiet then
         Text_IO.Put_Line ("Building import library...");
         Text_IO.Put_Line
           ("make " & Lib_File & " to use dynamic library " & Dll_File);
      end if;

      Utl.Dlltool
        (Def_File, Dll_File, Lib_File, Build_Import => True);
   end Build_Import_Library;

   ------------------
   -- Get_Dll_Name --
   ------------------

   function Get_Dll_Name (Lib_Filename : String) return String is
   begin
      if MDLL.Fil.Get_Ext (Lib_Filename) = "" then
         return Lib_Filename & ".dll";
      else
         return Lib_Filename;
      end if;
   end Get_Dll_Name;

end MDLL;
