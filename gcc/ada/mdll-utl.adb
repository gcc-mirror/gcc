------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M D L L . T O O L S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

--  Interface to externals tools used to build DLL and import libraries

with Ada.Text_IO;
with Ada.Exceptions;

with GNAT.Directory_Operations;
with Osint;

package body MDLL.Utl is

   use Ada;
   use GNAT;

   Dlltool_Name  : constant String := "dlltool";
   Dlltool_Exec  : OS_Lib.String_Access;

   Gcc_Name      : constant String := "gcc";
   Gcc_Exec      : OS_Lib.String_Access;

   Gnatbind_Name : constant String := "gnatbind";
   Gnatbind_Exec : OS_Lib.String_Access;

   Gnatlink_Name : constant String := "gnatlink";
   Gnatlink_Exec : OS_Lib.String_Access;

   procedure Print_Command
     (Tool_Name : String;
      Arguments : OS_Lib.Argument_List);
   --  display the command run when in Verbose mode

   -------------------
   -- Print_Command --
   -------------------

   procedure Print_Command
     (Tool_Name : String;
      Arguments : OS_Lib.Argument_List)
   is
   begin
      if Verbose then
         Text_IO.Put (Tool_Name);
         for K in Arguments'Range loop
            Text_IO.Put (" " & Arguments (K).all);
         end loop;
         Text_IO.New_Line;
      end if;
   end Print_Command;

   -------------
   -- Dlltool --
   -------------

   procedure Dlltool
     (Def_Filename : String;
      DLL_Name     : String;
      Library      : String;
      Exp_Table    : String := "";
      Base_File    : String := "";
      Build_Import : Boolean)
   is
      Arguments  : OS_Lib.Argument_List (1 .. 11);
      A          : Positive;

      Success    : Boolean;

      Def_Opt    : aliased String := "--def";
      Def_V      : aliased String := Def_Filename;
      Dll_Opt    : aliased String := "--dllname";
      Dll_V      : aliased String := DLL_Name;
      Lib_Opt    : aliased String := "--output-lib";
      Lib_V      : aliased String := Library;
      Exp_Opt    : aliased String := "--output-exp";
      Exp_V      : aliased String := Exp_Table;
      Bas_Opt    : aliased String := "--base-file";
      Bas_V      : aliased String := Base_File;
      No_Suf_Opt : aliased String := "-k";

   begin
      Arguments (1 .. 4) := (1 => Def_Opt'Unchecked_Access,
                             2 => Def_V'Unchecked_Access,
                             3 => Dll_Opt'Unchecked_Access,
                             4 => Dll_V'Unchecked_Access);
      A := 4;

      if Kill_Suffix then
         A := A + 1;
         Arguments (A) := No_Suf_Opt'Unchecked_Access;
      end if;

      if Library /= "" and then Build_Import then
         A := A + 1;
         Arguments (A) := Lib_Opt'Unchecked_Access;
         A := A + 1;
         Arguments (A) := Lib_V'Unchecked_Access;
      end if;

      if Exp_Table /= "" then
         A := A + 1;
         Arguments (A) := Exp_Opt'Unchecked_Access;
         A := A + 1;
         Arguments (A) := Exp_V'Unchecked_Access;
      end if;

      if Base_File /= "" then
         A := A + 1;
         Arguments (A) := Bas_Opt'Unchecked_Access;
         A := A + 1;
         Arguments (A) := Bas_V'Unchecked_Access;
      end if;

      Print_Command ("dlltool", Arguments (1 .. A));

      OS_Lib.Spawn (Dlltool_Exec.all, Arguments (1 .. A), Success);

      if not Success then
         Exceptions.Raise_Exception
           (Tools_Error'Identity, Dlltool_Name & " execution error.");
      end if;
   end Dlltool;

   ---------
   -- Gcc --
   ---------

   procedure Gcc
     (Output_File : String;
      Files       : Argument_List;
      Options     : Argument_List;
      Base_File   : String := "";
      Build_Lib   : Boolean := False)
   is
      use Osint;

      Arguments : OS_Lib.Argument_List
        (1 .. 5 + Files'Length + Options'Length);
      A         : Natural := 0;

      Success   : Boolean;
      C_Opt     : aliased String := "-c";
      Out_Opt   : aliased String := "-o";
      Out_V     : aliased String := Output_File;
      Bas_Opt   : aliased String := "-Wl,--base-file," & Base_File;
      Lib_Opt   : aliased String := "-mdll";
      Lib_Dir   : aliased String := "-L" & Object_Dir_Default_Prefix;

   begin
      A := A + 1;
      if Build_Lib then
         Arguments (A) := Lib_Opt'Unchecked_Access;
      else
         Arguments (A) := C_Opt'Unchecked_Access;
      end if;

      A := A + 1;
      Arguments (A .. A + 2) := (Out_Opt'Unchecked_Access,
                                 Out_V'Unchecked_Access,
                                 Lib_Dir'Unchecked_Access);
      A := A + 2;

      if Base_File /= "" then
         A := A + 1;
         Arguments (A) := Bas_Opt'Unchecked_Access;
      end if;

      A := A + 1;
      Arguments (A .. A + Files'Length - 1) := Files;
      A := A + Files'Length - 1;

      if Build_Lib then
         A := A + 1;
         Arguments (A .. A + Options'Length - 1) := Options;
         A := A + Options'Length - 1;
      else
         declare
            Largs : Argument_List (Options'Range);
            L     : Natural := Largs'First - 1;
         begin
            for K in Options'Range loop
               if Options (K) (1 .. 2) /= "-l" then
                  L := L + 1;
                  Largs (L) := Options (K);
               end if;
            end loop;
            A := A + 1;
            Arguments (A .. A + L - 1) := Largs (1 .. L);
            A := A + L - 1;
         end;
      end if;

      Print_Command ("gcc", Arguments (1 .. A));

      OS_Lib.Spawn (Gcc_Exec.all, Arguments (1 .. A), Success);

      if not Success then
         Exceptions.Raise_Exception
           (Tools_Error'Identity, Gcc_Name & " execution error.");
      end if;
   end Gcc;

   --------------
   -- Gnatbind --
   --------------

   procedure Gnatbind
     (Alis : Argument_List;
      Args : Argument_List := Null_Argument_List)
   is
      Arguments   : OS_Lib.Argument_List (1 .. 1 + Alis'Length + Args'Length);
      Success     : Boolean;

      No_Main_Opt : aliased String := "-n";

   begin
      Arguments (1) := No_Main_Opt'Unchecked_Access;
      Arguments (2 .. 1 + Alis'Length) := Alis;
      Arguments (2 + Alis'Length .. Arguments'Last) := Args;

      Print_Command ("gnatbind", Arguments);

      OS_Lib.Spawn (Gnatbind_Exec.all, Arguments, Success);

      --  Delete binder files on failure

      if not Success then
         declare
            Base_Name : constant String :=
              Directory_Operations.Base_Name (Alis (Alis'First).all, ".ali");
         begin
            OS_Lib.Delete_File ("b~" & Base_Name & ".ads", Success);
            OS_Lib.Delete_File ("b~" & Base_Name & ".adb", Success);
         end;

         Exceptions.Raise_Exception
           (Tools_Error'Identity, Gnatbind_Name & " execution error.");
      end if;
   end Gnatbind;

   --------------
   -- Gnatlink --
   --------------

   procedure Gnatlink
     (Ali  : String;
      Args : Argument_List := Null_Argument_List)
   is
      Arguments : OS_Lib.Argument_List (1 .. 1 + Args'Length);
      Success   : Boolean;

      Ali_Name  : aliased String := Ali;

   begin
      Arguments (1) := Ali_Name'Unchecked_Access;
      Arguments (2 .. Arguments'Last) := Args;

      Print_Command ("gnatlink", Arguments);

      OS_Lib.Spawn (Gnatlink_Exec.all, Arguments, Success);

      if not Success then
         --  Delete binder files
         declare
            Base_Name : constant String :=
                          Directory_Operations.Base_Name (Ali, ".ali");
         begin
            OS_Lib.Delete_File ("b~" & Base_Name & ".ads", Success);
            OS_Lib.Delete_File ("b~" & Base_Name & ".adb", Success);
            OS_Lib.Delete_File ("b~" & Base_Name & ".ali", Success);
            OS_Lib.Delete_File ("b~" & Base_Name & ".o", Success);
         end;

         Exceptions.Raise_Exception
           (Tools_Error'Identity, Gnatlink_Name & " execution error.");
      end if;
   end Gnatlink;

   ------------
   -- Locate --
   ------------

   procedure Locate is
      use type OS_Lib.String_Access;
   begin
      --  dlltool

      if Dlltool_Exec = null then
         Dlltool_Exec := OS_Lib.Locate_Exec_On_Path (Dlltool_Name);

         if Dlltool_Exec = null then
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Dlltool_Name & " not found in path");

         elsif Verbose then
            Text_IO.Put_Line ("using " & Dlltool_Exec.all);
         end if;
      end if;

      --  gcc

      if Gcc_Exec = null then
         Gcc_Exec := OS_Lib.Locate_Exec_On_Path (Gcc_Name);

         if Gcc_Exec = null then
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gcc_Name & " not found in path");

         elsif Verbose then
            Text_IO.Put_Line ("using " & Gcc_Exec.all);
         end if;
      end if;

      --  gnatbind

      if Gnatbind_Exec = null then
         Gnatbind_Exec := OS_Lib.Locate_Exec_On_Path (Gnatbind_Name);

         if Gnatbind_Exec = null then
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gnatbind_Name & " not found in path");

         elsif Verbose then
            Text_IO.Put_Line ("using " & Gnatbind_Exec.all);
         end if;
      end if;

      --  gnatlink

      if Gnatlink_Exec = null then
         Gnatlink_Exec := OS_Lib.Locate_Exec_On_Path (Gnatlink_Name);

         if Gnatlink_Exec = null then
            Exceptions.Raise_Exception
              (Tools_Error'Identity, Gnatlink_Name & " not found in path");

         elsif Verbose then
            Text_IO.Put_Line ("using " & Gnatlink_Exec.all);
            Text_IO.New_Line;
         end if;
      end if;
   end Locate;

end MDLL.Utl;
