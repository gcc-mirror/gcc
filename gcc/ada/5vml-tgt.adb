------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                              (VMS Version)                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2004, Free Software Foundation, Inc.         --
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

--  This is the VMS version of the body

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with MLib.Fil;
with MLib.Utl;
with Namet;             use Namet;
with Opt;               use Opt;
with Output;            use Output;
with Prj.Com;
with System;            use System;
with System.Case_Util;  use System.Case_Util;

package body MLib.Tgt is

   use GNAT;

   Empty_Argument_List : aliased Argument_List := (1 .. 0 => null);
   Additional_Objects  : Argument_List_Access := Empty_Argument_List'Access;
   --  Used to add the generated auto-init object files for auto-initializing
   --  stand-alone libraries.

   Macro_Name   : constant String := "macro";
   --  The name of the command to invoke the macro-assembler

   --  Options to use when invoking gcc to build the dynamic library

   No_Start_Files : aliased String := "-nostartfiles";

   VMS_Options : Argument_List :=
     (No_Start_Files'Access, null);

   Gnatsym_Name : constant String := "gnatsym";

   Gnatsym_Path : String_Access;

   Arguments : Argument_List_Access := null;
   Last_Argument : Natural := 0;

   Success : Boolean := False;

   ------------------------------
   -- Target dependent section --
   ------------------------------

   function Popen (Command, Mode : System.Address) return System.Address;
   pragma Import (C, Popen);

   function Pclose (File : System.Address) return Integer;
   pragma Import (C, Pclose);

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

   function Archive_Ext return String is
   begin
      return "olb";
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
      pragma Unreferenced (Lib_Address);
      pragma Unreferenced (Relocatable);

      Lib_File : constant String :=
                   Lib_Dir & Directory_Separator & "lib" &
                     Fil.Ext_To (Lib_Filename, DLL_Ext);

      Opts      : Argument_List := Options;
      Last_Opt  : Natural       := Opts'Last;
      Opts2     : Argument_List (Options'Range);
      Last_Opt2 : Natural       := Opts2'First - 1;

      Inter : constant Argument_List := Interfaces;

      function Is_Interface (Obj_File : String) return Boolean;
      --  For a Stand-Alone Library, returns True if Obj_File is the object
      --  file name of an interface of the SAL.
      --  For other libraries, always return True.

      function Option_File_Name return String;
      --  Returns Symbol_File, if not empty. Otherwise, returns "symvec.opt"

      function Version_String return String;
      --  Returns Lib_Version if not empty, otherwise returns "1".
      --  Fails gnatmake if Lib_Version is not the image of a positive number.

      ------------------
      -- Is_Interface --
      ------------------

      function Is_Interface (Obj_File : String) return Boolean is
         ALI : constant String :=
                 Fil.Ext_To
                  (Filename => To_Lower (Base_Name (Obj_File)),
                   New_Ext  => "ali");

      begin
         if Inter'Length = 0 then
            return True;

         elsif ALI'Length > 2 and then
               ALI (ALI'First .. ALI'First + 1) = "b$"
         then
            return True;

         else
            for J in Inter'Range loop
               if Inter (J).all = ALI then
                  return True;
               end if;
            end loop;

            return False;
         end if;
      end Is_Interface;

      ----------------------
      -- Option_File_Name --
      ----------------------

      function Option_File_Name return String is
      begin
         if Symbol_Data.Symbol_File = No_Name then
            return "symvec.opt";
         else
            return Get_Name_String (Symbol_Data.Symbol_File);
         end if;
      end Option_File_Name;

      --------------------
      -- Version_String --
      --------------------

      function Version_String return String is
         Version : Integer := 0;
      begin
         if Lib_Version = "" then
            return "1";

         else
            begin
               Version := Integer'Value (Lib_Version);

               if Version <= 0 then
                  raise Constraint_Error;
               end if;

               return Lib_Version;

            exception
               when Constraint_Error =>
                  Fail ("illegal version """, Lib_Version,
                        """ (on VMS version must be a positive number)");
                  return "";
            end;
         end if;
      end Version_String;

      Opt_File_Name  : constant String := Option_File_Name;
      Version        : constant String := Version_String;
      For_Linker_Opt : constant String_Access :=
                         new String'("--for-linker=" & Opt_File_Name);

   --  Start of processing for Build_Dynamic_Library

   begin
      VMS_Options (VMS_Options'First + 1) := For_Linker_Opt;

      for J in Inter'Range loop
         To_Lower (Inter (J).all);
      end loop;

      --  "gnatsym" is necessary for building the option file

      if Gnatsym_Path = null then
         Gnatsym_Path := OS_Lib.Locate_Exec_On_Path (Gnatsym_Name);

         if Gnatsym_Path = null then
            Fail (Gnatsym_Name, " not found in path");
         end if;
      end if;

      --  For auto-initialization of a stand-alone library, we create
      --  a macro-assembly file and we invoke the macro-assembler.

      if Auto_Init then
         declare
            Macro_File_Name : constant String := Lib_Filename & "$init.mar";
            Macro_File      : Ada.Text_IO.File_Type;
            Init_Proc       : String := Lib_Filename & "INIT";
            Popen_Result    : System.Address;
            Pclose_Result   : Integer;

            Command  : constant String :=
                         Macro_Name & " " & Macro_File_Name & ASCII.NUL;
            --  The command to invoke the macro-assembler on the generated
            --  assembly file.

            Mode : constant String := "r" & ASCII.NUL;
            --  The mode for the invocation of Popen

         begin
            To_Upper (Init_Proc);

            if Verbose_Mode then
               Write_Str ("Creating auto-init assembly file """);
               Write_Str (Macro_File_Name);
               Write_Line ("""");
            end if;

            begin
               Create (Macro_File, Out_File, Macro_File_Name);

               Put_Line (Macro_File, ASCII.HT & ".EXTRN LIB$INITIALIZE");
               Put_Line (Macro_File, ASCII.HT & ".EXTRN " & Init_Proc);
               Put_Line
                 (Macro_File,
                  ASCII.HT & ".PSECT LIB$INITIALIZE USR,GBL,NOEXE,NOWRT,LONG");
               Put_Line (Macro_File, ASCII.HT & ".ADDRESS " & Init_Proc);
               Put_Line (Macro_File, ASCII.HT & ".END");

               Close (Macro_File);

            exception
               when others =>
                  Fail ("creation of auto-init assembly file """,
                        Macro_File_Name, """ failed");
            end;

            --  Invoke the macro-assembler

            if Verbose_Mode then
               Write_Str ("Assembling auto-init assembly file """);
               Write_Str (Macro_File_Name);
               Write_Line ("""");
            end if;

            Popen_Result := Popen (Command (Command'First)'Address,
                                   Mode (Mode'First)'Address);

            if Popen_Result = Null_Address then
               Fail ("assembly of auto-init assembly file """,
                     Macro_File_Name, """ failed");
            end if;

            --  Wait for the end of execution of the macro-assembler

            Pclose_Result := Pclose (Popen_Result);

            if Pclose_Result < 0 then
               Fail ("assembly of auto init assembly file """,
                     Macro_File_Name, """ failed");
            end if;

            --  Add the generated object file to the list of objects to be
            --  included in the library.

            Additional_Objects :=
              new Argument_List'
                (1 => new String'(Lib_Filename & "$init.obj"));
         end;
      end if;

      --  Allocate the argument list and put the symbol file name, the
      --  reference (if any) and the policy (if not autonomous).

      Arguments := new Argument_List (1 .. Ofiles'Length + 8);

      Last_Argument := 0;

      --  Verbosity

      if Verbose_Mode then
         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) := new String'("-v");
      end if;

      --  Version number (major ID)

      if Lib_Version /= "" then
         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) := new String'("-V");
         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) := new String'(Version);
      end if;

      --  Symbol file

      Last_Argument := Last_Argument + 1;
      Arguments (Last_Argument) := new String'("-s");
      Last_Argument := Last_Argument + 1;
      Arguments (Last_Argument) := new String'(Opt_File_Name);

      --  Reference Symbol File

      if Symbol_Data.Reference /= No_Name then
         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) := new String'("-r");
         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) :=
           new String'(Get_Name_String (Symbol_Data.Reference));
      end if;

      --  Policy

      case Symbol_Data.Symbol_Policy is
         when Autonomous =>
            null;

         when Compliant =>
            Last_Argument := Last_Argument + 1;
            Arguments (Last_Argument) := new String'("-c");

         when Controlled =>
            Last_Argument := Last_Argument + 1;
            Arguments (Last_Argument) := new String'("-C");
      end case;

      --  Add each relevant object file

      for Index in Ofiles'Range loop
         if Is_Interface (Ofiles (Index).all) then
            Last_Argument := Last_Argument + 1;
            Arguments (Last_Argument) := new String'(Ofiles (Index).all);
         end if;
      end loop;

      --  Spawn gnatsym

      Spawn (Program_Name => Gnatsym_Path.all,
             Args         => Arguments (1 .. Last_Argument),
             Success      => Success);

      if not Success then
         Fail ("unable to create symbol file for library """,
               Lib_Filename, """");
      end if;

      Free (Arguments);

      --  Move all the -l switches from Opts to Opts2

      declare
         Index : Natural := Opts'First;
         Opt   : String_Access;

      begin
         while Index <= Last_Opt loop
            Opt := Opts (Index);

            if Opt'Length > 2 and then
              Opt (Opt'First .. Opt'First + 1) = "-l"
            then
               if Index < Last_Opt then
                  Opts (Index .. Last_Opt - 1) :=
                    Opts (Index + 1 .. Last_Opt);
               end if;

               Last_Opt := Last_Opt - 1;

               Last_Opt2 := Last_Opt2 + 1;
               Opts2 (Last_Opt2) := Opt;

            else
               Index := Index + 1;
            end if;
         end loop;
      end;

      --  Invoke gcc to build the library

      Utl.Gcc
        (Output_File => Lib_File,
         Objects     => Ofiles & Additional_Objects.all,
         Options     => VMS_Options,
         Options_2   => Opts (Opts'First .. Last_Opt) &
                        Opts2 (Opts2'First .. Last_Opt2),
         Driver_Name => Driver_Name);

      --  The auto-init object file need to be deleted, so that it will not
      --  be included in the library as a regular object file, otherwise
      --  it will be included twice when the library will be built next
      --  time, which may lead to errors.

      if Auto_Init then
         declare
            Auto_Init_Object_File_Name : constant String :=
                                           Lib_Filename & "$init.obj";
            Disregard : Boolean;

         begin
            if Verbose_Mode then
               Write_Str ("deleting auto-init object file """);
               Write_Str (Auto_Init_Object_File_Name);
               Write_Line ("""");
            end if;

            Delete_File (Auto_Init_Object_File_Name, Success => Disregard);
         end;
      end if;
   end Build_Dynamic_Library;

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
      return "exe";
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
      return Ext = ".obj";
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
      return Ext = ".olb" or else Ext = ".exe";
   end Is_Archive_Ext;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
      Libgnat_A : constant String := "libgnat.a";
      Libgnat_Olb : constant String := "libgnat.olb";

   begin
      Name_Len := Libgnat_A'Length;
      Name_Buffer (1 .. Name_Len) := Libgnat_A;

      if Osint.Find_File (Name_Enter, Osint.Library) /= No_File then
         return Libgnat_A;

      else
         return Libgnat_Olb;
      end if;
   end Libgnat;

   ------------------------
   -- Library_Exists_For --
   ------------------------

   function Library_Exists_For (Project : Project_Id) return Boolean is
   begin
      if not Projects.Table (Project).Library then
         Fail ("INTERNAL ERROR: Library_Exists_For called " &
               "for non library project");
         return False;

      else
         declare
            Lib_Dir : constant String :=
              Get_Name_String (Projects.Table (Project).Library_Dir);
            Lib_Name : constant String :=
              Get_Name_String (Projects.Table (Project).Library_Name);

         begin
            if Projects.Table (Project).Library_Kind = Static then
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  Fil.Ext_To (Lib_Name, Archive_Ext));

            else
               return Is_Regular_File
                 (Lib_Dir & Directory_Separator & "lib" &
                  Fil.Ext_To (Lib_Name, DLL_Ext));
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

   --------------------------------
   -- Linker_Library_Path_Option --
   --------------------------------

   function Linker_Library_Path_Option return String_Access is
   begin
      return null;
   end Linker_Library_Path_Option;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return "obj";
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
      return True;
   end Standalone_Library_Auto_Init_Is_Supported;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return Full;
   end Support_For_Libraries;

end MLib.Tgt;
