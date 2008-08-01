------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    M L I B . T G T . S P E C I F I C                     --
--                           (Alpha VMS Version)                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2008, Free Software Foundation, Inc.         --
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

--  This is the Alpha VMS version of the body

with Ada.Characters.Handling; use Ada.Characters.Handling;

with MLib.Fil;
with MLib.Utl;

with MLib.Tgt.VMS_Common;
pragma Warnings (Off, MLib.Tgt.VMS_Common);
--  MLib.Tgt.VMS_Common is with'ed only for elaboration purposes

with Opt;      use Opt;
with Output;   use Output;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with System;           use System;
with System.Case_Util; use System.Case_Util;
with System.CRTL;      use System.CRTL;

package body MLib.Tgt.Specific is

   --  Non default subprogram. See comment in mlib-tgt.ads.

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

   --  Local variables

   Empty_Argument_List : aliased Argument_List := (1 .. 0 => null);
   Additional_Objects  : Argument_List_Access := Empty_Argument_List'Access;
   --  Used to add the generated auto-init object files for auto-initializing
   --  stand-alone libraries.

   Macro_Name : constant String := "mcr gnu:[bin]gcc -c -x assembler";
   --  The name of the command to invoke the macro-assembler

   VMS_Options : Argument_List := (1 .. 1 => null);

   Gnatsym_Name : constant String := "gnatsym";

   Gnatsym_Path : String_Access;

   Arguments : Argument_List_Access := null;
   Last_Argument : Natural := 0;

   Success : Boolean := False;

   Shared_Libgcc : aliased String := "-shared-libgcc";

   Shared_Libgcc_Switch : constant Argument_List :=
                            (1 => Shared_Libgcc'Access);

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
      --  file name of an interface of the SAL. For other libraries, always
      --  return True.

      function Option_File_Name return String;
      --  Returns Symbol_File, if not empty. Otherwise, returns "symvec.opt"

      function Version_String return String;
      --  Returns Lib_Version if not empty and if Symbol_Data.Symbol_Policy is
      --  not Autonomous, otherwise returns "". When Symbol_Data.Symbol_Policy
      --  is Autonomous, fails gnatmake if Lib_Version is not the image of a
      --  positive number.

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
               ALI (ALI'First .. ALI'First + 2) = "b__"
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
         if Symbol_Data.Symbol_File = No_Path then
            return "symvec.opt";
         else
            Get_Name_String (Symbol_Data.Symbol_File);
            To_Lower (Name_Buffer (1 .. Name_Len));
            return Name_Buffer (1 .. Name_Len);
         end if;
      end Option_File_Name;

      --------------------
      -- Version_String --
      --------------------

      function Version_String return String is
         Version : Integer := 0;

      begin
         if Lib_Version = ""
           or else Symbol_Data.Symbol_Policy /= Autonomous
         then
            return "";

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

      ---------------------
      -- Local Variables --
      ---------------------

      Opt_File_Name  : constant String := Option_File_Name;
      Version        : constant String := Version_String;
      For_Linker_Opt : String_Access;

   --  Start of processing for Build_Dynamic_Library

   begin
      --  If option file name does not ends with ".opt", append "/OPTIONS"
      --  to its specification for the VMS linker.

      if Opt_File_Name'Length > 4
        and then
          Opt_File_Name (Opt_File_Name'Last - 3 .. Opt_File_Name'Last) = ".opt"
      then
         For_Linker_Opt := new String'("--for-linker=" & Opt_File_Name);
      else
         For_Linker_Opt :=
           new String'("--for-linker=" & Opt_File_Name & "/OPTIONS");
      end if;

      VMS_Options (VMS_Options'First) := For_Linker_Opt;

      for J in Inter'Range loop
         To_Lower (Inter (J).all);
      end loop;

      --  "gnatsym" is necessary for building the option file

      if Gnatsym_Path = null then
         Gnatsym_Path := Locate_Exec_On_Path (Gnatsym_Name);

         if Gnatsym_Path = null then
            Fail (Gnatsym_Name, " not found in path");
         end if;
      end if;

      --  For auto-initialization of a stand-alone library, we create
      --  a macro-assembly file and we invoke the macro-assembler.

      if Auto_Init then
         declare
            Macro_File_Name : constant String := Lib_Filename & "__init.asm";
            Macro_File      : File_Descriptor;
            Init_Proc       : String := Lib_Filename & "INIT";
            Popen_Result    : System.Address;
            Pclose_Result   : Integer;
            Len             : Natural;
            OK              : Boolean := True;

            command  : constant String :=
                         Macro_Name & " " & Macro_File_Name & ASCII.NUL;
            --  The command to invoke the assembler on the generated auto-init
            --  assembly file.

            mode : constant String := "r" & ASCII.NUL;
            --  The mode for the invocation of Popen

         begin
            To_Upper (Init_Proc);

            if Verbose_Mode then
               Write_Str ("Creating auto-init assembly file """);
               Write_Str (Macro_File_Name);
               Write_Line ("""");
            end if;

            --  Create and write the auto-init assembly file

            declare
               use ASCII;

               --  Output a dummy transfer address for debugging
               --  followed by the LIB$INITIALIZE section.

               Lines : constant String :=
                 HT & ".text" & LF &
                 HT & ".align 4" & LF &
                 HT & ".globl __main" & LF &
                 HT & ".ent __main" & LF &
                 "__main..en:" & LF &
                 HT & ".base $27" & LF &
                 HT & ".frame $29,0,$26,8" & LF &
                 HT & "ret $31,($26),1" & LF &
                 HT & ".link" & LF &
                 "__main:" & LF &
                 HT & ".pdesc __main..en,null" & LF &
                 HT & ".end __main" & LF & LF &
                 HT & ".section LIB$INITIALIZE,GBL,NOWRT" & LF &
                 HT & ".long " & Init_Proc & LF;

            begin
               Macro_File := Create_File (Macro_File_Name, Text);
               OK := Macro_File /= Invalid_FD;

               if OK then
                  Len := Write
                    (Macro_File, Lines (Lines'First)'Address,
                     Lines'Length);
                  OK := Len = Lines'Length;
               end if;

               if OK then
                  Close (Macro_File, OK);
               end if;

               if not OK then
                  Fail ("creation of auto-init assembly file """,
                        Macro_File_Name, """ failed");
               end if;
            end;

            --  Invoke the macro-assembler

            if Verbose_Mode then
               Write_Str ("Assembling auto-init assembly file """);
               Write_Str (Macro_File_Name);
               Write_Line ("""");
            end if;

            Popen_Result := popen (command (command'First)'Address,
                                   mode (mode'First)'Address);

            if Popen_Result = Null_Address then
               Fail ("assembly of auto-init assembly file """,
                     Macro_File_Name, """ failed");
            end if;

            --  Wait for the end of execution of the macro-assembler

            Pclose_Result := pclose (Popen_Result);

            if Pclose_Result < 0 then
               Fail ("assembly of auto init assembly file """,
                     Macro_File_Name, """ failed");
            end if;

            --  Add the generated object file to the list of objects to be
            --  included in the library.

            Additional_Objects :=
              new Argument_List'
                (1 => new String'(Lib_Filename & "__init.obj"));
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

      if Symbol_Data.Reference /= No_Path then
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

         when Restricted =>
            Last_Argument := Last_Argument + 1;
            Arguments (Last_Argument) := new String'("-R");

         when Direct =>
            Last_Argument := Last_Argument + 1;
            Arguments (Last_Argument) := new String'("-D");

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
         Options_2   => Shared_Libgcc_Switch &
                        Opts (Opts'First .. Last_Opt) &
                        Opts2 (Opts2'First .. Last_Opt2),
         Driver_Name => Driver_Name);

      --  The auto-init object file need to be deleted, so that it will not
      --  be included in the library as a regular object file, otherwise
      --  it will be included twice when the library will be built next
      --  time, which may lead to errors.

      if Auto_Init then
         declare
            Auto_Init_Object_File_Name : constant String :=
                                           Lib_Filename & "__init.obj";
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

--  Package initialization

begin
   Build_Dynamic_Library_Ptr    := Build_Dynamic_Library'Access;
end MLib.Tgt.Specific;
