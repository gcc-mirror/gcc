------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G E N E R A T E _ M I N I M A L _ R E P R O D U C E R          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2024-2025, Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by AdaCore.                        --
--                                                                          --
------------------------------------------------------------------------------

with Fmap;
with Fname.UF;
with Lib;
with Namet; use Namet;
with Osint; use Osint;
with Output; use Output;
with Sinfo.Nodes;
with System.CRTL;
with System.OS_Lib; use System.OS_Lib;
with Types; use Types;

procedure Generate_Minimal_Reproducer is
   Reproducer_Generation_Failed : exception;

   function Create_Reproducer_Directory return String;
   --  Create a directory that will be used to run adareducer, and will
   --  eventually contain the reduced set of sources to be collected by the
   --  user. The name of the directory makes its purpose clear, and it has a
   --  numeric suffix to avoid clashes with other compiler invocations that
   --  might have generated reproducers already.

   ---------------------------------
   -- Create_Reproducer_Directory --
   ---------------------------------

   function Create_Reproducer_Directory return String is
      Max_Id : constant Positive := 1000;

      Prefix : constant String := "reduce-crash-reproducer";

      Result : System.CRTL.int;
   begin
      for Id in 1 .. Max_Id loop
         declare
            Candidate_Path : String := Prefix & Positive'Image (Id);
         begin
            Candidate_Path (Prefix'Length + 1) := '-';

            Result := System.CRTL.mkdir (Candidate_Path & ASCII.NUL);

            --  If mkdir fails, we assume that it's because the directory
            --  already exists. We should check for EEXIST instead???
            if Result = 0 then
               return Candidate_Path;
            end if;
         end;
      end loop;

      Write_Line ("failed to create reproducer directory");
      raise Reproducer_Generation_Failed;
   end Create_Reproducer_Directory;

   Dirname : constant String := Create_Reproducer_Directory;

   Gpr_File_Path : constant String :=
     Dirname & Directory_Separator & "reduce_crash_reproducer.gpr";

   Src_Dir_Path : constant String := Dirname & Directory_Separator & "src";

   Oracle_Path : constant String :=
     Dirname & Directory_Separator & Executable_Name ("oracle");

   Result : Integer;
begin
   Create_Semantic_Closure_Project :
   declare
      Gpr_File : File_Descriptor;

      B : constant Saved_Output_Buffer := Save_Output_Buffer;
   begin
      Gpr_File := Create_File (Gpr_File_Path, Text);
      if Gpr_File = Invalid_FD then
         Write_Line ("failed to create GPR file");
         raise Reproducer_Generation_Failed;
      end if;

      Push_Output;
      Set_Output (Gpr_File);

      Write_Line ("project Reduce_Crash_Reproducer is");
      Write_Line ("   for Source_Dirs use (""src"");");
      Write_Line ("end Reduce_Crash_Reproducer;");

      Close (Gpr_File);
      Pop_Output;
      Restore_Output_Buffer (B);

      Result := System.CRTL.mkdir (Src_Dir_Path & ASCII.NUL);

      if Result /= 0 then
         Write_Line ("failed to create reproducer directory");
         raise Reproducer_Generation_Failed;
      end if;

      for J in Main_Unit .. Lib.Last_Unit loop
         declare
            Path : File_Name_Type :=
              Fmap.Mapped_Path_Name (Lib.Unit_File_Name (J));

            Default_File_Name : constant String :=
              Fname.UF.Get_Default_File_Name (Lib.Unit_Name (J));

            File_Copy_Path : constant String :=
              Src_Dir_Path & Directory_Separator & Default_File_Name;

            --  We may have synthesized units for child subprograms without
            --  spec files. We need to filter out those units because we would
            --  create bogus spec files that break compilation if we didn't.
            Is_Synthetic_Subprogram_Spec : constant Boolean :=
              not Sinfo.Nodes.Comes_From_Source (Lib.Cunit (J));
         begin
            if not Lib.Is_Internal_Unit (J)
              and then not Is_Synthetic_Subprogram_Spec
            then
               --  Mapped_Path_Name might have returned No_File. This has been
               --  observed for files with a Source_File_Name pragma.
               if Path = No_File then
                  Path := Find_File (Lib.Unit_File_Name (J), Osint.Source);
                  pragma Assert (Path /= No_File);
               end if;

               declare
                  File_Path : constant String := Get_Name_String (Path);
                  Success   : Boolean;
               begin
                  System.OS_Lib.Copy_File
                    (File_Path, File_Copy_Path, Success, Overwrite);

                  pragma Assert (Success);
               end;
            end if;
         end;
      end loop;
   end Create_Semantic_Closure_Project;

   Create_Oracle :
   declare
      Gnatmake_Path : String_Access := Locate_Exec_On_Path ("gnatmake");

      Oracle_Dir_Path : constant String :=
        Dirname & Directory_Separator & "oracle-src";

      Source_File_Path : constant String :=
        Oracle_Dir_Path & Directory_Separator & "oracle.adb";

      Source_File : File_Descriptor;

      Result : System.CRTL.int;
   begin
      if Gnatmake_Path = null then
         Write_Line ("-gnatd_m was specified but gnatmake is not available");
         raise Reproducer_Generation_Failed;
      end if;

      Result := System.CRTL.mkdir (Oracle_Dir_Path & ASCII.NUL);

      if Result /= 0 then
         Write_Line ("failed to create directory");
         raise Reproducer_Generation_Failed;
      end if;

      Source_File := Create_File (Source_File_Path, Text);
      if Source_File = Invalid_FD then
         Write_Line ("failed to create oracle source file");
         raise Reproducer_Generation_Failed;
      end if;

      Write_Oracle_Code :
      declare
         Old_Main_Path : constant String :=
           Get_Name_String
             (Fmap.Mapped_Path_Name (Lib.Unit_File_Name (Main_Unit)));

         Default_Main_Name : constant String :=
           Fname.UF.Get_Default_File_Name (Lib.Unit_Name (Main_Unit));

         New_Main_Path : constant String :=
           Src_Dir_Path & Directory_Separator & Default_Main_Name;

         Gnat1_Path : String (1 .. Len_Arg (0));

         B : constant Saved_Output_Buffer := Save_Output_Buffer;
      begin
         Fill_Arg (Gnat1_Path'Address, 0);

         Push_Output;
         Set_Output (Source_File);

         Write_Line ("with Ada.Command_Line;");
         Write_Line ("use Ada.Command_Line;");
         Write_Line ("with GNAT.Expect;");
         Write_Line ("with GNAT.OS_Lib;");
         Write_Eol;
         Write_Line ("procedure Oracle is");
         Write_Line ("   Child_Code : aliased Integer;");
         Write_Eol;
         Write_Line ("   Gnat1_Path : constant String := ");

         Write_Str ("     """);
         Write_Str (Gnat1_Path);
         Write_Line (""";");

         Write_Eol;
         Write_Line ("   Args : constant GNAT.OS_Lib.Argument_List :=");

         Write_Str ("     (new String'(""-gnatd_M"")");

         --  The following way of iterating through the command line arguments
         --  was copied from Set_Targ. TODO factorize???
         declare
            type Arg_Array is array (Nat) of Big_String_Ptr;
            type Arg_Array_Ptr is access Arg_Array;
            --  Types to access compiler arguments

            save_argc : Nat;
            pragma Import (C, save_argc);
            --  Saved value of argc (number of arguments), imported from
            --  misc.cc

            save_argv : Arg_Array_Ptr;
            pragma Import (C, save_argv);
            --  Saved value of argv (argument pointers), imported from misc.cc

            gnat_argc : Nat;
            gnat_argv : Arg_Array_Ptr;
            pragma Import (C, gnat_argc);
            pragma Import (C, gnat_argv);
            --  If save_argv is not set, default to gnat_argc/argv

            argc : Nat;
            argv : Arg_Array_Ptr;

            function Len_Arg (Arg : Big_String_Ptr) return Nat;
            --  Determine length of argument Arg (a nul terminated C string).

            -------------
            -- Len_Arg --
            -------------

            function Len_Arg (Arg : Big_String_Ptr) return Nat is
            begin
               for J in 1 .. Nat'Last loop
                  if Arg (Natural (J)) = ASCII.NUL then
                     return J - 1;
                  end if;
               end loop;

               raise Program_Error;
            end Len_Arg;

         begin
            if save_argv /= null then
               argv := save_argv;
               argc := save_argc;
            else
               --  Case of a non-GCC compiler, e.g. gnat2why or gnat2scil
               argv := gnat_argv;
               argc := gnat_argc;
            end if;

            for Arg in 1 .. argc - 1 loop
               declare
                  Argv_Ptr : constant Big_String_Ptr := argv (Arg);
                  Argv_Len : constant Nat := Len_Arg (Argv_Ptr);

                  Arg : constant String := Argv_Ptr (1 .. Natural (Argv_Len));
               begin
                  --  We filter out mapping file arguments because we want to
                  --  use the copies of source files we made.
                  if Argv_Len > 8 and then Arg (1 .. 8) = "-gnatem=" then
                     null;

                  --  We must not have the oracle run the compiler in
                  --  reduce-on-crash mode, that would result in recursive
                  --  invocations.
                  elsif Arg = "-gnatd_m" then
                     null;
                  else
                     Write_Line (",");
                     Write_Str ("      new String'(""");

                     --  We replace references to the main source file with
                     --  references to the copy we made.
                     if Old_Main_Path = Arg then
                        Write_Str (New_Main_Path);

                     --  We copy the other command line arguments unmodified
                     else
                        Write_Str (Arg);
                     end if;

                     Write_Str (""")");
                  end if;
               end;
            end loop;
         end;

         Write_Line (");");

         Write_Eol;

         Write_Line ("   Output : constant String :=");
         Write_Line ("     GNAT.Expect.Get_Command_Output");
         Write_Str ("       (Gnat1_Path, Args, """", Child_Code'Access, ");
         Write_Line ("Err_To_Out => True);");

         Write_Eol;

         Write_Line ("   Crash_Marker : constant String :=");
         Write_Line ("     ""+===========================GNAT BUG DETECTE"";");

         Write_Eol;

         Write_Line ("   Crashed : constant Boolean :=");
         Write_Line ("     Crash_Marker'Length <= Output'Length");
         Write_Str ("     and then Output (Output'First .. Output'First ");
         Write_Line ("+ Crash_Marker'Length - 1)");
         Write_Line ("              = Crash_Marker;");

         Write_Eol;

         Write_Str ("   Status_Code : Exit_Status := ");
         Write_Line ("(if Crashed then 0 else 1);");
         Write_Line ("begin");
         Write_Line ("   Set_Exit_Status (Status_Code);");
         Write_Line ("end Oracle;");

         Pop_Output;
         Restore_Output_Buffer (B);
      end Write_Oracle_Code;

      Close (Source_File);

      declare
         Args : constant Argument_List :=
           (new String'(Source_File_Path),
            new String'("-o"),
            new String'(Oracle_Path),
            new String'("-D"),
            new String'(Oracle_Dir_Path));

         Success : Boolean;
      begin
         Spawn (Gnatmake_Path.all, Args, Success);

         pragma Assert (Success);
      end;

      Free (Gnatmake_Path);
   end Create_Oracle;

   Run_Adareducer :
   declare
      --  See section 12.8.3 of the GNAT Studio user's guide for documentation
      --  about how to invoke adareducer.
      Gnatstudio_Cli_Path : String_Access :=
        Locate_Exec_On_Path ("gnatstudio_cli");

   begin
      if Gnatstudio_Cli_Path = null then
         Write_Line ("-gnatd_m was specified but adareducer is not available");
         return;
      end if;

      declare
         Args : constant Argument_List :=
           (new String'("adareducer"),
            new String'("-P"),
            new String'(Gpr_File_Path),
            new String'("-s"),
            new String'(Oracle_Path));

         Success : Boolean;
      begin
         Spawn (Gnatstudio_Cli_Path.all, Args, Success);
         pragma Assert (Success);
      end;

      Free (Gnatstudio_Cli_Path);
   end Run_Adareducer;

   Clean_Up_Reproducer_Source :
   declare

      use type System.Address;

      Directory_Stream : System.CRTL.DIRs;

      function opendir (file_name : String) return System.CRTL.DIRs with
        Import, Convention => C, External_Name => "__gnat_opendir";

      Conservative_Name_Max : constant Positive := 4096;

      Buffer : String (1 .. Conservative_Name_Max);
      Length : aliased Integer;

      Addr : System.Address;

      Dummy : Integer;

      Dummy_Success : Boolean;

      function readdir
        (Directory : System.CRTL.DIRs;
         Buffer    : System.Address;
         Length    : access Integer) return System.Address
      with Import, Convention => C, External_Name => "__gnat_readdir";

      function closedir (directory : System.CRTL.DIRs) return Integer with
        Import, Convention => C, External_Name => "__gnat_closedir";

   begin
      Directory_Stream := opendir (Src_Dir_Path & ASCII.NUL);

      if Directory_Stream = System.Null_Address then
         return;
      end if;

      loop
         Addr := readdir (Directory_Stream, Buffer'Address, Length'Access);
         if Addr = System.Null_Address then
            exit;
         end if;

         declare
            S : constant String := Buffer (1 .. Length);
         begin
            if (5 <= S'Length and then S (S'Last - 4 .. S'Last) = ".orig")
              or else (2 <= S'Length and then S (S'Last - 1 .. S'Last) = ".s")
            then
               System.OS_Lib.Delete_File
                 (Src_Dir_Path & Directory_Separator & S, Dummy_Success);
            end if;
         end;
      end loop;

      Dummy := closedir (Directory_Stream);
   end Clean_Up_Reproducer_Source;
end Generate_Minimal_Reproducer;
