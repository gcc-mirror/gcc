------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F R O N T E N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
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

with Atree;    use Atree;
with Checks;
with CStand;
with Debug;    use Debug;
with Elists;
with Exp_Ch11;
with Exp_Dbug;
with Fmap;
with Fname.UF;
with Hostparm; use Hostparm;
with Inline;   use Inline;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Live;     use Live;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;
with Output;   use Output;
with Par;
with Rtsfind;
with Sprint;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Elab; use Sem_Elab;
with Sem_Prag; use Sem_Prag;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Types;    use Types;

procedure Frontend is
      Pragmas : List_Id;
      Prag    : Node_Id;

      Save_Style_Check : constant Boolean := Opt.Style_Check;
      --  Save style check mode so it can be restored later

begin
   --  Carry out package initializations. These are initializations which
   --  might logically be performed at elaboration time, were it not for
   --  the fact that we may be doing things more than once in the big loop
   --  over files. Like elaboration, the order in which these calls are
   --  made is in some cases important. For example, Lib cannot be
   --  initialized until Namet, since it uses names table entries.

   Rtsfind.Initialize;
   Atree.Initialize;
   Nlists.Initialize;
   Elists.Initialize;
   Lib.Load.Initialize;
   Sem_Ch8.Initialize;
   Fname.UF.Initialize;
   Exp_Ch11.Initialize;
   Checks.Initialize;

   --  Create package Standard

   CStand.Create_Standard;

   --  Read and process gnat.adc file if one is present

   if Opt.Config_File then

      --  We always analyze the gnat.adc file with style checks off,
      --  since we don't want a miscellaneous gnat.adc that is around
      --  to discombobulate intended -gnatg compilations.

      Opt.Style_Check := False;

      --  Capture current suppress options, which may get modified

      Scope_Suppress := Opt.Suppress_Options;

      Name_Buffer (1 .. 8) := "gnat.adc";
      Name_Len := 8;
      Source_gnat_adc := Load_Config_File (Name_Enter);

      if Source_gnat_adc /= No_Source_File then
         Initialize_Scanner (No_Unit, Source_gnat_adc);
         Pragmas := Par (Configuration_Pragmas => True);

         if Pragmas /= Error_List
           and then Operating_Mode /= Check_Syntax
         then
            Prag := First (Pragmas);
            while Present (Prag) loop
               Analyze_Pragma (Prag);
               Next (Prag);
            end loop;
         end if;
      end if;

      --  Restore style check, but if gnat.adc turned on checks, leave on!

      Opt.Style_Check := Save_Style_Check or Style_Check;

      --  Capture any modifications to suppress options from config pragmas

      Opt.Suppress_Options := Scope_Suppress;
   end if;

   --  Read and process the configuration pragmas file if one is present

   if Config_File_Name /= null then

      declare
         New_Pragmas        : List_Id;
         Style_Check_Saved  : constant Boolean  := Opt.Style_Check;
         Source_Config_File : Source_File_Index := No_Source_File;

      begin
         --  We always analyze the config pragmas file with style checks off,
         --  since we don't want it to discombobulate intended
         --  -gnatg compilations.

         Opt.Style_Check := False;

         --  Capture current suppress options, which may get modified

         Scope_Suppress := Opt.Suppress_Options;

         Name_Buffer (1 .. Config_File_Name'Length) := Config_File_Name.all;
         Name_Len := Config_File_Name'Length;
         Source_Config_File := Load_Config_File (Name_Enter);

         if Source_Config_File = No_Source_File then
            Osint.Fail
              ("cannot find configuration pragmas file ",
               Config_File_Name.all);
         end if;

         Initialize_Scanner (No_Unit, Source_Config_File);
         New_Pragmas := Par (Configuration_Pragmas => True);

         if New_Pragmas /= Error_List
           and then Operating_Mode /= Check_Syntax
         then
            Prag := First (New_Pragmas);
            while Present (Prag) loop
               Analyze_Pragma (Prag);
               Next (Prag);
            end loop;
         end if;

         --  Restore style check, but if the config pragmas file
         --  turned on checks, leave on!

         Opt.Style_Check := Style_Check_Saved or Style_Check;

         --  Capture any modifications to suppress options from config pragmas

         Opt.Suppress_Options := Scope_Suppress;
      end;

   end if;

   --  If there was a -gnatem switch, initialize the mappings of unit names to
   --  file names and of file names to path names from the mapping file.

   if Mapping_File_Name /= null then
      Fmap.Initialize (Mapping_File_Name.all);
   end if;

   --  We have now processed the command line switches, and the gnat.adc
   --  file, so this is the point at which we want to capture the values
   --  of the configuration switches (see Opt for further details).

   Opt.Register_Opt_Config_Switches;

   --  Initialize the scanner. Note that we do this after the call to
   --  Create_Standard, which uses the scanner in its processing of
   --  floating-point bounds.

   Initialize_Scanner (Main_Unit, Source_Index (Main_Unit));

   --  Output header if in verbose mode or full list mode

   if Verbose_Mode or Full_List then
      Write_Eol;

      if Operating_Mode = Generate_Code then
         Write_Str ("Compiling: ");
      else
         Write_Str ("Checking: ");
      end if;

      Write_Name (Full_File_Name (Current_Source_File));

      if not Debug_Flag_7 then
         Write_Str (" (source file time stamp: ");
         Write_Time_Stamp (Current_Source_File);
         Write_Char (')');
      end if;

      Write_Eol;
   end if;

   --  Here we call the parser to parse the compilation unit (or units in
   --  the check syntax mode, but in that case we won't go on to the
   --  semantics in any case).

   declare
      Discard : List_Id;

   begin
      Discard := Par (Configuration_Pragmas => False);
   end;

   --  The main unit is now loaded, and subunits of it can be loaded,
   --  without reporting spurious loading circularities.

   Set_Loading (Main_Unit, False);

   --  Now on to the semantics. We skip the semantics if we are in syntax
   --  only mode, or if we encountered a fatal error during the parsing.

   if Operating_Mode /= Check_Syntax
     and then not Fatal_Error (Main_Unit)
   then
      --  Reset Operating_Mode to Check_Semantics for subunits. We cannot
      --  actually generate code for subunits, so we suppress expansion.
      --  This also corrects certain problems that occur if we try to
      --  incorporate subunits at a lower level.

      if Operating_Mode = Generate_Code
         and then Nkind (Unit (Cunit (Main_Unit))) = N_Subunit
      then
         Operating_Mode := Check_Semantics;
      end if;

      --  Analyze (and possibly expand) main unit

      Scope_Suppress := Suppress_Options;
      Semantics (Cunit (Main_Unit));

      --  Cleanup processing after completing main analysis

      if Operating_Mode = Generate_Code
         or else (Operating_Mode = Check_Semantics
                   and then Tree_Output)
      then
         Instantiate_Bodies;
      end if;

      if Operating_Mode = Generate_Code then

         if Inline_Processing_Required then
            Analyze_Inlined_Bodies;
         end if;

         --  Remove entities from program that do not have any
         --  execution time references.

         if Debug_Flag_UU then
            Collect_Garbage_Entities;
         end if;

         Check_Elab_Calls;

         --  Build unit exception table. We leave this up to the end to
         --  make sure that all the necessary information is at hand.

         Exp_Ch11.Generate_Unit_Exception_Table;

         --  Save the unit name and list of packages named in Use_Package
         --  clauses for subsequent use in generating a special symbol for
         --  the debugger for certain targets that require this.

         Exp_Dbug.Save_Unitname_And_Use_List
           (Cunit (Main_Unit), Nkind (Unit (Cunit (Main_Unit))));
      end if;

      --  List library units if requested

      if List_Units then
         Lib.List;
      end if;

      --  Output any messages for unreferenced entities

      Output_Unreferenced_Messages;
      Sem_Warn.Check_Unused_Withs;
   end if;

   --  Qualify all entity names in inner packages, package bodies, etc.,
   --  except when compiling for the JVM back end, which depends on
   --  having unqualified names in certain cases and handles the generation
   --  of qualified names when needed.

   if not Java_VM then
      Exp_Dbug.Qualify_All_Entity_Names;
      Exp_Dbug.Generate_Auxiliary_Types;
   end if;

   --  Dump the source now. Note that we do this as soon as the analysis
   --  of the tree is complete, because it is not just a dump in the case
   --  of -gnatD, where it rewrites all source locations in the tree.

   Sprint.Source_Dump;
end Frontend;
