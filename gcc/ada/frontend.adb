------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F R O N T E N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

with System.Strings; use System.Strings;

with Atree;    use Atree;
with Checks;
with CStand;
with Debug;    use Debug;
with Elists;
with Exp_Dbug;
with Exp_Unst;
with Fmap;
with Fname.UF;
with Ghost;    use Ghost;
with Inline;   use Inline;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Lib.Xref; use Lib.Xref;
with Live;     use Live;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;
with Par;
with Prep;
with Prepcomp;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Snames;   use Snames;
with Sprint;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Aux;
with Sem_Ch8;  use Sem_Ch8;
with Sem_SCIL;
with Sem_Elab; use Sem_Elab;
with Sem_Prag; use Sem_Prag;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with SCIL_LL;  use SCIL_LL;
with Tbuild;   use Tbuild;
with Types;    use Types;

procedure Frontend is
   Config_Pragmas : List_Id;
   --  Gather configuration pragmas

begin
   --  Carry out package initializations. These are initializations which might
   --  logically be performed at elaboration time, were it not for the fact
   --  that we may be doing things more than once in the big loop over files.
   --  Like elaboration, the order in which these calls are made is in some
   --  cases important. For example, Lib cannot be initialized before Namet,
   --  since it uses names table entries.

   Rtsfind.Initialize;
   Nlists.Initialize;
   Elists.Initialize;
   Lib.Load.Initialize;
   Sem_Aux.Initialize;
   Sem_Ch8.Initialize;
   Sem_Prag.Initialize;
   Fname.UF.Initialize;
   Checks.Initialize;
   Sem_Warn.Initialize;
   Prep.Initialize;

   if Generate_SCIL then
      SCIL_LL.Initialize;
   end if;

   --  Create package Standard

   CStand.Create_Standard;

   --  Check possible symbol definitions specified by -gnateD switches

   Prepcomp.Process_Command_Line_Symbol_Definitions;

   --  If -gnatep= was specified, parse the preprocessing data file

   if Preprocessing_Data_File /= null then
      Name_Len := Preprocessing_Data_File'Length;
      Name_Buffer (1 .. Name_Len) := Preprocessing_Data_File.all;
      Prepcomp.Parse_Preprocessing_Data_File (Name_Find);

   --  Otherwise, check if there were preprocessing symbols on the command
   --  line and set preprocessing if there are.

   else
      Prepcomp.Check_Symbols;
   end if;

   --  We set Parsing_Main_Extended_Source true here to cover processing of all
   --  the configuration pragma files, as well as the main source unit itself.

   Parsing_Main_Extended_Source := True;

   --  Now that the preprocessing situation is established, we are able to
   --  load the main source (this is no longer done by Lib.Load.Initialize).

   Lib.Load.Load_Main_Source;

   --  Return immediately if the main source could not be found

   if Sinput.Main_Source_File = No_Source_File then
      return;
   end if;

   --  Read and process configuration pragma files if present

   declare
      Save_Style_Check : constant Boolean := Opt.Style_Check;
      --  Save style check mode so it can be restored later

      Source_Config_File : Source_File_Index;
      --  Source reference for -gnatec configuration file

      Prag : Node_Id;

      Temp_File : Boolean;

   begin
      --  We always analyze config files with style checks off, since we
      --  don't want a miscellaneous gnat.adc that is around to discombobulate
      --  intended -gnatg or -gnaty compilations. We also disconnect checking
      --  for maximum line length.

      Opt.Style_Check := False;
      Style_Check := False;

      --  Capture current suppress options, which may get modified

      Scope_Suppress := Opt.Suppress_Options;

      --  First deal with gnat.adc file

      if Opt.Config_File then
         Name_Buffer (1 .. 8) := "gnat.adc";
         Name_Len := 8;
         Source_gnat_adc := Load_Config_File (Name_Enter);

         --  Case of gnat.adc file present

         if Source_gnat_adc /= No_Source_File then

            --  Parse the gnat.adc file for configuration pragmas

            Initialize_Scanner (No_Unit, Source_gnat_adc);
            Config_Pragmas := Par (Configuration_Pragmas => True);

            --  We unconditionally add a compilation dependency for gnat.adc
            --  so that if it changes, we force a recompilation. This is a
            --  fairly recent (2014-03-28) change.

            Prepcomp.Add_Dependency (Source_gnat_adc);

         --  Case of no gnat.adc file present

         else
            Config_Pragmas := Empty_List;
         end if;

      else
         Config_Pragmas := Empty_List;
      end if;

      --  Now deal with specified config pragmas files if there are any

      if Opt.Config_File_Names /= null then

         --  Loop through config pragmas files

         for Index in Opt.Config_File_Names'Range loop

            --  See if extension is .TMP/.tmp indicating a temporary config
            --  file (which we ignore from the dependency point of view).

            Name_Len := Config_File_Names (Index)'Length;
            Name_Buffer (1 .. Name_Len) := Config_File_Names (Index).all;
            Temp_File :=
              Name_Len > 4
                and then
                  (Name_Buffer (Name_Len - 3 .. Name_Len) = ".TMP"
                     or else
                   Name_Buffer (Name_Len - 3 .. Name_Len) = ".tmp");

            --  Load the file, error if we did not find it

            Source_Config_File := Load_Config_File (Name_Enter);

            if Source_Config_File = No_Source_File then
               Osint.Fail
                 ("cannot find configuration pragmas file "
                  & Config_File_Names (Index).all);

            --  If we did find the file, and it is not a temporary file, then
            --  we unconditionally add a compilation dependency for it so
            --  that if it changes, we force a recompilation. This is a
            --  fairly recent (2014-03-28) change.

            elsif not Temp_File then
               Prepcomp.Add_Dependency (Source_Config_File);
            end if;

            --  Parse the config pragmas file, and accumulate results

            Initialize_Scanner (No_Unit, Source_Config_File);
            Append_List_To
              (Config_Pragmas, Par (Configuration_Pragmas => True));
         end loop;
      end if;

      --  Now analyze all pragmas except those whose analysis must be
      --  deferred till after the main unit is analyzed.

      if Config_Pragmas /= Error_List
        and then Operating_Mode /= Check_Syntax
      then
         Prag := First (Config_Pragmas);
         while Present (Prag) loop
            if not Delay_Config_Pragma_Analyze (Prag) then
               Analyze_Pragma (Prag);
            end if;

            Next (Prag);
         end loop;
      end if;

      --  Restore style check, but if config file turned on checks, leave on

      Opt.Style_Check := Save_Style_Check or Style_Check;

      --  Capture any modifications to suppress options from config pragmas

      Opt.Suppress_Options := Scope_Suppress;
   end;

   --  If a target dependency info file has been read through switch -gnateT=,
   --  add it to the dependencies.

   if Target_Dependent_Info_Read_Name /= null then
      declare
         Index : Source_File_Index;
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Target_Dependent_Info_Read_Name.all);
         Index := Load_Config_File (Name_Enter);
         Prepcomp.Add_Dependency (Index);
      end;
   end if;

   --  This is where we can capture the value of the compilation unit specific
   --  restrictions that have been set by the config pragma files (or from
   --  Targparm), for later restoration when processing e.g. subunits.

   Save_Config_Cunit_Boolean_Restrictions;

   --  If there was a -gnatem switch, initialize the mappings of unit names to
   --  file names and of file names to path names from the mapping file.

   if Mapping_File_Name /= null then
      Fmap.Initialize (Mapping_File_Name.all);
   end if;

   --  Adjust Optimize_Alignment mode from debug switches if necessary

   if Debug_Flag_Dot_SS then
      Optimize_Alignment := 'S';
   elsif Debug_Flag_Dot_TT then
      Optimize_Alignment := 'T';
   end if;

   --  We have now processed the command line switches, and the configuration
   --  pragma files, so this is the point at which we want to capture the
   --  values of the configuration switches (see Opt for further details).

   Opt.Register_Opt_Config_Switches;

   --  Check for file which contains No_Body pragma

   if Source_File_Is_No_Body (Source_Index (Main_Unit)) then
      Change_Main_Unit_To_Spec;
   end if;

   --  Initialize the scanner. Note that we do this after the call to
   --  Create_Standard, which uses the scanner in its processing of
   --  floating-point bounds.

   Initialize_Scanner (Main_Unit, Source_Index (Main_Unit));

   --  Here we call the parser to parse the compilation unit (or units in
   --  the check syntax mode, but in that case we won't go on to the
   --  semantics in any case).

   Discard_List (Par (Configuration_Pragmas => False));
   Parsing_Main_Extended_Source := False;

   --  The main unit is now loaded, and subunits of it can be loaded,
   --  without reporting spurious loading circularities.

   Set_Loading (Main_Unit, False);

   --  Now that the main unit is installed, we can complete the analysis
   --  of the pragmas in gnat.adc and the configuration file, that require
   --  a context for their semantic processing.

   if Config_Pragmas /= Error_List
     and then Operating_Mode /= Check_Syntax

     --  Do not attempt to process deferred configuration pragmas if the main
     --  unit failed to load, to avoid cascaded inconsistencies that can lead
     --  to a compiler crash.

     and then Fatal_Error (Main_Unit) /= Error_Detected
   then
      --  Pragmas that require some semantic activity, such as Interrupt_State,
      --  cannot be processed until the main unit is installed, because they
      --  require a compilation unit on which to attach with_clauses, etc. So
      --  analyze them now.

      declare
         Prag : Node_Id;

      begin
         Prag := First (Config_Pragmas);
         while Present (Prag) loop

            --  Guard against the case where a configuration pragma may be
            --  split into multiple pragmas and the original rewritten as a
            --  null statement.

            if Nkind (Prag) = N_Pragma
              and then Delay_Config_Pragma_Analyze (Prag)
            then
               Analyze_Pragma (Prag);
            end if;

            Next (Prag);
         end loop;
      end;
   end if;

   --  If we have restriction No_Exception_Propagation, and we did not have an
   --  explicit switch turning off Warn_On_Non_Local_Exception, then turn on
   --  this warning by default if we have encountered an exception handler.

   if Restriction_Check_Required (No_Exception_Propagation)
     and then not No_Warn_On_Non_Local_Exception
     and then Exception_Handler_Encountered
   then
      Warn_On_Non_Local_Exception := True;
   end if;

   --  Now on to the semantics. Skip if in syntax only mode

   if Operating_Mode /= Check_Syntax then

      --  Install the configuration pragmas in the tree

      Set_Config_Pragmas (Aux_Decls_Node (Cunit (Main_Unit)), Config_Pragmas);

      --  Following steps are skipped if we had a fatal error during parsing

      if Fatal_Error (Main_Unit) /= Error_Detected then

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

         --  Comment needed for ASIS mode test and GNATprove mode test???

         pragma Assert
           (Operating_Mode = Generate_Code
             or else Operating_Mode = Check_Semantics);

         if Operating_Mode = Generate_Code
           or else (ASIS_Mode or GNATprove_Mode)
         then
            Instantiate_Bodies;
         end if;

         if Operating_Mode = Generate_Code then
            if Inline_Processing_Required then
               Analyze_Inlined_Bodies;
            end if;

            --  Remove entities from program that do not have any execution
            --  time references.

            if Debug_Flag_UU then
               Collect_Garbage_Entities;
            end if;

            Check_Elab_Calls;

            --  Remove any ignored Ghost code as it must not appear in the
            --  executable.

            Remove_Ignored_Ghost_Code;
         end if;

         --  At this stage we can unnest subprogram bodies if required

         Exp_Unst.Unnest_Subprograms (Cunit (Main_Unit));

         --  List library units if requested

         if List_Units then
            Lib.List;
         end if;

         --  Output waiting warning messages

         Lib.Xref.Process_Deferred_References;
         Sem_Warn.Output_Non_Modified_In_Out_Warnings;
         Sem_Warn.Output_Unreferenced_Messages;
         Sem_Warn.Check_Unused_Withs;
         Sem_Warn.Output_Unused_Warnings_Off_Warnings;
      end if;
   end if;

   --  Qualify all entity names in inner packages, package bodies, etc.

   Exp_Dbug.Qualify_All_Entity_Names;

   --  SCIL backend requirement. Check that SCIL nodes associated with
   --  dispatching calls reference subprogram calls.

   if Generate_SCIL then
      pragma Debug (Sem_SCIL.Check_SCIL_Nodes (Cunit (Main_Unit)));
      null;
   end if;

   --  Dump the source now. Note that we do this as soon as the analysis
   --  of the tree is complete, because it is not just a dump in the case
   --  of -gnatD, where it rewrites all source locations in the tree.

   Sprint.Source_Dump;

   --  Check again for configuration pragmas that appear in the context
   --  of the main unit. These pragmas only affect the main unit, and the
   --  corresponding flag is reset after each call to Semantics, but they
   --  may affect the generated ali for the unit, and therefore the flag
   --  must be set properly after compilation. Currently we only check for
   --  Initialize_Scalars, but others should be checked: as well???

   declare
      Item  : Node_Id;

   begin
      Item := First (Context_Items (Cunit (Main_Unit)));
      while Present (Item) loop
         if Nkind (Item) = N_Pragma
           and then Pragma_Name (Item) = Name_Initialize_Scalars
         then
            Initialize_Scalars := True;
         end if;

         Next (Item);
      end loop;
   end;

   --  If a mapping file has been specified by a -gnatem switch, update
   --  it if there has been some sources that were not in the mappings.

   if Mapping_File_Name /= null then
      Fmap.Update_Mapping_File (Mapping_File_Name.all);
   end if;

   return;
end Frontend;
