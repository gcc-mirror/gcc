------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2013, Free Software Foundation, Inc.         --
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

--  This package is for switch processing and should not depend on higher level
--  packages such as those for the scanner, parser, etc. Doing so may cause
--  circularities, especially for back ends using Adabkend.

with Debug;    use Debug;
with Lib;      use Lib;
with Osint;    use Osint;
with Opt;      use Opt;
with Stylesw;  use Stylesw;
with Ttypes;   use Ttypes;
with Validsw;  use Validsw;
with Warnsw;   use Warnsw;

with Ada.Unchecked_Deallocation;
with System.WCh_Con; use System.WCh_Con;

package body Switch.C is

   RTS_Specified : String_Access := null;
   --  Used to detect multiple use of --RTS= flag

   procedure Add_Symbol_Definition (Def : String);
   --  Add a symbol definition from the command line

   procedure Free is
      new Ada.Unchecked_Deallocation (String_List, String_List_Access);
   --  Avoid using System.Strings.Free, which also frees the designated strings

   function Get_Overflow_Mode (C : Character) return Overflow_Mode_Type;
   --  Given a digit in the range 0 .. 3, returns the corresponding value of
   --  Overflow_Mode_Type. Raises Program_Error if C is outside this range.

   function Switch_Subsequently_Cancelled
     (C        : String;
      Args     : String_List;
      Arg_Rank : Positive) return Boolean;
   --  This function is called from Scan_Front_End_Switches. It determines if
   --  the switch currently being scanned is followed by a switch of the form
   --  "-gnat-" & C, where C is the argument. If so, then True is returned,
   --  and Scan_Front_End_Switches will cancel the effect of the switch. If
   --  no such switch is found, False is returned.

   ---------------------------
   -- Add_Symbol_Definition --
   ---------------------------

   procedure Add_Symbol_Definition (Def : String) is
   begin
      --  If Preprocessor_Symbol_Defs is not large enough, double its size

      if Preprocessing_Symbol_Last = Preprocessing_Symbol_Defs'Last then
         declare
            New_Symbol_Definitions : constant String_List_Access :=
              new String_List (1 .. 2 * Preprocessing_Symbol_Last);
         begin
            New_Symbol_Definitions (Preprocessing_Symbol_Defs'Range) :=
              Preprocessing_Symbol_Defs.all;
            Free (Preprocessing_Symbol_Defs);
            Preprocessing_Symbol_Defs := New_Symbol_Definitions;
         end;
      end if;

      Preprocessing_Symbol_Last := Preprocessing_Symbol_Last + 1;
      Preprocessing_Symbol_Defs (Preprocessing_Symbol_Last) :=
        new String'(Def);
   end Add_Symbol_Definition;

   -----------------------
   -- Get_Overflow_Mode --
   -----------------------

   function Get_Overflow_Mode (C : Character) return Overflow_Mode_Type is
   begin
      case C is
         when '1' =>
            return Strict;

         when '2' =>
            return Minimized;

         --  Eliminated allowed only if Long_Long_Integer is 64 bits (since
         --  the current implementation of System.Bignums assumes this).

         when '3' =>
            if Standard_Long_Long_Integer_Size /= 64 then
               Bad_Switch ("-gnato3 not implemented for this configuration");
            else
               return Eliminated;
            end if;

         when others =>
            raise Program_Error;
      end case;
   end Get_Overflow_Mode;

   -----------------------------
   -- Scan_Front_End_Switches --
   -----------------------------

   procedure Scan_Front_End_Switches
     (Switch_Chars : String;
      Args         : String_List;
      Arg_Rank     : Positive)
   is
      First_Switch : Boolean := True;
      --  False for all but first switch

      Max : constant Natural := Switch_Chars'Last;
      Ptr : Natural;
      C   : Character := ' ';
      Dot : Boolean;

      Store_Switch : Boolean;
      --  For -gnatxx switches, the normal processing, signalled by this flag
      --  being set to True, is to store the switch on exit from the case
      --  statement, the switch stored is -gnat followed by the characters
      --  from First_Char to Ptr-1. For cases like -gnaty, where the switch
      --  is stored in separate pieces, this flag is set to False, and the
      --  appropriate calls to Store_Compilation_Switch are made from within
      --  the case branch.

      First_Char : Positive;
      --  Marks start of switch to be stored

      First_Ptr : Positive;
      --  Save position of first character after -gnatd (for checking that
      --  debug flags that must come first are first, in particular -gnatd.b),

   begin
      Ptr := Switch_Chars'First;

      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         Bad_Switch (C);
      else
         Ptr := Ptr + 1;
      end if;

      --  Handle switches that do not start with -gnat

      if Ptr + 3 > Max or else Switch_Chars (Ptr .. Ptr + 3) /= "gnat" then

         --  There are two front-end switches that do not start with -gnat:
         --  -I, --RTS

         if Switch_Chars (Ptr) = 'I' then

            --  Set flag Search_Directory_Present if switch is "-I" only:
            --  the directory will be the next argument.

            if Ptr = Max then
               Search_Directory_Present := True;
               return;
            end if;

            Ptr := Ptr + 1;

            --  Find out whether this is a -I- or regular -Ixxx switch

            --  Note: -I switches are not recorded in the ALI file, since the
            --  meaning of the program depends on the source files compiled,
            --  not where they came from.

            if Ptr = Max and then Switch_Chars (Ptr) = '-' then
               Look_In_Primary_Dir := False;
            else
               Add_Src_Search_Dir (Switch_Chars (Ptr .. Max));
            end if;

         --  Processing of the --RTS switch. --RTS may have been modified by
         --  gcc into -fRTS (for GCC targets).

         elsif Ptr + 3 <= Max
           and then (Switch_Chars (Ptr .. Ptr + 3) = "fRTS"
                       or else
                     Switch_Chars (Ptr .. Ptr + 3) = "-RTS")
         then
            Ptr := Ptr + 1;

            if Ptr + 4 > Max
              or else Switch_Chars (Ptr + 3) /= '='
            then
               Osint.Fail ("missing path for --RTS");
            else
               --  Check that this is the first time --RTS is specified or if
               --  it is not the first time, the same path has been specified.

               if RTS_Specified = null then
                  RTS_Specified := new String'(Switch_Chars (Ptr + 4 .. Max));

               elsif
                 RTS_Specified.all /= Switch_Chars (Ptr + 4 .. Max)
               then
                  Osint.Fail ("--RTS cannot be specified multiple times");
               end if;

               --  Valid --RTS switch

               Opt.No_Stdinc := True;
               Opt.RTS_Switch := True;

               RTS_Src_Path_Name :=
                 Get_RTS_Search_Dir
                   (Switch_Chars (Ptr + 4 .. Max), Include);

               RTS_Lib_Path_Name :=
                 Get_RTS_Search_Dir
                   (Switch_Chars (Ptr + 4 .. Max), Objects);

               if RTS_Src_Path_Name /= null
                 and then RTS_Lib_Path_Name /= null
               then
                  --  Store the -fRTS switch (Note: Store_Compilation_Switch
                  --  changes -fRTS back into --RTS for the actual output).

                  Store_Compilation_Switch (Switch_Chars);

               elsif RTS_Src_Path_Name = null
                 and then RTS_Lib_Path_Name = null
               then
                  Osint.Fail ("RTS path not valid: missing " &
                              "adainclude and adalib directories");

               elsif RTS_Src_Path_Name = null then
                  Osint.Fail ("RTS path not valid: missing " &
                              "adainclude directory");

               elsif RTS_Lib_Path_Name = null then
                  Osint.Fail ("RTS path not valid: missing " &
                              "adalib directory");
               end if;
            end if;

            --  There are no other switches not starting with -gnat

         else
            Bad_Switch (Switch_Chars);
         end if;

      --  Case of switch starting with -gnat

      else
         Ptr := Ptr + 4;

         --  Loop to scan through switches given in switch string

         while Ptr <= Max loop
            First_Char := Ptr;
            Store_Switch := True;

            C := Switch_Chars (Ptr);

            case C is

            --  -gnata (assertions enabled)

            when 'a' =>
               Ptr := Ptr + 1;
               Assertions_Enabled := True;

            --  -gnatA (disregard gnat.adc)

            when 'A' =>
               Ptr := Ptr + 1;
               Config_File := False;

            --  -gnatb (brief messages to stderr)

            when 'b' =>
               Ptr := Ptr + 1;
               Brief_Output := True;

            --  -gnatB (assume no invalid values)

            when 'B' =>
               Ptr := Ptr + 1;
               Assume_No_Invalid_Values := True;

            --  -gnatc (check syntax and semantics only)

            when 'c' =>
               if not First_Switch then
                  Osint.Fail
                    ("-gnatc must be first if combined with other switches");
               end if;

               --  Not allowed if previous -gnatR given

               if List_Representation_Info /= 0
                 or else List_Representation_Info_Mechanisms
               then
                  Osint.Fail
                    ("-gnatc not allowed since -gnatR given previously");
               end if;

               Ptr := Ptr + 1;
               Operating_Mode := Check_Semantics;

            --  -gnatC (Generate CodePeer information)

            when 'C' =>
               Ptr := Ptr + 1;

               if not CodePeer_Mode then
                  CodePeer_Mode := True;

                  --  Suppress compiler warnings by default, since what we are
                  --  interested in here is what CodePeer can find out. Note
                  --  that if -gnatwxxx is specified after -gnatC on the
                  --  command line, we do not want to override this setting in
                  --  Adjust_Global_Switches, and assume that the user wants to
                  --  get both warnings from GNAT and CodePeer messages.

                  Warning_Mode := Suppress;
               end if;

            --  -gnatd (compiler debug options)

            when 'd' =>
               Store_Switch := False;
               Dot := False;
               First_Ptr := Ptr + 1;

               --  Note: for the debug switch, the remaining characters in this
               --  switch field must all be debug flags, since all valid switch
               --  characters are also valid debug characters.

               --  Loop to scan out debug flags

               while Ptr < Max loop
                  Ptr := Ptr + 1;
                  C := Switch_Chars (Ptr);
                  exit when C = ASCII.NUL or else C = '/' or else C = '-';

                  if C in '1' .. '9' or else
                     C in 'a' .. 'z' or else
                     C in 'A' .. 'Z'
                  then
                     --  Case of dotted flag

                     if Dot then
                        Set_Dotted_Debug_Flag (C);
                        Store_Compilation_Switch ("-gnatd." & C);

                        --  Special check, -gnatd.b must come first

                        if C = 'b'
                          and then (Ptr /= First_Ptr + 1
                                      or else not First_Switch)
                        then
                           Osint.Fail
                             ("-gnatd.b must be first if combined "
                              & "with other switches");
                        end if;

                     --  Not a dotted flag

                     else
                        Set_Debug_Flag (C);
                        Store_Compilation_Switch ("-gnatd" & C);
                     end if;

                  elsif C = '.' then
                     Dot := True;

                  elsif Dot then
                     Bad_Switch ("-gnatd." & Switch_Chars (Ptr .. Max));
                  else
                     Bad_Switch ("-gnatd" & Switch_Chars (Ptr .. Max));
                  end if;
               end loop;

               return;

            --  -gnatD (debug expanded code)

            when 'D' =>
               Ptr := Ptr + 1;

               --  Not allowed if previous -gnatR given

               --  The reason for this prohibition is that the rewriting of
               --  Sloc values causes strange malfunctions in the tests of
               --  whether units belong to the main source. This is really a
               --  bug, but too hard to fix for a marginal capability ???

               --  The proper fix is to completely redo -gnatD processing so
               --  that the tree is not messed with, and instead a separate
               --  table is built on the side for debug information generation.

               if List_Representation_Info /= 0 then
                  Osint.Fail
                    ("-gnatD not permitted since -gnatR given previously");
               end if;

               --  Scan optional integer line limit value

               if Nat_Present (Switch_Chars, Max, Ptr) then
                  Scan_Nat (Switch_Chars, Max, Ptr, Sprint_Line_Limit, 'D');
                  Sprint_Line_Limit := Nat'Max (Sprint_Line_Limit, 40);
               end if;

               --  Note: -gnatD also sets -gnatx (to turn off cross-reference
               --  generation in the ali file) since otherwise this generation
               --  gets confused by the "wrong" Sloc values put in the tree.

               Debug_Generated_Code := True;
               Xref_Active := False;
               Set_Debug_Flag ('g');

            --  -gnate? (extended switches)

            when 'e' =>
               Ptr := Ptr + 1;

               --  The -gnate? switches are all double character switches
               --  so we must always have a character after the e.

               if Ptr > Max then
                  Bad_Switch ("-gnate");
               end if;

               case Switch_Chars (Ptr) is

                  --  -gnatea (initial delimiter of explicit switches)

                  --  This is an internal switch

                  --  All switches that come before -gnatea have been added by
                  --  the GCC driver and are not stored in the ALI file.
                  --  See also -gnatez below.

                  when 'a' =>
                     Store_Switch := False;
                     Enable_Switch_Storing;
                     Ptr := Ptr + 1;

                  --  -gnateA (aliasing checks on parameters)

                  when 'A' =>
                     Ptr := Ptr + 1;
                     Check_Aliasing_Of_Parameters := True;

                  --  -gnatec (configuration pragmas)

                  when 'c' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;

                     --  There may be an equal sign between -gnatec and
                     --  the path name of the config file.

                     if Ptr <= Max and then Switch_Chars (Ptr) = '=' then
                        Ptr := Ptr + 1;
                     end if;

                     if Ptr > Max then
                        Bad_Switch ("-gnatec");
                     end if;

                     declare
                        Config_File_Name : constant String_Access :=
                                             new String'
                                                  (Switch_Chars (Ptr .. Max));

                     begin
                        if Config_File_Names = null then
                           Config_File_Names :=
                             new String_List'(1 => Config_File_Name);

                        else
                           declare
                              New_Names : constant String_List_Access :=
                                            new String_List
                                              (1 ..
                                               Config_File_Names'Length + 1);

                           begin
                              for Index in Config_File_Names'Range loop
                                 New_Names (Index) :=
                                   Config_File_Names (Index);
                                 Config_File_Names (Index) := null;
                              end loop;

                              New_Names (New_Names'Last) := Config_File_Name;
                              Free (Config_File_Names);
                              Config_File_Names := New_Names;
                           end;
                        end if;
                     end;

                     return;

                  --  -gnateC switch (CodePeer SCIL generation)

                  --  Not enabled for now, keep it for later???
                  --  use -gnatd.I only for now

                  --  when 'C' =>
                  --     Ptr := Ptr + 1;
                  --     Generate_SCIL := True;

                  --  -gnated switch (disable atomic synchronization)

                  when 'd' =>
                     Suppress_Options.Suppress (Atomic_Synchronization) :=
                       True;

                  --  -gnateD switch (preprocessing symbol definition)

                  when 'D' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        Bad_Switch ("-gnateD");
                     end if;

                     Add_Symbol_Definition (Switch_Chars (Ptr .. Max));

                     --  Store the switch

                     Store_Compilation_Switch
                       ("-gnateD" & Switch_Chars (Ptr .. Max));
                     Ptr := Max + 1;

                  --  -gnateE (extra exception information)

                  when 'E' =>
                     Exception_Extra_Info := True;
                     Ptr := Ptr + 1;

                  --  -gnatef (full source path for brief error messages)

                  when 'f' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;
                     Full_Path_Name_For_Brief_Errors := True;

                  --  -gnateF (Check_Float_Overflow)

                  when 'F' =>
                     Ptr := Ptr + 1;
                     Check_Float_Overflow := True;

                  --  -gnateG (save preprocessor output)

                  when 'G' =>
                     Generate_Processed_File := True;
                     Ptr := Ptr + 1;

                  --  -gnatei (max number of instantiations)

                  when 'i' =>
                     Ptr := Ptr + 1;
                     Scan_Pos
                       (Switch_Chars, Max, Ptr, Maximum_Instantiations, C);

                  --  -gnateI (index of unit in multi-unit source)

                  when 'I' =>
                     Ptr := Ptr + 1;
                     Scan_Pos (Switch_Chars, Max, Ptr, Multiple_Unit_Index, C);

                  --  -gnatel

                  when 'l' =>
                     Ptr := Ptr + 1;
                     Elab_Info_Messages := True;

                  --  -gnateL

                  when 'L' =>
                     Ptr := Ptr + 1;
                     Elab_Info_Messages := False;

                  --  -gnatem (mapping file)

                  when 'm' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;

                     --  There may be an equal sign between -gnatem and
                     --  the path name of the mapping file.

                     if Ptr <= Max and then Switch_Chars (Ptr) = '=' then
                        Ptr := Ptr + 1;
                     end if;

                     if Ptr > Max then
                        Bad_Switch ("-gnatem");
                     end if;

                     Mapping_File_Name :=
                       new String'(Switch_Chars (Ptr .. Max));
                     return;

                  --  -gnateO= (object path file)

                  --  This is an internal switch

                  when 'O' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;

                     --  Check for '='

                     if Ptr >= Max or else Switch_Chars (Ptr) /= '=' then
                        Bad_Switch ("-gnateO");
                     else
                        Object_Path_File_Name :=
                          new String'(Switch_Chars (Ptr + 1 .. Max));
                     end if;

                     return;

                  --  -gnatep (preprocessing data file)

                  when 'p' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;

                     --  There may be an equal sign between -gnatep and
                     --  the path name of the mapping file.

                     if Ptr <= Max and then Switch_Chars (Ptr) = '=' then
                        Ptr := Ptr + 1;
                     end if;

                     if Ptr > Max then
                        Bad_Switch ("-gnatep");
                     end if;

                     Preprocessing_Data_File :=
                       new String'(Switch_Chars (Ptr .. Max));

                     --  Store the switch, normalizing to -gnatep=

                     Store_Compilation_Switch
                       ("-gnatep=" & Preprocessing_Data_File.all);

                     Ptr := Max + 1;

                  --  -gnateP (Treat pragma Pure/Preelaborate errs as warnings)

                  when 'P' =>
                     Treat_Categorization_Errors_As_Warnings := True;

                  --  -gnates=file (specify extra file switches for gnat2why)

                  --  This is an internal switch

                  when 's' =>
                     if not First_Switch then
                        Osint.Fail
                          ("-gnates must not be combined with other switches");
                     end if;

                     --  Check for '='

                     Ptr := Ptr + 1;

                     if Ptr >= Max or else Switch_Chars (Ptr) /= '=' then
                        Bad_Switch ("-gnates");
                     else
                        SPARK_Switches_File_Name :=
                          new String'(Switch_Chars (Ptr + 1 .. Max));
                     end if;

                     return;

                  --  -gnateS (generate SCO information)

                  --  Include Source Coverage Obligation information in ALI
                  --  files for use by source coverage analysis tools
                  --  (gnatcov) (equivalent to -fdump-scos, provided for
                  --  backwards compatibility).

                  when 'S' =>
                     Generate_SCO := True;
                     Generate_SCO_Instance_Table := True;
                     Ptr := Ptr + 1;

                  --  -gnatet (write target dependent information)

                  when 't' =>
                     if not First_Switch then
                        Osint.Fail
                          ("-gnatet must not be combined with other switches");
                     end if;

                     --  Check for '='

                     Ptr := Ptr + 1;

                     if Ptr >= Max or else Switch_Chars (Ptr) /= '=' then
                        Bad_Switch ("-gnatet");
                     else
                        Target_Dependent_Info_Write_Name :=
                          new String'(Switch_Chars (Ptr + 1 .. Max));
                     end if;

                     return;

                  --  -gnateT (read target dependent information)

                  when 'T' =>
                     if not First_Switch then
                        Osint.Fail
                          ("-gnateT must not be combined with other switches");
                     end if;

                     --  Check for '='

                     Ptr := Ptr + 1;

                     if Ptr >= Max or else Switch_Chars (Ptr) /= '=' then
                        Bad_Switch ("-gnateT");
                     else
                        --  This parameter was stored by Set_Targ earlier

                        pragma Assert
                          (Target_Dependent_Info_Read_Name.all =
                             Switch_Chars (Ptr + 1 .. Max));
                        null;
                     end if;

                     return;

                  --  -gnateu (unrecognized y,V,w switches)

                  when 'u' =>
                     Ptr := Ptr + 1;
                     Ignore_Unrecognized_VWY_Switches := True;

                  --  -gnateV (validity checks on parameters)

                  when 'V' =>
                     Ptr := Ptr + 1;
                     Check_Validity_Of_Parameters := True;

                  --  -gnateY (ignore Style_Checks pragmas)

                  when 'Y' =>
                     Ignore_Style_Checks_Pragmas := True;
                     Ptr := Ptr + 1;

                  --  -gnatez (final delimiter of explicit switches)

                  --  This is an internal switch

                  --  All switches that come after -gnatez have been added by
                  --  the GCC driver and are not stored in the ALI file. See
                  --  also -gnatea above.

                  when 'z' =>
                     Store_Switch := False;
                     Disable_Switch_Storing;
                     Ptr := Ptr + 1;

                  --  All other -gnate? switches are unassigned

                  when others =>
                     Bad_Switch ("-gnate" & Switch_Chars (Ptr .. Max));
               end case;

            --  -gnatE (dynamic elaboration checks)

            when 'E' =>
               Ptr := Ptr + 1;
               Dynamic_Elaboration_Checks := True;

            --  -gnatf (full error messages)

            when 'f' =>
               Ptr := Ptr + 1;
               All_Errors_Mode := True;

            --  -gnatF (overflow of predefined float types)

            when 'F' =>
               Ptr := Ptr + 1;
               External_Name_Exp_Casing := Uppercase;
               External_Name_Imp_Casing := Uppercase;

            --  -gnatg (GNAT implementation mode)

            when 'g' =>
               Ptr := Ptr + 1;
               GNAT_Mode := True;
               Identifier_Character_Set := 'n';
               System_Extend_Unit := Empty;
               Warning_Mode := Treat_As_Error;
               Style_Check_Main := True;

               --  Set Ada 2012 mode explicitly. We don't want to rely on the
               --  implicit setting here, since for example, we want
               --  Preelaborate_05 treated as Preelaborate

               Ada_Version          := Ada_2012;
               Ada_Version_Explicit := Ada_2012;
               Ada_Version_Pragma   := Empty;

               --  Set default warnings and style checks for -gnatg

               Set_GNAT_Mode_Warnings;
               Set_GNAT_Style_Check_Options;

            --  -gnatG (output generated code)

            when 'G' =>
               Ptr := Ptr + 1;
               Print_Generated_Code := True;

               --  Scan optional integer line limit value

               if Nat_Present (Switch_Chars, Max, Ptr) then
                  Scan_Nat (Switch_Chars, Max, Ptr, Sprint_Line_Limit, 'G');
                  Sprint_Line_Limit := Nat'Max (Sprint_Line_Limit, 40);
               end if;

            --  -gnath (help information)

            when 'h' =>
               Ptr := Ptr + 1;
               Usage_Requested := True;

            --  -gnati (character set)

            when 'i' =>
               if Ptr = Max then
                  Bad_Switch ("-gnati");
               end if;

               Ptr := Ptr + 1;
               C := Switch_Chars (Ptr);

               if C in '1' .. '5'
                 or else C = '8'
                 or else C = '9'
                 or else C = 'p'
                 or else C = 'f'
                 or else C = 'n'
                 or else C = 'w'
               then
                  Identifier_Character_Set := C;
                  Ptr := Ptr + 1;

               else
                  Bad_Switch ("-gnati" & Switch_Chars (Ptr .. Max));
               end if;

            --  -gnatI (ignore representation clauses)

            when 'I' =>
               Ptr := Ptr + 1;
               Ignore_Rep_Clauses := True;

            --  -gnatj (messages in limited length lines)

            when 'j' =>
               Ptr := Ptr + 1;
               Scan_Nat (Switch_Chars, Max, Ptr, Error_Msg_Line_Length, C);

            --  -gnatk (limit file name length)

            when 'k' =>
               Ptr := Ptr + 1;
                  Scan_Pos
                    (Switch_Chars, Max, Ptr, Maximum_File_Name_Length, C);

            --  -gnatl (output full source)

            when 'l' =>
               Ptr := Ptr + 1;
               Full_List := True;

               --  There may be an equal sign between -gnatl and a file name

               if Ptr <= Max and then Switch_Chars (Ptr) = '=' then
                  if Ptr = Max then
                     Osint.Fail ("file name for -gnatl= is null");
                  else
                     Opt.Full_List_File_Name :=
                       new String'(Switch_Chars (Ptr + 1 .. Max));
                     Ptr := Max + 1;
                  end if;
               end if;

            --  -gnatL (corresponding source text)

            when 'L' =>
               Ptr := Ptr + 1;
               Dump_Source_Text := True;

            --  -gnatm (max number or errors/warnings)

            when 'm' =>
               Ptr := Ptr + 1;
               Scan_Nat (Switch_Chars, Max, Ptr, Maximum_Messages, C);

            --  -gnatn (enable pragma Inline)

            when 'n' =>
               Ptr := Ptr + 1;
               Inline_Active := True;

               --  There may be a digit (1 or 2) appended to the switch

               if Ptr <= Max then
                  C := Switch_Chars (Ptr);

                  if C in '1' .. '2' then
                     Ptr := Ptr + 1;
                     Inline_Level := Character'Pos (C) - Character'Pos ('0');
                  end if;
               end if;

            --  -gnatN (obsolescent)

            when 'N' =>
               Ptr := Ptr + 1;
               Inline_Active := True;
               Front_End_Inlining := True;

            --  -gnato (overflow checks)

            when 'o' =>
               Ptr := Ptr + 1;
               Suppress_Options.Suppress (Overflow_Check) := False;

               --  Case of no digits after the -gnato

               if Ptr > Max or else Switch_Chars (Ptr) not in '1' .. '3' then
                  Suppress_Options.Overflow_Mode_General    := Strict;
                  Suppress_Options.Overflow_Mode_Assertions := Strict;

               --  At least one digit after the -gnato

               else
                  --  Handle first digit after -gnato

                  Suppress_Options.Overflow_Mode_General :=
                    Get_Overflow_Mode (Switch_Chars (Ptr));
                  Ptr := Ptr + 1;

                  --  Only one digit after -gnato, set assertions mode to
                  --  be the same as general mode.

                  if Ptr > Max
                    or else Switch_Chars (Ptr) not in '1' .. '3'
                  then
                     Suppress_Options.Overflow_Mode_Assertions :=
                       Suppress_Options.Overflow_Mode_General;

                  --  Process second digit after -gnato

                  else
                     Suppress_Options.Overflow_Mode_Assertions :=
                       Get_Overflow_Mode (Switch_Chars (Ptr));
                     Ptr := Ptr + 1;
                  end if;
               end if;

            --  -gnatO (specify name of the object file)

            --  This is an internal switch

            when 'O' =>
               Store_Switch := False;
               Ptr := Ptr + 1;
               Output_File_Name_Present := True;

            --  -gnatp (suppress all checks)

            when 'p' =>
               Ptr := Ptr + 1;

               --  Skip processing if cancelled by subsequent -gnat-p

               if Switch_Subsequently_Cancelled ("p", Args, Arg_Rank) then
                  Store_Switch := False;

               else
                  --  Set all specific options as well as All_Checks in the
                  --  Suppress_Options array, excluding Elaboration_Check,
                  --  since this is treated specially because we do not want
                  --  -gnatp to disable static elaboration processing. Also
                  --  exclude Atomic_Synchronization, since this is not a real
                  --  check.

                  for J in Suppress_Options.Suppress'Range loop
                     if J /= Elaboration_Check
                          and then
                        J /= Atomic_Synchronization
                     then
                        Suppress_Options.Suppress (J) := True;
                     end if;
                  end loop;

                  Validity_Checks_On  := False;
                  Opt.Suppress_Checks := True;
               end if;

            --  -gnatP (periodic poll)

            when 'P' =>
               Ptr := Ptr + 1;
               Polling_Required := True;

            --  -gnatq (don't quit)

            when 'q' =>
               Ptr := Ptr + 1;
               Try_Semantics := True;

            --  -gnatQ (always write ALI file)

            when 'Q' =>
               Ptr := Ptr + 1;
               Force_ALI_Tree_File := True;
               Try_Semantics := True;

            --  -gnatr (restrictions as warnings)

            when 'r' =>
               Ptr := Ptr + 1;
               Treat_Restrictions_As_Warnings := True;

            --  -gnatR (list rep. info)

            when 'R' =>

               --  Not allowed if previous -gnatD given. See more extensive
               --  comments in the 'D' section for the inverse test.

               if Debug_Generated_Code then
                  Osint.Fail
                    ("-gnatR not permitted since -gnatD given previously");
               end if;

               --  Not allowed if previous -gnatc was given, since we must
               --  call the code generator to determine rep information.

               if Operating_Mode = Check_Semantics then
                  Osint.Fail
                    ("-gnatR not permitted since -gnatc given previously");
               end if;

               --  Set to annotate rep info, and set default -gnatR mode

               Back_Annotate_Rep_Info := True;
               List_Representation_Info := 1;

               --  Scan possible parameter

               Ptr := Ptr + 1;
               while Ptr <= Max loop
                  C := Switch_Chars (Ptr);

                  if C in '1' .. '3' then
                     List_Representation_Info :=
                       Character'Pos (C) - Character'Pos ('0');

                  elsif Switch_Chars (Ptr) = 's' then
                     List_Representation_Info_To_File := True;

                  elsif Switch_Chars (Ptr) = 'm' then
                     List_Representation_Info_Mechanisms := True;

                  else
                     Bad_Switch ("-gnatR" & Switch_Chars (Ptr .. Max));
                  end if;

                  Ptr := Ptr + 1;
               end loop;

            --  -gnats (syntax check only)

            when 's' =>
               if not First_Switch then
                  Osint.Fail
                    ("-gnats must be first if combined with other switches");
               end if;

               Ptr := Ptr + 1;
               Operating_Mode := Check_Syntax;

            --  -gnatS (print package Standard)

            when 'S' =>
               Print_Standard := True;
               Ptr := Ptr + 1;

            --  -gnatt (output tree)

            when 't' =>
               Ptr := Ptr + 1;
               Tree_Output := True;
               Back_Annotate_Rep_Info := True;

            --  -gnatT (change start of internal table sizes)

            when 'T' =>
               Ptr := Ptr + 1;
               Scan_Pos (Switch_Chars, Max, Ptr, Table_Factor, C);

            --  -gnatu (list units for compilation)

            when 'u' =>
               Ptr := Ptr + 1;
               List_Units := True;

            --  -gnatU (unique tags)

            when 'U' =>
               Ptr := Ptr + 1;
               Unique_Error_Tag := True;

            --  -gnatv (verbose mode)

            when 'v' =>
               Ptr := Ptr + 1;
               Verbose_Mode := True;

            --  -gnatV (validity checks)

            when 'V' =>
               Store_Switch := False;
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Bad_Switch ("-gnatV");

               else
                  declare
                     OK  : Boolean;

                  begin
                     Set_Validity_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        Bad_Switch ("-gnatV" & Switch_Chars (Ptr .. Max));
                     end if;

                     for Index in First_Char + 1 .. Max loop
                        Store_Compilation_Switch
                          ("-gnatV" & Switch_Chars (Index));
                     end loop;
                  end;
               end if;

               Ptr := Max + 1;

            --  -gnatw (warning modes)

            when 'w' =>
               Store_Switch := False;
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Bad_Switch ("-gnatw");
               end if;

               while Ptr <= Max loop
                  C := Switch_Chars (Ptr);

                  --  Case of dot switch

                  if C = '.' and then Ptr < Max then
                     Ptr := Ptr + 1;
                     C := Switch_Chars (Ptr);

                     if Set_Dot_Warning_Switch (C) then
                        Store_Compilation_Switch ("-gnatw." & C);
                     else
                        Bad_Switch ("-gnatw." & Switch_Chars (Ptr .. Max));
                     end if;

                     --  Normal case, no dot

                  else
                     if Set_Warning_Switch (C) then
                        Store_Compilation_Switch ("-gnatw" & C);
                     else
                        Bad_Switch ("-gnatw" & Switch_Chars (Ptr .. Max));
                     end if;
                  end if;

                  Ptr := Ptr + 1;
               end loop;

               return;

            --  -gnatW (wide character encoding method)

            when 'W' =>
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Bad_Switch ("-gnatW");
               end if;

               begin
                  Wide_Character_Encoding_Method :=
                    Get_WC_Encoding_Method (Switch_Chars (Ptr));
               exception
                  when Constraint_Error =>
                     Bad_Switch ("-gnatW" & Switch_Chars (Ptr .. Max));
               end;

               Wide_Character_Encoding_Method_Specified := True;

               Upper_Half_Encoding :=
                 Wide_Character_Encoding_Method in
                   WC_Upper_Half_Encoding_Method;

               Ptr := Ptr + 1;

            --  -gnatx (suppress cross-ref information)

            when 'x' =>
               Ptr := Ptr + 1;
               Xref_Active := False;

            --  -gnatX (language extensions)

            when 'X' =>
               Ptr := Ptr + 1;
               Extensions_Allowed   := True;
               Ada_Version          := Ada_Version_Type'Last;
               Ada_Version_Explicit := Ada_Version_Type'Last;
               Ada_Version_Pragma   := Empty;

            --  -gnaty (style checks)

            when 'y' =>
               Ptr := Ptr + 1;
               Style_Check_Main := True;

               if Ptr > Max then
                  Set_Default_Style_Check_Options;

               else
                  Store_Switch := False;

                  declare
                     OK  : Boolean;

                  begin
                     Set_Style_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        Osint.Fail
                          ("bad -gnaty switch (" &
                           Style_Msg_Buf (1 .. Style_Msg_Len) & ')');
                     end if;

                     Ptr := First_Char + 1;
                     while Ptr <= Max loop
                        if Switch_Chars (Ptr) = 'M' then
                           First_Char := Ptr;
                           loop
                              Ptr := Ptr + 1;
                              exit when Ptr > Max
                                or else Switch_Chars (Ptr) not in '0' .. '9';
                           end loop;

                           Store_Compilation_Switch
                             ("-gnaty" & Switch_Chars (First_Char .. Ptr - 1));

                        else
                           Store_Compilation_Switch
                             ("-gnaty" & Switch_Chars (Ptr));
                           Ptr := Ptr + 1;
                        end if;
                     end loop;
                  end;
               end if;

            --  -gnatz (stub generation)

            when 'z' =>

               --  -gnatz must be the first and only switch in Switch_Chars,
               --  and is a two-letter switch.

               if Ptr /= Switch_Chars'First + 5
                 or else (Max - Ptr + 1) > 2
               then
                  Osint.Fail
                    ("-gnatz* may not be combined with other switches");
               end if;

               if Ptr = Max then
                  Bad_Switch ("-gnatz");
               end if;

               Ptr := Ptr + 1;

               --  Only one occurrence of -gnat* is permitted

               if Distribution_Stub_Mode = No_Stubs then
                  case Switch_Chars (Ptr) is
                     when 'r' =>
                        Distribution_Stub_Mode := Generate_Receiver_Stub_Body;

                     when 'c' =>
                        Distribution_Stub_Mode := Generate_Caller_Stub_Body;

                     when others =>
                        Bad_Switch ("-gnatz" & Switch_Chars (Ptr .. Max));
                  end case;

                  Ptr := Ptr + 1;

               else
                  Osint.Fail ("only one -gnatz* switch allowed");
               end if;

            --  -gnatZ (obsolescent)

            when 'Z' =>
               Ptr := Ptr + 1;
               Osint.Fail
                 ("-gnatZ is no longer supported: consider using --RTS=zcx");

            --  Note on language version switches: whenever a new language
            --  version switch is added, Switch.M.Normalize_Compiler_Switches
            --  must be updated.

            --  -gnat83

            when '8' =>
               if Ptr = Max then
                  Bad_Switch ("-gnat8");
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '3' then
                  Bad_Switch ("-gnat8" & Switch_Chars (Ptr .. Max));
               else
                  Ptr := Ptr + 1;
                  Ada_Version          := Ada_83;
                  Ada_Version_Explicit := Ada_83;
                  Ada_Version_Pragma   := Empty;
               end if;

            --  -gnat95

            when '9' =>
               if Ptr = Max then
                  Bad_Switch ("-gnat9");
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '5' then
                  Bad_Switch ("-gnat9" & Switch_Chars (Ptr .. Max));
               else
                  Ptr := Ptr + 1;
                  Ada_Version          := Ada_95;
                  Ada_Version_Explicit := Ada_95;
                  Ada_Version_Pragma   := Empty;
               end if;

            --  -gnat05

            when '0' =>
               if Ptr = Max then
                  Bad_Switch ("-gnat0");
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '5' then
                  Bad_Switch ("-gnat0" & Switch_Chars (Ptr .. Max));
               else
                  Ptr := Ptr + 1;
                  Ada_Version          := Ada_2005;
                  Ada_Version_Explicit := Ada_2005;
                  Ada_Version_Pragma   := Empty;
               end if;

            --  -gnat12

            when '1' =>
               if Ptr = Max then
                  Bad_Switch ("-gnat1");
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '2' then
                  Bad_Switch ("-gnat1" & Switch_Chars (Ptr .. Max));
               else
                  Ptr := Ptr + 1;
                  Ada_Version          := Ada_2012;
                  Ada_Version_Explicit := Ada_2012;
                  Ada_Version_Pragma   := Empty;
               end if;

            --  -gnat2005 and -gnat2012

            when '2' =>
               if Ptr > Max - 3 then
                  Bad_Switch ("-gnat" & Switch_Chars (Ptr .. Max));

               elsif Switch_Chars (Ptr .. Ptr + 3) = "2005" then
                  Ada_Version := Ada_2005;

               elsif Switch_Chars (Ptr .. Ptr + 3) = "2012" then
                  Ada_Version := Ada_2012;

               else
                  Bad_Switch ("-gnat" & Switch_Chars (Ptr .. Ptr + 3));
               end if;

               Ada_Version_Explicit := Ada_Version;
               Ada_Version_Pragma   := Empty;
               Ptr := Ptr + 4;

            --  Switch cancellation, currently only -gnat-p is allowed.
            --  All we do here is the error checking, since the actual
            --  processing for switch cancellation is done by calls to
            --  Switch_Subsequently_Cancelled at the appropriate point.

            when '-' =>

               --  Simple ignore -gnat-p

               if Switch_Chars = "-gnat-p" then
                  return;

               --  Any other occurrence of minus is ignored. This is for
               --  maximum compatibility with previous version which ignored
               --  all occurrences of minus.

               else
                  Store_Switch := False;
                  Ptr := Ptr + 1;
               end if;

            --  We ignore '/' in switches, this is historical, still needed???

            when '/' =>
               Store_Switch := False;

            --  Anything else is an error (illegal switch character)

            when others =>
               Bad_Switch ("-gnat" & Switch_Chars (Ptr .. Max));
            end case;

            if Store_Switch then
               Store_Compilation_Switch
                 ("-gnat" & Switch_Chars (First_Char .. Ptr - 1));
            end if;

            First_Switch := False;
         end loop;
      end if;
   end Scan_Front_End_Switches;

   -----------------------------------
   -- Switch_Subsequently_Cancelled --
   -----------------------------------

   function Switch_Subsequently_Cancelled
     (C        : String;
      Args     : String_List;
      Arg_Rank : Positive) return Boolean
   is
   begin
      --  Loop through arguments following the current one

      for Arg in Arg_Rank + 1 .. Args'Last loop
         if Args (Arg).all = "-gnat-" & C then
            return True;
         end if;
      end loop;

      --  No match found, not cancelled

      return False;
   end Switch_Subsequently_Cancelled;

end Switch.C;
