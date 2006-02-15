------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Debug;    use Debug;
with Lib;      use Lib;
with Osint;    use Osint;
with Opt;      use Opt;
with Prepcomp; use Prepcomp;
with Validsw;  use Validsw;
with Sem_Warn; use Sem_Warn;
with Stylesw;  use Stylesw;

with System.WCh_Con; use System.WCh_Con;

package body Switch.C is

   RTS_Specified : String_Access := null;
   --  Used to detect multiple use of --RTS= flag

   -----------------------------
   -- Scan_Front_End_Switches --
   -----------------------------

   procedure Scan_Front_End_Switches (Switch_Chars : String) is
      Switch_Starts_With_Gnat : Boolean;
      --  True if first four switch characters are "gnat"

      First_Switch : Boolean := True;
      --  False for all but first switch

      Ptr : Integer := Switch_Chars'First;
      Max : constant Integer := Switch_Chars'Last;
      C   : Character := ' ';
      Dot : Boolean;

      Store_Switch : Boolean  := True;
      First_Char   : Integer  := Ptr;
      Storing      : String   := Switch_Chars;
      First_Stored : Positive := Ptr + 1;
      --  The above need comments ???

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         Bad_Switch (C);
      else
         Ptr := Ptr + 1;
      end if;

      --  Remove "gnat" from the switch, if present

      Switch_Starts_With_Gnat :=
        Ptr + 3 <= Max and then Switch_Chars (Ptr .. Ptr + 3) = "gnat";

      if Switch_Starts_With_Gnat then
         Ptr := Ptr + 4;
         First_Stored := Ptr;
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         Store_Switch := True;
         First_Char := Ptr;
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case Switch_Starts_With_Gnat is

            when False =>

            --  There are few front-end switches that
            --  do not start with -gnat: -I, --RTS

               if Switch_Chars (Ptr) = 'I' then
                  Store_Switch := False;

                  --  Set flag Search_Directory_Present if switch is "-I" only:
                  --  the directory will be the next argument.

                  if Ptr = Max then
                     Search_Directory_Present := True;
                     return;
                  end if;

                  Ptr := Ptr + 1;

                  --  Find out whether this is a -I- or regular -Ixxx switch

                  if Ptr = Max and then Switch_Chars (Ptr) = '-' then
                     Look_In_Primary_Dir := False;

                  else
                     Add_Src_Search_Dir (Switch_Chars (Ptr .. Max));
                  end if;

                  Ptr := Max + 1;

               --  Processing of the --RTS switch. --RTS has been modified by
               --  gcc and is now of the form -fRTS

               elsif Ptr + 3 <= Max
                 and then Switch_Chars (Ptr .. Ptr + 3) = "fRTS"
               then
                  Ptr := Ptr + 1;

                  if Ptr + 4 > Max
                    or else Switch_Chars (Ptr + 3) /= '='
                  then
                     Osint.Fail ("missing path for --RTS");
                  else
                     --  Check that this is the first time --RTS is specified
                     --  or if it is not the first time, the same path has
                     --  been specified.

                     if RTS_Specified = null then
                        RTS_Specified :=
                          new String'(Switch_Chars (Ptr + 4 .. Max));

                     elsif
                       RTS_Specified.all /= Switch_Chars (Ptr + 4 .. Max)
                     then
                        Osint.Fail
                          ("--RTS cannot be specified multiple times");
                     end if;

                     --  Valid --RTS switch

                     Opt.No_Stdinc := True;
                     Opt.RTS_Switch := True;

                     RTS_Src_Path_Name := Get_RTS_Search_Dir
                                            (Switch_Chars (Ptr + 4 .. Max),
                                             Include);
                     RTS_Lib_Path_Name := Get_RTS_Search_Dir
                                            (Switch_Chars (Ptr + 4 .. Max),
                                             Objects);

                     if RTS_Src_Path_Name /= null and then
                        RTS_Lib_Path_Name /= null
                     then
                        Ptr := Max + 1;

                     elsif RTS_Src_Path_Name = null and then
                           RTS_Lib_Path_Name = null
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
               else
                  Bad_Switch (C);
               end if;

         when True =>

            --  Process -gnat* options

            case C is

            when 'a' =>
               Ptr := Ptr + 1;
               Assertions_Enabled := True;
               Debug_Pragmas_Enabled := True;

            --  Processing for A switch

            when 'A' =>
               Ptr := Ptr + 1;
               Config_File := False;

            --  Processing for b switch

            when 'b' =>
               Ptr := Ptr + 1;
               Brief_Output := True;

            --  Processing for c switch

            when 'c' =>
               if not First_Switch then
                  Osint.Fail
                    ("-gnatc must be first if combined with other switches");
               end if;

               Ptr := Ptr + 1;
               Operating_Mode := Check_Semantics;

               if Tree_Output then
                  ASIS_Mode := True;
               end if;

            --  Processing for d switch

            when 'd' =>
               Store_Switch := False;
               Storing (First_Stored) := 'd';
               Dot := False;

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
                     if Dot then
                        Set_Dotted_Debug_Flag (C);
                        Storing (First_Stored + 1) := '.';
                        Storing (First_Stored + 2) := C;
                        Store_Compilation_Switch
                          (Storing (Storing'First .. First_Stored + 2));
                        Dot := False;

                     else
                        Set_Debug_Flag (C);
                        Storing (First_Stored + 1) := C;
                        Store_Compilation_Switch
                          (Storing (Storing'First .. First_Stored + 1));
                     end if;

                  elsif C = '.' then
                     Dot := True;

                  else
                     Bad_Switch (C);
                  end if;
               end loop;

               return;

            --  Processing for D switch

            when 'D' =>
               Ptr := Ptr + 1;

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
                  Bad_Switch (C);
               end if;

               case Switch_Chars (Ptr) is

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
                        Bad_Switch (C);
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

                  --  -gnateD switch (symbol definition)

                  when 'D' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        Bad_Switch (C);
                     end if;

                     Add_Symbol_Definition (Switch_Chars (Ptr .. Max));

                     --  Store the switch

                     Storing (First_Stored .. First_Stored + 1) := "eD";
                     Storing
                       (First_Stored + 2 .. First_Stored + Max - Ptr + 2) :=
                       Switch_Chars (Ptr .. Max);
                     Store_Compilation_Switch (Storing
                              (Storing'First .. First_Stored + Max - Ptr + 2));
                     return;

                  --  -gnatef (full source path for brief error messages)

                  when 'f' =>
                     Store_Switch := False;
                     Ptr := Ptr + 1;
                     Full_Path_Name_For_Brief_Errors := True;
                     return;

                  --  -gnateI (index of unit in multi-unit source)

                  when 'I' =>
                     Ptr := Ptr + 1;
                     Scan_Pos
                       (Switch_Chars, Max, Ptr, Multiple_Unit_Index, C);

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
                        Bad_Switch (C);
                     end if;

                     Mapping_File_Name :=
                       new String'(Switch_Chars (Ptr .. Max));
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
                        Bad_Switch (C);
                     end if;

                     Preprocessing_Data_File :=
                       new String'(Switch_Chars (Ptr .. Max));

                     --  Store the switch.
                     --  Because we may store a longer switch (we normalize
                     --  to -gnatep=), use a local variable.

                     declare
                        To_Store : String
                          (1 .. Preprocessing_Data_File'Length + 8);

                     begin
                        To_Store (1 .. 8) := "-gnatep=";
                        To_Store (9 .. Preprocessing_Data_File'Length + 8) :=
                          Preprocessing_Data_File.all;
                        Store_Compilation_Switch (To_Store);
                     end;

                  return;

                  when 'z' =>
                     Store_Switch := False;
                     Disable_Switch_Storing;
                     Ptr := Ptr + 1;

                  --  All other -gnate? switches are unassigned

                  when others =>
                     Bad_Switch (C);
               end case;

            --  -gnatE (dynamic elaboration checks)

            when 'E' =>
               Ptr := Ptr + 1;
               Dynamic_Elaboration_Checks := True;

            --  -gnatf (full error messages)

            when 'f' =>
               Ptr := Ptr + 1;
               All_Errors_Mode := True;

            --  Processing for F switch

            when 'F' =>
               Ptr := Ptr + 1;
               External_Name_Exp_Casing := Uppercase;
               External_Name_Imp_Casing := Uppercase;

            --  Processing for g switch

            when 'g' =>
               Ptr := Ptr + 1;
               GNAT_Mode := True;
               Identifier_Character_Set := 'n';
               System_Extend_Unit := Empty;
               Warning_Mode := Treat_As_Error;

               --  Set Ada 2005 mode explicitly. We don't want to rely on the
               --  implicit setting here, since for example, we want
               --  Preelaborate_05 treated as Preelaborate

               Ada_Version := Ada_05;
               Ada_Version_Explicit := Ada_Version;

               --  Set default warnings for -gnatg (same set as -gnatwa)

               Check_Unreferenced           := True;
               Check_Unreferenced_Formals   := True;
               Check_Withs                  := True;
               Constant_Condition_Warnings  := True;
               Implementation_Unit_Warnings := True;
               Ineffective_Inline_Warnings  := True;
               Warn_On_Bad_Fixed_Value      := True;
               Warn_On_Constant             := True;
               Warn_On_Export_Import        := True;
               Warn_On_Modified_Unread      := True;
               Warn_On_No_Value_Assigned    := True;
               Warn_On_Obsolescent_Feature  := True;
               Warn_On_Redundant_Constructs := True;
               Warn_On_Unchecked_Conversion := True;
               Warn_On_Unrecognized_Pragma  := True;

               Set_Style_Check_Options ("3abcdefhiklmnprstux");

            --  Processing for G switch

            when 'G' =>
               Ptr := Ptr + 1;
               Print_Generated_Code := True;

            --  Processing for h switch

            when 'h' =>
               Ptr := Ptr + 1;
               Usage_Requested := True;

            --  Processing for H switch

            when 'H' =>
               Ptr := Ptr + 1;
               HLO_Active := True;

            --  Processing for i switch

            when 'i' =>
               if Ptr = Max then
                  Bad_Switch (C);
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
                  Bad_Switch (C);
               end if;

            --  Processing for k switch

            when 'k' =>
               Ptr := Ptr + 1;
                  Scan_Pos
                    (Switch_Chars, Max, Ptr, Maximum_File_Name_Length, C);

            --  Processing for l switch

            when 'l' =>
               Ptr := Ptr + 1;
               Full_List := True;

            --  Processing for L switch

            when 'L' =>
               Ptr := Ptr + 1;
               Osint.Fail
                 ("-gnatL is no longer supported: consider using --RTS=sjlj");

            --  Processing for m switch

            when 'm' =>
               Ptr := Ptr + 1;

               --  There may be an equal sign between -gnatm and the value

               if Ptr <= Max and then Switch_Chars (Ptr) = '=' then
                  Ptr := Ptr + 1;
               end if;

               Scan_Pos (Switch_Chars, Max, Ptr, Maximum_Errors, C);

            --  Processing for n switch

            when 'n' =>
               Ptr := Ptr + 1;
               Inline_Active := True;

            --  Processing for N switch

            when 'N' =>
               Ptr := Ptr + 1;
               Inline_Active := True;
               Front_End_Inlining := True;

            --  Processing for o switch

            when 'o' =>
               Ptr := Ptr + 1;
               Suppress_Options (Overflow_Check) := False;
               Opt.Enable_Overflow_Checks := True;

            --  Processing for O switch

            when 'O' =>
               Store_Switch := False;
               Ptr := Ptr + 1;
               Output_File_Name_Present := True;

            --  Processing for p switch

            when 'p' =>
               Ptr := Ptr + 1;

               --  Set all specific options as well as All_Checks in the
               --  Suppress_Options array, excluding Elaboration_Check, since
               --  this is treated specially because we do not want -gnatp to
               --  disable static elaboration processing.

               for J in Suppress_Options'Range loop
                  if J /= Elaboration_Check then
                     Suppress_Options (J) := True;
                  end if;
               end loop;

               Validity_Checks_On         := False;
               Opt.Suppress_Checks        := True;
               Opt.Enable_Overflow_Checks := False;

            --  Processing for P switch

            when 'P' =>
               Ptr := Ptr + 1;
               Polling_Required := True;

            --  Processing for q switch

            when 'q' =>
               Ptr := Ptr + 1;
               Try_Semantics := True;

            --  Processing for q switch

            when 'Q' =>
               Ptr := Ptr + 1;
               Force_ALI_Tree_File := True;
               Try_Semantics := True;

            --  Processing for R switch

            when 'R' =>
               Ptr := Ptr + 1;
               Back_Annotate_Rep_Info := True;
               List_Representation_Info := 1;

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
                     Bad_Switch (C);
                  end if;

                  Ptr := Ptr + 1;
               end loop;

            --  Processing for s switch

            when 's' =>
               if not First_Switch then
                  Osint.Fail
                    ("-gnats must be first if combined with other switches");
               end if;

               Ptr := Ptr + 1;
               Operating_Mode := Check_Syntax;

            --  Processing for S switch

            when 'S' =>
               Print_Standard := True;
               Ptr := Ptr + 1;

            --  Processing for t switch

            when 't' =>
               Ptr := Ptr + 1;
               Tree_Output := True;

               if Operating_Mode = Check_Semantics then
                  ASIS_Mode := True;
               end if;

               Back_Annotate_Rep_Info := True;

            --  Processing for T switch

            when 'T' =>
               Ptr := Ptr + 1;
               Scan_Pos (Switch_Chars, Max, Ptr, Table_Factor, C);

            --  Processing for u switch

            when 'u' =>
               Ptr := Ptr + 1;
               List_Units := True;

            --  Processing for U switch

            when 'U' =>
               Ptr := Ptr + 1;
               Unique_Error_Tag := True;

            --  Processing for v switch

            when 'v' =>
               Ptr := Ptr + 1;
               Verbose_Mode := True;

            --  Processing for V switch

            when 'V' =>
               Store_Switch := False;
               Storing (First_Stored) := 'V';
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Bad_Switch (C);

               else
                  declare
                     OK  : Boolean;

                  begin
                     Set_Validity_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        Bad_Switch (C);
                     end if;

                     for Index in First_Char + 1 .. Max loop
                        Storing (First_Stored + 1) :=
                          Switch_Chars (Index);
                        Store_Compilation_Switch
                          (Storing (Storing'First .. First_Stored + 1));
                     end loop;
                  end;
               end if;

               Ptr := Max + 1;

            --  Processing for w switch

            when 'w' =>
               Store_Switch := False;
               Storing (First_Stored) := 'w';
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Bad_Switch (C);
               end if;

               while Ptr <= Max loop
                  C := Switch_Chars (Ptr);

                  if Set_Warning_Switch (C) then
                     null;
                  else
                     Bad_Switch (C);
                  end if;

                  if C /= 'w' then
                     Storing (First_Stored + 1) := C;
                     Store_Compilation_Switch
                       (Storing (Storing'First .. First_Stored + 1));
                  end if;

                  Ptr := Ptr + 1;
               end loop;

               return;

            --  Processing for W switch

            when 'W' =>
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Bad_Switch (C);
               end if;

               for J in WC_Encoding_Method loop
                  if Switch_Chars (Ptr) = WC_Encoding_Letters (J) then
                     Wide_Character_Encoding_Method := J;
                     exit;

                  elsif J = WC_Encoding_Method'Last then
                     Bad_Switch (C);
                  end if;
               end loop;

               Upper_Half_Encoding :=
                 Wide_Character_Encoding_Method in
                 WC_Upper_Half_Encoding_Method;

               Ptr := Ptr + 1;

            --  Processing for x switch

            when 'x' =>
               Ptr := Ptr + 1;
               Xref_Active := False;

            --  Processing for X switch

            when 'X' =>
               Ptr := Ptr + 1;
               Extensions_Allowed := True;
               Ada_Version := Ada_Version_Type'Last;
               Ada_Version_Explicit := Ada_Version;

            --  Processing for y switch

            when 'y' =>
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Set_Default_Style_Check_Options;

               else
                  Store_Switch := False;
                  Storing (First_Stored) := 'y';

                  declare
                     OK  : Boolean;
                     Last_Stored : Integer;

                  begin
                     Set_Style_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        declare
                           R : String (1 .. Style_Msg_Len + 20);
                        begin
                           R (1 .. 19) := "bad -gnaty switch (";
                           R (20 .. R'Last - 1) :=
                             Style_Msg_Buf (1 .. Style_Msg_Len);
                           R (R'Last) := ')';
                           Osint.Fail (R);
                        end;
                     end if;

                     Ptr := First_Char + 1;
                     while Ptr <= Max loop
                        Last_Stored := First_Stored + 1;
                        Storing (Last_Stored) := Switch_Chars (Ptr);

                        if Switch_Chars (Ptr) = 'M' then
                           loop
                              Ptr := Ptr + 1;
                              exit when Ptr > Max
                                or else Switch_Chars (Ptr) not in '0' .. '9';
                              Last_Stored := Last_Stored + 1;
                              Storing (Last_Stored) := Switch_Chars (Ptr);
                           end loop;

                        else
                           Ptr := Ptr + 1;
                        end if;

                        Store_Compilation_Switch
                          (Storing (Storing'First .. Last_Stored));
                     end loop;
                  end;
               end if;

            --  Processing for z switch

            when 'z' =>
               Ptr := Ptr + 1;

               --  Allowed for compiler only if this is the only
               --  -z switch, we do not allow multiple occurrences

               if Distribution_Stub_Mode = No_Stubs then
                  case Switch_Chars (Ptr) is
                     when 'r' =>
                        Distribution_Stub_Mode := Generate_Receiver_Stub_Body;

                     when 'c' =>
                        Distribution_Stub_Mode := Generate_Caller_Stub_Body;

                     when others =>
                        Bad_Switch (C);
                  end case;

                  Ptr := Ptr + 1;

               end if;

            --  Processing for Z switch

            when 'Z' =>
               Ptr := Ptr + 1;
               Osint.Fail
                 ("-gnatZ is no longer supported: consider using --RTS=zcx");

            --  Processing for 83 switch

            when '8' =>
               if Ptr = Max then
                  Bad_Switch (C);
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '3' then
                  Bad_Switch (C);
               else
                  Ptr := Ptr + 1;
                  Ada_Version := Ada_83;
                  Ada_Version_Explicit := Ada_Version;
               end if;

            --  Processing for 95 switch

            when '9' =>
               if Ptr = Max then
                  Bad_Switch (C);
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '5' then
                  Bad_Switch (C);
               else
                  Ptr := Ptr + 1;
                  Ada_Version := Ada_95;
                  Ada_Version_Explicit := Ada_Version;
               end if;

            --  Processing for 05 switch

            when '0' =>
               if Ptr = Max then
                  Bad_Switch (C);
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '5' then
                  Bad_Switch (C);
               else
                  Ptr := Ptr + 1;
                  Ada_Version := Ada_05;
                  Ada_Version_Explicit := Ada_Version;
               end if;

            --  Ignore extra switch character

            when '/' | '-' =>
               Ptr := Ptr + 1;

            --  Anything else is an error (illegal switch character)

            when others =>
               Bad_Switch (C);
            end case;
         end case;

         if Store_Switch then
            Storing (First_Stored .. First_Stored + Ptr - First_Char - 1) :=
              Switch_Chars (First_Char .. Ptr - 1);
            Store_Compilation_Switch
              (Storing (Storing'First .. First_Stored + Ptr - First_Char - 1));
         end if;

         First_Switch := False;
      end loop;
   end Scan_Front_End_Switches;

end Switch.C;
