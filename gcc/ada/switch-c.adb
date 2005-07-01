------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2005 Free Software Foundation, Inc.          --
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
with Types;    use Types;
with Validsw;  use Validsw;
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
         raise Bad_Switch;
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

                  Ptr := Ptr + 1;

                  if Ptr > Max then
                     raise Bad_Switch;
                  end if;

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
                  raise Bad_Switch;
               end if;

         when True =>

            --  Process -gnat* options

            case C is

            when 'a' =>
               Ptr := Ptr + 1;
               Assertions_Enabled := True;

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
                     raise Bad_Switch;
                  end if;
               end loop;

               --  Make sure Zero_Cost_Exceptions is set if gnatdX set. This
               --  is for backwards compatibility with old versions and usage.

               if Debug_Flag_XX then
                  Zero_Cost_Exceptions_Set := True;
                  Zero_Cost_Exceptions_Val := True;
               end if;

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
                  raise Bad_Switch;
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
                        raise Bad_Switch;
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
                        raise Bad_Switch;
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
                     Scan_Pos (Switch_Chars, Max, Ptr, Multiple_Unit_Index);

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
                        raise Bad_Switch;
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
                        raise Bad_Switch;
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
                     raise Bad_Switch;
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

               Set_Style_Check_Options ("3abcdefhiklmnprstu");

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
                  raise Bad_Switch;
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
                  raise Bad_Switch;
               end if;

            --  Processing for k switch

            when 'k' =>
               Ptr := Ptr + 1;
               Scan_Pos (Switch_Chars, Max, Ptr, Maximum_File_Name_Length);

            --  Processing for l switch

            when 'l' =>
               Ptr := Ptr + 1;
               Full_List := True;

            --  Processing for L switch

            when 'L' =>
               Ptr := Ptr + 1;
               Zero_Cost_Exceptions_Set := True;
               Zero_Cost_Exceptions_Val := False;

            --  Processing for m switch

            when 'm' =>
               Ptr := Ptr + 1;
               Scan_Pos (Switch_Chars, Max, Ptr, Maximum_Errors);

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
               Suppress_Options           := (others => True);
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
                     raise Bad_Switch;
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
               Scan_Pos (Switch_Chars, Max, Ptr, Table_Factor);

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
                  raise Bad_Switch;

               else
                  declare
                     OK  : Boolean;

                  begin
                     Set_Validity_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        raise Bad_Switch;
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
                  raise Bad_Switch;
               end if;

               while Ptr <= Max loop
                  C := Switch_Chars (Ptr);

                  case C is
                     when 'a' =>
                        Check_Unreferenced              := True;
                        Check_Unreferenced_Formals      := True;
                        Check_Withs                     := True;
                        Constant_Condition_Warnings     := True;
                        Implementation_Unit_Warnings    := True;
                        Ineffective_Inline_Warnings     := True;
                        Warn_On_Ada_2005_Compatibility  := True;
                        Warn_On_Bad_Fixed_Value         := True;
                        Warn_On_Constant                := True;
                        Warn_On_Export_Import           := True;
                        Warn_On_Modified_Unread         := True;
                        Warn_On_No_Value_Assigned       := True;
                        Warn_On_Obsolescent_Feature     := True;
                        Warn_On_Redundant_Constructs    := True;
                        Warn_On_Unchecked_Conversion    := True;
                        Warn_On_Unrecognized_Pragma     := True;

                     when 'A' =>
                        Check_Unreferenced              := False;
                        Check_Unreferenced_Formals      := False;
                        Check_Withs                     := False;
                        Constant_Condition_Warnings     := False;
                        Elab_Warnings                   := False;
                        Implementation_Unit_Warnings    := False;
                        Ineffective_Inline_Warnings     := False;
                        Warn_On_Ada_2005_Compatibility  := False;
                        Warn_On_Bad_Fixed_Value         := False;
                        Warn_On_Constant                := False;
                        Warn_On_Dereference             := False;
                        Warn_On_Export_Import           := False;
                        Warn_On_Hiding                  := False;
                        Warn_On_Modified_Unread         := False;
                        Warn_On_No_Value_Assigned       := False;
                        Warn_On_Obsolescent_Feature     := False;
                        Warn_On_Redundant_Constructs    := False;
                        Warn_On_Unchecked_Conversion    := False;
                        Warn_On_Unrecognized_Pragma     := False;

                     when 'b' =>
                        Warn_On_Bad_Fixed_Value         := True;

                     when 'B' =>
                        Warn_On_Bad_Fixed_Value         := False;

                     when 'c' =>
                        Constant_Condition_Warnings     := True;

                     when 'C' =>
                        Constant_Condition_Warnings     := False;

                     when 'd' =>
                        Warn_On_Dereference             := True;

                     when 'D' =>
                        Warn_On_Dereference             := False;

                     when 'e' =>
                        Warning_Mode                    := Treat_As_Error;

                     when 'f' =>
                        Check_Unreferenced_Formals      := True;

                     when 'F' =>
                        Check_Unreferenced_Formals      := False;

                     when 'g' =>
                        Warn_On_Unrecognized_Pragma     := True;

                     when 'G' =>
                        Warn_On_Unrecognized_Pragma     := False;

                     when 'h' =>
                        Warn_On_Hiding                  := True;

                     when 'H' =>
                        Warn_On_Hiding                  := False;

                     when 'i' =>
                        Implementation_Unit_Warnings    := True;

                     when 'I' =>
                        Implementation_Unit_Warnings    := False;

                     when 'j' =>
                        Warn_On_Obsolescent_Feature     := True;

                     when 'J' =>
                        Warn_On_Obsolescent_Feature     := False;

                     when 'k' =>
                        Warn_On_Constant                := True;

                     when 'K' =>
                        Warn_On_Constant                := False;

                     when 'l' =>
                        Elab_Warnings                   := True;

                     when 'L' =>
                        Elab_Warnings                   := False;

                     when 'm' =>
                        Warn_On_Modified_Unread         := True;

                     when 'M' =>
                        Warn_On_Modified_Unread         := False;

                     when 'n' =>
                        Warning_Mode                    := Normal;

                     when 'o' =>
                        Address_Clause_Overlay_Warnings := True;

                     when 'O' =>
                        Address_Clause_Overlay_Warnings := False;

                     when 'p' =>
                        Ineffective_Inline_Warnings     := True;

                     when 'P' =>
                        Ineffective_Inline_Warnings     := False;

                     when 'r' =>
                        Warn_On_Redundant_Constructs    := True;

                     when 'R' =>
                        Warn_On_Redundant_Constructs    := False;

                     when 's' =>
                        Warning_Mode                    := Suppress;

                     when 'u' =>
                        Check_Unreferenced              := True;
                        Check_Withs                     := True;
                        Check_Unreferenced_Formals      := True;

                     when 'U' =>
                        Check_Unreferenced              := False;
                        Check_Withs                     := False;
                        Check_Unreferenced_Formals      := False;

                     when 'v' =>
                        Warn_On_No_Value_Assigned       := True;

                     when 'V' =>
                        Warn_On_No_Value_Assigned       := False;

                     when 'x' =>
                        Warn_On_Export_Import           := True;

                     when 'X' =>
                        Warn_On_Export_Import           := False;

                     when 'y' =>
                        Warn_On_Ada_2005_Compatibility  := True;

                     when 'Y' =>
                        Warn_On_Ada_2005_Compatibility  := False;

                     when 'z' =>
                        Warn_On_Unchecked_Conversion    := True;

                     when 'Z' =>
                        Warn_On_Unchecked_Conversion    := False;

                        --  Allow and ignore 'w' so that the old
                        --  format (e.g. -gnatwuwl) will work.

                     when 'w' =>
                        null;

                     when others =>
                        raise Bad_Switch;
                  end case;

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
                  raise Bad_Switch;
               end if;

               for J in WC_Encoding_Method loop
                  if Switch_Chars (Ptr) = WC_Encoding_Letters (J) then
                     Wide_Character_Encoding_Method := J;
                     exit;

                  elsif J = WC_Encoding_Method'Last then
                     raise Bad_Switch;
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
                        raise Bad_Switch;
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
                        raise Bad_Switch;
                  end case;

                  Ptr := Ptr + 1;

               end if;

            --  Processing for Z switch

            when 'Z' =>
               Ptr := Ptr + 1;
               Zero_Cost_Exceptions_Set := True;
               Zero_Cost_Exceptions_Val := True;

            --  Processing for 83 switch

            when '8' =>
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '3' then
                  raise Bad_Switch;
               else
                  Ptr := Ptr + 1;
                  Ada_Version := Ada_83;
                  Ada_Version_Explicit := Ada_Version;
               end if;

            --  Processing for 95 switch

            when '9' =>
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '5' then
                  raise Bad_Switch;
               else
                  Ptr := Ptr + 1;
                  Ada_Version := Ada_95;
                  Ada_Version_Explicit := Ada_Version;
               end if;

            --  Processing for 05 switch

            when '0' =>
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '5' then
                  raise Bad_Switch;
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
               raise Bad_Switch;
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

   exception
      when Bad_Switch =>
         Osint.Fail ("invalid switch: ", (1 => C));

      when Bad_Switch_Value =>
         Osint.Fail ("numeric value out of range for switch: ", (1 => C));

      when Missing_Switch_Value =>
         Osint.Fail ("missing numeric value for switch: ", (1 => C));

   end Scan_Front_End_Switches;

end Switch.C;
