------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

--  Option switch scanning for both the compiler and the binder

--  Note: this version of the package should be usable in both Unix and DOS

with Debug;    use Debug;
with Osint;    use Osint;
with Opt;      use Opt;
with Validsw;  use Validsw;
with Stylesw;  use Stylesw;
with Types;    use Types;

with System.WCh_Con; use System.WCh_Con;

package body Switch is

   Bad_Switch : exception;
   --  Exception raised if bad switch encountered

   Bad_Switch_Value : exception;
   --  Exception raised if bad switch value encountered

   Missing_Switch_Value : exception;
   --  Exception raised if no switch value encountered

   Too_Many_Output_Files : exception;
   --  Exception raised if the -o switch is encountered more than once

   Switch_Max_Value : constant := 999;
   --  Maximum value permitted in switches that take a value

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat);
   --  Scan natural integer parameter for switch. On entry, Ptr points
   --  just past the switch character, on exit it points past the last
   --  digit of the integer value.

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos);
   --  Scan positive integer parameter for switch. On entry, Ptr points
   --  just past the switch character, on exit it points past the last
   --  digit of the integer value.

   -------------------------
   -- Is_Front_End_Switch --
   -------------------------

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean is
      Ptr       : constant Positive := Switch_Chars'First;
   begin
      return Is_Switch (Switch_Chars)
        and then
          (Switch_Chars (Ptr + 1) = 'I'
             or else
          (Switch_Chars'Length >= 5
                         and then Switch_Chars (Ptr + 1 .. Ptr + 4) = "gnat"));
   end Is_Front_End_Switch;

   ---------------
   -- Is_Switch --
   ---------------

   function Is_Switch (Switch_Chars : String) return Boolean is
   begin
      return Switch_Chars'Length > 1
        and then (Switch_Chars (Switch_Chars'First) = '-'
                     or
                  Switch_Chars (Switch_Chars'First) = Switch_Character);
   end Is_Switch;

   --------------------------
   -- Scan_Binder_Switches --
   --------------------------

   procedure Scan_Binder_Switches (Switch_Chars : String) is
      Ptr : Integer := Switch_Chars'First;
      Max : Integer := Switch_Chars'Last;
      C   : Character := ' ';

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;
      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler

      if Switch_Chars'Last >= Ptr + 3
        and then Switch_Chars (Ptr .. Ptr + 3) = "gnat"
      then
         Osint.Fail ("invalid switch: """, Switch_Chars, """"
            & " (gnat not needed here)");

      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switch_Chars (Ptr);

         case C is

         --  Processing for A switch

         when 'A' =>
            Ptr := Ptr + 1;

            Ada_Bind_File := True;

         --  Processing for b switch

         when 'b' =>
            Ptr := Ptr + 1;
            Brief_Output := True;

         --  Processing for c switch

         when 'c' =>
            Ptr := Ptr + 1;

            Check_Only := True;

         --  Processing for C switch

         when 'C' =>
            Ptr := Ptr + 1;

            Ada_Bind_File := False;

         --  Processing for d switch

         when 'd' =>

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
                  Set_Debug_Flag (C);
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

         --  Processing for e switch

         when 'e' =>
            Ptr := Ptr + 1;
            Elab_Dependency_Output := True;

         --  Processing for E switch

         when 'E' =>
            Ptr := Ptr + 1;
            Exception_Tracebacks := True;

         --  Processing for f switch

         when 'f' =>
            Ptr := Ptr + 1;
            Force_RM_Elaboration_Order := True;

         --  Processing for g switch

         when 'g' =>
            Ptr := Ptr + 1;

            if Ptr <= Max then
               C := Switch_Chars (Ptr);

               if C in '0' .. '3' then
                  Debugger_Level :=
                    Character'Pos
                      (Switch_Chars (Ptr)) - Character'Pos ('0');
                  Ptr := Ptr + 1;
               end if;

            else
               Debugger_Level := 2;
            end if;

         --  Processing for G switch

         when 'G' =>
            Ptr := Ptr + 1;
            Print_Generated_Code := True;

         --  Processing for h switch

         when 'h' =>
            Ptr := Ptr + 1;
            Usage_Requested := True;

         --  Processing for i switch

         when 'i' =>
            if Ptr = Max then
               raise Bad_Switch;
            end if;

            Ptr := Ptr + 1;
            C := Switch_Chars (Ptr);

            if C in  '1' .. '5'
              or else C = '8'
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

         --  Processing for K switch

         when 'K' =>
            Ptr := Ptr + 1;

            if Program = Binder then
               Output_Linker_Option_List := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for l switch

         when 'l' =>
            Ptr := Ptr + 1;
            Elab_Order_Output := True;

         --  Processing for m switch

         when 'm' =>
            Ptr := Ptr + 1;
            Scan_Pos (Switch_Chars, Max, Ptr, Maximum_Errors);

         --  Processing for n switch

         when 'n' =>
            Ptr := Ptr + 1;
            Bind_Main_Program := False;

            --  Note: The -L option of the binder also implies -n, so
            --  any change here must also be reflected in the processing
            --  for -L that is found in Gnatbind.Scan_Bind_Arg.

         --  Processing for o switch

         when 'o' =>
            Ptr := Ptr + 1;

            if Output_File_Name_Present then
               raise Too_Many_Output_Files;

            else
               Output_File_Name_Present := True;
            end if;

         --  Processing for O switch

         when 'O' =>
            Ptr := Ptr + 1;
            Output_Object_List := True;

         --  Processing for p switch

         when 'p' =>
            Ptr := Ptr + 1;
            Pessimistic_Elab_Order := True;

         --  Processing for q switch

         when 'q' =>
            Ptr := Ptr + 1;
            Quiet_Output := True;

         --  Processing for s switch

         when 's' =>
            Ptr := Ptr + 1;
            All_Sources := True;
            Check_Source_Files := True;

         --  Processing for t switch

         when 't' =>
            Ptr := Ptr + 1;
            Tolerate_Consistency_Errors := True;

         --  Processing for T switch

         when 'T' =>
            Ptr := Ptr + 1;
            Time_Slice_Set := True;
            Scan_Nat (Switch_Chars, Max, Ptr, Time_Slice_Value);

         --  Processing for v switch

         when 'v' =>
            Ptr := Ptr + 1;
            Verbose_Mode := True;

         --  Processing for w switch

         when 'w' =>

            --  For the binder we only allow suppress/error cases

            Ptr := Ptr + 1;

            case Switch_Chars (Ptr) is

               when 'e' =>
                  Warning_Mode  := Treat_As_Error;

               when 's' =>
                  Warning_Mode  := Suppress;

               when others =>
                  raise Bad_Switch;
            end case;

            Ptr := Ptr + 1;

         --  Processing for W switch

         when 'W' =>
            Ptr := Ptr + 1;

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
            All_Sources := False;
            Check_Source_Files := False;

         --  Processing for z switch

         when 'z' =>
            Ptr := Ptr + 1;
            No_Main_Subprogram := True;

         --  Ignore extra switch character

         when '/' | '-' =>
            Ptr := Ptr + 1;

         --  Anything else is an error (illegal switch character)

         when others =>
            raise Bad_Switch;
         end case;
      end loop;

   exception
      when Bad_Switch =>
         Osint.Fail ("invalid switch: ", (1 => C));

      when Bad_Switch_Value =>
         Osint.Fail ("numeric value too big for switch: ", (1 => C));

      when Missing_Switch_Value =>
         Osint.Fail ("missing numeric value for switch: ", (1 => C));

      when Too_Many_Output_Files =>
         Osint.Fail ("duplicate -o switch");
   end Scan_Binder_Switches;

   -----------------------------
   -- Scan_Front_End_Switches --
   -----------------------------

   procedure Scan_Front_End_Switches (Switch_Chars : String) is
      Switch_Starts_With_Gnat : Boolean;
      Ptr : Integer := Switch_Chars'First;
      Max : constant Integer := Switch_Chars'Last;
      C   : Character := ' ';

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;

      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler (where it was already removed)

      Switch_Starts_With_Gnat :=
         Ptr + 3 <= Max and then Switch_Chars (Ptr .. Ptr + 3) = "gnat";

      if Switch_Starts_With_Gnat then
         Ptr := Ptr + 4;
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case Switch_Starts_With_Gnat is

         when False =>
            --  There is only one front-end switch that
            --  does not start with -gnat, namely -I

            case C is

            when 'I' =>
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

            when others =>
               --  Should not happen, as Scan_Switches is supposed
               --  to be called for front-end switches only.
               --  Still, it is safest to raise Bad_Switch error.

               raise Bad_Switch;
            end case;

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
               Ptr := Ptr + 1;
               Operating_Mode := Check_Semantics;

            --  Processing for C switch

            when 'C' =>
               Ptr := Ptr + 1;
               Compress_Debug_Names := True;

            --  Processing for d switch

            when 'd' =>

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
                     Set_Debug_Flag (C);

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

            --  Processing for e switch

            when 'e' =>
               Ptr := Ptr + 1;

               if Ptr > Max then
                  raise Bad_Switch;
               end if;

               case Switch_Chars (Ptr) is

                  --  Configuration pragmas

                  when 'c' =>
                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        raise Bad_Switch;
                     end if;

                     Config_File_Name :=
                        new String'(Switch_Chars (Ptr .. Max));

                     return;

                  --  Mapping file

                  when 'm' =>
                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        raise Bad_Switch;
                     end if;

                     Mapping_File_Name :=
                       new String'(Switch_Chars (Ptr .. Max));
                     return;

                  when others =>
                     raise Bad_Switch;
               end case;

            --  Processing for E switch

            when 'E' =>
               Ptr := Ptr + 1;
               Dynamic_Elaboration_Checks := True;

            --  Processing for f switch

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
               GNAT_Mode                := True;
               Identifier_Character_Set := 'n';
               Warning_Mode             := Treat_As_Error;
               Check_Unreferenced       := True;
               Check_Withs              := True;

               Set_Default_Style_Check_Options;

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
               Suppress_Options.Overflow_Checks := False;

            --  Processing for O switch

            when 'O' =>
               Ptr := Ptr + 1;
               Output_File_Name_Present := True;

            --  Processing for p switch

            when 'p' =>
               Ptr := Ptr + 1;
               Suppress_Options.Access_Checks        := True;
               Suppress_Options.Accessibility_Checks := True;
               Suppress_Options.Discriminant_Checks  := True;
               Suppress_Options.Division_Checks      := True;
               Suppress_Options.Elaboration_Checks   := True;
               Suppress_Options.Index_Checks         := True;
               Suppress_Options.Length_Checks        := True;
               Suppress_Options.Overflow_Checks      := True;
               Suppress_Options.Range_Checks         := True;
               Suppress_Options.Division_Checks      := True;
               Suppress_Options.Length_Checks        := True;
               Suppress_Options.Range_Checks         := True;
               Suppress_Options.Storage_Checks       := True;
               Suppress_Options.Tag_Checks           := True;

               Validity_Checks_On := False;

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

            --  Processing for r switch

            when 'r' =>
               Ptr := Ptr + 1;

               --  Temporarily allow -gnatr to mean -gnatyl (use RM layout)
               --  for compatibility with pre 3.12 versions of GNAT,
               --  to be removed for 3.13 ???

               Set_Style_Check_Options ("l");

            --  Processing for R switch

            when 'R' =>
               Ptr := Ptr + 1;
               Back_Annotate_Rep_Info := True;

               if Ptr <= Max
                 and then Switch_Chars (Ptr) in '0' .. '9'
               then
                  C := Switch_Chars (Ptr);

                  if C in '4' .. '9' then
                     raise Bad_Switch;
                  else
                     List_Representation_Info :=
                       Character'Pos (C) - Character'Pos ('0');
                     Ptr := Ptr + 1;
                  end if;

               else
                  List_Representation_Info := 1;
               end if;

            --  Processing for s switch

            when 's' =>
               Ptr := Ptr + 1;
               Operating_Mode := Check_Syntax;

            --  Processing for t switch

            when 't' =>
               Ptr := Ptr + 1;
               Tree_Output := True;
               Back_Annotate_Rep_Info := True;

            --  Processing for T switch

            when 'T' =>
               Ptr := Ptr + 1;
               Time_Slice_Set := True;
               Scan_Nat (Switch_Chars, Max, Ptr, Time_Slice_Value);

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
                  end;
               end if;

            --  Processing for w switch

            when 'w' =>
               Ptr := Ptr + 1;

               if Ptr > Max then
                  raise Bad_Switch;
               end if;

               while Ptr <= Max loop
                  C := Switch_Chars (Ptr);

                  case C is

                     when 'a' =>
                        Constant_Condition_Warnings  := True;
                        Elab_Warnings                := True;
                        Check_Unreferenced           := True;
                        Check_Withs                  := True;
                        Implementation_Unit_Warnings := True;
                        Ineffective_Inline_Warnings  := True;
                        Warn_On_Redundant_Constructs := True;

                     when 'A' =>
                        Constant_Condition_Warnings  := False;
                        Elab_Warnings                := False;
                        Check_Unreferenced           := False;
                        Check_Withs                  := False;
                        Implementation_Unit_Warnings := False;
                        Warn_On_Biased_Rounding      := False;
                        Warn_On_Hiding               := False;
                        Warn_On_Redundant_Constructs := False;
                        Ineffective_Inline_Warnings  := False;

                     when 'c' =>
                        Constant_Condition_Warnings := True;

                     when 'C' =>
                        Constant_Condition_Warnings := False;

                     when 'b' =>
                        Warn_On_Biased_Rounding := True;

                     when 'B' =>
                        Warn_On_Biased_Rounding := False;

                     when 'e' =>
                        Warning_Mode := Treat_As_Error;

                     when 'h' =>
                        Warn_On_Hiding := True;

                     when 'H' =>
                        Warn_On_Hiding := False;

                     when 'i' =>
                        Implementation_Unit_Warnings := True;

                     when 'I' =>
                        Implementation_Unit_Warnings := False;

                     when 'l' =>
                        Elab_Warnings := True;

                     when 'L' =>
                        Elab_Warnings := False;

                     when 'o' =>
                        Address_Clause_Overlay_Warnings := True;

                     when 'O' =>
                        Address_Clause_Overlay_Warnings := False;

                     when 'p' =>
                        Ineffective_Inline_Warnings := True;

                     when 'P' =>
                        Ineffective_Inline_Warnings := False;

                     when 'r' =>
                        Warn_On_Redundant_Constructs := True;

                     when 'R' =>
                        Warn_On_Redundant_Constructs := False;

                     when 's' =>
                        Warning_Mode  := Suppress;

                     when 'u' =>
                        Check_Unreferenced := True;
                        Check_Withs        := True;

                     when 'U' =>
                        Check_Unreferenced := False;
                        Check_Withs        := False;

                        --  Allow and ignore 'w' so that the old
                        --  format (e.g. -gnatwuwl) will work.

                     when 'w' =>
                        null;

                     when others =>
                        raise Bad_Switch;
                  end case;

                  Ptr := Ptr + 1;
               end loop;

               return;

            --  Processing for W switch

            when 'W' =>
               Ptr := Ptr + 1;

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

            --  Processing for y switch

            when 'y' =>
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Set_Default_Style_Check_Options;

               else
                  declare
                     OK  : Boolean;

                  begin
                     Set_Style_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        raise Bad_Switch;
                     end if;
                  end;
               end if;

            --  Processing for z switch

            when 'z' =>
               Ptr := Ptr + 1;

               --  Allowed for compiler, only if this is the only
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
                  Ada_95 := False;
                  Ada_83 := True;
               end if;

            --  Ignore extra switch character

            when '/' | '-' =>
               Ptr := Ptr + 1;

            --  Anything else is an error (illegal switch character)

            when others =>
               raise Bad_Switch;
            end case;
         end case;
      end loop;

   exception
      when Bad_Switch =>
         Osint.Fail ("invalid switch: ", (1 => C));

      when Bad_Switch_Value =>
         Osint.Fail ("numeric value too big for switch: ", (1 => C));

      when Missing_Switch_Value =>
         Osint.Fail ("missing numeric value for switch: ", (1 => C));

   end Scan_Front_End_Switches;

   ------------------------
   -- Scan_Make_Switches --
   ------------------------

   procedure Scan_Make_Switches (Switch_Chars : String) is
      Ptr : Integer := Switch_Chars'First;
      Max : Integer := Switch_Chars'Last;
      C   : Character := ' ';

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;

      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler (where it was already removed)

      if Switch_Chars'Length >= Ptr + 3
        and then Switch_Chars (Ptr .. Ptr + 3) = "gnat"
      then
         Osint.Fail
           ("invalid switch: """, Switch_Chars, """ (gnat not needed here)");
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case C is

         when 'a' =>
            Ptr := Ptr + 1;
            Check_Readonly_Files := True;

         --  Processing for b switch

         when 'b' =>
            Ptr := Ptr + 1;
            Bind_Only := True;

         --  Processing for c switch

         when 'c' =>
            Ptr := Ptr + 1;
            Compile_Only := True;

         when 'd' =>

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
                  Set_Debug_Flag (C);
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

         --  Processing for f switch

         when 'f' =>
            Ptr := Ptr + 1;
            Force_Compilations := True;

         --  Processing for G switch

         when 'G' =>
            Ptr := Ptr + 1;
            Print_Generated_Code := True;

         --  Processing for h switch

         when 'h' =>
            Ptr := Ptr + 1;
            Usage_Requested := True;

         --  Processing for i switch

         when 'i' =>
            Ptr := Ptr + 1;
            In_Place_Mode := True;

         --  Processing for j switch

         when 'j' =>
            Ptr := Ptr + 1;

            declare
               Max_Proc : Pos;
            begin
               Scan_Pos (Switch_Chars, Max, Ptr, Max_Proc);
               Maximum_Processes := Positive (Max_Proc);
            end;

         --  Processing for k switch

         when 'k' =>
            Ptr := Ptr + 1;
            Keep_Going := True;

         --  Processing for l switch

         when 'l' =>
            Ptr := Ptr + 1;
            Link_Only := True;

         when 'M' =>
            Ptr := Ptr + 1;
            List_Dependencies := True;

         --  Processing for n switch

         when 'n' =>
            Ptr := Ptr + 1;
            Do_Not_Execute := True;

         --  Processing for o switch

         when 'o' =>
            Ptr := Ptr + 1;

            if Output_File_Name_Present then
               raise Too_Many_Output_Files;
            else
               Output_File_Name_Present := True;
            end if;

         --  Processing for q switch

         when 'q' =>
            Ptr := Ptr + 1;
            Quiet_Output := True;

         --  Processing for s switch

         when 's' =>
            Ptr := Ptr + 1;
            Check_Switches := True;

         --  Processing for v switch

         when 'v' =>
            Ptr := Ptr + 1;
            Verbose_Mode := True;

         --  Processing for z switch

         when 'z' =>
            Ptr := Ptr + 1;
            No_Main_Subprogram := True;

         --  Ignore extra switch character

         when '/' | '-' =>
            Ptr := Ptr + 1;

         --  Anything else is an error (illegal switch character)

         when others =>
            raise Bad_Switch;

         end case;
      end loop;

   exception
      when Bad_Switch =>
         Osint.Fail ("invalid switch: ", (1 => C));

      when Bad_Switch_Value =>
         Osint.Fail ("numeric value too big for switch: ", (1 => C));

      when Missing_Switch_Value =>
         Osint.Fail ("missing numeric value for switch: ", (1 => C));

      when Too_Many_Output_Files =>
         Osint.Fail ("duplicate -o switch");

   end Scan_Make_Switches;

   --------------
   -- Scan_Nat --
   --------------

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat) is
   begin
      Result := 0;
      if Ptr > Max or else Switch_Chars (Ptr) not in '0' .. '9' then
         raise Missing_Switch_Value;
      end if;

      while Ptr <= Max and then Switch_Chars (Ptr) in '0' .. '9' loop
         Result := Result * 10 +
           Character'Pos (Switch_Chars (Ptr)) - Character'Pos ('0');
         Ptr := Ptr + 1;

         if Result > Switch_Max_Value then
            raise Bad_Switch_Value;
         end if;
      end loop;
   end Scan_Nat;

   --------------
   -- Scan_Pos --
   --------------

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos) is

   begin
      Scan_Nat (Switch_Chars, Max, Ptr, Result);
      if Result = 0 then
         raise Bad_Switch_Value;
      end if;
   end Scan_Pos;

end Switch;
