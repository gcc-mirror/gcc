------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               W A R N S W                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2024, Free Software Foundation, Inc.         --
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

with Opt;      use Opt;
with Output;   use Output;

with System.Case_Util; use System.Case_Util;

package body Warnsw is

   subtype Lowercase is Character range 'a' .. 'z';
   --  Warning-enable switches are lowercase letters

   Switch_To_Flag_Mapping : constant array (Warning_Family, Lowercase) of
     --  Mapping from the letter after "-gnatw", "-gnatw." or "-gnatw_" to
     --  the corresponding flag for the warning it enables. Special_Case means
     --  Set_Warning_Switch must do something special, as opposed to simply
     --  setting the corresponding flag. No_Such_Warning means the letter
     --  is not a defined warning switch, which is an error.
     X.Opt_Warnings_Enum :=
       (Plain =>
         ('a' | 'e' | 'n' | 's' | 'u' | 'y' => Special_Case,

          'b' => X.Warn_On_Bad_Fixed_Value,
          'c' => X.Constant_Condition_Warnings,
          'd' => X.Warn_On_Dereference,
          'f' => X.Check_Unreferenced_Formals,
          'g' => X.Warn_On_Unrecognized_Pragma,
          'h' => X.Warn_On_Hiding,
          'i' => X.Implementation_Unit_Warnings,
          'j' => X.Warn_On_Obsolescent_Feature,
          'k' => X.Warn_On_Constant,
          'l' => X.Elab_Warnings,
          'm' => X.Warn_On_Modified_Unread,
          'o' => X.Address_Clause_Overlay_Warnings,
          'p' => X.Ineffective_Inline_Warnings,
          'q' => X.Warn_On_Questionable_Missing_Parens,
          'r' => X.Warn_On_Redundant_Constructs,
          't' => X.Warn_On_Deleted_Code,
          'v' => X.Warn_On_No_Value_Assigned,
          'w' => X.Warn_On_Assumed_Low_Bound,
          'x' => X.Warn_On_Export_Import,
          'z' => X.Warn_On_Unchecked_Conversion),

        '.' =>
         ('e' | 'g' | 'x' => Special_Case,

          'a' => X.Warn_On_Assertion_Failure,
          'b' => X.Warn_On_Biased_Representation,
          'c' => X.Warn_On_Unrepped_Components,
          'd' => X.Warning_Doc_Switch,
          'f' => X.Warn_On_Elab_Access,
          'h' => X.Warn_On_Record_Holes,
          'i' => X.Warn_On_Overlap,
          'j' => X.Warn_On_Late_Primitives,
          'k' => X.Warn_On_Standard_Redefinition,
          'l' => X.List_Inherited_Aspects,
          'm' => X.Warn_On_Suspicious_Modulus_Value,
          'n' => X.Warn_On_Atomic_Synchronization,
          'o' => X.Warn_On_All_Unread_Out_Parameters,
          'p' => X.Warn_On_Parameter_Order,
          'q' => X.Warn_On_Questionable_Layout,
          'r' => X.Warn_On_Object_Renames_Function,
          's' => X.Warn_On_Overridden_Size,
          't' => X.Warn_On_Suspicious_Contract,
          'u' => X.Warn_On_Unordered_Enumeration_Type,
          'v' => X.Warn_On_Reverse_Bit_Order,
          'w' => X.Warn_On_Warnings_Off,
          'y' => X.List_Body_Required_Info,
          'z' => X.Warn_On_Size_Alignment),

        '_' =>
         ('b' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' |
          'n' | 'o' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' =>
           No_Such_Warning,

          'a' => X.Warn_On_Anonymous_Allocators,
          'c' => X.Warn_On_Unknown_Compile_Time_Warning,
          'p' => X.Warn_On_Pedantic_Checks,
          'q' => X.Warn_On_Ignored_Equality,
          'r' => X.Warn_On_Component_Order,
          's' => X.Warn_On_Ineffective_Predicate_Test));

   All_Warnings : constant Warnings_State := --  Warnings set by -gnatw.e
     (X.Elab_Info_Messages |
      X.Warning_Doc_Switch |
      X.Warn_On_Ada_2022_Compatibility |
      X.Warn_On_Elab_Access |
      X.No_Warn_On_Non_Local_Exception => False,
      others => True);
   --  Warning_Doc_Switch is not really a warning to be enabled, but controls
   --  the form of warnings printed. No_Warn_On_Non_Local_Exception is handled
   --  specially (see Warn_On_Non_Local_Exception). The others are not part of
   --  -gnatw.e for historical reasons.

   WA_Warnings : constant Warnings_State := --  Warnings set by -gnatwa
     (X.Check_Unreferenced                  | -- -gnatwf/-gnatwu
      X.Check_Unreferenced_Formals          | -- -gnatwf/-gnatwu
      X.Check_Withs                         | -- -gnatwu
      X.Constant_Condition_Warnings         | -- -gnatwc
      X.Implementation_Unit_Warnings        | -- -gnatwi
      X.Ineffective_Inline_Warnings         | -- -gnatwp
      X.Warn_On_Ada_2005_Compatibility      | -- -gnatwy
      X.Warn_On_Ada_2012_Compatibility      | -- -gnatwy
      X.Warn_On_Anonymous_Allocators        | -- -gnatw_a
      X.Warn_On_Assertion_Failure           | -- -gnatw.a
      X.Warn_On_Assumed_Low_Bound           | -- -gnatww
      X.Warn_On_Bad_Fixed_Value             | -- -gnatwb
      X.Warn_On_Biased_Representation       | -- -gnatw.b
      X.Warn_On_Constant                    | -- -gnatwk
      X.Warn_On_Export_Import               | -- -gnatwx
      X.Warn_On_Ineffective_Predicate_Test  | -- -gnatw_s
      X.Warn_On_Late_Primitives             | -- -gnatw.j
      X.Warn_On_Modified_Unread             | -- -gnatwm
      X.Warn_On_No_Value_Assigned           | -- -gnatwv
      X.Warn_On_Non_Local_Exception         | -- -gnatw.x
      X.Warn_On_Object_Renames_Function     | -- -gnatw.r
      X.Warn_On_Obsolescent_Feature         | -- -gnatwj
      X.Warn_On_Overlap                     | -- -gnatw.i
      X.Warn_On_Parameter_Order             | -- -gnatw.p
      X.Warn_On_Questionable_Missing_Parens | -- -gnatwq
      X.Warn_On_Redundant_Constructs        | -- -gnatwr
      X.Warn_On_Reverse_Bit_Order           | -- -gnatw.v
      X.Warn_On_Size_Alignment              | -- -gnatw.z
      X.Warn_On_Suspicious_Contract         | -- -gnatw.t
      X.Warn_On_Suspicious_Modulus_Value    | -- -gnatw.m
      X.Warn_On_Unchecked_Conversion        | -- -gnatwz
      X.Warn_On_Unrecognized_Pragma         | -- -gnatwg
      X.Warn_On_Unrepped_Components         => -- -gnatw.c
        True,

      others => False);

   ----------------------
   -- Restore_Warnings --
   ----------------------

   procedure Restore_Warnings (W : Warnings_State) is
   begin
      Warning_Flags := W;
   end Restore_Warnings;

   -------------------
   -- Save_Warnings --
   -------------------

   function Save_Warnings return Warnings_State is
   begin
      return Warning_Flags;
   end Save_Warnings;

   ----------------------------
   -- Set_GNAT_Mode_Warnings --
   ----------------------------

   procedure Set_GNAT_Mode_Warnings is
   begin
      --  Set -gnatwa warnings and no others

      Warning_Flags := (Warning_Flags and not All_Warnings) or WA_Warnings;

      --  These warnings are added to the -gnatwa set

      Address_Clause_Overlay_Warnings     := True;
      Warn_On_Questionable_Layout         := True;
      Warn_On_Overridden_Size             := True;

      --  These warnings are removed from the -gnatwa set

      Implementation_Unit_Warnings        := False;
      Warn_On_Non_Local_Exception         := False;
      No_Warn_On_Non_Local_Exception      := True;
      Warn_On_Reverse_Bit_Order           := False;
      Warn_On_Size_Alignment              := False;
      Warn_On_Unrepped_Components         := False;
   end Set_GNAT_Mode_Warnings;

   ------------------------
   -- Set_Warning_Switch --
   ------------------------

   function Set_Warning_Switch
     (Family : Warning_Family; C : Character) return Boolean
   is
      L : constant Character := To_Lower (C);
   begin
      --  Error case

      if L not in Lowercase
        or else Switch_To_Flag_Mapping (Family, L) = No_Such_Warning
      then
         if Ignore_Unrecognized_VWY_Switches then
            declare
               Family_Switch : constant String :=
                 (case Family is
                   when Plain => "", when '.' => ".", when '_' => "_");
            begin
               Write_Line
                 ("unrecognized switch -gnatw" & Family_Switch & C &
                  " ignored");
            end;
            return True;
         else
            return False;
         end if;
      end if;

      --  Special cases that don't fall into the normal pattern below

      if Switch_To_Flag_Mapping (Family, L) = Special_Case then
         case Family is
            when Plain =>
               case C is
                  when 'a' =>
                     --  "or" in the -gnatwa flags, possibly leaving others set
                     Warning_Flags := Warning_Flags or WA_Warnings;

                  when 'A' =>
                     --  Turn off the All_Warnings flags, except that
                     --  No_Warn_On_Non_Local_Exception is a special case.
                     Warning_Flags := Warning_Flags and not All_Warnings;
                     No_Warn_On_Non_Local_Exception := True;

                  when 'e' =>
                     Warning_Mode := Treat_As_Error;

                  when 'E' =>
                     Warning_Mode := Treat_Run_Time_Warnings_As_Errors;

                  when 'n' =>
                     Warning_Mode := Normal;

                  when 's' =>
                     Warning_Mode := Suppress;

                  when 'u' =>
                     Check_Unreferenced := True;
                     Check_Withs := True;
                     Check_Unreferenced_Formals := True;

                  when 'U' =>
                     Check_Unreferenced := False;
                     Check_Withs := False;
                     Check_Unreferenced_Formals := False;

                  when 'y' =>
                     Warn_On_Ada_2005_Compatibility := True;
                     Warn_On_Ada_2012_Compatibility := True;

                  when 'Y' =>
                     Warn_On_Ada_2005_Compatibility := False;
                     Warn_On_Ada_2012_Compatibility := False;

                  when others => raise Program_Error;
               end case;

            when '.' =>
               case C is
                  when 'e' =>
                     --  "or" in the All_Warnings flags
                     Warning_Flags := Warning_Flags or All_Warnings;
                  when 'g' =>
                     Set_GNAT_Mode_Warnings;

                  when 'x' =>
                     Warn_On_Non_Local_Exception := True;

                  when 'X' =>
                     Warn_On_Non_Local_Exception := False;
                     No_Warn_On_Non_Local_Exception := True;

                  when others => raise Program_Error;
               end case;

            when '_' =>
               raise Program_Error;
         end case;

         return True;
      end if;

      --  Normal pattern (lower case enables the warning, upper case disables
      --  the warning).

      if C in Lowercase then
         Warning_Flags (Switch_To_Flag_Mapping (Family, C)) := True;
      elsif L in Lowercase then
         Warning_Flags (Switch_To_Flag_Mapping (Family, L)) := False;
      else
         raise Program_Error;
      end if;

      return True;
   end Set_Warning_Switch;

end Warnsw;
