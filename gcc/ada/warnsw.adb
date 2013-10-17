------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               W A R N S W                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2013, Free Software Foundation, Inc.         --
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

with Err_Vars; use Err_Vars;
with Opt;      use Opt;
with Output;   use Output;

package body Warnsw is

   ----------------------
   -- Restore_Warnings --
   ----------------------

   procedure Restore_Warnings (W : Warning_Record) is
   begin
      Address_Clause_Overlay_Warnings     :=
        W.Address_Clause_Overlay_Warnings;
      Check_Unreferenced                  :=
        W.Check_Unreferenced;
      Check_Unreferenced_Formals          :=
        W.Check_Unreferenced_Formals;
      Check_Withs                         :=
        W.Check_Withs;
      Constant_Condition_Warnings         :=
        W.Constant_Condition_Warnings;
      Elab_Warnings                       :=
        W.Elab_Warnings;
      Implementation_Unit_Warnings        :=
        W.Implementation_Unit_Warnings;
      Ineffective_Inline_Warnings         :=
        W.Ineffective_Inline_Warnings;
      List_Body_Required_Info             :=
        W.List_Body_Required_Info;
      List_Inherited_Aspects              :=
        W.List_Inherited_Aspects;
      Warning_Doc_Switch                  :=
        W.Warning_Doc_Switch;
      Warn_On_Ada_2005_Compatibility      :=
        W.Warn_On_Ada_2005_Compatibility;
      Warn_On_Ada_2012_Compatibility      :=
        W.Warn_On_Ada_2012_Compatibility;
      Warn_On_All_Unread_Out_Parameters   :=
        W.Warn_On_All_Unread_Out_Parameters;
      Warn_On_Assertion_Failure           :=
        W.Warn_On_Assertion_Failure;
      Warn_On_Assumed_Low_Bound           :=
        W.Warn_On_Assumed_Low_Bound;
      Warn_On_Atomic_Synchronization      :=
        W.Warn_On_Atomic_Synchronization;
      Warn_On_Bad_Fixed_Value             :=
        W.Warn_On_Bad_Fixed_Value;
      Warn_On_Biased_Representation       :=
        W.Warn_On_Biased_Representation;
      Warn_On_Constant                    :=
        W.Warn_On_Constant;
      Warn_On_Deleted_Code                :=
        W.Warn_On_Deleted_Code;
      Warn_On_Dereference                 :=
        W.Warn_On_Dereference;
      Warn_On_Export_Import               :=
        W.Warn_On_Export_Import;
      Warn_On_Hiding                      :=
        W.Warn_On_Hiding;
      Warn_On_Modified_Unread             :=
        W.Warn_On_Modified_Unread;
      Warn_On_No_Value_Assigned           :=
        W.Warn_On_No_Value_Assigned;
      Warn_On_Non_Local_Exception         :=
        W.Warn_On_Non_Local_Exception;
      Warn_On_Object_Renames_Function     :=
        W.Warn_On_Object_Renames_Function;
      Warn_On_Obsolescent_Feature         :=
        W.Warn_On_Obsolescent_Feature;
      Warn_On_Overlap                     :=
        W.Warn_On_Overlap;
      Warn_On_Overridden_Size             :=
        W.Warn_On_Overridden_Size;
      Warn_On_Parameter_Order             :=
        W.Warn_On_Parameter_Order;
      Warn_On_Questionable_Missing_Parens :=
        W.Warn_On_Questionable_Missing_Parens;
      Warn_On_Record_Holes                :=
        W.Warn_On_Record_Holes;
      Warn_On_Redundant_Constructs        :=
        W.Warn_On_Redundant_Constructs;
      Warn_On_Reverse_Bit_Order           :=
        W.Warn_On_Reverse_Bit_Order;
      Warn_On_Standard_Redefinition       :=
        W.Warn_On_Standard_Redefinition;
      Warn_On_Suspicious_Contract         :=
        W.Warn_On_Suspicious_Contract;
      Warn_On_Unchecked_Conversion        :=
        W.Warn_On_Unchecked_Conversion;
      Warn_On_Unordered_Enumeration_Type  :=
        W.Warn_On_Unordered_Enumeration_Type;
      Warn_On_Unrecognized_Pragma         :=
        W.Warn_On_Unrecognized_Pragma;
      Warn_On_Unrepped_Components         :=
        W.Warn_On_Unrepped_Components;
      Warn_On_Warnings_Off                :=
        W.Warn_On_Warnings_Off;
   end Restore_Warnings;

   -------------------
   -- Save_Warnings --
   -------------------

   function Save_Warnings return Warning_Record is
      W : Warning_Record;

   begin
      W.Address_Clause_Overlay_Warnings     :=
        Address_Clause_Overlay_Warnings;
      W.Check_Unreferenced                  :=
        Check_Unreferenced;
      W.Check_Unreferenced_Formals          :=
        Check_Unreferenced_Formals;
      W.Check_Withs                         :=
        Check_Withs;
      W.Constant_Condition_Warnings         :=
        Constant_Condition_Warnings;
      W.Elab_Warnings                       :=
        Elab_Warnings;
      W.Implementation_Unit_Warnings        :=
        Implementation_Unit_Warnings;
      W.Ineffective_Inline_Warnings         :=
        Ineffective_Inline_Warnings;
      W.List_Body_Required_Info             :=
        List_Body_Required_Info;
      W.List_Inherited_Aspects              :=
        List_Inherited_Aspects;
      W.Warning_Doc_Switch                  :=
        Warning_Doc_Switch;
      W.Warn_On_Ada_2005_Compatibility      :=
        Warn_On_Ada_2005_Compatibility;
      W.Warn_On_Ada_2012_Compatibility      :=
        Warn_On_Ada_2012_Compatibility;
      W.Warn_On_All_Unread_Out_Parameters   :=
        Warn_On_All_Unread_Out_Parameters;
      W.Warn_On_Assertion_Failure           :=
        Warn_On_Assertion_Failure;
      W.Warn_On_Assumed_Low_Bound           :=
        Warn_On_Assumed_Low_Bound;
      W.Warn_On_Atomic_Synchronization      :=
        Warn_On_Atomic_Synchronization;
      W.Warn_On_Bad_Fixed_Value             :=
        Warn_On_Bad_Fixed_Value;
      W.Warn_On_Biased_Representation       :=
        Warn_On_Biased_Representation;
      W.Warn_On_Constant                    :=
        Warn_On_Constant;
      W.Warn_On_Deleted_Code                :=
        Warn_On_Deleted_Code;
      W.Warn_On_Dereference                 :=
        Warn_On_Dereference;
      W.Warn_On_Export_Import               :=
        Warn_On_Export_Import;
      W.Warn_On_Hiding                      :=
        Warn_On_Hiding;
      W.Warn_On_Modified_Unread             :=
        Warn_On_Modified_Unread;
      W.Warn_On_No_Value_Assigned           :=
        Warn_On_No_Value_Assigned;
      W.Warn_On_Non_Local_Exception         :=
        Warn_On_Non_Local_Exception;
      W.Warn_On_Object_Renames_Function     :=
        Warn_On_Object_Renames_Function;
      W.Warn_On_Obsolescent_Feature         :=
        Warn_On_Obsolescent_Feature;
      W.Warn_On_Overlap                     :=
        Warn_On_Overlap;
      W.Warn_On_Overridden_Size             :=
        Warn_On_Overridden_Size;
      W.Warn_On_Parameter_Order             :=
        Warn_On_Parameter_Order;
      W.Warn_On_Questionable_Missing_Parens :=
        Warn_On_Questionable_Missing_Parens;
      W.Warn_On_Record_Holes                :=
        Warn_On_Record_Holes;
      W.Warn_On_Redundant_Constructs        :=
        Warn_On_Redundant_Constructs;
      W.Warn_On_Reverse_Bit_Order           :=
        Warn_On_Reverse_Bit_Order;
      W.Warn_On_Standard_Redefinition       :=
        Warn_On_Standard_Redefinition;
      W.Warn_On_Suspicious_Contract         :=
        Warn_On_Suspicious_Contract;
      W.Warn_On_Unchecked_Conversion        :=
        Warn_On_Unchecked_Conversion;
      W.Warn_On_Unordered_Enumeration_Type  :=
        Warn_On_Unordered_Enumeration_Type;
      W.Warn_On_Unrecognized_Pragma         :=
        Warn_On_Unrecognized_Pragma;
      W.Warn_On_Unrepped_Components         :=
        Warn_On_Unrepped_Components;
      W.Warn_On_Warnings_Off                :=
        Warn_On_Warnings_Off;

      return W;
   end Save_Warnings;

   ----------------------------
   -- Set_Dot_Warning_Switch --
   ----------------------------

   function Set_Dot_Warning_Switch (C : Character) return Boolean is
   begin
      case C is
         when 'a' =>
            Warn_On_Assertion_Failure           := True;

         when 'A' =>
            Warn_On_Assertion_Failure           := False;

         when 'b' =>
            Warn_On_Biased_Representation       := True;

         when 'B' =>
            Warn_On_Biased_Representation       := False;

         when 'c' =>
            Warn_On_Unrepped_Components         := True;

         when 'C' =>
            Warn_On_Unrepped_Components         := False;

         when 'd' =>
            Warning_Doc_Switch                  := True;

         when 'D' =>
            Warning_Doc_Switch                  := False;

         when 'e' =>
            Address_Clause_Overlay_Warnings     := True;
            Check_Unreferenced                  := True;
            Check_Unreferenced_Formals          := True;
            Check_Withs                         := True;
            Constant_Condition_Warnings         := True;
            Elab_Warnings                       := True;
            Implementation_Unit_Warnings        := True;
            Ineffective_Inline_Warnings         := True;
            List_Body_Required_Info             := True;
            List_Inherited_Aspects              := True;
            Warning_Doc_Switch                  := True;
            Warn_On_Ada_2005_Compatibility      := True;
            Warn_On_Ada_2012_Compatibility      := True;
            Warn_On_All_Unread_Out_Parameters   := True;
            Warn_On_Assertion_Failure           := True;
            Warn_On_Assumed_Low_Bound           := True;
            Warn_On_Atomic_Synchronization      := True;
            Warn_On_Bad_Fixed_Value             := True;
            Warn_On_Biased_Representation       := True;
            Warn_On_Constant                    := True;
            Warn_On_Deleted_Code                := True;
            Warn_On_Dereference                 := True;
            Warn_On_Export_Import               := True;
            Warn_On_Hiding                      := True;
            Warn_On_Modified_Unread             := True;
            Warn_On_No_Value_Assigned           := True;
            Warn_On_Non_Local_Exception         := True;
            Warn_On_Object_Renames_Function     := True;
            Warn_On_Obsolescent_Feature         := True;
            Warn_On_Overlap                     := True;
            Warn_On_Overridden_Size             := True;
            Warn_On_Parameter_Order             := True;
            Warn_On_Questionable_Missing_Parens := True;
            Warn_On_Record_Holes                := True;
            Warn_On_Redundant_Constructs        := True;
            Warn_On_Reverse_Bit_Order           := True;
            Warn_On_Standard_Redefinition       := True;
            Warn_On_Suspicious_Contract         := True;
            Warn_On_Unchecked_Conversion        := True;
            Warn_On_Unordered_Enumeration_Type  := True;
            Warn_On_Unrecognized_Pragma         := True;
            Warn_On_Unrepped_Components         := True;
            Warn_On_Warnings_Off                := True;

         when 'g' =>
            Set_GNAT_Mode_Warnings;

         when 'h' =>
            Warn_On_Record_Holes                := True;

         when 'H' =>
            Warn_On_Record_Holes                := False;

         when 'i' =>
            Warn_On_Overlap                     := True;

         when 'I' =>
            Warn_On_Overlap                     := False;

         when 'k' =>
            Warn_On_Standard_Redefinition       := True;

         when 'K' =>
            Warn_On_Standard_Redefinition       := False;

         when 'l' =>
            List_Inherited_Aspects              := True;

         when 'L' =>
            List_Inherited_Aspects              := False;

         when 'm' =>
            Warn_On_Suspicious_Modulus_Value    := True;

         when 'M' =>
            Warn_On_Suspicious_Modulus_Value    := False;

         when 'n' =>
            Warn_On_Atomic_Synchronization      := True;

         when 'N' =>
            Warn_On_Atomic_Synchronization      := False;

         when 'o' =>
            Warn_On_All_Unread_Out_Parameters   := True;

         when 'O' =>
            Warn_On_All_Unread_Out_Parameters   := False;

         when 'p' =>
            Warn_On_Parameter_Order             := True;

         when 'P' =>
            Warn_On_Parameter_Order             := False;

         when 'r' =>
            Warn_On_Object_Renames_Function     := True;

         when 'R' =>
            Warn_On_Object_Renames_Function     := False;

         when 's' =>
            Warn_On_Overridden_Size             := True;

         when 'S' =>
            Warn_On_Overridden_Size             := False;

         when 't' =>
            Warn_On_Suspicious_Contract         := True;

         when 'T' =>
            Warn_On_Suspicious_Contract         := False;

         when 'u' =>
            Warn_On_Unordered_Enumeration_Type  := True;

         when 'U' =>
            Warn_On_Unordered_Enumeration_Type  := False;

         when 'v' =>
            Warn_On_Reverse_Bit_Order           := True;

         when 'V' =>
            Warn_On_Reverse_Bit_Order           := False;

         when 'w' =>
            Warn_On_Warnings_Off                := True;

         when 'W' =>
            Warn_On_Warnings_Off                := False;

         when 'x' =>
            Warn_On_Non_Local_Exception         := True;

         when 'X' =>
            Warn_On_Non_Local_Exception         := False;
            No_Warn_On_Non_Local_Exception      := True;

         when 'y' =>
            List_Body_Required_Info             := True;

         when 'Y' =>
            List_Body_Required_Info             := False;

         when others =>
            if Ignore_Unrecognized_VWY_Switches then
               Write_Line ("unrecognized switch -gnatw." & C & " ignored");
            else
               return False;
            end if;
      end case;

      return True;
   end Set_Dot_Warning_Switch;

   ----------------------------
   -- Set_GNAT_Mode_Warnings --
   ----------------------------

   procedure Set_GNAT_Mode_Warnings is
   begin
      Address_Clause_Overlay_Warnings     := True;
      Check_Unreferenced                  := True;
      Check_Unreferenced_Formals          := True;
      Check_Withs                         := True;
      Constant_Condition_Warnings         := True;
      Elab_Warnings                       := False;
      Implementation_Unit_Warnings        := False;
      Ineffective_Inline_Warnings         := True;
      List_Body_Required_Info             := False;
      List_Inherited_Aspects              := False;
      Warning_Doc_Switch                  := False;
      Warn_On_Ada_2005_Compatibility      := True;
      Warn_On_Ada_2012_Compatibility      := True;
      Warn_On_All_Unread_Out_Parameters   := False;
      Warn_On_Assertion_Failure           := True;
      Warn_On_Assumed_Low_Bound           := True;
      Warn_On_Atomic_Synchronization      := False;
      Warn_On_Bad_Fixed_Value             := True;
      Warn_On_Biased_Representation       := True;
      Warn_On_Constant                    := True;
      Warn_On_Deleted_Code                := False;
      Warn_On_Dereference                 := False;
      Warn_On_Export_Import               := True;
      Warn_On_Hiding                      := False;
      Warn_On_Modified_Unread             := True;
      Warn_On_No_Value_Assigned           := True;
      Warn_On_Non_Local_Exception         := False;
      Warn_On_Object_Renames_Function     := True;
      Warn_On_Obsolescent_Feature         := True;
      Warn_On_Overlap                     := True;
      Warn_On_Overridden_Size             := True;
      Warn_On_Parameter_Order             := True;
      Warn_On_Questionable_Missing_Parens := True;
      Warn_On_Record_Holes                := False;
      Warn_On_Redundant_Constructs        := True;
      Warn_On_Reverse_Bit_Order           := False;
      Warn_On_Suspicious_Contract         := True;
      Warn_On_Unchecked_Conversion        := True;
      Warn_On_Unordered_Enumeration_Type  := False;
      Warn_On_Unrecognized_Pragma         := True;
      Warn_On_Unrepped_Components         := False;
      Warn_On_Warnings_Off                := False;
   end Set_GNAT_Mode_Warnings;

   ------------------------
   -- Set_Warning_Switch --
   ------------------------

   function Set_Warning_Switch (C : Character) return Boolean is
   begin
      case C is
         when 'a' =>
            Check_Unreferenced                  := True;
            Check_Unreferenced_Formals          := True;
            Check_Withs                         := True;
            Constant_Condition_Warnings         := True;
            Implementation_Unit_Warnings        := True;
            Ineffective_Inline_Warnings         := True;
            Warn_On_Ada_2005_Compatibility      := True;
            Warn_On_Ada_2012_Compatibility      := True;
            Warn_On_Assertion_Failure           := True;
            Warn_On_Assumed_Low_Bound           := True;
            Warn_On_Bad_Fixed_Value             := True;
            Warn_On_Biased_Representation       := True;
            Warn_On_Constant                    := True;
            Warn_On_Export_Import               := True;
            Warn_On_Modified_Unread             := True;
            Warn_On_No_Value_Assigned           := True;
            Warn_On_Non_Local_Exception         := True;
            Warn_On_Object_Renames_Function     := True;
            Warn_On_Obsolescent_Feature         := True;
            Warn_On_Overlap                     := True;
            Warn_On_Parameter_Order             := True;
            Warn_On_Questionable_Missing_Parens := True;
            Warn_On_Redundant_Constructs        := True;
            Warn_On_Reverse_Bit_Order           := True;
            Warn_On_Suspicious_Contract         := True;
            Warn_On_Unchecked_Conversion        := True;
            Warn_On_Unrecognized_Pragma         := True;
            Warn_On_Unrepped_Components         := True;

         when 'A' =>
            Address_Clause_Overlay_Warnings     := False;
            Check_Unreferenced                  := False;
            Check_Unreferenced_Formals          := False;
            Check_Withs                         := False;
            Constant_Condition_Warnings         := False;
            Elab_Warnings                       := False;
            Implementation_Unit_Warnings        := False;
            Ineffective_Inline_Warnings         := False;
            List_Body_Required_Info             := False;
            List_Inherited_Aspects              := False;
            Warning_Doc_Switch                  := False;
            Warn_On_Ada_2005_Compatibility      := False;
            Warn_On_Ada_2012_Compatibility      := False;
            Warn_On_All_Unread_Out_Parameters   := False;
            Warn_On_Assertion_Failure           := False;
            Warn_On_Assumed_Low_Bound           := False;
            Warn_On_Bad_Fixed_Value             := False;
            Warn_On_Biased_Representation       := False;
            Warn_On_Constant                    := False;
            Warn_On_Deleted_Code                := False;
            Warn_On_Dereference                 := False;
            Warn_On_Export_Import               := False;
            Warn_On_Hiding                      := False;
            Warn_On_Modified_Unread             := False;
            Warn_On_No_Value_Assigned           := False;
            Warn_On_Non_Local_Exception         := False;
            Warn_On_Object_Renames_Function     := False;
            Warn_On_Obsolescent_Feature         := False;
            Warn_On_Overlap                     := False;
            Warn_On_Overridden_Size             := False;
            Warn_On_Parameter_Order             := False;
            Warn_On_Record_Holes                := False;
            Warn_On_Questionable_Missing_Parens := False;
            Warn_On_Redundant_Constructs        := False;
            Warn_On_Reverse_Bit_Order           := False;
            Warn_On_Standard_Redefinition       := False;
            Warn_On_Suspicious_Contract         := False;
            Warn_On_Suspicious_Modulus_Value    := False;
            Warn_On_Unchecked_Conversion        := False;
            Warn_On_Unordered_Enumeration_Type  := False;
            Warn_On_Unrecognized_Pragma         := False;
            Warn_On_Unrepped_Components         := False;
            Warn_On_Warnings_Off                := False;

            No_Warn_On_Non_Local_Exception      := True;

         when 'b' =>
            Warn_On_Bad_Fixed_Value             := True;

         when 'B' =>
            Warn_On_Bad_Fixed_Value             := False;

         when 'c' =>
            Constant_Condition_Warnings         := True;

         when 'C' =>
            Constant_Condition_Warnings         := False;

         when 'd' =>
            Warn_On_Dereference                 := True;

         when 'D' =>
            Warn_On_Dereference                 := False;

         when 'e' =>
            Warning_Mode                        := Treat_As_Error;

         when 'f' =>
            Check_Unreferenced_Formals          := True;

         when 'F' =>
            Check_Unreferenced_Formals          := False;

         when 'g' =>
            Warn_On_Unrecognized_Pragma         := True;

         when 'G' =>
            Warn_On_Unrecognized_Pragma         := False;

         when 'h' =>
            Warn_On_Hiding                      := True;

         when 'H' =>
            Warn_On_Hiding                      := False;

         when 'i' =>
            Implementation_Unit_Warnings        := True;

         when 'I' =>
            Implementation_Unit_Warnings        := False;

         when 'j' =>
            Warn_On_Obsolescent_Feature         := True;

         when 'J' =>
            Warn_On_Obsolescent_Feature         := False;

         when 'k' =>
            Warn_On_Constant                    := True;

         when 'K' =>
            Warn_On_Constant                    := False;

         when 'l' =>
            Elab_Warnings                       := True;

         when 'L' =>
            Elab_Warnings                       := False;

         when 'm' =>
            Warn_On_Modified_Unread             := True;

         when 'M' =>
            Warn_On_Modified_Unread             := False;

         when 'n' =>
            Warning_Mode                        := Normal;

         when 'o' =>
            Address_Clause_Overlay_Warnings     := True;

         when 'O' =>
            Address_Clause_Overlay_Warnings     := False;

         when 'p' =>
            Ineffective_Inline_Warnings         := True;

         when 'P' =>
            Ineffective_Inline_Warnings         := False;

         when 'q' =>
            Warn_On_Questionable_Missing_Parens := True;

         when 'Q' =>
            Warn_On_Questionable_Missing_Parens := False;

         when 'r' =>
            Warn_On_Redundant_Constructs        := True;

         when 'R' =>
            Warn_On_Redundant_Constructs        := False;

         when 's' =>
            Warning_Mode                        := Suppress;

         when 't' =>
            Warn_On_Deleted_Code                := True;

         when 'T' =>
            Warn_On_Deleted_Code                := False;

         when 'u' =>
            Check_Unreferenced                  := True;
            Check_Withs                         := True;
            Check_Unreferenced_Formals          := True;

         when 'U' =>
            Check_Unreferenced                  := False;
            Check_Withs                         := False;
            Check_Unreferenced_Formals          := False;

         when 'v' =>
            Warn_On_No_Value_Assigned           := True;

         when 'V' =>
            Warn_On_No_Value_Assigned           := False;

         when 'w' =>
            Warn_On_Assumed_Low_Bound           := True;

         when 'W' =>
            Warn_On_Assumed_Low_Bound           := False;

         when 'x' =>
            Warn_On_Export_Import               := True;

         when 'X' =>
            Warn_On_Export_Import               := False;

         when 'y' =>
            Warn_On_Ada_2005_Compatibility      := True;
            Warn_On_Ada_2012_Compatibility      := True;

         when 'Y' =>
            Warn_On_Ada_2005_Compatibility      := False;
            Warn_On_Ada_2012_Compatibility      := False;

         when 'z' =>
            Warn_On_Unchecked_Conversion        := True;

         when 'Z' =>
            Warn_On_Unchecked_Conversion        := False;

         when others =>
            if Ignore_Unrecognized_VWY_Switches then
               Write_Line ("unrecognized switch -gnatw" & C & " ignored");
            else
               return False;
            end if;
            return False;
      end case;

      return True;
   end Set_Warning_Switch;

end Warnsw;
