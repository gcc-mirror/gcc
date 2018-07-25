------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Gnatvsn; use Gnatvsn;
with System;  use System;
with Tree_IO; use Tree_IO;

package body Opt is

   SU : constant := Storage_Unit;
   --  Shorthand for System.Storage_Unit

   -------------------------
   -- Back_End_Exceptions --
   -------------------------

   function Back_End_Exceptions return Boolean is
   begin
      return
        Exception_Mechanism = Back_End_SJLJ
          or else
        Exception_Mechanism = Back_End_ZCX;
   end Back_End_Exceptions;

   -------------------------
   -- Front_End_Exceptions --
   -------------------------

   function Front_End_Exceptions return Boolean is
   begin
      return Exception_Mechanism = Front_End_SJLJ;
   end Front_End_Exceptions;

   --------------------
   -- SJLJ_Exceptions --
   --------------------

   function SJLJ_Exceptions return Boolean is
   begin
      return
        Exception_Mechanism = Back_End_SJLJ
          or else
        Exception_Mechanism = Front_End_SJLJ;
   end SJLJ_Exceptions;

   --------------------
   -- ZCX_Exceptions --
   --------------------

   function ZCX_Exceptions return Boolean is
   begin
      return Exception_Mechanism = Back_End_ZCX;
   end ZCX_Exceptions;

   ------------------------------
   -- Register_Config_Switches --
   ------------------------------

   procedure Register_Config_Switches is
   begin
      Ada_Version_Config                    := Ada_Version;
      Ada_Version_Pragma_Config             := Ada_Version_Pragma;
      Ada_Version_Explicit_Config           := Ada_Version_Explicit;
      Assertions_Enabled_Config             := Assertions_Enabled;
      Assume_No_Invalid_Values_Config       := Assume_No_Invalid_Values;
      Check_Float_Overflow_Config           := Check_Float_Overflow;
      Check_Policy_List_Config              := Check_Policy_List;
      Default_Pool_Config                   := Default_Pool;
      Default_SSO_Config                    := Default_SSO;
      Dynamic_Elaboration_Checks_Config     := Dynamic_Elaboration_Checks;
      Exception_Locations_Suppressed_Config := Exception_Locations_Suppressed;
      Extensions_Allowed_Config             := Extensions_Allowed;
      External_Name_Exp_Casing_Config       := External_Name_Exp_Casing;
      External_Name_Imp_Casing_Config       := External_Name_Imp_Casing;
      Fast_Math_Config                      := Fast_Math;
      Initialize_Scalars_Config             := Initialize_Scalars;
      No_Component_Reordering_Config        := No_Component_Reordering;
      Optimize_Alignment_Config             := Optimize_Alignment;
      Persistent_BSS_Mode_Config            := Persistent_BSS_Mode;
      Polling_Required_Config               := Polling_Required;
      Prefix_Exception_Messages_Config      := Prefix_Exception_Messages;
      SPARK_Mode_Config                     := SPARK_Mode;
      SPARK_Mode_Pragma_Config              := SPARK_Mode_Pragma;
      Uneval_Old_Config                     := Uneval_Old;
      Use_VADS_Size_Config                  := Use_VADS_Size;
      Warnings_As_Errors_Count_Config       := Warnings_As_Errors_Count;

      --  Reset the indication that Optimize_Alignment was set locally, since
      --  if we had a pragma in the config file, it would set this flag True,
      --  but that's not a local setting.

      Optimize_Alignment_Local := False;
   end Register_Config_Switches;

   -----------------------------
   -- Restore_Config_Switches --
   -----------------------------

   procedure Restore_Config_Switches (Save : Config_Switches_Type) is
   begin
      Ada_Version                    := Save.Ada_Version;
      Ada_Version_Pragma             := Save.Ada_Version_Pragma;
      Ada_Version_Explicit           := Save.Ada_Version_Explicit;
      Assertions_Enabled             := Save.Assertions_Enabled;
      Assume_No_Invalid_Values       := Save.Assume_No_Invalid_Values;
      Check_Float_Overflow           := Save.Check_Float_Overflow;
      Check_Policy_List              := Save.Check_Policy_List;
      Default_Pool                   := Save.Default_Pool;
      Default_SSO                    := Save.Default_SSO;
      Dynamic_Elaboration_Checks     := Save.Dynamic_Elaboration_Checks;
      Exception_Locations_Suppressed := Save.Exception_Locations_Suppressed;
      Extensions_Allowed             := Save.Extensions_Allowed;
      External_Name_Exp_Casing       := Save.External_Name_Exp_Casing;
      External_Name_Imp_Casing       := Save.External_Name_Imp_Casing;
      Fast_Math                      := Save.Fast_Math;
      Initialize_Scalars             := Save.Initialize_Scalars;
      No_Component_Reordering        := Save.No_Component_Reordering;
      Optimize_Alignment             := Save.Optimize_Alignment;
      Optimize_Alignment_Local       := Save.Optimize_Alignment_Local;
      Persistent_BSS_Mode            := Save.Persistent_BSS_Mode;
      Polling_Required               := Save.Polling_Required;
      Prefix_Exception_Messages      := Save.Prefix_Exception_Messages;
      SPARK_Mode                     := Save.SPARK_Mode;
      SPARK_Mode_Pragma              := Save.SPARK_Mode_Pragma;
      Uneval_Old                     := Save.Uneval_Old;
      Use_VADS_Size                  := Save.Use_VADS_Size;
      Warnings_As_Errors_Count       := Save.Warnings_As_Errors_Count;

      --  Update consistently the value of Init_Or_Norm_Scalars. The value of
      --  Normalize_Scalars is not saved/restored because after set to True its
      --  value is never changed. That is, if a compilation unit has pragma
      --  Normalize_Scalars then it forces that value for all with'ed units.

      Init_Or_Norm_Scalars := Initialize_Scalars or Normalize_Scalars;
   end Restore_Config_Switches;

   --------------------------
   -- Save_Config_Switches --
   --------------------------

   function Save_Config_Switches return Config_Switches_Type is
   begin
      return
        (Ada_Version                    => Ada_Version,
         Ada_Version_Pragma             => Ada_Version_Pragma,
         Ada_Version_Explicit           => Ada_Version_Explicit,
         Assertions_Enabled             => Assertions_Enabled,
         Assume_No_Invalid_Values       => Assume_No_Invalid_Values,
         Check_Float_Overflow           => Check_Float_Overflow,
         Check_Policy_List              => Check_Policy_List,
         Default_Pool                   => Default_Pool,
         Default_SSO                    => Default_SSO,
         Dynamic_Elaboration_Checks     => Dynamic_Elaboration_Checks,
         Exception_Locations_Suppressed => Exception_Locations_Suppressed,
         Extensions_Allowed             => Extensions_Allowed,
         External_Name_Exp_Casing       => External_Name_Exp_Casing,
         External_Name_Imp_Casing       => External_Name_Imp_Casing,
         Fast_Math                      => Fast_Math,
         Initialize_Scalars             => Initialize_Scalars,
         No_Component_Reordering        => No_Component_Reordering,
         Normalize_Scalars              => Normalize_Scalars,
         Optimize_Alignment             => Optimize_Alignment,
         Optimize_Alignment_Local       => Optimize_Alignment_Local,
         Persistent_BSS_Mode            => Persistent_BSS_Mode,
         Polling_Required               => Polling_Required,
         Prefix_Exception_Messages      => Prefix_Exception_Messages,
         SPARK_Mode                     => SPARK_Mode,
         SPARK_Mode_Pragma              => SPARK_Mode_Pragma,
         Uneval_Old                     => Uneval_Old,
         Use_VADS_Size                  => Use_VADS_Size,
         Warnings_As_Errors_Count       => Warnings_As_Errors_Count);
   end Save_Config_Switches;

   -------------------------
   -- Set_Config_Switches --
   -------------------------

   procedure Set_Config_Switches
     (Internal_Unit : Boolean;
      Main_Unit     : Boolean)
   is
   begin
      --  Case of internal unit

      if Internal_Unit then

         --  Set standard switches. Note we do NOT set Ada_Version_Explicit
         --  since the whole point of this is that it still properly indicates
         --  the configuration setting even in a run time unit.

         Ada_Version                 := Ada_Version_Runtime;
         Ada_Version_Pragma          := Empty;
         Default_SSO                 := ' ';
         Dynamic_Elaboration_Checks  := False;
         Extensions_Allowed          := True;
         External_Name_Exp_Casing    := As_Is;
         External_Name_Imp_Casing    := Lowercase;
         No_Component_Reordering     := False;
         Optimize_Alignment          := 'O';
         Optimize_Alignment_Local    := True;
         Persistent_BSS_Mode         := False;
         Prefix_Exception_Messages   := True;
         Uneval_Old                  := 'E';
         Use_VADS_Size               := False;

         --  Note: we do not need to worry about Warnings_As_Errors_Count since
         --  we do not expect to get any warnings from compiling such a unit.

         --  For an internal unit, assertions/debug pragmas are off unless this
         --  is the main unit and they were explicitly enabled, or unless the
         --  main unit was compiled in GNAT mode. We also make sure we do not
         --  assume that values are necessarily valid and that SPARK_Mode is
         --  set to its configuration value.

         if Main_Unit then
            Assertions_Enabled       := Assertions_Enabled_Config;
            Assume_No_Invalid_Values := Assume_No_Invalid_Values_Config;
            Check_Policy_List        := Check_Policy_List_Config;
            SPARK_Mode               := SPARK_Mode_Config;
            SPARK_Mode_Pragma        := SPARK_Mode_Pragma_Config;

         else
            if GNAT_Mode_Config then
               Assertions_Enabled    := Assertions_Enabled_Config;
            else
               Assertions_Enabled    := False;
            end if;

            Assume_No_Invalid_Values := False;
            Check_Policy_List        := Empty;
            SPARK_Mode               := None;
            SPARK_Mode_Pragma        := Empty;
         end if;

      --  Case of non-internal unit

      else
         Ada_Version                 := Ada_Version_Config;
         Ada_Version_Pragma          := Ada_Version_Pragma_Config;
         Ada_Version_Explicit        := Ada_Version_Explicit_Config;
         Assertions_Enabled          := Assertions_Enabled_Config;
         Assume_No_Invalid_Values    := Assume_No_Invalid_Values_Config;
         Check_Float_Overflow        := Check_Float_Overflow_Config;
         Check_Policy_List           := Check_Policy_List_Config;
         Default_SSO                 := Default_SSO_Config;
         Dynamic_Elaboration_Checks  := Dynamic_Elaboration_Checks_Config;
         Extensions_Allowed          := Extensions_Allowed_Config;
         External_Name_Exp_Casing    := External_Name_Exp_Casing_Config;
         External_Name_Imp_Casing    := External_Name_Imp_Casing_Config;
         Fast_Math                   := Fast_Math_Config;
         Initialize_Scalars          := Initialize_Scalars_Config;
         No_Component_Reordering     := No_Component_Reordering_Config;
         Optimize_Alignment          := Optimize_Alignment_Config;
         Optimize_Alignment_Local    := False;
         Persistent_BSS_Mode         := Persistent_BSS_Mode_Config;
         Prefix_Exception_Messages   := Prefix_Exception_Messages_Config;
         SPARK_Mode                  := SPARK_Mode_Config;
         SPARK_Mode_Pragma           := SPARK_Mode_Pragma_Config;
         Uneval_Old                  := Uneval_Old_Config;
         Use_VADS_Size               := Use_VADS_Size_Config;
         Warnings_As_Errors_Count    := Warnings_As_Errors_Count_Config;

         --  Update consistently the value of Init_Or_Norm_Scalars. The value
         --  of Normalize_Scalars is not saved/restored because once set to
         --  True its value is never changed. That is, if a compilation unit
         --  has pragma Normalize_Scalars then it forces that value for all
         --  with'ed units.

         Init_Or_Norm_Scalars := Initialize_Scalars or Normalize_Scalars;
      end if;

      --  Values set for all units

      Default_Pool                   := Default_Pool_Config;
      Exception_Locations_Suppressed := Exception_Locations_Suppressed_Config;
      Fast_Math                      := Fast_Math_Config;
      Polling_Required               := Polling_Required_Config;
   end Set_Config_Switches;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
      Tree_Version_String_Len         : Nat;
      Ada_Version_Config_Val          : Nat;
      Ada_Version_Explicit_Config_Val : Nat;
      Assertions_Enabled_Config_Val   : Nat;

   begin
      Tree_Read_Int  (Tree_ASIS_Version_Number);

      Tree_Read_Bool (Address_Is_Private);
      Tree_Read_Bool (Brief_Output);
      Tree_Read_Bool (GNAT_Mode);
      Tree_Read_Char (Identifier_Character_Set);
      Tree_Read_Bool (Ignore_Rep_Clauses);
      Tree_Read_Bool (Ignore_Style_Checks_Pragmas);
      Tree_Read_Int  (Maximum_File_Name_Length);
      Tree_Read_Data (Suppress_Options'Address,
                      (Suppress_Options'Size + SU - 1) / SU);
      Tree_Read_Bool (Verbose_Mode);
      Tree_Read_Data (Warning_Mode'Address,
                      (Warning_Mode'Size + SU - 1) / SU);
      Tree_Read_Int  (Ada_Version_Config_Val);
      Tree_Read_Int  (Ada_Version_Explicit_Config_Val);
      Tree_Read_Int  (Assertions_Enabled_Config_Val);
      Tree_Read_Bool (All_Errors_Mode);
      Tree_Read_Bool (Assertions_Enabled);
      Tree_Read_Bool (Check_Float_Overflow);
      Tree_Read_Int  (Int (Check_Policy_List));
      Tree_Read_Int  (Int (Default_Pool));
      Tree_Read_Bool (Full_List);

      Ada_Version_Config :=
        Ada_Version_Type'Val (Ada_Version_Config_Val);
      Ada_Version_Explicit_Config :=
        Ada_Version_Type'Val (Ada_Version_Explicit_Config_Val);
      Assertions_Enabled_Config :=
        Boolean'Val (Assertions_Enabled_Config_Val);

      --  Read version string: we have to get the length first

      Tree_Read_Int (Tree_Version_String_Len);

      declare
         Tmp : String (1 .. Integer (Tree_Version_String_Len));
      begin
         Tree_Read_Data
           (Tmp'Address, Tree_Version_String_Len);
         System.Strings.Free (Tree_Version_String);
         Free (Tree_Version_String);
         Tree_Version_String := new String'(Tmp);
      end;

      Tree_Read_Data (Distribution_Stub_Mode'Address,
                      (Distribution_Stub_Mode'Size + SU - 1) / Storage_Unit);
      Tree_Read_Bool (Inline_Active);
      Tree_Read_Bool (Inline_Processing_Required);
      Tree_Read_Bool (List_Units);
      Tree_Read_Int  (Multiple_Unit_Index);
      Tree_Read_Bool (Configurable_Run_Time_Mode);
      Tree_Read_Data (Operating_Mode'Address,
                      (Operating_Mode'Size + SU - 1) / Storage_Unit);
      Tree_Read_Bool (Suppress_Checks);
      Tree_Read_Bool (Try_Semantics);
      Tree_Read_Data (Wide_Character_Encoding_Method'Address,
                      (Wide_Character_Encoding_Method'Size + SU - 1) / SU);
      Tree_Read_Bool (Upper_Half_Encoding);
      Tree_Read_Bool (Force_ALI_Tree_File);
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
      Version_String : String := Gnat_Version_String;

   begin
      Tree_Write_Int  (ASIS_Version_Number);

      Tree_Write_Bool (Address_Is_Private);
      Tree_Write_Bool (Brief_Output);
      Tree_Write_Bool (GNAT_Mode);
      Tree_Write_Char (Identifier_Character_Set);
      Tree_Write_Bool (Ignore_Rep_Clauses);
      Tree_Write_Bool (Ignore_Style_Checks_Pragmas);
      Tree_Write_Int  (Maximum_File_Name_Length);
      Tree_Write_Data (Suppress_Options'Address,
                       (Suppress_Options'Size + SU - 1) / SU);
      Tree_Write_Bool (Verbose_Mode);
      Tree_Write_Data (Warning_Mode'Address,
                       (Warning_Mode'Size + SU - 1) / Storage_Unit);
      Tree_Write_Int  (Ada_Version_Type'Pos (Ada_Version_Config));
      Tree_Write_Int  (Ada_Version_Type'Pos (Ada_Version_Explicit_Config));
      Tree_Write_Int  (Boolean'Pos (Assertions_Enabled_Config));
      Tree_Write_Bool (All_Errors_Mode);
      Tree_Write_Bool (Assertions_Enabled);
      Tree_Write_Bool (Check_Float_Overflow);
      Tree_Write_Int  (Int (Check_Policy_List));
      Tree_Write_Int  (Int (Default_Pool));
      Tree_Write_Bool (Full_List);
      Tree_Write_Int  (Int (Version_String'Length));
      Tree_Write_Data (Version_String'Address, Version_String'Length);
      Tree_Write_Data (Distribution_Stub_Mode'Address,
                       (Distribution_Stub_Mode'Size + SU - 1) / SU);
      Tree_Write_Bool (Inline_Active);
      Tree_Write_Bool (Inline_Processing_Required);
      Tree_Write_Bool (List_Units);
      Tree_Write_Int  (Multiple_Unit_Index);
      Tree_Write_Bool (Configurable_Run_Time_Mode);
      Tree_Write_Data (Operating_Mode'Address,
                       (Operating_Mode'Size + SU - 1) / SU);
      Tree_Write_Bool (Suppress_Checks);
      Tree_Write_Bool (Try_Semantics);
      Tree_Write_Data (Wide_Character_Encoding_Method'Address,
                       (Wide_Character_Encoding_Method'Size + SU - 1) / SU);
      Tree_Write_Bool (Upper_Half_Encoding);
      Tree_Write_Bool (Force_ALI_Tree_File);
   end Tree_Write;

end Opt;
