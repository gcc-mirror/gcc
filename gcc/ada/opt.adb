------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;
with Gnatvsn; use Gnatvsn;
with System;  use System;
with Tree_IO; use Tree_IO;

package body Opt is

   Tree_Version_String : String (Gnat_Version_String'Range);
   --  Used to store the compiler version string read from a tree file to
   --  check if it is the same as stored in the version ctring in Gnatvsn.
   --  Therefore its length is taken directly from the version string in
   --  Gnatvsn. If the length of the version string stored in the three is
   --  different, then versions are for sure different.

   Immediate_Errors : Boolean := True;
   --  This is an obsolete flag that is no longer present in opt.ads. We
   --  retain it here because this flag was written to the tree and there
   --  is no point in making trees incomaptible just for the sake of saving
   --  one byte of data. The value written is ignored.

   ----------------------------------
   -- Register_Opt_Config_Switches --
   ----------------------------------

   procedure Register_Opt_Config_Switches is
   begin
      Ada_83_Config                     := Ada_83;
      Dynamic_Elaboration_Checks_Config := Dynamic_Elaboration_Checks;
      Extensions_Allowed_Config         := Extensions_Allowed;
      External_Name_Exp_Casing_Config   := External_Name_Exp_Casing;
      External_Name_Imp_Casing_Config   := External_Name_Imp_Casing;
      Polling_Required_Config           := Polling_Required;
      Use_VADS_Size_Config              := Use_VADS_Size;
   end Register_Opt_Config_Switches;

   ---------------------------------
   -- Restore_Opt_Config_Switches --
   ---------------------------------

   procedure Restore_Opt_Config_Switches (Save : Config_Switches_Type) is
   begin
      Ada_83                     := Save.Ada_83;
      Ada_95                     := not Ada_83;
      Dynamic_Elaboration_Checks := Save.Dynamic_Elaboration_Checks;
      Extensions_Allowed         := Save.Extensions_Allowed;
      External_Name_Exp_Casing   := Save.External_Name_Exp_Casing;
      External_Name_Imp_Casing   := Save.External_Name_Imp_Casing;
      Polling_Required           := Save.Polling_Required;
      Use_VADS_Size              := Save.Use_VADS_Size;
   end Restore_Opt_Config_Switches;

   ------------------------------
   -- Save_Opt_Config_Switches --
   ------------------------------

   procedure Save_Opt_Config_Switches (Save : out Config_Switches_Type) is
   begin
      Save.Ada_83                     := Ada_83;
      Save.Dynamic_Elaboration_Checks := Dynamic_Elaboration_Checks;
      Save.Extensions_Allowed         := Extensions_Allowed;
      Save.External_Name_Exp_Casing   := External_Name_Exp_Casing;
      Save.External_Name_Imp_Casing   := External_Name_Imp_Casing;
      Save.Polling_Required           := Polling_Required;
      Save.Use_VADS_Size              := Use_VADS_Size;
   end Save_Opt_Config_Switches;

   -----------------------------
   -- Set_Opt_Config_Switches --
   -----------------------------

   procedure Set_Opt_Config_Switches (Internal_Unit : Boolean) is
   begin
      if Internal_Unit then
         Ada_83                     := False;
         Ada_95                     := True;
         Dynamic_Elaboration_Checks := False;
         Extensions_Allowed         := True;
         External_Name_Exp_Casing   := As_Is;
         External_Name_Imp_Casing   := Lowercase;
         Use_VADS_Size              := False;

      else
         Ada_83                     := Ada_83_Config;
         Ada_95                     := not Ada_83_Config;
         Dynamic_Elaboration_Checks := Dynamic_Elaboration_Checks_Config;
         Extensions_Allowed         := Extensions_Allowed_Config;
         External_Name_Exp_Casing   := External_Name_Exp_Casing_Config;
         External_Name_Imp_Casing   := External_Name_Imp_Casing_Config;
         Use_VADS_Size              := Use_VADS_Size_Config;
      end if;

      Polling_Required := Polling_Required_Config;
   end Set_Opt_Config_Switches;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
      Tree_Version_String_Len : Nat;

   begin
      Tree_Read_Bool (Brief_Output);
      Tree_Read_Bool (GNAT_Mode);
      Tree_Read_Char (Identifier_Character_Set);
      Tree_Read_Int  (Maximum_File_Name_Length);
      Tree_Read_Data (Suppress_Options'Address,
                      Suppress_Record'Object_Size / Storage_Unit);
      Tree_Read_Bool (Verbose_Mode);
      Tree_Read_Data (Warning_Mode'Address,
                      Warning_Mode_Type'Object_Size / Storage_Unit);
      Tree_Read_Bool (Ada_83_Config);
      Tree_Read_Bool (All_Errors_Mode);
      Tree_Read_Bool (Assertions_Enabled);
      Tree_Read_Bool (Full_List);

      --  Read and check version string

      Tree_Read_Int (Tree_Version_String_Len);

      if Tree_Version_String_Len = Tree_Version_String'Length then
         Tree_Read_Data
           (Tree_Version_String'Address, Tree_Version_String'Length);
      end if;

      if Tree_Version_String_Len /= Tree_Version_String'Length
        or else Tree_Version_String /= Gnat_Version_String
      then
         Raise_Exception
           (Program_Error'Identity, "Inconsistent versions of GNAT and ASIS");
      end if;

      Tree_Read_Data (Distribution_Stub_Mode'Address,
                      Distribution_Stub_Mode_Type'Object_Size / Storage_Unit);
      Tree_Read_Bool (Immediate_Errors);
      Tree_Read_Bool (Inline_Active);
      Tree_Read_Bool (Inline_Processing_Required);
      Tree_Read_Bool (List_Units);
      Tree_Read_Bool (No_Run_Time);
      Tree_Read_Data (Operating_Mode'Address,
                      Operating_Mode_Type'Object_Size / Storage_Unit);
      Tree_Read_Bool (Software_Overflow_Checking);
      Tree_Read_Bool (Try_Semantics);
      Tree_Read_Data (Wide_Character_Encoding_Method'Address,
                      WC_Encoding_Method'Object_Size / Storage_Unit);
      Tree_Read_Bool (Upper_Half_Encoding);
      Tree_Read_Bool (Force_ALI_Tree_File);
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Tree_Write_Bool (Brief_Output);
      Tree_Write_Bool (GNAT_Mode);
      Tree_Write_Char (Identifier_Character_Set);
      Tree_Write_Int  (Maximum_File_Name_Length);
      Tree_Write_Data (Suppress_Options'Address,
                       Suppress_Record'Object_Size / Storage_Unit);
      Tree_Write_Bool (Verbose_Mode);
      Tree_Write_Data (Warning_Mode'Address,
                       Warning_Mode_Type'Object_Size / Storage_Unit);
      Tree_Write_Bool (Ada_83_Config);
      Tree_Write_Bool (All_Errors_Mode);
      Tree_Write_Bool (Assertions_Enabled);
      Tree_Write_Bool (Full_List);
      Tree_Write_Int  (Int (Gnat_Version_String'Length));
      Tree_Write_Data (Gnat_Version_String'Address,
                       Gnat_Version_String'Length);
      Tree_Write_Data (Distribution_Stub_Mode'Address,
                       Distribution_Stub_Mode_Type'Object_Size / Storage_Unit);
      Tree_Write_Bool (Immediate_Errors);
      Tree_Write_Bool (Inline_Active);
      Tree_Write_Bool (Inline_Processing_Required);
      Tree_Write_Bool (List_Units);
      Tree_Write_Bool (No_Run_Time);
      Tree_Write_Data (Operating_Mode'Address,
                       Operating_Mode_Type'Object_Size / Storage_Unit);
      Tree_Write_Bool (Software_Overflow_Checking);
      Tree_Write_Bool (Try_Semantics);
      Tree_Write_Data (Wide_Character_Encoding_Method'Address,
                       WC_Encoding_Method'Object_Size / Storage_Unit);
      Tree_Write_Bool (Upper_Half_Encoding);
      Tree_Write_Bool (Force_ALI_Tree_File);
   end Tree_Write;

end Opt;
