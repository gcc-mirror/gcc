------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ T A R G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  Version for use with GCC

package body Get_Targ is

   --  Functions returning individual run-time values. For the standard (GCC)
   --  back end, these come from C interface functions (one for each value).

   -----------------------
   -- Get_Bits_Per_Unit --
   -----------------------

   function Get_Bits_Per_Unit return Pos is
      function C_Get_Bits_Per_Unit return Pos;
      pragma Import (C, C_Get_Bits_Per_Unit,
                        "get_target_bits_per_unit");
   begin
      return C_Get_Bits_Per_Unit;
   end Get_Bits_Per_Unit;

   -----------------------
   -- Get_Bits_Per_Word --
   -----------------------

   function Get_Bits_Per_Word return Pos is
      function C_Get_Bits_Per_Word return Pos;
      pragma Import (C, C_Get_Bits_Per_Word,
                        "get_target_bits_per_word");
   begin
      return C_Get_Bits_Per_Word;
   end Get_Bits_Per_Word;

   -------------------
   -- Get_Char_Size --
   -------------------

   function Get_Char_Size return Pos is
      function C_Get_Char_Size return Pos;
      pragma Import (C, C_Get_Char_Size,
                        "get_target_char_size");
   begin
      return C_Get_Char_Size;
   end Get_Char_Size;

   ----------------------
   -- Get_Wchar_T_Size --
   ----------------------

   function Get_Wchar_T_Size return Pos is
      function C_Get_Wchar_T_Size return Pos;
      pragma Import (C, C_Get_Wchar_T_Size,
                        "get_target_wchar_t_size");
   begin
      return C_Get_Wchar_T_Size;
   end Get_Wchar_T_Size;

   --------------------
   -- Get_Short_Size --
   --------------------

   function Get_Short_Size return Pos is
      function C_Get_Short_Size return Pos;
      pragma Import (C, C_Get_Short_Size,
                        "get_target_short_size");
   begin
      return C_Get_Short_Size;
   end Get_Short_Size;

   ------------------
   -- Get_Int_Size --
   ------------------

   function Get_Int_Size return Pos is
      function C_Get_Int_Size return Pos;
      pragma Import (C, C_Get_Int_Size,
                        "get_target_int_size");
   begin
      return C_Get_Int_Size;
   end Get_Int_Size;

   -------------------
   -- Get_Long_Size --
   -------------------

   function Get_Long_Size return Pos is
      function C_Get_Long_Size return Pos;
      pragma Import (C, C_Get_Long_Size,
                        "get_target_long_size");
   begin
      return C_Get_Long_Size;
   end Get_Long_Size;

   ------------------------
   -- Get_Long_Long_Size --
   ------------------------

   function Get_Long_Long_Size return Pos is
      function C_Get_Long_Long_Size return Pos;
      pragma Import (C, C_Get_Long_Long_Size,
                        "get_target_long_long_size");
   begin
      return C_Get_Long_Long_Size;
   end Get_Long_Long_Size;

   -----------------------------
   -- Get_Long_Long_Long_Size --
   -----------------------------

   function Get_Long_Long_Long_Size return Pos is
      function C_Get_Long_Long_Long_Size return Pos;
      pragma Import (C, C_Get_Long_Long_Long_Size,
                        "get_target_long_long_long_size");
   begin
      return C_Get_Long_Long_Long_Size;
   end Get_Long_Long_Long_Size;

   ----------------------
   -- Get_Pointer_Size --
   ----------------------

   function Get_Pointer_Size return Pos is
      function C_Get_Pointer_Size return Pos;
      pragma Import (C, C_Get_Pointer_Size,
                        "get_target_pointer_size");
   begin
      return C_Get_Pointer_Size;
   end Get_Pointer_Size;

   ---------------------------
   -- Get_Maximum_Alignment --
   ---------------------------

   function Get_Maximum_Alignment return Pos is
      function C_Get_Maximum_Alignment return Pos;
      pragma Import (C, C_Get_Maximum_Alignment,
                        "get_target_maximum_alignment");
   begin
      return C_Get_Maximum_Alignment;
   end Get_Maximum_Alignment;

   ------------------------
   -- Get_Float_Words_BE --
   ------------------------

   function Get_Float_Words_BE return Nat is
      function C_Get_Float_Words_BE return Nat;
      pragma Import (C, C_Get_Float_Words_BE,
                        "get_target_float_words_be");
   begin
      return C_Get_Float_Words_BE;
   end Get_Float_Words_BE;

   ------------------
   -- Get_Words_BE --
   ------------------

   function Get_Words_BE return Nat is
      function C_Get_Words_BE return Nat;
      pragma Import (C, C_Get_Words_BE,
                        "get_target_words_be");
   begin
      return C_Get_Words_BE;
   end Get_Words_BE;

   ------------------
   -- Get_Bytes_BE --
   ------------------

   function Get_Bytes_BE return Nat is
      function C_Get_Bytes_BE return Nat;
      pragma Import (C, C_Get_Bytes_BE,
                        "get_target_bytes_be");
   begin
      return C_Get_Bytes_BE;
   end Get_Bytes_BE;

   -----------------
   -- Get_Bits_BE --
   -----------------

   function Get_Bits_BE return Nat is
      function C_Get_Bits_BE return Nat;
      pragma Import (C, C_Get_Bits_BE,
                        "get_target_bits_be");
   begin
      return C_Get_Bits_BE;
   end Get_Bits_BE;

   ---------------------
   -- Get_Short_Enums --
   ---------------------

   function Get_Short_Enums return Int is
      flag_short_enums : Int;
      pragma Import (C, flag_short_enums);
   begin
      return flag_short_enums;
   end Get_Short_Enums;

   --------------------------
   -- Get_Strict_Alignment --
   --------------------------

   function Get_Strict_Alignment return Nat is
      function C_Get_Strict_Alignment return Nat;
      pragma Import (C, C_Get_Strict_Alignment,
                        "get_target_strict_alignment");
   begin
      return C_Get_Strict_Alignment;
   end Get_Strict_Alignment;

   ------------------------------------
   -- Get_System_Allocator_Alignment --
   ------------------------------------

   function Get_System_Allocator_Alignment return Nat is
      function C_Get_System_Allocator_Alignment return Nat;
      pragma Import (C, C_Get_System_Allocator_Alignment,
                        "get_target_system_allocator_alignment");
   begin
      return C_Get_System_Allocator_Alignment;
   end Get_System_Allocator_Alignment;

   --------------------------------
   -- Get_Double_Float_Alignment --
   --------------------------------

   function Get_Double_Float_Alignment return Nat is
      function C_Get_Double_Float_Alignment return Nat;
      pragma Import (C, C_Get_Double_Float_Alignment,
                        "get_target_double_float_alignment");
   begin
      return C_Get_Double_Float_Alignment;
   end Get_Double_Float_Alignment;

   ---------------------------------
   -- Get_Double_Scalar_Alignment --
   ---------------------------------

   function Get_Double_Scalar_Alignment return Nat is
      function C_Get_Double_Scalar_Alignment return Nat;
      pragma Import (C, C_Get_Double_Scalar_Alignment,
                        "get_target_double_scalar_alignment");
   begin
      return C_Get_Double_Scalar_Alignment;
   end Get_Double_Scalar_Alignment;

   ------------------------------
   -- Get_Back_End_Config_File --
   ------------------------------

   function Get_Back_End_Config_File return String_Ptr is
   begin
      return null;
   end Get_Back_End_Config_File;

   -----------------------------
   -- Get_Max_Unaligned_Field --
   -----------------------------

   function Get_Max_Unaligned_Field return Pos is
   begin
      return 64;  -- Can be different on some targets
   end Get_Max_Unaligned_Field;

   -----------------------------
   -- Register_Back_End_Types --
   -----------------------------

   procedure Register_Back_End_Types (Call_Back : Register_Type_Proc) is
      procedure Enumerate_Modes (Call_Back : Register_Type_Proc);
      pragma Import (C, Enumerate_Modes, "enumerate_modes");
   begin
      Enumerate_Modes (Call_Back);
   end Register_Back_End_Types;

end Get_Targ;
