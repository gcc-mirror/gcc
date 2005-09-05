------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             W I D E C H A R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

--  Note: this package uses the generic subprograms in System.Wch_Cnv, which
--  completely encapsulate the set of wide character encoding methods, so no
--  modifications are required when adding new encoding methods.

with Opt; use Opt;

with System.WCh_Cnv; use System.WCh_Cnv;
with System.WCh_Con; use System.WCh_Con;

package body Widechar is

   ---------------------------
   -- Is_Start_Of_Wide_Char --
   ---------------------------

   function Is_Start_Of_Wide_Char
     (S : Source_Buffer_Ptr;
      P : Source_Ptr) return Boolean
   is
   begin
      case Wide_Character_Encoding_Method is
         when WCEM_Hex =>
            return S (P) = ASCII.ESC;

         when WCEM_Upper     |
              WCEM_Shift_JIS |
              WCEM_EUC       |
              WCEM_UTF8      =>
            return S (P) >= Character'Val (16#80#);

         when WCEM_Brackets =>
            return P <= S'Last - 2
              and then S (P) = '['
              and then S (P + 1) = '"'
              and then S (P + 2) /= '"';
      end case;
   end Is_Start_Of_Wide_Char;

   -----------------
   -- Length_Wide --
   -----------------

   function Length_Wide return Nat is
   begin
      return WC_Longest_Sequence;
   end Length_Wide;

   ---------------
   -- Scan_Wide --
   ---------------

   procedure Scan_Wide
     (S   : Source_Buffer_Ptr;
      P   : in out Source_Ptr;
      C   : out Char_Code;
      Err : out Boolean)
   is
      P_Init : constant Source_Ptr := P;

      function In_Char return Character;
      --  Function to obtain characters of wide character escape sequence

      -------------
      -- In_Char --
      -------------

      function In_Char return Character is
      begin
         P := P + 1;
         return S (P - 1);
      end In_Char;

      function WC_In is new Char_Sequence_To_UTF_32 (In_Char);

   --  Start of processingf for Scan_Wide

   begin
      C := Char_Code (WC_In (In_Char, Wide_Character_Encoding_Method));
      Err := False;
      Wide_Char_Byte_Count := Wide_Char_Byte_Count + Nat (P - P_Init - 1);

   exception
      when Constraint_Error =>
         C := Char_Code (0);
         P := P - 1;
         Err := True;
   end Scan_Wide;

   --------------
   -- Set_Wide --
   --------------

   procedure Set_Wide
     (C : Char_Code;
      S : in out String;
      P : in out Natural)
   is
      procedure Out_Char (C : Character);
      --  Procedure to store one character of wide character sequence

      --------------
      -- Out_Char --
      --------------

      procedure Out_Char (C : Character) is
      begin
         P := P + 1;
         S (P) := C;
      end Out_Char;

      procedure WC_Out is new UTF_32_To_Char_Sequence (Out_Char);

   --  Start of processing for Set_Wide

   begin
      WC_Out (UTF_32_Code (C), Wide_Character_Encoding_Method);
   end Set_Wide;

   ---------------
   -- Skip_Wide --
   ---------------

   procedure Skip_Wide (S : String; P : in out Natural) is
      P_Init : constant Natural := P;

      function Skip_Char return Character;
      --  Function to skip one character of wide character escape sequence

      ---------------
      -- Skip_Char --
      ---------------

      function Skip_Char return Character is
      begin
         P := P + 1;
         return S (P - 1);
      end Skip_Char;

      function WC_Skip is new Char_Sequence_To_UTF_32 (Skip_Char);

      Discard : UTF_32_Code;
      pragma Warnings (Off, Discard);

   --  Start of processing for Skip_Wide

   begin
      Discard := WC_Skip (Skip_Char, Wide_Character_Encoding_Method);
      Wide_Char_Byte_Count := Wide_Char_Byte_Count + Nat (P - P_Init - 1);
   end Skip_Wide;

   ---------------
   -- Skip_Wide --
   ---------------

   procedure Skip_Wide (S : Source_Buffer_Ptr; P : in out Source_Ptr) is
      P_Init : constant Source_Ptr := P;

      function Skip_Char return Character;
      --  Function to skip one character of wide character escape sequence

      ---------------
      -- Skip_Char --
      ---------------

      function Skip_Char return Character is
      begin
         P := P + 1;
         return S (P - 1);
      end Skip_Char;

      function WC_Skip is new Char_Sequence_To_UTF_32 (Skip_Char);

      Discard : UTF_32_Code;
      pragma Warnings (Off, Discard);

   --  Start of processing for Skip_Wide

   begin
      Discard := WC_Skip (Skip_Char, Wide_Character_Encoding_Method);
      Wide_Char_Byte_Count := Wide_Char_Byte_Count + Nat (P - P_Init - 1);
   end Skip_Wide;

end Widechar;
