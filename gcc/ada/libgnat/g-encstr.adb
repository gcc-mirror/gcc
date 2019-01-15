------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . E N C O D E _ S T R I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2007-2019, Free Software Foundation, Inc.        --
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

with Interfaces; use Interfaces;

with System.WCh_Con; use System.WCh_Con;
with System.WCh_Cnv; use System.WCh_Cnv;

package body GNAT.Encode_String is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Bad;
   pragma No_Return (Bad);
   --  Raise error for bad character code

   procedure Past_End;
   pragma No_Return (Past_End);
   --  Raise error for off end of string

   ---------
   -- Bad --
   ---------

   procedure Bad is
   begin
      raise Constraint_Error with
        "character cannot be encoded with given Encoding_Method";
   end Bad;

   ------------------------
   -- Encode_Wide_String --
   ------------------------

   function Encode_Wide_String (S : Wide_String) return String is
      Long : constant Natural := WC_Longest_Sequences (Encoding_Method);
      Result : String (1 .. S'Length * Long);
      Length : Natural;
   begin
      Encode_Wide_String (S, Result, Length);
      return Result (1 .. Length);
   end Encode_Wide_String;

   procedure Encode_Wide_String
     (S      : Wide_String;
      Result : out String;
      Length : out Natural)
   is
      Ptr : Natural;

   begin
      Ptr := S'First;
      for J in S'Range loop
         Encode_Wide_Character (S (J), Result, Ptr);
      end loop;

      Length := Ptr - S'First;
   end Encode_Wide_String;

   -----------------------------
   -- Encode_Wide_Wide_String --
   -----------------------------

   function Encode_Wide_Wide_String (S : Wide_Wide_String) return String is
      Long : constant Natural := WC_Longest_Sequences (Encoding_Method);
      Result : String (1 .. S'Length * Long);
      Length : Natural;
   begin
      Encode_Wide_Wide_String (S, Result, Length);
      return Result (1 .. Length);
   end Encode_Wide_Wide_String;

   procedure Encode_Wide_Wide_String
     (S      : Wide_Wide_String;
      Result : out String;
      Length : out Natural)
   is
      Ptr : Natural;

   begin
      Ptr := S'First;
      for J in S'Range loop
         Encode_Wide_Wide_Character (S (J), Result, Ptr);
      end loop;

      Length := Ptr - S'First;
   end Encode_Wide_Wide_String;

   ---------------------------
   -- Encode_Wide_Character --
   ---------------------------

   procedure Encode_Wide_Character
     (Char   : Wide_Character;
      Result : in out String;
      Ptr    : in out Natural)
   is
   begin
      Encode_Wide_Wide_Character
        (Wide_Wide_Character'Val (Wide_Character'Pos (Char)), Result, Ptr);

   exception
      when Constraint_Error =>
         Bad;
   end Encode_Wide_Character;

   --------------------------------
   -- Encode_Wide_Wide_Character --
   --------------------------------

   procedure Encode_Wide_Wide_Character
     (Char   : Wide_Wide_Character;
      Result : in out String;
      Ptr    : in out Natural)
   is
      U : Unsigned_32;

      procedure Out_Char (C : Character);
      pragma Inline (Out_Char);
      --  Procedure to store one character for instantiation below

      --------------
      -- Out_Char --
      --------------

      procedure Out_Char (C : Character) is
      begin
         if Ptr > Result'Last then
            Past_End;
         else
            Result (Ptr) := C;
            Ptr := Ptr + 1;
         end if;
      end Out_Char;

   --  Start of processing for Encode_Wide_Wide_Character;

   begin
      --  Efficient code for UTF-8 case

      if Encoding_Method = WCEM_UTF8 then

         --  Note: for details of UTF8 encoding see RFC 3629

         U := Unsigned_32 (Wide_Wide_Character'Pos (Char));

         --  16#00_0000#-16#00_007F#: 0xxxxxxx

         if U <= 16#00_007F# then
            Out_Char (Character'Val (U));

         --  16#00_0080#-16#00_07FF#: 110xxxxx 10xxxxxx

         elsif U <= 16#00_07FF# then
            Out_Char (Character'Val (2#11000000# or Shift_Right (U, 6)));
            Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

         --  16#00_0800#-16#00_FFFF#: 1110xxxx 10xxxxxx 10xxxxxx

         elsif U <= 16#00_FFFF# then
            Out_Char (Character'Val (2#11100000# or Shift_Right (U, 12)));
            Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                          and 2#00111111#)));
            Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

         --  16#01_0000#-16#10_FFFF#: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

         elsif U <= 16#10_FFFF# then
            Out_Char (Character'Val (2#11110000# or Shift_Right (U, 18)));
            Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 12)
                                                          and 2#00111111#)));
            Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                       and 2#00111111#)));
            Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

         --  16#0020_0000#-16#03FF_FFFF#: 111110xx 10xxxxxx 10xxxxxx
         --                               10xxxxxx 10xxxxxx

         elsif U <= 16#03FF_FFFF# then
            Out_Char (Character'Val (2#11111000# or Shift_Right (U, 24)));
            Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 18)
                                                       and 2#00111111#)));
            Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 12)
                                                       and 2#00111111#)));
            Out_Char (Character'Val (2#10000000# or (Shift_Right (U, 6)
                                                       and 2#00111111#)));
            Out_Char (Character'Val (2#10000000# or (U and 2#00111111#)));

         --  All other cases are invalid character codes, not this includes:

         --  16#0400_0000#-16#7FFF_FFFF#: 1111110x 10xxxxxx 10xxxxxx
         --                               10xxxxxx 10xxxxxx 10xxxxxx

         --  since Wide_Wide_Character values cannot exceed 16#3F_FFFF#

         else
            Bad;
         end if;

      --  All encoding methods other than UTF-8

      else
         Non_UTF8 : declare
            procedure UTF_32_To_String is
              new UTF_32_To_Char_Sequence (Out_Char);
            --  Instantiate conversion procedure with above Out_Char routine

         begin
            UTF_32_To_String
              (UTF_32_Code (Wide_Wide_Character'Pos (Char)), Encoding_Method);

         exception
            when Constraint_Error =>
               Bad;
         end Non_UTF8;
      end if;
   end Encode_Wide_Wide_Character;

   --------------
   -- Past_End --
   --------------

   procedure Past_End is
   begin
      raise Constraint_Error with "past end of string";
   end Past_End;

end GNAT.Encode_String;
