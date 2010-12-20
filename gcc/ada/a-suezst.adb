------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                ADA.STRINGS.UTF_ENCODING.WIDE_WIDE_STRINGS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2010, Free Software Foundation, Inc.           --
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

package body Ada.Strings.UTF_Encoding.Wide_Wide_Strings is
   use Interfaces;

   ------------
   -- Decode --
   ------------

   --  Decode UTF-8/UTF-16BE/UTF-16LE input to Wide_Wide_String

   function Decode
     (Item         : UTF_String;
      Input_Scheme : Encoding_Scheme) return Wide_Wide_String
   is
   begin
      if Input_Scheme = UTF_8 then
         return Decode (Item);
      else
         return Decode (To_UTF_16 (Item, Input_Scheme));
      end if;
   end Decode;

   --  Decode UTF-8 input to Wide_Wide_String

   function Decode (Item : UTF_8_String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      --  Result string (worst case is same length as input)

      Len : Natural := 0;
      --  Length of result stored so far

      Iptr : Natural;
      --  Input string pointer

      C : Unsigned_8;
      R : Unsigned_32;

      procedure Get_Continuation;
      --  Reads a continuation byte of the form 10xxxxxx, shifts R left
      --  by 6 bits, and or's in the xxxxxx to the low order 6 bits. On
      --  return Ptr is incremented. Raises exception if continuation
      --  byte does not exist or is invalid.

      ----------------------
      -- Get_Continuation --
      ----------------------

      procedure Get_Continuation is
      begin
         if Iptr > Item'Last then
            Raise_Encoding_Error (Iptr - 1);

         else
            C := To_Unsigned_8 (Item (Iptr));
            Iptr := Iptr + 1;

            if C not in 2#10_000000# .. 2#10_111111# then
               Raise_Encoding_Error (Iptr - 1);
            else
               R := Shift_Left (R, 6) or Unsigned_32 (C and 2#00_111111#);
            end if;
         end if;
      end Get_Continuation;

   --  Start of processing for Decode

   begin
      Iptr := Item'First;

      --  Skip BOM at start

      if Item'Length >= 3
        and then Item (Iptr .. Iptr + 2) = BOM_8
      then
         Iptr := Iptr + 3;

      --  Error if bad BOM

      elsif Item'Length >= 2
        and then (Item (Iptr .. Iptr + 1) = BOM_16BE
                    or else
                  Item (Iptr .. Iptr + 1) = BOM_16LE)
      then
         Raise_Encoding_Error (Iptr);
      end if;

      --  Loop through input characters

      while Iptr <= Item'Last loop
         C := To_Unsigned_8 (Item (Iptr));
         Iptr := Iptr + 1;

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            R := Unsigned_32 (C);

         --  No initial code can be of the form 10xxxxxx. Such codes are used
         --  only for continuations.

         elsif C <= 2#10_111111# then
            Raise_Encoding_Error (Iptr - 1);

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 2#110_11111# then
            R := Unsigned_32 (C and 2#000_11111#);
            Get_Continuation;

         --  Codes in the range 16#800# - 16#FFFF# are represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         elsif C <= 2#1110_1111# then
            R := Unsigned_32 (C and 2#0000_1111#);
            Get_Continuation;
            Get_Continuation;

         --  Codes in the range 16#10000# - 16#10FFFF# are represented as
         --    11110zzz 10zzyyyy 10yyyyxx 10xxxxxx

         elsif C <= 2#11110_111# then
            R := Unsigned_32 (C and 2#00000_111#);
            Get_Continuation;
            Get_Continuation;
            Get_Continuation;

         --  Any other code is an error

         else
            Raise_Encoding_Error (Iptr - 1);
         end if;

         Len := Len + 1;
         Result (Len) := Wide_Wide_Character'Val (R);
      end loop;

      return Result (1 .. Len);
   end Decode;

   --  Decode UTF-16 input to Wide_Wide_String

   function Decode (Item : UTF_16_Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      --  Result cannot be longer than the input string

      Len : Natural := 0;
      --  Length of result

      Iptr : Natural;
      --  Pointer to next element in Item

      C : Unsigned_16;
      R : Unsigned_32;

   begin
      --  Skip UTF-16 BOM at start

      Iptr := Item'First;

      if Iptr <= Item'Last and then Item (Iptr) = BOM_16 (1) then
         Iptr := Iptr + 1;
      end if;

      --  Loop through input characters

      while Iptr <= Item'Last loop
         C := To_Unsigned_16 (Item (Iptr));
         Iptr := Iptr + 1;

         --  Codes in the range 16#0000#..16#D7FF# or 16#E000#..16#FFFD#
         --  represent their own value.

         if C <= 16#D7FF# or else C in 16#E000# .. 16#FFFD# then
            Len := Len + 1;
            Result (Len) := Wide_Wide_Character'Val (C);

         --  Codes in the range 16#D800#..16#DBFF# represent the first of the
         --  two surrogates used to encode the range 16#01_000#..16#10_FFFF".
         --  The first surrogate provides 10 high order bits of the result.

         elsif C <= 16#DBFF# then
            R := Shift_Left ((Unsigned_32 (C) - 16#D800#), 10);

            --  Error if at end of string

            if Iptr > Item'Last then
               Raise_Encoding_Error (Iptr - 1);

            --  Otherwise next character must be valid low order surrogate
            --  which provides the low 10 order bits of the result.

            else
               C := To_Unsigned_16 (Item (Iptr));
               Iptr := Iptr + 1;

               if C not in 16#DC00# .. 16#DFFF# then
                  Raise_Encoding_Error (Iptr - 1);

               else
                  R := R or (Unsigned_32 (C) mod 2 ** 10);

               --  The final adjustment is to add 16#01_0000 to get the
               --  result back in the required 21 bit range.

                  R := R + 16#01_0000#;
                  Len := Len + 1;
                  Result (Len) := Wide_Wide_Character'Val (R);
               end if;
            end if;

         --  Remaining codes are invalid

         else
            Raise_Encoding_Error (Iptr - 1);
         end if;
      end loop;

      return Result (1 .. Len);
   end Decode;

   ------------
   -- Encode --
   ------------

   --  Encode Wide_Wide_String in UTF-8, UTF-16BE or UTF-16LE

   function Encode
     (Item          : Wide_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean  := False) return UTF_String
   is
   begin
      if Output_Scheme = UTF_8 then
         return Encode (Item, Output_BOM);
      else
         return From_UTF_16 (Encode (Item), Output_Scheme, Output_BOM);
      end if;
   end Encode;

   --  Encode Wide_Wide_String in UTF-8

   function Encode
     (Item       : Wide_Wide_String;
      Output_BOM : Boolean  := False) return UTF_8_String
   is
      Result : String (1 .. 4 * Item'Length + 3);
      --  Worst case is four bytes per input byte + space for BOM

      Len  : Natural;
      --  Number of output codes stored in Result

      C : Unsigned_32;
      --  Single input character

      procedure Store (C : Unsigned_32);
      pragma Inline (Store);
      --  Store one output code (input is in range 0 .. 255)

      -----------
      -- Store --
      -----------

      procedure Store (C : Unsigned_32) is
      begin
         Len := Len + 1;
         Result (Len) := Character'Val (C);
      end Store;

   --  Start of processing for Encode

   begin
      --  Output BOM if required

      if Output_BOM then
         Result (1 .. 3) := BOM_8;
         Len := 3;
      else
         Len := 0;
      end if;

      --  Loop through characters of input

      for Iptr in Item'Range loop
         C := To_Unsigned_32 (Item (Iptr));

         --  Codes in the range 16#00#..16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            Store (C);

         --  Codes in the range 16#80#..16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 16#7FF# then
            Store (2#110_00000# or Shift_Right (C, 6));
            Store (2#10_000000# or (C and 2#00_111111#));

         --  Codes in the range 16#800#..16#D7FF# or 16#E000#..16#FFFD# are
         --  represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         elsif C <= 16#D7FF# or else C in 16#E000# .. 16#FFFD# then
            Store (2#1110_0000# or Shift_Right (C, 12));
            Store (2#10_000000# or
                     Shift_Right (C and 2#111111_000000#, 6));
            Store (2#10_000000# or (C and 2#00_111111#));

         --  Codes in the range 16#10000# - 16#10FFFF# are represented as
         --    11110zzz 10zzyyyy 10yyyyxx 10xxxxxx

         elsif C in 16#1_0000# .. 16#10_FFFF# then
            Store (2#11110_000# or
                     Shift_Right (C, 18));
            Store (2#10_000000# or
                     Shift_Right (C and 2#111111_000000_000000#, 12));
            Store (2#10_000000# or
                     Shift_Right (C and 2#111111_000000#, 6));
            Store (2#10_000000# or
                     (C and 2#00_111111#));

         --  All other codes are invalid

         else
            Raise_Encoding_Error (Iptr);
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode;

   --  Encode Wide_Wide_String in UTF-16

   function Encode
     (Item       : Wide_Wide_String;
      Output_BOM : Boolean  := False) return UTF_16_Wide_String
   is
      Result : UTF_16_Wide_String (1 .. 2 * Item'Length + 1);
      --  Worst case is each input character generates two output characters
      --  plus one for possible BOM.

      Len : Integer;
      --  Length of output string

      C : Unsigned_32;

   begin
      --  Output BOM if needed

      if Output_BOM then
         Result (1) := BOM_16 (1);
         Len := 1;
      else
         Len := 0;
      end if;

      --  Loop through input characters encoding them

      for Iptr in Item'Range loop
         C := To_Unsigned_32 (Item (Iptr));

         --  Codes in the range 16#00_0000#..16#00_D7FF# or 16#E000#..16#FFFD#
         --  are output unchanged

         if C <= 16#00_D7FF# or else C in 16#E000# .. 16#FFFD# then
            Len := Len + 1;
            Result (Len) := Wide_Character'Val (C);

         --  Codes in the range 16#01_0000#..16#10_FFFF# are output using two
         --  surrogate characters. First 16#1_0000# is subtracted from the code
         --  point to give a 20-bit value. This is then split into two separate
         --  10-bit values each of which is represented as a surrogate with the
         --  most significant half placed in the first surrogate. The ranges of
         --  values used for the two surrogates are 16#D800#-16#DBFF# for the
         --  first, most significant surrogate and 16#DC00#-16#DFFF# for the
         --  second, least significant surrogate.

         elsif C in 16#1_0000# ..  16#10_FFFF# then
            C := C - 16#1_0000#;

            Len := Len + 1;
            Result (Len) := Wide_Character'Val (16#D800# + C / 2 ** 10);

            Len := Len + 1;
            Result (Len) := Wide_Character'Val (16#DC00# + C mod 2 ** 10);

         --  All other codes are invalid

         else
            Raise_Encoding_Error (Iptr);
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode;

end Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
