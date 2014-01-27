------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.STRINGS.UTF_ENCODING.STRINGS                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2013, Free Software Foundation, Inc.         --
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

package body Ada.Strings.UTF_Encoding.Strings is
   use Interfaces;

   ------------
   -- Decode --
   ------------

   --  Decode UTF-8/UTF-16BE/UTF-16LE input to String

   function Decode
     (Item         : UTF_String;
      Input_Scheme : Encoding_Scheme) return String
   is
   begin
      if Input_Scheme = UTF_8 then
         return Decode (Item);
      else
         return Decode (To_UTF_16 (Item, Input_Scheme));
      end if;
   end Decode;

   --  Decode UTF-8 input to String

   function Decode (Item : UTF_8_String) return String is
      Result : String (1 .. Item'Length);
      --  Result string (worst case is same length as input)

      Len : Natural := 0;
      --  Length of result stored so far

      Iptr : Natural;
      --  Input Item pointer

      C : Unsigned_8;
      R : Unsigned_16;

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
               R := Shift_Left (R, 6) or Unsigned_16 (C and 2#00_111111#);
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

      while Iptr <= Item'Last loop
         C := To_Unsigned_8 (Item (Iptr));
         Iptr := Iptr + 1;

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            R := Unsigned_16 (C);

         --  No initial code can be of the form 10xxxxxx. Such codes are used
         --  only for continuations.

         elsif C <= 2#10_111111# then
            Raise_Encoding_Error (Iptr - 1);

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         elsif C <= 2#110_11111# then
            R := Unsigned_16 (C and 2#000_11111#);
            Get_Continuation;

         --  Codes in the range 16#800# - 16#FFFF# are represented as
         --    1110yyyy 10yyyyxx 10xxxxxx

         --  Such codes are out of range for type Character

         --  Codes in the range 16#10000# - 16#10FFFF# are represented as
         --    11110zzz 10zzyyyy 10yyyyxx 10xxxxxx

         --  Such codes are out of range for Wide_String output

         --  Thus all remaining cases raise Encoding_Error

         else
            Raise_Encoding_Error (Iptr - 1);
         end if;

         Len := Len + 1;
         Result (Len) := Character'Val (R);
      end loop;

      return Result (1 .. Len);

   exception
      --  'Val may have been out of range

      when others =>
         Raise_Encoding_Error (Iptr - 1);
   end Decode;

   --  Decode UTF-16 input to String

   function Decode (Item : UTF_16_Wide_String) return String is
      Result : String (1 .. Item'Length);
      --  Result is same length as input (possibly minus 1 if BOM present)

      Len : Natural := 0;
      --  Length of result

      Iptr : Natural;
      --  Index of next Item element

      C : Unsigned_16;

   begin
      --  Skip UTF-16 BOM at start

      Iptr := Item'First;

      if Item'Length > 0 and then Item (Iptr) = BOM_16 (1) then
         Iptr := Iptr + 1;
      end if;

      --  Loop through input characters

      while Iptr <= Item'Last loop
         C := To_Unsigned_16 (Item (Iptr));
         Iptr := Iptr + 1;

         --  Codes in the range 16#0000#..16#00FF# represent their own value

         if C <= 16#00FF# then
            Len := Len + 1;
            Result (Len) := Character'Val (C);

         --  All other codes are invalid, either they are invalid UTF-16
         --  encoding sequences, or they represent values that are out of
         --  range for type Character.

         else
            Raise_Encoding_Error (Iptr - 1);
         end if;
      end loop;

      return Result (1 .. Len);
   end Decode;

   ------------
   -- Encode --
   ------------

   --  Encode String in UTF-8, UTF-16BE or UTF-16LE

   function Encode
     (Item          : String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean  := False) return UTF_String
   is
   begin
      --  Case of UTF_8

      if Output_Scheme = UTF_8 then
         return Encode (Item, Output_BOM);

      --  Case of UTF_16LE or UTF_16BE, use UTF-16 intermediary

      else
         return From_UTF_16 (UTF_16_Wide_String'(Encode (Item)),
                             Output_Scheme, Output_BOM);
      end if;
   end Encode;

   --  Encode String in UTF-8

   function Encode
     (Item       : String;
      Output_BOM : Boolean  := False) return UTF_8_String
   is
      Result : UTF_8_String (1 .. 3 * Item'Length + 3);
      --  Worst case is three bytes per input byte + space for BOM

      Len : Natural;
      --  Number of output codes stored in Result

      C : Unsigned_8;
      --  Single input character

      procedure Store (C : Unsigned_8);
      pragma Inline (Store);
      --  Store one output code, C is in the range 0 .. 255

      -----------
      -- Store --
      -----------

      procedure Store (C : Unsigned_8) is
      begin
         Len := Len + 1;
         Result (Len) := Character'Val (C);
      end Store;

   --  Start of processing for UTF8_Encode

   begin
      --  Output BOM if required

      if Output_BOM then
         Result (1 .. 3) := BOM_8;
         Len := 3;
      else
         Len := 0;
      end if;

      --  Loop through characters of input

      for J in Item'Range loop
         C := To_Unsigned_8 (Item (J));

         --  Codes in the range 16#00# - 16#7F# are represented as
         --    0xxxxxxx

         if C <= 16#7F# then
            Store (C);

         --  Codes in the range 16#80# - 16#7FF# are represented as
         --    110yyyxx 10xxxxxx

         --  For type character of course, the limit is 16#FF# in any case

         else
            Store (2#110_00000# or Shift_Right (C, 6));
            Store (2#10_000000# or (C and 2#00_111111#));
         end if;
      end loop;

      return Result (1 .. Len);
   end Encode;

   --  Encode String in UTF-16

   function Encode
     (Item       : String;
      Output_BOM : Boolean  := False) return UTF_16_Wide_String
   is
      Result : UTF_16_Wide_String
                 (1 .. Item'Length + Boolean'Pos (Output_BOM));
      --  Output is same length as input + possible BOM

      Len : Integer;
      --  Length of output string

      C : Unsigned_8;

   begin
      --  Output BOM if required

      if Output_BOM then
         Result (1) := BOM_16 (1);
         Len := 1;
      else
         Len := 0;
      end if;

      --  Loop through input characters encoding them

      for Iptr in Item'Range loop
         C := To_Unsigned_8 (Item (Iptr));

         --  Codes in the range 16#0000#..16#00FF# are output unchanged. This
         --  includes all possible cases of Character values.

         Len := Len + 1;
         Result (Len) := Wide_Character'Val (C);
      end loop;

      return Result;
   end Encode;

end Ada.Strings.UTF_Encoding.Strings;
