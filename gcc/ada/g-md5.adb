------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                             G N A T . M D 5                              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--              Copyright (C) 2002 Ada Core Technologies, Inc.              --
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

with Ada.Unchecked_Conversion;

package body GNAT.MD5 is

   use Interfaces;

   Padding : constant String :=
     (1 => Character'Val (16#80#), 2 .. 64 => ASCII.NUL);

   Hex_Digit : constant array (Unsigned_32 range 0 .. 15) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
   --  Look-up table for each hex digit of the Message-Digest.
   --  Used by function Digest (Context).

   --  The sixten values used to rotate the context words.
   --  Four for each rounds. Used in procedure Transform.

   --  Round 1

   S11 : constant := 7;
   S12 : constant := 12;
   S13 : constant := 17;
   S14 : constant := 22;

   --  Round 2

   S21 : constant := 5;
   S22 : constant := 9;
   S23 : constant := 14;
   S24 : constant := 20;

   --  Round 3

   S31 : constant := 4;
   S32 : constant := 11;
   S33 : constant := 16;
   S34 : constant := 23;

   --  Round 4

   S41 : constant := 6;
   S42 : constant := 10;
   S43 : constant := 15;
   S44 : constant := 21;

   type Sixteen_Words is array (Natural range 0 .. 15)
     of Interfaces.Unsigned_32;
   --  Sixteen 32-bit words, converted from block of 64 characters.
   --  Used in procedure Decode and Transform.

   procedure Decode
     (Block : String;
      X     : out Sixteen_Words);
   --  Convert a String of 64 characters into 16 32-bit numbers

   --  The following functions (F, FF, G, GG, H, HH, I and II) are the
   --  equivalent of the macros of the same name in the example
   --  C implementation in the annex of RFC 1321.

   function F (X, Y, Z : Unsigned_32) return Unsigned_32;
   pragma Inline (F);

   procedure FF
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive);
   pragma Inline (FF);

   function G (X, Y, Z : Unsigned_32) return Unsigned_32;
   pragma Inline (G);

   procedure GG
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive);
   pragma Inline (GG);

   function H (X, Y, Z : Unsigned_32) return Unsigned_32;
   pragma Inline (H);

   procedure HH
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive);
   pragma Inline (HH);

   function I (X, Y, Z : Unsigned_32) return Unsigned_32;
   pragma Inline (I);

   procedure II
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive);
   pragma Inline (II);

   procedure Transform
     (C     : in out Context;
      Block : String);
   --  Process one block of 64 characters.

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Block : String;
      X     : out Sixteen_Words)
   is
      Cur   : Positive := Block'First;

   begin
      pragma Assert (Block'Length = 64);

      for Index in X'Range loop
         X (Index) :=
           Unsigned_32 (Character'Pos (Block (Cur))) +
           Shift_Left (Unsigned_32 (Character'Pos (Block (Cur + 1))), 8) +
           Shift_Left (Unsigned_32 (Character'Pos (Block (Cur + 2))), 16) +
           Shift_Left (Unsigned_32 (Character'Pos (Block (Cur + 3))), 24);
         Cur := Cur + 4;
      end loop;
   end Decode;

   ------------
   -- Digest --
   ------------

   function Digest (C : Context) return Message_Digest is
      Result : Message_Digest;

      Cur : Natural := 1;
      --  Index in Result where the next character will be placed.

      procedure Convert (X : Unsigned_32);
      --  Put the contribution of one of the four words (A, B, C, D) of the
      --  Context in Result. Increments Cur.

      -------------
      -- Convert --
      -------------

      procedure Convert (X : Unsigned_32) is
         Y : Unsigned_32 := X;

      begin
         for J in 1 .. 4 loop
            Result (Cur + 1) := Hex_Digit (Y and Unsigned_32'(16#0F#));
            Y := Shift_Right (Y, 4);
            Result (Cur) := Hex_Digit (Y and Unsigned_32'(16#0F#));
            Y := Shift_Right (Y, 4);
            Cur := Cur + 2;
         end loop;
      end Convert;

   --  Start of processing for Digest

   begin
      Convert (C.A);
      Convert (C.B);
      Convert (C.C);
      Convert (C.D);
      return Result;
   end Digest;

   function Digest (S : String) return Message_Digest is
      C : Context;

   begin
      Update (C, S);
      return Digest (C);
   end Digest;

   function Digest
     (A    : Ada.Streams.Stream_Element_Array)
      return Message_Digest
   is
      C : Context;

   begin
      Update (C, A);
      return Digest (C);
   end Digest;

   -------
   -- F --
   -------

   function F (X, Y, Z : Unsigned_32) return Unsigned_32 is
   begin
      return (X and Y) or ((not X) and Z);
   end F;

   --------
   -- FF --
   --------

   procedure FF
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive)
   is
   begin
      A := A + F (B, C, D) + X + AC;
      A := Rotate_Left (A, S);
      A := A + B;
   end FF;

   -------
   -- G --
   -------

   function G (X, Y, Z : Unsigned_32) return Unsigned_32 is
   begin
      return (X and Z) or (Y and (not Z));
   end G;

   --------
   -- GG --
   --------

   procedure GG
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive)
   is
   begin
      A := A + G (B, C, D) + X + AC;
      A := Rotate_Left (A, S);
      A := A + B;
   end GG;

   -------
   -- H --
   -------

   function H (X, Y, Z : Unsigned_32) return Unsigned_32 is
   begin
      return X xor Y xor Z;
   end H;

   --------
   -- HH --
   --------

   procedure HH
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive)
   is
   begin
      A := A + H (B, C, D) + X + AC;
      A := Rotate_Left (A, S);
      A := A + B;
   end HH;

   -------
   -- I --
   -------

   function I (X, Y, Z : Unsigned_32) return Unsigned_32 is
   begin
      return Y xor (X or (not Z));
   end I;

   --------
   -- II --
   --------

   procedure II
     (A       : in out Unsigned_32;
      B, C, D : Unsigned_32;
      X       : Unsigned_32;
      AC      : Unsigned_32;
      S       : Positive)
   is
   begin
      A := A + I (B, C, D) + X + AC;
      A := Rotate_Left (A, S);
      A := A + B;
   end II;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (C     : in out Context;
      Block : String)
   is
      X : Sixteen_Words;

      AA : Unsigned_32 := C.A;
      BB : Unsigned_32 := C.B;
      CC : Unsigned_32 := C.C;
      DD : Unsigned_32 := C.D;

   begin
      pragma Assert (Block'Length = 64);

      Decode (Block, X);

      --  Round 1

      FF (AA, BB, CC, DD, X (00), 16#D76aa478#, S11); --  1
      FF (DD, AA, BB, CC, X (01), 16#E8c7b756#, S12); --  2
      FF (CC, DD, AA, BB, X (02), 16#242070db#, S13); --  3
      FF (BB, CC, DD, AA, X (03), 16#C1bdceee#, S14); --  4

      FF (AA, BB, CC, DD, X (04), 16#f57c0faf#, S11); --  5
      FF (DD, AA, BB, CC, X (05), 16#4787c62a#, S12); --  6
      FF (CC, DD, AA, BB, X (06), 16#a8304613#, S13); --  7
      FF (BB, CC, DD, AA, X (07), 16#fd469501#, S14); --  8

      FF (AA, BB, CC, DD, X (08), 16#698098d8#, S11); --  9
      FF (DD, AA, BB, CC, X (09), 16#8b44f7af#, S12); --  10
      FF (CC, DD, AA, BB, X (10), 16#ffff5bb1#, S13); --  11
      FF (BB, CC, DD, AA, X (11), 16#895cd7be#, S14); --  12

      FF (AA, BB, CC, DD, X (12), 16#6b901122#, S11); --  13
      FF (DD, AA, BB, CC, X (13), 16#fd987193#, S12); --  14
      FF (CC, DD, AA, BB, X (14), 16#a679438e#, S13); --  15
      FF (BB, CC, DD, AA, X (15), 16#49b40821#, S14); --  16

      --  Round 2

      GG (AA, BB, CC, DD, X (01), 16#f61e2562#, S21); --  17
      GG (DD, AA, BB, CC, X (06), 16#c040b340#, S22); --  18
      GG (CC, DD, AA, BB, X (11), 16#265e5a51#, S23); --  19
      GG (BB, CC, DD, AA, X (00), 16#e9b6c7aa#, S24); --  20

      GG (AA, BB, CC, DD, X (05), 16#d62f105d#, S21); --  21
      GG (DD, AA, BB, CC, X (10), 16#02441453#, S22); --  22
      GG (CC, DD, AA, BB, X (15), 16#d8a1e681#, S23); --  23
      GG (BB, CC, DD, AA, X (04), 16#e7d3fbc8#, S24); --  24

      GG (AA, BB, CC, DD, X (09), 16#21e1cde6#, S21); --  25
      GG (DD, AA, BB, CC, X (14), 16#c33707d6#, S22); --  26
      GG (CC, DD, AA, BB, X (03), 16#f4d50d87#, S23); --  27
      GG (BB, CC, DD, AA, X (08), 16#455a14ed#, S24); --  28

      GG (AA, BB, CC, DD, X (13), 16#a9e3e905#, S21); --  29
      GG (DD, AA, BB, CC, X (02), 16#fcefa3f8#, S22); --  30
      GG (CC, DD, AA, BB, X (07), 16#676f02d9#, S23); --  31
      GG (BB, CC, DD, AA, X (12), 16#8d2a4c8a#, S24); --  32

      --  Round 3

      HH (AA, BB, CC, DD, X (05), 16#fffa3942#, S31); --  33
      HH (DD, AA, BB, CC, X (08), 16#8771f681#, S32); --  34
      HH (CC, DD, AA, BB, X (11), 16#6d9d6122#, S33); --  35
      HH (BB, CC, DD, AA, X (14), 16#fde5380c#, S34); --  36

      HH (AA, BB, CC, DD, X (01), 16#a4beea44#, S31); --  37
      HH (DD, AA, BB, CC, X (04), 16#4bdecfa9#, S32); --  38
      HH (CC, DD, AA, BB, X (07), 16#f6bb4b60#, S33); --  39
      HH (BB, CC, DD, AA, X (10), 16#bebfbc70#, S34); --  40

      HH (AA, BB, CC, DD, X (13), 16#289b7ec6#, S31); --  41
      HH (DD, AA, BB, CC, X (00), 16#eaa127fa#, S32); --  42
      HH (CC, DD, AA, BB, X (03), 16#d4ef3085#, S33); --  43
      HH (BB, CC, DD, AA, X (06), 16#04881d05#, S34); --  44

      HH (AA, BB, CC, DD, X (09), 16#d9d4d039#, S31); --  45
      HH (DD, AA, BB, CC, X (12), 16#e6db99e5#, S32); --  46
      HH (CC, DD, AA, BB, X (15), 16#1fa27cf8#, S33); --  47
      HH (BB, CC, DD, AA, X (02), 16#c4ac5665#, S34); --  48

      --  Round 4

      II (AA, BB, CC, DD, X (00), 16#f4292244#, S41); --  49
      II (DD, AA, BB, CC, X (07), 16#432aff97#, S42); --  50
      II (CC, DD, AA, BB, X (14), 16#ab9423a7#, S43); --  51
      II (BB, CC, DD, AA, X (05), 16#fc93a039#, S44); --  52

      II (AA, BB, CC, DD, X (12), 16#655b59c3#, S41); --  53
      II (DD, AA, BB, CC, X (03), 16#8f0ccc92#, S42); --  54
      II (CC, DD, AA, BB, X (10), 16#ffeff47d#, S43); --  55
      II (BB, CC, DD, AA, X (01), 16#85845dd1#, S44); --  56

      II (AA, BB, CC, DD, X (08), 16#6fa87e4f#, S41); --  57
      II (DD, AA, BB, CC, X (15), 16#fe2ce6e0#, S42); --  58
      II (CC, DD, AA, BB, X (06), 16#a3014314#, S43); --  59
      II (BB, CC, DD, AA, X (13), 16#4e0811a1#, S44); --  60

      II (AA, BB, CC, DD, X (04), 16#f7537e82#, S41); --  61
      II (DD, AA, BB, CC, X (11), 16#bd3af235#, S42); --  62
      II (CC, DD, AA, BB, X (02), 16#2ad7d2bb#, S43); --  63
      II (BB, CC, DD, AA, X (09), 16#eb86d391#, S44); --  64

      C.A := C.A + AA;
      C.B := C.B + BB;
      C.C := C.C + CC;
      C.D := C.D + DD;

   end Transform;

   ------------
   -- Update --
   ------------

   procedure Update
     (C     : in out Context;
      Input : String)
   is
      Cur        : Positive := Input'First;
      Last_Block : String (1 .. 64);

   begin
      while Cur + 63 <= Input'Last loop
         Transform (C, Input (Cur .. Cur + 63));
         Cur := Cur + 64;
      end loop;

      Last_Block (1 .. Input'Last - Cur + 1) := Input (Cur .. Input'Last);

      if Input'Last - Cur + 1 > 56 then
         Cur := Input'Last - Cur + 2;
         Last_Block (Cur .. 64) := Padding (1 .. 64 - Cur + 1);
         Transform (C, Last_Block);
         Last_Block := (others => ASCII.NUL);

      else
         Cur := Input'Last - Cur + 2;
         Last_Block (Cur .. 56) := Padding (1 .. 56 - Cur + 1);
      end if;

      --  Add the input length as 8 characters

      Last_Block (57 .. 64) := (others => ASCII.NUL);

      declare
         L : Unsigned_64 := Unsigned_64 (Input'Length) * 8;

      begin
         Cur := 57;
         while L > 0 loop
            Last_Block (Cur) := Character'Val (L and 16#Ff#);
            L := Shift_Right (L, 8);
            Cur := Cur + 1;
         end loop;
      end;

      Transform (C, Last_Block);
   end Update;

   procedure Update
     (C     : in out Context;
      Input : Ada.Streams.Stream_Element_Array)
   is
      subtype Stream_Array is Ada.Streams.Stream_Element_Array (Input'Range);
      subtype Stream_String is
        String (1 + Integer (Input'First) .. 1 + Integer (Input'Last));

      function To_String is new Ada.Unchecked_Conversion
        (Stream_Array, Stream_String);

      String_Input : constant String := To_String (Input);
   begin
      Update (C, String_Input);
   end Update;

   -----------------
   -- Wide_Digest --
   -----------------

   function Wide_Digest (W : Wide_String) return Message_Digest is
      C : Context;

   begin
      Wide_Update (C, W);
      return Digest (C);
   end Wide_Digest;

   -----------------
   -- Wide_Update --
   -----------------

   procedure Wide_Update
     (C     : in out Context;
      Input : Wide_String)
   is

      String_Input : String (1 .. 2 * Input'Length);
      Cur          : Positive := 1;

   begin
      for Index in Input'Range loop
         String_Input (Cur) :=
           Character'Val
            (Unsigned_32 (Wide_Character'Pos (Input (Index))) and 16#FF#);
         Cur := Cur + 1;
         String_Input (Cur) :=
           Character'Val
           (Shift_Right (Unsigned_32 (Wide_Character'Pos (Input (Index))), 8)
            and 16#FF#);
         Cur := Cur + 1;
      end loop;

      Update (C, String_Input);
   end Wide_Update;

end GNAT.MD5;
