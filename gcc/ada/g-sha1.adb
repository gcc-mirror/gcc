------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                           G N A T . S H A 1                              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                     Copyright (C) 2002-2006, AdaCore                     --
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

--  Note: the code for this unit is derived from GNAT.MD5

with Ada.Unchecked_Conversion;

package body GNAT.SHA1 is

   use Interfaces;

   Padding : constant String :=
     (1 => Character'Val (16#80#), 2 .. 64 => ASCII.NUL);

   Hex_Digit : constant array (Unsigned_32 range 0 .. 15) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
   --  Look-up table for each hex digit of the Message-Digest.
   --  Used by function Digest (Context).

   type Sixteen_Words is array (Natural range 0 .. 15)
     of Interfaces.Unsigned_32;
   --  Sixteen 32-bit words, converted from block of 64 characters.
   --  Used in procedure Decode and Transform.

   procedure Decode (Block : String; X : out Sixteen_Words);
   --  Convert a String of 64 characters into 16 32-bit numbers

   --  The following functions are the four elementary components of each
   --  of the four round groups (0 .. 19, 20 .. 39, 40 .. 59, and 60 .. 79)
   --  defined in RFC 3174.

   function F0 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F0);

   function F1 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F1);

   function F2 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F2);

   function F3 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F3);

   procedure Transform (Ctx : in out Context; Block : String);
   --  Process one block of 64 characters

   ------------
   -- Decode --
   ------------

   procedure Decode (Block : String; X : out Sixteen_Words) is
      Cur : Positive := Block'First;

   begin
      pragma Assert (Block'Length = 64);

      for Index in X'Range loop
         X (Index) :=
           Unsigned_32 (Character'Pos (Block (Cur + 3))) +
           Shift_Left (Unsigned_32 (Character'Pos (Block (Cur + 2))), 8) +
           Shift_Left (Unsigned_32 (Character'Pos (Block (Cur + 1))), 16) +
           Shift_Left (Unsigned_32 (Character'Pos (Block (Cur))), 24);
         Cur := Cur + 4;
      end loop;
   end Decode;

   ------------
   -- Digest --
   ------------

   function Digest (C : Context) return Message_Digest is
      Result : Message_Digest;

      Cur : Natural := 1;
      --  Index in Result where the next character will be placed

      Last_Block : String (1 .. 64);

      C1 : Context := C;

      procedure Convert (X : Unsigned_32);
      --  Put the contribution of one of the five H words of the Context in
      --  Result. Increments Cur.

      -------------
      -- Convert --
      -------------

      procedure Convert (X : Unsigned_32) is
         Y : Unsigned_32 := X;
      begin
         for J in 1 .. 8 loop
            Y := Rotate_Left (Y, 4);
            Result (Cur) := Hex_Digit (Y and Unsigned_32'(16#0F#));
            Cur := Cur + 1;
         end loop;
      end Convert;

   --  Start of processing for Digest

   begin
      --  Process characters in the context buffer, if any

      pragma Assert (C.Last /= C.Buffer'Last);
      Last_Block (1 .. C.Last) := C.Buffer (1 .. C.Last);

      if C.Last > 55 then
         Last_Block (C.Last + 1 .. 64) := Padding (1 .. 64 - C.Last);
         Transform (C1, Last_Block);
         Last_Block := (others => ASCII.NUL);

      else
         Last_Block (C.Last + 1 .. 56) := Padding (1 .. 56 - C.Last);
      end if;

      --  Add the input length (as stored in the context) as 8 characters

      Last_Block (57 .. 64) := (others => ASCII.NUL);

      declare
         L   : Unsigned_64 := Unsigned_64 (C.Length) * 8;
         Idx : Positive := 64;
      begin
         while L > 0 loop
            Last_Block (Idx) := Character'Val (L and 16#Ff#);
            L := Shift_Right (L, 8);
            Idx := Idx - 1;
         end loop;
      end;

      Transform (C1, Last_Block);

      Convert (C1.H (0));
      Convert (C1.H (1));
      Convert (C1.H (2));
      Convert (C1.H (3));
      Convert (C1.H (4));
      return Result;
   end Digest;

   function Digest (S : String) return Message_Digest is
      C : Context;
   begin
      Update (C, S);
      return Digest (C);
   end Digest;

   function Digest
     (A : Ada.Streams.Stream_Element_Array) return Message_Digest
   is
      C : Context;
   begin
      Update (C, A);
      return Digest (C);
   end Digest;

   --------
   -- F0 --
   --------

   function F0
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
   begin
      return (B and C) or ((not B) and D);
   end F0;

   --------
   -- F1 --
   --------

   function F1
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
   begin
      return B xor C xor D;
   end F1;

   --------
   -- F2 --
   --------

   function F2
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
   begin
      return (B and C) or (B and D) or (C and D);
   end F2;

   --------
   -- F3 --
   --------

   function F3
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     renames F1;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Ctx   : in out Context;
      Block : String)
   is
      W : array (0 .. 79) of Interfaces.Unsigned_32;

      A, B, C, D, E, Temp : Interfaces.Unsigned_32;

   begin
      pragma Assert (Block'Length = 64);

      --  a. Divide data block into sixteen words

      Decode (Block, Sixteen_Words (W (0 .. 15)));

      --  b. Prepare working block of 80 words

      for T in 16 .. 79 loop

         --  W(t) = S^1(W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16))

         W (T) := Rotate_Left
           (W (T - 3) xor W (T - 8) xor W (T - 14) xor W (T - 16), 1);

      end loop;

      --  c. Set up transformation variables

      A := Ctx.H (0);
      B := Ctx.H (1);
      C := Ctx.H (2);
      D := Ctx.H (3);
      E := Ctx.H (4);

      --  d. For each of the 80 rounds, compute:

      --  TEMP = S^5(A) + f(t;B,C,D) + E + W(t) + K(t);
      --  E = D;  D = C;  C = S^30(B);  B = A; A = TEMP;

      for T in 0 .. 19 loop
         Temp := Rotate_Left (A, 5) + F0 (B, C, D) + E + W (T) + 16#5A827999#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      for T in 20 .. 39 loop
         Temp := Rotate_Left (A, 5) + F1 (B, C, D) + E + W (T) + 16#6ED9EBA1#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      for T in 40 .. 59 loop
         Temp := Rotate_Left (A, 5) + F2 (B, C, D) + E + W (T) + 16#8F1BBCDC#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      for T in 60 .. 79 loop
         Temp := Rotate_Left (A, 5) + F3 (B, C, D) + E + W (T) + 16#CA62C1D6#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      --  e. Update context:
      --  H0 = H0 + A, H1 = H1 + B, H2 = H2 + C, H3 = H3 + D, H4 = H4 + E

      Ctx.H (0) := Ctx.H (0) + A;
      Ctx.H (1) := Ctx.H (1) + B;
      Ctx.H (2) := Ctx.H (2) + C;
      Ctx.H (3) := Ctx.H (3) + D;
      Ctx.H (4) := Ctx.H (4) + E;
   end Transform;

   ------------
   -- Update --
   ------------

   procedure Update
     (C     : in out Context;
      Input : String)
   is
      Inp : constant String := C.Buffer (1 .. C.Last) & Input;
      Cur : Positive := Inp'First;

   begin
      C.Length := C.Length + Input'Length;

      while Cur + 63 <= Inp'Last loop
         Transform (C, Inp (Cur .. Cur + 63));
         Cur := Cur + 64;
      end loop;

      C.Last := Inp'Last - Cur + 1;
      C.Buffer (1 .. C.Last) := Inp (Cur .. Inp'Last);
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

end GNAT.SHA1;
