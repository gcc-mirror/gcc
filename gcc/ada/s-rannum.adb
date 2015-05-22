------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . R A N D O M _ N U M B E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2007-2015, Free Software Foundation, Inc.         --
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

------------------------------------------------------------------------------
--                                                                          --
-- The implementation here is derived from a C-program for MT19937, with    --
-- initialization improved 2002/1/26. As required, the following notice is  --
-- copied from the original program.                                        --
--                                                                          --
-- Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,        --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--   1. Redistributions of source code must retain the above copyright      --
--      notice, this list of conditions and the following disclaimer.       --
--                                                                          --
--   2. Redistributions in binary form must reproduce the above copyright   --
--      notice, this list of conditions and the following disclaimer in the --
--      documentation and/or other materials provided with the distribution.--
--                                                                          --
--   3. The names of its contributors may not be used to endorse or promote --
--      products derived from this software without specific prior written  --
--      permission.                                                         --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT    --
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,    --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--                                                                          --
-- This is an implementation of the Mersenne Twister, twisted generalized   --
-- feedback shift register of rational normal form, with state-bit          --
-- reflection and tempering. This version generates 32-bit integers with a  --
-- period of 2**19937 - 1 (a Mersenne prime, hence the name). For           --
-- applications requiring more than 32 bits (up to 64), we concatenate two  --
-- 32-bit numbers.                                                          --
--                                                                          --
-- See http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html for         --
-- details.                                                                 --
--                                                                          --
-- In contrast to the original code, we do not generate random numbers in   --
-- batches of N. Measurement seems to show this has very little if any      --
-- effect on performance, and it may be marginally better for real-time     --
-- applications with hard deadlines.                                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with System.Random_Seed;

with Interfaces; use Interfaces;

use Ada;

package body System.Random_Numbers with
  SPARK_Mode => Off
is
   Image_Numeral_Length : constant := Max_Image_Width / N;

   subtype Image_String is String (1 .. Max_Image_Width);

   ----------------------------
   -- Algorithmic Parameters --
   ----------------------------

   Lower_Mask : constant := 2**31 - 1;
   Upper_Mask : constant := 2**31;

   Matrix_A   : constant array (State_Val range 0 .. 1) of State_Val
     := (0, 16#9908b0df#);
   --  The twist transformation is represented by a matrix of the form
   --
   --               [  0    I(31) ]
   --               [    _a       ]
   --
   --  where 0 is a 31x31 block of 0s, I(31) is the 31x31 identity matrix and
   --  _a is a particular bit row-vector, represented here by a 32-bit integer.
   --  If integer x represents a row vector of bits (with x(0), the units bit,
   --  last), then
   --           x * A = [0 x(31..1)] xor Matrix_A(x(0)).

   U      : constant := 11;
   S      : constant := 7;
   B_Mask : constant := 16#9d2c5680#;
   T      : constant := 15;
   C_Mask : constant := 16#efc60000#;
   L      : constant := 18;
   --  The tempering shifts and bit masks, in the order applied

   Seed0 : constant := 5489;
   --  Default seed, used to initialize the state vector when Reset not called

   Seed1 : constant := 19650218;
   --  Seed used to initialize the state vector when calling Reset with an
   --  initialization vector.

   Mult0 : constant := 1812433253;
   --  Multiplier for a modified linear congruential generator used to
   --  initialize the state vector when calling Reset with a single integer
   --  seed.

   Mult1 : constant := 1664525;
   Mult2 : constant := 1566083941;
   --  Multipliers for two modified linear congruential generators used to
   --  initialize the state vector when calling Reset with an initialization
   --  vector.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Init (Gen : Generator; Initiator : Unsigned_32);
   --  Perform a default initialization of the state of Gen. The resulting
   --  state is identical for identical values of Initiator.

   procedure Insert_Image
     (S     : in out Image_String;
      Index : Integer;
      V     : State_Val);
   --  Insert image of V into S, in the Index'th 11-character substring

   function Extract_Value (S : String; Index : Integer) return State_Val;
   --  Treat S as a sequence of 11-character decimal numerals and return
   --  the result of converting numeral #Index (numbering from 0)

   function To_Unsigned is
     new Unchecked_Conversion (Integer_32, Unsigned_32);
   function To_Unsigned is
     new Unchecked_Conversion (Integer_64, Unsigned_64);

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Unsigned_32 is
      G : Generator renames Gen.Writable.Self.all;
      Y : State_Val;
      I : Integer;      --  should avoid use of identifier I ???

   begin
      I := G.I;

      if I < N - M then
         Y := (G.S (I) and Upper_Mask) or (G.S (I + 1) and Lower_Mask);
         Y := G.S (I + M) xor Shift_Right (Y, 1) xor Matrix_A (Y and 1);
         I := I + 1;

      elsif I < N - 1 then
         Y := (G.S (I) and Upper_Mask) or (G.S (I + 1) and Lower_Mask);
         Y := G.S (I + (M - N))
                xor Shift_Right (Y, 1)
                xor Matrix_A (Y and 1);
         I := I + 1;

      elsif I = N - 1 then
         Y := (G.S (I) and Upper_Mask) or (G.S (0) and Lower_Mask);
         Y := G.S (M - 1) xor Shift_Right (Y, 1) xor Matrix_A (Y and 1);
         I := 0;

      else
         Init (G, Seed0);
         return Random (Gen);
      end if;

      G.S (G.I) := Y;
      G.I := I;

      Y := Y xor Shift_Right (Y, U);
      Y := Y xor (Shift_Left (Y, S)  and B_Mask);
      Y := Y xor (Shift_Left (Y, T) and C_Mask);
      Y := Y xor Shift_Right (Y, L);

      return Y;
   end Random;

   generic
      type Unsigned is mod <>;
      type Real is digits <>;
      with function Random (G : Generator) return Unsigned is <>;
   function Random_Float_Template (Gen : Generator) return Real;
   pragma Inline (Random_Float_Template);
   --  Template for a random-number generator implementation that delivers
   --  values of type Real in the range [0 .. 1], using values from Gen,
   --  assuming that Unsigned is large enough to hold the bits of a mantissa
   --  for type Real.

   ---------------------------
   -- Random_Float_Template --
   ---------------------------

   function Random_Float_Template (Gen : Generator) return Real is

      pragma Compile_Time_Error
        (Unsigned'Last <= 2**(Real'Machine_Mantissa - 1),
         "insufficiently large modular type used to hold mantissa");

   begin
      --  This code generates random floating-point numbers from unsigned
      --  integers. Assuming that Real'Machine_Radix = 2, it can deliver all
      --  machine values of type Real (as implied by Real'Machine_Mantissa and
      --  Real'Machine_Emin), which is not true of the standard method (to
      --  which we fall back for nonbinary radix): computing Real(<random
      --  integer>) / (<max random integer>+1). To do so, we first extract an
      --  (M-1)-bit significand (where M is Real'Machine_Mantissa), and then
      --  decide on a normalized exponent by repeated coin flips, decrementing
      --  from 0 as long as we flip heads (1 bits). This process yields the
      --  proper geometric distribution for the exponent: in a uniformly
      --  distributed set of floating-point numbers, 1/2 of them will be in
      --  (0.5, 1], 1/4 will be in (0.25, 0.5], and so forth. It makes a
      --  further adjustment at binade boundaries (see comments below) to give
      --  the effect of selecting a uniformly distributed real deviate in
      --  [0..1] and then rounding to the nearest representable floating-point
      --  number.  The algorithm attempts to be stingy with random integers. In
      --  the worst case, it can consume roughly -Real'Machine_Emin/32 32-bit
      --  integers, but this case occurs with probability around
      --  2**Machine_Emin, and the expected number of calls to integer-valued
      --  Random is 1.  For another discussion of the issues addressed by this
      --  process, see Allen Downey's unpublished paper at
      --  http://allendowney.com/research/rand/downey07randfloat.pdf.

      if Real'Machine_Radix /= 2 then
         return Real'Machine
           (Real (Unsigned'(Random (Gen))) * 2.0**(-Unsigned'Size));

      else
         declare
            type Bit_Count is range 0 .. 4;

            subtype T is Real'Base;

            Trailing_Ones : constant array (Unsigned_32 range 0 .. 15)
              of Bit_Count :=
                  (2#00000# => 0, 2#00001# => 1, 2#00010# => 0, 2#00011# => 2,
                   2#00100# => 0, 2#00101# => 1, 2#00110# => 0, 2#00111# => 3,
                   2#01000# => 0, 2#01001# => 1, 2#01010# => 0, 2#01011# => 2,
                   2#01100# => 0, 2#01101# => 1, 2#01110# => 0, 2#01111# => 4);

            Pow_Tab : constant array (Bit_Count range 0 .. 3) of Real
              := (0 => 2.0**(0 - T'Machine_Mantissa),
                  1 => 2.0**(-1 - T'Machine_Mantissa),
                  2 => 2.0**(-2 - T'Machine_Mantissa),
                  3 => 2.0**(-3 - T'Machine_Mantissa));

            Extra_Bits : constant Natural :=
                         (Unsigned'Size - T'Machine_Mantissa + 1);
            --  Random bits left over after selecting mantissa

            Mantissa : Unsigned;

            X      : Real;            --  Scaled mantissa
            R      : Unsigned_32;     --  Supply of random bits
            R_Bits : Natural;         --  Number of bits left in R
            K      : Bit_Count;       --  Next decrement to exponent

         begin
            Mantissa := Random (Gen) / 2**Extra_Bits;
            R := Unsigned_32 (Mantissa mod 2**Extra_Bits);
            R_Bits := Extra_Bits;
            X := Real (2**(T'Machine_Mantissa - 1) + Mantissa); -- Exact

            if Extra_Bits < 4 and then R < 2 ** Extra_Bits - 1 then

               --  We got lucky and got a zero in our few extra bits

               K := Trailing_Ones (R);

            else
               Find_Zero : loop

                  --  R has R_Bits unprocessed random bits, a multiple of 4.
                  --  X needs to be halved for each trailing one bit. The
                  --  process stops as soon as a 0 bit is found. If R_Bits
                  --  becomes zero, reload R.

                  --  Process 4 bits at a time for speed: the two iterations
                  --  on average with three tests each was still too slow,
                  --  probably because the branches are not predictable.
                  --  This loop now will only execute once 94% of the cases,
                  --  doing more bits at a time will not help.

                  while R_Bits >= 4 loop
                     K := Trailing_Ones (R mod 16);

                     exit Find_Zero when K < 4; -- Exits 94% of the time

                     R_Bits := R_Bits - 4;
                     X := X / 16.0;
                     R := R / 16;
                  end loop;

                  --  Do not allow us to loop endlessly even in the (very
                  --  unlikely) case that Random (Gen) keeps yielding all ones.

                  exit Find_Zero when X = 0.0;
                  R := Random (Gen);
                  R_Bits := 32;
               end loop Find_Zero;
            end if;

            --  K has the count of trailing ones not reflected yet in X. The
            --  following multiplication takes care of that, as well as the
            --  correction to move the radix point to the left of the mantissa.
            --  Doing it at the end avoids repeated rounding errors in the
            --  exceedingly unlikely case of ever having a subnormal result.

            X := X * Pow_Tab (K);

            --  The smallest value in each binade is rounded to by 0.75 of
            --  the span of real numbers as its next larger neighbor, and
            --  1.0 is rounded to by half of the span of real numbers as its
            --  next smaller neighbor. To account for this, when we encounter
            --  the smallest number in a binade, we substitute the smallest
            --  value in the next larger binade with probability 1/2.

            if Mantissa = 0 and then Unsigned_32'(Random (Gen)) mod 2 = 0 then
               X := 2.0 * X;
            end if;

            return X;
         end;
      end if;
   end Random_Float_Template;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Float is
      function F is new Random_Float_Template (Unsigned_32, Float);
   begin
      return F (Gen);
   end Random;

   function Random (Gen : Generator) return Long_Float is
      function F is new Random_Float_Template (Unsigned_64, Long_Float);
   begin
      return F (Gen);
   end Random;

   function Random (Gen : Generator) return Unsigned_64 is
   begin
      return Shift_Left (Unsigned_64 (Unsigned_32'(Random (Gen))), 32)
        or Unsigned_64 (Unsigned_32'(Random (Gen)));
   end Random;

   ---------------------
   -- Random_Discrete --
   ---------------------

   function Random_Discrete
     (Gen : Generator;
      Min : Result_Subtype := Default_Min;
      Max : Result_Subtype := Result_Subtype'Last) return Result_Subtype
   is
   begin
      if Max = Min then
         return Max;

      elsif Max < Min then
         raise Constraint_Error;

      elsif Result_Subtype'Base'Size > 32 then
         declare
            --  In the 64-bit case, we have to be careful, since not all 64-bit
            --  unsigned values are representable in GNAT's root_integer type.
            --  Ignore different-size warnings here since GNAT's handling
            --  is correct.

            pragma Warnings ("Z");
            function Conv_To_Unsigned is
               new Unchecked_Conversion (Result_Subtype'Base, Unsigned_64);
            function Conv_To_Result is
               new Unchecked_Conversion (Unsigned_64, Result_Subtype'Base);
            pragma Warnings ("z");

            N : constant Unsigned_64 :=
                  Conv_To_Unsigned (Max) - Conv_To_Unsigned (Min) + 1;

            X, Slop : Unsigned_64;

         begin
            if N = 0 then
               return Conv_To_Result (Conv_To_Unsigned (Min) + Random (Gen));

            else
               Slop := Unsigned_64'Last rem N + 1;

               loop
                  X := Random (Gen);
                  exit when Slop = N or else X <= Unsigned_64'Last - Slop;
               end loop;

               return Conv_To_Result (Conv_To_Unsigned (Min) + X rem N);
            end if;
         end;

      elsif Result_Subtype'Pos (Max) - Result_Subtype'Pos (Min) =
                                                         2 ** 32 - 1
      then
         return Result_Subtype'Val
           (Result_Subtype'Pos (Min) + Unsigned_32'Pos (Random (Gen)));
      else
         declare
            N    : constant Unsigned_32 :=
                     Unsigned_32 (Result_Subtype'Pos (Max) -
                                    Result_Subtype'Pos (Min) + 1);
            Slop : constant Unsigned_32 := Unsigned_32'Last rem N + 1;
            X    : Unsigned_32;

         begin
            loop
               X := Random (Gen);
               exit when Slop = N or else X <= Unsigned_32'Last - Slop;
            end loop;

            return
              Result_Subtype'Val
                (Result_Subtype'Pos (Min) + Unsigned_32'Pos (X rem N));
         end;
      end if;
   end Random_Discrete;

   ------------------
   -- Random_Float --
   ------------------

   function Random_Float (Gen : Generator) return Result_Subtype is
   begin
      if Result_Subtype'Base'Digits > Float'Digits then
         return Result_Subtype'Machine (Result_Subtype
                                         (Long_Float'(Random (Gen))));
      else
         return Result_Subtype'Machine (Result_Subtype
                                         (Float'(Random (Gen))));
      end if;
   end Random_Float;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : Generator) is
   begin
      Init (Gen, Unsigned_32'Mod (Random_Seed.Get_Seed));
   end Reset;

   procedure Reset (Gen : Generator; Initiator : Integer_32) is
   begin
      Init (Gen, To_Unsigned (Initiator));
   end Reset;

   procedure Reset (Gen : Generator; Initiator : Unsigned_32) is
   begin
      Init (Gen, Initiator);
   end Reset;

   procedure Reset (Gen : Generator; Initiator : Integer) is
   begin
      --  This is probably an unnecessary precaution against future change, but
      --  since the test is a static expression, no extra code is involved.

      if Integer'Size <= 32 then
         Init (Gen, To_Unsigned (Integer_32 (Initiator)));

      else
         declare
            Initiator1 : constant Unsigned_64 :=
                           To_Unsigned (Integer_64 (Initiator));
            Init0      : constant Unsigned_32 :=
                           Unsigned_32 (Initiator1 mod 2 ** 32);
            Init1      : constant Unsigned_32 :=
                           Unsigned_32 (Shift_Right (Initiator1, 32));
         begin
            Reset (Gen, Initialization_Vector'(Init0, Init1));
         end;
      end if;
   end Reset;

   procedure Reset (Gen : Generator; Initiator : Initialization_Vector) is
      G    : Generator renames Gen.Writable.Self.all;
      I, J : Integer;

   begin
      Init (G, Seed1);
      I := 1;
      J := 0;

      if Initiator'Length > 0 then
         for K in reverse 1 .. Integer'Max (N, Initiator'Length) loop
            G.S (I) :=
              (G.S (I) xor ((G.S (I - 1)
                               xor Shift_Right (G.S (I - 1), 30)) * Mult1))
              + Initiator (J + Initiator'First) + Unsigned_32 (J);

            I := I + 1;
            J := J + 1;

            if I >= N then
               G.S (0) := G.S (N - 1);
               I := 1;
            end if;

            if J >= Initiator'Length then
               J := 0;
            end if;
         end loop;
      end if;

      for K in reverse 1 .. N - 1 loop
         G.S (I) :=
           (G.S (I) xor ((G.S (I - 1)
                            xor Shift_Right (G.S (I - 1), 30)) * Mult2))
           - Unsigned_32 (I);
         I := I + 1;

         if I >= N then
            G.S (0) := G.S (N - 1);
            I := 1;
         end if;
      end loop;

      G.S (0) := Upper_Mask;
   end Reset;

   procedure Reset (Gen : Generator; From_State : Generator) is
      G : Generator renames Gen.Writable.Self.all;
   begin
      G.S := From_State.S;
      G.I := From_State.I;
   end Reset;

   procedure Reset (Gen : Generator; From_State : State) is
      G : Generator renames Gen.Writable.Self.all;
   begin
      G.I := 0;
      G.S := From_State;
   end Reset;

   procedure Reset (Gen : Generator; From_Image : String) is
      G : Generator renames Gen.Writable.Self.all;
   begin
      G.I := 0;

      for J in 0 .. N - 1 loop
         G.S (J) := Extract_Value (From_Image, J);
      end loop;
   end Reset;

   ----------
   -- Save --
   ----------

   procedure Save (Gen : Generator; To_State : out State) is
      Gen2 : Generator;

   begin
      if Gen.I = N then
         Init (Gen2, 5489);
         To_State := Gen2.S;

      else
         To_State (0 .. N - 1 - Gen.I) := Gen.S (Gen.I .. N - 1);
         To_State (N - Gen.I .. N - 1) := Gen.S (0 .. Gen.I - 1);
      end if;
   end Save;

   -----------
   -- Image --
   -----------

   function Image (Of_State : State) return String is
      Result : Image_String;

   begin
      Result := (others => ' ');

      for J in Of_State'Range loop
         Insert_Image (Result, J, Of_State (J));
      end loop;

      return Result;
   end Image;

   function Image (Gen : Generator) return String is
      Result : Image_String;

   begin
      Result := (others => ' ');
      for J in 0 .. N - 1 loop
         Insert_Image (Result, J, Gen.S ((J + Gen.I) mod N));
      end loop;

      return Result;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return State is
      Gen : Generator;
      S   : State;
   begin
      Reset (Gen, Coded_State);
      Save (Gen, S);
      return S;
   end Value;

   ----------
   -- Init --
   ----------

   procedure Init (Gen : Generator; Initiator : Unsigned_32) is
      G : Generator renames Gen.Writable.Self.all;
   begin
      G.S (0) := Initiator;

      for I in 1 .. N - 1 loop
         G.S (I) :=
           (G.S (I - 1) xor Shift_Right (G.S (I - 1), 30)) * Mult0
           + Unsigned_32 (I);
      end loop;

      G.I := 0;
   end Init;

   ------------------
   -- Insert_Image --
   ------------------

   procedure Insert_Image
     (S     : in out Image_String;
      Index : Integer;
      V     : State_Val)
   is
      Value : constant String := State_Val'Image (V);
   begin
      S (Index * 11 + 1 .. Index * 11 + Value'Length) := Value;
   end Insert_Image;

   -------------------
   -- Extract_Value --
   -------------------

   function Extract_Value (S : String; Index : Integer) return State_Val is
      Start : constant Integer := S'First + Index * Image_Numeral_Length;
   begin
      return State_Val'Value (S (Start .. Start + Image_Numeral_Length - 1));
   end Extract_Value;

end System.Random_Numbers;
