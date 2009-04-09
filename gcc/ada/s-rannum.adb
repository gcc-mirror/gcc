------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . R A N D O M _ N U M B E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2007,2009  Free Software Foundation, Inc.         --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Unchecked_Conversion;
with Interfaces;                use Interfaces;

use Ada;

package body System.Random_Numbers is

   -------------------------
   -- Implementation Note --
   -------------------------

   --  The design of this spec is very awkward, as a result of Ada 95 not
   --  permitting in-out parameters for function formals (most naturally,
   --  Generator values would be passed this way). In pure Ada 95, the only
   --  solution is to use the heap and pointers, and, to avoid memory leaks,
   --  controlled types.

   --  This is awfully heavy, so what we do is to use Unrestricted_Access to
   --  get a pointer to the state in the passed Generator. This works because
   --  Generator is a limited type and will thus always be passed by reference.

   Low31_Mask : constant := 2**31-1;
   Bit31_Mask : constant := 2**31;

   Matrix_A_X : constant array (State_Val range 0 .. 1) of State_Val :=
                  (0, 16#9908b0df#);

   Y2K : constant Calendar.Time :=
           Calendar.Time_Of
             (Year => 2000, Month => 1, Day => 1, Seconds => 0.0);
   --  First Year 2000 day

   subtype Image_String is String (1 .. Max_Image_Width);

   --  Utility functions

   procedure Init (Gen : out Generator; Initiator : Unsigned_32);
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
      G : Generator renames Gen'Unrestricted_Access.all;
      Y : State_Val;
      I : Integer;

   begin
      I := G.I;

      if I < N - M then
         Y := (G.S (I) and Bit31_Mask) or (G.S (I + 1) and Low31_Mask);
         Y := G.S (I + M) xor Shift_Right (Y, 1) xor Matrix_A_X (Y and 1);
         I := I + 1;

      elsif I < N - 1 then
         Y := (G.S (I) and Bit31_Mask) or (G.S (I + 1) and Low31_Mask);
         Y := G.S (I + (M - N))
                xor Shift_Right (Y, 1)
                xor Matrix_A_X (Y and 1);
         I := I + 1;

      elsif I = N - 1 then
         Y := (G.S (I) and Bit31_Mask) or (G.S (0) and Low31_Mask);
         Y := G.S (M - 1) xor Shift_Right (Y, 1) xor Matrix_A_X (Y and 1);
         I := 0;

      else
         Init (G, 5489);
         return Random (Gen);
      end if;

      G.S (G.I) := Y;
      G.I := I;

      Y := Y xor Shift_Right (Y, 11);
      Y := Y xor (Shift_Left (Y, 7)  and 16#9d2c5680#);
      Y := Y xor (Shift_Left (Y, 15) and 16#efc60000#);
      Y := Y xor Shift_Right (Y, 18);

      return Y;
   end Random;

   function Random (Gen : Generator) return Float is

      --  Note: The application of Float'Machine (...) is necessary to avoid
      --  returning extra significand bits. Without it, the function's value
      --  will change if it is spilled, for example, causing
      --  gratuitous nondeterminism.

      Result : constant Float :=
                 Float'Machine
                   (Float (Unsigned_32'(Random (Gen))) * 2.0 ** (-32));
   begin
      if Result < 1.0 then
         return Result;
      else
         return Float'Adjacent (1.0, 0.0);
      end if;
   end Random;

   function Random (Gen : Generator) return Long_Float is
      Result : constant Long_Float :=
                 Long_Float'Machine ((Long_Float (Unsigned_32'(Random (Gen)))
                   * 2.0 ** (-32))
                   + (Long_Float (Unsigned_32'(Random (Gen))) * 2.0 ** (-64)));
   begin
      if Result < 1.0 then
         return Result;
      else
         return Long_Float'Adjacent (1.0, 0.0);
      end if;
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
            --  Ignore different-size warnings here; since GNAT's handling
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

   procedure Reset (Gen : out Generator) is
      X : constant Unsigned_32 := Unsigned_32 ((Calendar.Clock - Y2K) * 64.0);
   begin
      Init (Gen, X);
   end Reset;

   procedure Reset (Gen : out Generator; Initiator : Integer_32) is
   begin
      Init (Gen, To_Unsigned (Initiator));
   end Reset;

   procedure Reset (Gen : out Generator; Initiator : Unsigned_32) is
   begin
      Init (Gen, Initiator);
   end Reset;

   procedure Reset (Gen : out Generator; Initiator : Integer) is
   begin
      pragma Warnings ("C");
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

      pragma Warnings ("c");
   end Reset;

   procedure Reset (Gen : out Generator; Initiator : Initialization_Vector) is
      I, J : Integer;

   begin
      Init (Gen, 19650218);
      I := 1;
      J := 0;

      if Initiator'Length > 0 then
         for K in reverse 1 .. Integer'Max (N, Initiator'Length) loop
            Gen.S (I) :=
              (Gen.S (I)
                 xor ((Gen.S (I - 1) xor Shift_Right (Gen.S (I - 1), 30))
                                                                 * 1664525))
              + Initiator (J + Initiator'First) + Unsigned_32 (J);

            I := I + 1;
            J := J + 1;

            if I >= N then
               Gen.S (0) := Gen.S (N - 1);
               I := 1;
            end if;

            if J >= Initiator'Length then
               J := 0;
            end if;
         end loop;
      end if;

      for K in reverse 1 .. N - 1 loop
         Gen.S (I) :=
           (Gen.S (I) xor ((Gen.S (I - 1)
                            xor Shift_Right (Gen.S (I - 1), 30)) * 1566083941))
           - Unsigned_32 (I);
         I := I + 1;

         if I >= N then
            Gen.S (0) := Gen.S (N - 1);
            I := 1;
         end if;
      end loop;

      Gen.S (0) := Bit31_Mask;
   end Reset;

   procedure Reset (Gen : out Generator; From_State : Generator) is
   begin
      Gen.S := From_State.S;
      Gen.I := From_State.I;
   end Reset;

   procedure Reset (Gen : out Generator; From_State : State) is
   begin
      Gen.I := 0;
      Gen.S := From_State;
   end Reset;

   procedure Reset (Gen : out Generator; From_Image : String) is
   begin
      Gen.I := 0;

      for J in 0 .. N - 1 loop
         Gen.S (J) := Extract_Value (From_Image, J);
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

   procedure Init (Gen : out Generator; Initiator : Unsigned_32) is
   begin
      Gen.S (0) := Initiator;

      for I in 1 .. N - 1 loop
         Gen.S (I) :=
           1812433253
             * (Gen.S (I - 1) xor Shift_Right (Gen.S (I - 1), 30))
           + Unsigned_32 (I);
      end loop;

      Gen.I := 0;
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
   begin
      return State_Val'Value (S (S'First + Index * 11 ..
                                 S'First + Index * 11 + 11));
   end Extract_Value;

end System.Random_Numbers;
