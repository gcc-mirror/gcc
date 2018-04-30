------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2018, Free Software Foundation, Inc.          --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
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

--  This file is an alternate version of s-stratt.adb based on the XDR
--  standard. It is especially useful for exchanging streams between two
--  different systems with different basic type representations and endianness.

pragma Warnings (Off, "*not allowed in compiler unit");
--  This body is used only when rebuilding the runtime library, not when
--  building the compiler, so it's OK to depend on features that would
--  otherwise break bootstrap (e.g. IF-expressions).

with Ada.IO_Exceptions;
with Ada.Streams;              use Ada.Streams;
with Ada.Unchecked_Conversion;

package body System.Stream_Attributes is

   pragma Suppress (Range_Check);
   pragma Suppress (Overflow_Check);

   use UST;

   Data_Error : exception renames Ada.IO_Exceptions.End_Error;
   --  Exception raised if insufficient data read (End_Error is mandated by
   --  AI95-00132).

   SU : constant := System.Storage_Unit;
   --  The code in this body assumes that SU = 8

   BB : constant := 2 ** SU;           --  Byte base
   BL : constant := 2 ** SU - 1;       --  Byte last
   BS : constant := 2 ** (SU - 1);     --  Byte sign

   US : constant := Unsigned'Size;     --  Unsigned size
   UB : constant := (US - 1) / SU + 1; --  Unsigned byte
   UL : constant := 2 ** US - 1;       --  Unsigned last

   subtype SE  is Ada.Streams.Stream_Element;
   subtype SEA is Ada.Streams.Stream_Element_Array;
   subtype SEO is Ada.Streams.Stream_Element_Offset;

   generic function UC renames Ada.Unchecked_Conversion;

   type Field_Type is
      record
         E_Size       : Integer; --  Exponent bit size
         E_Bias       : Integer; --  Exponent bias
         F_Size       : Integer; --  Fraction bit size
         E_Last       : Integer; --  Max exponent value
         F_Mask       : SE;      --  Mask to apply on first fraction byte
         E_Bytes      : SEO;     --  N. of exponent bytes completely used
         F_Bytes      : SEO;     --  N. of fraction bytes completely used
         F_Bits       : Integer; --  N. of bits used on first fraction word
      end record;

   type Precision is (Single, Double, Quadruple);

   Fields : constant array (Precision) of Field_Type := (

               --  Single precision

              (E_Size  => 8,
               E_Bias  => 127,
               F_Size  => 23,
               E_Last  => 2 ** 8 - 1,
               F_Mask  => 16#7F#,                  --  2 ** 7 - 1,
               E_Bytes => 2,
               F_Bytes => 3,
               F_Bits  => 23 mod US),

               --  Double precision

              (E_Size  => 11,
               E_Bias  => 1023,
               F_Size  => 52,
               E_Last  => 2 ** 11 - 1,
               F_Mask  => 16#0F#,                  --  2 ** 4 - 1,
               E_Bytes => 2,
               F_Bytes => 7,
               F_Bits  => 52 mod US),

               --  Quadruple precision

              (E_Size  => 15,
               E_Bias  => 16383,
               F_Size  => 112,
               E_Last  => 2 ** 8 - 1,
               F_Mask  => 16#FF#,                  --  2 ** 8 - 1,
               E_Bytes => 2,
               F_Bytes => 14,
               F_Bits  => 112 mod US));

   --  The representation of all items requires a multiple of four bytes
   --  (or 32 bits) of data. The bytes are numbered 0 through n-1. The bytes
   --  are read or written to some byte stream such that byte m always
   --  precedes byte m+1. If the n bytes needed to contain the data are not
   --  a multiple of four, then the n bytes are followed by enough (0 to 3)
   --  residual zero bytes, r, to make the total byte count a multiple of 4.

   --  An XDR signed integer is a 32-bit datum that encodes an integer
   --  in the range [-2147483648,2147483647]. The integer is represented
   --  in two's complement notation. The most and least significant bytes
   --  are 0 and 3, respectively. Integers are declared as follows:

   --        (MSB)                   (LSB)
   --      +-------+-------+-------+-------+
   --      |byte 0 |byte 1 |byte 2 |byte 3 |
   --      +-------+-------+-------+-------+
   --      <------------32 bits------------>

   SSI_L : constant := 1;
   SI_L  : constant := 2;
   I_L   : constant := 4;
   LI_L  : constant := 8;
   LLI_L : constant := 8;

   subtype XDR_S_SSI is SEA (1 .. SSI_L);
   subtype XDR_S_SI  is SEA (1 .. SI_L);
   subtype XDR_S_I   is SEA (1 .. I_L);
   subtype XDR_S_LI  is SEA (1 .. LI_L);
   subtype XDR_S_LLI is SEA (1 .. LLI_L);

   function Short_Short_Integer_To_XDR_S_SSI is
      new Ada.Unchecked_Conversion (Short_Short_Integer, XDR_S_SSI);
   function XDR_S_SSI_To_Short_Short_Integer is
      new Ada.Unchecked_Conversion (XDR_S_SSI, Short_Short_Integer);

   function Short_Integer_To_XDR_S_SI is
      new Ada.Unchecked_Conversion (Short_Integer, XDR_S_SI);
   function XDR_S_SI_To_Short_Integer is
      new Ada.Unchecked_Conversion (XDR_S_SI, Short_Integer);

   function Integer_To_XDR_S_I is
      new Ada.Unchecked_Conversion (Integer, XDR_S_I);
   function XDR_S_I_To_Integer is
     new Ada.Unchecked_Conversion (XDR_S_I, Integer);

   function Long_Long_Integer_To_XDR_S_LI is
      new Ada.Unchecked_Conversion (Long_Long_Integer, XDR_S_LI);
   function XDR_S_LI_To_Long_Long_Integer is
      new Ada.Unchecked_Conversion (XDR_S_LI, Long_Long_Integer);

   function Long_Long_Integer_To_XDR_S_LLI is
      new Ada.Unchecked_Conversion (Long_Long_Integer, XDR_S_LLI);
   function XDR_S_LLI_To_Long_Long_Integer is
      new Ada.Unchecked_Conversion (XDR_S_LLI, Long_Long_Integer);

   --  An XDR unsigned integer is a 32-bit datum that encodes a nonnegative
   --  integer in the range [0,4294967295]. It is represented by an unsigned
   --  binary number whose most and least significant bytes are 0 and 3,
   --  respectively. An unsigned integer is declared as follows:

   --        (MSB)                   (LSB)
   --      +-------+-------+-------+-------+
   --      |byte 0 |byte 1 |byte 2 |byte 3 |
   --      +-------+-------+-------+-------+
   --      <------------32 bits------------>

   SSU_L : constant := 1;
   SU_L  : constant := 2;
   U_L   : constant := 4;
   LU_L  : constant := 8;
   LLU_L : constant := 8;

   subtype XDR_S_SSU is SEA (1 .. SSU_L);
   subtype XDR_S_SU  is SEA (1 .. SU_L);
   subtype XDR_S_U   is SEA (1 .. U_L);
   subtype XDR_S_LU  is SEA (1 .. LU_L);
   subtype XDR_S_LLU is SEA (1 .. LLU_L);

   type XDR_SSU is mod BB ** SSU_L;
   type XDR_SU  is mod BB ** SU_L;
   type XDR_U   is mod BB ** U_L;

   function Short_Unsigned_To_XDR_S_SU is
      new Ada.Unchecked_Conversion (Short_Unsigned, XDR_S_SU);
   function XDR_S_SU_To_Short_Unsigned is
      new Ada.Unchecked_Conversion (XDR_S_SU, Short_Unsigned);

   function Unsigned_To_XDR_S_U is
      new Ada.Unchecked_Conversion (Unsigned, XDR_S_U);
   function XDR_S_U_To_Unsigned is
      new Ada.Unchecked_Conversion (XDR_S_U, Unsigned);

   function Long_Long_Unsigned_To_XDR_S_LU is
      new Ada.Unchecked_Conversion (Long_Long_Unsigned, XDR_S_LU);
   function XDR_S_LU_To_Long_Long_Unsigned is
      new Ada.Unchecked_Conversion (XDR_S_LU, Long_Long_Unsigned);

   function Long_Long_Unsigned_To_XDR_S_LLU is
      new Ada.Unchecked_Conversion (Long_Long_Unsigned, XDR_S_LLU);
   function XDR_S_LLU_To_Long_Long_Unsigned is
      new Ada.Unchecked_Conversion (XDR_S_LLU, Long_Long_Unsigned);

   --  The standard defines the floating-point data type "float" (32 bits
   --  or 4 bytes). The encoding used is the IEEE standard for normalized
   --  single-precision floating-point numbers.

   --  The standard defines the encoding used for the double-precision
   --  floating-point data type "double" (64 bits or 8 bytes). The encoding
   --  used is the IEEE standard for normalized double-precision floating-point
   --  numbers.

   SF_L  : constant := 4;   --  Single precision
   F_L   : constant := 4;   --  Single precision
   LF_L  : constant := 8;   --  Double precision
   LLF_L : constant := 16;  --  Quadruple precision

   TM_L : constant := 8;
   subtype XDR_S_TM is SEA (1 .. TM_L);
   type XDR_TM is mod BB ** TM_L;

   type XDR_SA is mod 2 ** Standard'Address_Size;
   function To_XDR_SA is new UC (System.Address, XDR_SA);
   function To_XDR_SA is new UC (XDR_SA, System.Address);

   --  Enumerations have the same representation as signed integers.
   --  Enumerations are handy for describing subsets of the integers.

   --  Booleans are important enough and occur frequently enough to warrant
   --  their own explicit type in the standard. Booleans are declared as
   --  an enumeration, with FALSE = 0 and TRUE = 1.

   --  The standard defines a string of n (numbered 0 through n-1) ASCII
   --  bytes to be the number n encoded as an unsigned integer (as described
   --  above), and followed by the n bytes of the string. Byte m of the string
   --  always precedes byte m+1 of the string, and byte 0 of the string always
   --  follows the string's length. If n is not a multiple of four, then the
   --  n bytes are followed by enough (0 to 3) residual zero bytes, r, to make
   --  the total byte count a multiple of four.

   --  To fit with XDR string, do not consider character as an enumeration
   --  type.

   C_L   : constant := 1;
   subtype XDR_S_C is SEA (1 .. C_L);

   --  Consider Wide_Character as an enumeration type

   WC_L  : constant := 4;
   subtype XDR_S_WC is SEA (1 .. WC_L);
   type XDR_WC is mod BB ** WC_L;

   --  Consider Wide_Wide_Character as an enumeration type

   WWC_L : constant := 8;
   subtype XDR_S_WWC is SEA (1 .. WWC_L);
   type XDR_WWC is mod BB ** WWC_L;

   --  Optimization: if we already have the correct Bit_Order, then some
   --  computations can be avoided since the source and the target will be
   --  identical anyway. They will be replaced by direct unchecked
   --  conversions.

   Optimize_Integers : constant Boolean :=
     Default_Bit_Order = High_Order_First;

   -----------------
   -- Block_IO_OK --
   -----------------

   --  We must inhibit Block_IO, because in XDR mode, each element is output
   --  according to XDR requirements, which is not at all the same as writing
   --  the whole array in one block.

   function Block_IO_OK return Boolean is
   begin
      return False;
   end Block_IO_OK;

   ----------
   -- I_AD --
   ----------

   function I_AD (Stream : not null access RST) return Fat_Pointer is
      FP : Fat_Pointer;

   begin
      FP.P1 := I_AS (Stream).P1;
      FP.P2 := I_AS (Stream).P1;

      return FP;
   end I_AD;

   ----------
   -- I_AS --
   ----------

   function I_AS (Stream : not null access RST) return Thin_Pointer is
      S : XDR_S_TM;
      L : SEO;
      U : XDR_TM := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      else
         for N in S'Range loop
            U := U * BB + XDR_TM (S (N));
         end loop;

         return (P1 => To_XDR_SA (XDR_SA (U)));
      end if;
   end I_AS;

   ---------
   -- I_B --
   ---------

   function I_B (Stream : not null access RST) return Boolean is
   begin
      case I_SSU (Stream) is
         when 0      => return False;
         when 1      => return True;
         when others => raise Data_Error;
      end case;
   end I_B;

   ---------
   -- I_C --
   ---------

   function I_C (Stream : not null access RST) return Character is
      S : XDR_S_C;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      else
         --  Use Ada requirements on Character representation clause

         return Character'Val (S (1));
      end if;
   end I_C;

   ---------
   -- I_F --
   ---------

   function I_F (Stream : not null access RST) return Float is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;

      Is_Positive : Boolean;
      Exponent    : Long_Unsigned;
      Fraction    : Long_Unsigned;
      Result      : Float;
      S           : SEA (1 .. F_L);
      L           : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;
      end if;

      --  Extract Fraction, Sign and Exponent

      Fraction := Long_Unsigned (S (F_L + 1 - F_Bytes) and F_Mask);
      for N in F_L + 2 - F_Bytes .. F_L loop
         Fraction := Fraction * BB + Long_Unsigned (S (N));
      end loop;
      Result := Float'Scaling (Float (Fraction), -F_Size);

      if BS <= S (1) then
         Is_Positive := False;
         Exponent := Long_Unsigned (S (1) - BS);
      else
         Is_Positive := True;
         Exponent := Long_Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         Exponent := Exponent * BB + Long_Unsigned (S (N));
      end loop;
      Exponent := Shift_Right (Exponent, Integer (E_Bytes) * SU - E_Size - 1);

      --  NaN or Infinities

      if Integer (Exponent) = E_Last then
         raise Constraint_Error;

      elsif Exponent = 0 then

         --  Signed zeros

         if Fraction = 0 then
            null;

         --  Denormalized float

         else
            Result := Float'Scaling (Result, 1 - E_Bias);
         end if;

      --  Normalized float

      else
         Result := Float'Scaling
           (1.0 + Result, Integer (Exponent) - E_Bias);
      end if;

      if not Is_Positive then
         Result := -Result;
      end if;

      return Result;
   end I_F;

   ---------
   -- I_I --
   ---------

   function I_I (Stream : not null access RST) return Integer is
      S : XDR_S_I;
      L : SEO;
      U : XDR_U := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_I_To_Integer (S);

      else
         for N in S'Range loop
            U := U * BB + XDR_U (S (N));
         end loop;

         --  Test sign and apply two complement notation

         if S (1) < BL then
            return Integer (U);

         else
            return Integer (-((XDR_U'Last xor U) + 1));
         end if;
      end if;
   end I_I;

   ----------
   -- I_LF --
   ----------

   function I_LF (Stream : not null access RST) return Long_Float is
      I       : constant Precision := Double;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;

      Is_Positive : Boolean;
      Exponent    : Long_Unsigned;
      Fraction    : Long_Long_Unsigned;
      Result      : Long_Float;
      S           : SEA (1 .. LF_L);
      L           : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;
      end if;

      --  Extract Fraction, Sign and Exponent

      Fraction := Long_Long_Unsigned (S (LF_L + 1 - F_Bytes) and F_Mask);
      for N in LF_L + 2 - F_Bytes .. LF_L loop
         Fraction := Fraction * BB + Long_Long_Unsigned (S (N));
      end loop;

      Result := Long_Float'Scaling (Long_Float (Fraction), -F_Size);

      if BS <= S (1) then
         Is_Positive := False;
         Exponent := Long_Unsigned (S (1) - BS);
      else
         Is_Positive := True;
         Exponent := Long_Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         Exponent := Exponent * BB + Long_Unsigned (S (N));
      end loop;

      Exponent := Shift_Right (Exponent, Integer (E_Bytes) * SU - E_Size - 1);

      --  NaN or Infinities

      if Integer (Exponent) = E_Last then
         raise Constraint_Error;

      elsif Exponent = 0 then

         --  Signed zeros

         if Fraction = 0 then
            null;

         --  Denormalized float

         else
            Result := Long_Float'Scaling (Result, 1 - E_Bias);
         end if;

      --  Normalized float

      else
         Result := Long_Float'Scaling
           (1.0 + Result, Integer (Exponent) - E_Bias);
      end if;

      if not Is_Positive then
         Result := -Result;
      end if;

      return Result;
   end I_LF;

   ----------
   -- I_LI --
   ----------

   function I_LI (Stream : not null access RST) return Long_Integer is
      S : XDR_S_LI;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return Long_Integer (XDR_S_LI_To_Long_Long_Integer (S));

      else

         --  Compute using machine unsigned
         --  rather than long_long_unsigned

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned

            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         --  Test sign and apply two complement notation

         if S (1) < BL then
            return Long_Integer (X);
         else
            return Long_Integer (-((Long_Unsigned'Last xor X) + 1));
         end if;

      end if;
   end I_LI;

   -----------
   -- I_LLF --
   -----------

   function I_LLF (Stream : not null access RST) return Long_Long_Float is
      I       : constant Precision := Quadruple;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;

      Is_Positive   : Boolean;
      Exponent   : Long_Unsigned;
      Fraction_1 : Long_Long_Unsigned := 0;
      Fraction_2 : Long_Long_Unsigned := 0;
      Result     : Long_Long_Float;
      HF         : constant Natural := F_Size / 2;
      S          : SEA (1 .. LLF_L);
      L          : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;
      end if;

      --  Extract Fraction, Sign and Exponent

      for I in LLF_L - F_Bytes + 1 .. LLF_L - 7 loop
         Fraction_1 := Fraction_1 * BB + Long_Long_Unsigned (S (I));
      end loop;

      for I in SEO (LLF_L - 6) .. SEO (LLF_L) loop
         Fraction_2 := Fraction_2 * BB + Long_Long_Unsigned (S (I));
      end loop;

      Result := Long_Long_Float'Scaling (Long_Long_Float (Fraction_2), -HF);
      Result := Long_Long_Float (Fraction_1) + Result;
      Result := Long_Long_Float'Scaling (Result, HF - F_Size);

      if BS <= S (1) then
         Is_Positive := False;
         Exponent := Long_Unsigned (S (1) - BS);
      else
         Is_Positive := True;
         Exponent := Long_Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         Exponent := Exponent * BB + Long_Unsigned (S (N));
      end loop;

      Exponent := Shift_Right (Exponent, Integer (E_Bytes) * SU - E_Size - 1);

      --  NaN or Infinities

      if Integer (Exponent) = E_Last then
         raise Constraint_Error;

      elsif Exponent = 0 then

         --  Signed zeros

         if Fraction_1 = 0 and then Fraction_2 = 0 then
            null;

         --  Denormalized float

         else
            Result := Long_Long_Float'Scaling (Result, 1 - E_Bias);
         end if;

      --  Normalized float

      else
         Result := Long_Long_Float'Scaling
           (1.0 + Result, Integer (Exponent) - E_Bias);
      end if;

      if not Is_Positive then
         Result := -Result;
      end if;

      return Result;
   end I_LLF;

   -----------
   -- I_LLI --
   -----------

   function I_LLI (Stream : not null access RST) return Long_Long_Integer is
      S : XDR_S_LLI;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_LLI_To_Long_Long_Integer (S);

      else
         --  Compute using machine unsigned for computing
         --  rather than long_long_unsigned.

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned

            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         --  Test sign and apply two complement notation

         if S (1) < BL then
            return Long_Long_Integer (X);
         else
            return Long_Long_Integer (-((Long_Long_Unsigned'Last xor X) + 1));
         end if;
      end if;
   end I_LLI;

   -----------
   -- I_LLU --
   -----------

   function I_LLU (Stream : not null access RST) return Long_Long_Unsigned is
      S : XDR_S_LLU;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_LLU_To_Long_Long_Unsigned (S);

      else
         --  Compute using machine unsigned
         --  rather than long_long_unsigned.

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned

            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         return X;
      end if;
   end I_LLU;

   ----------
   -- I_LU --
   ----------

   function I_LU (Stream : not null access RST) return Long_Unsigned is
      S : XDR_S_LU;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return Long_Unsigned (XDR_S_LU_To_Long_Long_Unsigned (S));

      else
         --  Compute using machine unsigned
         --  rather than long_unsigned.

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned

            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         return X;
      end if;
   end I_LU;

   ----------
   -- I_SF --
   ----------

   function I_SF (Stream : not null access RST) return Short_Float is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;

      Exponent    : Long_Unsigned;
      Fraction    : Long_Unsigned;
      Is_Positive : Boolean;
      Result      : Short_Float;
      S           : SEA (1 .. SF_L);
      L           : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;
      end if;

      --  Extract Fraction, Sign and Exponent

      Fraction := Long_Unsigned (S (SF_L + 1 - F_Bytes) and F_Mask);
      for N in SF_L + 2 - F_Bytes .. SF_L loop
         Fraction := Fraction * BB + Long_Unsigned (S (N));
      end loop;
      Result := Short_Float'Scaling (Short_Float (Fraction), -F_Size);

      if BS <= S (1) then
         Is_Positive := False;
         Exponent := Long_Unsigned (S (1) - BS);
      else
         Is_Positive := True;
         Exponent := Long_Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         Exponent := Exponent * BB + Long_Unsigned (S (N));
      end loop;
      Exponent := Shift_Right (Exponent, Integer (E_Bytes) * SU - E_Size - 1);

      --  NaN or Infinities

      if Integer (Exponent) = E_Last then
         raise Constraint_Error;

      elsif Exponent = 0 then

         --  Signed zeros

         if Fraction = 0 then
            null;

         --  Denormalized float

         else
            Result := Short_Float'Scaling (Result, 1 - E_Bias);
         end if;

      --  Normalized float

      else
         Result := Short_Float'Scaling
           (1.0 + Result, Integer (Exponent) - E_Bias);
      end if;

      if not Is_Positive then
         Result := -Result;
      end if;

      return Result;
   end I_SF;

   ----------
   -- I_SI --
   ----------

   function I_SI (Stream : not null access RST) return Short_Integer is
      S : XDR_S_SI;
      L : SEO;
      U : XDR_SU := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_SI_To_Short_Integer (S);

      else
         for N in S'Range loop
            U := U * BB + XDR_SU (S (N));
         end loop;

         --  Test sign and apply two complement notation

         if S (1) < BL then
            return Short_Integer (U);
         else
            return Short_Integer (-((XDR_SU'Last xor U) + 1));
         end if;
      end if;
   end I_SI;

   -----------
   -- I_SSI --
   -----------

   function I_SSI (Stream : not null access RST) return Short_Short_Integer is
      S : XDR_S_SSI;
      L : SEO;
      U : XDR_SSU;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_SSI_To_Short_Short_Integer (S);

      else
         U := XDR_SSU (S (1));

         --  Test sign and apply two complement notation

         if S (1) < BL then
            return Short_Short_Integer (U);
         else
            return Short_Short_Integer (-((XDR_SSU'Last xor U) + 1));
         end if;
      end if;
   end I_SSI;

   -----------
   -- I_SSU --
   -----------

   function I_SSU (Stream : not null access RST) return Short_Short_Unsigned is
      S : XDR_S_SSU;
      L : SEO;
      U : XDR_SSU := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      else
         U := XDR_SSU (S (1));
         return Short_Short_Unsigned (U);
      end if;
   end I_SSU;

   ----------
   -- I_SU --
   ----------

   function I_SU (Stream : not null access RST) return Short_Unsigned is
      S : XDR_S_SU;
      L : SEO;
      U : XDR_SU := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_SU_To_Short_Unsigned (S);

      else
         for N in S'Range loop
            U := U * BB + XDR_SU (S (N));
         end loop;

         return Short_Unsigned (U);
      end if;
   end I_SU;

   ---------
   -- I_U --
   ---------

   function I_U (Stream : not null access RST) return Unsigned is
      S : XDR_S_U;
      L : SEO;
      U : XDR_U := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      elsif Optimize_Integers then
         return XDR_S_U_To_Unsigned (S);

      else
         for N in S'Range loop
            U := U * BB + XDR_U (S (N));
         end loop;

         return Unsigned (U);
      end if;
   end I_U;

   ----------
   -- I_WC --
   ----------

   function I_WC (Stream : not null access RST) return Wide_Character is
      S : XDR_S_WC;
      L : SEO;
      U : XDR_WC := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      else
         for N in S'Range loop
            U := U * BB + XDR_WC (S (N));
         end loop;

         --  Use Ada requirements on Wide_Character representation clause

         return Wide_Character'Val (U);
      end if;
   end I_WC;

   -----------
   -- I_WWC --
   -----------

   function I_WWC (Stream : not null access RST) return Wide_Wide_Character is
      S : XDR_S_WWC;
      L : SEO;
      U : XDR_WWC := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Data_Error;

      else
         for N in S'Range loop
            U := U * BB + XDR_WWC (S (N));
         end loop;

         --  Use Ada requirements on Wide_Wide_Character representation clause

         return Wide_Wide_Character'Val (U);
      end if;
   end I_WWC;

   ----------
   -- W_AD --
   ----------

   procedure W_AD (Stream : not null access RST; Item : Fat_Pointer) is
      S : XDR_S_TM;
      U : XDR_TM;

   begin
      U := XDR_TM (To_XDR_SA (Item.P1));
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      U := XDR_TM (To_XDR_SA (Item.P2));
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Data_Error;
      end if;
   end W_AD;

   ----------
   -- W_AS --
   ----------

   procedure W_AS (Stream : not null access RST; Item : Thin_Pointer) is
      S : XDR_S_TM;
      U : XDR_TM := XDR_TM (To_XDR_SA (Item.P1));

   begin
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Data_Error;
      end if;
   end W_AS;

   ---------
   -- W_B --
   ---------

   procedure W_B (Stream : not null access RST; Item : Boolean) is
   begin
      if Item then
         W_SSU (Stream, 1);
      else
         W_SSU (Stream, 0);
      end if;
   end W_B;

   ---------
   -- W_C --
   ---------

   procedure W_C (Stream : not null access RST; Item : Character) is
      S : XDR_S_C;

      pragma Assert (C_L = 1);

   begin
      --  Use Ada requirements on Character representation clause

      S (1) := SE (Character'Pos (Item));

      Ada.Streams.Write (Stream.all, S);
   end W_C;

   ---------
   -- W_F --
   ---------

   procedure W_F (Stream : not null access RST; Item : Float) is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;

      Exponent    : Long_Unsigned;
      Fraction    : Long_Unsigned;
      Is_Positive : Boolean;
      E           : Integer;
      F           : Float;
      S           : SEA (1 .. F_L) := (others => 0);

   begin
      if not Item'Valid then
         raise Constraint_Error;
      end if;

      --  Compute Sign

      Is_Positive := (0.0 <= Item);
      F := abs (Item);

      --  Signed zero

      if F = 0.0 then
         Exponent := 0;
         Fraction := 0;

      else
         E := Float'Exponent (F) - 1;

         --  Denormalized float

         if E <= -E_Bias then
            F := Float'Scaling (F, F_Size + E_Bias - 1);
            E := -E_Bias;
         else
            F := Float'Scaling (Float'Fraction (F), F_Size + 1);
         end if;

         --  Compute Exponent and Fraction

         Exponent := Long_Unsigned (E + E_Bias);
         Fraction := Long_Unsigned (F * 2.0) / 2;
      end if;

      --  Store Fraction

      for I in reverse F_L - F_Bytes + 1 .. F_L loop
         S (I) := SE (Fraction mod BB);
         Fraction := Fraction / BB;
      end loop;

      --  Remove implicit bit

      S (F_L - F_Bytes + 1) := S (F_L - F_Bytes + 1) and F_Mask;

      --  Store Exponent (not always at the beginning of a byte)

      Exponent := Shift_Left (Exponent, Integer (E_Bytes) * SU - E_Size - 1);
      for N in reverse 1 .. E_Bytes loop
         S (N) := SE (Exponent mod BB) + S (N);
         Exponent := Exponent / BB;
      end loop;

      --  Store Sign

      if not Is_Positive then
         S (1) := S (1) + BS;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_F;

   ---------
   -- W_I --
   ---------

   procedure W_I (Stream : not null access RST; Item : Integer) is
      S : XDR_S_I;
      U : XDR_U;

   begin
      if Optimize_Integers then
         S := Integer_To_XDR_S_I (Item);

      else
         --  Test sign and apply two complement notation

         U := (if Item < 0
               then XDR_U'Last xor XDR_U (-(Item + 1))
               else XDR_U (Item));

         for N in reverse S'Range loop
            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_I;

   ----------
   -- W_LF --
   ----------

   procedure W_LF (Stream : not null access RST; Item : Long_Float) is
      I       : constant Precision := Double;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;

      Exponent    : Long_Unsigned;
      Fraction    : Long_Long_Unsigned;
      Is_Positive : Boolean;
      E           : Integer;
      F           : Long_Float;
      S           : SEA (1 .. LF_L) := (others => 0);

   begin
      if not Item'Valid then
         raise Constraint_Error;
      end if;

      --  Compute Sign

      Is_Positive := (0.0 <= Item);
      F := abs (Item);

      --  Signed zero

      if F = 0.0 then
         Exponent := 0;
         Fraction := 0;

      else
         E := Long_Float'Exponent (F) - 1;

         --  Denormalized float

         if E <= -E_Bias then
            E := -E_Bias;
            F := Long_Float'Scaling (F, F_Size + E_Bias - 1);
         else
            F := Long_Float'Scaling (F, F_Size - E);
         end if;

         --  Compute Exponent and Fraction

         Exponent := Long_Unsigned (E + E_Bias);
         Fraction := Long_Long_Unsigned (F * 2.0) / 2;
      end if;

      --  Store Fraction

      for I in reverse LF_L - F_Bytes + 1 .. LF_L loop
         S (I) := SE (Fraction mod BB);
         Fraction := Fraction / BB;
      end loop;

      --  Remove implicit bit

      S (LF_L - F_Bytes + 1) := S (LF_L - F_Bytes + 1) and F_Mask;

      --  Store Exponent (not always at the beginning of a byte)

      Exponent := Shift_Left (Exponent, Integer (E_Bytes) * SU - E_Size - 1);
      for N in reverse 1 .. E_Bytes loop
         S (N) := SE (Exponent mod BB) + S (N);
         Exponent := Exponent / BB;
      end loop;

      --  Store Sign

      if not Is_Positive then
         S (1) := S (1) + BS;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_LF;

   ----------
   -- W_LI --
   ----------

   procedure W_LI (Stream : not null access RST; Item : Long_Integer) is
      S : XDR_S_LI;
      U : Unsigned;
      X : Long_Unsigned;

   begin
      if Optimize_Integers then
         S := Long_Long_Integer_To_XDR_S_LI (Long_Long_Integer (Item));

      else
         --  Test sign and apply two complement notation

         if Item < 0 then
            X := Long_Unsigned'Last xor Long_Unsigned (-(Item + 1));
         else
            X := Long_Unsigned (Item);
         end if;

         --  Compute using machine unsigned rather than long_unsigned

         for N in reverse S'Range loop

            --  We have filled an unsigned

            if (LU_L - N) mod UB = 0 then
               U := Unsigned (X and UL);
               X := Shift_Right (X, US);
            end if;

            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_LI;

   -----------
   -- W_LLF --
   -----------

   procedure W_LLF (Stream : not null access RST; Item : Long_Long_Float) is
      I       : constant Precision := Quadruple;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;

      HFS : constant Integer := F_Size / 2;

      Exponent    : Long_Unsigned;
      Fraction_1  : Long_Long_Unsigned;
      Fraction_2  : Long_Long_Unsigned;
      Is_Positive : Boolean;
      E           : Integer;
      F           : Long_Long_Float := Item;
      S           : SEA (1 .. LLF_L) := (others => 0);

   begin
      if not Item'Valid then
         raise Constraint_Error;
      end if;

      --  Compute Sign

      Is_Positive := (0.0 <= Item);

      if F < 0.0 then
         F := -Item;
      end if;

      --  Signed zero

      if F = 0.0 then
         Exponent   := 0;
         Fraction_1 := 0;
         Fraction_2 := 0;

      else
         E := Long_Long_Float'Exponent (F) - 1;

         --  Denormalized float

         if E <= -E_Bias then
            F := Long_Long_Float'Scaling (F, E_Bias - 1);
            E := -E_Bias;
         else
            F := Long_Long_Float'Scaling
              (Long_Long_Float'Fraction (F), 1);
         end if;

         --  Compute Exponent and Fraction

         Exponent   := Long_Unsigned (E + E_Bias);
         F          := Long_Long_Float'Scaling (F, F_Size - HFS);
         Fraction_1 := Long_Long_Unsigned (Long_Long_Float'Floor (F));
         F          := F - Long_Long_Float (Fraction_1);
         F          := Long_Long_Float'Scaling (F, HFS);
         Fraction_2 := Long_Long_Unsigned (Long_Long_Float'Floor (F));
      end if;

      --  Store Fraction_1

      for I in reverse LLF_L - F_Bytes + 1 .. LLF_L - 7 loop
         S (I) := SE (Fraction_1 mod BB);
         Fraction_1 := Fraction_1 / BB;
      end loop;

      --  Store Fraction_2

      for I in reverse LLF_L - 6 .. LLF_L loop
         S (SEO (I)) := SE (Fraction_2 mod BB);
         Fraction_2 := Fraction_2 / BB;
      end loop;

      --  Store Exponent (not always at the beginning of a byte)

      Exponent := Shift_Left (Exponent, Integer (E_Bytes) * SU - E_Size - 1);
      for N in reverse 1 .. E_Bytes loop
         S (N) := SE (Exponent mod BB) + S (N);
         Exponent := Exponent / BB;
      end loop;

      --  Store Sign

      if not Is_Positive then
         S (1) := S (1) + BS;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_LLF;

   -----------
   -- W_LLI --
   -----------

   procedure W_LLI
     (Stream : not null access RST;
      Item   : Long_Long_Integer)
   is
      S : XDR_S_LLI;
      U : Unsigned;
      X : Long_Long_Unsigned;

   begin
      if Optimize_Integers then
         S := Long_Long_Integer_To_XDR_S_LLI (Item);

      else
         --  Test sign and apply two complement notation

         if Item < 0 then
            X := Long_Long_Unsigned'Last xor Long_Long_Unsigned (-(Item + 1));
         else
            X := Long_Long_Unsigned (Item);
         end if;

         --  Compute using machine unsigned rather than long_long_unsigned

         for N in reverse S'Range loop

            --  We have filled an unsigned

            if (LLU_L - N) mod UB = 0 then
               U := Unsigned (X and UL);
               X := Shift_Right (X, US);
            end if;

            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_LLI;

   -----------
   -- W_LLU --
   -----------

   procedure W_LLU
     (Stream : not null access RST;
      Item   : Long_Long_Unsigned)
   is
      S : XDR_S_LLU;
      U : Unsigned;
      X : Long_Long_Unsigned := Item;

   begin
      if Optimize_Integers then
         S := Long_Long_Unsigned_To_XDR_S_LLU (Item);

      else
         --  Compute using machine unsigned rather than long_long_unsigned

         for N in reverse S'Range loop

            --  We have filled an unsigned

            if (LLU_L - N) mod UB = 0 then
               U := Unsigned (X and UL);
               X := Shift_Right (X, US);
            end if;

            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_LLU;

   ----------
   -- W_LU --
   ----------

   procedure W_LU (Stream : not null access RST; Item : Long_Unsigned) is
      S : XDR_S_LU;
      U : Unsigned;
      X : Long_Unsigned := Item;

   begin
      if Optimize_Integers then
         S := Long_Long_Unsigned_To_XDR_S_LU (Long_Long_Unsigned (Item));

      else
         --  Compute using machine unsigned rather than long_unsigned

         for N in reverse S'Range loop

            --  We have filled an unsigned

            if (LU_L - N) mod UB = 0 then
               U := Unsigned (X and UL);
               X := Shift_Right (X, US);
            end if;
            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_LU;

   ----------
   -- W_SF --
   ----------

   procedure W_SF (Stream : not null access RST; Item : Short_Float) is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;

      Exponent    : Long_Unsigned;
      Fraction    : Long_Unsigned;
      Is_Positive : Boolean;
      E           : Integer;
      F           : Short_Float;
      S           : SEA (1 .. SF_L) := (others => 0);

   begin
      if not Item'Valid then
         raise Constraint_Error;
      end if;

      --  Compute Sign

      Is_Positive := (0.0 <= Item);
      F := abs (Item);

      --  Signed zero

      if F = 0.0 then
         Exponent := 0;
         Fraction := 0;

      else
         E := Short_Float'Exponent (F) - 1;

         --  Denormalized float

         if E <= -E_Bias then
            E := -E_Bias;
            F := Short_Float'Scaling (F, F_Size + E_Bias - 1);
         else
            F := Short_Float'Scaling (F, F_Size - E);
         end if;

         --  Compute Exponent and Fraction

         Exponent := Long_Unsigned (E + E_Bias);
         Fraction := Long_Unsigned (F * 2.0) / 2;
      end if;

      --  Store Fraction

      for I in reverse SF_L - F_Bytes + 1 .. SF_L loop
         S (I) := SE (Fraction mod BB);
         Fraction := Fraction / BB;
      end loop;

      --  Remove implicit bit

      S (SF_L - F_Bytes + 1) := S (SF_L - F_Bytes + 1) and F_Mask;

      --  Store Exponent (not always at the beginning of a byte)

      Exponent := Shift_Left (Exponent, Integer (E_Bytes) * SU - E_Size - 1);
      for N in reverse 1 .. E_Bytes loop
         S (N) := SE (Exponent mod BB) + S (N);
         Exponent := Exponent / BB;
      end loop;

      --  Store Sign

      if not Is_Positive then
         S (1) := S (1) + BS;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_SF;

   ----------
   -- W_SI --
   ----------

   procedure W_SI (Stream : not null access RST; Item : Short_Integer) is
      S : XDR_S_SI;
      U : XDR_SU;

   begin
      if Optimize_Integers then
         S := Short_Integer_To_XDR_S_SI (Item);

      else
         --  Test sign and apply two complement's notation

         U := (if Item < 0
               then XDR_SU'Last xor XDR_SU (-(Item + 1))
               else XDR_SU (Item));

         for N in reverse S'Range loop
            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_SI;

   -----------
   -- W_SSI --
   -----------

   procedure W_SSI
     (Stream : not null access RST;
      Item   : Short_Short_Integer)
   is
      S : XDR_S_SSI;
      U : XDR_SSU;

   begin
      if Optimize_Integers then
         S := Short_Short_Integer_To_XDR_S_SSI (Item);

      else
         --  Test sign and apply two complement's notation

         U := (if Item < 0
               then XDR_SSU'Last xor XDR_SSU (-(Item + 1))
               else XDR_SSU (Item));

         S (1) := SE (U);
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_SSI;

   -----------
   -- W_SSU --
   -----------

   procedure W_SSU
     (Stream : not null access RST;
      Item   : Short_Short_Unsigned)
   is
      U : constant XDR_SSU := XDR_SSU (Item);
      S : XDR_S_SSU;

   begin
      S (1) := SE (U);
      Ada.Streams.Write (Stream.all, S);
   end W_SSU;

   ----------
   -- W_SU --
   ----------

   procedure W_SU (Stream : not null access RST; Item : Short_Unsigned) is
      S : XDR_S_SU;
      U : XDR_SU := XDR_SU (Item);

   begin
      if Optimize_Integers then
         S := Short_Unsigned_To_XDR_S_SU (Item);

      else
         for N in reverse S'Range loop
            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_SU;

   ---------
   -- W_U --
   ---------

   procedure W_U (Stream : not null access RST; Item : Unsigned) is
      S : XDR_S_U;
      U : XDR_U := XDR_U (Item);

   begin
      if Optimize_Integers then
         S := Unsigned_To_XDR_S_U (Item);

      else
         for N in reverse S'Range loop
            S (N) := SE (U mod BB);
            U := U / BB;
         end loop;

         if U /= 0 then
            raise Data_Error;
         end if;
      end if;

      Ada.Streams.Write (Stream.all, S);
   end W_U;

   ----------
   -- W_WC --
   ----------

   procedure W_WC (Stream : not null access RST; Item : Wide_Character) is
      S : XDR_S_WC;
      U : XDR_WC;

   begin
      --  Use Ada requirements on Wide_Character representation clause

      U := XDR_WC (Wide_Character'Pos (Item));

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Data_Error;
      end if;
   end W_WC;

   -----------
   -- W_WWC --
   -----------

   procedure W_WWC
     (Stream : not null access RST; Item : Wide_Wide_Character)
   is
      S : XDR_S_WWC;
      U : XDR_WWC;

   begin
      --  Use Ada requirements on Wide_Wide_Character representation clause

      U := XDR_WWC (Wide_Wide_Character'Pos (Item));

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Data_Error;
      end if;
   end W_WWC;

end System.Stream_Attributes;
