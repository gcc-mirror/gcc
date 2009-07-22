------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--                  S Y S T E M . S C A L A R _ V A L U E S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2009, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

package body System.Scalar_Values is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Mode1 : Character; Mode2 : Character) is
      C1 : Character := Mode1;
      C2 : Character := Mode2;

      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "__gnat_getenv");

      subtype String2 is String (1 .. 2);
      type String2_Ptr is access all String2;

      Env_Value_Ptr    : aliased String2_Ptr;
      Env_Value_Length : aliased Integer;

      EV_Val : aliased constant String :=
                 "GNAT_INIT_SCALARS" & ASCII.NUL;

      B : Byte1;

      EFloat : constant Boolean := Long_Long_Float'Size > Long_Float'Size;
      --  Set True if we are on an x86 with 96-bit floats for extended

      AFloat : constant Boolean :=
                 Long_Float'Size = 48 and then Long_Long_Float'Size = 48;
      --  Set True if we are on an AAMP with 48-bit extended floating point

      type ByteLF is array (0 .. 7 - 2 * Boolean'Pos (AFloat)) of Byte1;

      for ByteLF'Component_Size use 8;

      --  Type used to hold Long_Float values on all targets and to initialize
      --  48-bit Long_Float values used on AAMP. On AAMP, this type is 6 bytes.
      --  On other targets the type is 8 bytes, and type Byte8 is used for
      --  values that are then converted to ByteLF.

      pragma Warnings (Off); --  why ???
      function To_ByteLF is new Ada.Unchecked_Conversion (Byte8, ByteLF);
      pragma Warnings (On);

      type ByteLLF is
        array (0 .. 7 + 4 * Boolean'Pos (EFloat) - 2 * Boolean'Pos (AFloat))
          of Byte1;

      for ByteLLF'Component_Size use 8;

      --  Type used to initialize Long_Long_Float values used on x86 and
      --  any other target with the same 80-bit floating-point values that
      --  GCC always stores in 96-bits. Note that we are assuming Intel
      --  format little-endian addressing for this type. On non-Intel
      --  architectures, this is the same length as Byte8 and holds
      --  a Long_Float value.

      --  The following variables are used to initialize the float values
      --  by overlay. We can't assign directly to the float values, since
      --  we may be assigning signalling Nan's that will cause a trap if
      --  loaded into a floating-point register.

      IV_Isf : aliased Byte4;     -- Initialize short float
      IV_Ifl : aliased Byte4;     -- Initialize float
      IV_Ilf : aliased ByteLF;    -- Initialize long float
      IV_Ill : aliased ByteLLF;   -- Initialize long long float

      for IV_Isf'Address use IS_Isf'Address;
      for IV_Ifl'Address use IS_Ifl'Address;
      for IV_Ilf'Address use IS_Ilf'Address;
      for IV_Ill'Address use IS_Ill'Address;

      --  The following pragmas are used to suppress initialization

      pragma Import (Ada, IV_Isf);
      pragma Import (Ada, IV_Ifl);
      pragma Import (Ada, IV_Ilf);
      pragma Import (Ada, IV_Ill);

   begin
      --  Acquire environment variable value if necessary

      if C1 = 'E' and then C2 = 'V' then
         Get_Env_Value_Ptr
           (EV_Val'Address, Env_Value_Length'Address, Env_Value_Ptr'Address);

         --  Ignore if length is not 2

         if Env_Value_Length /= 2 then
            C1 := 'I';
            C2 := 'N';

         --  Length is 2, see if it is a valid value

         else
            --  Acquire two characters and fold to upper case

            C1 := Env_Value_Ptr (1);
            C2 := Env_Value_Ptr (2);

            if C1 in 'a' .. 'z' then
               C1 := Character'Val (Character'Pos (C1) - 32);
            end if;

            if C2 in 'a' .. 'z' then
               C2 := Character'Val (Character'Pos (C2) - 32);
            end if;

            --  IN/LO/HI are ok values

            if (C1 = 'I' and then C2 = 'N')
                  or else
               (C1 = 'L' and then C2 = 'O')
                  or else
               (C1 = 'H' and then C2 = 'I')
            then
               null;

            --  Try for valid hex digits

            elsif (C1 in '0' .. '9' or else C1 in 'A' .. 'Z')
                     or else
                  (C2 in '0' .. '9' or else C2 in 'A' .. 'Z')
            then
               null;

            --  Otherwise environment value is bad, ignore and use IN (invalid)

            else
               C1 := 'I';
               C2 := 'N';
            end if;
         end if;
      end if;

      --  IN (invalid value)

      if C1 = 'I' and then C2 = 'N' then
         IS_Is1 := 16#80#;
         IS_Is2 := 16#8000#;
         IS_Is4 := 16#8000_0000#;
         IS_Is8 := 16#8000_0000_0000_0000#;

         IS_Iu1 := 16#FF#;
         IS_Iu2 := 16#FFFF#;
         IS_Iu4 := 16#FFFF_FFFF#;
         IS_Iu8 := 16#FFFF_FFFF_FFFF_FFFF#;

         IS_Iz1 := 16#00#;
         IS_Iz2 := 16#0000#;
         IS_Iz4 := 16#0000_0000#;
         IS_Iz8 := 16#0000_0000_0000_0000#;

         if AFloat then
            IV_Isf := 16#FFFF_FF00#;
            IV_Ifl := 16#FFFF_FF00#;
            IV_Ilf := (0, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#);

         else
            IV_Isf := IS_Iu4;
            IV_Ifl := IS_Iu4;
            IV_Ilf := To_ByteLF (IS_Iu8);
         end if;

         if EFloat then
            IV_Ill := (0, 0, 0, 0, 0, 0, 0, 16#C0#, 16#FF#, 16#FF#, 0, 0);
         end if;

      --  LO (Low values)

      elsif C1 = 'L' and then C2 = 'O' then
         IS_Is1 := 16#80#;
         IS_Is2 := 16#8000#;
         IS_Is4 := 16#8000_0000#;
         IS_Is8 := 16#8000_0000_0000_0000#;

         IS_Iu1 := 16#00#;
         IS_Iu2 := 16#0000#;
         IS_Iu4 := 16#0000_0000#;
         IS_Iu8 := 16#0000_0000_0000_0000#;

         IS_Iz1 := 16#00#;
         IS_Iz2 := 16#0000#;
         IS_Iz4 := 16#0000_0000#;
         IS_Iz8 := 16#0000_0000_0000_0000#;

         if AFloat then
            IV_Isf := 16#0000_0001#;
            IV_Ifl := 16#0000_0001#;
            IV_Ilf := (1, 0, 0, 0, 0, 0);

         else
            IV_Isf := 16#FF80_0000#;
            IV_Ifl := 16#FF80_0000#;
            IV_Ilf := To_ByteLF (16#FFF0_0000_0000_0000#);
         end if;

         if EFloat then
            IV_Ill := (0, 0, 0, 0, 0, 0, 0, 16#80#, 16#FF#, 16#FF#, 0, 0);
         end if;

      --  HI (High values)

      elsif C1 = 'H' and then C2 = 'I' then
         IS_Is1 := 16#7F#;
         IS_Is2 := 16#7FFF#;
         IS_Is4 := 16#7FFF_FFFF#;
         IS_Is8 := 16#7FFF_FFFF_FFFF_FFFF#;

         IS_Iu1 := 16#FF#;
         IS_Iu2 := 16#FFFF#;
         IS_Iu4 := 16#FFFF_FFFF#;
         IS_Iu8 := 16#FFFF_FFFF_FFFF_FFFF#;

         IS_Iz1 := 16#FF#;
         IS_Iz2 := 16#FFFF#;
         IS_Iz4 := 16#FFFF_FFFF#;
         IS_Iz8 := 16#FFFF_FFFF_FFFF_FFFF#;

         if AFloat then
            IV_Isf := 16#7FFF_FFFF#;
            IV_Ifl := 16#7FFF_FFFF#;
            IV_Ilf := (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#7F#);

         else
            IV_Isf := 16#7F80_0000#;
            IV_Ifl := 16#7F80_0000#;
            IV_Ilf := To_ByteLF (16#7FF0_0000_0000_0000#);
         end if;

         if EFloat then
            IV_Ill := (0, 0, 0, 0, 0, 0, 0, 16#80#, 16#FF#, 16#7F#, 0, 0);
         end if;

      --  -Shh (hex byte)

      else
         --  Convert the two hex digits (we know they are valid here)

         B := 16 * (Character'Pos (C1)
                     - (if C1 in '0' .. '9'
                        then Character'Pos ('0')
                        else Character'Pos ('A') - 10))
                 + (Character'Pos (C2)
                     - (if C2 in '0' .. '9'
                        then Character'Pos ('0')
                        else Character'Pos ('A') - 10));

         --  Initialize data values from the hex value

         IS_Is1 := B;
         IS_Is2 := 2**8  * Byte2 (IS_Is1) + Byte2 (IS_Is1);
         IS_Is4 := 2**16 * Byte4 (IS_Is2) + Byte4 (IS_Is2);
         IS_Is8 := 2**32 * Byte8 (IS_Is4) + Byte8 (IS_Is4);

         IS_Iu1 := IS_Is1;
         IS_Iu2 := IS_Is2;
         IS_Iu4 := IS_Is4;
         IS_Iu8 := IS_Is8;

         IS_Iz1 := IS_Is1;
         IS_Iz2 := IS_Is2;
         IS_Iz4 := IS_Is4;
         IS_Iz8 := IS_Is8;

         IV_Isf := IS_Is4;
         IV_Ifl := IS_Is4;

         if AFloat then
            IV_Ill := (B, B, B, B, B, B);
         else
            IV_Ilf := To_ByteLF (IS_Is8);
         end if;

         if EFloat then
            IV_Ill := (B, B, B, B, B, B, B, B, B, B, B, B);
         end if;
      end if;

      --  If no separate Long_Long_Float, then use Long_Float value as
      --  Long_Long_Float initial value.

      if not EFloat then
         declare
            pragma Warnings (Off);  -- why???
            function To_ByteLLF is
              new Ada.Unchecked_Conversion (ByteLF, ByteLLF);
            pragma Warnings (On);
         begin
            IV_Ill := To_ByteLLF (IV_Ilf);
         end;
      end if;
   end Initialize;

end System.Scalar_Values;
