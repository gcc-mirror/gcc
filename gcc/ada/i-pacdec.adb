------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            I N T E R F A C E S . P A C K E D _ D E C I M A L             --
--                                                                          --
--                                 B o d y                                  --
--            (Version for IBM Mainframe Packed Decimal Format)             --
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

with System;                  use System;
with Unchecked_Conversion;

package body Interfaces.Packed_Decimal is

   type Packed is array (Byte_Length) of Unsigned_8;
   --  The type used internally to represent packed decimal

   type Packed_Ptr is access Packed;
   function To_Packed_Ptr is new Unchecked_Conversion (Address, Packed_Ptr);

   --  The following array is used to convert a value in the range 0-99 to
   --  a packed decimal format with two hexadecimal nibbles. It is worth
   --  using table look up in this direction because divides are expensive.

   Packed_Byte : constant array (00 .. 99) of Unsigned_8 :=
      (16#00#, 16#01#, 16#02#, 16#03#, 16#04#,
       16#05#, 16#06#, 16#07#, 16#08#, 16#09#,
       16#10#, 16#11#, 16#12#, 16#13#, 16#14#,
       16#15#, 16#16#, 16#17#, 16#18#, 16#19#,
       16#20#, 16#21#, 16#22#, 16#23#, 16#24#,
       16#25#, 16#26#, 16#27#, 16#28#, 16#29#,
       16#30#, 16#31#, 16#32#, 16#33#, 16#34#,
       16#35#, 16#36#, 16#37#, 16#38#, 16#39#,
       16#40#, 16#41#, 16#42#, 16#43#, 16#44#,
       16#45#, 16#46#, 16#47#, 16#48#, 16#49#,
       16#50#, 16#51#, 16#52#, 16#53#, 16#54#,
       16#55#, 16#56#, 16#57#, 16#58#, 16#59#,
       16#60#, 16#61#, 16#62#, 16#63#, 16#64#,
       16#65#, 16#66#, 16#67#, 16#68#, 16#69#,
       16#70#, 16#71#, 16#72#, 16#73#, 16#74#,
       16#75#, 16#76#, 16#77#, 16#78#, 16#79#,
       16#80#, 16#81#, 16#82#, 16#83#, 16#84#,
       16#85#, 16#86#, 16#87#, 16#88#, 16#89#,
       16#90#, 16#91#, 16#92#, 16#93#, 16#94#,
       16#95#, 16#96#, 16#97#, 16#98#, 16#99#);

   ---------------------
   -- Int32_To_Packed --
   ---------------------

   procedure Int32_To_Packed (V : Integer_32; P : System.Address; D : D32) is
      PP           : constant Packed_Ptr  := To_Packed_Ptr (P);
      Empty_Nibble : constant Boolean     := ((D rem 2) = 0);
      B            : constant Byte_Length := (D / 2) + 1;
      VV           : Integer_32 := V;

   begin
      --  Deal with sign byte first

      if VV >= 0 then
         PP (B) := Unsigned_8 (VV rem 10) * 16 + 16#C#;
         VV := VV / 10;

      else
         VV := -VV;
         PP (B) := Unsigned_8 (VV rem 10) * 16 + 16#D#;
      end if;

      for J in reverse B - 1 .. 2 loop
         if VV = 0 then
            for K in 1 .. J loop
               PP (K) := 16#00#;
            end loop;

            return;

         else
            PP (J) := Packed_Byte (Integer (VV rem 100));
            VV := VV / 100;
         end if;
      end loop;

      --  Deal with leading byte

      if Empty_Nibble then
         if VV > 9 then
            raise Constraint_Error;
         else
            PP (1) := Unsigned_8 (VV);
         end if;

      else
         if VV > 99 then
            raise Constraint_Error;
         else
            PP (1) := Packed_Byte (Integer (VV));
         end if;
      end if;

   end Int32_To_Packed;

   ---------------------
   -- Int64_To_Packed --
   ---------------------

   procedure Int64_To_Packed (V : Integer_64; P : System.Address; D : D64) is
      PP           : constant Packed_Ptr  := To_Packed_Ptr (P);
      Empty_Nibble : constant Boolean     := ((D rem 2) = 0);
      B            : constant Byte_Length := (D / 2) + 1;
      VV           : Integer_64 := V;

   begin
      --  Deal with sign byte first

      if VV >= 0 then
         PP (B) := Unsigned_8 (VV rem 10) * 16 + 16#C#;
         VV := VV / 10;

      else
         VV := -VV;
         PP (B) := Unsigned_8 (VV rem 10) * 16 + 16#D#;
      end if;

      for J in reverse B - 1 .. 2 loop
         if VV = 0 then
            for K in 1 .. J loop
               PP (K) := 16#00#;
            end loop;

            return;

         else
            PP (J) := Packed_Byte (Integer (VV rem 100));
            VV := VV / 100;
         end if;
      end loop;

      --  Deal with leading byte

      if Empty_Nibble then
         if VV > 9 then
            raise Constraint_Error;
         else
            PP (1) := Unsigned_8 (VV);
         end if;

      else
         if VV > 99 then
            raise Constraint_Error;
         else
            PP (1) := Packed_Byte (Integer (VV));
         end if;
      end if;

   end Int64_To_Packed;

   ---------------------
   -- Packed_To_Int32 --
   ---------------------

   function Packed_To_Int32 (P : System.Address; D : D32) return Integer_32 is
      PP           : constant Packed_Ptr  := To_Packed_Ptr (P);
      Empty_Nibble : constant Boolean     := ((D mod 2) = 0);
      B            : constant Byte_Length := (D / 2) + 1;
      V            : Integer_32;
      Dig          : Unsigned_8;
      Sign         : Unsigned_8;
      J            : Positive;

   begin
      --  Cases where there is an unused (zero) nibble in the first byte.
      --  Deal with the single digit nibble at the right of this byte

      if Empty_Nibble then
         V := Integer_32 (PP (1));
         J := 2;

         if V > 9 then
            raise Constraint_Error;
         end if;

      --  Cases where all nibbles are used

      else
         V := 0;
         J := 1;
      end if;

      --  Loop to process bytes containing two digit nibbles

      while J < B loop
         Dig := Shift_Right (PP (J), 4);

         if Dig > 9 then
            raise Constraint_Error;
         else
            V := V * 10 + Integer_32 (Dig);
         end if;

         Dig := PP (J) and 16#0F#;

         if Dig > 9 then
            raise Constraint_Error;
         else
            V := V * 10 + Integer_32 (Dig);
         end if;

         J := J + 1;
      end loop;

      --  Deal with digit nibble in sign byte

      Dig := Shift_Right (PP (J), 4);

      if Dig > 9 then
         raise Constraint_Error;
      else
         V := V * 10 + Integer_32 (Dig);
      end if;

      Sign :=  PP (J) and 16#0F#;

      --  Process sign nibble (deal with most common cases first)

      if Sign = 16#C# then
         return V;

      elsif Sign = 16#D# then
         return -V;

      elsif Sign = 16#B# then
         return -V;

      elsif Sign >= 16#A# then
         return V;

      else
         raise Constraint_Error;
      end if;
   end Packed_To_Int32;

   ---------------------
   -- Packed_To_Int64 --
   ---------------------

   function Packed_To_Int64 (P : System.Address; D : D64) return Integer_64 is
      PP           : constant Packed_Ptr  := To_Packed_Ptr (P);
      Empty_Nibble : constant Boolean     := ((D mod 2) = 0);
      B            : constant Byte_Length := (D / 2) + 1;
      V            : Integer_64;
      Dig          : Unsigned_8;
      Sign         : Unsigned_8;
      J            : Positive;

   begin
      --  Cases where there is an unused (zero) nibble in the first byte.
      --  Deal with the single digit nibble at the right of this byte

      if Empty_Nibble then
         V := Integer_64 (PP (1));
         J := 2;

         if V > 9 then
            raise Constraint_Error;
         end if;

      --  Cases where all nibbles are used

      else
         J := 1;
         V := 0;
      end if;

      --  Loop to process bytes containing two digit nibbles

      while J < B loop
         Dig := Shift_Right (PP (J), 4);

         if Dig > 9 then
            raise Constraint_Error;
         else
            V := V * 10 + Integer_64 (Dig);
         end if;

         Dig := PP (J) and 16#0F#;

         if Dig > 9 then
            raise Constraint_Error;
         else
            V := V * 10 + Integer_64 (Dig);
         end if;

         J := J + 1;
      end loop;

      --  Deal with digit nibble in sign byte

      Dig := Shift_Right (PP (J), 4);

      if Dig > 9 then
         raise Constraint_Error;
      else
         V := V * 10 + Integer_64 (Dig);
      end if;

      Sign :=  PP (J) and 16#0F#;

      --  Process sign nibble (deal with most common cases first)

      if Sign = 16#C# then
         return V;

      elsif Sign = 16#D# then
         return -V;

      elsif Sign = 16#B# then
         return -V;

      elsif Sign >= 16#A# then
         return V;

      else
         raise Constraint_Error;
      end if;
   end Packed_To_Int64;

end Interfaces.Packed_Decimal;
