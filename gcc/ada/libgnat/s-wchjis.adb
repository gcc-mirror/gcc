------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ J I S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

package body System.WCh_JIS is

   type Byte is mod 256;

   EUC_Hankaku_Kana : constant Byte := 16#8E#;
   --  Prefix byte in EUC for Hankaku Kana (small Katakana). Such characters
   --  in EUC are represented by a prefix byte followed by the code, which
   --  is in the upper half (the corresponding JIS internal code is in the
   --  range 16#0080# - 16#00FF#).

   function EUC_To_JIS (EUC1, EUC2 : Character) return Wide_Character is
      EUC1B  : constant Byte := Character'Pos (EUC1);
      EUC2B  : constant Byte := Character'Pos (EUC2);

   begin
      if EUC2B not in 16#A0# .. 16#FE# then
         raise Constraint_Error;
      end if;

      if EUC1B = EUC_Hankaku_Kana then
         return Wide_Character'Val (EUC2B);

      else
         if EUC1B not in 16#A0# .. 16#FE# then
            raise Constraint_Error;
         else
            return Wide_Character'Val
              (256 * Natural (EUC1B and 16#7F#) + Natural (EUC2B and 16#7F#));
         end if;
      end if;
   end EUC_To_JIS;

   ----------------
   -- JIS_To_EUC --
   ----------------

   procedure JIS_To_EUC
     (J    : Wide_Character;
      EUC1 : out Character;
      EUC2 : out Character)
   is
      JIS1 : constant Natural := Wide_Character'Pos (J) / 256;
      JIS2 : constant Natural := Wide_Character'Pos (J) rem 256;

   begin
      --  Special case of small Katakana

      if JIS1 = 0 then

         --  The value must be in the range 16#80# to 16#FF# so that the upper
         --  bit is set in both bytes.

         if JIS2 < 16#80# then
            raise Constraint_Error;
         end if;

         EUC1 := Character'Val (EUC_Hankaku_Kana);
         EUC2 := Character'Val (JIS2);

      --  The upper bit of both characters must be clear, or this is not
      --  a valid character for representation in EUC form.

      elsif JIS1 > 16#7F# or else JIS2 > 16#7F# then
         raise Constraint_Error;

      --  Result is just the two characters with upper bits set

      else
         EUC1 := Character'Val (JIS1 + 16#80#);
         EUC2 := Character'Val (JIS2 + 16#80#);
      end if;
   end JIS_To_EUC;

   ----------------------
   -- JIS_To_Shift_JIS --
   ----------------------

   procedure JIS_To_Shift_JIS
     (J   : Wide_Character;
      SJ1 : out Character;
      SJ2 : out Character)
   is
      JIS1 : Byte;
      JIS2 : Byte;

   begin
      --  The following is the required algorithm, it's hard to make any
      --  more intelligent comments. This was copied from a public domain
      --  C program called etos.c (author unknown).

      JIS1 := Byte (Natural (Wide_Character'Pos (J) / 256));
      JIS2 := Byte (Natural (Wide_Character'Pos (J) rem 256));

      if JIS1 > 16#5F# then
         JIS1 := JIS1 + 16#80#;
      end if;

      if (JIS1 mod 2) = 0 then
         SJ1 := Character'Val ((JIS1 - 16#30#) / 2 + 16#88#);
         SJ2 := Character'Val (JIS2 + 16#7E#);

      else
         if JIS2 >= 16#60# then
            JIS2 := JIS2 + 16#01#;
         end if;

         SJ1 := Character'Val ((JIS1 - 16#31#) / 2 + 16#89#);
         SJ2 := Character'Val (JIS2 + 16#1F#);
      end if;
   end JIS_To_Shift_JIS;

   ----------------------
   -- Shift_JIS_To_JIS --
   ----------------------

   function Shift_JIS_To_JIS (SJ1, SJ2 : Character) return Wide_Character is
      SJIS1 : Byte;
      SJIS2 : Byte;
      JIS1  : Byte;
      JIS2  : Byte;

   begin
      --  The following is the required algorithm, it's hard to make any
      --  more intelligent comments. This was copied from a public domain
      --  C program called stoj.c written by shige@csk.JUNET.

      SJIS1 := Character'Pos (SJ1);
      SJIS2 := Character'Pos (SJ2);

      if SJIS1 >= 16#E0# then
         SJIS1 := SJIS1 - 16#40#;
      end if;

      if SJIS2 >= 16#9F# then
         JIS1 := (SJIS1 - 16#88#) * 2 + 16#30#;
         JIS2 := SJIS2 - 16#7E#;

      else
         if SJIS2 >= 16#7F# then
            SJIS2 := SJIS2 - 16#01#;
         end if;

         JIS1 := (SJIS1 - 16#89#) * 2 + 16#31#;
         JIS2 := SJIS2 - 16#1F#;
      end if;

      if JIS1 not in 16#20# .. 16#7E#
        or else JIS2 not in 16#20# .. 16#7E#
      then
         raise Constraint_Error;
      else
         return Wide_Character'Val (256 * Natural (JIS1) + Natural (JIS2));
      end if;
   end Shift_JIS_To_JIS;

end System.WCh_JIS;
