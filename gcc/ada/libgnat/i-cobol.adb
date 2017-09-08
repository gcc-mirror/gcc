------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     I N T E R F A C E S . C O B O L                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  The body of Interfaces.COBOL is implementation independent (i.e. the same
--  version is used with all versions of GNAT). The specialization to a
--  particular COBOL format is completely contained in the private part of
--  the spec.

with Interfaces; use Interfaces;
with System;     use System;
with Ada.Unchecked_Conversion;

package body Interfaces.COBOL is

   -----------------------------------------------
   -- Declarations for External Binary Handling --
   -----------------------------------------------

   subtype B1 is Byte_Array (1 .. 1);
   subtype B2 is Byte_Array (1 .. 2);
   subtype B4 is Byte_Array (1 .. 4);
   subtype B8 is Byte_Array (1 .. 8);
   --  Representations for 1,2,4,8 byte binary values

   function To_B1 is new Ada.Unchecked_Conversion (Integer_8,  B1);
   function To_B2 is new Ada.Unchecked_Conversion (Integer_16, B2);
   function To_B4 is new Ada.Unchecked_Conversion (Integer_32, B4);
   function To_B8 is new Ada.Unchecked_Conversion (Integer_64, B8);
   --  Conversions from native binary to external binary

   function From_B1 is new Ada.Unchecked_Conversion (B1, Integer_8);
   function From_B2 is new Ada.Unchecked_Conversion (B2, Integer_16);
   function From_B4 is new Ada.Unchecked_Conversion (B4, Integer_32);
   function From_B8 is new Ada.Unchecked_Conversion (B8, Integer_64);
   --  Conversions from external binary to signed native binary

   function From_B1U is new Ada.Unchecked_Conversion (B1, Unsigned_8);
   function From_B2U is new Ada.Unchecked_Conversion (B2, Unsigned_16);
   function From_B4U is new Ada.Unchecked_Conversion (B4, Unsigned_32);
   function From_B8U is new Ada.Unchecked_Conversion (B8, Unsigned_64);
   --  Conversions from external binary to unsigned native binary

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Binary_To_Decimal
     (Item   : Byte_Array;
      Format : Binary_Format) return Integer_64;
   --  This function converts a numeric value in the given format to its
   --  corresponding integer value. This is the non-generic implementation
   --  of Decimal_Conversions.To_Decimal. The generic routine does the
   --  final conversion to the fixed-point format.

   function Numeric_To_Decimal
     (Item   : Numeric;
      Format : Display_Format) return Integer_64;
   --  This function converts a numeric value in the given format to its
   --  corresponding integer value. This is the non-generic implementation
   --  of Decimal_Conversions.To_Decimal. The generic routine does the
   --  final conversion to the fixed-point format.

   function Packed_To_Decimal
     (Item   : Packed_Decimal;
      Format : Packed_Format) return Integer_64;
   --  This function converts a packed value in the given format to its
   --  corresponding integer value. This is the non-generic implementation
   --  of Decimal_Conversions.To_Decimal. The generic routine does the
   --  final conversion to the fixed-point format.

   procedure Swap (B : in out Byte_Array; F : Binary_Format);
   --  Swaps the bytes if required by the binary format F

   function To_Display
     (Item   : Integer_64;
      Format : Display_Format;
      Length : Natural) return Numeric;
   --  This function converts the given integer value into display format,
   --  using the given format, with the length in bytes of the result given
   --  by the last parameter. This is the non-generic implementation of
   --  Decimal_Conversions.To_Display. The conversion of the item from its
   --  original decimal format to Integer_64 is done by the generic routine.

   function To_Packed
     (Item   : Integer_64;
      Format : Packed_Format;
      Length : Natural) return Packed_Decimal;
   --  This function converts the given integer value into packed format,
   --  using the given format, with the length in digits of the result given
   --  by the last parameter. This is the non-generic implementation of
   --  Decimal_Conversions.To_Display. The conversion of the item from its
   --  original decimal format to Integer_64 is done by the generic routine.

   function Valid_Numeric
     (Item   : Numeric;
      Format : Display_Format) return Boolean;
   --  This is the non-generic implementation of Decimal_Conversions.Valid
   --  for the display case.

   function Valid_Packed
     (Item   : Packed_Decimal;
      Format : Packed_Format) return Boolean;
   --  This is the non-generic implementation of Decimal_Conversions.Valid
   --  for the packed case.

   -----------------------
   -- Binary_To_Decimal --
   -----------------------

   function Binary_To_Decimal
     (Item   : Byte_Array;
      Format : Binary_Format) return Integer_64
   is
      Len : constant Natural := Item'Length;

   begin
      if Len = 1 then
         if Format in Binary_Unsigned_Format then
            return Integer_64 (From_B1U (Item));
         else
            return Integer_64 (From_B1 (Item));
         end if;

      elsif Len = 2 then
         declare
            R : B2 := Item;

         begin
            Swap (R, Format);

            if Format in Binary_Unsigned_Format then
               return Integer_64 (From_B2U (R));
            else
               return Integer_64 (From_B2 (R));
            end if;
         end;

      elsif Len = 4 then
         declare
            R : B4 := Item;

         begin
            Swap (R, Format);

            if Format in Binary_Unsigned_Format then
               return Integer_64 (From_B4U (R));
            else
               return Integer_64 (From_B4 (R));
            end if;
         end;

      elsif Len = 8 then
         declare
            R : B8 := Item;

         begin
            Swap (R, Format);

            if Format in Binary_Unsigned_Format then
               return Integer_64 (From_B8U (R));
            else
               return Integer_64 (From_B8 (R));
            end if;
         end;

      --  Length is not 1, 2, 4 or 8

      else
         raise Conversion_Error;
      end if;
   end Binary_To_Decimal;

   ------------------------
   -- Numeric_To_Decimal --
   ------------------------

   --  The following assumptions are made in the coding of this routine:

   --    The range of COBOL_Digits is compact and the ten values
   --    represent the digits 0-9 in sequence

   --    The range of COBOL_Plus_Digits is compact and the ten values
   --    represent the digits 0-9 in sequence with a plus sign.

   --    The range of COBOL_Minus_Digits is compact and the ten values
   --    represent the digits 0-9 in sequence with a minus sign.

   --    The COBOL_Minus_Digits set is disjoint from COBOL_Digits

   --  These assumptions are true for all COBOL representations we know of

   function Numeric_To_Decimal
     (Item   : Numeric;
      Format : Display_Format) return Integer_64
   is
      pragma Unsuppress (Range_Check);
      Sign   : COBOL_Character := COBOL_Plus;
      Result : Integer_64 := 0;

   begin
      if not Valid_Numeric (Item, Format) then
         raise Conversion_Error;
      end if;

      for J in Item'Range loop
         declare
            K : constant COBOL_Character := Item (J);

         begin
            if K in COBOL_Digits then
               Result := Result * 10 +
                           (COBOL_Character'Pos (K) -
                             COBOL_Character'Pos (COBOL_Digits'First));

            elsif K in COBOL_Plus_Digits then
               Result := Result * 10 +
                           (COBOL_Character'Pos (K) -
                             COBOL_Character'Pos (COBOL_Plus_Digits'First));

            elsif K in COBOL_Minus_Digits then
               Result := Result * 10 +
                           (COBOL_Character'Pos (K) -
                             COBOL_Character'Pos (COBOL_Minus_Digits'First));
               Sign := COBOL_Minus;

            --  Only remaining possibility is COBOL_Plus or COBOL_Minus

            else
               Sign := K;
            end if;
         end;
      end loop;

      if Sign = COBOL_Plus then
         return Result;
      else
         return -Result;
      end if;

   exception
      when Constraint_Error =>
         raise Conversion_Error;

   end Numeric_To_Decimal;

   -----------------------
   -- Packed_To_Decimal --
   -----------------------

   function Packed_To_Decimal
     (Item   : Packed_Decimal;
      Format : Packed_Format) return Integer_64
   is
      pragma Unsuppress (Range_Check);
      Result : Integer_64 := 0;
      Sign   : constant Decimal_Element := Item (Item'Last);

   begin
      if not Valid_Packed (Item, Format) then
         raise Conversion_Error;
      end if;

      case Packed_Representation is
         when IBM =>
            for J in Item'First .. Item'Last - 1 loop
               Result := Result * 10 + Integer_64 (Item (J));
            end loop;

            if Sign = 16#0B# or else Sign = 16#0D# then
               return -Result;
            else
               return +Result;
            end if;
      end case;

   exception
      when Constraint_Error =>
         raise Conversion_Error;
   end Packed_To_Decimal;

   ----------
   -- Swap --
   ----------

   procedure Swap (B : in out Byte_Array; F : Binary_Format) is
      Little_Endian : constant Boolean :=
                        System.Default_Bit_Order = System.Low_Order_First;

   begin
      --  Return if no swap needed

      case F is
         when H | HU =>
            if not Little_Endian then
               return;
            end if;

         when L | LU =>
            if Little_Endian then
               return;
            end if;

         when N | NU =>
            return;
      end case;

      --  Here a swap is needed

      declare
         Len : constant Natural := B'Length;

      begin
         for J in 1 .. Len / 2 loop
            declare
               Temp : constant Byte := B (J);

            begin
               B (J) := B (Len + 1 - J);
               B (Len + 1 - J) := Temp;
            end;
         end loop;
      end;
   end Swap;

   -----------------------
   -- To_Ada (function) --
   -----------------------

   function To_Ada (Item : Alphanumeric) return String is
      Result : String (Item'Range);

   begin
      for J in Item'Range loop
         Result (J) := COBOL_To_Ada (Item (J));
      end loop;

      return Result;
   end To_Ada;

   ------------------------
   -- To_Ada (procedure) --
   ------------------------

   procedure To_Ada
     (Item   : Alphanumeric;
      Target : out String;
      Last   : out Natural)
   is
      Last_Val : Integer;

   begin
      if Item'Length > Target'Length then
         raise Constraint_Error;
      end if;

      Last_Val := Target'First - 1;
      for J in Item'Range loop
         Last_Val := Last_Val + 1;
         Target (Last_Val) := COBOL_To_Ada (Item (J));
      end loop;

      Last := Last_Val;
   end To_Ada;

   -------------------------
   -- To_COBOL (function) --
   -------------------------

   function To_COBOL (Item : String) return Alphanumeric is
      Result : Alphanumeric (Item'Range);

   begin
      for J in Item'Range loop
         Result (J) := Ada_To_COBOL (Item (J));
      end loop;

      return Result;
   end To_COBOL;

   --------------------------
   -- To_COBOL (procedure) --
   --------------------------

   procedure To_COBOL
     (Item   : String;
      Target : out Alphanumeric;
      Last   : out Natural)
   is
      Last_Val : Integer;

   begin
      if Item'Length > Target'Length then
         raise Constraint_Error;
      end if;

      Last_Val := Target'First - 1;
      for J in Item'Range loop
         Last_Val := Last_Val + 1;
         Target (Last_Val) := Ada_To_COBOL (Item (J));
      end loop;

      Last := Last_Val;
   end To_COBOL;

   ----------------
   -- To_Display --
   ----------------

   function To_Display
     (Item   : Integer_64;
      Format : Display_Format;
      Length : Natural) return Numeric
   is
      Result : Numeric (1 .. Length);
      Val    : Integer_64 := Item;

      procedure Convert (First, Last : Natural);
      --  Convert the number in Val into COBOL_Digits, storing the result
      --  in Result (First .. Last). Raise Conversion_Error if too large.

      procedure Embed_Sign (Loc : Natural);
      --  Used for the nonseparate formats to embed the appropriate sign
      --  at the specified location (i.e. at Result (Loc))

      -------------
      -- Convert --
      -------------

      procedure Convert (First, Last : Natural) is
         J : Natural;

      begin
         J := Last;
         while J >= First loop
            Result (J) :=
              COBOL_Character'Val
                (COBOL_Character'Pos (COBOL_Digits'First) +
                                                   Integer (Val mod 10));
            Val := Val / 10;

            if Val = 0 then
               for K in First .. J - 1 loop
                  Result (J) := COBOL_Digits'First;
               end loop;

               return;

            else
               J := J - 1;
            end if;
         end loop;

         raise Conversion_Error;
      end Convert;

      ----------------
      -- Embed_Sign --
      ----------------

      procedure Embed_Sign (Loc : Natural) is
         Digit : Natural range 0 .. 9;

      begin
         Digit := COBOL_Character'Pos (Result (Loc)) -
                  COBOL_Character'Pos (COBOL_Digits'First);

         if Item >= 0 then
            Result (Loc) :=
              COBOL_Character'Val
                (COBOL_Character'Pos (COBOL_Plus_Digits'First) + Digit);
         else
            Result (Loc) :=
              COBOL_Character'Val
                (COBOL_Character'Pos (COBOL_Minus_Digits'First) + Digit);
         end if;
      end Embed_Sign;

   --  Start of processing for To_Display

   begin
      case Format is
         when Unsigned =>
            if Val < 0 then
               raise Conversion_Error;
            else
               Convert (1, Length);
            end if;

         when Leading_Separate =>
            if Val < 0 then
               Result (1) := COBOL_Minus;
               Val := -Val;
            else
               Result (1) := COBOL_Plus;
            end if;

            Convert (2, Length);

         when Trailing_Separate =>
            if Val < 0 then
               Result (Length) := COBOL_Minus;
               Val := -Val;
            else
               Result (Length) := COBOL_Plus;
            end if;

            Convert (1, Length - 1);

         when Leading_Nonseparate =>
            Val := abs Val;
            Convert (1, Length);
            Embed_Sign (1);

         when Trailing_Nonseparate =>
            Val := abs Val;
            Convert (1, Length);
            Embed_Sign (Length);
      end case;

      return Result;
   end To_Display;

   ---------------
   -- To_Packed --
   ---------------

   function To_Packed
     (Item   : Integer_64;
      Format : Packed_Format;
      Length : Natural) return Packed_Decimal
   is
      Result : Packed_Decimal (1 .. Length);
      Val    : Integer_64;

      procedure Convert (First, Last : Natural);
      --  Convert the number in Val into a sequence of Decimal_Element values,
      --  storing the result in Result (First .. Last). Raise Conversion_Error
      --  if the value is too large to fit.

      -------------
      -- Convert --
      -------------

      procedure Convert (First, Last : Natural) is
         J : Natural := Last;

      begin
         while J >= First loop
            Result (J) := Decimal_Element (Val mod 10);

            Val := Val / 10;

            if Val = 0 then
               for K in First .. J - 1 loop
                  Result (K) := 0;
               end loop;

               return;

            else
               J := J - 1;
            end if;
         end loop;

         raise Conversion_Error;
      end Convert;

   --  Start of processing for To_Packed

   begin
      case Packed_Representation is
         when IBM =>
            if Format = Packed_Unsigned then
               if Item < 0 then
                  raise Conversion_Error;
               else
                  Result (Length) := 16#F#;
                  Val := Item;
               end if;

            elsif Item >= 0 then
               Result (Length) := 16#C#;
               Val := Item;

            else -- Item < 0
               Result (Length) := 16#D#;
               Val := -Item;
            end if;

            Convert (1, Length - 1);
            return Result;
      end case;
   end To_Packed;

   -------------------
   -- Valid_Numeric --
   -------------------

   function Valid_Numeric
     (Item   : Numeric;
      Format : Display_Format) return Boolean
   is
   begin
      if Item'Length = 0 then
         return False;
      end if;

      --  All character positions except first and last must be Digits.
      --  This is true for all the formats.

      for J in Item'First + 1 .. Item'Last - 1 loop
         if Item (J) not in COBOL_Digits then
            return False;
         end if;
      end loop;

      case Format is
         when Unsigned =>
            return Item (Item'First) in COBOL_Digits
              and then Item (Item'Last) in COBOL_Digits;

         when Leading_Separate =>
            return (Item (Item'First) = COBOL_Plus or else
                    Item (Item'First) = COBOL_Minus)
              and then Item (Item'Last) in COBOL_Digits;

         when Trailing_Separate =>
            return Item (Item'First) in COBOL_Digits
              and then
                (Item (Item'Last) = COBOL_Plus or else
                 Item (Item'Last) = COBOL_Minus);

         when Leading_Nonseparate =>
            return (Item (Item'First) in COBOL_Plus_Digits or else
                    Item (Item'First) in COBOL_Minus_Digits)
              and then Item (Item'Last) in COBOL_Digits;

         when Trailing_Nonseparate =>
            return Item (Item'First) in COBOL_Digits
              and then
                (Item (Item'Last) in COBOL_Plus_Digits or else
                 Item (Item'Last) in COBOL_Minus_Digits);

      end case;
   end Valid_Numeric;

   ------------------
   -- Valid_Packed --
   ------------------

   function Valid_Packed
     (Item   : Packed_Decimal;
      Format : Packed_Format) return Boolean
   is
   begin
      case Packed_Representation is
         when IBM =>
            for J in Item'First .. Item'Last - 1 loop
               if Item (J) > 9 then
                  return False;
               end if;
            end loop;

            --  For unsigned, sign digit must be F

            if Format = Packed_Unsigned then
               return Item (Item'Last) = 16#F#;

            --  For signed, accept all standard and non-standard signs

            else
               return Item (Item'Last) in 16#A# .. 16#F#;
            end if;
      end case;
   end Valid_Packed;

   -------------------------
   -- Decimal_Conversions --
   -------------------------

   package body Decimal_Conversions is

      ---------------------
      -- Length (binary) --
      ---------------------

      --  Note that the tests here are all compile time tests

      function Length (Format : Binary_Format) return Natural is
         pragma Unreferenced (Format);
      begin
         if Num'Digits <= 2 then
            return 1;
         elsif Num'Digits <= 4 then
            return 2;
         elsif Num'Digits <= 9 then
            return 4;
         else -- Num'Digits in 10 .. 18
            return 8;
         end if;
      end Length;

      ----------------------
      -- Length (display) --
      ----------------------

      function Length (Format : Display_Format) return Natural is
      begin
         if Format = Leading_Separate or else Format = Trailing_Separate then
            return Num'Digits + 1;
         else
            return Num'Digits;
         end if;
      end Length;

      ---------------------
      -- Length (packed) --
      ---------------------

      --  Note that the tests here are all compile time checks

      function Length
        (Format : Packed_Format) return Natural
      is
         pragma Unreferenced (Format);
      begin
         case Packed_Representation is
            when IBM =>
               return (Num'Digits + 2) / 2 * 2;
         end case;
      end Length;

      ---------------
      -- To_Binary --
      ---------------

      function To_Binary
        (Item   : Num;
         Format : Binary_Format) return Byte_Array
      is
      begin
         --  Note: all these tests are compile time tests

         if Num'Digits <= 2 then
            return To_B1 (Integer_8'Integer_Value (Item));

         elsif Num'Digits <= 4 then
            declare
               R : B2 := To_B2 (Integer_16'Integer_Value (Item));

            begin
               Swap (R, Format);
               return R;
            end;

         elsif Num'Digits <= 9 then
            declare
               R : B4 := To_B4 (Integer_32'Integer_Value (Item));

            begin
               Swap (R, Format);
               return R;
            end;

         else -- Num'Digits in 10 .. 18
            declare
               R : B8 := To_B8 (Integer_64'Integer_Value (Item));

            begin
               Swap (R, Format);
               return R;
            end;
         end if;

      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Binary;

      ---------------------------------
      -- To_Binary (internal binary) --
      ---------------------------------

      function To_Binary (Item : Num) return Binary is
         pragma Unsuppress (Range_Check);
      begin
         return Binary'Integer_Value (Item);
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Binary;

      -------------------------
      -- To_Decimal (binary) --
      -------------------------

      function To_Decimal
        (Item   : Byte_Array;
         Format : Binary_Format) return Num
      is
         pragma Unsuppress (Range_Check);
      begin
         return Num'Fixed_Value (Binary_To_Decimal (Item, Format));
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Decimal;

      ----------------------------------
      -- To_Decimal (internal binary) --
      ----------------------------------

      function To_Decimal (Item : Binary) return Num is
         pragma Unsuppress (Range_Check);
      begin
         return Num'Fixed_Value (Item);
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Decimal;

      --------------------------
      -- To_Decimal (display) --
      --------------------------

      function To_Decimal
        (Item   : Numeric;
         Format : Display_Format) return Num
      is
         pragma Unsuppress (Range_Check);

      begin
         return Num'Fixed_Value (Numeric_To_Decimal (Item, Format));
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Decimal;

      ---------------------------------------
      -- To_Decimal (internal long binary) --
      ---------------------------------------

      function To_Decimal (Item : Long_Binary) return Num is
         pragma Unsuppress (Range_Check);
      begin
         return Num'Fixed_Value (Item);
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Decimal;

      -------------------------
      -- To_Decimal (packed) --
      -------------------------

      function To_Decimal
        (Item   : Packed_Decimal;
         Format : Packed_Format) return Num
      is
         pragma Unsuppress (Range_Check);
      begin
         return Num'Fixed_Value (Packed_To_Decimal (Item, Format));
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Decimal;

      ----------------
      -- To_Display --
      ----------------

      function To_Display
        (Item   : Num;
         Format : Display_Format) return Numeric
      is
         pragma Unsuppress (Range_Check);
      begin
         return
           To_Display
             (Integer_64'Integer_Value (Item),
              Format,
              Length (Format));
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Display;

      --------------------
      -- To_Long_Binary --
      --------------------

      function To_Long_Binary (Item : Num) return Long_Binary is
         pragma Unsuppress (Range_Check);
      begin
         return Long_Binary'Integer_Value (Item);
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Long_Binary;

      ---------------
      -- To_Packed --
      ---------------

      function To_Packed
        (Item   : Num;
         Format : Packed_Format) return Packed_Decimal
      is
         pragma Unsuppress (Range_Check);
      begin
         return
           To_Packed
             (Integer_64'Integer_Value (Item),
              Format,
              Length (Format));
      exception
         when Constraint_Error =>
            raise Conversion_Error;
      end To_Packed;

      --------------------
      -- Valid (binary) --
      --------------------

      function Valid
        (Item   : Byte_Array;
         Format : Binary_Format) return Boolean
      is
         Val : Num;
         pragma Unreferenced (Val);
      begin
         Val := To_Decimal (Item, Format);
         return True;
      exception
         when Conversion_Error =>
            return False;
      end Valid;

      ---------------------
      -- Valid (display) --
      ---------------------

      function Valid
        (Item   : Numeric;
         Format : Display_Format) return Boolean
      is
      begin
         return Valid_Numeric (Item, Format);
      end Valid;

      --------------------
      -- Valid (packed) --
      --------------------

      function Valid
        (Item   : Packed_Decimal;
         Format : Packed_Format) return Boolean
      is
      begin
         return Valid_Packed (Item, Format);
      end Valid;

   end Decimal_Conversions;

end Interfaces.COBOL;
