------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      I N T E R F A C E S . C O B O L                     --
--                                                                          --
--                                 S p e c                                  --
--                             (ASCII Version)                              --
--                                                                          --
--          Copyright (C) 1993-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This version of the COBOL interfaces package assumes that the COBOL
--  compiler uses ASCII as its internal representation of characters, i.e.
--  that the type COBOL_Character has the same representation as the Ada
--  type Standard.Character.

package Interfaces.COBOL is
   pragma Preelaborate (COBOL);

   ------------------------------------------------------------
   -- Types And Operations For Internal Data Representations --
   ------------------------------------------------------------

   type Floating      is new Float;
   type Long_Floating is new Long_Float;

   type Binary        is new Integer;
   type Long_Binary   is new Long_Long_Integer;

   Max_Digits_Binary      : constant := 9;
   Max_Digits_Long_Binary : constant := 18;

   type Decimal_Element is mod 16;
   type Packed_Decimal is array (Positive range <>) of Decimal_Element;
   pragma Pack (Packed_Decimal);

   type COBOL_Character is new Character;

   Ada_To_COBOL : array (Standard.Character) of COBOL_Character := (
     COBOL_Character'Val (000), COBOL_Character'Val (001),
     COBOL_Character'Val (002), COBOL_Character'Val (003),
     COBOL_Character'Val (004), COBOL_Character'Val (005),
     COBOL_Character'Val (006), COBOL_Character'Val (007),
     COBOL_Character'Val (008), COBOL_Character'Val (009),
     COBOL_Character'Val (010), COBOL_Character'Val (011),
     COBOL_Character'Val (012), COBOL_Character'Val (013),
     COBOL_Character'Val (014), COBOL_Character'Val (015),
     COBOL_Character'Val (016), COBOL_Character'Val (017),
     COBOL_Character'Val (018), COBOL_Character'Val (019),
     COBOL_Character'Val (020), COBOL_Character'Val (021),
     COBOL_Character'Val (022), COBOL_Character'Val (023),
     COBOL_Character'Val (024), COBOL_Character'Val (025),
     COBOL_Character'Val (026), COBOL_Character'Val (027),
     COBOL_Character'Val (028), COBOL_Character'Val (029),
     COBOL_Character'Val (030), COBOL_Character'Val (031),
     COBOL_Character'Val (032), COBOL_Character'Val (033),
     COBOL_Character'Val (034), COBOL_Character'Val (035),
     COBOL_Character'Val (036), COBOL_Character'Val (037),
     COBOL_Character'Val (038), COBOL_Character'Val (039),
     COBOL_Character'Val (040), COBOL_Character'Val (041),
     COBOL_Character'Val (042), COBOL_Character'Val (043),
     COBOL_Character'Val (044), COBOL_Character'Val (045),
     COBOL_Character'Val (046), COBOL_Character'Val (047),
     COBOL_Character'Val (048), COBOL_Character'Val (049),
     COBOL_Character'Val (050), COBOL_Character'Val (051),
     COBOL_Character'Val (052), COBOL_Character'Val (053),
     COBOL_Character'Val (054), COBOL_Character'Val (055),
     COBOL_Character'Val (056), COBOL_Character'Val (057),
     COBOL_Character'Val (058), COBOL_Character'Val (059),
     COBOL_Character'Val (060), COBOL_Character'Val (061),
     COBOL_Character'Val (062), COBOL_Character'Val (063),
     COBOL_Character'Val (064), COBOL_Character'Val (065),
     COBOL_Character'Val (066), COBOL_Character'Val (067),
     COBOL_Character'Val (068), COBOL_Character'Val (069),
     COBOL_Character'Val (070), COBOL_Character'Val (071),
     COBOL_Character'Val (072), COBOL_Character'Val (073),
     COBOL_Character'Val (074), COBOL_Character'Val (075),
     COBOL_Character'Val (076), COBOL_Character'Val (077),
     COBOL_Character'Val (078), COBOL_Character'Val (079),
     COBOL_Character'Val (080), COBOL_Character'Val (081),
     COBOL_Character'Val (082), COBOL_Character'Val (083),
     COBOL_Character'Val (084), COBOL_Character'Val (085),
     COBOL_Character'Val (086), COBOL_Character'Val (087),
     COBOL_Character'Val (088), COBOL_Character'Val (089),
     COBOL_Character'Val (090), COBOL_Character'Val (091),
     COBOL_Character'Val (092), COBOL_Character'Val (093),
     COBOL_Character'Val (094), COBOL_Character'Val (095),
     COBOL_Character'Val (096), COBOL_Character'Val (097),
     COBOL_Character'Val (098), COBOL_Character'Val (099),
     COBOL_Character'Val (100), COBOL_Character'Val (101),
     COBOL_Character'Val (102), COBOL_Character'Val (103),
     COBOL_Character'Val (104), COBOL_Character'Val (105),
     COBOL_Character'Val (106), COBOL_Character'Val (107),
     COBOL_Character'Val (108), COBOL_Character'Val (109),
     COBOL_Character'Val (110), COBOL_Character'Val (111),
     COBOL_Character'Val (112), COBOL_Character'Val (113),
     COBOL_Character'Val (114), COBOL_Character'Val (115),
     COBOL_Character'Val (116), COBOL_Character'Val (117),
     COBOL_Character'Val (118), COBOL_Character'Val (119),
     COBOL_Character'Val (120), COBOL_Character'Val (121),
     COBOL_Character'Val (122), COBOL_Character'Val (123),
     COBOL_Character'Val (124), COBOL_Character'Val (125),
     COBOL_Character'Val (126), COBOL_Character'Val (127),
     COBOL_Character'Val (128), COBOL_Character'Val (129),
     COBOL_Character'Val (130), COBOL_Character'Val (131),
     COBOL_Character'Val (132), COBOL_Character'Val (133),
     COBOL_Character'Val (134), COBOL_Character'Val (135),
     COBOL_Character'Val (136), COBOL_Character'Val (137),
     COBOL_Character'Val (138), COBOL_Character'Val (139),
     COBOL_Character'Val (140), COBOL_Character'Val (141),
     COBOL_Character'Val (142), COBOL_Character'Val (143),
     COBOL_Character'Val (144), COBOL_Character'Val (145),
     COBOL_Character'Val (146), COBOL_Character'Val (147),
     COBOL_Character'Val (148), COBOL_Character'Val (149),
     COBOL_Character'Val (150), COBOL_Character'Val (151),
     COBOL_Character'Val (152), COBOL_Character'Val (153),
     COBOL_Character'Val (154), COBOL_Character'Val (155),
     COBOL_Character'Val (156), COBOL_Character'Val (157),
     COBOL_Character'Val (158), COBOL_Character'Val (159),
     COBOL_Character'Val (160), COBOL_Character'Val (161),
     COBOL_Character'Val (162), COBOL_Character'Val (163),
     COBOL_Character'Val (164), COBOL_Character'Val (165),
     COBOL_Character'Val (166), COBOL_Character'Val (167),
     COBOL_Character'Val (168), COBOL_Character'Val (169),
     COBOL_Character'Val (170), COBOL_Character'Val (171),
     COBOL_Character'Val (172), COBOL_Character'Val (173),
     COBOL_Character'Val (174), COBOL_Character'Val (175),
     COBOL_Character'Val (176), COBOL_Character'Val (177),
     COBOL_Character'Val (178), COBOL_Character'Val (179),
     COBOL_Character'Val (180), COBOL_Character'Val (181),
     COBOL_Character'Val (182), COBOL_Character'Val (183),
     COBOL_Character'Val (184), COBOL_Character'Val (185),
     COBOL_Character'Val (186), COBOL_Character'Val (187),
     COBOL_Character'Val (188), COBOL_Character'Val (189),
     COBOL_Character'Val (190), COBOL_Character'Val (191),
     COBOL_Character'Val (192), COBOL_Character'Val (193),
     COBOL_Character'Val (194), COBOL_Character'Val (195),
     COBOL_Character'Val (196), COBOL_Character'Val (197),
     COBOL_Character'Val (198), COBOL_Character'Val (199),
     COBOL_Character'Val (200), COBOL_Character'Val (201),
     COBOL_Character'Val (202), COBOL_Character'Val (203),
     COBOL_Character'Val (204), COBOL_Character'Val (205),
     COBOL_Character'Val (206), COBOL_Character'Val (207),
     COBOL_Character'Val (208), COBOL_Character'Val (209),
     COBOL_Character'Val (210), COBOL_Character'Val (211),
     COBOL_Character'Val (212), COBOL_Character'Val (213),
     COBOL_Character'Val (214), COBOL_Character'Val (215),
     COBOL_Character'Val (216), COBOL_Character'Val (217),
     COBOL_Character'Val (218), COBOL_Character'Val (219),
     COBOL_Character'Val (220), COBOL_Character'Val (221),
     COBOL_Character'Val (222), COBOL_Character'Val (223),
     COBOL_Character'Val (224), COBOL_Character'Val (225),
     COBOL_Character'Val (226), COBOL_Character'Val (227),
     COBOL_Character'Val (228), COBOL_Character'Val (229),
     COBOL_Character'Val (230), COBOL_Character'Val (231),
     COBOL_Character'Val (232), COBOL_Character'Val (233),
     COBOL_Character'Val (234), COBOL_Character'Val (235),
     COBOL_Character'Val (236), COBOL_Character'Val (237),
     COBOL_Character'Val (238), COBOL_Character'Val (239),
     COBOL_Character'Val (240), COBOL_Character'Val (241),
     COBOL_Character'Val (242), COBOL_Character'Val (243),
     COBOL_Character'Val (244), COBOL_Character'Val (245),
     COBOL_Character'Val (246), COBOL_Character'Val (247),
     COBOL_Character'Val (248), COBOL_Character'Val (249),
     COBOL_Character'Val (250), COBOL_Character'Val (251),
     COBOL_Character'Val (252), COBOL_Character'Val (253),
     COBOL_Character'Val (254), COBOL_Character'Val (255));

   COBOL_To_Ada : array (COBOL_Character) of Standard.Character := (
     Standard.Character'Val (000), Standard.Character'Val (001),
     Standard.Character'Val (002), Standard.Character'Val (003),
     Standard.Character'Val (004), Standard.Character'Val (005),
     Standard.Character'Val (006), Standard.Character'Val (007),
     Standard.Character'Val (008), Standard.Character'Val (009),
     Standard.Character'Val (010), Standard.Character'Val (011),
     Standard.Character'Val (012), Standard.Character'Val (013),
     Standard.Character'Val (014), Standard.Character'Val (015),
     Standard.Character'Val (016), Standard.Character'Val (017),
     Standard.Character'Val (018), Standard.Character'Val (019),
     Standard.Character'Val (020), Standard.Character'Val (021),
     Standard.Character'Val (022), Standard.Character'Val (023),
     Standard.Character'Val (024), Standard.Character'Val (025),
     Standard.Character'Val (026), Standard.Character'Val (027),
     Standard.Character'Val (028), Standard.Character'Val (029),
     Standard.Character'Val (030), Standard.Character'Val (031),
     Standard.Character'Val (032), Standard.Character'Val (033),
     Standard.Character'Val (034), Standard.Character'Val (035),
     Standard.Character'Val (036), Standard.Character'Val (037),
     Standard.Character'Val (038), Standard.Character'Val (039),
     Standard.Character'Val (040), Standard.Character'Val (041),
     Standard.Character'Val (042), Standard.Character'Val (043),
     Standard.Character'Val (044), Standard.Character'Val (045),
     Standard.Character'Val (046), Standard.Character'Val (047),
     Standard.Character'Val (048), Standard.Character'Val (049),
     Standard.Character'Val (050), Standard.Character'Val (051),
     Standard.Character'Val (052), Standard.Character'Val (053),
     Standard.Character'Val (054), Standard.Character'Val (055),
     Standard.Character'Val (056), Standard.Character'Val (057),
     Standard.Character'Val (058), Standard.Character'Val (059),
     Standard.Character'Val (060), Standard.Character'Val (061),
     Standard.Character'Val (062), Standard.Character'Val (063),
     Standard.Character'Val (064), Standard.Character'Val (065),
     Standard.Character'Val (066), Standard.Character'Val (067),
     Standard.Character'Val (068), Standard.Character'Val (069),
     Standard.Character'Val (070), Standard.Character'Val (071),
     Standard.Character'Val (072), Standard.Character'Val (073),
     Standard.Character'Val (074), Standard.Character'Val (075),
     Standard.Character'Val (076), Standard.Character'Val (077),
     Standard.Character'Val (078), Standard.Character'Val (079),
     Standard.Character'Val (080), Standard.Character'Val (081),
     Standard.Character'Val (082), Standard.Character'Val (083),
     Standard.Character'Val (084), Standard.Character'Val (085),
     Standard.Character'Val (086), Standard.Character'Val (087),
     Standard.Character'Val (088), Standard.Character'Val (089),
     Standard.Character'Val (090), Standard.Character'Val (091),
     Standard.Character'Val (092), Standard.Character'Val (093),
     Standard.Character'Val (094), Standard.Character'Val (095),
     Standard.Character'Val (096), Standard.Character'Val (097),
     Standard.Character'Val (098), Standard.Character'Val (099),
     Standard.Character'Val (100), Standard.Character'Val (101),
     Standard.Character'Val (102), Standard.Character'Val (103),
     Standard.Character'Val (104), Standard.Character'Val (105),
     Standard.Character'Val (106), Standard.Character'Val (107),
     Standard.Character'Val (108), Standard.Character'Val (109),
     Standard.Character'Val (110), Standard.Character'Val (111),
     Standard.Character'Val (112), Standard.Character'Val (113),
     Standard.Character'Val (114), Standard.Character'Val (115),
     Standard.Character'Val (116), Standard.Character'Val (117),
     Standard.Character'Val (118), Standard.Character'Val (119),
     Standard.Character'Val (120), Standard.Character'Val (121),
     Standard.Character'Val (122), Standard.Character'Val (123),
     Standard.Character'Val (124), Standard.Character'Val (125),
     Standard.Character'Val (126), Standard.Character'Val (127),
     Standard.Character'Val (128), Standard.Character'Val (129),
     Standard.Character'Val (130), Standard.Character'Val (131),
     Standard.Character'Val (132), Standard.Character'Val (133),
     Standard.Character'Val (134), Standard.Character'Val (135),
     Standard.Character'Val (136), Standard.Character'Val (137),
     Standard.Character'Val (138), Standard.Character'Val (139),
     Standard.Character'Val (140), Standard.Character'Val (141),
     Standard.Character'Val (142), Standard.Character'Val (143),
     Standard.Character'Val (144), Standard.Character'Val (145),
     Standard.Character'Val (146), Standard.Character'Val (147),
     Standard.Character'Val (148), Standard.Character'Val (149),
     Standard.Character'Val (150), Standard.Character'Val (151),
     Standard.Character'Val (152), Standard.Character'Val (153),
     Standard.Character'Val (154), Standard.Character'Val (155),
     Standard.Character'Val (156), Standard.Character'Val (157),
     Standard.Character'Val (158), Standard.Character'Val (159),
     Standard.Character'Val (160), Standard.Character'Val (161),
     Standard.Character'Val (162), Standard.Character'Val (163),
     Standard.Character'Val (164), Standard.Character'Val (165),
     Standard.Character'Val (166), Standard.Character'Val (167),
     Standard.Character'Val (168), Standard.Character'Val (169),
     Standard.Character'Val (170), Standard.Character'Val (171),
     Standard.Character'Val (172), Standard.Character'Val (173),
     Standard.Character'Val (174), Standard.Character'Val (175),
     Standard.Character'Val (176), Standard.Character'Val (177),
     Standard.Character'Val (178), Standard.Character'Val (179),
     Standard.Character'Val (180), Standard.Character'Val (181),
     Standard.Character'Val (182), Standard.Character'Val (183),
     Standard.Character'Val (184), Standard.Character'Val (185),
     Standard.Character'Val (186), Standard.Character'Val (187),
     Standard.Character'Val (188), Standard.Character'Val (189),
     Standard.Character'Val (190), Standard.Character'Val (191),
     Standard.Character'Val (192), Standard.Character'Val (193),
     Standard.Character'Val (194), Standard.Character'Val (195),
     Standard.Character'Val (196), Standard.Character'Val (197),
     Standard.Character'Val (198), Standard.Character'Val (199),
     Standard.Character'Val (200), Standard.Character'Val (201),
     Standard.Character'Val (202), Standard.Character'Val (203),
     Standard.Character'Val (204), Standard.Character'Val (205),
     Standard.Character'Val (206), Standard.Character'Val (207),
     Standard.Character'Val (208), Standard.Character'Val (209),
     Standard.Character'Val (210), Standard.Character'Val (211),
     Standard.Character'Val (212), Standard.Character'Val (213),
     Standard.Character'Val (214), Standard.Character'Val (215),
     Standard.Character'Val (216), Standard.Character'Val (217),
     Standard.Character'Val (218), Standard.Character'Val (219),
     Standard.Character'Val (220), Standard.Character'Val (221),
     Standard.Character'Val (222), Standard.Character'Val (223),
     Standard.Character'Val (224), Standard.Character'Val (225),
     Standard.Character'Val (226), Standard.Character'Val (227),
     Standard.Character'Val (228), Standard.Character'Val (229),
     Standard.Character'Val (230), Standard.Character'Val (231),
     Standard.Character'Val (232), Standard.Character'Val (233),
     Standard.Character'Val (234), Standard.Character'Val (235),
     Standard.Character'Val (236), Standard.Character'Val (237),
     Standard.Character'Val (238), Standard.Character'Val (239),
     Standard.Character'Val (240), Standard.Character'Val (241),
     Standard.Character'Val (242), Standard.Character'Val (243),
     Standard.Character'Val (244), Standard.Character'Val (245),
     Standard.Character'Val (246), Standard.Character'Val (247),
     Standard.Character'Val (248), Standard.Character'Val (249),
     Standard.Character'Val (250), Standard.Character'Val (251),
     Standard.Character'Val (252), Standard.Character'Val (253),
     Standard.Character'Val (254), Standard.Character'Val (255));

   type Alphanumeric is array (Positive range <>) of COBOL_Character;
   --  pragma Pack (Alphanumeric);

   function To_COBOL (Item : String) return Alphanumeric;
   function To_Ada   (Item : Alphanumeric) return String;

   procedure To_COBOL
     (Item   : String;
      Target : out Alphanumeric;
      Last   : out Natural);

   procedure To_Ada
     (Item   : Alphanumeric;
      Target : out String;
      Last   : out Natural);

   type Numeric is array (Positive range <>) of COBOL_Character;
   --  pragma Pack (Numeric);

   --------------------------------------------
   -- Formats For COBOL Data Representations --
   --------------------------------------------

   type Display_Format is private;

   Unsigned             : constant Display_Format;
   Leading_Separate     : constant Display_Format;
   Trailing_Separate    : constant Display_Format;
   Leading_Nonseparate  : constant Display_Format;
   Trailing_Nonseparate : constant Display_Format;

   type Binary_Format is private;

   High_Order_First          : constant Binary_Format;
   Low_Order_First           : constant Binary_Format;
   Native_Binary             : constant Binary_Format;
   High_Order_First_Unsigned : constant Binary_Format;
   Low_Order_First_Unsigned  : constant Binary_Format;
   Native_Binary_Unsigned    : constant Binary_Format;

   type Packed_Format is private;

   Packed_Unsigned   : constant Packed_Format;
   Packed_Signed     : constant Packed_Format;

   ------------------------------------------------------------
   -- Types For External Representation Of COBOL Binary Data --
   ------------------------------------------------------------

   type Byte is mod 2 ** COBOL_Character'Size;
   type Byte_Array is array (Positive range <>) of Byte;
   --  pragma Pack (Byte_Array);

   Conversion_Error : exception;

   generic
      type Num is delta <> digits <>;

   package Decimal_Conversions is

      --  Display Formats: data values are represented as Numeric

      function Valid
        (Item   : Numeric;
         Format : Display_Format) return Boolean;

      function Length
        (Format : Display_Format) return Natural;

      function To_Decimal
        (Item   : Numeric;
         Format : Display_Format)
         return   Num;

      function To_Display
        (Item   : Num;
         Format : Display_Format) return Numeric;

      --  Packed Formats: data values are represented as Packed_Decimal

      function Valid
        (Item   : Packed_Decimal;
         Format : Packed_Format) return Boolean;

      function Length
        (Format : Packed_Format) return Natural;

      function To_Decimal
        (Item   : Packed_Decimal;
         Format : Packed_Format) return Num;

      function To_Packed
        (Item   : Num;
         Format : Packed_Format) return Packed_Decimal;

      --  Binary Formats: external data values are represented as Byte_Array

      function Valid
        (Item   : Byte_Array;
         Format : Binary_Format) return Boolean;

      function Length
        (Format : Binary_Format)
         return   Natural;

      function To_Decimal
        (Item   : Byte_Array;
         Format : Binary_Format) return Num;

      function To_Binary
        (Item   : Num;
         Format : Binary_Format) return Byte_Array;

      --  Internal Binary formats: data values are of type Binary/Long_Binary

      function To_Decimal (Item : Binary)      return Num;
      function To_Decimal (Item : Long_Binary) return Num;

      function To_Binary      (Item : Num)  return Binary;
      function To_Long_Binary (Item : Num)  return Long_Binary;

   private
      pragma Inline (Length);
      pragma Inline (To_Binary);
      pragma Inline (To_Decimal);
      pragma Inline (To_Display);
      pragma Inline (To_Long_Binary);
      pragma Inline (Valid);

   end Decimal_Conversions;

   ------------------------------------------
   -- Implementation Dependent Definitions --
   ------------------------------------------

   --  The implementation dependent definitions are wholly contained in the
   --  private part of this spec (the body is implementation independent)

private
   -------------------
   -- Binary Format --
   -------------------

   type Binary_Format is (H, L, N, HU, LU, NU);

   subtype Binary_Unsigned_Format is Binary_Format range HU .. NU;

   High_Order_First          : constant Binary_Format := H;
   Low_Order_First           : constant Binary_Format := L;
   Native_Binary             : constant Binary_Format := N;
   High_Order_First_Unsigned : constant Binary_Format := HU;
   Low_Order_First_Unsigned  : constant Binary_Format := LU;
   Native_Binary_Unsigned    : constant Binary_Format := NU;

   ---------------------------
   -- Packed Decimal Format --
   ---------------------------

   --  Packed decimal numbers use the IBM mainframe format:

   --     dd dd ... dd dd ds

   --  where d are the Digits, in natural left to right order, and s is
   --  the sign digit. If the number of Digits os even, then the high
   --  order (leftmost) Digits is always a 0. For example, a six digit
   --  number has the format:

   --     0d dd dd ds

   --  The sign digit has the possible values

   --     16#0A#     non-standard plus sign
   --     16#0B#     non-standard minus sign
   --     16#0C#     standard plus sign
   --     16#0D#     standard minus sign
   --     16#0E#     non-standard plus sign
   --     16#0F#     standard unsigned sign

   --  The non-standard signs are recognized on input, but never generated
   --  for output numbers. The 16#0F# distinguishes unsigned numbers from
   --  signed positive numbers, but is treated as positive for computational
   --  purposes. This format provides distinguished positive and negative
   --  zero values, which behave the same in all operations.

   type Packed_Format is (U, S);

   Packed_Unsigned : constant Packed_Format := U;
   Packed_Signed   : constant Packed_Format := S;

   type Packed_Representation_Type is (IBM);
   --  Indicator for format used for packed decimal

   Packed_Representation : constant Packed_Representation_Type := IBM;
   --  This version of the spec uses IBM internal format, as described above

   -----------------------------
   -- Display Decimal Formats --
   -----------------------------

   --  Display numbers are stored in standard ASCII format, as ASCII strings.
   --  For the embedded signs, the following codes are used:

   --     0-9 positive:  16#30# .. 16#39# (i.e. natural ASCII digit code)
   --     0-9 negative:  16#20# .. 16#29# (ASCII digit code - 16#10#)

   type Display_Format is (U, LS, TS, LN, TN);

   Unsigned             : constant Display_Format := U;
   Leading_Separate     : constant Display_Format := LS;
   Trailing_Separate    : constant Display_Format := TS;
   Leading_Nonseparate  : constant Display_Format := LN;
   Trailing_Nonseparate : constant Display_Format := TN;

   subtype COBOL_Digits is COBOL_Character range '0' .. '9';
   --  Digit values in display decimal

   COBOL_Space : constant COBOL_Character := ' ';
   COBOL_Plus  : constant COBOL_Character := '+';
   COBOL_Minus : constant COBOL_Character := '-';
   --  Sign values for Leading_Separate and Trailing_Separate formats

   subtype COBOL_Plus_Digits is COBOL_Character
     range COBOL_Character'Val (16#30#) .. COBOL_Character'Val (16#39#);
   --  Values used for embedded plus signs in nonseparate formats

   subtype COBOL_Minus_Digits is COBOL_Character
     range COBOL_Character'Val (16#20#) .. COBOL_Character'Val (16#29#);
   --  Values used for embedded minus signs in nonseparate formats

end Interfaces.COBOL;
