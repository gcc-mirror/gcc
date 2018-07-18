------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . S T R I N G S . U T F _ E N C O D I N G             --
--                                                                          --
--                                 S p e c                                  --
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

--  This is one of the Ada 2012 package defined in AI05-0137-1. It is a parent
--  package that contains declarations used in the child packages for handling
--  UTF encoded strings. Note: this package is consistent with Ada 95, and may
--  be used in Ada 95 or Ada 2005 mode.

with Interfaces;
with Unchecked_Conversion;

package Ada.Strings.UTF_Encoding is
   pragma Pure (UTF_Encoding);

   subtype UTF_String is String;
   --  Used to represent a string of 8-bit values containing a sequence of
   --  values encoded in one of three ways (UTF-8, UTF-16BE, or UTF-16LE).
   --  Typically used in connection with a Scheme parameter indicating which
   --  of the encodings applies. This is not strictly a String value in the
   --  sense defined in the Ada RM, but in practice type String accommodates
   --  all possible 256 codes, and can be used to hold any sequence of 8-bit
   --  codes. We use String directly rather than create a new type so that
   --  all existing facilities for manipulating type String (e.g. the child
   --  packages of Ada.Strings) are available for manipulation of UTF_Strings.

   type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE);
   --  Used to specify which of three possible encodings apply to a UTF_String

   subtype UTF_8_String is String;
   --  Similar to UTF_String but specifically represents a UTF-8 encoded string

   subtype UTF_16_Wide_String is Wide_String;
   --  This is similar to UTF_8_String but is used to represent a Wide_String
   --  value which is a sequence of 16-bit values encoded using UTF-16. Again
   --  this is not strictly a Wide_String in the sense of the Ada RM, but the
   --  type Wide_String can be used to represent a sequence of arbitrary 16-bit
   --  values, and it is more convenient to use Wide_String than a new type.

   Encoding_Error : exception;
   --  This exception is raised in the following situations:
   --    a) A UTF encoded string contains an invalid encoding sequence
   --    b) A UTF-16BE or UTF-16LE input string has an odd length
   --    c) An incorrect character value is present in the Input string
   --    d) The result for a Wide_Character output exceeds 16#FFFF#
   --  The exception message has the index value where the error occurred.

   --  The BOM (BYTE_ORDER_MARK) values defined here are used at the start of
   --  a string to indicate the encoding. The convention in this package is
   --  that on input a correct BOM is ignored and an incorrect BOM causes an
   --  Encoding_Error exception. On output, the output string may or may not
   --  include a BOM depending on the setting of Output_BOM.

   BOM_8    : constant UTF_8_String :=
                Character'Val (16#EF#) &
                Character'Val (16#BB#) &
                Character'Val (16#BF#);

   BOM_16BE : constant UTF_String :=
                Character'Val (16#FE#) &
                Character'Val (16#FF#);

   BOM_16LE : constant UTF_String :=
                Character'Val (16#FF#) &
                Character'Val (16#FE#);

   BOM_16   : constant UTF_16_Wide_String :=
                (1 => Wide_Character'Val (16#FEFF#));

   function Encoding
     (Item    : UTF_String;
      Default : Encoding_Scheme := UTF_8) return Encoding_Scheme;
   --  This function inspects a UTF_String value to determine whether it
   --  starts with a BOM for UTF-8, UTF-16BE, or UTF_16LE. If so, the result
   --  is the scheme corresponding to the BOM. If no valid BOM is present
   --  then the result is the specified Default value.

private
   function To_Unsigned_8 is new
     Unchecked_Conversion (Character, Interfaces.Unsigned_8);

   function To_Unsigned_16 is new
     Unchecked_Conversion (Wide_Character, Interfaces.Unsigned_16);

   function To_Unsigned_32 is new
     Unchecked_Conversion (Wide_Wide_Character, Interfaces.Unsigned_32);

   subtype UTF_XE_Encoding is Encoding_Scheme range UTF_16BE .. UTF_16LE;
   --  Subtype containing only UTF_16BE and UTF_16LE entries

   --  Utility routines for converting between UTF-16 and UTF-16LE/BE

   function From_UTF_16
     (Item          : UTF_16_Wide_String;
      Output_Scheme : UTF_XE_Encoding;
      Output_BOM    : Boolean := False) return UTF_String;
   --  The input string Item is encoded in UTF-16. The output is encoded using
   --  Output_Scheme (which is either UTF-16LE or UTF-16BE). There are no error
   --  cases. The output starts with BOM_16BE/LE if Output_BOM is True.

   function To_UTF_16
     (Item          : UTF_String;
      Input_Scheme  : UTF_XE_Encoding;
      Output_BOM    : Boolean := False) return UTF_16_Wide_String;
   --  The input string Item is encoded using Input_Scheme which is either
   --  UTF-16LE or UTF-16BE. The output is the corresponding UTF_16 wide
   --  string. Encoding error is raised if the length of the input is odd.
   --  The output starts with BOM_16 if Output_BOM is True.

   procedure Raise_Encoding_Error (Index : Natural);
   pragma No_Return (Raise_Encoding_Error);
   --  Raise Encoding_Error exception for bad encoding in input item. The
   --  parameter Index is the index of the location in Item for the error.

end Ada.Strings.UTF_Encoding;
