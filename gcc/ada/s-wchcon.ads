------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ C O N                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

--  This package defines the codes used to identify the encoding method for
--  wide characters in string and character constants. This is needed both
--  at compile time and at runtime (for the wide character runtime routines)

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

package System.WCh_Con is
pragma Pure (WCh_Con);

   -------------------------------------
   -- Wide_Character Encoding Methods --
   -------------------------------------

   --  A wide character encoding method is a method for uniquely representing
   --  a Wide_Character or Wide_Wide_Character value using a one or more
   --  Character values. Three types of encoding method are supported by GNAT:

   --    An escape encoding method uses ESC as the first character of the
   --    sequence, and subsequent characters determine the wide character
   --    value that is represented. Any character other than ESC stands
   --    for itself as a single byte (i.e. any character in Latin-1, other
   --    than ESC itself, is represented as a single character: itself).

   --    An upper half encoding method uses a character in the upper half
   --    range (i.e. in the range 16#80# .. 16#FF#) as the first byte of
   --    a wide character encoding sequence. Subsequent characters are
   --    used to determine the wide character value that is represented.
   --    Any character in the lower half (16#00# .. 16#7F#) represents
   --    itself as a single character.

   --    The brackets notation, where a wide character is represented by the
   --    sequence ["xx"] or ["xxxx"] or ["xxxxxx"] where xx are hexadecimal
   --    characters. Note that currently this is the only encoding that
   --    supports the full UTF-32 range.

   --  Note that GNAT does not currently support escape-in, escape-out
   --  encoding methods, where an escape sequence is used to set a mode
   --  used to recognize subsequent characters. All encoding methods use
   --  individual character-by-character encodings, so that a sequence of
   --  wide characters is represented by a sequence of encodings.

   --  To add new encoding methods, the following steps are required:

   --     1.  Define a code for a new value of type WC_Encoding_Method
   --     2.  Adjust the definition of WC_Encoding_Method accordingly
   --     3.  Provide appropriate conversion routines in System.Wch_Cnv
   --     4.  Adjust definition of WC_Longest_Sequence if necessary
   --     5.  Add an entry in WC_Encoding_Letters for the new method
   --     6.  Add proper code to s-wchstw.adb, s-wchwts.adb, s-widwch.adb

   --  Note that the WC_Encoding_Method values must be kept ordered so that
   --  the definitions of the subtypes WC_Upper_Half_Encoding_Method and
   --  WC_ESC_Encoding_Method are still correct.

   ---------------------------------
   -- Encoding Method Definitions --
   ---------------------------------

   type WC_Encoding_Method is range 1 .. 6;
   --  Type covering the range of values used to represent wide character
   --  encoding methods. An enumeration type might be a little neater, but
   --  more trouble than it's worth, given the need to pass these values
   --  from the compiler to the backend, and to record them in the ALI file.

   WCEM_Hex : constant WC_Encoding_Method := 1;
   --  The wide character with code 16#abcd# is represented by the escape
   --  sequence ESC a b c d (five characters, where abcd are ASCII hex
   --  characters, using upper case for letters). This method is easy
   --  to deal with in external environments that do not support wide
   --  characters, and covers the whole BMP. This is the default encoding
   --  method.

   WCEM_Upper : constant WC_Encoding_Method := 2;
   --  The wide character with encoding 16#abcd#, where the upper bit is on
   --  (i.e. a is in the range 8-F) is represented as two bytes 16#ab# and
   --  16#cd#. The second byte may never be a format control character, but
   --  is not required to be in the upper half. This method can be also used
   --  for shift-JIS or EUC where the internal coding matches the external
   --  coding.

   WCEM_Shift_JIS : constant WC_Encoding_Method := 3;
   --  A wide character is represented by a two character sequence 16#ab#
   --  and 16#cd#, with the restrictions described for upper half encoding
   --  as described above. The internal character code is the corresponding
   --  JIS character according to the standard algorithm for Shift-JIS
   --  conversion. See the body of package System.JIS_Conversions for
   --  further details.

   WCEM_EUC : constant WC_Encoding_Method := 4;
   --  A wide character is represented by a two character sequence 16#ab# and
   --  16#cd#, with both characters being in the upper half set. The internal
   --  character code is the corresponding JIS character according to the EUC
   --  encoding algorithm. See the body of package System.JIS_Conversions for
   --  further details.

   WCEM_UTF8 : constant WC_Encoding_Method := 5;
   --  An ISO 10646-1 BMP/Unicode wide character is represented in
   --  UCS Transformation Format 8 (UTF-8) as defined in Annex R of ISO
   --  10646-1/Am.2.  Depending on the character value, a Unicode character
   --  is represented as the one to six byte sequence.
   --
   --    16#0000_0000#-16#0000_007f#: 2#0xxxxxxx#
   --    16#0000_0080#-16#0000_07ff#: 2#110xxxxx# 2#10xxxxxx#
   --    16#0000_0800#-16#0000_ffff#: 2#1110xxxx# 2#10xxxxxx# 2#10xxxxxx#
   --    16#0001_0000#-16#001F_FFFF#: 2#11110xxx# 2#10xxxxxx# 2#10xxxxxx#
   --                                 2#10xxxxxx#
   --    16#0020_0000#-16#03FF_FFFF#: 2#111110xx# 2#10xxxxxx# 2#10xxxxxx#
   --                                 2#10xxxxxx# 2#10xxxxxx#
   --    16#0400_0000#-16#7FFF_FFFF#: 2#1111110x# 2#10xxxxxx# 2#10xxxxxx#
   --                                 2#10xxxxxx# 2#10xxxxxx# 2#10xxxxxx#
   --
   --  where the xxx bits correspond to the left-padded bits of the
   --  16-bit character value. Note that all lower half ASCII characters
   --  are represented as ASCII bytes and all upper half characters and
   --  other wide characters are represented as sequences of upper-half.

   WCEM_Brackets : constant WC_Encoding_Method := 6;
   --  A wide character is represented using one of the following sequences:
   --
   --    ["xx"]
   --    ["xxxx"]
   --    ["xxxxxx"]
   --    ["xxxxxxxx"]
   --
   --  where xx are hexadecimal digits representing the character code.

   WC_Encoding_Letters : constant array (WC_Encoding_Method) of Character :=
     (WCEM_Hex       => 'h',
      WCEM_Upper     => 'u',
      WCEM_Shift_JIS => 's',
      WCEM_EUC       => 'e',
      WCEM_UTF8      => '8',
      WCEM_Brackets  => 'b');
   --  Letters used for selection of wide character encoding method in the
   --  compiler options (-gnatW? switch) and for Wide_Text_IO (WCEM parameter
   --  in the form string).

   subtype WC_ESC_Encoding_Method is
     WC_Encoding_Method range WCEM_Hex .. WCEM_Hex;
   --  Encoding methods using an ESC character at the start of the sequence.

   subtype WC_Upper_Half_Encoding_Method is
     WC_Encoding_Method range WCEM_Upper .. WCEM_UTF8;
   --  Encoding methods using an upper half character (16#80#..16#FF) at
   --  the start of the sequence.

   WC_Longest_Sequence : constant := 10;
   --  The longest number of characters that can be used for a wide character
   --  or wide wide character sequence for any of the active encoding methods.

end System.WCh_Con;
