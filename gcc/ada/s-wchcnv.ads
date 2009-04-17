------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ C N V                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This package contains generic subprograms used for converting between
--  sequences of Character and Wide_Character. Wide_Wide_Character values
--  are also handled, but represented using integer range types defined in
--  this package, so that this package can be used from applications that
--  are restricted to Ada 95 compatibility (such as the compiler itself).

--  All the algorithms for encoding and decoding are isolated in this package
--  and in System.WCh_JIS and should not be duplicated elsewhere. The only
--  exception to this is that GNAT.Decode_String and GNAT.Encode_String have
--  their own circuits for UTF-8 conversions, for improved efficiency.

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

pragma Compiler_Unit;

with System.WCh_Con;

package System.WCh_Cnv is
   pragma Pure;

   type UTF_32_Code is range 0 .. 16#7FFF_FFFF#;
   for UTF_32_Code'Size use 32;
   --  Range of allowed UTF-32 encoding values

   type UTF_32_String is array (Positive range <>) of UTF_32_Code;

   generic
      with function In_Char return Character;
   function Char_Sequence_To_Wide_Char
     (C  : Character;
      EM : System.WCh_Con.WC_Encoding_Method) return Wide_Character;
   --  C is the first character of a sequence of one or more characters which
   --  represent a wide character sequence. Calling the function In_Char for
   --  additional characters as required, Char_To_Wide_Char returns the
   --  corresponding wide character value. Constraint_Error is raised if the
   --  sequence of characters encountered is not a valid wide character
   --  sequence for the given encoding method.
   --
   --  Note on the use of brackets encoding (WCEM_Brackets). The brackets
   --  encoding method is ambiguous in the context of this function, since
   --  there is no way to tell if ["1234"] is eight unencoded characters or
   --  one encoded character. In the context of Ada sources, any sequence
   --  starting [" must be the start of an encoding (since that sequence is
   --  not valid in Ada source otherwise). The routines in this package use
   --  the same approach. If the input string contains the sequence [" then
   --  this is assumed to be the start of a brackets encoding sequence, and
   --  if it does not match the syntax, an error is raised.

   generic
      with function In_Char return Character;
   function Char_Sequence_To_UTF_32
     (C  : Character;
      EM : System.WCh_Con.WC_Encoding_Method) return UTF_32_Code;
   --  This is similar to the above, but the function returns a code from
   --  the full UTF_32 code set, which covers the full range of possible
   --  values in Wide_Wide_Character. The result can be converted to
   --  Wide_Wide_Character form using Wide_Wide_Character'Val.

   generic
      with procedure Out_Char (C : Character);
   procedure Wide_Char_To_Char_Sequence
     (WC : Wide_Character;
      EM : System.WCh_Con.WC_Encoding_Method);
   --  Given a wide character, converts it into a sequence of one or
   --  more characters, calling the given Out_Char procedure for each.
   --  Constraint_Error is raised if the given wide character value is
   --  not a valid value for the given encoding method.
   --
   --  Note on brackets encoding (WCEM_Brackets). For the input routines above,
   --  upper half characters can be represented as ["hh"] but this procedure
   --  will only use brackets encodings for codes higher than 16#FF#, so upper
   --  half characters will be output as single Character values.

   generic
      with procedure Out_Char (C : Character);
   procedure UTF_32_To_Char_Sequence
     (Val : UTF_32_Code;
      EM  : System.WCh_Con.WC_Encoding_Method);
   --  This is similar to the above, but the input value is a code from the
   --  full UTF_32 code set, which covers the full range of possible values
   --  in Wide_Wide_Character. To convert a Wide_Wide_Character value, the
   --  caller can use Wide_Wide_Character'Pos in the call.

end System.WCh_Cnv;
