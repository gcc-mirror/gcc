------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . D E C O D E _ S T R I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

--  This generic package provides utility routines for converting from an
--  encoded string to a corresponding Wide_String or Wide_Wide_String value
--  using a specified encoding convention, which is supplied as the generic
--  parameter. UTF-8 is handled especially efficiently, and if the encoding
--  method is known at compile time to be WCEM_UTF8, then the instantiation
--  is specialized to handle only the UTF-8 case and exclude code for the
--  other encoding methods. The package also provides positioning routines
--  for skipping encoded characters in either direction, and for validating
--  strings for correct encodings.

--  Note: this package is only about decoding sequences of 8-bit characters
--  into corresponding 16-bit Wide_String or 32-bit Wide_Wide_String values.
--  It knows nothing at all about the character encodings being used for the
--  resulting Wide_Character and Wide_Wide_Character values. Most often this
--  will be Unicode/ISO-10646 as specified by the Ada RM, but this package
--  does not make any assumptions about the character coding. See also the
--  packages Ada.Wide_[Wide_]Characters.Unicode for unicode specific functions.

--  In particular, in the case of UTF-8, all valid UTF-8 encodings, as listed
--  in table 3.6 of the Unicode Standard, version 6.2.0, are recognized as
--  legitimate. This includes the full range 16#0000_0000# .. 16#03FF_FFFF#.
--  This includes codes in the range 16#D800# - 16#DFFF#. These codes all
--  have UTF-8 encoding sequences that are well-defined (e.g. the encoding for
--  16#D800# is ED A0 80). But these codes do not correspond to defined Unicode
--  characters and are thus considered to be "not well-formed" (see table 3.7
--  of the Unicode Standard). If you need to exclude these codes, you must do
--  that manually, e.g. use Decode_Wide_Character/Decode_Wide_String and check
--  that the resulting code(s) are not in this range.

--  Note on the use of brackets encoding (WCEM_Brackets). The brackets encoding
--  method is ambiguous in the context of this package, since there is no way
--  to tell if ["1234"] is eight unencoded characters or one encoded character.
--  In the context of Ada sources, any sequence starting [" must be the start
--  of an encoding (since that sequence is not valid in Ada source otherwise).
--  The routines in this package use the same approach. If the input string
--  contains the sequence [" then this is assumed to be the start of a brackets
--  encoding sequence, and if it does not match the syntax, an error is raised.
--  In the case of the Prev functions, a sequence ending with "] is assumed to
--  be a valid brackets sequence, and an error is raised if it is not.

with System.WCh_Con;

generic
   Encoding_Method : System.WCh_Con.WC_Encoding_Method;

package GNAT.Decode_String is
   pragma Pure;

   function Decode_Wide_String (S : String) return Wide_String;
   pragma Inline (Decode_Wide_String);
   --  Decode the given String, which is encoded using the indicated coding
   --  method, returning the corresponding decoded Wide_String value. If S
   --  contains a character code that cannot be represented with the given
   --  encoding, then Constraint_Error is raised.

   procedure Decode_Wide_String
     (S      : String;
      Result : out Wide_String;
      Length : out Natural);
   --  Similar to the above function except that the result is stored in the
   --  given Wide_String variable Result, starting at Result (Result'First). On
   --  return, Length is set to the number of characters stored in Result. The
   --  caller must ensure that Result is long enough (an easy choice is to set
   --  the length equal to the S'Length, since decoding can never increase the
   --  string length). If the length of Result is insufficient Constraint_Error
   --  will be raised.

   function Decode_Wide_Wide_String (S : String) return Wide_Wide_String;
   --  Same as above function but for Wide_Wide_String output

   procedure Decode_Wide_Wide_String
     (S      : String;
      Result : out Wide_Wide_String;
      Length : out Natural);
   --  Same as above procedure, but for Wide_Wide_String output

   function Validate_Wide_String (S : String) return Boolean;
   --  This function inspects the string S to determine if it contains only
   --  valid encodings corresponding to Wide_Character values using the
   --  given encoding. If a call to Decode_Wide_String (S) would return
   --  without raising Constraint_Error, then Validate_Wide_String will
   --  return True. If the call would have raised Constraint_Error, then
   --  Validate_Wide_String will return False.

   function Validate_Wide_Wide_String (S : String) return Boolean;
   --  Similar to Validate_Wide_String, except that it succeeds if the string
   --  contains only encodings corresponding to Wide_Wide_Character values.

   procedure Decode_Wide_Character
     (Input  : String;
      Ptr    : in out Natural;
      Result : out Wide_Character);
   pragma Inline (Decode_Wide_Character);
   --  This is a lower level procedure that decodes a single character using
   --  the given encoding method. The encoded character is stored in Input,
   --  starting at Input (Ptr). The resulting output character is stored in
   --  Result, and on return Ptr is updated past the input character or
   --  encoding sequence. Constraint_Error will be raised if the input has
   --  has a character that cannot be represented using the given encoding,
   --  or if Ptr is outside the bounds of the Input string.

   procedure Decode_Wide_Wide_Character
     (Input  : String;
      Ptr    : in out Natural;
      Result : out Wide_Wide_Character);
   pragma Inline (Decode_Wide_Wide_Character);
   --  Same as above procedure but with Wide_Wide_Character input

   procedure Next_Wide_Character (Input : String; Ptr : in out Natural);
   pragma Inline (Next_Wide_Character);
   --  This procedure examines the input string starting at Input (Ptr), and
   --  advances Ptr past one character in the encoded string, so that on return
   --  Ptr points to the next encoded character. Constraint_Error is raised if
   --  an invalid encoding is encountered, or the end of the string is reached
   --  or if Ptr is less than String'First on entry, or if the character
   --  skipped is not a valid Wide_Character code.

   procedure Prev_Wide_Character (Input : String; Ptr : in out Natural);
   --  This procedure is similar to Next_Encoded_Character except that it moves
   --  backwards in the string, so that on return, Ptr is set to point to the
   --  previous encoded character. Constraint_Error is raised if the start of
   --  the string is encountered. It is valid for Ptr to be one past the end
   --  of the string for this call (in which case on return it will point to
   --  the last encoded character).
   --
   --  Note: it is not generally possible to do this function efficiently with
   --  all encodings, the current implementation is only efficient for the case
   --  of UTF-8 (Encoding_Method = WCEM_UTF8) and Brackets (Encoding_Method =
   --  WCEM_Brackets). For all other encodings, we work by starting at the
   --  beginning of the string and moving forward till Ptr is reached, which
   --  is correct but slow.
   --
   --  Note: this routine assumes that the sequence prior to Ptr is correctly
   --  encoded, it does not have a defined behavior if this is not the case.

   procedure Next_Wide_Wide_Character (Input : String; Ptr : in out Natural);
   pragma Inline (Next_Wide_Wide_Character);
   --  Similar to Next_Wide_Character except that codes skipped must be valid
   --  Wide_Wide_Character codes.

   procedure Prev_Wide_Wide_Character (Input : String; Ptr : in out Natural);
   --  Similar to Prev_Wide_Character except that codes skipped must be valid
   --  Wide_Wide_Character codes.

end GNAT.Decode_String;
