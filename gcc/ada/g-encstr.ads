------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . E N C O D E _ S T R I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2010, AdaCore                     --
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

--  This generic package provides utility routines for converting from
--  Wide_String or Wide_Wide_String to encoded String using a specified
--  encoding convention, which is supplied as the generic parameter. If
--  this parameter is a known at compile time constant (e.g. a constant
--  defined in System.WCh_Con), the instantiation is specialized so that
--  it applies only to this specified coding.

--  Note: this package is only about encoding sequences of 16- or 32-bit
--  characters into a sequence of 8-bit codes. It knows nothing at all about
--  the character encodings being used for the input Wide_Character and
--  Wide_Wide_Character values, although some of the encoding methods (notably
--  JIS and EUC) have built in assumptions about the range of possible input
--  code values. Most often the input will be Unicode/ISO-10646 as specified by
--  the Ada RM, but this package does not make any assumptions about the
--  character coding, and in the case of UTF-8 all possible code values can be
--  encoded. See also the packages Ada.Wide_[Wide_]Characters.Unicode for
--  unicode specific functions.

--  Note on brackets encoding (WCEM_Brackets). On input, upper half characters
--  can be represented as ["hh"] but the routines in this package will only use
--  brackets encodings for codes higher than 16#FF#, so upper half characters
--  will be output as single Character values.

with System.WCh_Con;

generic
   Encoding_Method : System.WCh_Con.WC_Encoding_Method;

package GNAT.Encode_String is
   pragma Pure;

   function Encode_Wide_String (S : Wide_String) return String;
   pragma Inline (Encode_Wide_String);
   --  Encode the given Wide_String, returning a String encoded using the
   --  given encoding method. Constraint_Error will be raised if the encoding
   --  method cannot accommodate the input data.

   procedure Encode_Wide_String
     (S      : Wide_String;
      Result : out String;
      Length : out Natural);
   --  Encode the given Wide_String, storing the encoded string in Result,
   --  with Length being set to the length of the encoded string. The caller
   --  must ensure that Result is long enough (see useful constants defined
   --  in System.WCh_Con: WC_Longest_Sequence, WC_Longest_Sequences). If the
   --  length of Result is insufficient Constraint_Error will be raised.
   --  Constraint_Error will also be raised if the encoding method cannot
   --  accommodate the input data.

   function Encode_Wide_Wide_String (S : Wide_Wide_String) return String;
   pragma Inline (Encode_Wide_Wide_String);
   --  Same as above function but for Wide_Wide_String input

   procedure Encode_Wide_Wide_String
     (S      : Wide_Wide_String;
      Result : out String;
      Length : out Natural);
   --  Same as above procedure, but for Wide_Wide_String input

   procedure Encode_Wide_Character
     (Char   : Wide_Character;
      Result : in out String;
      Ptr    : in out Natural);
   pragma Inline (Encode_Wide_Character);
   --  This is a lower level procedure that encodes the single character Char.
   --  The output is stored in Result starting at Result (Ptr), and Ptr is
   --  updated past the stored value. Constraint_Error is raised if Result
   --  is not long enough to accommodate the result, or if the encoding method
   --  specified does not accommodate the input character value, or if Ptr is
   --  outside the bounds of the Result string.

   procedure Encode_Wide_Wide_Character
     (Char   : Wide_Wide_Character;
      Result : in out String;
      Ptr    : in out Natural);
   --  Same as above procedure but with Wide_Wide_Character input

end GNAT.Encode_String;
