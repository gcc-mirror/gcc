------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                          G N A T . U T F _ 3 2                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2005 Free Software Foundation, Inc.            --
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

--  This package is an internal package that provides basic character
--  classification capabilities needed by the compiler for handling full
--  32-bit wide wide characters. We avoid the use of the actual type
--  Wide_Wide_Character, since we want to use these routines in the compiler
--  itself, and we want to be able to compile the compiler with old versions
--  of GNAT that did not implement Wide_Wide_Character.

--  This package is not available directly for use in application programs,
--  but it serves as the basis for GNAT.Wide_Case_Utilities and
--  GNAT.Wide_Wide_Case_Utilities, which can be used directly.

package GNAT.UTF_32 is

   type UTF_32 is mod 2 ** 32;
   --  The actual allowed range is 16#00_0000# .. 16#01_FFFF#

   function Is_UTF_32_Letter (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Letter);
   --  Returns true iff U is a letter that can be used to start an identifier.
   --  This means that it is in one of the following categories:
   --    Letter, Uppercase (Lu)
   --    Letter, Lowercase (Ll)
   --    Letter, Titlecase (Lt)
   --    Letter, Modifier  (Lm)
   --    Letter, Other     (Lo)
   --    Number, Letter    (Nl)

   function Is_UTF_32_Digit (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Digit);
   --  Returns true iff U is a digit that can be used to extend an identifer,
   --  which means it is in one of the following categories:
   --    Number, Decimal_Digit (Nd)

   function Is_UTF_32_Line_Terminator (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Line_Terminator);
   --  Returns true iff U is an allowed line terminator for source programs,
   --  which means it is in one of the following categories:
   --    Separator, Line (Zl)
   --    Separator, Paragraph (Zp)
   --  or that it is a conventional line terminator (CR, LF, VT, FF)

   function Is_UTF_32_Mark (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Mark);
   --  Returns true iff U is a mark character which can be used to extend
   --  an identifier. This means it is in one of the following categories:
   --    Mark, Non-Spacing (Mn)
   --    Mark, Spacing Combining (Mc)

   function Is_UTF_32_Other (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Other);
   --  Returns true iff U is an other format character, which means that it
   --  can be used to extend an identifier, but is ignored for the purposes of
   --  matching of identiers. This means that it is in one of the following
   --  categories:
   --    Other, Format (Cf)

   function Is_UTF_32_Punctuation (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Punctuation);
   --  Returns true iff U is a punctuation character that can be used to
   --  separate pices of an identifier. This means that it is in one of the
   --  following categories:
   --    Punctuation, Connector (Pc)

   function Is_UTF_32_Space (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Space);
   --  Returns true iff U is considered a space to be ignored, which means
   --  that it is in one of the following categories:
   --    Separator, Space (Zs)

   function Is_UTF_32_Non_Graphic (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Non_Graphic);
   --  Returns true iff U is considered to be a non-graphic character,
   --  which means that it is in one of the following categories:
   --    Other, Control (Cc)
   --    Other, Private Use (Co)
   --    Other, Surrogate (Cs)
   --    Other, Format (Cf)
   --    Separator, Line (Zl)
   --    Separator, Paragraph (Zp)
   --
   --  Note that the Ada category format effector is subsumed by the above
   --  list of Unicode categories.

   function UTF_32_To_Upper_Case (U : UTF_32) return UTF_32;
   pragma Inline (UTF_32_To_Upper_Case);
   --  If U represents a lower case letter, returns the corresponding upper
   --  case letter, otherwise U is returned unchanged. The folding is locale
   --  independent as defined by documents referenced in the note in section
   --  1 of ISO/IEC 10646:2003

end GNAT.UTF_32;
