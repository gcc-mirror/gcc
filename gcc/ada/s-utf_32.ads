------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . U T F _ 3 2                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2005-2009, Free Software Foundation, Inc.         --
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

--  This package is an internal package that provides basic character
--  classification capabilities needed by the compiler for handling full
--  32-bit wide wide characters. We avoid the use of the actual type
--  Wide_Wide_Character, since we want to use these routines in the compiler
--  itself, and we want to be able to compile the compiler with old versions
--  of GNAT that did not implement Wide_Wide_Character.

--  System.UTF_32 should not be directly used from an application program, but
--  an equivalent package GNAT.UTF_32 can be used directly and provides exactly
--  the same services. The reason this package is in System is so that it can
--  with'ed by other packages in the Ada and System hierarchies.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package System.UTF_32 is

   type UTF_32 is range 0 .. 16#7FFF_FFFF#;
   --  So far, the only defined character codes are in 0 .. 16#01_FFFF#

   --  The following type defines the categories from the unicode definitions.
   --  The one addition we make is Fe, which represents the characters FFFE
   --  and FFFF in any of the planes.

   type Category is (
     Cc,   --  Other, Control
     Cf,   --  Other, Format
     Cn,   --  Other, Not Assigned
     Co,   --  Other, Private Use
     Cs,   --  Other, Surrogate
     Ll,   --  Letter, Lowercase
     Lm,   --  Letter, Modifier
     Lo,   --  Letter, Other
     Lt,   --  Letter, Titlecase
     Lu,   --  Letter, Uppercase
     Mc,   --  Mark, Spacing Combining
     Me,   --  Mark, Enclosing
     Mn,   --  Mark, Nonspacing
     Nd,   --  Number, Decimal Digit
     Nl,   --  Number, Letter
     No,   --  Number, Other
     Pc,   --  Punctuation, Connector
     Pd,   --  Punctuation, Dash
     Pe,   --  Punctuation, Close
     Pf,   --  Punctuation, Final quote
     Pi,   --  Punctuation, Initial quote
     Po,   --  Punctuation, Other
     Ps,   --  Punctuation, Open
     Sc,   --  Symbol, Currency
     Sk,   --  Symbol, Modifier
     Sm,   --  Symbol, Math
     So,   --  Symbol, Other
     Zl,   --  Separator, Line
     Zp,   --  Separator, Paragraph
     Zs,   --  Separator, Space
     Fe);  --  relative position FFFE/FFFF in any plane

   function Get_Category (U : UTF_32) return Category;
   --  Given a UTF32 code, returns corresponding Category, or Cn if
   --  the code does not have an assigned unicode category.

   --  The following functions perform category tests corresponding to lexical
   --  classes defined in the Ada standard. There are two interfaces for each
   --  function. The second takes a Category (e.g. returned by Get_Category).
   --  The first takes a UTF_32 code. The form taking the UTF_32 code is
   --  typically more efficient than calling Get_Category, but if several
   --  different tests are to be performed on the same code, it is more
   --  efficient to use Get_Category to get the category, then test the
   --  resulting category.

   function Is_UTF_32_Letter (U : UTF_32)   return Boolean;
   function Is_UTF_32_Letter (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Letter);
   --  Returns true iff U is a letter that can be used to start an identifier,
   --  or if C is one of the corresponding categories, which are the following:
   --    Letter, Uppercase (Lu)
   --    Letter, Lowercase (Ll)
   --    Letter, Titlecase (Lt)
   --    Letter, Modifier  (Lm)
   --    Letter, Other     (Lo)
   --    Number, Letter    (Nl)

   function Is_UTF_32_Digit (U : UTF_32)   return Boolean;
   function Is_UTF_32_Digit (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Digit);
   --  Returns true iff U is a digit that can be used to extend an identifier,
   --  or if C is one of the corresponding categories, which are the following:
   --    Number, Decimal_Digit (Nd)

   function Is_UTF_32_Line_Terminator (U : UTF_32) return Boolean;
   pragma Inline (Is_UTF_32_Line_Terminator);
   --  Returns true iff U is an allowed line terminator for source programs,
   --  if U is in the category Zp (Separator, Paragraph), or Zs (Separator,
   --  Line), or if U is a conventional line terminator (CR, LF, VT, FF).
   --  There is no category version for this function, since the set of
   --  characters does not correspond to a set of Unicode categories.

   function Is_UTF_32_Mark (U : UTF_32)   return Boolean;
   function Is_UTF_32_Mark (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Mark);
   --  Returns true iff U is a mark character which can be used to extend an
   --  identifier, or if C is one of the corresponding categories, which are
   --  the following:
   --    Mark, Non-Spacing (Mn)
   --    Mark, Spacing Combining (Mc)

   function Is_UTF_32_Other (U : UTF_32)   return Boolean;
   function Is_UTF_32_Other (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Other);
   --  Returns true iff U is an other format character, which means that it
   --  can be used to extend an identifier, but is ignored for the purposes of
   --  matching of identifiers, or if C is one of the corresponding categories,
   --  which are the following:
   --    Other, Format (Cf)

   function Is_UTF_32_Punctuation (U : UTF_32)   return Boolean;
   function Is_UTF_32_Punctuation (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Punctuation);
   --  Returns true iff U is a punctuation character that can be used to
   --  separate pieces of an identifier, or if C is one of the corresponding
   --  categories, which are the following:
   --    Punctuation, Connector (Pc)

   function Is_UTF_32_Space (U : UTF_32)   return Boolean;
   function Is_UTF_32_Space (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Space);
   --  Returns true iff U is considered a space to be ignored, or if C is one
   --  of the corresponding categories, which are the following:
   --    Separator, Space (Zs)

   function Is_UTF_32_Non_Graphic (U : UTF_32)   return Boolean;
   function Is_UTF_32_Non_Graphic (C : Category) return Boolean;
   pragma Inline (Is_UTF_32_Non_Graphic);
   --  Returns true iff U is considered to be a non-graphic character, or if C
   --  is one of the corresponding categories, which are the following:
   --    Other, Control (Cc)
   --    Other, Private Use (Co)
   --    Other, Surrogate (Cs)
   --    Separator, Line (Zl)
   --    Separator, Paragraph (Zp)
   --    FFFE or FFFF positions in any plane (Fe)
   --
   --  Note that the Ada category format effector is subsumed by the above
   --  list of Unicode categories.
   --
   --  Note that Other, Unassigned (Cn) is quite deliberately not included
   --  in the list of categories above. This means that should any of these
   --  code positions be defined in future with graphic characters they will
   --  be allowed without a need to change implementations or the standard.
   --
   --  Note that Other, Format (Cf) is also quite deliberately not included
   --  in the list of categories above. This means that these characters can
   --  be included in character and string literals.

   --  The following function is used to fold to upper case, as required by
   --  the Ada 2005 standard rules for identifier case folding. Two
   --  identifiers are equivalent if they are identical after folding all
   --  letters to upper case using this routine.

   function UTF_32_To_Upper_Case (U : UTF_32) return UTF_32;
   pragma Inline (UTF_32_To_Upper_Case);
   --  If U represents a lower case letter, returns the corresponding upper
   --  case letter, otherwise U is returned unchanged. The folding is locale
   --  independent as defined by documents referenced in the note in section
   --  1 of ISO/IEC 10646:2003

end System.UTF_32;
