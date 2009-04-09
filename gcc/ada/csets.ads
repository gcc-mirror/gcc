------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C S E T S                                 --
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

package Csets is
   pragma Elaborate_Body;

   --  This package contains character tables for the various character
   --  sets that are supported for source representation. Character and
   --  string literals are not affected, only identifiers. For each set,
   --  the table in this package gives the mapping of letters to their
   --  upper case equivalent. Each table thus provides the information
   --  for building the table used to fold lower case to upper case, and
   --  also the table of flags showing which characters are allowed in
   --  identifiers.

   type Translate_Table is array (Character) of Character;
   --  Type used to describe translate tables

   type Char_Array_Flags is array (Character) of Boolean;
   --  Type used for character attribute arrays. Note that we deliberately
   --  do NOT pack this table, since we don't want the extra overhead of
   --  accessing a packed bit string.

   ----------------------------------------------
   -- Character Tables For Current Compilation --
   ----------------------------------------------

   procedure Initialize;
   --  Routine to initialize following character tables, whose content depends
   --  on the character code being used to represent the source program. In
   --  particular, the use of the upper half of the 8-bit code set varies.
   --  The character set in use is specified by the value stored in
   --  Opt.Identifier_Character_Set, which has the following settings:

   --    '1'  Latin-1 (ISO-8859-1)
   --    '2'  Latin-2 (ISO-8859-2)
   --    '3'  Latin-3 (ISO-8859-3)
   --    '4'  Latin-4 (ISO-8859-4)
   --    '5'  Latin-5 (ISO-8859-5, Cyrillic)
   --    'p'  IBM PC  (code page 437)
   --    '8'  IBM PC  (code page 850)
   --    '9'  Latin-9 (ISO-9959-9)
   --    'f'  Full upper set (all distinct)
   --    'n'  No upper characters (Ada/83 rules)
   --    'w'  Latin-1 plus wide characters also allowed

   function Is_Upper_Case_Letter (C : Character) return Boolean;
   pragma Inline (Is_Upper_Case_Letter);
   --  Determine if character is upper case letter

   function Is_Lower_Case_Letter (C : Character) return Boolean;
   pragma Inline (Is_Lower_Case_Letter);
   --  Determine if character is lower case letter

   Fold_Upper : Translate_Table;
   --  Table to fold lower case identifier letters to upper case

   Fold_Lower : Translate_Table;
   --  Table to fold upper case identifier letters to lower case

   Identifier_Char : Char_Array_Flags;
   --  This table has True entries for all characters that can legally appear
   --  in identifiers, including digits, the underline character, all letters
   --  including upper and lower case and extended letters (as controlled by
   --  the setting of Opt.Identifier_Character_Set, left bracket for brackets
   --  notation wide characters and also ESC if wide characters are permitted
   --  in identifiers using escape sequences starting with ESC.

end Csets;
