------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C S E T S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

package Csets is
pragma Elaborate_Body (Csets);

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

   -----------------------------------------------
   --  Character Tables For Current Compilation --
   -----------------------------------------------

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
