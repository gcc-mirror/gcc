------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             W I D E C H A R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  Subprograms for manipulation of wide character sequences. Note that in
--  this package, wide character and wide wide character are not distinguished
--  since this package is basically concerned with syntactic notions, and it
--  deals with Char_Code values, rather than values of actual Ada types.

with Types; use Types;

package Widechar is

   Wide_Char_Byte_Count : Nat := 0;
   --  This value is incremented whenever Scan_Wide or Skip_Wide is called.
   --  The amount added is the length of the wide character sequence minus
   --  one. This means that the count that accumulates here represents the
   --  difference between the length in characters and the length in bytes.
   --  This is used for checking the line length in characters.

   function Length_Wide return Nat;
   --  Returns the maximum length in characters for the escape sequence that
   --  is used to encode wide character literals outside the ASCII range. Used
   --  only in the implementation of the attribute Width for Wide_Character
   --  and Wide_Wide_Character.

   procedure Scan_Wide
     (S   : Source_Buffer_Ptr;
      P   : in out Source_Ptr;
      C   : out Char_Code;
      Err : out Boolean);
   --  On entry S (P) points to the first character in the source text for
   --  a wide character (i.e. to an ESC character, a left bracket, or an
   --  upper half character, depending on the representation method). A
   --  single wide character is scanned. If no error is found, the value
   --  stored in C is the code for this wide character, P is updated past
   --  the sequence and Err is set to False. If an error is found, then
   --  P points to the improper character, C is undefined, and Err is
   --  set to True.

   procedure Set_Wide
     (C : Char_Code;
      S : in out String;
      P : in out Natural);
   --  The escape sequence (including any leading ESC character) for the
   --  given character code is stored starting at S (P + 1), and on return
   --  P points to the last stored character (i.e. P is the count of stored
   --  characters on entry and exit, and the escape sequence is appended to
   --  the end of the stored string). The character code C represents a code
   --  originally constructed by Scan_Wide, so it is known to be in a range
   --  that is appropriate for the encoding method in use.

   procedure Skip_Wide (S : String; P : in out Natural);
   --  On entry, S (P) points to an ESC character for a wide character escape
   --  sequence or to an upper half character if the encoding method uses the
   --  upper bit, or to a left bracket if the brackets encoding method is in
   --  use. On exit, P is bumped past the wide character sequence. No error
   --  checking is done, since this is only used on escape sequences generated
   --  by Set_Wide, which are known to be correct.

   procedure Skip_Wide (S : Source_Buffer_Ptr; P : in out Source_Ptr);
   --  Similar to the above procedure, but operates on a source buffer
   --  instead of a string, with P being a Source_Ptr referencing the
   --  contents of the source buffer.

   function Is_Start_Of_Wide_Char
     (S : Source_Buffer_Ptr;
      P : Source_Ptr) return Boolean;
   --  Determines if S (P) is the start of a wide character sequence

private
   pragma Inline (Is_Start_Of_Wide_Char);

end Widechar;
