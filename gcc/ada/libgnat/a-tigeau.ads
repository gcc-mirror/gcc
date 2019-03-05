------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . G E N E R I C _ A U X               --
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

--  This package contains a set of auxiliary routines used by the Text_IO
--  generic children, including for reading and writing numeric strings.

private package Ada.Text_IO.Generic_Aux is

   --  Note: for all the Load routines, File indicates the file to be read,
   --  Buf is the string into which data is stored, Ptr is the index of the
   --  last character stored so far, and is updated if additional characters
   --  are stored. Data_Error is raised if the input overflows Buf. The only
   --  Load routines that do a file status check are Load_Skip and Load_Width
   --  so one of these two routines must be called first.

   procedure Check_End_Of_Field
     (Buf   : String;
      Stop  : Integer;
      Ptr   : Integer;
      Width : Field);
   --  This routine is used after doing a get operations on a numeric value.
   --  Buf is the string being scanned, and Stop is the last character of
   --  the field being scanned. Ptr is as set by the call to the scan routine
   --  that scanned out the numeric value, i.e. it points one past the last
   --  character scanned, and Width is the width parameter from the Get call.
   --
   --  There are two cases, if Width is non-zero, then a check is made that
   --  the remainder of the field is all blanks. If Width is zero, then it
   --  means that the scan routine scanned out only part of the field. We
   --  have already scanned out the field that the ACVC tests seem to expect
   --  us to read (even if it does not follow the syntax of the type being
   --  scanned, e.g. allowing negative exponents in integers, and underscores
   --  at the end of the string), so we just raise Data_Error.

   procedure Check_On_One_Line (File : File_Type; Length : Integer);
   --  Check to see if item of length Integer characters can fit on
   --  current line. Call New_Line if not, first checking that the
   --  line length can accommodate Length characters, raise Layout_Error
   --  if item is too large for a single line.

   function Getc (File : File_Type) return Integer;
   --  Gets next character from file, which has already been checked for
   --  being in read status, and returns the character read if no error
   --  occurs. The result is EOF if the end of file was read. Note that
   --  the Col value is not bumped, so it is the caller's responsibility
   --  to bump it if necessary.

   function Is_Blank (C : Character) return Boolean;
   --  Determines if C is a blank (space or tab)

   procedure Load_Width
     (File  : File_Type;
      Width : Field;
      Buf   : out String;
      Ptr   : in out Integer);
   --  Loads exactly Width characters, unless a line mark is encountered first

   procedure Load_Skip (File  : File_Type);
   --  Skips leading blanks and line and page marks, if the end of file is
   --  read without finding a non-blank character, then End_Error is raised.
   --  Note: a blank is defined as a space or horizontal tab (RM A.10.6(5)).

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char   : Character;
      Loaded : out Boolean);
   --  If next character is Char, loads it, otherwise no characters are loaded
   --  Loaded is set to indicate whether or not the character was found.

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char   : Character);
   --  Same as above, but no indication if character is loaded

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char1  : Character;
      Char2  : Character;
      Loaded : out Boolean);
   --  If next character is Char1 or Char2, loads it, otherwise no characters
   --  are loaded. Loaded is set to indicate whether or not one of the two
   --  characters was found.

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char1  : Character;
      Char2  : Character);
   --  Same as above, but no indication if character is loaded

   procedure Load_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Loaded : out Boolean);
   --  Loads a sequence of zero or more decimal digits. Loaded is set if
   --  at least one digit is loaded.

   procedure Load_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer);
   --  Same as above, but no indication if character is loaded

   procedure Load_Extended_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Loaded : out Boolean);
   --  Like Load_Digits, but also allows extended digits a-f and A-F

   procedure Load_Extended_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer);
   --  Same as above, but no indication if character is loaded

   function Nextc (File : File_Type) return Integer;
   --  Like Getc, but includes a call to Ungetc, so that the file
   --  pointer is not moved by the call.

   procedure Put_Item (File : File_Type; Str : String);
   --  This routine is like Text_IO.Put, except that it checks for overflow
   --  of bounded lines, as described in (RM A.10.6(8)). It is used for
   --  all output of numeric values and of enumeration values.

   procedure Store_Char
     (File : File_Type;
      ch   : Integer;
      Buf  : in out String;
      Ptr  : in out Integer);
   --  Store a single character in buffer, checking for overflow and
   --  adjusting the column number in the file to reflect the fact
   --  that a character has been acquired from the input stream. If
   --  the character will not fit in the buffer it is stored in the
   --  last character position of the buffer and Ptr is unchanged.
   --  No exception is raised in this case, it is the caller's job
   --  to raise Data_Error if the buffer fills up, so typically the
   --  caller will make the buffer one character longer than needed.

   procedure String_Skip (Str : String; Ptr : out Integer);
   --  Used in the Get from string procedures to skip leading blanks in the
   --  string. Ptr is set to the index of the first non-blank. If the string
   --  is all blanks, then the exception End_Error is raised, Note that blank
   --  is defined as a space or horizontal tab (RM A.10.6(5)).

   procedure Ungetc (ch : Integer; File : File_Type);
   --  Pushes back character into stream, using ungetc. The caller has
   --  checked that the file is in read status. Device_Error is raised
   --  if the character cannot be pushed back. An attempt to push back
   --  an end of file (EOF) is ignored.

private
   pragma Inline (Is_Blank);

end Ada.Text_IO.Generic_Aux;
