------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package Lib.Util is

   --  This package implements a buffered write of library information

   procedure Write_Info_Char (C : Character);
   pragma Inline (Write_Info_Char);
   --  Adds one character to the info

   procedure Write_Info_Char_Code (Code : Char_Code);
   --  Write a single character code. Upper half values in the range
   --  16#80..16#FF are written as Uhh (hh = 2 hex digits), and values
   --  greater than 16#FF are written as Whhhh (hhhh = 4 hex digits).

   function Write_Info_Col return Positive;
   --  Returns the column in which the next character will be written

   procedure Write_Info_EOL;
   --  Terminate current info line. This only flushes the buffer
   --  if there is not enough room for another complete line or
   --  if the host system needs a write for each line.

   procedure Write_Info_Initiate (Key : Character);
   --  Initiates write of new line to info file, the parameter is the
   --  keyword character for the line. The caller is responsible for
   --  writing the required blank after the key character.

   procedure Write_Info_Nat (N : Nat);
   --  Adds image of N to Info_Buffer with no leading or trailing blanks

   procedure Write_Info_Name (Name : Name_Id);
   --  Adds characters of Name to Info_Buffer

   procedure Write_Info_Str (Val : String);
   --  Adds characters of Val to Info_Buffer surrounded by quotes

   procedure Write_Info_Tab (Col : Positive);
   --  Tab out with blanks and HT's to column Col. If already at or past
   --  Col, writes a single blank, so that we do get a required field
   --  separation.

   procedure Write_Info_Terminate;
   --  Terminate current info line and output lines built in Info_Buffer

end Lib.Util;
