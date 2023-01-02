------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_BUFFERS.FORMATTING                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2023, Free Software Foundation, Inc.       --
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

with Ada.Strings.Text_Buffers.Utils;

package Ada.Strings.Text_Buffers.Formatting is

   --  Template-based output, based loosely on C's printf family. Unlike
   --  printf, it is type safe. We don't support myriad formatting options; the
   --  caller is expected to call 'Image, or other functions that might have
   --  various formatting capabilities.

   type Template is new Utils.UTF_8;

   procedure Put
     (S : in out Root_Buffer_Type'Class; T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "");
   --  Prints the template as is, except for the following escape sequences:
   --    "\n" is end of line.
   --    "\i" indents by the default amount, and "\o" outdents.
   --    "\I" indents by one space, and "\O" outdents.
   --    "\1" is replaced with X1, and similarly for 2, 3, ....
   --    "\\" is "\".

   --  Note that the template is not type String, to avoid this sort of thing:
   --
   --      https://xkcd.com/327/

   procedure Put
     (T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "");
   --  Sends to standard output

   procedure Err
     (T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "");
   --  Sends to standard error

   function Format
     (T : Template;
      X1, X2, X3, X4, X5, X6, X7, X8, X9 : Utils.UTF_8_Lines := "")
     return Utils.UTF_8_Lines;
   --  Returns a UTF-8-encoded String

end Ada.Strings.Text_Buffers.Formatting;
