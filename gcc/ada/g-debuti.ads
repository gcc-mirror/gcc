------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                 G N A T . D E B U G _ U T I L I T I E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1995-1998 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Debugging utilities

--  This package provides some useful utility subprograms for use in writing
--  routines that generate debugging output.

with System;

package GNAT.Debug_Utilities is
pragma Pure (Debug_Utilities);

   function Image (S : String) return String;
   --  Returns a string image of S, obtained by prepending and appending
   --  quote (") characters and doubling any quote characters in the string.
   --  The maximum length of the result is thus 2 ** S'Length + 2.

   Address_Image_Length : constant :=
                            13 + 10 * Boolean'Pos (Standard'Address_Size > 32);
   --  Length of string returned by Image function

   function Image (A : System.Address) return String;
   --  Returns a string of the form 16#xxxx_xxxx# for 32-bit addresses
   --  or 16#xxxx_xxxx_xxxx_xxxx# for 64-bit addresses. Hex characters
   --  are in upper case.

   function Value (S : String) return System.Address;
   --  Given a valid integer literal in any form, including the form returned
   --  by the Image function in this package, yields the corresponding address.

end GNAT.Debug_Utilities;
