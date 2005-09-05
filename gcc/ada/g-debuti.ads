------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . D E B U G _ U T I L I T I E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2005, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  Debugging utilities

--  This package provides some useful utility subprograms for use in writing
--  routines that generate debugging output.

with System;

package GNAT.Debug_Utilities is
   pragma Pure;

   Address_64 : constant Boolean := Standard'Address_Size = 64;
   --  Set true if 64 bit addresses (assumes only 32 and 64 are possible)

   Address_Image_Length : constant := 13 + 10 * Boolean'Pos (Address_64);
   --  Length of string returned by Image function for an address

   subtype Image_String is String (1 .. Address_Image_Length);
   --  Subtype returned by Image function for an address

   Address_Image_C_Length : constant := 10 + 8 * Boolean'Pos (Address_64);
   --  Length of string returned by Image_C function

   subtype Image_C_String is String (1 .. Address_Image_C_Length);
   --  Subtype returned by Image_C function

   function Image (S : String) return String;
   --  Returns a string image of S, obtained by prepending and appending
   --  quote (") characters and doubling any quote characters in the string.
   --  The maximum length of the result is thus 2 ** S'Length + 2.

   function Image (A : System.Address) return Image_String;
   --  Returns a string of the form 16#hhhh_hhhh# for 32-bit addresses
   --  or 16#hhhh_hhhh_hhhh_hhhh# for 64-bit addresses. Hex characters
   --  are in upper case.

   function Image_C (A : System.Address) return Image_C_String;
   --  Returns a string of the form 0xhhhhhhhh for 32 bit addresses or
   --  0xhhhhhhhhhhhhhhhh for 64-bit addresses. Hex characters are in
   --  upper case.

   function Value (S : String) return System.Address;
   --  Given a valid integer literal in any form, including the form returned
   --  by the Image function in this package, yields the corresponding address.
   --  Note that this routine will handle any Ada integer format, and will
   --  also handle hex constants in C format (0xhh..hhh). Constraint_Error
   --  may be raised for obviously incorrect data, but the routine is fairly
   --  permissive, and in particular, all underscores in whatever position
   --  are simply ignored completely.

end GNAT.Debug_Utilities;
