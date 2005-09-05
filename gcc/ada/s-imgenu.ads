------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . I M G _ E N U M                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2005 Free Software Foundation, Inc.          --
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

--  Enumeration_Type'Image for all enumeration types except those in package
--  Standard (where we have no opportunity to build image tables), and in
--  package System (where it is too early to start building image tables).
--  Special routines exist for the enumeration routines in these packages.

package System.Img_Enum is
   pragma Pure;

   function Image_Enumeration_8
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address)
      return    String;
   --  Used to compute Enum'Image (Str) where Enum is some enumeration type
   --  other than those defined in package Standard. Names is a string with a
   --  lower bound of 1 containing the characters of all the enumeration
   --  literals concatenated together in sequence. Indexes is the address of an
   --  array of type array (0 .. N) of Natural_8, where N is the number of
   --  enumeration literals in the type. The Indexes values are the starting
   --  subscript of each enumeration literal, indexed by Pos values, with an
   --  extra entry at the end containing Names'Length + 1. The reason that
   --  Indexes is passed by address is that the actual type is created on the
   --  fly by the expander. The value returned is the desired 'Image value.

   function Image_Enumeration_16
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address)
      return    String;
   --  Identical to Image_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_16 for the Indexes table.

   function Image_Enumeration_32
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address)
      return    String;
   --  Identical to Image_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_32 for the Indexes table.

end System.Img_Enum;
