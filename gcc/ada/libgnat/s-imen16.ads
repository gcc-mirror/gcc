------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . I M G _ E N U M _ 1 6                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2021-2024, Free Software Foundation, Inc.      --
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

--  Instantiation of ``System.Image_N`` for enumeration types whose names table
--  has a length that fits in a 16-bit but not a 8-bit integer.

with Interfaces;
with System.Image_N;

package System.Img_Enum_16 is
   pragma Pure;

   package Impl is new Image_N (Interfaces.Integer_16);

   procedure Image_Enumeration_16
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
     renames Impl.Image_Enumeration;
   --  Used to compute Enum'Image (Str) where Enum is some enumeration type
   --  other than those defined in package Standard. Names is a string with
   --  a lower bound of 1 containing the characters of all the enumeration
   --  literals concatenated together in sequence. Indexes is the address of
   --  an array of type array (0 .. N) of Interfaces.Integer_16, where N is the
   --  number of enumeration literals in the type. The Indexes values are the
   --  starting subscript of each enumeration literal, indexed by Pos values,
   --  with an extra entry at the end containing Names'Length + 1. The reason
   --  that Indexes is passed by address is that the actual type is created on
   --  the fly by the expander. The desired 'Image value is stored in S
   --  (1 .. P) and P is set on return. The caller guarantees that S is long
   --  enough to hold the result and that the lower bound is 1.

end System.Img_Enum_16;
