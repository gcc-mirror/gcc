------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . I M G _ E N U M                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2020, Free Software Foundation, Inc.         --
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

--  Enumeration_Type'Image for all enumeration types except those in package
--  Standard (where we have no opportunity to build image tables), and in
--  package System (where it is too early to start building image tables).
--  Special routines exist for the enumeration types in these packages.

--  Note: this is an obsolete package, replaced by System.Img_Enum_New, which
--  provides procedures instead of functions for these enumeration image calls.
--  The reason we maintain this package is that when bootstrapping with old
--  compilers, the old compiler will search for this unit, expecting to find
--  these functions. The new compiler will search for procedures in the new
--  version of the unit.

pragma Compiler_Unit_Warning;

package System.Img_Enum is
   pragma Pure;

   function Image_Enumeration_8
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address) return String;
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
      Indexes : System.Address) return String;
   --  Identical to Image_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_16 for the Indexes table.

   function Image_Enumeration_32
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address) return String;
   --  Identical to Image_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_32 for the Indexes table.

end System.Img_Enum;
