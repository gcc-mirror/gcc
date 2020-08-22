------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ E N U M                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  This package is used to compute the Value attribute for enumeration types
--  other than those in packages Standard and System. See unit Exp_Imgv for
--  details of the format of constructed image tables.

package System.Val_Enum is
   pragma Preelaborate;

   function Value_Enumeration_8
     (Names   : String;
      Indexes : System.Address;
      Num     : Natural;
      Str     : String)
      return    Natural;
   --  Used to compute Enum'Value (Str) where Enum is some enumeration type
   --  other than those defined in package Standard. Names is a string with
   --  a lower bound of 1 containing the characters of all the enumeration
   --  literals concatenated together in sequence. Indexes is the address
   --  of an array of type array (0 .. N) of Natural_8, where N is the
   --  number of enumeration literals in the type. The Indexes values are
   --  the starting subscript of each enumeration literal, indexed by Pos
   --  values, with an extra entry at the end containing Names'Length + 1.
   --  The parameter Num is the value N - 1 (i.e. Enum'Pos (Enum'Last)).
   --  The reason that Indexes is passed by address is that the actual type
   --  is created on the fly by the expander.
   --
   --  Str is the argument of the attribute function, and may have leading
   --  and trailing spaces, and letters can be upper or lower case or mixed.
   --  If the image is found in Names, then the corresponding Pos value is
   --  returned. If not, Constraint_Error is raised.

   function Value_Enumeration_16
     (Names   : String;
      Indexes : System.Address;
      Num     : Natural;
      Str     : String)
      return    Natural;
   --  Identical to Value_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_16 for the Indexes table.

   function Value_Enumeration_32
     (Names   : String;
      Indexes : System.Address;
      Num     : Natural;
      Str     : String)
      return    Natural;
   --  Identical to Value_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_32 for the Indexes table.

end System.Val_Enum;
