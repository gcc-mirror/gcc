------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ E N U M                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2000, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package is used to compute the Value attribute for enumeration types
--  other than those in packages Standard and System. See unit Exp_Imgv for
--  details of the format of constructed image tables.

package System.Val_Enum is
pragma Pure (Val_Enum);

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
