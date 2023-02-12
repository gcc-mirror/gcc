------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . W W D _ E N U M                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This package contains routines used for Enumeration_Type'Wide_[Wide_]Width

with System.WCh_Con;

package System.WWd_Enum is
   pragma Pure;

   function Wide_Width_Enumeration_8
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : System.WCh_Con.WC_Encoding_Method) return Natural;
   --  Used to compute Enum'Wide_Width where Enum is an enumeration subtype
   --  other than those defined in package Standard. Names is a string with
   --  a lower bound of 1 containing the characters of all the enumeration
   --  literals concatenated together in sequence. Indexes is the address
   --  of an array of type array (0 .. N) of Natural_8, where N is the
   --  number of enumeration literals in the type. The Indexes values are
   --  the starting subscript of each enumeration literal, indexed by Pos
   --  values, with an extra entry at the end containing Names'Length + 1.
   --  The reason that Indexes is passed by address is that the actual type
   --  is created on the fly by the expander.
   --
   --  Lo and Hi are the Pos values of the lower and upper bounds of the
   --  subtype. The result is the value of Width, i.e. the maximum value
   --  of the length of any enumeration literal in the given range. The
   --  fifth parameter, EM, is the wide character encoding method used in
   --  the Names table.

   function Wide_Width_Enumeration_16
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : System.WCh_Con.WC_Encoding_Method) return Natural;
   --  Identical to Wide_Width_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_16 for the Indexes table.

   function Wide_Width_Enumeration_32
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : System.WCh_Con.WC_Encoding_Method) return Natural;
   --  Identical to Wide_Width_Enumeration_8 except that it handles types
   --  using array (0 .. Num) of Natural_32 for the Indexes table.

   function Wide_Wide_Width_Enumeration_8
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : System.WCh_Con.WC_Encoding_Method) return Natural;
   --  Same function for Wide_Wide_Width attribute

   function Wide_Wide_Width_Enumeration_16
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : System.WCh_Con.WC_Encoding_Method) return Natural;
   --  Same function for Wide_Wide_Width attribute

   function Wide_Wide_Width_Enumeration_32
     (Names   : String;
      Indexes : System.Address;
      Lo, Hi  : Natural;
      EM      : System.WCh_Con.WC_Encoding_Method) return Natural;
   --  Same function for Wide_Wide_Width attribute

end System.WWd_Enum;
