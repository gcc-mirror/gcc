------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                      S Y S T E M . I M G _ E N U M                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2002 Free Software Foundation, Inc.          --
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

with Unchecked_Conversion;

package body System.Img_Enum is

   -------------------------
   -- Image_Enumeration_8 --
   -------------------------

   function Image_Enumeration_8
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address)
      return    String
   is
      type Natural_8 is range 0 .. 2 ** 7 - 1;
      type Index_Table is array (Natural) of Natural_8;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

      subtype Result_Type is String (1 .. Next - Start);
      --  We need this result type to force the result to have the
      --  required lower bound of 1, rather than the slice bounds.

   begin
      return Result_Type (Names (Start .. Next - 1));
   end Image_Enumeration_8;

   --------------------------
   -- Image_Enumeration_16 --
   --------------------------

   function Image_Enumeration_16
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address)
      return    String
   is
      type Natural_16 is range 0 .. 2 ** 15 - 1;
      type Index_Table is array (Natural) of Natural_16;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

      subtype Result_Type is String (1 .. Next - Start);
      --  We need this result type to force the result to have the
      --  required lower bound of 1, rather than the slice bounds.

   begin
      return Result_Type (Names (Start .. Next - 1));
   end Image_Enumeration_16;

   --------------------------
   -- Image_Enumeration_32 --
   --------------------------

   function Image_Enumeration_32
     (Pos     : Natural;
      Names   : String;
      Indexes : System.Address)
      return    String
   is
      type Natural_32 is range 0 .. 2 ** 31 - 1;
      type Index_Table is array (Natural) of Natural_32;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

      subtype Result_Type is String (1 .. Next - Start);
      --  We need this result type to force the result to have the
      --  required lower bound of 1, rather than the slice bounds.

   begin
      return Result_Type (Names (Start .. Next - 1));
   end Image_Enumeration_32;

end System.Img_Enum;
