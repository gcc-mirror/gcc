------------------------------------------------------------------------------
--                                                                          --
--                   GNU ADA RUNTIME LIBRARY COMPONENTS                     --
--                                                                          --
--        S Y S T E M . C O M P A R E _ A R R A Y _ S I G N E D _ 16        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2002 Free Software Foundation, Inc.            --
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

package body System.Compare_Array_Signed_16 is

   type Word is mod 2 ** 32;
   --  Used to process operands by words

   type Half is range -(2 ** 15) .. (2 ** 15) - 1;
   for Half'Size use 16;
   --  Used to process operands by half words

   type Uhalf is record
      H : Half;
   end record;
   pragma Pack (Uhalf);
   for Uhalf'Alignment use 1;
   --  Used to process operands when unaligned

   type WP is access Word;
   type HP is access Half;
   type UP is access Uhalf;

   function W is new Unchecked_Conversion (Address, WP);
   function H is new Unchecked_Conversion (Address, HP);
   function U is new Unchecked_Conversion (Address, UP);

   -----------------------
   -- Compare_Array_S16 --
   -----------------------

   function Compare_Array_S16
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural)
      return      Integer
   is
      Clen : Natural := Natural'Min (Left_Len, Right_Len);
      --  Number of elements left to compare

      L : Address := Left;
      R : Address := Right;
      --  Pointers to next elements to compare

   begin
      --  Go by words if possible

      if ((Left or Right) and (4 - 1)) = 0 then
         while Clen > 1
           and then W (L).all = W (R).all
         loop
            Clen := Clen - 2;
            L := L + 4;
            R := R + 4;
         end loop;
      end if;

      --  Case of going by aligned half words

      if ((Left or Right) and (2 - 1)) = 0 then
         while Clen /= 0 loop
            if H (L).all /= H (R).all then
               if H (L).all > H (R).all then
                  return +1;
               else
                  return -1;
               end if;
            end if;

            Clen := Clen - 1;
            L := L + 2;
            R := R + 2;
         end loop;

      --  Case of going by unaligned half words

      else
         while Clen /= 0 loop
            if U (L).H /= U (R).H then
               if U (L).H > U (R).H then
                  return +1;
               else
                  return -1;
               end if;
            end if;

            Clen := Clen - 1;
            L := L + 2;
            R := R + 2;
         end loop;
      end if;

      --  Here if common section equal, result decided by lengths

      if Left_Len = Right_Len then
         return 0;
      elsif Left_Len > Right_Len then
         return +1;
      else
         return -1;
      end if;
   end Compare_Array_S16;

end System.Compare_Array_Signed_16;
