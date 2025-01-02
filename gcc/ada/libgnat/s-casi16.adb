------------------------------------------------------------------------------
--                                                                          --
--                    GNAT RUN-TIME LIBRARY COMPONENTS                      --
--                                                                          --
--       S Y S T E M . C O M P A R E _ A R R A Y _ S I G N E D _ 1 6        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2025, Free Software Foundation, Inc.         --
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

with System.Storage_Elements; use System.Storage_Elements;

with Ada.Unchecked_Conversion;

package body System.Compare_Array_Signed_16 is

   type Word is mod 2 ** 32;
   --  Used to process operands by words

   type Half is range -(2 ** 15) .. (2 ** 15) - 1;
   for Half'Size use 16;
   --  Used to process operands by half words

   type Uhalf is new Half;
   for Uhalf'Alignment use 1;
   --  Used to process operands when unaligned

   type WP is access Word;
   type HP is access Half;
   type UP is access Uhalf;

   function W is new Ada.Unchecked_Conversion (Address, WP);
   function H is new Ada.Unchecked_Conversion (Address, HP);
   function U is new Ada.Unchecked_Conversion (Address, UP);

   -----------------------
   -- Compare_Array_S16 --
   -----------------------

   pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                    "early returns for performance");

   function Compare_Array_S16
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      Clen : Natural := Natural'Min (Left_Len, Right_Len);
      --  Number of elements left to compare

      L : Address := Left;
      R : Address := Right;
      --  Pointers to next elements to compare

   begin
      --  Go by words if possible

      if Left mod Storage_Offset (4) = 0
        and then Right mod Storage_Offset (4) = 0
      then
         while Clen > 1
           and then W (L).all = W (R).all
         loop
            Clen := Clen - 2;
            L := L + Storage_Offset (4);
            R := R + Storage_Offset (4);
         end loop;
      end if;

      --  Case of going by aligned half words

      if Left mod Storage_Offset (2) = 0
        and then Right mod Storage_Offset (2) = 0
      then
         while Clen /= 0 loop
            if H (L).all /= H (R).all then
               if H (L).all > H (R).all then
                  return +1;
               else
                  return -1;
               end if;
            end if;

            Clen := Clen - 1;
            L := L + Storage_Offset (2);
            R := R + Storage_Offset (2);
         end loop;

      --  Case of going by unaligned half words

      else
         while Clen /= 0 loop
            if U (L).all /= U (R).all then
               if U (L).all > U (R).all then
                  return +1;
               else
                  return -1;
               end if;
            end if;

            Clen := Clen - 1;
            L := L + Storage_Offset (2);
            R := R + Storage_Offset (2);
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

   pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
end System.Compare_Array_Signed_16;
