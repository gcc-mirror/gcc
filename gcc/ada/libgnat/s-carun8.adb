------------------------------------------------------------------------------
--                                                                          --
--                    GNAT RUN-TIME LIBRARY COMPONENTS                      --
--                                                                          --
--       S Y S T E M . C O M P A R E _ A R R A Y _ U N S I G N E D _ 8      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2019, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with System.Address_Operations; use System.Address_Operations;

with Ada.Unchecked_Conversion;

package body System.Compare_Array_Unsigned_8 is

   type Word is mod 2 ** 32;
   --  Used to process operands by words

   type Big_Words is array (Natural) of Word;
   type Big_Words_Ptr is access Big_Words;
   for Big_Words_Ptr'Storage_Size use 0;
   --  Array type used to access by words

   type Byte is mod 2 ** 8;
   --  Used to process operands by bytes

   type Big_Bytes is array (Natural) of Byte;
   type Big_Bytes_Ptr is access Big_Bytes;
   for Big_Bytes_Ptr'Storage_Size use 0;
   --  Array type used to access by bytes

   function To_Big_Words is new
     Ada.Unchecked_Conversion (System.Address, Big_Words_Ptr);

   function To_Big_Bytes is new
     Ada.Unchecked_Conversion (System.Address, Big_Bytes_Ptr);

   ----------------------
   -- Compare_Array_U8 --
   ----------------------

   function Compare_Array_U8
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      Compare_Len : constant Natural := Natural'Min (Left_Len, Right_Len);

   begin
      --  If operands are non-aligned, or length is too short, go by bytes

      if (ModA (OrA (Left, Right), 4) /= 0) or else Compare_Len < 4 then
         return Compare_Array_U8_Unaligned (Left, Right, Left_Len, Right_Len);
      end if;

      --  Here we can go by words

      declare
         LeftP                   : constant Big_Words_Ptr :=
                                     To_Big_Words (Left);
         RightP                  : constant Big_Words_Ptr :=
                                     To_Big_Words (Right);
         Words_To_Compare        : constant Natural := Compare_Len / 4;
         Bytes_Compared_As_Words : constant Natural := Words_To_Compare * 4;

      begin
         for J in 0 .. Words_To_Compare - 1 loop
            if LeftP (J) /= RightP (J) then
               return Compare_Array_U8_Unaligned
                        (AddA (Left,  Address (4 * J)),
                         AddA (Right, Address (4 * J)),
                         4, 4);
            end if;
         end loop;

         return Compare_Array_U8_Unaligned
                  (AddA (Left,  Address (Bytes_Compared_As_Words)),
                   AddA (Right, Address (Bytes_Compared_As_Words)),
                   Left_Len  - Bytes_Compared_As_Words,
                   Right_Len - Bytes_Compared_As_Words);
      end;
   end Compare_Array_U8;

   --------------------------------
   -- Compare_Array_U8_Unaligned --
   --------------------------------

   function Compare_Array_U8_Unaligned
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural) return Integer
   is
      Compare_Len : constant Natural := Natural'Min (Left_Len, Right_Len);

      LeftP  : constant Big_Bytes_Ptr := To_Big_Bytes (Left);
      RightP : constant Big_Bytes_Ptr := To_Big_Bytes (Right);

   begin
      for J in 0 .. Compare_Len - 1 loop
         if LeftP (J) /= RightP (J) then
            if LeftP (J) > RightP (J) then
               return +1;
            else
               return -1;
            end if;
         end if;
      end loop;

      if Left_Len = Right_Len then
         return 0;
      elsif Left_Len > Right_Len then
         return +1;
      else
         return -1;
      end if;
   end Compare_Array_U8_Unaligned;

end System.Compare_Array_Unsigned_8;
