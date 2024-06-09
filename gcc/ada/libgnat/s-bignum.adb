------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . B I G N U M S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2012-2024, Free Software Foundation, Inc.         --
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

with System.Generic_Bignums;
with System.Secondary_Stack;  use System.Secondary_Stack;
with System.Shared_Bignums;   use System.Shared_Bignums;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Bignums is

   function Allocate_Bignum (D : Digit_Vector; Neg : Boolean) return Bignum;
   --  Allocate Bignum value with the given contents

   procedure Free_Bignum (X : in out Bignum) is null;
   --  No op when using the secondary stack

   function To_Bignum (X : aliased in out Bignum) return Bignum is (X);

   ---------------------
   -- Allocate_Bignum --
   ---------------------

   function Allocate_Bignum (D : Digit_Vector; Neg : Boolean) return Bignum is
      Addr : aliased Address;
   begin
      --  Note: The approach used here is designed to avoid strict aliasing
      --  warnings that appeared previously using unchecked conversion.

      SS_Allocate (Addr, Storage_Offset (4 + 4 * D'Length));

      declare
         B : Bignum;
         for B'Address use Addr'Address;
         pragma Import (Ada, B);

         BD : Bignum_Data (D'Length);
         for BD'Address use Addr;
         pragma Import (Ada, BD);

         --  Expose a writable view of discriminant BD.Len so that we can
         --  initialize it. We need to use the exact layout of the record
         --  to ensure that the Length field has 24 bits as expected.

         type Bignum_Data_Header is record
            Len : Length;
            Neg : Boolean;
         end record;

         for Bignum_Data_Header use record
            Len at 0 range 0 .. 23;
            Neg at 3 range 0 .. 7;
         end record;

         BDH : Bignum_Data_Header;
         for BDH'Address use BD'Address;
         pragma Import (Ada, BDH);

         pragma Assert (BDH.Len'Size = BD.Len'Size);

      begin
         BDH.Len := D'Length;
         BDH.Neg := Neg;
         B.D := D;
         return B;
      end;
   end Allocate_Bignum;

   package Sec_Stack_Bignums is new System.Generic_Bignums
     (Bignum, Allocate_Bignum, Free_Bignum, To_Bignum);

   function Big_Add (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Add;

   function Big_Sub (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Sub;

   function Big_Mul (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Mul;

   function Big_Div (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Div;

   function Big_Exp (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Exp;

   function Big_Mod (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Mod;

   function Big_Rem (X, Y : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Rem;

   function Big_Neg (X : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Neg;

   function Big_Abs (X : Bignum) return Bignum
     renames Sec_Stack_Bignums.Big_Abs;

   function Big_EQ  (X, Y : Bignum) return Boolean
     renames Sec_Stack_Bignums.Big_EQ;
   function Big_NE  (X, Y : Bignum) return Boolean
     renames Sec_Stack_Bignums.Big_NE;
   function Big_GE  (X, Y : Bignum) return Boolean
     renames Sec_Stack_Bignums.Big_GE;
   function Big_LE  (X, Y : Bignum) return Boolean
     renames Sec_Stack_Bignums.Big_LE;
   function Big_GT  (X, Y : Bignum) return Boolean
     renames Sec_Stack_Bignums.Big_GT;
   function Big_LT  (X, Y : Bignum) return Boolean
     renames Sec_Stack_Bignums.Big_LT;

   function Bignum_In_LLI_Range (X : Bignum) return Boolean
     renames Sec_Stack_Bignums.Bignum_In_LLI_Range;

   function To_Bignum (X : Long_Long_Integer) return Bignum
     renames Sec_Stack_Bignums.To_Bignum;

   function From_Bignum (X : Bignum) return Long_Long_Integer
     renames Sec_Stack_Bignums.From_Bignum;

end System.Bignums;
