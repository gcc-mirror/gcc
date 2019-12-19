------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . B I G N U M S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2012-2019, Free Software Foundation, Inc.         --
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
with Ada.Unchecked_Conversion;

package body System.Bignums is

   package Sec_Stack_Bignums is new
     System.Generic_Bignums (Use_Secondary_Stack => True);
   use Sec_Stack_Bignums;

   function "+" is new Ada.Unchecked_Conversion
     (Bignum, Sec_Stack_Bignums.Bignum);

   function "-" is new Ada.Unchecked_Conversion
     (Sec_Stack_Bignums.Bignum, Bignum);

   function Big_Add (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Add (+X, +Y));

   function Big_Sub (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Sub (+X, +Y));

   function Big_Mul (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Mul (+X, +Y));

   function Big_Div (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Div (+X, +Y));

   function Big_Exp (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Exp (+X, +Y));

   function Big_Mod (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Mod (+X, +Y));

   function Big_Rem (X, Y : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Rem (+X, +Y));

   function Big_Neg (X    : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Neg (+X));

   function Big_Abs (X    : Bignum) return Bignum is
     (-Sec_Stack_Bignums.Big_Abs (+X));

   function Big_EQ  (X, Y : Bignum) return Boolean is
     (Sec_Stack_Bignums.Big_EQ (+X, +Y));
   function Big_NE  (X, Y : Bignum) return Boolean is
     (Sec_Stack_Bignums.Big_NE (+X, +Y));
   function Big_GE  (X, Y : Bignum) return Boolean is
     (Sec_Stack_Bignums.Big_GE (+X, +Y));
   function Big_LE  (X, Y : Bignum) return Boolean is
     (Sec_Stack_Bignums.Big_LE (+X, +Y));
   function Big_GT  (X, Y : Bignum) return Boolean is
     (Sec_Stack_Bignums.Big_GT (+X, +Y));
   function Big_LT  (X, Y : Bignum) return Boolean is
     (Sec_Stack_Bignums.Big_LT (+X, +Y));

   function Bignum_In_LLI_Range (X : Bignum) return Boolean is
     (Sec_Stack_Bignums.Bignum_In_LLI_Range (+X));

   function To_Bignum (X : Long_Long_Integer) return Bignum is
     (-Sec_Stack_Bignums.To_Bignum (X));

   function From_Bignum (X : Bignum) return Long_Long_Integer is
     (Sec_Stack_Bignums.From_Bignum (+X));

end System.Bignums;
