------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.NUMERICS.BIG_NUMBERS.BIG_INTEGERS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2020, Free Software Foundation, Inc.      --
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

pragma Ada_2020;

with Ada.Unchecked_Deallocation;
with Ada.Strings.Text_Output.Utils;

with Interfaces; use Interfaces;

with System.Generic_Bignums;
with System.Shared_Bignums; use System.Shared_Bignums;

package body Ada.Numerics.Big_Numbers.Big_Integers is

   function Allocate_Bignum (D : Digit_Vector; Neg : Boolean) return Bignum;
   --  Allocate Bignum value with the given contents

   procedure Free_Bignum (X : in out Bignum);
   --  Free memory associated with X

   function To_Bignum (X : aliased in out Bignum) return Bignum is (X);

   procedure Free is new Ada.Unchecked_Deallocation (Bignum_Data, Bignum);

   ---------------------
   -- Allocate_Bignum --
   ---------------------

   function Allocate_Bignum (D : Digit_Vector; Neg : Boolean) return Bignum is
   begin
      return new Bignum_Data'(D'Length, Neg, D);
   end Allocate_Bignum;

   -----------------
   -- Free_Bignum --
   -----------------

   procedure Free_Bignum (X : in out Bignum) is
   begin
      Free (X);
   end Free_Bignum;

   package Bignums is new System.Generic_Bignums
     (Bignum, Allocate_Bignum, Free_Bignum, To_Bignum);

   use Bignums, System;

   function Get_Bignum (Arg : Big_Integer) return Bignum is
     (if Arg.Value.C = System.Null_Address
      then raise Constraint_Error with "invalid big integer"
      else To_Bignum (Arg.Value.C));
   --  Check for validity of Arg and return the Bignum value stored in Arg.
   --  Raise Constraint_Error if Arg is uninitialized.

   procedure Set_Bignum (Arg : out Big_Integer; Value : Bignum)
     with Inline;
   --  Set the Bignum value stored in Arg to Value

   ----------------
   -- Set_Bignum --
   ----------------

   procedure Set_Bignum (Arg : out Big_Integer; Value : Bignum) is
   begin
      Arg.Value.C := To_Address (Value);
   end Set_Bignum;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Arg : Big_Integer) return Boolean is
     (Arg.Value.C /= System.Null_Address);

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return Big_EQ (Get_Bignum (L), Get_Bignum (R));
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return Big_LT (Get_Bignum (L), Get_Bignum (R));
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return Big_LE (Get_Bignum (L), Get_Bignum (R));
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return Big_GT (Get_Bignum (L), Get_Bignum (R));
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return Big_GE (Get_Bignum (L), Get_Bignum (R));
   end ">=";

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Arg : Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, To_Bignum (Long_Long_Integer (Arg)));
      return Result;
   end To_Big_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Arg : Valid_Big_Integer) return Integer is
   begin
      return Integer (From_Bignum (Get_Bignum (Arg)));
   end To_Integer;

   ------------------------
   -- Signed_Conversions --
   ------------------------

   package body Signed_Conversions is

      --------------------
      -- To_Big_Integer --
      --------------------

      function To_Big_Integer (Arg : Int) return Valid_Big_Integer is
         Result : Big_Integer;
      begin
         Set_Bignum (Result, To_Bignum (Long_Long_Integer (Arg)));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int is
      begin
         return Int (From_Bignum (Get_Bignum (Arg)));
      end From_Big_Integer;

   end Signed_Conversions;

   --------------------------
   -- Unsigned_Conversions --
   --------------------------

   package body Unsigned_Conversions is

      --------------------
      -- To_Big_Integer --
      --------------------

      function To_Big_Integer (Arg : Int) return Valid_Big_Integer is
         Result : Big_Integer;
      begin
         Set_Bignum (Result, To_Bignum (Unsigned_64 (Arg)));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int is
      begin
         return Int (From_Bignum (Get_Bignum (Arg)));
      end From_Big_Integer;

   end Unsigned_Conversions;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Arg : Valid_Big_Integer; Width : Field := 0; Base : Number_Base := 10)
      return String is
   begin
      return To_String (Get_Bignum (Arg), Natural (Width), Positive (Base));
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Arg : String) return Big_Integer is
      Result : Big_Integer;
   begin
      --  ??? only support Long_Long_Integer, good enough for now
      Set_Bignum (Result, To_Bignum (Long_Long_Integer'Value (Arg)));
      return Result;
   end From_String;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image (S : in out Sink'Class; V : Big_Integer) is
      --  This is implemented in terms of To_String. It might be more elegant
      --  and more efficient to do it the other way around, but this is the
      --  most expedient implementation for now.
   begin
      Strings.Text_Output.Utils.Put_UTF_8 (S, To_String (V));
   end Put_Image;

   ---------
   -- "+" --
   ---------

   function "+" (L : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, new Bignum_Data'(Get_Bignum (L).all));
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Neg (Get_Bignum (L)));
      return Result;
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Abs (Get_Bignum (L)));
      return Result;
   end "abs";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Add (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Sub (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Mul (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Div (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "/";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Mod (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Rem (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "rem";

   ----------
   -- "**" --
   ----------

   function "**"
     (L : Valid_Big_Integer; R : Natural) return Valid_Big_Integer is
   begin
      declare
         Exp    : Bignum := To_Bignum (Long_Long_Integer (R));
         Result : Big_Integer;
      begin
         Set_Bignum (Result, Big_Exp (Get_Bignum (L), Exp));
         Free (Exp);
         return Result;
      end;
   end "**";

   ---------
   -- Min --
   ---------

   function Min (L, R : Valid_Big_Integer) return Valid_Big_Integer is
     (if L < R then L else R);

   ---------
   -- Max --
   ---------

   function Max (L, R : Valid_Big_Integer) return Valid_Big_Integer is
     (if L > R then L else R);

   -----------------------------
   -- Greatest_Common_Divisor --
   -----------------------------

   function Greatest_Common_Divisor
     (L, R : Valid_Big_Integer) return Big_Positive
   is
      function GCD (A, B : Big_Integer) return Big_Integer;
      --  Recursive internal version

      ---------
      -- GCD --
      ---------

      function GCD (A, B : Big_Integer) return Big_Integer is
      begin
         if Is_Zero (Get_Bignum (B)) then
            return A;
         else
            return GCD (B, A rem B);
         end if;
      end GCD;

   begin
      return GCD (abs L, abs R);
   end Greatest_Common_Divisor;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Controlled_Bignum) is
   begin
      if This.C /= System.Null_Address then
         This.C := To_Address (new Bignum_Data'(To_Bignum (This.C).all));
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Controlled_Bignum) is
      Tmp : Bignum := To_Bignum (This.C);
   begin
      Free (Tmp);
      This.C := System.Null_Address;
   end Finalize;

end Ada.Numerics.Big_Numbers.Big_Integers;
