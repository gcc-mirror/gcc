------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.NUMERICS.BIG_NUMBERS.BIG_INTEGERS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Interfaces; use Interfaces;

with System.Generic_Bignums;

package body Ada.Numerics.Big_Numbers.Big_Integers is

   package Bignums is new
     System.Generic_Bignums (Use_Secondary_Stack => False);
   use Bignums, System;

   procedure Free is new Ada.Unchecked_Deallocation (Bignum_Data, Bignum);

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

   function "=" (L, R : Big_Integer) return Boolean is
   begin
      return Big_EQ (Get_Bignum (L), Get_Bignum (R));
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Big_Integer) return Boolean is
   begin
      return Big_LT (Get_Bignum (L), Get_Bignum (R));
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Big_Integer) return Boolean is
   begin
      return Big_LE (Get_Bignum (L), Get_Bignum (R));
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Big_Integer) return Boolean is
   begin
      return Big_GT (Get_Bignum (L), Get_Bignum (R));
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Big_Integer) return Boolean is
   begin
      return Big_GE (Get_Bignum (L), Get_Bignum (R));
   end ">=";

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Arg : Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, To_Bignum (Long_Long_Integer (Arg)));
      return Result;
   end To_Big_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Arg : Big_Integer) return Integer is
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

      function To_Big_Integer (Arg : Int) return Big_Integer is
         Result : Big_Integer;
      begin
         Set_Bignum (Result, To_Bignum (Long_Long_Integer (Arg)));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Big_Integer) return Int is
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

      function To_Big_Integer (Arg : Int) return Big_Integer is
         Result : Big_Integer;
      begin
         Set_Bignum (Result, To_Bignum (Unsigned_64 (Arg)));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Big_Integer) return Int is
      begin
         return Int (From_Bignum (Get_Bignum (Arg)));
      end From_Big_Integer;

   end Unsigned_Conversions;

   ---------------
   -- To_String --
   ---------------

   Hex_Chars : constant array (0 .. 15) of Character := "0123456789ABCDEF";

   function To_String
     (Arg : Big_Integer; Width : Field := 0; Base : Number_Base := 10)
      return String
   is
      Big_Base : constant Big_Integer := To_Big_Integer (Integer (Base));

      function Add_Base (S : String) return String;
      --  Add base information if Base /= 10

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String;
      --  Return padding of Char concatenated with Str so that the resulting
      --  string is at least Min_Length long.

      function Image (Arg : Big_Integer) return String;
      --  Return image of Arg, assuming Arg is positive.

      function Image (N : Natural) return String;
      --  Return image of N, with no leading space.

      --------------
      -- Add_Base --
      --------------

      function Add_Base (S : String) return String is
      begin
         if Base = 10 then
            return S;
         else
            return Image (Base) & "#" & S & "#";
         end if;
      end Add_Base;

      -----------
      -- Image --
      -----------

      function Image (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         return S (2 .. S'Last);
      end Image;

      function Image (Arg : Big_Integer) return String is
      begin
         if Arg < Big_Base then
            return (1 => Hex_Chars (To_Integer (Arg)));
         else
            return Image (Arg / Big_Base)
              & Hex_Chars (To_Integer (Arg rem Big_Base));
         end if;
      end Image;

      ---------------------
      -- Leading_Padding --
      ---------------------

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String is
      begin
         return (1 .. Integer'Max (Integer (Min_Length) - Str'Length, 0)
                        => Char) & Str;
      end Leading_Padding;

   begin
      if Arg < To_Big_Integer (0) then
         return Leading_Padding ("-" & Add_Base (Image (-Arg)), Width);
      else
         return Leading_Padding (" " & Add_Base (Image (Arg)), Width);
      end if;
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

   procedure Put_Image
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Arg    : Big_Integer) is
   begin
      Wide_Wide_String'Write (Stream, To_Wide_Wide_String (To_String (Arg)));
   end Put_Image;

   ---------
   -- "+" --
   ---------

   function "+" (L : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, new Bignum_Data'(Get_Bignum (L).all));
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Neg (Get_Bignum (L)));
      return Result;
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Abs (Get_Bignum (L)));
      return Result;
   end "abs";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Add (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Sub (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Mul (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Div (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "/";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Mod (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Set_Bignum (Result, Big_Rem (Get_Bignum (L), Get_Bignum (R)));
      return Result;
   end "rem";

   ----------
   -- "**" --
   ----------

   function "**" (L : Big_Integer; R : Natural) return Big_Integer is
   begin
      --  Explicitly check for validity before allocating Exp so that
      --  the call to Get_Bignum below cannot raise an exception before
      --  we get a chance to free Exp.

      if not Is_Valid (L) then
         raise Constraint_Error with "invalid big integer";
      end if;

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

   function Min (L, R : Big_Integer) return Big_Integer is
     (if L < R then L else R);

   ---------
   -- Max --
   ---------

   function Max (L, R : Big_Integer) return Big_Integer is
     (if L > R then L else R);

   -----------------------------
   -- Greatest_Common_Divisor --
   -----------------------------

   function Greatest_Common_Divisor (L, R : Big_Integer) return Big_Positive is
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
