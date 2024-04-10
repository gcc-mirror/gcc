------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.NUMERICS.BIG_NUMBERS.BIG_INTEGERS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2024, Free Software Foundation, Inc.      --
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

--  This is the GMP version of this package

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;               use Interfaces.C;
with Interfaces.C.Strings;       use Interfaces.C.Strings;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

package body Ada.Numerics.Big_Numbers.Big_Integers is

   use System;

   pragma Linker_Options ("-lgmp");

   type mpz_t is record
      mp_alloc : Integer;
      mp_size  : Integer;
      mp_d     : System.Address;
   end record;
   pragma Convention (C, mpz_t);
   type mpz_t_ptr is access all mpz_t;

   function To_Mpz is new Ada.Unchecked_Conversion (System.Address, mpz_t_ptr);
   function To_Address is new
     Ada.Unchecked_Conversion (mpz_t_ptr, System.Address);

   function Get_Mpz (Arg : Big_Integer) return mpz_t_ptr is
     (To_Mpz (Arg.Value.C));
   --  Return the mpz_t value stored in Arg

   procedure Set_Mpz (Arg : in out Big_Integer; Value : mpz_t_ptr)
     with Inline;
   --  Set the mpz_t value stored in Arg to Value

   procedure Allocate (This : in out Big_Integer) with Inline;
   --  Allocate a Big_Integer, including the underlying mpz

   procedure mpz_init_set (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_init_set, "__gmpz_init_set");

   procedure mpz_set (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_set, "__gmpz_set");

   function mpz_cmp (OP1, OP2 : access constant mpz_t) return Integer;
   pragma Import (C, mpz_cmp, "__gmpz_cmp");

   function mpz_cmp_ui
     (OP1 : access constant mpz_t; OP2 : unsigned_long) return Integer;
   pragma Import (C, mpz_cmp_ui, "__gmpz_cmp_ui");

   procedure mpz_set_si (ROP : access mpz_t; OP : long);
   pragma Import (C, mpz_set_si, "__gmpz_set_si");

   procedure mpz_set_ui (ROP : access mpz_t; OP : unsigned_long);
   pragma Import (C, mpz_set_ui, "__gmpz_set_ui");

   function mpz_get_si (OP : access constant mpz_t) return long;
   pragma Import (C, mpz_get_si, "__gmpz_get_si");

   function mpz_get_ui (OP : access constant mpz_t) return unsigned_long;
   pragma Import (C, mpz_get_ui, "__gmpz_get_ui");

   procedure mpz_neg (ROP : access mpz_t;  OP : access constant mpz_t);
   pragma Import (C, mpz_neg, "__gmpz_neg");

   procedure mpz_sub (ROP : access mpz_t;  OP1, OP2 : access constant mpz_t);
   pragma Import (C, mpz_sub, "__gmpz_sub");

   -------------
   -- Set_Mpz --
   -------------

   procedure Set_Mpz (Arg : in out Big_Integer; Value : mpz_t_ptr) is
   begin
      Arg.Value.C := To_Address (Value);
   end Set_Mpz;

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
      return mpz_cmp (Get_Mpz (L), Get_Mpz (R)) = 0;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return mpz_cmp (Get_Mpz (L), Get_Mpz (R)) < 0;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return mpz_cmp (Get_Mpz (L), Get_Mpz (R)) <= 0;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return mpz_cmp (Get_Mpz (L), Get_Mpz (R)) > 0;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Valid_Big_Integer) return Boolean is
   begin
      return mpz_cmp (Get_Mpz (L), Get_Mpz (R)) >= 0;
   end ">=";

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Arg : Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Allocate (Result);
      mpz_set_si (Get_Mpz (Result), long (Arg));
      return Result;
   end To_Big_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Arg : Valid_Big_Integer) return Integer is
   begin
      return Integer (mpz_get_si (Get_Mpz (Arg)));
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
         Allocate (Result);
         mpz_set_si (Get_Mpz (Result), long (Arg));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int is
      begin
         return Int (mpz_get_si (Get_Mpz (Arg)));
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
         Allocate (Result);
         mpz_set_ui (Get_Mpz (Result), unsigned_long (Arg));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int is
      begin
         return Int (mpz_get_ui (Get_Mpz (Arg)));
      end From_Big_Integer;

   end Unsigned_Conversions;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Arg : Valid_Big_Integer; Width : Field := 0; Base : Number_Base := 10)
      return String
   is
      function mpz_get_str
        (STR  : System.Address;
         BASE : Integer;
         OP   : access constant mpz_t) return chars_ptr;
      pragma Import (C, mpz_get_str, "__gmpz_get_str");

      function mpz_sizeinbase
         (this : access constant mpz_t; base : Integer) return size_t;
      pragma Import (C, mpz_sizeinbase, "__gmpz_sizeinbase");

      function Add_Base (S : String) return String;
      --  Add base information if Base /= 10

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String;
      --  Return padding of Char concatenated with Str so that the resulting
      --  string is at least Min_Length long.

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
            return Image (Base) & "#" & To_Upper (S) & "#";
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

      Number_Digits : constant Integer :=
        Integer (mpz_sizeinbase (Get_Mpz (Arg), Integer (abs Base)));

      Buffer : aliased String (1 .. Number_Digits + 2);
      --  The correct number to allocate is 2 more than Number_Digits in order
      --  to handle a possible minus sign and the null-terminator.

      Result : constant chars_ptr :=
        mpz_get_str (Buffer'Address, Integer (Base), Get_Mpz (Arg));
      S      : constant String := Value (Result);

   begin
      if S (1) = '-' then
         return Leading_Padding ("-" & Add_Base (S (2 .. S'Last)), Width);
      else
         return Leading_Padding (" " & Add_Base (S), Width);
      end if;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Arg : String) return Valid_Big_Integer is
      function mpz_set_str
        (this : access mpz_t;
         str  : System.Address;
         base : Integer := 10) return Integer;
      pragma Import (C, mpz_set_str, "__gmpz_set_str");

      Result : Big_Integer;
      First  : Natural;
      Last   : Natural;
      Base   : Natural;

   begin
      Allocate (Result);

      if Arg (Arg'Last) /= '#' then

         --  Base 10 number

         First := Arg'First;
         Last  := Arg'Last;
         Base  := 10;
      else
         --  Compute the xx base in a xx#yyyyy# number

         if Arg'Length < 4 then
            raise Constraint_Error;
         end if;

         First := 0;
         Last  := Arg'Last - 1;

         for J in Arg'First + 1 .. Last loop
            if Arg (J) = '#' then
               First := J;
               exit;
            end if;
         end loop;

         if First = 0 then
            raise Constraint_Error;
         end if;

         Base  := Natural'Value (Arg (Arg'First .. First - 1));
         First := First + 1;
      end if;

      declare
         Str   : aliased String (1 .. Last - First + 2);
         Index : Natural := 0;
      begin
         --  Strip underscores

         for J in First .. Last loop
            if Arg (J) /= '_' then
               Index := Index + 1;
               Str (Index) := Arg (J);
            end if;
         end loop;

         Index := Index + 1;
         Str (Index) := ASCII.NUL;

         if mpz_set_str (Get_Mpz (Result), Str'Address, Base) /= 0 then
            raise Constraint_Error;
         end if;
      end;

      return Result;
   end From_String;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image (S : in out Root_Buffer_Type'Class; V : Big_Integer) is
      --  This is implemented in terms of To_String. It might be more elegant
      --  and more efficient to do it the other way around, but this is the
      --  most expedient implementation for now.
   begin
      Strings.Text_Buffers.Put_UTF_8 (S, To_String (V));
   end Put_Image;

   ---------
   -- "+" --
   ---------

   function "+" (L : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Set_Mpz (Result, new mpz_t);
      mpz_init_set (Get_Mpz (Result), Get_Mpz (L));
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Allocate (Result);
      mpz_neg (Get_Mpz (Result), Get_Mpz (L));
      return Result;
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer is
      procedure mpz_abs (ROP : access mpz_t;  OP : access constant mpz_t);
      pragma Import (C, mpz_abs, "__gmpz_abs");

      Result : Big_Integer;
   begin
      Allocate (Result);
      mpz_abs (Get_Mpz (Result), Get_Mpz (L));
      return Result;
   end "abs";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      procedure mpz_add
        (ROP : access mpz_t;  OP1, OP2 : access constant mpz_t);
      pragma Import (C, mpz_add, "__gmpz_add");

      Result : Big_Integer;

   begin
      Allocate (Result);
      mpz_add (Get_Mpz (Result), Get_Mpz (L), Get_Mpz (R));
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      Result : Big_Integer;
   begin
      Allocate (Result);
      mpz_sub (Get_Mpz (Result), Get_Mpz (L), Get_Mpz (R));
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      procedure mpz_mul
        (ROP : access mpz_t;  OP1, OP2 : access constant mpz_t);
      pragma Import (C, mpz_mul, "__gmpz_mul");

      Result : Big_Integer;

   begin
      Allocate (Result);
      mpz_mul (Get_Mpz (Result), Get_Mpz (L), Get_Mpz (R));
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      procedure mpz_tdiv_q (Q : access mpz_t;  N, D : access constant mpz_t);
      pragma Import (C, mpz_tdiv_q, "__gmpz_tdiv_q");
   begin
      if mpz_cmp_ui (Get_Mpz (R), 0) = 0 then
         raise Constraint_Error;
      end if;

      declare
         Result : Big_Integer;
      begin
         Allocate (Result);
         mpz_tdiv_q (Get_Mpz (Result), Get_Mpz (L), Get_Mpz (R));
         return Result;
      end;
   end "/";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      procedure mpz_mod (R : access mpz_t;  N, D : access constant mpz_t);
      pragma Import (C, mpz_mod, "__gmpz_mod");
      --  result is always non-negative

      L_Negative, R_Negative : Boolean;

   begin
      if mpz_cmp_ui (Get_Mpz (R), 0) = 0 then
         raise Constraint_Error;
      end if;

      declare
         Result : Big_Integer;
      begin
         Allocate (Result);
         L_Negative := mpz_cmp_ui (Get_Mpz (L), 0) < 0;
         R_Negative := mpz_cmp_ui (Get_Mpz (R), 0) < 0;

         if not (L_Negative or R_Negative) then
            mpz_mod (Get_Mpz (Result), Get_Mpz (L), Get_Mpz (R));
         else
            --  The GMP library provides operators defined by C semantics, but
            --  the semantics of Ada's mod operator are not the same as C's
            --  when negative values are involved. We do the following to
            --  implement the required Ada semantics.

            declare
               Temp_Left   : Big_Integer;
               Temp_Right  : Big_Integer;
               Temp_Result : Big_Integer;

            begin
               Allocate (Temp_Result);
               Set_Mpz (Temp_Left, new mpz_t);
               Set_Mpz (Temp_Right, new mpz_t);
               mpz_init_set (Get_Mpz (Temp_Left), Get_Mpz (L));
               mpz_init_set (Get_Mpz (Temp_Right), Get_Mpz (R));

               if L_Negative then
                  mpz_neg (Get_Mpz (Temp_Left), Get_Mpz (Temp_Left));
               end if;

               if R_Negative then
                  mpz_neg (Get_Mpz (Temp_Right), Get_Mpz (Temp_Right));
               end if;

               --  now both Temp_Left and Temp_Right are nonnegative

               mpz_mod (Get_Mpz (Temp_Result),
                        Get_Mpz (Temp_Left),
                        Get_Mpz (Temp_Right));

               if mpz_cmp_ui (Get_Mpz (Temp_Result), 0) = 0 then
                  --  if Temp_Result is zero we are done
                  mpz_set (Get_Mpz (Result), Get_Mpz (Temp_Result));

               elsif L_Negative then
                  if R_Negative then
                     mpz_neg (Get_Mpz (Result), Get_Mpz (Temp_Result));
                  else -- L is negative but R is not
                     mpz_sub (Get_Mpz (Result),
                              Get_Mpz (Temp_Right),
                              Get_Mpz (Temp_Result));
                  end if;
               else
                  pragma Assert (R_Negative);
                  mpz_sub (Get_Mpz (Result),
                           Get_Mpz (Temp_Result),
                           Get_Mpz (Temp_Right));
               end if;
            end;
         end if;

         return Result;
      end;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem" (L, R : Valid_Big_Integer) return Valid_Big_Integer is
      procedure mpz_tdiv_r (R : access mpz_t;  N, D : access constant mpz_t);
      pragma Import (C, mpz_tdiv_r, "__gmpz_tdiv_r");
      --   R will have the same sign as N.

   begin
      if mpz_cmp_ui (Get_Mpz (R), 0) = 0 then
         raise Constraint_Error;
      end if;

      declare
         Result : Big_Integer;
      begin
         Allocate (Result);
         mpz_tdiv_r (R => Get_Mpz (Result),
                     N => Get_Mpz (L),
                     D => Get_Mpz (R));
         --  the result takes the sign of N, as required by the RM

         return Result;
      end;
   end "rem";

   ----------
   -- "**" --
   ----------

   function "**"
     (L : Valid_Big_Integer; R : Natural) return Valid_Big_Integer
   is
      procedure mpz_pow_ui (ROP : access mpz_t;
                            BASE : access constant mpz_t;
                            EXP : unsigned_long);
      pragma Import (C, mpz_pow_ui, "__gmpz_pow_ui");

      Result : Big_Integer;

   begin
      Allocate (Result);
      mpz_pow_ui (Get_Mpz (Result), Get_Mpz (L), unsigned_long (R));
      return Result;
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
      procedure mpz_gcd
        (ROP : access mpz_t;  Op1, Op2 : access constant mpz_t);
      pragma Import (C, mpz_gcd, "__gmpz_gcd");

      Result : Big_Integer;

   begin
      Allocate (Result);
      mpz_gcd (Get_Mpz (Result), Get_Mpz (L), Get_Mpz (R));
      return Result;
   end Greatest_Common_Divisor;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (This : in out Big_Integer) is
      procedure mpz_init (this : access mpz_t);
      pragma Import (C, mpz_init, "__gmpz_init");
   begin
      Set_Mpz (This, new mpz_t);
      mpz_init (Get_Mpz (This));
   end Allocate;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Controlled_Bignum) is
      Value : constant mpz_t_ptr := To_Mpz (This.C);
   begin
      if Value /= null then
         This.C := To_Address (new mpz_t);
         mpz_init_set (To_Mpz (This.C), Value);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Controlled_Bignum) is
      procedure Free is new Ada.Unchecked_Deallocation (mpz_t, mpz_t_ptr);

      procedure mpz_clear (this : access mpz_t);
      pragma Import (C, mpz_clear, "__gmpz_clear");

      Mpz : mpz_t_ptr;

   begin
      if This.C /= System.Null_Address then
         Mpz := To_Mpz (This.C);
         mpz_clear (Mpz);
         Free (Mpz);
         This.C := System.Null_Address;
      end if;
   end Finalize;

end Ada.Numerics.Big_Numbers.Big_Integers;
