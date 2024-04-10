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

with Ada.Unchecked_Deallocation;

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
      return Integer (Long_Long_Integer'(From_Bignum (Get_Bignum (Arg))));
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
         Set_Bignum (Result, To_Bignum (Long_Long_Long_Integer (Arg)));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int is
      begin
         return Int (Long_Long_Long_Integer'(From_Bignum (Get_Bignum (Arg))));
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
         Set_Bignum (Result, To_Bignum (Unsigned_128 (Arg)));
         return Result;
      end To_Big_Integer;

      ----------------------
      -- From_Big_Integer --
      ----------------------

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int is
      begin
         return Int (Unsigned_128'(From_Bignum (Get_Bignum (Arg))));
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

   function From_String (Arg : String) return Valid_Big_Integer is
      procedure Scan_Decimal
        (Arg : String; J : in out Natural; Result : out Big_Integer);
      --  Scan decimal value starting at Arg (J). Store value in Result if
      --  successful, raise Constraint_Error if not. On exit, J points to the
      --  first index past the decimal value.

      ------------------
      -- Scan_Decimal --
      ------------------

      procedure Scan_Decimal
        (Arg : String; J : in out Natural; Result : out Big_Integer)
      is
         Initial_J : constant Natural := J;
         Ten       : constant Big_Integer := To_Big_Integer (10);
      begin
         Result := To_Big_Integer (0);

         while J <= Arg'Last loop
            if Arg (J) in '0' .. '9' then
               Result :=
                 Result * Ten + To_Big_Integer (Character'Pos (Arg (J))
                                                  - Character'Pos ('0'));

            elsif Arg (J) = '_' then
               if J in Initial_J | Arg'Last
                 or else Arg (J - 1) not in '0' .. '9'
                 or else Arg (J + 1) not in '0' .. '9'
               then
                  raise Constraint_Error with "invalid integer value: " & Arg;
               end if;
            else
               exit;
            end if;

            J := J + 1;
         end loop;
      end Scan_Decimal;

      Result : Big_Integer;

   begin
      --  First try the fast path via Long_Long_Long_Integer'Value

      Set_Bignum (Result, To_Bignum (Long_Long_Long_Integer'Value (Arg)));
      return Result;

   exception
      when Constraint_Error =>
         --  Then try the slow path

         declare
            Neg        : Boolean  := False;
            Base_Found : Boolean  := False;
            Base_Int   : Positive := 10;
            J          : Natural  := Arg'First;
            Val        : Natural;
            Base       : Big_Integer;
            Exp        : Big_Integer;

         begin
            --  Scan past leading blanks

            while J <= Arg'Last and then Arg (J) = ' ' loop
               J := J + 1;
            end loop;

            if J > Arg'Last then
               raise;
            end if;

            --  Scan and store negative sign if found

            if Arg (J) = '-' then
               Neg := True;
               J   := J + 1;
            end if;

            --  Scan decimal value: either the result itself, or the base
            --  value if followed by a '#'.

            Scan_Decimal (Arg, J, Result);

            --  Scan explicit base if requested

            if J <= Arg'Last and then Arg (J) = '#' then
               Base_Int := To_Integer (Result);

               if Base_Int not in 2 .. 16 then
                  raise;
               end if;

               Base_Found := True;
               Base       := Result;
               Result     := To_Big_Integer (0);
               J          := J + 1;

               while J <= Arg'Last loop
                  case Arg (J) is
                     when '0' .. '9' =>
                        Val := Character'Pos (Arg (J)) - Character'Pos ('0');

                        if Val >= Base_Int then
                           raise;
                        end if;

                        Result := Result * Base + To_Big_Integer (Val);

                     when 'a' .. 'f' =>
                        Val :=
                          10 + Character'Pos (Arg (J)) - Character'Pos ('a');

                        if Val >= Base_Int then
                           raise;
                        end if;

                        Result := Result * Base + To_Big_Integer (Val);

                     when 'A' .. 'F' =>
                        Val :=
                          10 + Character'Pos (Arg (J)) - Character'Pos ('A');

                        if Val >= Base_Int then
                           raise;
                        end if;

                        Result := Result * Base + To_Big_Integer (Val);

                     when '_' =>

                        --  We only allow _ preceded and followed by a valid
                        --  number and not any other character.

                        if J in Arg'First | Arg'Last
                          or else Arg (J - 1) in '_' | '#'
                          or else Arg (J + 1) = '#'
                        then
                           raise;
                        end if;

                     when '#' =>
                        J := J + 1;
                        exit;

                     when others =>
                        raise;
                  end case;

                  J := J + 1;
               end loop;
            else
               Base := To_Big_Integer (10);
            end if;

            if Base_Found and then Arg (J - 1) /= '#' then
               raise;
            end if;

            if J <= Arg'Last then

               --  Scan exponent

               if Arg (J) in 'e' | 'E' then
                  J := J + 1;

                  if Arg (J) = '+' then
                     J := J + 1;
                  end if;

                  Scan_Decimal (Arg, J, Exp);
                  Result := Result * (Base ** To_Integer (Exp));
               end if;

               --  Scan past trailing blanks

               while J <= Arg'Last and then Arg (J) = ' ' loop
                  J := J + 1;
               end loop;

               if J <= Arg'Last then
                  raise;
               end if;
            end if;

            if Neg then
               return -Result;
            else
               return Result;
            end if;
         end;
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
