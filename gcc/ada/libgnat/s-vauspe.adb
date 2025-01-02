------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . V A L U E _ U _ S P E C                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2022-2025, Free Software Foundation, Inc.         --
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

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

package body System.Value_U_Spec with SPARK_Mode is

   -----------------------------
   -- Exponent_Unsigned_Ghost --
   -----------------------------

   function Exponent_Unsigned_Ghost
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10) return Uns_Option
   is
      (if Exp = 0 or Value = 0 then (Overflow => False, Value => Value)
       elsif Scan_Overflows_Ghost (0, Base, Value) then (Overflow => True)
       else Exponent_Unsigned_Ghost (Value * Base, Exp - 1, Base));

   ---------------------
   -- Last_Hexa_Ghost --
   ---------------------

   function Last_Hexa_Ghost (Str : String) return Positive is
   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "occurs in ghost code, not executable");

      for J in Str'Range loop
         if Str (J) not in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_' then
            return J - 1;
         end if;

         pragma Loop_Invariant
           (for all K in Str'First .. J =>
              Str (K) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_');
      end loop;

      return Str'Last;

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Last_Hexa_Ghost;

   -----------------------------
   -- Lemmas with null bodies --
   -----------------------------

   procedure Lemma_Scan_Based_Number_Ghost_Base
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   is null;

   procedure Lemma_Scan_Based_Number_Ghost_Underscore
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   is null;

   procedure Lemma_Scan_Based_Number_Ghost_Overflow
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   is null;

   procedure Lemma_Scan_Based_Number_Ghost_Step
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   is null;

   procedure Lemma_Exponent_Unsigned_Ghost_Base
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10)
   is null;

   procedure Lemma_Exponent_Unsigned_Ghost_Overflow
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10)
   is null;

   procedure Lemma_Exponent_Unsigned_Ghost_Step
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10)
   is null;

   --------------------------------------
   -- Prove_Scan_Based_Number_Ghost_Eq --
   --------------------------------------

   procedure Prove_Scan_Based_Number_Ghost_Eq
     (Str1, Str2 : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   is
   begin
      if From > To then
         null;
      elsif Str1 (From) = '_' then
         Prove_Scan_Based_Number_Ghost_Eq
           (Str1, Str2, From + 1, To, Base, Acc);
      elsif Scan_Overflows_Ghost
        (Hexa_To_Unsigned_Ghost (Str1 (From)), Base, Acc)
      then
         null;
      else
         Prove_Scan_Based_Number_Ghost_Eq
           (Str1, Str2, From + 1, To, Base,
            Base * Acc + Hexa_To_Unsigned_Ghost (Str1 (From)));
      end if;
   end Prove_Scan_Based_Number_Ghost_Eq;

   -----------------------------------
   -- Prove_Scan_Only_Decimal_Ghost --
   -----------------------------------

   procedure Prove_Scan_Only_Decimal_Ghost
     (Str : String;
      Val : Uns)
   is
      pragma Assert (Str (Str'First + 1) /= ' ');
      Non_Blank : constant Positive := First_Non_Space_Ghost
        (Str, Str'First, Str'Last);
      pragma Assert (Non_Blank = Str'First + 1);
      Fst_Num   : constant Positive :=
        (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank);
      pragma Assert (Fst_Num = Str'First + 1);
   begin
      pragma Assert
        (Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last)));
      pragma Assert
        (Scan_Split_No_Overflow_Ghost (Str, Str'First + 1, Str'Last));
      pragma Assert
        ((Val, 10, 0) = Scan_Split_Value_Ghost (Str, Str'First + 1, Str'Last));
      pragma Assert
        (Raw_Unsigned_No_Overflow_Ghost (Str, Fst_Num, Str'Last));
      pragma Assert (Val = Exponent_Unsigned_Ghost (Val, 0, 10).Value);
      pragma Assert (Is_Unsigned_Ghost (Str));
      pragma Assert (Is_Value_Unsigned_Ghost (Str, Val));
   end Prove_Scan_Only_Decimal_Ghost;

   -----------------------------
   -- Scan_Based_Number_Ghost --
   -----------------------------

   function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0) return Uns_Option
   is
      (if From > To then (Overflow => False, Value => Acc)
       elsif Str (From) = '_'
       then Scan_Based_Number_Ghost (Str, From + 1, To, Base, Acc)
       elsif Scan_Overflows_Ghost
         (Hexa_To_Unsigned_Ghost (Str (From)), Base, Acc)
       then (Overflow => True)
       else Scan_Based_Number_Ghost
         (Str, From + 1, To, Base,
          Base * Acc + Hexa_To_Unsigned_Ghost (Str (From))));

end System.Value_U_Spec;
