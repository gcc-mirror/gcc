------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . V A L U E _ I _ S P E C                  --
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

package body System.Value_I_Spec is

   -----------------------------------
   -- Prove_Scan_Only_Decimal_Ghost --
   -----------------------------------

   procedure Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int) is
      Non_Blank : constant Positive := First_Non_Space_Ghost
        (Str, Str'First, Str'Last);
      pragma Assert (Str (Str'First + 1) /= ' ');
      pragma Assert
        (if Val < 0 then Non_Blank = Str'First
         else
           Str (Str'First) = ' '
            and then Non_Blank = Str'First + 1);
      Minus     : constant Boolean := Str (Non_Blank) = '-';
      Fst_Num   : constant Positive :=
        (if Minus then Non_Blank + 1 else Non_Blank);
      pragma Assert (Fst_Num = Str'First + 1);
      Uval      : constant Uns := Abs_Uns_Of_Int (Val);

      procedure Prove_Conversion_Is_Identity (Val : Int; Uval : Uns)
      with
        Pre  => Minus = (Val < 0)
          and then Uval = Abs_Uns_Of_Int (Val),
        Post => Uns_Is_Valid_Int (Minus, Uval)
          and then Is_Int_Of_Uns (Minus, Uval, Val);
      --  Local proof of the unicity of the signed representation

      procedure Prove_Conversion_Is_Identity (Val : Int; Uval : Uns) is null;

   --  Start of processing for Prove_Scan_Only_Decimal_Ghost

   begin
      Prove_Conversion_Is_Identity (Val, Uval);
      pragma Assert
        (U_Spec.Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last)));
      pragma Assert
        (U_Spec.Scan_Split_No_Overflow_Ghost (Str, Fst_Num, Str'Last));
      U_Spec.Lemma_Exponent_Unsigned_Ghost_Base (Uval, 0, 10);
      pragma Assert
        (U_Spec.Raw_Unsigned_No_Overflow_Ghost (Str, Fst_Num, Str'Last));
      pragma Assert (Only_Space_Ghost
        (Str, U_Spec.Raw_Unsigned_Last_Ghost
                        (Str, Fst_Num, Str'Last), Str'Last));
      pragma Assert (Is_Integer_Ghost (Str));
      pragma Assert (Is_Value_Integer_Ghost (Str, Val));
   end Prove_Scan_Only_Decimal_Ghost;

end System.Value_I_Spec;
