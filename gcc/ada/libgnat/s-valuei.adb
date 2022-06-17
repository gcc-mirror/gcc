------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

package body System.Value_I is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Subprogram_Variant => Ignore);

   -----------------------------------
   -- Prove_Scan_Only_Decimal_Ghost --
   -----------------------------------

   procedure Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int) is
      Non_Blank : constant Positive := First_Non_Space_Ghost
        (Str, Str'First, Str'Last);
      pragma Assert
        (if Val < 0 then Non_Blank = Str'First
         else
            Only_Space_Ghost (Str, Str'First, Str'First)
            and then Non_Blank = Str'First + 1);
      Minus : constant Boolean := Str (Non_Blank) = '-';
      Fst_Num   : constant Positive :=
        (if Minus then Non_Blank + 1 else Non_Blank);
      pragma Assert (Fst_Num = Str'First + 1);
      Uval      : constant Uns :=
        Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last);

      procedure Unique_Int_Of_Uns (Val1, Val2 : Int)
      with
        Pre  => Uns_Is_Valid_Int (Minus, Uval)
          and then Is_Int_Of_Uns (Minus, Uval, Val1)
          and then Is_Int_Of_Uns (Minus, Uval, Val2),
        Post => Val1 = Val2;
      --  Local proof of the unicity of the signed representation

      procedure Unique_Int_Of_Uns (Val1, Val2 : Int) is null;

   --  Start of processing for Prove_Scan_Only_Decimal_Ghost

   begin
      pragma Assert (Minus = (Val < 0));
      pragma Assert (Uval = Abs_Uns_Of_Int (Val));
      pragma Assert (if Minus then Uval <= Uns (Int'Last) + 1
                     else Uval <= Uns (Int'Last));
      pragma Assert (Uns_Is_Valid_Int (Minus, Uval));
      pragma Assert
        (if Minus and then Uval = Uns (Int'Last) + 1 then Val = Int'First
         elsif Minus then Val = -(Int (Uval))
         else Val = Int (Uval));
      pragma Assert (Is_Int_Of_Uns (Minus, Uval, Val));
      pragma Assert
        (Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last)));
      pragma Assert
        (not Raw_Unsigned_Overflows_Ghost (Str, Fst_Num, Str'Last));
      pragma Assert (Only_Space_Ghost
        (Str, Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last), Str'Last));
      pragma Assert (Is_Integer_Ghost (Str));
      pragma Assert (Is_Value_Integer_Ghost (Str, Val));
      Unique_Int_Of_Uns (Val, Value_Integer (Str));
   end Prove_Scan_Only_Decimal_Ghost;

   ------------------
   -- Scan_Integer --
   ------------------

   procedure Scan_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Int)
   is
      Uval : Uns;
      --  Unsigned result

      Minus : Boolean;
      --  Set to True if minus sign is present, otherwise to False

      Unused_Start : Positive;
      --  Saves location of first non-blank (not used in this case)

      Non_Blank : constant Positive :=
        First_Non_Space_Ghost (Str, Ptr.all, Max)
      with Ghost;

      Fst_Num   : constant Positive :=
        (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1
         else Non_Blank)
      with Ghost;

   begin
      Scan_Sign (Str, Ptr, Max, Minus, Unused_Start);

      if Str (Ptr.all) not in '0' .. '9' then
         Ptr.all := Unused_Start;
         Bad_Value (Str);
      end if;

      Scan_Raw_Unsigned (Str, Ptr, Max, Uval);
      pragma Assert (Uval = Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Max));

      --  Deal with overflow cases, and also with largest negative number

      if Uval > Uns (Int'Last) then
         if Minus and then Uval = Uns (Int'Last) + 1 then
            Res := Int'First;
         else
            Bad_Value (Str);
         end if;

      --  Negative values

      elsif Minus then
         Res := -(Int (Uval));

      --  Positive values

      else
         Res := Int (Uval);
      end if;
   end Scan_Integer;

   -------------------
   -- Value_Integer --
   -------------------

   function Value_Integer (Str : String) return Int is
   begin
      --  We have to special case Str'Last = Positive'Last because the normal
      --  circuit ends up setting P to Str'Last + 1 which is out of bounds. We
      --  deal with this by converting to a subtype which fixes the bounds.

      if Str'Last = Positive'Last then
         declare
            subtype NT is String (1 .. Str'Length);
         begin
            return Value_Integer (NT (Str));
         end;

      --  Normal case where Str'Last < Positive'Last

      else
         declare
            V : Int;
            P : aliased Integer := Str'First;

            Non_Blank : constant Positive := First_Non_Space_Ghost
              (Str, Str'First, Str'Last)
            with Ghost;

            Fst_Num   : constant Positive :=
              (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1
               else Non_Blank)
            with Ghost;
         begin
            pragma Assert
              (Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last)));

            declare
               P_Acc : constant not null access Integer := P'Access;
            begin
               Scan_Integer (Str, P_Acc, Str'Last, V);
            end;

            pragma Assert
              (P = Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last));

            Scan_Trailing_Blanks (Str, P);

            pragma Assert
              (Is_Value_Integer_Ghost (Slide_If_Necessary (Str), V));
            return V;
         end;
      end if;
   end Value_Integer;

end System.Value_I;
