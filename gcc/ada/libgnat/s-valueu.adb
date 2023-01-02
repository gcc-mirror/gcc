------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ U                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

package body System.Value_U is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Subprogram_Variant => Ignore);

   use type Spec.Uns_Option;
   use type Spec.Split_Value_Ghost;

   --  Local lemmas

   procedure Lemma_Digit_Not_Last
     (Str  : String;
      P    : Integer;
      From : Integer;
      To   : Integer)
   with Ghost,
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
       and then P in From .. To
       and then P <= Spec.Last_Hexa_Ghost (Str (From .. To)) + 1
       and then Spec.Is_Based_Format_Ghost (Str (From .. To)),
     Post =>
       (if Str (P) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
        then P <= Spec.Last_Hexa_Ghost (Str (From .. To)));

   procedure Lemma_Underscore_Not_Last
     (Str  : String;
      P    : Integer;
      From : Integer;
      To   : Integer)
   with Ghost,
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
       and then P in From .. To
       and then Str (P) = '_'
       and then P <= Spec.Last_Hexa_Ghost (Str (From .. To)) + 1
       and then Spec.Is_Based_Format_Ghost (Str (From .. To)),
     Post => P + 1 <= Spec.Last_Hexa_Ghost (Str (From .. To))
       and then Str (P + 1) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';

   -----------------------------
   -- Local lemma null bodies --
   -----------------------------

   procedure Lemma_Digit_Not_Last
     (Str  : String;
      P    : Integer;
      From : Integer;
      To   : Integer)
   is null;

   procedure Lemma_Underscore_Not_Last
     (Str  : String;
      P    : Integer;
      From : Integer;
      To   : Integer)
   is null;

   -----------------------
   -- Scan_Raw_Unsigned --
   -----------------------

   procedure Scan_Raw_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Uns)
   is
      P : Integer;
      --  Local copy of the pointer

      Uval : Uns;
      --  Accumulated unsigned integer result

      Expon : Integer;
      --  Exponent value

      Overflow : Boolean := False;
      --  Set True if overflow is detected at any point

      Base_Char : Character;
      --  Base character (# or :) in based case

      Base : Uns := 10;
      --  Base value (reset in based case)

      Digit : Uns;
      --  Digit value

      Ptr_Old       : constant Integer := Ptr.all
      with Ghost;
      Last_Num_Init : constant Integer :=
        Last_Number_Ghost (Str (Ptr.all .. Max))
      with Ghost;
      Init_Val      : constant Spec.Uns_Option :=
        Spec.Scan_Based_Number_Ghost (Str, Ptr.all, Last_Num_Init)
      with Ghost;
      Starts_As_Based : constant Boolean :=
        Last_Num_Init < Max - 1
        and then Str (Last_Num_Init + 1) in '#' | ':'
        and then Str (Last_Num_Init + 2) in
        '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
      with Ghost;
      Last_Num_Based  : constant Integer :=
        (if Starts_As_Based
         then Spec.Last_Hexa_Ghost (Str (Last_Num_Init + 2 .. Max))
         else Last_Num_Init)
      with Ghost;
      Is_Based        : constant Boolean :=
        Starts_As_Based
        and then Last_Num_Based < Max
        and then Str (Last_Num_Based + 1) = Str (Last_Num_Init + 1)
      with Ghost;
      Based_Val       : constant Spec.Uns_Option :=
        (if Starts_As_Based and then not Init_Val.Overflow
         then Spec.Scan_Based_Number_Ghost
           (Str, Last_Num_Init + 2, Last_Num_Based, Init_Val.Value)
         else Init_Val)
      with Ghost;
      First_Exp       : constant Integer :=
        (if Is_Based then Last_Num_Based + 2 else Last_Num_Init + 1)
      with Ghost;

   begin
      --  We do not tolerate strings with Str'Last = Positive'Last

      if Str'Last = Positive'Last then
         raise Program_Error with
           "string upper bound is Positive'Last, not supported";
      end if;

      P := Ptr.all;
      Spec.Lemma_Scan_Based_Number_Ghost_Step (Str, P, Last_Num_Init);
      Uval := Character'Pos (Str (P)) - Character'Pos ('0');
      P := P + 1;

      --  Scan out digits of what is either the number or the base.
      --  In either case, we are definitely scanning out in base 10.

      declare
         Umax : constant Uns := (Uns'Last - 9) / 10;
         --  Max value which cannot overflow on accumulating next digit

         Umax10 : constant Uns := Uns'Last / 10;
         --  Numbers bigger than Umax10 overflow if multiplied by 10

      begin
         --  Loop through decimal digits
         loop
            pragma Loop_Invariant (P in P'Loop_Entry .. Last_Num_Init + 1);
            pragma Loop_Invariant
              (if Overflow then Init_Val.Overflow);
            pragma Loop_Invariant
              (if not Overflow
               then Init_Val = Spec.Scan_Based_Number_Ghost
                 (Str, P, Last_Num_Init, Acc => Uval));

            exit when P > Max;

            Digit := Character'Pos (Str (P)) - Character'Pos ('0');

            --  Non-digit encountered

            if Digit > 9 then
               if Str (P) = '_' then
                  Spec.Lemma_Scan_Based_Number_Ghost_Underscore
                    (Str, P, Last_Num_Init, Acc => Uval);
                  Scan_Underscore (Str, P, Ptr, Max, False);
               else
                  exit;
               end if;

            --  Accumulate result, checking for overflow

            else
               Spec.Lemma_Scan_Based_Number_Ghost_Step
                 (Str, P, Last_Num_Init, Acc => Uval);
               Spec.Lemma_Scan_Based_Number_Ghost_Overflow
                 (Str, P, Last_Num_Init, Acc => Uval);

               if Uval <= Umax then
                  pragma Assert
                    (Spec.Hexa_To_Unsigned_Ghost (Str (P)) = Digit);
                  Uval := 10 * Uval + Digit;
                  pragma Assert
                    (if not Overflow
                     then Init_Val = Spec.Scan_Based_Number_Ghost
                       (Str, P + 1, Last_Num_Init, Acc => Uval));

               elsif Uval > Umax10 then
                  Overflow := True;

               else
                  Uval := 10 * Uval + Digit;

                  if Uval < Umax10 then
                     Overflow := True;
                  end if;
                  pragma Assert
                    (if not Overflow
                     then Init_Val = Spec.Scan_Based_Number_Ghost
                       (Str, P + 1, Last_Num_Init, Acc => Uval));
               end if;

               P := P + 1;
            end if;
         end loop;
         Spec.Lemma_Scan_Based_Number_Ghost_Base
            (Str, P, Last_Num_Init, Acc => Uval);
      end;

      pragma Assert_And_Cut
        (P = Last_Num_Init + 1
         and then Overflow = Init_Val.Overflow
         and then (if not Overflow then Init_Val.Value = Uval));

      Ptr.all := P;

      --  Deal with based case. We recognize either the standard '#' or the
      --  allowed alternative replacement ':' (see RM J.2(3)).

      if P < Max and then (Str (P) = '#' or else Str (P) = ':') then
         Base_Char := Str (P);
         P := P + 1;
         Base := Uval;
         Uval := 0;

         --  Check base value. Overflow is set True if we find a bad base, or
         --  a digit that is out of range of the base. That way, we scan out
         --  the numeral that is still syntactically correct, though illegal.
         --  We use a safe base of 16 for this scan, to avoid zero divide.

         if Base not in 2 .. 16 then
            Overflow := True;
            Base := 16;
         end if;

         --  Scan out based integer

         declare
            Umax : constant Uns := (Uns'Last - Base + 1) / Base;
            --  Max value which cannot overflow on accumulating next digit

            UmaxB : constant Uns := Uns'Last / Base;
            --  Numbers bigger than UmaxB overflow if multiplied by base

         begin
            pragma Assert
              (if Str (P) in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'
               then Spec.Is_Based_Format_Ghost (Str (P .. Max)));

            --  Loop to scan out based integer value

            loop
               --  We require a digit at this stage

               if Str (P) in '0' .. '9' then
                  Digit := Character'Pos (Str (P)) - Character'Pos ('0');

               elsif Str (P) in 'A' .. 'F' then
                  Digit :=
                    Character'Pos (Str (P)) - (Character'Pos ('A') - 10);

               elsif Str (P) in 'a' .. 'f' then
                  Digit :=
                    Character'Pos (Str (P)) - (Character'Pos ('a') - 10);

               --  If we don't have a digit, then this is not a based number
               --  after all, so we use the value we scanned out as the base
               --  (now in Base), and the pointer to the base character was
               --  already stored in Ptr.all.

               else
                  Spec.Lemma_Scan_Based_Number_Ghost_Base
                    (Str, P, Last_Num_Based, Base, Uval);
                  Uval := Base;
                  Base := 10;
                  pragma Assert (Ptr.all = Last_Num_Init + 1);
                  pragma Assert
                    (if Starts_As_Based then P = Last_Num_Based + 1);
                  pragma Assert (not Is_Based);
                  pragma Assert (if not Overflow then Uval = Init_Val.Value);
                  exit;
               end if;

               pragma Loop_Invariant (P in P'Loop_Entry .. Last_Num_Based);
               pragma Loop_Invariant
                 (Str (P) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
                  and then Digit = Spec.Hexa_To_Unsigned_Ghost (Str (P)));
               pragma Loop_Invariant
                 (if Overflow'Loop_Entry then Overflow);
               pragma Loop_Invariant
                 (if Overflow then
                    (Overflow'Loop_Entry or else Based_Val.Overflow));
               pragma Loop_Invariant
                 (if not Overflow
                  then Based_Val = Spec.Scan_Based_Number_Ghost
                    (Str, P, Last_Num_Based, Base, Uval));
               pragma Loop_Invariant (Ptr.all = Last_Num_Init + 1);

               Spec.Lemma_Scan_Based_Number_Ghost_Step
                 (Str, P, Last_Num_Based, Base, Uval);
               Spec.Lemma_Scan_Based_Number_Ghost_Overflow
                 (Str, P, Last_Num_Based, Base, Uval);

               --  If digit is too large, just signal overflow and continue.
               --  The idea here is to keep scanning as long as the input is
               --  syntactically valid, even if we have detected overflow

               if Digit >= Base then
                  Overflow := True;

               --  Here we accumulate the value, checking overflow

               elsif Uval <= Umax then
                  Uval := Base * Uval + Digit;
                  pragma Assert
                    (if not Overflow
                     then Based_Val = Spec.Scan_Based_Number_Ghost
                       (Str, P + 1, Last_Num_Based, Base, Uval));

               elsif Uval > UmaxB then
                  Overflow := True;

               else
                  Uval := Base * Uval + Digit;

                  if Uval < UmaxB then
                     Overflow := True;
                  end if;
                  pragma Assert
                    (if not Overflow
                     then Based_Val = Spec.Scan_Based_Number_Ghost
                       (Str, P + 1, Last_Num_Based, Base, Uval));
               end if;

               --  If at end of string with no base char, not a based number
               --  but we signal Constraint_Error and set the pointer past
               --  the end of the field, since this is what the ACVC tests
               --  seem to require, see CE3704N, line 204.

               P := P + 1;

               if P > Max then
                  Ptr.all := P;
                  Bad_Value (Str);
               end if;

               --  If terminating base character, we are done with loop

               if Str (P) = Base_Char then
                  Ptr.all := P + 1;
                  pragma Assert (P = Last_Num_Based + 1);
                  pragma Assert (Ptr.all = Last_Num_Based + 2);
                  pragma Assert (Starts_As_Based);
                  pragma Assert (Last_Num_Based < Max);
                  pragma Assert (Str (Last_Num_Based + 1) = Base_Char);
                  pragma Assert (Base_Char = Str (Last_Num_Init + 1));
                  pragma Assert (Is_Based);
                  Spec.Lemma_Scan_Based_Number_Ghost_Base
                    (Str, P, Last_Num_Based, Base, Uval);
                  exit;

               --  Deal with underscore

               elsif Str (P) = '_' then
                  Lemma_Underscore_Not_Last (Str, P, Last_Num_Init + 2, Max);
                  Spec.Lemma_Scan_Based_Number_Ghost_Underscore
                    (Str, P, Last_Num_Based, Base, Uval);
                  Scan_Underscore (Str, P, Ptr, Max, True);
                  pragma Assert
                    (if not Overflow
                     then Based_Val = Spec.Scan_Based_Number_Ghost
                       (Str, P, Last_Num_Based, Base, Uval));
                  pragma Assert (Str (P) /= '_');
                  pragma Assert (Str (P) /= Base_Char);
               end if;

               Lemma_Digit_Not_Last (Str, P, Last_Num_Init + 2, Max);
               pragma Assert (Str (P) /= '_');
               pragma Assert (Str (P) /= Base_Char);
            end loop;
         end;
         pragma Assert
           (if Starts_As_Based then P = Last_Num_Based + 1
            else P = Last_Num_Init + 2);
         pragma Assert
           (Last_Num_Init < Max - 1
            and then Str (Last_Num_Init + 1) in '#' | ':');
         pragma Assert
           (Overflow =
              (Init_Val.Overflow
               or else Init_Val.Value not in 2 .. 16
               or else (Starts_As_Based and then Based_Val.Overflow)));
         pragma Assert
           (Overflow /= Spec.Scan_Split_No_Overflow_Ghost (Str, Ptr_Old, Max));
      end if;

      pragma Assert_And_Cut
        (Overflow /= Spec.Scan_Split_No_Overflow_Ghost (Str, Ptr_Old, Max)
         and then
           (if not Overflow then
                (if Is_Based then Uval = Based_Val.Value
                 else Uval = Init_Val.Value))
         and then Ptr.all = First_Exp
         and then Base in 2 .. 16
         and then
           (if not Overflow then
                (if Is_Based then Base = Init_Val.Value else Base = 10)));

      --  Come here with scanned unsigned value in Uval. The only remaining
      --  required step is to deal with exponent if one is present.

      Scan_Exponent (Str, Ptr, Max, Expon);

      pragma Assert
        (Ptr.all = Spec.Raw_Unsigned_Last_Ghost (Str, Ptr_Old, Max));
      pragma Assert
        (if not Overflow
         then Spec.Scan_Split_Value_Ghost (Str, Ptr_Old, Max) =
           (Uval, Base, Expon));

      if Expon /= 0 and then Uval /= 0 then

         --  For non-zero value, scale by exponent value. No need to do this
         --  efficiently, since use of exponent in integer literals is rare,
         --  and in any case the exponent cannot be very large.

         declare
            UmaxB : constant Uns := Uns'Last / Base;
            --  Numbers bigger than UmaxB overflow if multiplied by base

            Res_Val : constant Spec.Uns_Option :=
              Spec.Exponent_Unsigned_Ghost (Uval, Expon, Base)
            with Ghost;
         begin
            for J in 1 .. Expon loop
               pragma Loop_Invariant
                 (if Overflow'Loop_Entry then Overflow);
               pragma Loop_Invariant
                 (if Overflow
                  then Overflow'Loop_Entry or else Res_Val.Overflow);
               pragma Loop_Invariant (Uval /= 0);
               pragma Loop_Invariant
                 (if not Overflow
                  then Res_Val = Spec.Exponent_Unsigned_Ghost
                    (Uval, Expon - J + 1, Base));

               pragma Assert
                 ((Uval > UmaxB) = Spec.Scan_Overflows_Ghost (0, Base, Uval));

               if Uval > UmaxB then
                  Spec.Lemma_Exponent_Unsigned_Ghost_Overflow
                     (Uval, Expon - J + 1, Base);
                  Overflow := True;
                  exit;
               end if;

               Spec.Lemma_Exponent_Unsigned_Ghost_Step
                  (Uval, Expon - J + 1, Base);

               Uval := Uval * Base;
            end loop;
            Spec.Lemma_Exponent_Unsigned_Ghost_Base (Uval, 0, Base);

            pragma Assert
              (Overflow /=
                 Spec.Raw_Unsigned_No_Overflow_Ghost (Str, Ptr_Old, Max));
            pragma Assert (if not Overflow then Res_Val = (False, Uval));
         end;
      end if;
      Spec.Lemma_Exponent_Unsigned_Ghost_Base (Uval, Expon, Base);
      pragma Assert
        (if Expon = 0 or else Uval = 0 then
            Spec.Exponent_Unsigned_Ghost (Uval, Expon, Base) = (False, Uval));
      pragma Assert
        (Overflow /=
           Spec.Raw_Unsigned_No_Overflow_Ghost (Str, Ptr_Old, Max));
      pragma Assert
        (if not Overflow then
            Uval = Spec.Scan_Raw_Unsigned_Ghost (Str, Ptr_Old, Max));

      --  Return result, dealing with overflow

      if Overflow then
         Bad_Value (Str);
         pragma Annotate
           (GNATprove, Intentional,
            "call to nonreturning subprogram might be executed",
            "it is expected that Constraint_Error is raised in case of"
            & " overflow");
      else
         Res := Uval;
      end if;
   end Scan_Raw_Unsigned;

   -------------------
   -- Scan_Unsigned --
   -------------------

   procedure Scan_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Uns)
   is
      Start : Positive;
      --  Save location of first non-blank character

   begin
      pragma Warnings
        (Off,
         """Start"" is set by ""Scan_Plus_Sign"" but not used after the call");
      Scan_Plus_Sign (Str, Ptr, Max, Start);
      pragma Warnings
        (On,
         """Start"" is set by ""Scan_Plus_Sign"" but not used after the call");

      if Str (Ptr.all) not in '0' .. '9' then
         Ptr.all := Start;
         Bad_Value (Str);
      end if;

      Scan_Raw_Unsigned (Str, Ptr, Max, Res);
   end Scan_Unsigned;

   --------------------
   -- Value_Unsigned --
   --------------------

   function Value_Unsigned (Str : String) return Uns is
   begin
      --  We have to special case Str'Last = Positive'Last because the normal
      --  circuit ends up setting P to Str'Last + 1 which is out of bounds. We
      --  deal with this by converting to a subtype which fixes the bounds.

      if Str'Last = Positive'Last then
         declare
            subtype NT is String (1 .. Str'Length);
            procedure Prove_Is_Unsigned_Ghost with
              Ghost,
              Pre  => Str'Length < Natural'Last
              and then not Only_Space_Ghost (Str, Str'First, Str'Last)
              and then Spec.Is_Unsigned_Ghost (Spec.Slide_To_1 (Str)),
              Post => Spec.Is_Unsigned_Ghost (NT (Str));
            procedure Prove_Is_Unsigned_Ghost is null;
         begin
            Prove_Is_Unsigned_Ghost;
            return Value_Unsigned (NT (Str));
         end;

      --  Normal case where Str'Last < Positive'Last

      else
         declare
            V : Uns;
            P : aliased Integer := Str'First;
            Non_Blank : constant Positive := First_Non_Space_Ghost
              (Str, Str'First, Str'Last)
            with Ghost;
            Fst_Num   : constant Positive :=
              (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank)
            with Ghost;
         begin
            declare
               P_Acc : constant not null access Integer := P'Access;
            begin
               Scan_Unsigned (Str, P_Acc, Str'Last, V);
            end;

            pragma Assert
              (P = Spec.Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last));
            pragma Assert
              (V = Spec.Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last));

            Scan_Trailing_Blanks (Str, P);

            pragma Assert
              (Spec.Is_Value_Unsigned_Ghost
                 (Spec.Slide_If_Necessary (Str), V));
            return V;
         end;
      end if;
   end Value_Unsigned;

end System.Value_U;
