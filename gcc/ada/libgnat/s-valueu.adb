------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ U                        --
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

package body System.Value_U is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Subprogram_Variant => Ignore);

   --  Local lemmas

   procedure Lemma_Digit_Is_Before_Last
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
       and then Str (P) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F',
     Post => P /= Last_Hexa_Ghost (Str (From .. To)) + 1;
   --  If the character at position P is a digit, P cannot be the position of
   --  of the first non-digit in Str.

   procedure Lemma_End_Of_Scan
     (Str  : String;
      From : Integer;
      To   : Integer;
      Base : Uns;
      Acc  : Uns)
   with Ghost,
     Pre  => Str'Last /= Positive'Last and then From > To,
     Post => Scan_Based_Number_Ghost (Str, From, To, Base, Acc) =
       (False, Acc);
   --  Unfold the definition of Scan_Based_Number_Ghost on an empty string

   procedure Lemma_Scan_Digit
     (Str          : String;
      P            : Integer;
      Lst          : Integer;
      Digit        : Uns;
      Base         : Uns;
      Old_Acc      : Uns;
      Acc          : Uns;
      Scan_Val     : Uns_Option;
      Old_Overflow : Boolean;
      Overflow     : Boolean)
   with Ghost,
     Pre  => Str'Last /= Positive'Last
       and then Lst in Str'Range
       and then P in Str'First .. Lst
       and then Str (P) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
       and then Digit = Hexa_To_Unsigned_Ghost (Str (P))
       and then Only_Hexa_Ghost (Str, P, Lst)
       and then Base in 2 .. 16
       and then (if Digit < Base and then Old_Acc <= Uns'Last / Base
                 then Acc = Base * Old_Acc + Digit)
       and then (if Digit >= Base
                   or else Old_Acc > Uns'Last / Base
                   or else (Old_Acc > (Uns'Last - Base + 1) / Base
                            and then Acc < Uns'Last / Base)
                 then Overflow
                 else Overflow = Old_Overflow)
       and then
         (if not Old_Overflow then
           Scan_Val = Scan_Based_Number_Ghost
              (Str, P, Lst, Base, Old_Acc)),
     Post =>
        (if not Overflow then
           Scan_Val = Scan_Based_Number_Ghost
             (Str, P + 1, Lst, Base, Acc))
       and then
        (if Overflow then Old_Overflow or else Scan_Val.Overflow);
   --  Unfold the definition of Scan_Based_Number_Ghost when the string starts
   --  with a digit.

   procedure Lemma_Scan_Underscore
     (Str      : String;
      P        : Integer;
      From     : Integer;
      To       : Integer;
      Lst      : Integer;
      Base     : Uns;
      Acc      : Uns;
      Scan_Val : Uns_Option;
      Overflow : Boolean;
      Ext      : Boolean)
   with Ghost,
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Lst <= To
       and then P in From .. Lst + 1
       and then P <= To
       and then
         (if Ext then
            Is_Based_Format_Ghost (Str (From .. To))
            and then Lst = Last_Hexa_Ghost (Str (From .. To))
          else Is_Natural_Format_Ghost (Str (From .. To))
            and then Lst = Last_Number_Ghost (Str (From .. To)))
       and then Str (P) = '_'
       and then
         (if not Overflow then
           Scan_Val = Scan_Based_Number_Ghost (Str, P, Lst, Base, Acc)),
     Post => P + 1 <= Lst
       and then
         (if Ext then Str (P + 1) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
          else Str (P + 1) in '0' .. '9')
       and then
         (if not Overflow then
           Scan_Val = Scan_Based_Number_Ghost (Str, P + 1, Lst, Base, Acc));
   --  Unfold the definition of Scan_Based_Number_Ghost when the string starts
   --  with an underscore.

   -----------------------------
   -- Local lemma null bodies --
   -----------------------------

   procedure Lemma_Digit_Is_Before_Last
     (Str  : String;
      P    : Integer;
      From : Integer;
      To   : Integer)
   is null;

   procedure Lemma_End_Of_Scan
     (Str          : String;
      From         : Integer;
      To           : Integer;
      Base         : Uns;
      Acc          : Uns)
   is null;

   procedure Lemma_Scan_Underscore
     (Str      : String;
      P        : Integer;
      From     : Integer;
      To       : Integer;
      Lst      : Integer;
      Base     : Uns;
      Acc      : Uns;
      Scan_Val : Uns_Option;
      Overflow : Boolean;
      Ext      : Boolean)
   is null;

   ---------------------
   -- Last_Hexa_Ghost --
   ---------------------

   function Last_Hexa_Ghost (Str : String) return Positive is
   begin
      for J in Str'Range loop
         if Str (J) not in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_' then
            return J - 1;
         end if;

         pragma Loop_Invariant
           (for all K in Str'First .. J =>
              Str (K) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_');
      end loop;

      return Str'Last;
   end Last_Hexa_Ghost;

   ----------------------
   -- Lemma_Scan_Digit --
   ----------------------

   procedure Lemma_Scan_Digit
     (Str          : String;
      P            : Integer;
      Lst          : Integer;
      Digit        : Uns;
      Base         : Uns;
      Old_Acc      : Uns;
      Acc          : Uns;
      Scan_Val     : Uns_Option;
      Old_Overflow : Boolean;
      Overflow     : Boolean)
   is
      pragma Unreferenced (Str, P, Lst, Scan_Val, Overflow, Old_Overflow);
   begin
      if Digit >= Base then
         null;

      elsif Old_Acc <= (Uns'Last - Base + 1) / Base then
         pragma Assert (not Scan_Overflows_Ghost (Digit, Base, Old_Acc));

      elsif Old_Acc > Uns'Last / Base then
         null;

      else
         pragma Assert
           ((Acc < Uns'Last / Base) =
              Scan_Overflows_Ghost (Digit, Base, Old_Acc));
      end if;
   end Lemma_Scan_Digit;

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
      Init_Val      : constant Uns_Option :=
        Scan_Based_Number_Ghost (Str, Ptr.all, Last_Num_Init)
      with Ghost;
      Starts_As_Based : constant Boolean :=
        Last_Num_Init < Max - 1
        and then Str (Last_Num_Init + 1) in '#' | ':'
        and then Str (Last_Num_Init + 2) in
        '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
      with Ghost;
      Last_Num_Based  : constant Integer :=
        (if Starts_As_Based
         then Last_Hexa_Ghost (Str (Last_Num_Init + 2 .. Max))
         else Last_Num_Init)
      with Ghost;
      Is_Based        : constant Boolean :=
        Starts_As_Based
        and then Last_Num_Based < Max
        and then Str (Last_Num_Based + 1) = Str (Last_Num_Init + 1)
      with Ghost;
      Based_Val       : constant Uns_Option :=
        (if Starts_As_Based and then not Init_Val.Overflow
         then Scan_Based_Number_Ghost
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
      Uval := Character'Pos (Str (P)) - Character'Pos ('0');
      P := P + 1;

      --  Scan out digits of what is either the number or the base.
      --  In either case, we are definitely scanning out in base 10.

      declare
         Umax : constant Uns := (Uns'Last - 9) / 10;
         --  Max value which cannot overflow on accumulating next digit

         Umax10 : constant Uns := Uns'Last / 10;
         --  Numbers bigger than Umax10 overflow if multiplied by 10

         Old_Uval     : Uns with Ghost;
         Old_Overflow : Boolean with Ghost;

      begin
         --  Loop through decimal digits
         loop
            pragma Loop_Invariant (P in P'Loop_Entry .. Last_Num_Init + 1);
            pragma Loop_Invariant
              (if Overflow then Init_Val.Overflow);
            pragma Loop_Invariant
              (if not Overflow
               then Init_Val = Scan_Based_Number_Ghost
                 (Str, P, Last_Num_Init, Acc => Uval));

            exit when P > Max;

            Digit := Character'Pos (Str (P)) - Character'Pos ('0');

            --  Non-digit encountered

            if Digit > 9 then
               if Str (P) = '_' then
                  Lemma_Scan_Underscore
                    (Str, P, Ptr_Old, Max, Last_Num_Init, 10, Uval,
                     Init_Val, Overflow, False);
                  Scan_Underscore (Str, P, Ptr, Max, False);
               else
                  exit;
               end if;

            --  Accumulate result, checking for overflow

            else
               Old_Uval := Uval;
               Old_Overflow := Overflow;

               if Uval <= Umax then
                  Uval := 10 * Uval + Digit;

               elsif Uval > Umax10 then
                  Overflow := True;

               else
                  Uval := 10 * Uval + Digit;

                  if Uval < Umax10 then
                     Overflow := True;
                  end if;
               end if;

               Lemma_Scan_Digit
                 (Str, P, Last_Num_Init, Digit, 10, Old_Uval, Uval, Init_Val,
                  Old_Overflow, Overflow);

               P := P + 1;
            end if;
         end loop;
         pragma Assert (P = Last_Num_Init + 1);
         pragma Assert (Init_Val.Overflow = Overflow);
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

            Old_Uval     : Uns with Ghost;
            Old_Overflow : Boolean with Ghost;

         begin
            pragma Assert
              (if Str (P) in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'
               then Is_Based_Format_Ghost (Str (P .. Max)));

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
                  Uval := Base;
                  Base := 10;
                  pragma Assert (Ptr.all = Last_Num_Init + 1);
                  pragma Assert (if not Overflow then Uval = Init_Val.Value);
                  exit;
               end if;

               Lemma_Digit_Is_Before_Last (Str, P, Last_Num_Init + 2, Max);

               pragma Loop_Invariant (P in P'Loop_Entry .. Last_Num_Based);
               pragma Loop_Invariant
                 (Str (P) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
                  and then Digit = Hexa_To_Unsigned_Ghost (Str (P)));
               pragma Loop_Invariant
                 (if Overflow'Loop_Entry then Overflow);
               pragma Loop_Invariant
                 (if Overflow then
                    Overflow'Loop_Entry or else Based_Val.Overflow);
               pragma Loop_Invariant
                 (if not Overflow
                  then Based_Val = Scan_Based_Number_Ghost
                    (Str, P, Last_Num_Based, Base, Uval));
               pragma Loop_Invariant (Ptr.all = Last_Num_Init + 1);

               Old_Uval := Uval;
               Old_Overflow := Overflow;

               --  If digit is too large, just signal overflow and continue.
               --  The idea here is to keep scanning as long as the input is
               --  syntactically valid, even if we have detected overflow

               if Digit >= Base then
                  Overflow := True;

               --  Here we accumulate the value, checking overflow

               elsif Uval <= Umax then
                  Uval := Base * Uval + Digit;

               elsif Uval > UmaxB then
                  Overflow := True;

               else
                  Uval := Base * Uval + Digit;

                  if Uval < UmaxB then
                     Overflow := True;
                  end if;
               end if;

               Lemma_Scan_Digit
                 (Str, P, Last_Num_Based, Digit, Base, Old_Uval, Uval,
                  Based_Val, Old_Overflow, Overflow);

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
                  pragma Assert (Ptr.all = Last_Num_Based + 2);
                  Lemma_End_Of_Scan (Str, P, Last_Num_Based, Base, Uval);
                  pragma Assert (if not Overflow then Uval = Based_Val.Value);
                  exit;

               --  Deal with underscore

               elsif Str (P) = '_' then
                  Lemma_Scan_Underscore
                    (Str, P, Last_Num_Init + 2, Max, Last_Num_Based, Base,
                     Uval, Based_Val, Overflow, True);
                  Scan_Underscore (Str, P, Ptr, Max, True);
                  pragma Assert
                    (if not Overflow
                     then Based_Val = Scan_Based_Number_Ghost
                       (Str, P, Last_Num_Based, Base, Uval));
               end if;
            end loop;
         end;
         pragma Assert
           (if Starts_As_Based then P = Last_Num_Based + 1
            else P = Last_Num_Init + 2);
         pragma Assert
           (Overflow =
              (Init_Val.Overflow
               or else Init_Val.Value not in 2 .. 16
               or else (Starts_As_Based and then Based_Val.Overflow)));
      end if;

      pragma Assert_And_Cut
        (Overflow =
           (Init_Val.Overflow
            or else
              (Last_Num_Init < Max - 1
               and then Str (Last_Num_Init + 1) in '#' | ':'
               and then Init_Val.Value not in 2 .. 16)
            or else (Starts_As_Based and then Based_Val.Overflow))
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
        (if Starts_As_Exponent_Format_Ghost (Str (First_Exp .. Max))
         then Expon = Scan_Exponent_Ghost (Str (First_Exp .. Max)));

      if Expon /= 0 and then Uval /= 0 then

         --  For non-zero value, scale by exponent value. No need to do this
         --  efficiently, since use of exponent in integer literals is rare,
         --  and in any case the exponent cannot be very large.

         declare
            UmaxB : constant Uns := Uns'Last / Base;
            --  Numbers bigger than UmaxB overflow if multiplied by base

            Res_Val : constant Uns_Option :=
              Exponent_Unsigned_Ghost (Uval, Expon, Base)
            with Ghost;
         begin
            for J in 1 .. Expon loop
               pragma Loop_Invariant
                 (if Overflow'Loop_Entry then Overflow);
               pragma Loop_Invariant
                 (if Overflow
                  then Overflow'Loop_Entry or else Res_Val.Overflow);
               pragma Loop_Invariant
                 (if not Overflow
                  then Res_Val = Exponent_Unsigned_Ghost
                    (Uval, Expon - J + 1, Base));

               pragma Assert
                 ((Uval > UmaxB) = Scan_Overflows_Ghost (0, Base, Uval));

               if Uval > UmaxB then
                  Overflow := True;
                  exit;
               end if;

               Uval := Uval * Base;
            end loop;
            pragma Assert
              (Overflow = (Init_Val.Overflow
               or else
                 (Last_Num_Init < Max - 1
                  and then Str (Last_Num_Init + 1) in '#' | ':'
                  and then Init_Val.Value not in 2 .. 16)
               or else (Starts_As_Based and then Based_Val.Overflow)
               or else Res_Val.Overflow));
            pragma Assert
              (Overflow = Raw_Unsigned_Overflows_Ghost (Str, Ptr_Old, Max));
            pragma Assert
              (Exponent_Unsigned_Ghost (Uval, 0, Base) = (False, Uval));
            pragma Assert
              (if not Overflow then Uval = Res_Val.Value);
            pragma Assert
              (if not Overflow then
                  Uval = Scan_Raw_Unsigned_Ghost (Str, Ptr_Old, Max));
         end;
      end if;
      pragma Assert
        (if Expon = 0 or else Uval = 0 then
            Exponent_Unsigned_Ghost (Uval, Expon, Base) = (False, Uval));
      pragma Assert
        (Overflow = Raw_Unsigned_Overflows_Ghost (Str, Ptr_Old, Max));
      pragma Assert
        (if not Overflow then
            Uval = Scan_Raw_Unsigned_Ghost (Str, Ptr_Old, Max));

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
         begin
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
            pragma Assert
              (Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last)));

            declare
               P_Acc : constant not null access Integer := P'Access;
            begin
               Scan_Unsigned (Str, P_Acc, Str'Last, V);
            end;

            pragma Assert
              (P = Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last));
            pragma Assert
              (V = Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last));

            Scan_Trailing_Blanks (Str, P);

            pragma Assert
              (Is_Value_Unsigned_Ghost (Slide_If_Necessary (Str), V));
            return V;
         end;
      end if;
   end Value_Unsigned;

end System.Value_U;
