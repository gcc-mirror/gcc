------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            G N A T . M B B S _ D I S C R E T E _ R A N D O M             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with Ada.Calendar;

with Interfaces; use Interfaces;

package body GNAT.MBBS_Discrete_Random is

   package Calendar renames Ada.Calendar;

   Fits_In_32_Bits : constant Boolean :=
                       Rst'Size < 31
                         or else (Rst'Size = 31
                                  and then Rst'Pos (Rst'First) < 0);
   --  This is set True if we do not need more than 32 bits in the result. If
   --  we need 64-bits, we will only use the meaningful 48 bits of any 64-bit
   --  number generated, since if more than 48 bits are required, we split the
   --  computation into two separate parts, since the algorithm does not behave
   --  above 48 bits.

   --  The way this expression works is that obviously if the size is 31 bits,
   --  it fits in 32 bits. In the 32-bit case, it fits in 32-bit signed if the
   --  range has negative values. It is too conservative in the case that the
   --  programmer has set a size greater than the default, e.g. a size of 33
   --  for an integer type with a range of 1..10, but an over-conservative
   --  result is OK. The important thing is that the value is only True if
   --  we know the result will fit in 32-bits signed. If the value is False
   --  when it could be True, the behavior will be correct, just a bit less
   --  efficient than it could have been in some unusual cases.
   --
   --  One might assume that we could get a more accurate result by testing
   --  the lower and upper bounds of the type Rst against the bounds of 32-bit
   --  Integer. However, there is no easy way to do that. Why? Because in the
   --  relatively rare case where this expression has to be evaluated at run
   --  time rather than compile time (when the bounds are dynamic), we need a
   --  type to use for the computation. But the possible range of upper bound
   --  values for Rst (remembering the possibility of 64-bit modular types) is
   --  from -2**63 to 2**64-1, and no run-time type has a big enough range.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Square_Mod_N (X, N : Int) return Int;
   pragma Inline (Square_Mod_N);
   --  Computes X**2 mod N avoiding intermediate overflow

   -----------
   -- Image --
   -----------

   function Image (Of_State : State) return String is
   begin
      return Int'Image (Of_State.X1) &
             ','                     &
             Int'Image (Of_State.X2) &
             ','                     &
             Int'Image (Of_State.Q);
   end Image;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Rst is
      S    : State renames Gen.Writable.Self.Gen_State;
      Temp : Int;
      TF   : Flt;

   begin
      --  Check for flat range here, since we are typically run with checks
      --  off, note that in practice, this condition will usually be static
      --  so we will not actually generate any code for the normal case.

      if Rst'Last < Rst'First then
         raise Constraint_Error;
      end if;

      --  Continue with computation if non-flat range

      S.X1 := Square_Mod_N (S.X1, S.P);
      S.X2 := Square_Mod_N (S.X2, S.Q);
      Temp := S.X2 - S.X1;

      --  Following duplication is not an error, it is a loop unwinding

      if Temp < 0 then
         Temp := Temp + S.Q;
      end if;

      if Temp < 0 then
         Temp := Temp + S.Q;
      end if;

      TF := Offs + (Flt (Temp) * Flt (S.P) + Flt (S.X1)) * S.Scl;

      --  Pathological, but there do exist cases where the rounding implicit
      --  in calculating the scale factor will cause rounding to 'Last + 1.
      --  In those cases, returning 'First results in the least bias.

      if TF >= Flt (Rst'Pos (Rst'Last)) + 0.5 then
         return Rst'First;

      elsif not Fits_In_32_Bits then
         return Rst'Val (Interfaces.Integer_64 (TF));

      else
         return Rst'Val (Int (TF));
      end if;
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : Generator; Initiator : Integer) is
      S      : State renames Gen.Writable.Self.Gen_State;
      X1, X2 : Int;

   begin
      X1 := 2 + Int (Initiator) mod (K1 - 3);
      X2 := 2 + Int (Initiator) mod (K2 - 3);

      for J in 1 .. 5 loop
         X1 := Square_Mod_N (X1, K1);
         X2 := Square_Mod_N (X2, K2);
      end loop;

      --  Eliminate effects of small Initiators

      S :=
        (X1  => X1,
         X2  => X2,
         P   => K1,
         Q   => K2,
         FP  => K1F,
         Scl => Scal);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : Generator) is
      S    : State renames Gen.Writable.Self.Gen_State;
      Now  : constant Calendar.Time := Calendar.Clock;
      X1   : Int;
      X2   : Int;

   begin
      X1 := Int (Calendar.Year    (Now)) * 12 * 31 +
            Int (Calendar.Month   (Now) * 31)      +
            Int (Calendar.Day     (Now));

      X2 := Int (Calendar.Seconds (Now) * Duration (1000.0));

      X1 := 2 + X1 mod (K1 - 3);
      X2 := 2 + X2 mod (K2 - 3);

      --  Eliminate visible effects of same day starts

      for J in 1 .. 5 loop
         X1 := Square_Mod_N (X1, K1);
         X2 := Square_Mod_N (X2, K2);
      end loop;

      S :=
        (X1  => X1,
         X2  => X2,
         P   => K1,
         Q   => K2,
         FP  => K1F,
         Scl => Scal);

   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : Generator; From_State : State) is
   begin
      Gen.Writable.Self.Gen_State := From_State;
   end Reset;

   ----------
   -- Save --
   ----------

   procedure Save (Gen : Generator; To_State : out State) is
   begin
      To_State := Gen.Gen_State;
   end Save;

   ------------------
   -- Square_Mod_N --
   ------------------

   function Square_Mod_N (X, N : Int) return Int is
   begin
      return Int ((Integer_64 (X) ** 2) mod (Integer_64 (N)));
   end Square_Mod_N;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return State is
      Last  : constant Natural := Coded_State'Last;
      Start : Positive := Coded_State'First;
      Stop  : Positive := Coded_State'First;
      Outs  : State;

   begin
      while Stop <= Last and then Coded_State (Stop) /= ',' loop
         Stop := Stop + 1;
      end loop;

      if Stop > Last then
         raise Constraint_Error;
      end if;

      Outs.X1 := Int'Value (Coded_State (Start .. Stop - 1));
      Start := Stop + 1;

      loop
         Stop := Stop + 1;
         exit when Stop > Last or else Coded_State (Stop) = ',';
      end loop;

      if Stop > Last then
         raise Constraint_Error;
      end if;

      Outs.X2  := Int'Value (Coded_State (Start .. Stop - 1));
      Outs.Q   := Int'Value (Coded_State (Stop + 1 .. Last));
      Outs.P   := Outs.Q * 2 + 1;
      Outs.FP  := Flt (Outs.P);
      Outs.Scl := (RstL - RstF + 1.0) / (Flt (Outs.P) * Flt (Outs.Q));

      --  Now do *some* sanity checks

      if Outs.Q < 31
        or else Outs.X1 not in 2 .. Outs.P - 1
        or else Outs.X2 not in 2 .. Outs.Q - 1
      then
         raise Constraint_Error;
      end if;

      return Outs;
   end Value;

end GNAT.MBBS_Discrete_Random;
