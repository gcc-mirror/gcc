------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . N U M E R I C S . D I S C R E T E _ R A N D O M          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Interfaces; use Interfaces;

package body Ada.Numerics.Discrete_Random is

   -------------------------
   -- Implementation Note --
   -------------------------

   --  The design of this spec is very awkward, as a result of Ada 95 not
   --  permitting in-out parameters for function formals (most naturally
   --  Generator values would be passed this way). In pure Ada 95, the only
   --  solution is to use the heap and pointers, and, to avoid memory leaks,
   --  controlled types.

   --  This is awfully heavy, so what we do is to use Unrestricted_Access to
   --  get a pointer to the state in the passed Generator. This works because
   --  Generator is a limited type and will thus always be passed by reference.

   type Pointer is access all State;

   Need_64 : constant Boolean := Rst'Pos (Rst'Last) > Int'Last;

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
             ','                            &
             Int'Image (Of_State.X2) &
             ','                            &
             Int'Image (Of_State.Q);
   end Image;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Rst is
      Genp : constant Pointer := Gen.Gen_State'Unrestricted_Access;
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

      Genp.X1 := Square_Mod_N (Genp.X1, Genp.P);
      Genp.X2 := Square_Mod_N (Genp.X2, Genp.Q);
      Temp := Genp.X2 - Genp.X1;

      --  Following duplication is not an error, it is a loop unwinding!

      if Temp < 0 then
         Temp := Temp + Genp.Q;
      end if;

      if Temp < 0 then
         Temp := Temp + Genp.Q;
      end if;

      TF :=  Offs + (Flt (Temp) * Flt (Genp.P) + Flt (Genp.X1)) * Genp.Scl;

      --  Pathological, but there do exist cases where the rounding implicit
      --  in calculating the scale factor will cause rounding to 'Last + 1.
      --  In those cases, returning 'First results in the least bias.

      if TF >= Flt (Rst'Pos (Rst'Last)) + 0.5 then
         return Rst'First;

      elsif Need_64 then
         return Rst'Val (Interfaces.Integer_64 (TF));

      else
         return Rst'Val (Int (TF));
      end if;

   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : Generator; Initiator : Integer) is
      Genp   : constant Pointer := Gen.Gen_State'Unrestricted_Access;
      X1, X2 : Int;

   begin
      X1 := 2 + Int (Initiator) mod (K1 - 3);
      X2 := 2 + Int (Initiator) mod (K2 - 3);

      for J in 1 .. 5 loop
         X1 := Square_Mod_N (X1, K1);
         X2 := Square_Mod_N (X2, K2);
      end loop;

      --  eliminate effects of small Initiators.

      Genp.all :=
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
      Genp : constant Pointer       := Gen.Gen_State'Unrestricted_Access;
      Now  : constant Calendar.Time := Calendar.Clock;
      X1   : Int;
      X2   : Int;

   begin
      X1 := Int (Calendar.Year    (Now)) * 12 * 31 +
            Int (Calendar.Month   (Now) * 31)     +
            Int (Calendar.Day     (Now));

      X2 := Int (Calendar.Seconds (Now) * Duration (1000.0));

      X1 := 2 + X1 mod (K1 - 3);
      X2 := 2 + X2 mod (K2 - 3);

      --  Eliminate visible effects of same day starts

      for J in 1 .. 5 loop
         X1 := Square_Mod_N (X1, K1);
         X2 := Square_Mod_N (X2, K2);
      end loop;

      Genp.all :=
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
      Genp : constant Pointer := Gen.Gen_State'Unrestricted_Access;

   begin
      Genp.all := From_State;
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
      Start : Positive := Coded_State'First;
      Stop  : Positive := Coded_State'First;
      Outs  : State;

   begin
      while Coded_State (Stop) /= ',' loop
         Stop := Stop + 1;
      end loop;

      Outs.X1 := Int'Value (Coded_State (Start .. Stop - 1));
      Start := Stop + 1;

      loop
         Stop := Stop + 1;
         exit when Coded_State (Stop) = ',';
      end loop;

      Outs.X2  := Int'Value (Coded_State (Start .. Stop - 1));
      Outs.Q   := Int'Value (Coded_State (Stop + 1 .. Coded_State'Last));
      Outs.P   := Outs.Q * 2 + 1;
      Outs.FP  := Flt (Outs.P);
      Outs.Scl := (RstL - RstF + 1.0) / (Flt (Outs.P) * Flt (Outs.Q));

      --  Now do *some* sanity checks.

      if Outs.Q < 31
        or else Outs.X1 not in 2 .. Outs.P - 1
        or else Outs.X2 not in 2 .. Outs.Q - 1
      then
         raise Constraint_Error;
      end if;

      return Outs;
   end Value;

end Ada.Numerics.Discrete_Random;
