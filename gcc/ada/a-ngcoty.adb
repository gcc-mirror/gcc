------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--   A D A . N U M E R I C S . G E N E R I C _ C O M P L E X _ T Y P E S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

with Ada.Numerics.Aux; use Ada.Numerics.Aux;

package body Ada.Numerics.Generic_Complex_Types is

   subtype R is Real'Base;

   Two_Pi  : constant R := R (2.0) * Pi;
   Half_Pi : constant R := Pi / R (2.0);

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Complex) return Complex is
      X : R;
      Y : R;

   begin
      X := Left.Re * Right.Re - Left.Im * Right.Im;
      Y := Left.Re * Right.Im + Left.Im * Right.Re;

      --  If either component overflows, try to scale (skip in fast math mode)

      if not Standard'Fast_Math then
         if abs (X) > R'Last then
            X := R'(4.0) * (R'(Left.Re / 2.0)  * R'(Right.Re / 2.0)
                            - R'(Left.Im / 2.0) * R'(Right.Im / 2.0));
         end if;

         if abs (Y) > R'Last then
            Y := R'(4.0) * (R'(Left.Re / 2.0)  * R'(Right.Im / 2.0)
                            - R'(Left.Im / 2.0) * R'(Right.Re / 2.0));
         end if;
      end if;

      return (X, Y);
   end "*";

   function "*" (Left, Right : Imaginary) return Real'Base is
   begin
      return -(R (Left) * R (Right));
   end "*";

   function "*" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Complex'(Left.Re * Right, Left.Im * Right);
   end "*";

   function "*" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Left * Right.Re, Left * Right.Im);
   end "*";

   function "*" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return Complex'(-(Left.Im * R (Right)), Left.Re * R (Right));
   end "*";

   function "*" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return Complex'(-(R (Left) * Right.Im), R (Left) * Right.Re);
   end "*";

   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Left * Imaginary (Right);
   end "*";

   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Left * R (Right));
   end "*";

   ----------
   -- "**" --
   ----------

   function "**" (Left : Complex; Right : Integer) return Complex is
      Result : Complex := (1.0, 0.0);
      Factor : Complex := Left;
      Exp    : Integer := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. For positive exponents we
      --  multiply the result by this factor, for negative exponents, we
      --  divide by this factor.

      if Exp >= 0 then

         --  For a positive exponent, if we get a constraint error during
         --  this loop, it is an overflow, and the constraint error will
         --  simply be passed on to the caller.

         while Exp /= 0 loop
            if Exp rem 2 /= 0 then
               Result := Result * Factor;
            end if;

            Factor := Factor * Factor;
            Exp := Exp / 2;
         end loop;

         return Result;

      else -- Exp < 0 then

         --  For the negative exponent case, a constraint error during this
         --  calculation happens if Factor gets too large, and the proper
         --  response is to return 0.0, since what we essentially have is
         --  1.0 / infinity, and the closest model number will be zero.

         begin
            while Exp /= 0 loop
               if Exp rem 2 /= 0 then
                  Result := Result * Factor;
               end if;

               Factor := Factor * Factor;
               Exp := Exp / 2;
            end loop;

            return R'(1.0) / Result;

         exception
            when Constraint_Error =>
               return (0.0, 0.0);
         end;
      end if;
   end "**";

   function "**" (Left : Imaginary; Right : Integer) return Complex is
      M : constant R := R (Left) ** Right;
   begin
      case Right mod 4 is
         when 0 => return (M,   0.0);
         when 1 => return (0.0, M);
         when 2 => return (-M,  0.0);
         when 3 => return (0.0, -M);
         when others => raise Program_Error;
      end case;
   end "**";

   ---------
   -- "+" --
   ---------

   function "+" (Right : Complex) return Complex is
   begin
      return Right;
   end "+";

   function "+" (Left, Right : Complex) return Complex is
   begin
      return Complex'(Left.Re + Right.Re, Left.Im + Right.Im);
   end "+";

   function "+" (Right : Imaginary) return Imaginary is
   begin
      return Right;
   end "+";

   function "+" (Left, Right : Imaginary) return Imaginary is
   begin
      return Imaginary (R (Left) + R (Right));
   end "+";

   function "+" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Complex'(Left.Re + Right, Left.Im);
   end "+";

   function "+" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return Complex'(Left + Right.Re, Right.Im);
   end "+";

   function "+" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return Complex'(Left.Re, Left.Im + R (Right));
   end "+";

   function "+" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return Complex'(Right.Re, R (Left) + Right.Im);
   end "+";

   function "+" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return Complex'(Right, R (Left));
   end "+";

   function "+" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return Complex'(Left, R (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Complex) return Complex is
   begin
      return (-Right.Re, -Right.Im);
   end "-";

   function "-" (Left, Right : Complex) return Complex is
   begin
      return (Left.Re - Right.Re, Left.Im - Right.Im);
   end "-";

   function "-" (Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-R (Right));
   end "-";

   function "-" (Left, Right : Imaginary) return Imaginary is
   begin
      return Imaginary (R (Left) - R (Right));
   end "-";

   function "-" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Complex'(Left.Re - Right, Left.Im);
   end "-";

   function "-" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return Complex'(Left - Right.Re, -Right.Im);
   end "-";

   function "-" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return Complex'(Left.Re, Left.Im - R (Right));
   end "-";

   function "-" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return Complex'(-Right.Re, R (Left) - Right.Im);
   end "-";

   function "-" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return Complex'(-Right, R (Left));
   end "-";

   function "-" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return Complex'(Left, -R (Right));
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Complex) return Complex is
      a : constant R := Left.Re;
      b : constant R := Left.Im;
      c : constant R := Right.Re;
      d : constant R := Right.Im;

   begin
      if c = 0.0 and then d = 0.0 then
         raise Constraint_Error;
      else
         return Complex'(Re => ((a * c) + (b * d)) / (c ** 2 + d ** 2),
                         Im => ((b * c) - (a * d)) / (c ** 2 + d ** 2));
      end if;
   end "/";

   function "/" (Left, Right : Imaginary) return Real'Base is
   begin
      return R (Left) / R (Right);
   end "/";

   function "/" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return Complex'(Left.Re / Right, Left.Im / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Complex) return Complex is
      a : constant R := Left;
      c : constant R := Right.Re;
      d : constant R := Right.Im;
   begin
      return Complex'(Re =>   (a * c) / (c ** 2 + d ** 2),
                      Im => -((a * d) / (c ** 2 + d ** 2)));
   end "/";

   function "/" (Left : Complex; Right : Imaginary) return Complex is
      a : constant R := Left.Re;
      b : constant R := Left.Im;
      d : constant R := R (Right);

   begin
      return (b / d,  -(a / d));
   end "/";

   function "/" (Left : Imaginary; Right : Complex) return Complex is
      b : constant R := R (Left);
      c : constant R := Right.Re;
      d : constant R := Right.Im;

   begin
      return (Re => b * d / (c ** 2 + d ** 2),
              Im => b * c / (c ** 2 + d ** 2));
   end "/";

   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (R (Left) / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-(Left / R (Right)));
   end "/";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Imaginary) return Boolean is
   begin
      return R (Left) < R (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Imaginary) return Boolean is
   begin
      return R (Left) <= R (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Imaginary) return Boolean is
   begin
      return R (Left) > R (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Imaginary) return Boolean is
   begin
      return R (Left) >= R (Right);
   end ">=";

   -----------
   -- "abs" --
   -----------

   function "abs" (Right : Imaginary) return Real'Base is
   begin
      return abs R (Right);
   end "abs";

   --------------
   -- Argument --
   --------------

   function Argument (X : Complex) return Real'Base is
      a   : constant R := X.Re;
      b   : constant R := X.Im;
      arg : R;

   begin
      if b = 0.0 then

         if a >= 0.0 then
            return 0.0;
         else
            return R'Copy_Sign (Pi, b);
         end if;

      elsif a = 0.0 then

         if b >= 0.0 then
            return Half_Pi;
         else
            return -Half_Pi;
         end if;

      else
         arg := R (Atan (Double (abs (b / a))));

         if a > 0.0 then
            if b > 0.0 then
               return arg;
            else                  --  b < 0.0
               return -arg;
            end if;

         else                     --  a < 0.0
            if b >= 0.0 then
               return Pi - arg;
            else                  --  b < 0.0
               return -(Pi - arg);
            end if;
         end if;
      end if;

   exception
      when Constraint_Error =>
         if b > 0.0 then
            return Half_Pi;
         else
            return -Half_Pi;
         end if;
   end Argument;

   function Argument (X : Complex; Cycle : Real'Base) return Real'Base is
   begin
      if Cycle > 0.0 then
         return Argument (X) * Cycle / Two_Pi;
      else
         raise Argument_Error;
      end if;
   end Argument;

   ----------------------------
   -- Compose_From_Cartesian --
   ----------------------------

   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex is
   begin
      return (Re, Im);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re : Real'Base) return Complex is
   begin
      return (Re, 0.0);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Im : Imaginary) return Complex is
   begin
      return (0.0, R (Im));
   end Compose_From_Cartesian;

   ------------------------
   -- Compose_From_Polar --
   ------------------------

   function Compose_From_Polar (
     Modulus, Argument : Real'Base)
     return Complex
   is
   begin
      if Modulus = 0.0 then
         return (0.0, 0.0);
      else
         return (Modulus * R (Cos (Double (Argument))),
                 Modulus * R (Sin (Double (Argument))));
      end if;
   end Compose_From_Polar;

   function Compose_From_Polar (
     Modulus, Argument, Cycle : Real'Base)
     return Complex
   is
      Arg : Real'Base;

   begin
      if Modulus = 0.0 then
         return (0.0, 0.0);

      elsif Cycle > 0.0 then
         if Argument = 0.0 then
            return (Modulus, 0.0);

         elsif Argument = Cycle / 4.0 then
            return (0.0, Modulus);

         elsif Argument = Cycle / 2.0 then
            return (-Modulus, 0.0);

         elsif Argument = 3.0 * Cycle / R (4.0) then
            return (0.0, -Modulus);
         else
            Arg := Two_Pi * Argument / Cycle;
            return (Modulus * R (Cos (Double (Arg))),
                    Modulus * R (Sin (Double (Arg))));
         end if;
      else
         raise Argument_Error;
      end if;
   end Compose_From_Polar;

   ---------------
   -- Conjugate --
   ---------------

   function Conjugate (X : Complex) return Complex is
   begin
      return Complex'(X.Re, -X.Im);
   end Conjugate;

   --------
   -- Im --
   --------

   function Im (X : Complex) return Real'Base is
   begin
      return X.Im;
   end Im;

   function Im (X : Imaginary) return Real'Base is
   begin
      return R (X);
   end Im;

   -------------
   -- Modulus --
   -------------

   function Modulus (X : Complex) return Real'Base is
      Re2, Im2 : R;

   begin

      begin
         Re2 := X.Re ** 2;

         --  To compute (a**2 + b**2) ** (0.5) when a**2 may be out of bounds,
         --  compute a * (1 + (b/a) **2) ** (0.5). On a machine where the
         --  squaring does not raise constraint_error but generates infinity,
         --  we can use an explicit comparison to determine whether to use
         --  the scaling expression.

         --  The scaling expression is computed in double format throughout
         --  in order to prevent inaccuracies on machines where not all
         --  immediate expressions are rounded, such as PowerPC.

         if Re2 > R'Last then
            raise Constraint_Error;
         end if;

      exception
         when Constraint_Error =>
            return R (Double (abs (X.Re))
              * Sqrt (1.0 + (Double (X.Im) / Double (X.Re)) ** 2));
      end;

      begin
         Im2 := X.Im ** 2;

         if Im2 > R'Last then
            raise Constraint_Error;
         end if;

      exception
         when Constraint_Error =>
            return R (Double (abs (X.Im))
              * Sqrt (1.0 + (Double (X.Re) / Double (X.Im)) ** 2));
      end;

      --  Now deal with cases of underflow. If only one of the squares
      --  underflows, return the modulus of the other component. If both
      --  squares underflow, use scaling as above.

      if Re2 = 0.0 then

         if X.Re = 0.0 then
            return abs (X.Im);

         elsif Im2 = 0.0 then

            if X.Im = 0.0 then
               return abs (X.Re);

            else
               if abs (X.Re) > abs (X.Im) then
                  return
                    R (Double (abs (X.Re))
                      * Sqrt (1.0 + (Double (X.Im) / Double (X.Re)) ** 2));
               else
                  return
                    R (Double (abs (X.Im))
                      * Sqrt (1.0 + (Double (X.Re) / Double (X.Im)) ** 2));
               end if;
            end if;

         else
            return abs (X.Im);
         end if;

      elsif Im2 = 0.0 then
         return abs (X.Re);

      --  In all other cases, the naive computation will do

      else
         return R (Sqrt (Double (Re2 + Im2)));
      end if;
   end Modulus;

   --------
   -- Re --
   --------

   function Re (X : Complex) return Real'Base is
   begin
      return X.Re;
   end Re;

   ------------
   -- Set_Im --
   ------------

   procedure Set_Im (X : in out Complex; Im : Real'Base) is
   begin
      X.Im := Im;
   end Set_Im;

   procedure Set_Im (X : out Imaginary; Im : Real'Base) is
   begin
      X := Imaginary (Im);
   end Set_Im;

   ------------
   -- Set_Re --
   ------------

   procedure Set_Re (X : in out Complex; Re : Real'Base) is
   begin
      X.Re := Re;
   end Set_Re;

end Ada.Numerics.Generic_Complex_Types;
