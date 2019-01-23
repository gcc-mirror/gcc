------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2012-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the Post aspects that have been added to the spec.       --
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

generic
   type Float_Type is digits <>;

package Ada.Numerics.Generic_Elementary_Functions with
  SPARK_Mode => On
is
   pragma Pure;

   --  Preconditions in this unit are meant for analysis only, not for run-time
   --  checking, so that the expected exceptions are raised when calling
   --  Assert. This is enforced by setting the corresponding assertion policy
   --  to Ignore. This is done in the generic spec so that it applies to all
   --  instances.

   pragma Assertion_Policy (Pre => Ignore);

   function Sqrt (X : Float_Type'Base) return Float_Type'Base with
     Pre  => X >= 0.0,
     Post => Sqrt'Result >= 0.0
       and then (if X = 0.0 then Sqrt'Result = 0.0)
       and then (if X = 1.0 then Sqrt'Result = 1.0)

       --  Finally if X is positive, the result of Sqrt is positive (because
       --  the sqrt of numbers greater than 1 is greater than or equal to 1,
       --  and the sqrt of numbers less than 1 is greater than the argument).

       --  This property is useful in particular for static analysis. The
       --  property that X is positive is not expressed as (X > 0.0), as
       --  the value X may be held in registers that have larger range and
       --  precision on some architecture (for example, on x86 using x387
       --  FPU, as opposed to SSE2). So, it might be possible for X to be
       --  2.0**(-5000) or so, which could cause the number to compare as
       --  greater than 0, but Sqrt would still return a zero result.

       --  Note: we use the comparison with Succ (0.0) here because this is
       --  more amenable to CodePeer analysis than the use of 'Machine.

       and then (if X >= Float_Type'Succ (0.0) then Sqrt'Result > 0.0);

   function Log (X : Float_Type'Base) return Float_Type'Base with
     Pre  => X > 0.0,
     Post => (if X = 1.0 then Log'Result = 0.0);

   function Log (X, Base : Float_Type'Base) return Float_Type'Base with
     Pre  => X > 0.0 and Base > 0.0 and Base /= 1.0,
     Post => (if X = 1.0 then Log'Result = 0.0);

   function Exp (X : Float_Type'Base) return Float_Type'Base with
     Post => (if X = 0.0 then Exp'Result = 1.0);

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base with
     Pre  => (if Left = 0.0 then Right > 0.0) and Left >= 0.0,
     Post => "**"'Result >= 0.0
       and then (if Right = 0.0 then "**"'Result = 1.0)
       and then (if Right = 1.0 then "**"'Result = Left)
       and then (if Left  = 1.0 then "**"'Result = 1.0)
       and then (if Left  = 0.0 then "**"'Result = 0.0);

   function Sin (X : Float_Type'Base) return Float_Type'Base with
     Post => Sin'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Sin'Result = 0.0);

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base with
     Pre  => Cycle > 0.0,
     Post => Sin'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Sin'Result = 0.0);

   function Cos (X : Float_Type'Base) return Float_Type'Base with
     Post => Cos'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Cos'Result = 1.0);

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base with
     Pre  => Cycle > 0.0,
     Post => Cos'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Cos'Result = 1.0);

   function Tan (X : Float_Type'Base) return Float_Type'Base with
     Post => (if X = 0.0 then Tan'Result = 0.0);

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base with
     Pre  => Cycle > 0.0
       and then abs Float_Type'Base'Remainder (X, Cycle) /= 0.25 * Cycle,
     Post => (if X = 0.0 then Tan'Result = 0.0);

   function Cot (X : Float_Type'Base) return Float_Type'Base with
     Pre => X /= 0.0;

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base with
     Pre => Cycle > 0.0
       and then X /= 0.0
       and then Float_Type'Base'Remainder (X, Cycle) /= 0.0
       and then abs Float_Type'Base'Remainder (X, Cycle) = 0.5 * Cycle;

   function Arcsin (X : Float_Type'Base) return Float_Type'Base with
     Pre  => abs X <= 1.0,
     Post => (if X = 0.0 then Arcsin'Result = 0.0);

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base with
     Pre  => Cycle > 0.0 and abs X <= 1.0,
     Post => (if X = 0.0 then Arcsin'Result = 0.0);

   function Arccos (X : Float_Type'Base) return Float_Type'Base with
     Pre  => abs X <= 1.0,
     Post => (if X = 1.0 then Arccos'Result = 0.0);

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base with
     Pre  => Cycle > 0.0 and abs X <= 1.0,
     Post => (if X = 1.0 then Arccos'Result = 0.0);

   function Arctan
     (Y : Float_Type'Base;
      X : Float_Type'Base := 1.0) return Float_Type'Base
   with
     Pre  => X /= 0.0 or Y /= 0.0,
     Post => (if X > 0.0 and then Y = 0.0 then Arctan'Result = 0.0);

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base
   with
     Pre  => Cycle > 0.0 and (X /= 0.0 or Y /= 0.0),
     Post => (if X > 0.0 and then Y = 0.0 then Arctan'Result = 0.0);

   function Arccot
     (X   : Float_Type'Base;
      Y   : Float_Type'Base := 1.0) return Float_Type'Base
   with
     Pre  => X /= 0.0 or Y /= 0.0,
     Post => (if X > 0.0 and then Y = 0.0 then Arccot'Result = 0.0);

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base
   with
     Pre  => Cycle > 0.0 and (X /= 0.0 or Y /= 0.0),
     Post => (if X > 0.0 and then Y = 0.0 then Arccot'Result = 0.0);

   function Sinh (X : Float_Type'Base) return Float_Type'Base with
     Post => (if X = 0.0 then Sinh'Result = 0.0);

   function Cosh (X : Float_Type'Base) return Float_Type'Base with
     Post => Cosh'Result >= 1.0
       and then (if X = 0.0 then Cosh'Result = 1.0);

   function Tanh (X : Float_Type'Base) return Float_Type'Base with
     Post => Tanh'Result in -1.0 .. 1.0
       and then (if X = 0.0 then Tanh'Result = 0.0);

   function Coth (X : Float_Type'Base) return Float_Type'Base with
     Pre  => X /= 0.0,
     Post => abs Coth'Result >= 1.0;

   function Arcsinh (X : Float_Type'Base) return Float_Type'Base with
     Post => (if X = 0.0 then Arcsinh'Result = 0.0);

   function Arccosh (X : Float_Type'Base) return Float_Type'Base with
     Pre  => X >= 1.0,
     Post => Arccosh'Result >= 0.0
       and then (if X = 1.0 then Arccosh'Result = 0.0);

   function Arctanh (X : Float_Type'Base) return Float_Type'Base with
     Pre  => abs X < 1.0,
     Post => (if X = 0.0 then Arctanh'Result = 0.0);

   function Arccoth (X : Float_Type'Base) return Float_Type'Base with
     Pre => abs X > 1.0;

end Ada.Numerics.Generic_Elementary_Functions;
