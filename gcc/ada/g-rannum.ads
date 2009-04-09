------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   G N A T . R A N D O M _ N U M B E R S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--      Copyright (C) 2007-2009  Free Software Foundation, Inc.             --
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

--  Extended pseudo-random number generation

--  This package provides a type representing pseudo-random number generators,
--  and subprograms to extract various distributions of numbers from them. It
--  also provides types for representing initialization values and snapshots of
--  internal generator state, which permit reproducible pseudo-random streams.

--  The generator currently provided by this package has an extremely long
--  period (at least 2**19937-1), and passes the Big Crush test suite, with the
--  exception of the two linear complexity tests. Therefore, it is suitable for
--  simulations, but should not be used as a cryptographic pseudo-random source
--  without additional processing.

--  The design of this package effects is simplified compared to the design
--  of standard Ada.Numerics packages. There is no separate State type; the
--  Generator type itself suffices for this purpose. The parameter modes on
--  Reset procedures better reflect the effect of these routines.

with System.Random_Numbers;
with Interfaces; use Interfaces;

package GNAT.Random_Numbers is

   type Generator is limited private;
   subtype Initialization_Vector is
     System.Random_Numbers.Initialization_Vector;

   function Random (Gen : Generator) return Float;
   function Random (Gen : Generator) return Long_Float;
   --  Return pseudo-random numbers uniformly distributed on [0 .. 1)

   function Random (Gen : Generator) return Interfaces.Integer_32;
   function Random (Gen : Generator) return Interfaces.Unsigned_32;
   function Random (Gen : Generator) return Interfaces.Integer_64;
   function Random (Gen : Generator) return Interfaces.Unsigned_64;
   function Random (Gen : Generator) return Integer;
   function Random (Gen : Generator) return Long_Integer;
   --  Return pseudo-random numbers uniformly distributed on T'First .. T'Last
   --  for various builtin integer types.

   generic
      type Result_Subtype is (<>);
      Default_Min : Result_Subtype := Result_Subtype'Val (0);
   function Random_Discrete
     (Gen   : Generator;
      Min   : Result_Subtype := Default_Min;
      Max   : Result_Subtype := Result_Subtype'Last) return Result_Subtype;
   --  Returns pseudo-random numbers uniformly distributed on Min .. Max

   generic
      type Result_Subtype is digits <>;
   function Random_Float (Gen   : Generator) return Result_Subtype;
   --  Returns pseudo-random numbers uniformly distributed on [0 .. 1)

   function Random_Gaussian (Gen : Generator) return Long_Float;
   function Random_Gaussian (Gen : Generator) return Float;
   --  Returns pseudo-random numbers normally distributed value with mean 0
   --  and standard deviation 1.0.

   procedure Reset (Gen : out Generator);
   --  Re-initialize the state of Gen from the time of day

   procedure Reset
     (Gen       : out Generator;
      Initiator : Initialization_Vector);
   procedure Reset
     (Gen       : out Generator;
      Initiator : Interfaces.Integer_32);
   procedure Reset
     (Gen       : out Generator;
      Initiator : Interfaces.Unsigned_32);
   procedure Reset
     (Gen       : out Generator;
      Initiator : Integer);
   --  Re-initialize Gen based on the Initiator in various ways. Identical
   --  values of Initiator cause identical sequences of values.

   procedure Reset (Gen : out Generator; From_State : Generator);
   --  Causes the state of Gen to be identical to that of From_State; Gen
   --  and From_State will produce identical sequences of values subsequently.

   procedure Reset (Gen : out Generator; From_Image : String);
   function Image (Gen : Generator) return String;
   --  The call
   --     Reset (Gen2, Image (Gen1))
   --  has the same effect as Reset (Gen2, Gen1);

   Max_Image_Width : constant :=
     System.Random_Numbers.Max_Image_Width + 2 + 20 + 5;
   --  Maximum possible length of result of Image (...)

private

   type Generator is limited record
      Rep : System.Random_Numbers.Generator;

      Have_Gaussian : Boolean;
      --  The algorithm used for Random_Gaussian produces deviates in
      --  pairs. Have_Gaussian is true iff Random_Gaussian has returned one
      --  member of the pair and Next_Gaussian contains the other.

      Next_Gaussian : Long_Float;
      --  Next random deviate to be produced by Random_Gaussian, if
      --  Have_Gaussian.
   end record;

end GNAT.Random_Numbers;
