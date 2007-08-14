------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . R A N D O M _ N U M B E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2007, Free Software Foundation, Inc.              --
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

--  Extended pseudo-random number generation

--  This package provides a type representing pseudo-random number generators,
--  and subprograms to extract various uniform distributions of numbers
--  from them. It also provides types for representing initialization values
--  and snapshots of internal generator state, which permit reproducible
--  pseudo-random streams.

--  The generator currently provided by this package has an extremely long
--  period (at least 2**19937-1), and passes the Big Crush test suite, with the
--  exception of the two linear complexity tests. Therefore, it is suitable
--  for simulations, but should not be used as a cryptographic pseudo-random
--  source without additional processing.

--  Note: this package is in the System hierarchy so that it can be directly
--  used by other predefined packages. User access to this package is via
--  the package GNAT.Random_Numbers (file g-rannum.ads), which also extends
--  its capabilities. The interfaces are different so as to include in
--  System.Random_Numbers only the definitions necessary to implement the
--  standard random-number packages Ada.Numerics.Float_Random and
--  Ada.Numerics.Discrete_Random.

with Interfaces;

package System.Random_Numbers is

   type Generator is limited private;
   type State is private;
   --  A non-limited version of a Generator's internal state

   function Random (Gen : Generator) return Float;
   function Random (Gen : Generator) return Long_Float;
   --  Return pseudo-random numbers uniformly distributed on [0 .. 1)

   function Random (Gen : Generator) return Interfaces.Unsigned_32;
   function Random (Gen : Generator) return Interfaces.Unsigned_64;
   --  Return pseudo-random numbers uniformly distributed on T'First .. T'Last
   --  for builtin integer types.

   generic
      type Result_Subtype is (<>);
      Default_Min : Result_Subtype := Result_Subtype'Val (0);
   function Random_Discrete
     (Gen : Generator;
      Min : Result_Subtype := Default_Min;
      Max : Result_Subtype := Result_Subtype'Last) return Result_Subtype;
   --  Returns pseudo-random numbers uniformly distributed on Min .. Max

   generic
      type Result_Subtype is digits <>;
   function Random_Float (Gen : Generator) return Result_Subtype;
   --  Returns pseudo-random numbers uniformly distributed on [0 .. 1)

   type Initialization_Vector is
     array (Integer range <>) of Interfaces.Unsigned_32;
   --  Provides the most general initialization values for a generator (used
   --  in Reset).  In general, there is little point in providing more than
   --  a certain number of values (currently 624).

   procedure Reset (Gen : out Generator);
   --  Re-initialize the state of Gen from the time of day

   procedure Reset (Gen : out Generator; Initiator : Initialization_Vector);
   procedure Reset (Gen : out Generator; Initiator : Interfaces.Integer_32);
   procedure Reset (Gen : out Generator; Initiator : Interfaces.Unsigned_32);
   procedure Reset (Gen : out Generator; Initiator : Integer);
   --  Re-initialize Gen based on the Initiator in various ways. Identical
   --  values of Initiator cause identical sequences of values.

   procedure Reset (Gen : out Generator; From_State : Generator);
   --  Causes the state of Gen to be identical to that of From_State; Gen
   --  and From_State will produce identical sequences of values subsequently.

   procedure Reset (Gen : out Generator; From_State : State);
   procedure Save  (Gen : Generator; To_State : out State);
   --  The sequence
   --     Save (Gen2, S); Reset (Gen1, S)
   --  has the same effect as Reset (Gen2, Gen1).

   procedure Reset (Gen : out Generator; From_Image : String);
   function Image (Gen : Generator) return String;
   --  The call
   --     Reset (Gen2, Image (Gen1))
   --  has the same effect as Reset (Gen2, Gen1);

   Max_Image_Width : constant := 11 * 624;
   --  Maximum possible length of result of Image (...)

   function Image (Of_State : State) return String;
   --  A String representation of Of_State. Identical to the result of
   --  Image (Gen), if Of_State has been set with Save (Gen, Of_State).

   function Value (Coded_State : String) return State;
   --  Inverse of Image on States

private

   N : constant := 624;
   --  The number of 32-bit integers in the shift register

   M : constant := 397;
   --  Feedback distance from the current position

   subtype State_Val is Interfaces.Unsigned_32;
   type State is array (0 .. N - 1) of State_Val;

   type Generator is limited record
      S : State := (others => 0);
      --  The shift register, a circular buffer

      I : Integer := N;
      --  Current starting position in shift register S
   end record;

end System.Random_Numbers;
