------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               G N A T . M B B S _ F L O A T _ R A N D O M                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  The implementation used in this package was contributed by
--  Robert Eachus. It is based on the work of L. Blum, M. Blum, and
--  M. Shub, SIAM Journal of Computing, Vol 15. No 2, May 1986. The
--  particular choices for P and Q chosen here guarantee a period of
--  562,085,314,430,582 (about 2**49), and the generated sequence has
--  excellent randomness properties. For further details, see the
--  paper "Fast Generation of Trustworthy Random Numbers", by Robert
--  Eachus, which describes both the algorithm and the efficient
--  implementation approach used here.

--  Formerly, this package was Ada.Numerics.Float_Random. It is retained
--  here in part to allow users to reconstruct number sequences generated
--  by previous versions.

with Interfaces;

package GNAT.MBBS_Float_Random is

   --  Basic facilities

   type Generator is limited private;

   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;

   function Random (Gen : Generator) return Uniformly_Distributed;

   procedure Reset (Gen : Generator);
   procedure Reset (Gen : Generator; Initiator : Integer);

   --  Advanced facilities

   type State is private;

   procedure Save  (Gen : Generator; To_State   : out State);
   procedure Reset (Gen : Generator; From_State : State);

   Max_Image_Width : constant := 80;

   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

private
   type Int is new Interfaces.Integer_32;

   --  We prefer to use 14 digits for Flt, but some targets are more limited

   type Flt is digits Positive'Min (14, Long_Long_Float'Digits);

   K1   : constant := 94_833_359;
   K1F  : constant := 94_833_359.0;
   K2   : constant := 47_416_679;
   K2F  : constant := 47_416_679.0;
   Scal : constant := 1.0 / (K1F * K2F);

   type State is record
      X1  : Int := 2999 ** 2;      --  Square mod p
      X2  : Int := 1439 ** 2;      --  Square mod q
      P   : Int := K1;
      Q   : Int := K2;
      X   : Int := 1;
      Scl : Flt := Scal;
   end record;

   type Generator is limited record
      Gen_State : State;
   end record;

end GNAT.MBBS_Float_Random;
