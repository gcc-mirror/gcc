------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . N U M E R I C S . D I S C R E T E _ R A N D O M          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  Note: the implementation used in this package is a version of the
--  Mersenne Twister. See s-rannum.adb for details and references.

with System.Random_Numbers;

generic
   type Result_Subtype is (<>);

package Ada.Numerics.Discrete_Random with
  SPARK_Mode => Off
is

   --  Basic facilities

   type Generator is limited private;

   function Random (Gen : Generator) return Result_Subtype;

   function Random
     (Gen   : Generator;
      First : Result_Subtype;
      Last  : Result_Subtype) return Result_Subtype
     with Post => Random'Result in First .. Last;

   procedure Reset (Gen : Generator; Initiator : Integer);
   procedure Reset (Gen : Generator);

   --  Advanced facilities

   type State is private;

   procedure Save  (Gen : Generator; To_State   : out State);
   procedure Reset (Gen : Generator; From_State : State);

   Max_Image_Width : constant := System.Random_Numbers.Max_Image_Width;

   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

private

   type Generator is new System.Random_Numbers.Generator;

   type State is new System.Random_Numbers.State;

end Ada.Numerics.Discrete_Random;
