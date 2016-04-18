------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . N U M E R I C S . D I S C R E T E _ R A N D O M          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

package body Ada.Numerics.Discrete_Random with
  SPARK_Mode => Off
is

   package SRN renames System.Random_Numbers;
   use SRN;

   -----------
   -- Image --
   -----------

   function Image (Of_State : State) return String is
   begin
      return Image (SRN.State (Of_State));
   end Image;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Result_Subtype is
      function Random is
        new SRN.Random_Discrete (Result_Subtype, Result_Subtype'First);
   begin
      return Random (SRN.Generator (Gen));
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : Generator) is
   begin
      Reset (SRN.Generator (Gen));
   end Reset;

   procedure Reset (Gen : Generator; Initiator : Integer) is
   begin
      Reset (SRN.Generator (Gen), Initiator);
   end Reset;

   procedure Reset (Gen : Generator; From_State : State) is
   begin
      Reset (SRN.Generator (Gen), SRN.State (From_State));
   end Reset;

   ----------
   -- Save --
   ----------

   procedure Save (Gen : Generator; To_State   : out State) is
   begin
      Save (SRN.Generator (Gen), SRN.State (To_State));
   end Save;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return State is
   begin
      return State (SRN.State'(Value (Coded_State)));
   end Value;

end Ada.Numerics.Discrete_Random;
