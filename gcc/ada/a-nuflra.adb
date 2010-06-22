------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            A D A . N U M E R I C S . F L O A T _ R A N D O M             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

with Interfaces; use Interfaces;

with System.Random_Numbers; use System.Random_Numbers;

package body Ada.Numerics.Float_Random is

   -------------------------
   -- Implementation Note --
   -------------------------

   --  The design of this spec is a bit awkward, as a result of Ada 95 not
   --  permitting in-out parameters for function formals (most naturally
   --  Generator values would be passed this way). In pure Ada 95, the only
   --  solution would be to add a self-referential component to the generator
   --  allowing access to the generator object from inside the function. This
   --  would work because the generator is limited, which prevents any copy.

   --  This is a bit heavy, so what we do is to use Unrestricted_Access to
   --  get a pointer to the state in the passed Generator. This works because
   --  Generator is a limited type and will thus always be passed by reference.

   subtype Rep_Generator is System.Random_Numbers.Generator;
   subtype Rep_State is System.Random_Numbers.State;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Uniformly_Distributed is
   begin
      return Random (Gen.Rep);
   end Random;

   -----------
   -- Reset --
   -----------

   --  Version that works from given initiator value

   procedure Reset (Gen : Generator; Initiator : Integer) is
      G : Rep_Generator renames Gen.Rep'Unrestricted_Access.all;
   begin
      Reset (G, Integer_32 (Initiator));
   end Reset;

   --  Version that works from calendar

   procedure Reset (Gen : Generator) is
      G : Rep_Generator renames Gen.Rep'Unrestricted_Access.all;
   begin
      Reset (G);
   end Reset;

   --  Version that works from specific saved state

   procedure Reset (Gen : Generator; From_State : State) is
      G : Rep_Generator renames Gen.Rep'Unrestricted_Access.all;
   begin
      Reset (G, From_State);
   end Reset;

   ----------
   -- Save --
   ----------

   procedure Save  (Gen : Generator; To_State : out State) is
   begin
      Save (Gen.Rep, State (To_State));
   end Save;

   -----------
   -- Image --
   -----------

   function Image (Of_State : State) return String is
   begin
      return Image (Rep_State (Of_State));
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return State is
      G : Generator;
      S : Rep_State;
   begin
      Reset (G.Rep, Coded_State);
      System.Random_Numbers.Save (G.Rep, S);
      return State (S);
   end Value;

end Ada.Numerics.Float_Random;
