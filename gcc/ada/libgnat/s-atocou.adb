------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ C O U N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2021, Free Software Foundation, Inc.         --
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

--  This is version of the package, for use on platforms where this capability
--  is not supported. All Atomic_Counter operations raises Program_Error,
--  Atomic_Unsigned operations processed in non-atomic manner.

package body System.Atomic_Counters is

   ---------------
   -- Decrement --
   ---------------

   function Decrement (Item : in out Atomic_Counter) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Decrement;

   function Decrement (Item : aliased in out Atomic_Unsigned) return Boolean is
   begin
      --  Could not use Item := Item - 1; because it is disabled in spec.
      Item := Atomic_Unsigned'Pred (Item);
      return Item = 0;
   end Decrement;

   procedure Decrement (Item : aliased in out Atomic_Unsigned) is
   begin
      Item := Atomic_Unsigned'Pred (Item);
   end Decrement;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Item : in out Atomic_Counter) is
   begin
      raise Program_Error;
   end Increment;

   procedure Increment (Item : aliased in out Atomic_Unsigned) is
   begin
      Item := Atomic_Unsigned'Succ (Item);
   end Increment;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : out Atomic_Counter) is
   begin
      raise Program_Error;
   end Initialize;

   ------------
   -- Is_One --
   ------------

   function Is_One (Item : Atomic_Counter) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Is_One;

end System.Atomic_Counters;
