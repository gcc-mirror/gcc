------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ C O U N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2020, Free Software Foundation, Inc.         --
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

--  This implementation of the package for x86 processor. GCC can't generate
--  code for atomic builtins for 386 CPU. Only increment/decrement instructions
--  are supported, thus this implementaton uses machine code insertions to
--  access the necessary instructions.

with System.Machine_Code;

package body System.Atomic_Counters is

   --  Add comments showing in normal asm language what we generate???

   ---------------
   -- Decrement --
   ---------------

   function Decrement (Item : aliased in out Atomic_Unsigned) return Boolean is
      Aux : Boolean;

   begin
      System.Machine_Code.Asm
        (Template =>
           "lock%; decl" & ASCII.HT & "%0" & ASCII.LF & ASCII.HT
             & "sete %1",
         Outputs  =>
           (Atomic_Unsigned'Asm_Output ("=m", Item),
            Boolean'Asm_Output ("=qm", Aux)),
         Inputs   => Atomic_Unsigned'Asm_Input ("m", Item),
         Volatile => True);

      return Aux;
   end Decrement;

   procedure Decrement (Item : aliased in out Atomic_Unsigned) is
   begin
      if Decrement (Item) then
         null;
      end if;
   end Decrement;

   function Decrement (Item : in out Atomic_Counter) return Boolean is
   begin
      return Decrement (Item.Value);
   end Decrement;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Item : aliased in out Atomic_Unsigned) is
   begin
      System.Machine_Code.Asm
        (Template => "lock%; incl" & ASCII.HT & "%0",
         Outputs  => Atomic_Unsigned'Asm_Output ("=m", Item),
         Inputs   => Atomic_Unsigned'Asm_Input ("m", Item),
         Volatile => True);
   end Increment;

   procedure Increment (Item : in out Atomic_Counter) is
   begin
      Increment (Item.Value);
   end Increment;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : out Atomic_Counter) is
   begin
      Item.Value := 1;
   end Initialize;

   ------------
   -- Is_One --
   ------------

   function Is_One (Item : Atomic_Counter) return Boolean is
   begin
      return Item.Value = 1;
   end Is_One;

end System.Atomic_Counters;
