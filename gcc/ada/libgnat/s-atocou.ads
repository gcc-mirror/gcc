------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ C O U N T E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2024, Free Software Foundation, Inc.         --
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

--  This package provides atomic counters routines

--  This package support the following platforms:
--    - all Alpha platforms
--    - all AARCH64 platforms
--    - all ARM platforms
--    - all ia64 platforms
--    - all PowerPC platforms
--    - all SPARC V9 platforms
--    - all x86 platforms
--    - all x86_64 platforms

package System.Atomic_Counters is

   pragma Pure;
   pragma Preelaborate;

   type Atomic_Counter is limited private;
   --  Type for atomic counter objects. Note, initial value of the counter is
   --  one. This allows using an atomic counter as member of record types when
   --  object of these types are created at library level in preelaborable
   --  compilation units.
   --
   --  Atomic_Counter is declared as private limited type to provide highest
   --  level of protection from unexpected use. All available operations are
   --  declared below, and this set should be as small as possible.
   --  Increment/Decrement operations for this type raise Program_Error on
   --  platforms not supporting the atomic primitives.

   procedure Increment (Item : in out Atomic_Counter);
   pragma Inline_Always (Increment);
   --  Increments value of atomic counter.

   function Decrement (Item : in out Atomic_Counter) return Boolean;
   pragma Inline_Always (Decrement);
   --  Decrements value of atomic counter, returns True when value reach zero

   function Is_One (Item : Atomic_Counter) return Boolean;
   pragma Inline_Always (Is_One);
   --  Returns True when value of the atomic counter is one

   procedure Initialize (Item : out Atomic_Counter);
   pragma Inline_Always (Initialize);
   --  Initialize counter by setting its value to one. This subprogram is
   --  intended to be used in special cases when the counter object cannot be
   --  initialized in standard way.

   type Atomic_Unsigned is mod 2 ** 32 with Default_Value => 0, Atomic;
   --  Modular compatible atomic unsigned type.
   --  Increment/Decrement operations for this type are atomic only on
   --  supported platforms. See top of the file.

   procedure Increment
     (Item : aliased in out Atomic_Unsigned) with Inline_Always;
   --  Increments value of atomic counter

   function Decrement
     (Item : aliased in out Atomic_Unsigned) return Boolean with Inline_Always;

   procedure Decrement
     (Item : aliased in out Atomic_Unsigned) with Inline_Always;
   --  Decrements value of atomic counter

   --  The "+" and "-" abstract routine provided below to disable BT := BT + 1
   --  constructions.

   function "+"
     (Left, Right : Atomic_Unsigned) return Atomic_Unsigned is abstract;

   function "-"
     (Left, Right : Atomic_Unsigned) return Atomic_Unsigned is abstract;

private

   type Atomic_Counter is record
      Value : aliased Atomic_Unsigned := 1;
   end record;

end System.Atomic_Counters;
