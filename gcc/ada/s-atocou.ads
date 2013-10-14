------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ C O U N T E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2013, Free Software Foundation, Inc.         --
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

--  This package provides atomic counter on platforms where it is supported:
--    - all Alpha platforms
--    - all ia64 platforms
--    - all PowerPC platforms
--    - all SPARC V9 platforms
--    - all x86 platforms
--    - all x86_64 platforms

--  Why isn't this package available to application programs???

package System.Atomic_Counters is

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

   procedure Increment (Item : in out Atomic_Counter);
   pragma Inline_Always (Increment);
   --  Increments value of atomic counter.

   function Decrement (Item : in out Atomic_Counter) return Boolean;
   pragma Inline_Always (Decrement);
   --  Decrements value of atomic counter, returns True when value reach zero.

   function Is_One (Item : Atomic_Counter) return Boolean;
   pragma Inline_Always (Is_One);
   --  Returns True when value of the atomic counter is one.

   procedure Initialize (Item : out Atomic_Counter);
   pragma Inline_Always (Initialize);
   --  Initialize counter by setting its value to one. This subprogram is
   --  intended to be used in special cases when counter object can't be
   --  initialized in standard way.

private

   type Unsigned_32 is mod 2 ** 32;

   type Atomic_Counter is limited record
      Value : aliased Unsigned_32 := 1;
      pragma Atomic (Value);
   end record;

end System.Atomic_Counters;
