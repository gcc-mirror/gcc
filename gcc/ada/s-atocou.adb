------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ C O U N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2011-2013, AdaCore                      --
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

--  This is dummy version of the package, for use on platforms where this
--  capability is not supported. Any use of any of the routines in this
--  package will raise Program_Error.

--  Why don't we use pragma Unimplemented_Unit in a dummy spec, this would
--  seem much more useful than raising an exception at run time ???

package body System.Atomic_Counters is

   ---------------
   -- Decrement --
   ---------------

   function Decrement (Item : in out Atomic_Counter) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Decrement;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Item : in out Atomic_Counter) is
   begin
      raise Program_Error;
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
