------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S Y N C H R O N O U S _ B A R R I E R S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This is the spec of this package using POSIX barriers

with System;
private with Ada.Finalization;
private with Interfaces.C;

package Ada.Synchronous_Barriers with SPARK_Mode => Off is
   pragma Preelaborate (Synchronous_Barriers);

   subtype Barrier_Limit is Positive range 1 .. Positive'Last;

   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is
      limited private;

   procedure Wait_For_Release
     (The_Barrier : in out Synchronous_Barrier;
      Notified    : out Boolean);

private
   --  POSIX barrier data type

   SIZEOF_PTHREAD_BARRIER_T : constant :=
     (if System.Word_Size = 64 then 32 else 20);
   --  Value defined according to the linux definition in pthreadtypes.h. On
   --  other system, e.g. MIPS IRIX, the object is smaller, so it works
   --  correctly although we are wasting some space.

   type pthread_barrier_t_view is (size_based, align_based);

   type pthread_barrier_t (Kind : pthread_barrier_t_view := size_based) is
      record
         case Kind is
            when size_based =>
               size : Interfaces.C.char_array (1 .. SIZEOF_PTHREAD_BARRIER_T);
            when align_based =>
               align : Interfaces.C.long;
         end case;
      end record;
   pragma Unchecked_Union (pthread_barrier_t);

   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is
     new Ada.Finalization.Limited_Controlled with
        record
           POSIX_Barrier : aliased pthread_barrier_t;
        end record;

   overriding procedure Initialize (Barrier : in out Synchronous_Barrier);
   overriding procedure Finalize   (Barrier : in out Synchronous_Barrier);
end Ada.Synchronous_Barriers;
