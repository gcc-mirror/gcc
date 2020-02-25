------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S Y N C H R O N O U S _ B A R R I E R S              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  This is the body of this package using POSIX barriers

with Interfaces.C; use Interfaces.C;

package body Ada.Synchronous_Barriers with SPARK_Mode => Off is

   --------------------
   -- POSIX barriers --
   --------------------

   function pthread_barrier_init
     (barrier : not null access pthread_barrier_t;
      attr    : System.Address := System.Null_Address;
      count   : unsigned) return int;
   pragma Import (C, pthread_barrier_init, "pthread_barrier_init");
   --  Initialize barrier with the attributes in attr. The barrier is opened
   --  when count waiters arrived. If attr is null the default barrier
   --  attributes are used.

   function pthread_barrier_destroy
     (barrier : not null access pthread_barrier_t) return int;
   pragma Import (C, pthread_barrier_destroy, "pthread_barrier_destroy");
   --  Destroy a previously dynamically initialized barrier

   function pthread_barrier_wait
     (barrier : not null access pthread_barrier_t) return int;
   pragma Import (C, pthread_barrier_wait, "pthread_barrier_wait");
   --  Wait on barrier

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Barrier : in out Synchronous_Barrier) is
      Result : int;
   begin
      Result := pthread_barrier_destroy (Barrier.POSIX_Barrier'Access);
      pragma Assert (Result = 0);
   end Finalize;

   overriding procedure Initialize (Barrier : in out Synchronous_Barrier) is
      Result : int;
   begin
      Result :=
        pthread_barrier_init
          (barrier => Barrier.POSIX_Barrier'Access,
           attr    => System.Null_Address,
           count   => unsigned (Barrier.Release_Threshold));
      pragma Assert (Result = 0);
   end Initialize;

   ----------------------
   -- Wait_For_Release --
   ----------------------

   procedure Wait_For_Release
     (The_Barrier : in out Synchronous_Barrier;
      Notified    : out Boolean)
   is
      Result : int;

      PTHREAD_BARRIER_SERIAL_THREAD : constant := -1;
      --  Value used to indicate the task which receives the notification for
      --  the barrier open.

   begin
      Result :=
        pthread_barrier_wait
          (barrier => The_Barrier.POSIX_Barrier'Access);
      pragma Assert
        (Result = 0 or else Result = PTHREAD_BARRIER_SERIAL_THREAD);

      Notified := (Result = PTHREAD_BARRIER_SERIAL_THREAD);
   end Wait_For_Release;

end Ada.Synchronous_Barriers;
