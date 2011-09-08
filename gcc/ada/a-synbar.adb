------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S Y N C H R O N O U S _ B A R R I E R S              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

package body Ada.Synchronous_Barriers is

   protected body Synchronous_Barrier is

      --  The condition "Wait'Count = Release_Threshold" opens the barrier when
      --  the required number of tasks is reached. The condition "Keep_Open"
      --  leaves the barrier open while there are queued tasks. While there are
      --  tasks in the queue no new task will be queued (no new protected
      --  action can be started on a protected object while another protected
      --  action on the same protected object is underway, RM 9.5.1 (4)),
      --  guaranteeing that the barrier will remain open only for those tasks
      --  already inside the queue when the barrier was open.

      entry Wait (Notified : out Boolean)
        when Keep_Open or else Wait'Count = Release_Threshold
      is
      begin
         --  If we are executing the entry it means that the required number of
         --  tasks have been queued in the entry. Keep_Open barrier will remain
         --  true until all queued tasks are out.

         Keep_Open := Wait'Count > 0;

         --  The last released task will close the barrier and get the Notified
         --  token.

         Notified := Wait'Count = 0;
      end Wait;
   end Synchronous_Barrier;

   ----------------------
   -- Wait_For_Release --
   ----------------------

   procedure Wait_For_Release
     (The_Barrier : in out Synchronous_Barrier;
      Notified    : out Boolean)
   is
   begin
      The_Barrier.Wait (Notified);
   end Wait_For_Release;

end Ada.Synchronous_Barriers;
