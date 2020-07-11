------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ L O C K                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2020, AdaCore                     --
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

--  Simple task lock and unlock routines

--  A small package containing a task lock and unlock routines for creating
--  a critical region. The lock involved is a global lock, shared by all
--  tasks, and by all calls to these routines, so these routines should be
--  used with care to avoid unnecessary reduction of concurrency.

--  These routines may be used in a non-tasking program, and in that case
--  they have no effect (they do NOT cause the tasking runtime to be loaded).

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.Task_Lock (file g-tasloc.ads).

package System.Task_Lock is
   pragma Preelaborate;

   procedure Lock;
   pragma Inline (Lock);
   --  Acquires the global lock, starts the execution of a critical region
   --  which no other task can enter until the locking task calls Unlock

   procedure Unlock;
   pragma Inline (Unlock);
   --  Releases the global lock, allowing another task to successfully
   --  complete a Lock operation. Terminates the critical region.
   --
   --  The recommended protocol for using these two procedures is as
   --  follows:
   --
   --    Locked_Processing : begin
   --       Lock;
   --       ...
   --       TSL.Unlock;
   --
   --    exception
   --       when others =>
   --          Unlock;
   --          raise;
   --    end Locked_Processing;
   --
   --  This ensures that the lock is not left set if an exception is raised
   --  explicitly or implicitly during the critical locked region.
   --
   --  Note on multiple calls to Lock: It is permissible to call Lock
   --  more than once with no intervening Unlock from a single task,
   --  and the lock will not be released until the corresponding number
   --  of Unlock operations has been performed. For example:
   --
   --    System.Task_Lock.Lock;     -- acquires lock
   --    System.Task_Lock.Lock;     -- no effect
   --    System.Task_Lock.Lock;     -- no effect
   --    System.Task_Lock.Unlock;   -- no effect
   --    System.Task_Lock.Unlock;   -- no effect
   --    System.Task_Lock.Unlock;   -- releases lock
   --
   --  However, as previously noted, the Task_Lock facility should only
   --  be used for very local locks where the probability of conflict is
   --  low, so usually this kind of nesting is not a good idea in any case.
   --  In more complex locking situations, it is more appropriate to define
   --  an appropriate protected type to provide the required locking.
   --
   --  It is an error to call Unlock when there has been no prior call to
   --  Lock. The effect of such an erroneous call is undefined, and may
   --  result in deadlock, or other malfunction of the run-time system.

end System.Task_Lock;
