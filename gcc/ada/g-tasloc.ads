------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       G N A T . T A S K _ L O C K                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--            Copyright (C) 1998-2001 Ada Core Technologies, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Simple task lock and unlock routines

--  A small package containing a task lock and unlock routines for creating
--  a critical region. The lock involved is a global lock, shared by all
--  tasks, and by all calls to these routines, so these routines should be
--  used with care to avoid unnecessary reduction of concurrency.

--  These routines may be used in a non-tasking program, and in that case
--  they have no effect (they do NOT cause the tasking runtime to be loaded).

package GNAT.Task_Lock is
pragma Elaborate_Body (Task_Lock);

   procedure Lock;
   pragma Inline (Lock);
   --  Acquires the global lock, starts the execution of a critical region
   --  which no other task can enter until the locking task calls Unlock

   procedure Unlock;
   pragma Inline (Unlock);
   --  Releases the global lock, allowing another task to successfully
   --  complete a Lock operation. Terminates the critical region.

   --  The recommended protocol for using these two procedures is as
   --  follows:

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

   --  This ensures that the lock is not left set if an exception is raised
   --  explicitly or implicitly during the critical locked region.

   --  Note on multiple calls to Lock: It is permissible to call Lock
   --  more than once with no intervening Unlock from a single task,
   --  and the lock will not be released until the corresponding number
   --  of Unlock operations has been performed. For example:

   --    GNAT.Task_Lock.Lock;     -- acquires lock
   --    GNAT.Task_Lock.Lock;     -- no effect
   --    GNAT.Task_Lock.Lock;     -- no effect
   --    GNAT.Task_Lock.Unlock;   -- no effect
   --    GNAT.Task_Lock.Unlock;   -- no effect
   --    GNAT.Task_Lock.Unlock;   -- releases lock

   --  However, as previously noted, the Task_Lock facility should only
   --  be used for very local locks where the probability of conflict is
   --  low, so usually this kind of nesting is not a good idea in any case.
   --  In more complex locking situations, it is more appropriate to define
   --  an appropriate protected type to provide the required locking.

end GNAT.Task_Lock;
