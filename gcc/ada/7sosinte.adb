------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                              $Revision: 1.6 $
--                                                                          --
--            Copyright (C) 1997-2001 Florida State University              --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is a FSU Threads version of this package

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C;

package body System.OS_Interface is

   use Interfaces.C;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
   end To_Duration;

   function To_Duration (TV : struct_timeval) return Duration is
   begin
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / 10#1#E6;
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;

   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec' (tv_sec => S,
        tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   ----------------
   -- To_Timeval --
   ----------------

   function To_Timeval (D : Duration) return struct_timeval is
      S : long;
      F : Duration;

   begin
      S := long (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return struct_timeval' (tv_sec => S,
        tv_usec => long (Long_Long_Integer (F * 10#1#E6)));
   end To_Timeval;

   -------------
   -- sigwait --
   -------------

   --  FSU_THREADS has a nonstandard sigwait

   function sigwait
     (set  : access sigset_t;
      sig  : access Signal) return int
   is
      Result : int;

      function sigwait_base (set : access sigset_t) return int;
      pragma Import (C, sigwait_base, "sigwait");

   begin
      Result := sigwait_base (set);

      if Result = -1 then
         sig.all := 0;
         return errno;
      end if;

      sig.all := Signal (Result);
      return 0;
   end sigwait;

   ------------------------
   -- pthread_mutex_lock --
   ------------------------

   --  FSU_THREADS has nonstandard pthread_mutex_lock and unlock.
   --  It sets errno but the standard Posix requires it to be returned.

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int is
      function pthread_mutex_lock_base
        (mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_mutex_lock_base, "pthread_mutex_lock");

      Result : int;

   begin
      Result := pthread_mutex_lock_base (mutex);

      if Result /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutex_lock;

   --------------------------
   -- pthread_mutex_unlock --
   --------------------------

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t) return int
   is
      function pthread_mutex_unlock_base
        (mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_mutex_unlock_base, "pthread_mutex_unlock");

      Result : int;

   begin
      Result := pthread_mutex_unlock_base (mutex);

      if Result /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutex_unlock;

   -----------------------
   -- pthread_cond_wait --
   -----------------------

   --  FSU_THREADS has a nonstandard pthread_cond_wait.
   --  The FSU_THREADS version returns EINTR when interrupted.

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int
   is
      function pthread_cond_wait_base
        (cond  : access pthread_cond_t;
         mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_cond_wait_base, "pthread_cond_wait");

      Result : int;

   begin
      Result := pthread_cond_wait_base (cond, mutex);

      if Result = EINTR then
         return 0;
      else
         return Result;
      end if;
   end pthread_cond_wait;

   ----------------------------
   -- pthread_cond_timedwait --
   ----------------------------

   --  FSU_THREADS has a nonstandard pthread_cond_timedwait. The
   --  FSU_THREADS version returns -1 and set errno to EAGAIN for timeout.

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int
   is
      function pthread_cond_timedwait_base
        (cond    : access pthread_cond_t;
         mutex   : access pthread_mutex_t;
         abstime : access timespec) return int;
      pragma Import (C, pthread_cond_timedwait_base, "pthread_cond_timedwait");

      Result : int;

   begin
      Result := pthread_cond_timedwait_base (cond, mutex, abstime);

      if Result = -1 then
         if errno = EAGAIN then
            return ETIMEDOUT;
         else
            return EINVAL;
         end if;
      end if;

      return 0;
   end pthread_cond_timedwait;

   ---------------------------
   -- pthread_setschedparam --
   ---------------------------

   --  FSU_THREADS does not have pthread_setschedparam

   --  This routine returns a non-negative value upon failure
   --  but the error code can not be set conforming the POSIX standard.

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int
   is
      function pthread_setschedattr
        (thread : pthread_t;
         attr   : pthread_attr_t) return int;
      pragma Import (C, pthread_setschedattr, "pthread_setschedattr");

      attr   : aliased pthread_attr_t;
      Result : int;

   begin
      Result := pthread_attr_init (attr'Access);

      if Result /= 0 then
         return Result;
      end if;

      attr.sched := policy;

      --  Short-cut around pthread_attr_setprio

      attr.prio := param.sched_priority;

      Result := pthread_setschedattr (thread, attr);

      if Result /= 0 then
         return Result;
      end if;

      Result := pthread_attr_destroy (attr'Access);

      if Result /= 0 then
         return Result;
      else
         return 0;
      end if;
   end pthread_setschedparam;

   -------------------------
   -- pthread_getspecific --
   -------------------------

   --  FSU_THREADS has a nonstandard pthread_getspecific

   function pthread_getspecific (key : pthread_key_t) return System.Address is
      function pthread_getspecific_base
        (key   : pthread_key_t;
         value : access System.Address) return int;
      pragma Import (C, pthread_getspecific_base, "pthread_getspecific");

      Tmp    : aliased System.Address;
      Result : int;

   begin
      Result := pthread_getspecific_base (key, Tmp'Access);

      if Result /= 0 then
         return System.Null_Address;
      end if;

      return Tmp;
   end pthread_getspecific;

   ---------------------------------
   -- pthread_attr_setdetachstate --
   ---------------------------------

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int
   is
      function pthread_attr_setdetachstate_base
        (attr        : access pthread_attr_t;
         detachstate : access int) return int;
      pragma Import
        (C, pthread_attr_setdetachstate_base, "pthread_attr_setdetachstate");

      Tmp : aliased int := detachstate;

   begin
      return pthread_attr_setdetachstate_base (attr, Tmp'Access);
   end pthread_attr_setdetachstate;

   -----------------
   -- sched_yield --
   -----------------

   --  FSU_THREADS does not have sched_yield;

   function sched_yield return int is
      procedure sched_yield_base (arg : System.Address);
      pragma Import (C, sched_yield_base, "pthread_yield");

   begin
      sched_yield_base (System.Null_Address);
      return 0;
   end sched_yield;

   ----------------
   -- Stack_Base --
   ----------------

   function Get_Stack_Base (thread : pthread_t) return Address is
   begin
      return thread.stack_base;
   end Get_Stack_Base;

end System.OS_Interface;
