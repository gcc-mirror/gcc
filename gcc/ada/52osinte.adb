------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                              $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1999-2000 Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a LynxOS (Native) version of this package

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C;

package body System.OS_Interface is

   use Interfaces.C;

   -------------------
   -- clock_gettime --
   -------------------

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return  int
   is
      function clock_gettime_base
        (clock_id : clockid_t;
         tp       : access timespec)
         return  int;
      pragma Import (C, clock_gettime_base, "clock_gettime");

   begin
      if clock_gettime_base (clock_id, tp) /= 0 then
         return errno;
      end if;

      return 0;
   end clock_gettime;

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

      return struct_timeval' (tv_sec => S,
        tv_usec => time_t (Long_Long_Integer (F * 10#1#E6)));
   end To_Timeval;

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function sigwait
     (set :  access sigset_t;
      sig :  access Signal)
      return int
   is
      function sigwait_base
        (set   : access sigset_t;
         value : System.Address)
        return Signal;
      pragma Import (C, sigwait_base, "sigwait");

   begin
      sig.all := sigwait_base (set, Null_Address);

      if sig.all = -1 then
         return errno;
      end if;

      return 0;
   end sigwait;

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   --  For all the following functions, LynxOS threads has the POSIX Draft 4
   --  begavior; it sets errno but the standard Posix requires it to be
   --  returned.

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t)
      return int
   is
      function pthread_mutexattr_create
        (attr : access pthread_mutexattr_t)
         return int;
      pragma Import (C, pthread_mutexattr_create, "pthread_mutexattr_create");

   begin
      if pthread_mutexattr_create (attr) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutexattr_init;

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t)
      return int
   is
      function pthread_mutexattr_delete
        (attr : access pthread_mutexattr_t)
         return int;
      pragma Import (C, pthread_mutexattr_delete, "pthread_mutexattr_delete");

   begin
      if pthread_mutexattr_delete (attr) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutexattr_destroy;

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t)
      return  int
   is
      function pthread_mutex_init_base
        (mutex : access pthread_mutex_t;
         attr  : pthread_mutexattr_t)
         return  int;
      pragma Import (C, pthread_mutex_init_base, "pthread_mutex_init");

   begin
      if pthread_mutex_init_base (mutex, attr.all) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutex_init;

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t)
      return  int
   is
      function pthread_mutex_destroy_base
        (mutex : access pthread_mutex_t)
         return  int;
      pragma Import (C, pthread_mutex_destroy_base, "pthread_mutex_destroy");

   begin
      if pthread_mutex_destroy_base (mutex) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutex_destroy;

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t)
      return  int
   is
      function pthread_mutex_lock_base
        (mutex : access pthread_mutex_t)
         return  int;
      pragma Import (C, pthread_mutex_lock_base, "pthread_mutex_lock");

   begin
      if pthread_mutex_lock_base (mutex) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutex_lock;

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t)
      return  int
   is
      function pthread_mutex_unlock_base
        (mutex : access pthread_mutex_t)
         return  int;
      pragma Import (C, pthread_mutex_unlock_base, "pthread_mutex_unlock");

   begin
      if pthread_mutex_unlock_base (mutex) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_mutex_unlock;

   function pthread_condattr_init
     (attr : access pthread_condattr_t)
      return int
   is
      function pthread_condattr_create
        (attr : access pthread_condattr_t)
         return int;
      pragma Import (C, pthread_condattr_create, "pthread_condattr_create");

   begin
      if pthread_condattr_create (attr) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_condattr_init;

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t)
      return int
   is
      function pthread_condattr_delete
        (attr : access pthread_condattr_t)
         return int;
      pragma Import (C, pthread_condattr_delete, "pthread_condattr_delete");

   begin
      if pthread_condattr_delete (attr) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_condattr_destroy;

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t)
      return int
   is
      function pthread_cond_init_base
        (cond : access pthread_cond_t;
         attr : pthread_condattr_t)
         return int;
      pragma Import (C, pthread_cond_init_base, "pthread_cond_init");

   begin
      if pthread_cond_init_base (cond, attr.all) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_cond_init;

   function pthread_cond_destroy
     (cond : access pthread_cond_t)
      return int
   is
      function pthread_cond_destroy_base
        (cond : access pthread_cond_t)
         return int;
      pragma Import (C, pthread_cond_destroy_base, "pthread_cond_destroy");

   begin
      if pthread_cond_destroy_base (cond) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_cond_destroy;

   function pthread_cond_signal
     (cond : access pthread_cond_t)
      return int
   is
      function pthread_cond_signal_base
        (cond : access pthread_cond_t)
         return int;
      pragma Import (C, pthread_cond_signal_base, "pthread_cond_signal");

   begin
      if pthread_cond_signal_base (cond) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_cond_signal;

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t)
      return  int
   is
      function pthread_cond_wait_base
        (cond  : access pthread_cond_t;
         mutex : access pthread_mutex_t)
         return  int;
      pragma Import (C, pthread_cond_wait_base, "pthread_cond_wait");

   begin
      if pthread_cond_wait_base (cond, mutex) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_cond_wait;

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      reltime : access timespec) return int
   is
      function pthread_cond_timedwait_base
        (cond    : access pthread_cond_t;
         mutex   : access pthread_mutex_t;
         reltime : access timespec) return int;
      pragma Import (C, pthread_cond_timedwait_base, "pthread_cond_timedwait");

   begin
      if pthread_cond_timedwait_base (cond, mutex, reltime) /= 0 then
         if errno = EAGAIN then
            return ETIMEDOUT;
         end if;

         return errno;
      end if;

      return 0;
   end pthread_cond_timedwait;

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param)
      return   int
   is
      function pthread_setscheduler
        (thread : pthread_t;
         policy : int;
         prio   : int)
         return   int;
      pragma Import (C, pthread_setscheduler, "pthread_setscheduler");

   begin
      if pthread_setscheduler (thread, policy, param.sched_priority) = -1 then
         return errno;
      end if;

      return 0;
   end pthread_setschedparam;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int)
      return     int
   is
   begin
      return 0;
   end pthread_mutexattr_setprotocol;

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int)
      return        int
   is
   begin
      return 0;
   end pthread_mutexattr_setprioceiling;

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int)
      return            int
   is
   begin
      return 0;
   end pthread_attr_setscope;

   function sched_yield return int is
      procedure pthread_yield;
      pragma Import (C, pthread_yield, "pthread_yield");

   begin
      pthread_yield;
      return 0;
   end sched_yield;

   -----------------------------
   --  P1003.1c - Section 16  --
   -----------------------------

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int)
      return        int
   is
   begin
      return 0;
   end pthread_attr_setdetachstate;

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address)
      return          int
   is
      --  The LynxOS pthread_create doesn't seems to work.
      --  Workaround : We're using st_new instead.
      --
      --   function pthread_create_base
      --     (thread        : access pthread_t;
      --      attributes    : pthread_attr_t;
      --      start_routine : Thread_Body;
      --      arg           : System.Address)
      --      return          int;
      --   pragma Import (C, pthread_create_base, "pthread_create");

      St : aliased st_t := attributes.st;

      function st_new
        (start_routine : Thread_Body;
         arg           : System.Address;
         attributes    : access st_t;
         thread        : access pthread_t)
         return          int;
      pragma Import (C, st_new, "st_new");

   begin
      --  Following code would be used if above commented function worked

      --   if pthread_create_base
      --        (thread, attributes.all, start_routine, arg) /= 0 then

      if st_new (start_routine, arg, St'Access, thread) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_create;

   function pthread_detach (thread : pthread_t) return int is
      aliased_thread : aliased pthread_t := thread;

      function pthread_detach_base (thread : access pthread_t) return int;
      pragma Import (C, pthread_detach_base, "pthread_detach");

   begin
      if pthread_detach_base (aliased_thread'Access) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_detach;

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address)
      return  int
   is
      function pthread_setspecific_base
        (key   : pthread_key_t;
         value : System.Address)
         return  int;
      pragma Import (C, pthread_setspecific_base, "pthread_setspecific");

   begin
      if pthread_setspecific_base (key, value) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_setspecific;

   function pthread_getspecific (key : pthread_key_t) return System.Address is
      procedure pthread_getspecific_base
        (key   : pthread_key_t;
         value : access System.Address);
      pragma Import (C, pthread_getspecific_base, "pthread_getspecific");

      value : aliased System.Address := System.Null_Address;

   begin
      pthread_getspecific_base (key, value'Unchecked_Access);
      return value;
   end pthread_getspecific;

   function Get_Stack_Base (thread : pthread_t) return Address is
   begin
      return Null_Address;
   end Get_Stack_Base;

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer)
      return       int
   is
      function pthread_keycreate
        (key        : access pthread_key_t;
         destructor : destructor_pointer)
         return       int;
      pragma Import (C, pthread_keycreate, "pthread_keycreate");

   begin
      if pthread_keycreate (key, destructor) /= 0 then
         return errno;
      end if;

      return 0;
   end pthread_key_create;

   procedure pthread_init is
   begin
      null;
   end pthread_init;

end System.OS_Interface;
