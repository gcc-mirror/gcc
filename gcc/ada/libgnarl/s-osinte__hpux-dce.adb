------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2024, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a DCE version of this package.
--  Currently HP-UX and SNI use this file

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

with Interfaces.C; use Interfaces.C;

package body System.OS_Interface is

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
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

      return timespec'(tv_sec => S,
                       tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal) return int
   is
      Result : int;

   begin
      Result := sigwait (set);

      if Result = -1 then
         sig.all := 0;
         return errno;
      end if;

      sig.all := Signal (Result);
      return 0;
   end sigwait;

   --  DCE_THREADS does not have pthread_kill. Instead, we just ignore it

   function pthread_kill (thread : pthread_t; sig : Signal) return int is
      pragma Unreferenced (thread, sig);
   begin
      return 0;
   end pthread_kill;

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   --  For all following functions, DCE Threads has a non standard behavior.
   --  It sets errno but the standard Posix requires it to be returned.

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int
   is
      function pthread_mutexattr_create
        (attr : access pthread_mutexattr_t) return int;
      pragma Import (C, pthread_mutexattr_create, "pthread_mutexattr_create");

   begin
      if pthread_mutexattr_create (attr) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_mutexattr_init;

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int
   is
      function pthread_mutexattr_delete
        (attr : access pthread_mutexattr_t) return int;
      pragma Import (C, pthread_mutexattr_delete, "pthread_mutexattr_delete");

   begin
      if pthread_mutexattr_delete (attr) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_mutexattr_destroy;

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int
   is
      function pthread_mutex_init_base
        (mutex : access pthread_mutex_t;
         attr  : pthread_mutexattr_t) return int;
      pragma Import (C, pthread_mutex_init_base, "pthread_mutex_init");

   begin
      if pthread_mutex_init_base (mutex, attr.all) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_mutex_init;

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t) return int
   is
      function pthread_mutex_destroy_base
        (mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_mutex_destroy_base, "pthread_mutex_destroy");

   begin
      if pthread_mutex_destroy_base (mutex) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_mutex_destroy;

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t) return int
   is
      function pthread_mutex_lock_base
        (mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_mutex_lock_base, "pthread_mutex_lock");

   begin
      if pthread_mutex_lock_base (mutex) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_mutex_lock;

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t) return int
   is
      function pthread_mutex_unlock_base
        (mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_mutex_unlock_base, "pthread_mutex_unlock");

   begin
      if pthread_mutex_unlock_base (mutex) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_mutex_unlock;

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int
   is
      function pthread_condattr_create
        (attr : access pthread_condattr_t) return int;
      pragma Import (C, pthread_condattr_create, "pthread_condattr_create");

   begin
      if pthread_condattr_create (attr) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_condattr_init;

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int
   is
      function pthread_condattr_delete
        (attr : access pthread_condattr_t) return int;
      pragma Import (C, pthread_condattr_delete, "pthread_condattr_delete");

   begin
      if pthread_condattr_delete (attr) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_condattr_destroy;

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int
   is
      function pthread_cond_init_base
        (cond : access pthread_cond_t;
         attr : pthread_condattr_t) return int;
      pragma Import (C, pthread_cond_init_base, "pthread_cond_init");

   begin
      if pthread_cond_init_base (cond, attr.all) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_cond_init;

   function pthread_cond_destroy
     (cond : access pthread_cond_t) return int
   is
      function pthread_cond_destroy_base
        (cond : access pthread_cond_t) return int;
      pragma Import (C, pthread_cond_destroy_base, "pthread_cond_destroy");

   begin
      if pthread_cond_destroy_base (cond) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_cond_destroy;

   function pthread_cond_signal
     (cond : access pthread_cond_t) return int
   is
      function pthread_cond_signal_base
        (cond : access pthread_cond_t) return int;
      pragma Import (C, pthread_cond_signal_base, "pthread_cond_signal");

   begin
      if pthread_cond_signal_base (cond) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_cond_signal;

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int
   is
      function pthread_cond_wait_base
        (cond  : access pthread_cond_t;
         mutex : access pthread_mutex_t) return int;
      pragma Import (C, pthread_cond_wait_base, "pthread_cond_wait");

   begin
      if pthread_cond_wait_base (cond, mutex) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_cond_wait;

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

   begin
      if pthread_cond_timedwait_base (cond, mutex, abstime) /= 0 then
         return (if errno = EAGAIN then ETIMEDOUT else errno);
      else
         return 0;
      end if;
   end pthread_cond_timedwait;

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int
   is
      function pthread_setscheduler
        (thread   : pthread_t;
         policy   : int;
         priority : int) return int;
      pragma Import (C, pthread_setscheduler, "pthread_setscheduler");

   begin
      if pthread_setscheduler (thread, policy, param.sched_priority) = -1 then
         return errno;
      else
         return 0;
      end if;
   end pthread_setschedparam;

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

   function pthread_attr_init
     (attributes : access pthread_attr_t) return int
   is
      function pthread_attr_create
        (attributes : access pthread_attr_t) return int;
      pragma Import (C, pthread_attr_create, "pthread_attr_create");

   begin
      if pthread_attr_create (attributes) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_attr_init;

   function pthread_attr_destroy
     (attributes : access pthread_attr_t) return int
   is
      function pthread_attr_delete
        (attributes : access pthread_attr_t) return int;
      pragma Import (C, pthread_attr_delete, "pthread_attr_delete");

   begin
      if pthread_attr_delete (attributes) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_attr_destroy;

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int
   is
      function pthread_attr_setstacksize_base
        (attr      : access pthread_attr_t;
         stacksize : size_t) return int;
      pragma Import (C, pthread_attr_setstacksize_base,
                     "pthread_attr_setstacksize");

   begin
      if pthread_attr_setstacksize_base (attr, stacksize) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_attr_setstacksize;

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int
   is
      function pthread_create_base
        (thread        : access pthread_t;
         attributes    : pthread_attr_t;
         start_routine : Thread_Body;
         arg           : System.Address) return int;
      pragma Import (C, pthread_create_base, "pthread_create");

   begin
      if pthread_create_base
        (thread, attributes.all, start_routine, arg) /= 0
      then
         return errno;
      else
         return 0;
      end if;
   end pthread_create;

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return int
   is
      function pthread_setspecific_base
        (key   : pthread_key_t;
         value : System.Address) return int;
      pragma Import (C, pthread_setspecific_base, "pthread_setspecific");

   begin
      if pthread_setspecific_base (key, value) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_setspecific;

   function pthread_getspecific (key : pthread_key_t) return System.Address is
      function pthread_getspecific_base
        (key   : pthread_key_t;
         value : access System.Address) return  int;
      pragma Import (C, pthread_getspecific_base, "pthread_getspecific");
      Addr : aliased System.Address;

   begin
      if pthread_getspecific_base (key, Addr'Access) /= 0 then
         return System.Null_Address;
      else
         return Addr;
      end if;
   end pthread_getspecific;

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int
   is
      function pthread_keycreate
        (key        : access pthread_key_t;
         destructor : destructor_pointer) return int;
      pragma Import (C, pthread_keycreate, "pthread_keycreate");

   begin
      if pthread_keycreate (key, destructor) /= 0 then
         return errno;
      else
         return 0;
      end if;
   end pthread_key_create;

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Warnings (Off, thread);
   begin
      return Null_Address;
   end Get_Stack_Base;

   procedure pthread_init is
   begin
      null;
   end pthread_init;

   function intr_attach (sig : int; handler : isr_address) return long is
      function c_signal (sig : int; handler : isr_address) return long;
      pragma Import (C, c_signal, "signal");
   begin
      return c_signal (sig, handler);
   end intr_attach;

end System.OS_Interface;
