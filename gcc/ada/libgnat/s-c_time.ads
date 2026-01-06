------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . C _ T I M E                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2025-2026, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software; you can  redistribute it  and/or modify it under --
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
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the time_t, timeval and timespec types corresponding
--  to the C types defined by the OS, as well as various conversion functions.

with Interfaces.C;

with System.OS_Constants;

package System.C_Time
  with Pure
is
   --  These two C structs represent durations with different accuracies and
   --  maximal values.

   type time_t is range -2 ** (OS_Constants.SIZEOF_tv_sec * 8 - 1) ..
                         2 ** (OS_Constants.SIZEOF_tv_sec * 8 - 1) - 1
     with Convention => C, Size => OS_Constants.SIZEOF_tv_sec * 8;

   type usec_t is range -2 ** (OS_Constants.SIZEOF_tv_usec * 8 - 1) ..
                         2 ** (OS_Constants.SIZEOF_tv_usec * 8 - 1) - 1
     with Convention => C, Size => OS_Constants.SIZEOF_tv_usec * 8;
   --  Larger than the suseconds_t C type on ARM 32 bits with GNU libc
   --  when __TIME_BITS=64.

   type timeval is record
      tv_sec  : time_t;  --  seconds
      tv_usec : usec_t;  --  microseconds
   end record
     with Convention => C;

   type nsec_t is range -2 ** (OS_Constants.SIZEOF_tv_nsec * 8 - 1) ..
                         2 ** (OS_Constants.SIZEOF_tv_nsec * 8 - 1) - 1
     with Convention => C, Size => OS_Constants.SIZEOF_tv_nsec * 8;
   --  Larger than the signed long int C type on x32.

   type timespec is record
      tv_sec  : time_t;  --  seconds
      tv_nsec : nsec_t;  --  nanoseconds
   end record
     with Convention => C;

   --  All conversion functions truncate the result if it is inexact

   function To_Duration (T : timespec) return Duration with Inline;
   function To_Duration (T : timeval)  return Duration with Inline;

   function To_Timespec (D : Duration) return timespec with Inline;
   function To_Timeval  (D : Duration) return timeval  with Inline;

   function In_Timeval_Duration (T : timeval) return Boolean with Inline;
   --  g-socket.adb if not Windows target

   function Milliseconds_To_Timeval (M : Interfaces.C.int) return timeval
     with Inline;
   --  g-sothco.ads
   --  g-spogwa.adb

   function Nanoseconds_To_Timespec (N : Interfaces.C.int) return timespec
     with Inline;
   function To_Timespec (T : timeval) return timespec with Inline;
   --  s-osinte__darwin.adb

end System.C_Time;
