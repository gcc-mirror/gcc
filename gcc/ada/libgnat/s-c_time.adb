------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . C _ T I M E                        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 2025, Free Software Foundation, Inc.           --
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

package body System.C_Time is

   --  Two Duration representations are described in targparm.ads:
   --   Size      Small       Last = (2**(Size - 1) - 1) * Small
   --    32    0.02                   42_949_672.94
   --    64    0.000_000_001       9_223_372_036.854_775_807

   Recip : constant := (if Duration'Size = 32 then 50 else 1_000_000_000);
   --  The reciprocal of the Small used to write "* Small" as "/ Recip"

   Milli : constant := 1_000;
   Micro : constant := 1_000_000;
   Nano  : constant := 1_000_000_000;
   --  The standard divisors

   pragma Unsuppress (Overflow_Check);
   --  Overflow may occur during the various conversions

   -------------------------
   -- In_Timeval_Duration --
   -------------------------

   --  Immediate : constant Duration := 0.0;

   --  Forever   : constant Duration :=
   --    Duration'Min (Duration'Last, 1.0 * OS_Constants.MAX_tv_sec);

   --  subtype Timeval_Duration is Duration range Immediate .. Forever;

   function In_Timeval_Duration (T : timeval) return Boolean is
      Max_Dur  : constant := 2**(Duration'Size - 1) - 1;
      Max_Sec  : constant := Max_Dur / Recip;
      Max_Usec : constant := (Max_Dur mod Recip) * Micro / Recip;

      --  When Duration'Size = 64 and time_t'Size = 32, the compiler
      --  complains that Max_Sec does not fit in time_t, hence cannot
      --  be compared with T.tv_sec.
      Safe_Max_Sec : constant :=
        (if Max_Sec > time_t'Last then time_t'Last else Max_Sec);
      Safe_Max_Usec : constant :=
        (if Max_Sec > time_t'Last then usec_t'Last else Max_Usec);

   begin
      pragma Warnings (Off, "condition is always");
      return T.tv_sec >= 0
        and then (T.tv_sec > 0 or else T.tv_usec >= 0)
        and then T.tv_sec <= Safe_Max_Sec
        and then (T.tv_sec < Safe_Max_Sec or else T.tv_usec <= Safe_Max_Usec)
        and then T.tv_sec <= OS_Constants.MAX_tv_sec
        and then (T.tv_sec < OS_Constants.MAX_tv_sec or else T.tv_usec = 0);
      pragma Warnings (On, "condition is always");
   end In_Timeval_Duration;

   -----------------------------
   -- Milliseconds_To_Timeval --
   -----------------------------

   function Milliseconds_To_Timeval (M : Interfaces.C.int) return timeval is
      use Interfaces.C;
      Q : constant int := M  /  Milli;
      R : constant int := M rem Milli;

   begin
      return (tv_sec => time_t (Q), tv_usec => usec_t (R) * (Micro / Milli));
   end Milliseconds_To_Timeval;

   -----------------------------
   -- Nanoseconds_To_Timespec --
   -----------------------------

   function Nanoseconds_To_Timespec (N : Interfaces.C.int) return timespec is
      use Interfaces.C;
      Q : constant int := N  /  Nano;
      R : constant int := N rem Nano;

   begin
      return (tv_sec => time_t (Q), tv_nsec => nsec_t (R));
   end Nanoseconds_To_Timespec;

   -----------------
   -- To_Duration --
   -----------------

   --  Duration (tv_usec) is OK even when Duration'Size = 32, see above

   function To_Duration (T : timeval) return Duration is
   begin
      return Duration (T.tv_sec) + Duration (T.tv_usec) / Micro;
   end To_Duration;

   --  Duration (tv_nsec) overflows when Duration'Size = 32, see above.
   --  Scale down nanoseconds by the value of the Small in nanoseconds.

   function To_Duration (T : timespec) return Duration is
      S : constant := Nano / Recip;

   begin
      return Duration (T.tv_sec) + Duration (T.tv_nsec / S) / (Nano / S);
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (T : timeval) return timespec is
   begin
      return (tv_sec => T.tv_sec, tv_nsec => nsec_t (T.tv_usec) * Milli);
   end To_Timespec;

   function To_Timespec (D : Duration) return timespec is
      tv_sec  : time_t;
      tv_nsec : nsec_t;

   begin
      if D = 0.0 then
         tv_sec  := 0;
         tv_nsec := 0;

      elsif D < 0.0 then
         tv_sec := time_t (D + 0.5);
         if D = Duration (tv_sec) then
            tv_nsec := 0;
         else
            tv_nsec := nsec_t ((D - Duration (tv_sec)) * Nano + 0.5);
         end if;

      else
         tv_sec := time_t (D - 0.5);
         if D = Duration (tv_sec) then
            tv_nsec := 0;
         else
            tv_nsec := nsec_t ((D - Duration (tv_sec)) * Nano - 0.5);
         end if;
      end if;

      return (tv_sec, tv_nsec);
   end To_Timespec;

   -----------------
   -- To_Timeval --
   -----------------

   function To_Timeval (D : Duration) return timeval is
      tv_sec  : time_t;
      tv_usec : usec_t;

   begin
      if D = 0.0 then
         tv_sec  := 0;
         tv_usec := 0;

      elsif D < 0.0 then
         tv_sec := time_t (D + 0.5);
         if D = Duration (tv_sec) then
            tv_usec := 0;
         else
            tv_usec := usec_t ((D - Duration (tv_sec)) * Micro + 0.5);
         end if;

      else
         tv_sec := time_t (D - 0.5);
         if D = Duration (tv_sec) then
            tv_usec := 0;
         else
            tv_usec := usec_t ((D - Duration (tv_sec)) * Micro - 0.5);
         end if;
      end if;

      return (tv_sec, tv_usec);
   end To_Timeval;

end System.C_Time;
