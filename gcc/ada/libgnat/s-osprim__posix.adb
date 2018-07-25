------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

--  This version is for POSIX-like operating systems

package body System.OS_Primitives is

   --  ??? These definitions are duplicated from System.OS_Interface
   --  because we don't want to depend on any package. Consider removing
   --  these declarations in System.OS_Interface and move these ones in
   --  the spec.

   type time_t is new Long_Integer;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : Long_Integer;
   end record;
   pragma Convention (C, timespec);

   function nanosleep (rqtp, rmtp : not null access timespec) return Integer;
   pragma Import (C, nanosleep, "nanosleep");

   -----------
   -- Clock --
   -----------

   function Clock return Duration is

      type timeval is array (1 .. 3) of Long_Integer;
      --  The timeval array is sized to contain Long_Long_Integer sec and
      --  Long_Integer usec. If Long_Long_Integer'Size = Long_Integer'Size then
      --  it will be overly large but that will not effect the implementation
      --  since it is not accessed directly.

      procedure timeval_to_duration
        (T    : not null access timeval;
         sec  : not null access Long_Long_Integer;
         usec : not null access Long_Integer);
      pragma Import (C, timeval_to_duration, "__gnat_timeval_to_duration");

      Micro  : constant := 10**6;
      sec    : aliased Long_Long_Integer;
      usec   : aliased Long_Integer;
      TV     : aliased timeval;
      Result : Integer;
      pragma Unreferenced (Result);

      function gettimeofday
        (Tv : access timeval;
         Tz : System.Address := System.Null_Address) return Integer;
      pragma Import (C, gettimeofday, "gettimeofday");

   begin
      --  The return codes for gettimeofday are as follows (from man pages):
      --    EPERM  settimeofday is called by someone other than the superuser
      --    EINVAL Timezone (or something else) is invalid
      --    EFAULT One of tv or tz pointed outside accessible address space

      --  None of these codes signal a potential clock skew, hence the return
      --  value is never checked.

      Result := gettimeofday (TV'Access, System.Null_Address);
      timeval_to_duration (TV'Access, sec'Access, usec'Access);
      return Duration (sec) + Duration (usec) / Micro;
   end Clock;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec;

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

      return
        timespec'(tv_sec  => S,
                  tv_nsec => Long_Integer (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Duration;
      Mode : Integer)
   is separate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

end System.OS_Primitives;
