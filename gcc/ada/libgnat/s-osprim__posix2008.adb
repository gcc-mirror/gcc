------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2020, Free Software Foundation, Inc.         --
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

--  This version is for POSIX.1-2008-like operating systems

with System.CRTL;
with System.OS_Constants;
package body System.OS_Primitives is

   subtype int is System.CRTL.int;

   --  ??? These definitions are duplicated from System.OS_Interface because
   --  we don't want to depend on any package. Consider removing these
   --  declarations in System.OS_Interface and move these ones to the spec.

   type time_t is new System.CRTL.int64;

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
      TS     : aliased timespec;
      Result : int;

      type clockid_t is new int;
      CLOCK_REALTIME : constant clockid_t :=
         System.OS_Constants.CLOCK_REALTIME;

      function clock_gettime
        (clock_id : clockid_t;
         tp       : access timespec) return int;
      pragma Import (C, clock_gettime, "clock_gettime");

   begin
      Result := clock_gettime (CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
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
