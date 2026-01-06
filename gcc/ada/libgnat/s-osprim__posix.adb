------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2026, Free Software Foundation, Inc.         --
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

with System.C_Time;

package body System.OS_Primitives is

   -----------
   -- Clock --
   -----------

   function Clock return Duration is

      TV     : aliased C_Time.timeval;
      Result : Integer;
      pragma Unreferenced (Result);

      function gettimeofday
        (Tv : access C_Time.timeval;
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
      return C_Time.To_Duration (TV);
   end Clock;

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
