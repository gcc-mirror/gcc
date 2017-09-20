------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1998-2017, Free Software Foundation, Inc.         --
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

--  This package provides low level primitives used to implement clock and
--  delays in non tasking applications, for LynxOS.

--  The choice of the real clock/delay implementation (depending on whether
--  tasking is involved or not) is done via soft links (see s-soflin.ads)

--  NEVER add any dependency to tasking packages here

package System.OS_Primitives is
   pragma Preelaborate;

   Max_Sensible_Delay : constant Duration := 16#10_0000.0#;
   --  LynxOS does not support delays as long as half a year, so we set this to
   --  a shorter, but still fairly long, duration. Experiments show that if
   --  pthread_cond_timedwait is passed an abstime much greater than about
   --  2**21, it fails, returning EAGAIN. The cutoff is somewhere between
   --  16#20_8000.0# and 16#20_F000.0#. This behavior is not documented.

   procedure Initialize;
   --  Initialize global settings related to this package. This procedure
   --  should be called before any other subprograms in this package. Note
   --  that this procedure can be called several times.

   function Clock return Duration;
   pragma Inline (Clock);
   --  Returns "absolute" time, represented as an offset relative to "the
   --  Epoch", which is Jan 1, 1970 00:00:00 UTC on UNIX systems. This
   --  implementation is affected by system's clock changes.

   Relative          : constant := 0;
   Absolute_Calendar : constant := 1;
   Absolute_RT       : constant := 2;
   --  Values for Mode call below. Note that the compiler (exp_ch9.adb) relies
   --  on these values. So any change here must be reflected in corresponding
   --  changes in the compiler.

   procedure Timed_Delay (Time : Duration; Mode : Integer);
   --  Implements the semantics of the delay statement when no tasking is used
   --  in the application.
   --
   --    Mode is one of the three values above
   --
   --    Time is a relative or absolute duration value, depending on Mode.
   --
   --  Note that currently Ada.Real_Time always uses the tasking run time,
   --  so this procedure should never be called with Mode set to Absolute_RT.
   --  This may change in future or bare board implementations.

end System.OS_Primitives;
