------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1998-2001 Free Software Foundation, Inc.          --
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

--  This package provides low level primitives used to implement clock and
--  delays in non tasking applications.

--  The choice of the real clock/delay implementation (depending on whether
--  tasking is involved or not) is done via soft links (see s-tasoli.ads)

--  NEVER add any dependency to tasking packages here

package System.OS_Primitives is

   Max_Sensible_Delay : constant Duration := 183 * 24 * 60 * 60.0;
   --  Max of half a year delay, needed to prevent exceptions for large
   --  delay values. It seems unlikely that any test will notice this
   --  restriction, except in the case of applications setting the clock at
   --  at run time (see s-tastim.adb). Also note that a larger value might
   --  cause problems (e.g overflow, or more likely OS limitation in the
   --  primitives used).

   function Clock return Duration;
   pragma Inline (Clock);
   --  Returns "absolute" time, represented as an offset
   --  relative to "the Epoch", which is Jan 1, 1970 on unixes.
   --  This implementation is affected by system's clock changes.

   function Monotonic_Clock return Duration;
   pragma Inline (Monotonic_Clock);
   --  Returns "absolute" time, represented as an offset
   --  relative to "the Epoch", which is Jan 1, 1970.
   --  This clock implementation is immune to the system's clock changes.

   Relative          : constant := 0;
   Absolute_Calendar : constant := 1;
   Absolute_RT       : constant := 2;
   --  Values for Mode call below. Note that the compiler (exp_ch9.adb)
   --  relies on these values. So any change here must be reflected in
   --  corresponding changes in the compiler.

   procedure Timed_Delay (Time : Duration; Mode : Integer);
   --  Implements the semantics of the delay statement when no tasking is
   --  used in the application.
   --
   --    Mode is one of the three values above
   --
   --    Time is a relative or absolute duration value, depending on Mode.
   --
   --  Note that currently Ada.Real_Time always uses the tasking run time, so
   --  this procedure should never be called with Mode set to Absolute_RT.
   --  This may change in future or bare board implementations.

end System.OS_Primitives;
