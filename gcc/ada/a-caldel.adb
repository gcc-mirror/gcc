------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2006, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with System.OS_Primitives;
--  Used for Delay_Modes
--           Max_Sensible_Delay

with System.Soft_Links;
--  Used for Timed_Delay

with System.Traces;
--  Used for Send_Trace_Info

with System.Parameters;
--  used for Runtime_Traces

package body Ada.Calendar.Delays is

   package OSP renames System.OS_Primitives;
   package SSL renames System.Soft_Links;

   use type SSL.Timed_Delay_Call;

   use System.Traces;

   --  Earlier, System.Time_Opeations was used to implement the following
   --  operations. The idea was to avoid sucking in the tasking packages. This
   --  did not work. Logically, we can't have it both ways. There is no way to
   --  implement time delays that will have correct task semantics without
   --  reference to the tasking run-time system. To achieve this goal, we now
   --  use soft links.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Timed_Delay_NT (Time : Duration; Mode : Integer);
   --  Timed delay procedure used when no tasking is active

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (D : Duration) is
   begin
      if System.Parameters.Runtime_Traces then
         Send_Trace_Info (W_Delay, D);
      end if;

      SSL.Timed_Delay.all (Duration'Min (D, OSP.Max_Sensible_Delay),
                           OSP.Relative);

      if System.Parameters.Runtime_Traces then
         Send_Trace_Info (M_Delay, D);
      end if;
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      D : constant Duration := To_Duration (T);

   begin
      if System.Parameters.Runtime_Traces then
         Send_Trace_Info (WU_Delay, D);
      end if;

      SSL.Timed_Delay.all (D, OSP.Absolute_Calendar);

      if System.Parameters.Runtime_Traces then
         Send_Trace_Info (M_Delay, D);
      end if;
   end Delay_Until;

   --------------------
   -- Timed_Delay_NT --
   --------------------

   procedure Timed_Delay_NT (Time : Duration; Mode : Integer) is
   begin
      OSP.Timed_Delay (Time, Mode);
   end Timed_Delay_NT;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
   begin
      --  Since time has multiple representations on different platforms, a
      --  target independent operation in Ada.Calendar is used to perform
      --  this conversion.

      return Delays_Operations.To_Duration (T);
   end To_Duration;

begin
   --  Set up the Timed_Delay soft link to the non tasking version if it has
   --  not been already set.

   --  If tasking is present, Timed_Delay has already set this soft link, or
   --  this will be overriden during the elaboration of
   --  System.Tasking.Initialization

   if SSL.Timed_Delay = null then
      SSL.Timed_Delay := Timed_Delay_NT'Access;
   end if;

end Ada.Calendar.Delays;
