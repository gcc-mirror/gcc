------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--                     Copyright (C) 1995-2021, AdaCore                     --
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

with System.OS_Primitives;
with System.Soft_Links;

package body Ada.Calendar.Delays is

   package OSP renames System.OS_Primitives;
   package SSL renames System.Soft_Links;

   use type SSL.Timed_Delay_Call;

   --  Earlier, System.Time_Operations was used to implement the following
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
      SSL.Timed_Delay.all (Duration'Min (D, OSP.Max_Sensible_Delay),
                           OSP.Relative);
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      D : constant Duration := To_Duration (T);

   begin
      SSL.Timed_Delay.all (D, OSP.Absolute_Calendar);
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

      return Delay_Operations.To_Duration (T);
   end To_Duration;

begin
   --  Set up the Timed_Delay soft link to the non tasking version if it has
   --  not been already set. If tasking is present, Timed_Delay has already set
   --  this soft link, or this will be overridden during the elaboration of
   --  System.Tasking.Initialization

   pragma Annotate (CodePeer, Modified, SSL.Timed_Delay);

   if SSL.Timed_Delay = null then
      SSL.Timed_Delay := Timed_Delay_NT'Access;
   end if;

end Ada.Calendar.Delays;
