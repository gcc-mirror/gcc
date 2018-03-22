------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . R E A L _ T I M E . D E L A Y S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2018, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with System.Tasking;
with System.Task_Primitives.Operations;

package body Ada.Real_Time.Delays is

   package STPO renames System.Task_Primitives.Operations;

   ----------------
   -- Local Data --
   ----------------

   Absolute_RT : constant := 2;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Self_Id : constant System.Tasking.Task_Id := STPO.Self;

   begin
      --  If pragma Detect_Blocking is active, Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if System.Tasking.Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      else
         STPO.Timed_Delay (Self_Id, To_Duration (T), Absolute_RT);
      end if;
   end Delay_Until;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
   begin
      return To_Duration (Time_Span (T));
   end To_Duration;

end Ada.Real_Time.Delays;
