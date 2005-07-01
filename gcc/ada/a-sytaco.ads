------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
--  Allow withing of non-Preelaborated units in Ada 2005 mode where this
--  package will be implicitly categorized as Preelaborate. See AI-362 for
--  details. It is safe in the context of the run-time to violate the rules!

with System.Task_Primitives;
--  Used for Suspension_Object

with Ada.Finalization;
--  Used for Limited_Controlled

pragma Warnings (On);

package Ada.Synchronous_Task_Control is
pragma Preelaborate_05 (Synchronous_Task_Control);
--  In accordance with Ada 2005 AI-362

   type Suspension_Object is limited private;

   procedure Set_True (S : in out Suspension_Object);

   procedure Set_False (S : in out Suspension_Object);

   function Current_State (S : Suspension_Object) return Boolean;

   procedure Suspend_Until_True (S : in out Suspension_Object);

private

   procedure Initialize (S : in out Suspension_Object);
   --  Initialization for Suspension_Object

   procedure Finalize (S : in out Suspension_Object);
   --  Finalization for Suspension_Object

   type Suspension_Object is
     new Ada.Finalization.Limited_Controlled with record
      SO : System.Task_Primitives.Suspension_Object;
      --  Use low-level suspension objects so that the synchronization
      --  functionality provided by this object can be achieved using
      --  efficient operating system primitives.
     end record;

   pragma Inline (Set_True);
   pragma Inline (Set_False);
   pragma Inline (Current_State);
   pragma Inline (Suspend_Until_True);
   pragma Inline (Initialize);
   pragma Inline (Finalize);

end Ada.Synchronous_Task_Control;
