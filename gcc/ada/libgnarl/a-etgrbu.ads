------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . E X E C U T I O N _ T I M E . G R O U P _ B U D G E T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2015-2020, Free Software Foundation, Inc.       --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This unit is not implemented in typical GNAT implementations that lie on
--  top of operating systems, because it is infeasible to implement in such
--  environments.

--  If a target environment provides appropriate support for this package,
--  then the Unimplemented_Unit pragma should be removed from this spec and
--  an appropriate body provided.

with System;
with System.Multiprocessors;

package Ada.Execution_Time.Group_Budgets is
   pragma Unimplemented_Unit;

   type Group_Budget
     (CPU : System.Multiprocessors.CPU := System.Multiprocessors.CPU'First)
   is tagged limited private;

   type Group_Budget_Handler is access
      protected procedure (GB : in out Group_Budget);

   type Task_Array is
      array (Positive range <>) of Ada.Task_Identification.Task_Id;

   Min_Handler_Ceiling : constant System.Any_Priority :=
                           System.Any_Priority'First;
   --  Initial value is an arbitrary choice ???

   procedure Add_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   procedure Remove_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   function Is_Member
     (GB : Group_Budget;
      T  : Ada.Task_Identification.Task_Id) return Boolean;

   function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean;

   function Members (GB : Group_Budget) return Task_Array;

   procedure Replenish
     (GB : in out Group_Budget;
      To : Ada.Real_Time.Time_Span);

   procedure Add
     (GB       : in out Group_Budget;
      Interval : Ada.Real_Time.Time_Span);

   function Budget_Has_Expired (GB : Group_Budget) return Boolean;

   function Budget_Remaining
     (GB : Group_Budget) return Ada.Real_Time.Time_Span;

   procedure Set_Handler
     (GB      : in out Group_Budget;
      Handler : Group_Budget_Handler);

   function Current_Handler (GB : Group_Budget) return Group_Budget_Handler;

   procedure Cancel_Handler
     (GB        : in out Group_Budget;
      Cancelled : out Boolean);

   Group_Budget_Error : exception;

private
   type Group_Budget
     (CPU : System.Multiprocessors.CPU := System.Multiprocessors.CPU'First)
   is tagged limited null record;
end Ada.Execution_Time.Group_Budgets;
