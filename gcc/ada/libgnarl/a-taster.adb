------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . T A S K _ T E R M I N A T I O N                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2018, Free Software Foundation, Inc.         --
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

with System.Tasking;
with System.Task_Primitives.Operations;
with System.Parameters;
with System.Soft_Links;

with Ada.Unchecked_Conversion;

package body Ada.Task_Termination is

   use type Ada.Task_Identification.Task_Id;

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;

   use System.Parameters;

   -----------------------
   -- Local subprograms --
   -----------------------

   function To_TT is new Ada.Unchecked_Conversion
     (System.Tasking.Termination_Handler, Termination_Handler);

   function To_ST is new Ada.Unchecked_Conversion
     (Termination_Handler, System.Tasking.Termination_Handler);

   function To_Task_Id is new Ada.Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);

   -----------------------------------
   -- Current_Task_Fallback_Handler --
   -----------------------------------

   function Current_Task_Fallback_Handler return Termination_Handler is
   begin
      --  There is no need for explicit protection against race conditions
      --  for this function because this function can only be executed by
      --  Self, and the Fall_Back_Handler can only be modified by Self.

      return To_TT (STPO.Self.Common.Fall_Back_Handler);
   end Current_Task_Fallback_Handler;

   -------------------------------------
   -- Set_Dependents_Fallback_Handler --
   -------------------------------------

   procedure Set_Dependents_Fallback_Handler
     (Handler : Termination_Handler)
   is
      Self : constant System.Tasking.Task_Id := STPO.Self;

   begin
      SSL.Abort_Defer.all;

      if Single_Lock then
         STPO.Lock_RTS;
      end if;

      STPO.Write_Lock (Self);

      Self.Common.Fall_Back_Handler := To_ST (Handler);

      STPO.Unlock (Self);

      if Single_Lock then
         STPO.Unlock_RTS;
      end if;

      SSL.Abort_Undefer.all;
   end Set_Dependents_Fallback_Handler;

   --------------------------
   -- Set_Specific_Handler --
   --------------------------

   procedure Set_Specific_Handler
     (T       : Ada.Task_Identification.Task_Id;
      Handler : Termination_Handler)
   is
   begin
      --  Tasking_Error is raised if the task identified by T has already
      --  terminated. Program_Error is raised if the value of T is
      --  Null_Task_Id.

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      elsif Ada.Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      else
         declare
            Target : constant System.Tasking.Task_Id := To_Task_Id (T);

         begin
            SSL.Abort_Defer.all;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Target);

            Target.Common.Specific_Handler := To_ST (Handler);

            STPO.Unlock (Target);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;

            SSL.Abort_Undefer.all;
         end;
      end if;
   end Set_Specific_Handler;

   ----------------------
   -- Specific_Handler --
   ----------------------

   function Specific_Handler
     (T : Ada.Task_Identification.Task_Id) return Termination_Handler
   is
   begin
      --  Tasking_Error is raised if the task identified by T has already
      --  terminated. Program_Error is raised if the value of T is
      --  Null_Task_Id.

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      elsif Ada.Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      else
         declare
            Target : constant System.Tasking.Task_Id := To_Task_Id (T);
            TH     : Termination_Handler;

         begin
            SSL.Abort_Defer.all;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Target);

            TH := To_TT (Target.Common.Specific_Handler);

            STPO.Unlock (Target);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;

            SSL.Abort_Undefer.all;

            return TH;
         end;
      end if;
   end Specific_Handler;

end Ada.Task_Termination;
