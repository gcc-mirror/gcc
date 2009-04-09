------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  SYSTEM.TASK_PRIMITIVES.OPERATIONS.DEC                   --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2000-2009, Free Software Foundation, Inc.         --
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

--   This package is for OpenVMS/Alpha

with System.OS_Interface;
with System.Parameters;
with System.Tasking;
with Ada.Unchecked_Conversion;
with System.Soft_Links;

package body System.Task_Primitives.Operations.DEC is

   use System.OS_Interface;
   use System.Parameters;
   use System.Tasking;
   use System.Aux_DEC;
   use type Interfaces.C.int;

   package SSL renames System.Soft_Links;

   --  The FAB_RAB_Type specifies where the context field (the calling
   --  task) is stored.  Other fields defined for FAB_RAB arent' need and
   --  so are ignored.

   type FAB_RAB_Type is record
      CTX : Unsigned_Longword;
   end record;

   for FAB_RAB_Type use record
      CTX at 24 range 0 .. 31;
   end record;

   for FAB_RAB_Type'Size use 224;

   type FAB_RAB_Access_Type is access all FAB_RAB_Type;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Unsigned_Longword is new
     Ada.Unchecked_Conversion (Task_Id, Unsigned_Longword);

   function To_Task_Id is new
     Ada.Unchecked_Conversion (Unsigned_Longword, Task_Id);

   function To_FAB_RAB is new
     Ada.Unchecked_Conversion (Address, FAB_RAB_Access_Type);

   ---------------------------
   -- Interrupt_AST_Handler --
   ---------------------------

   procedure Interrupt_AST_Handler (ID : Address) is
      Result      : Interfaces.C.int;
      AST_Self_ID : constant Task_Id := To_Task_Id (ID);
   begin
      Result := pthread_cond_signal_int_np (AST_Self_ID.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end Interrupt_AST_Handler;

   ---------------------
   -- RMS_AST_Handler --
   ---------------------

   procedure RMS_AST_Handler (ID : Address) is
      AST_Self_ID : constant Task_Id := To_Task_Id (To_FAB_RAB (ID).CTX);
      Result      : Interfaces.C.int;

   begin
      AST_Self_ID.Common.LL.AST_Pending := False;
      Result := pthread_cond_signal_int_np (AST_Self_ID.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end RMS_AST_Handler;

   ----------
   -- Self --
   ----------

   function Self return Unsigned_Longword is
      Self_ID : constant Task_Id := Self;
   begin
      Self_ID.Common.LL.AST_Pending := True;
      return To_Unsigned_Longword (Self);
   end Self;

   -------------------------
   -- Starlet_AST_Handler --
   -------------------------

   procedure Starlet_AST_Handler (ID : Address) is
      Result      : Interfaces.C.int;
      AST_Self_ID : constant Task_Id := To_Task_Id (ID);
   begin
      AST_Self_ID.Common.LL.AST_Pending := False;
      Result := pthread_cond_signal_int_np (AST_Self_ID.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end Starlet_AST_Handler;

   ----------------
   -- Task_Synch --
   ----------------

   procedure Task_Synch is
      Synch_Self_ID : constant Task_Id := Self;

   begin
      if Single_Lock then
         Lock_RTS;
      else
         Write_Lock (Synch_Self_ID);
      end if;

      SSL.Abort_Defer.all;
      Synch_Self_ID.Common.State := AST_Server_Sleep;

      while Synch_Self_ID.Common.LL.AST_Pending loop
         Sleep (Synch_Self_ID, AST_Server_Sleep);
      end loop;

      Synch_Self_ID.Common.State := Runnable;

      if Single_Lock then
         Unlock_RTS;
      else
         Unlock (Synch_Self_ID);
      end if;

      SSL.Abort_Undefer.all;
   end Task_Synch;

end System.Task_Primitives.Operations.DEC;
