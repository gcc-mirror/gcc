------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                      Copyright (C) 1995-2005 AdaCore                     --
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
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This version is for systems that do not support interrupts (or signals)

with Ada.Exceptions;

package body System.Interrupts is

   pragma Warnings (Off); -- kill warnings on unreferenced formals

   use System.Tasking;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Unimplemented;
   --  This procedure raises a Program_Error with an appropriate message
   --  indicating that an unimplemented feature has been used.

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False)
   is
   begin
      Unimplemented;
   end Attach_Handler;

   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   procedure Bind_Interrupt_To_Entry
     (T       : Task_Id;
      E       : Task_Entry_Index;
      Int_Ref : System.Address)
   is
   begin
      Unimplemented;
   end Bind_Interrupt_To_Entry;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented;
   end Block_Interrupt;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Interrupt : Interrupt_ID) return Parameterless_Handler
   is
   begin
      Unimplemented;
      return null;
   end Current_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler
     (Interrupt : Interrupt_ID;
      Static    : Boolean := False)
   is
   begin
      Unimplemented;
   end Detach_Handler;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Task_Id) is
   begin
      Unimplemented;
   end Detach_Interrupt_Entries;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False)
   is
   begin
      Old_Handler := null;
      Unimplemented;
   end Exchange_Handler;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Static_Interrupt_Protection) is
   begin
      Unimplemented;
   end Finalize;

   -------------------------------------
   -- Has_Interrupt_Or_Attach_Handler --
   -------------------------------------

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Dynamic_Interrupt_Protection)
      return   Boolean
   is
      pragma Warnings (Off, Object);
   begin
      Unimplemented;
      return True;
   end Has_Interrupt_Or_Attach_Handler;

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Static_Interrupt_Protection)
      return   Boolean
   is
      pragma Warnings (Off, Object);
   begin
      Unimplemented;
      return True;
   end Has_Interrupt_Or_Attach_Handler;

   ----------------------
   -- Ignore_Interrupt --
   ----------------------

   procedure Ignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented;
   end Ignore_Interrupt;

   ----------------------
   -- Install_Handlers --
   ----------------------

   procedure Install_Handlers
     (Object       : access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array)
   is
   begin
      Unimplemented;
   end Install_Handlers;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented;
      return True;
   end Is_Blocked;

   -----------------------
   -- Is_Entry_Attached --
   -----------------------

   function Is_Entry_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented;
      return True;
   end Is_Entry_Attached;

   -------------------------
   -- Is_Handler_Attached --
   -------------------------

   function Is_Handler_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented;
      return True;
   end Is_Handler_Attached;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented;
      return True;
   end Is_Ignored;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented;
      return True;
   end Is_Reserved;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address is
   begin
      Unimplemented;
      return Interrupt'Address;
   end Reference;

   --------------------------------
   -- Register_Interrupt_Handler --
   --------------------------------

   procedure Register_Interrupt_Handler
     (Handler_Addr : System.Address)
   is
   begin
      Unimplemented;
   end Register_Interrupt_Handler;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented;
   end Unblock_Interrupt;

   ------------------
   -- Unblocked_By --
   ------------------

   function Unblocked_By (Interrupt : Interrupt_ID)
     return System.Tasking.Task_Id is
   begin
      Unimplemented;
      return null;
   end Unblocked_By;

   ------------------------
   -- Unignore_Interrupt --
   ------------------------

   procedure Unignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented;
   end Unignore_Interrupt;

   -------------------
   -- Unimplemented; --
   -------------------

   procedure Unimplemented is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity, "interrupts/signals not implemented");
      raise Program_Error;
   end Unimplemented;

end System.Interrupts;
