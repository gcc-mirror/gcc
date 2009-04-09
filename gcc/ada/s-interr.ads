------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

--  This package encapsulates the implementation of interrupt or signal
--  handlers. It is logically an extension of the body of Ada.Interrupts. It
--  is made a child of System to allow visibility of various runtime system
--  internal data and operations.

--  See System.Interrupt_Management for core interrupt/signal interfaces

--  These two packages are separated to allow System.Interrupt_Management to be
--  used without requiring the whole tasking implementation to be linked and
--  elaborated.

with System.Tasking;
with System.Tasking.Protected_Objects.Entries;
with System.OS_Interface;

package System.Interrupts is

   pragma Elaborate_Body;
   --  Comment needed on why this is here ???

   -------------------------
   -- Constants and types --
   -------------------------

   Default_Interrupt_Priority : constant System.Interrupt_Priority :=
     System.Interrupt_Priority'Last;
   --  Default value used when a pragma Interrupt_Handler or Attach_Handler is
   --  specified without an Interrupt_Priority pragma, see D.3(10).

   type Ada_Interrupt_ID is range 0 .. System.OS_Interface.Max_Interrupt;
   --  Avoid inheritance by Ada.Interrupts.Interrupt_ID of unwanted operations

   type Interrupt_ID is range 0 .. System.OS_Interface.Max_Interrupt;

   subtype System_Interrupt_Id is Interrupt_ID;
   --  This synonym is introduced so that the type is accessible through
   --  rtsfind, otherwise the name clashes with its homonym in Ada.Interrupts.

   type Parameterless_Handler is access protected procedure;

   ----------------------
   -- General services --
   ----------------------

   --  Attempt to attach a Handler to an Interrupt to which an Entry is
   --  already bound will raise a Program_Error.

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean;

   function Is_Entry_Attached (Interrupt : Interrupt_ID) return Boolean;

   function Is_Handler_Attached (Interrupt : Interrupt_ID) return Boolean;

   function Current_Handler
     (Interrupt : Interrupt_ID) return Parameterless_Handler;

   --  Calling the following procedures with New_Handler = null and Static =
   --  true means that we want to modify the current handler regardless of the
   --  previous handler's binding status. (i.e. we do not care whether it is a
   --  dynamic or static handler)

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False);

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False);

   procedure Detach_Handler
     (Interrupt : Interrupt_ID;
      Static    : Boolean := False);

   function Reference
     (Interrupt : Interrupt_ID) return System.Address;

   --------------------------------
   -- Interrupt Entries Services --
   --------------------------------

   --  Routines needed for Interrupt Entries

   procedure Bind_Interrupt_To_Entry
     (T       : System.Tasking.Task_Id;
      E       : System.Tasking.Task_Entry_Index;
      Int_Ref : System.Address);
   --  Bind the given interrupt to the given entry. If the interrupt is
   --  already bound to another entry, Program_Error will be raised.

   procedure Detach_Interrupt_Entries (T : System.Tasking.Task_Id);
   --  This procedure detaches all the Interrupt Entries bound to a task

   ------------------------------
   -- POSIX.5 Signals Services --
   ------------------------------

   --  Routines needed for POSIX dot5 POSIX_Signals

   procedure Block_Interrupt (Interrupt : Interrupt_ID);
   --  Block the Interrupt on the process level

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID);

   function Unblocked_By
     (Interrupt : Interrupt_ID) return System.Tasking.Task_Id;
   --  It returns the ID of the last Task which Unblocked this Interrupt.
   --  It returns Null_Task if no tasks have ever requested the Unblocking
   --  operation or the Interrupt is currently Blocked.

   function Is_Blocked (Interrupt : Interrupt_ID) return Boolean;
   --  Comment needed ???

   procedure Ignore_Interrupt (Interrupt : Interrupt_ID);
   --  Set the sigaction for the interrupt to SIG_IGN

   procedure Unignore_Interrupt (Interrupt : Interrupt_ID);
   --  Comment needed ???

   function Is_Ignored (Interrupt : Interrupt_ID) return Boolean;
   --  Comment needed ???

   --  Note : Direct calls to sigaction, sigprocmask, thr_sigsetmask or any
   --  other low-level interface that changes the signal action or signal mask
   --  needs a careful thought.

   --  One may achieve the effect of system calls first making RTS blocked (by
   --  calling Block_Interrupt) for the signal under consideration. This will
   --  make all the tasks in RTS blocked for the Interrupt.

   ----------------------
   -- Protection Types --
   ----------------------

   --  Routines and types needed to implement Interrupt_Handler and
   --  Attach_Handler.

   --  There are two kinds of protected objects that deal with interrupts:

   --  (1) Only Interrupt_Handler pragmas are used. We need to be able to tell
   --  if an Interrupt_Handler applies to a given procedure, so
   --  Register_Interrupt_Handler has to be called for all the potential
   --  handlers, it should be done by calling Register_Interrupt_Handler with
   --  the handler code address. On finalization, which can happen only has
   --  part of library level finalization since PO with Interrupt_Handler
   --  pragmas can only be declared at library level, nothing special needs to
   --  be done since the default handlers have been restored as part of task
   --  completion which is done just before global finalization.
   --  Dynamic_Interrupt_Protection should be used in this case.

   --  (2) Attach_Handler pragmas are used, and possibly Interrupt_Handler
   --  pragma. We need to attach the handlers to the given interrupts when the
   --  object is elaborated. This should be done by constructing an array of
   --  pairs (interrupt, handler) from the pragmas and calling Install_Handlers
   --  with it (types to be used are New_Handler_Item and New_Handler_Array).
   --  On finalization, we need to restore the handlers that were installed
   --  before the elaboration of the PO, so we need to store these previous
   --  handlers. This is also done by Install_Handlers, the room for these
   --  informations is provided by adding a discriminant which is the number
   --  of Attach_Handler pragmas and an array of this size in the protection
   --  type, Static_Interrupt_Protection.

   procedure Register_Interrupt_Handler
     (Handler_Addr : System.Address);
   --  This routine should be called by the compiler to allow the handler be
   --  used as an Interrupt Handler. That means call this procedure for each
   --  pragma Interrupt_Handler providing the address of the handler (not
   --  including the pointer to the actual PO, this way this routine is called
   --  only once for each type definition of PO).

   type Static_Handler_Index is range 0 .. Integer'Last;
   subtype Positive_Static_Handler_Index is
     Static_Handler_Index range 1 .. Static_Handler_Index'Last;
   --  Comment needed ???

   type Previous_Handler_Item is record
      Interrupt : Interrupt_ID;
      Handler   : Parameterless_Handler;
      Static    : Boolean;
   end record;
   --  Contains all the information needed to restore a previous handler

   type Previous_Handler_Array is array
     (Positive_Static_Handler_Index range <>) of Previous_Handler_Item;

   type New_Handler_Item is record
      Interrupt : Interrupt_ID;
      Handler   : Parameterless_Handler;
   end record;
   --  Contains all the information from an Attach_Handler pragma

   type New_Handler_Array is
     array (Positive_Static_Handler_Index range <>) of New_Handler_Item;
   --  Comment needed ???

   --  Case (1)

   type Dynamic_Interrupt_Protection is new
     Tasking.Protected_Objects.Entries.Protection_Entries with null record;

   --  ??? Finalize is not overloaded since we currently have no
   --  way to detach the handlers during library level finalization.

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Dynamic_Interrupt_Protection) return Boolean;
   --  Returns True

   --  Case (2)

   type Static_Interrupt_Protection
     (Num_Entries        : Tasking.Protected_Objects.Protected_Entry_Index;
      Num_Attach_Handler : Static_Handler_Index)
   is new
     Tasking.Protected_Objects.Entries.Protection_Entries (Num_Entries) with
     record
       Previous_Handlers : Previous_Handler_Array (1 .. Num_Attach_Handler);
     end record;

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Static_Interrupt_Protection) return Boolean;
   --  Returns True

   procedure Finalize (Object : in out Static_Interrupt_Protection);
   --  Restore previous handlers as required by C.3.1(12) then call
   --  Finalize (Protection).

   procedure Install_Handlers
     (Object       : access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array);
   --  Store the old handlers in Object.Previous_Handlers and install
   --  the new static handlers.

   procedure Install_Restricted_Handlers (Handlers : New_Handler_Array);
   --  Install the static Handlers for the given interrupts and do not store
   --  previously installed handlers. This procedure is used when the Ravenscar
   --  restrictions are in place since in that case there are only
   --  library-level protected handlers that will be installed at
   --  initialization and never be replaced.

end System.Interrupts;
