------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                      I N T E R F A C E S . V X W O R K S                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--             Copyright (C) 1999-2004 Ada Core Technologies, Inc.          --
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

--  This is the x86 VxWorks version of this package

--  This package provides a limited binding to the VxWorks API
--  In particular, it interfaces with the VxWorks hardware interrupt
--  facilities, allowing the use of low-latency direct-vectored
--  interrupt handlers. Note that such handlers have a variety of
--  restrictions regarding system calls and language constructs. In particular,
--  the use of exception handlers and functions returning variable-length
--  objects cannot be used. Less restrictive, but higher-latency handlers can
--  be written using Ada protected procedures, Ada 83 style interrupt entries,
--  or by signalling an Ada task from within an interrupt handler using a
--  binary semaphore as described in the VxWorks Programmer's Manual.
--
--  For complete documentation of the operations in this package, please
--  consult the VxWorks Programmer's Manual and VxWorks Reference Manual.

with System.VxWorks;

package Interfaces.VxWorks is
   pragma Preelaborate (VxWorks);

   ------------------------------------------------------------------------
   --  Here is a complete example that shows how to handle the Interrupt 0x33
   --  with a direct-vectored interrupt handler in Ada using this package:

   --  with Interfaces.VxWorks; use Interfaces.VxWorks;
   --  with System;
   --
   --  package P is
   --
   --     Count : Integer;
   --     pragma Atomic (Count);
   --
   --     procedure Handler (Parameter : System.Address);
   --
   --  end P;
   --
   --  package body P is
   --
   --     procedure Handler (Parameter : System.Address) is
   --     begin
   --        Count := Count + 1;
   --        logMsg ("received an interrupt" & ASCII.LF & ASCII.Nul);
   --     end Handler;
   --  end P;
   --
   --  with Interfaces.VxWorks; use Interfaces.VxWorks;
   --  with Ada.Text_IO; use Ada.Text_IO;
   --  with Ada.Interrupts;
   --  with Machine_Code; use Machine_Code;
   --
   --  with P; use P;
   --  procedure Useint is
   --     --  Be sure to use a reasonable interrupt number for the target
   --     --  board!
   --     --  This one is an unreserved interrupt for the Pentium 3 BSP
   --     Interrupt : constant := 16#33#;
   --
   --     task T;
   --
   --     S : STATUS;
   --
   --     task body T is
   --     begin
   --        loop
   --           Put_Line ("Generating an interrupt...");
   --           delay 1.0;
   --
   --           --  Generate interrupt, using interrupt number
   --           Asm ("int %0",
   --                Inputs =>
   --                  Ada.Interrupts.Interrupt_ID'Asm_Input
   --                    ("i", Interrupt));
   --        end loop;
   --     end T;
   --
   --  begin
   --     S := intConnect (INUM_TO_IVEC (Interrupt), Handler'Access);
   --
   --     loop
   --        delay 2.0;
   --        Put_Line ("value of count:" & P.Count'Img);
   --     end loop;
   --  end Useint;
   -------------------------------------

   subtype int is Integer;

   type STATUS is new int;
   --  Equivalent of the C type STATUS

   OK    : constant STATUS := 0;
   ERROR : constant STATUS := -1;

   type VOIDFUNCPTR is access procedure (parameter : System.Address);
   type Interrupt_Vector is new System.Address;
   type Exception_Vector is new System.Address;

   function intConnect
     (vector    : Interrupt_Vector;
      handler   : VOIDFUNCPTR;
      parameter : System.Address := System.Null_Address) return STATUS;
   --  Binding to the C routine intConnect. Use this to set up an
   --  user handler. The routine generates a wrapper around the user
   --  handler to save and restore context

   function intVecGet
     (Vector : Interrupt_Vector) return VOIDFUNCPTR;
   --  Binding to the C routine intVecGet. Use this to get the
   --  existing handler for later restoral

   procedure intVecSet
     (Vector  : Interrupt_Vector;
      Handler : VOIDFUNCPTR);
   --  Binding to the C routine intVecSet. Use this to restore a
   --  handler obtained using intVecGet

   procedure intVecGet2
     (vector       : Interrupt_Vector;
      pFunction    : out VOIDFUNCPTR;
      pIdtGate     : access int;
      pIdtSelector : access int);
   --  Binding to the C routine intVecGet2. Use this to get the
   --  existing handler for later restoral

   procedure intVecSet2
     (vector       : Interrupt_Vector;
      pFunction    : VOIDFUNCPTR;
      pIdtGate     : access int;
      pIdtSelector : access int);
   --  Binding to the C routine intVecSet2. Use this to restore a
   --  handler obtained using intVecGet2

   function INUM_TO_IVEC (intNum : int) return Interrupt_Vector;
   --  Equivalent to the C macro INUM_TO_IVEC used to convert an interrupt
   --  number to an interrupt vector

   procedure logMsg
     (fmt : String; arg1, arg2, arg3, arg4, arg5, arg6 : int := 0);
   --  Binding to the C routine logMsg. Note that it is the caller's
   --  responsibility to ensure that fmt is a null-terminated string
   --  (e.g logMsg ("Interrupt" & ASCII.NUL))

   type FP_CONTEXT is private;
   --  Floating point context save and restore. Handlers using floating
   --  point must be bracketed with these calls. The pFpContext parameter
   --  should be an object of type FP_CONTEXT that is
   --  declared local to the handler.
   --  See the VxWorks Intel Architecture Supplement regarding
   --  these routines.

   procedure fppRestore (pFpContext : in out FP_CONTEXT);
   --  Restore floating point context - old style

   procedure fppSave (pFpContext : in out FP_CONTEXT);
   --  Save floating point context - old style

   procedure fppXrestore (pFpContext : in out FP_CONTEXT);
   --  Restore floating point context - new style

   procedure fppXsave (pFpContext : in out FP_CONTEXT);
   --  Save floating point context - new style

private

   type FP_CONTEXT is new System.VxWorks.FP_CONTEXT;
   --  Target-dependent floating point context type

   pragma Import (C, intConnect, "intConnect");
   pragma Import (C, intVecGet, "intVecGet");
   pragma Import (C, intVecSet, "intVecSet");
   pragma Import (C, intVecGet2, "intVecGet2");
   pragma Import (C, intVecSet2, "intVecSet2");
   pragma Import (C, INUM_TO_IVEC, "__gnat_inum_to_ivec");
   pragma Import (C, logMsg, "logMsg");
   pragma Import (C, fppRestore, "fppRestore");
   pragma Import (C, fppSave, "fppSave");
   pragma Import (C, fppXrestore, "fppXrestore");
   pragma Import (C, fppXsave, "fppXsave");
end Interfaces.VxWorks;
