------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                      I N T E R F A C E S . V X W O R K S                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                     Copyright (C) 1999-2013, AdaCore                     --
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

--  This package provides a limited binding to the VxWorks API

--  In particular, it interfaces with the VxWorks hardware interrupt
--  facilities, allowing the use of low-latency direct-vectored interrupt
--  handlers. Note that such handlers have a variety of restrictions regarding
--  system calls and language constructs. In particular, the use of exception
--  handlers and functions returning variable-length objects cannot be used.
--  Less restrictive, but higher-latency handlers can be written using Ada
--  protected procedures, Ada 83 style interrupt entries, or by signalling
--  an Ada task from within an interrupt handler using a binary semaphore
--  as described in the VxWorks Programmer's Manual.
--
--  For complete documentation of the operations in this package, please
--  consult the VxWorks Programmer's Manual and VxWorks Reference Manual.

pragma Warnings (Off, "*foreign convention*");
pragma Warnings (Off, "*add Convention pragma*");
--  These are temporary pragmas to suppress warnings about mismatching
--  conventions, which will be a problem when we get rid of trampolines ???

with System.VxWorks;

package Interfaces.VxWorks is
   pragma Preelaborate;

   ------------------------------------------------------------------------
   --  Here is a complete example that shows how to handle the Interrupt 0x14
   --  with a direct-vectored interrupt handler in Ada using this package:

   --  with Interfaces.VxWorks; use Interfaces.VxWorks;
   --  with System;
   --
   --  package P is
   --
   --     Count : Integer;
   --     pragma Atomic (Count);
   --
   --     Level : constant := 1;
   --     --  Interrupt level used by this example
   --
   --     procedure Handler (parameter : System.Address);
   --
   --  end P;
   --
   --  package body P is
   --
   --     procedure Handler (parameter : System.Address) is
   --        S : STATUS;
   --     begin
   --        Count := Count + 1;
   --        logMsg ("received an interrupt" & ASCII.LF & ASCII.NUL);
   --
   --        --  Acknowledge VME interrupt
   --
   --        S := sysBusIntAck (intLevel => Level);
   --     end Handler;
   --  end P;
   --
   --  with Interfaces.VxWorks; use Interfaces.VxWorks;
   --  with Ada.Text_IO; use Ada.Text_IO;
   --
   --  with P; use P;
   --  procedure Useint is
   --
   --     --  Be sure to use a reasonable interrupt number for  board.
   --     --  This one is the unused VME graphics interrupt on the PPC MV2604
   --
   --     Interrupt : constant := 16#14#;
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
   --           --  Generate VME interrupt, using interrupt number
   --
   --           S := sysBusIntGen (1, Interrupt);
   --        end loop;
   --     end T;
   --
   --  begin
   --     S := sysIntEnable (intLevel => Level);
   --     S := intConnect (INUM_TO_IVEC (Interrupt), handler'Access);
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
   --  Binding to the C routine intConnect. Use this to set up an user handler.
   --  The routine generates a wrapper around the user handler to save and
   --  restore context

   function intContext return int;
   --  Binding to the C routine intContext. This function returns 1 only if the
   --  current execution state is in interrupt context.

   function intVecGet
     (Vector : Interrupt_Vector) return VOIDFUNCPTR;
   --  Binding to the C routine intVecGet. Use this to get the existing handler
   --  for later restoral

   procedure intVecSet
     (Vector  : Interrupt_Vector;
      Handler : VOIDFUNCPTR);
   --  Binding to the C routine intVecSet. Use this to restore a handler
   --  obtained using intVecGet

   function INUM_TO_IVEC (intNum : int) return Interrupt_Vector;
   --  Equivalent to the C macro INUM_TO_IVEC used to convert an interrupt
   --  number to an interrupt vector

   function sysIntEnable (intLevel : int) return STATUS;
   --  Binding to the C routine sysIntEnable

   function sysIntDisable (intLevel : int) return STATUS;
   --  Binding to the C routine sysIntDisable

   function sysBusIntAck (intLevel : int) return STATUS;
   --  Binding to the C routine sysBusIntAck

   function sysBusIntGen (intLevel : int; Intnum : int) return STATUS;
   --  Binding to the C routine sysBusIntGen. Note that the T2 documentation
   --  implies that a vector address is the proper argument - it's not. The
   --  interrupt number in the range 0 .. 255 (for 68K and PPC) is the correct
   --  argument.

   procedure logMsg
     (fmt : String; arg1, arg2, arg3, arg4, arg5, arg6 : int := 0);
   --  Binding to the C routine logMsg. Note that it is the caller's
   --  responsibility to ensure that fmt is a null-terminated string
   --  (e.g logMsg ("Interrupt" & ASCII.NUL))

   type FP_CONTEXT is private;
   --  Floating point context save and restore. Handlers using floating point
   --  must be bracketed with these calls. The pFpContext parameter should be
   --  an object of type FP_CONTEXT that is declared local to the handler.

   procedure fppRestore (pFpContext : in out FP_CONTEXT);
   --  Restore floating point context

   procedure fppSave (pFpContext : in out FP_CONTEXT);
   --  Save floating point context

private

   type FP_CONTEXT is new System.VxWorks.FP_CONTEXT;
   --  Target-dependent floating point context type

   pragma Import (C, intConnect, "intConnect");
   pragma Import (C, intContext, "intContext");
   pragma Import (C, intVecGet, "intVecGet");
   pragma Import (C, intVecSet, "intVecSet");
   pragma Import (C, INUM_TO_IVEC, "__gnat_inum_to_ivec");
   pragma Import (C, sysIntEnable, "sysIntEnable");
   pragma Import (C, sysIntDisable, "sysIntDisable");
   pragma Import (C, sysBusIntAck, "sysBusIntAck");
   pragma Import (C, sysBusIntGen, "sysBusIntGen");
   pragma Import (C, logMsg, "logMsg");
   pragma Import (C, fppRestore, "fppRestore");
   pragma Import (C, fppSave, "fppSave");
end Interfaces.VxWorks;
