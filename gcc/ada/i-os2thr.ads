------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             I N T E R F A C E S . O S 2 L I B . T H R E A D S            --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1993-2005 Free Software Foundation, Inc.          --
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

with Interfaces.C;

package Interfaces.OS2Lib.Threads is
pragma Preelaborate (Threads);

   package IC renames Interfaces.C;

   type PID is new IC.unsigned_long;
   type PPID is access all PID;
   --  Process ID, and pointer to process ID

   type TID is new IC.unsigned_long;
   type PTID is access all TID;
   --  Thread ID, and pointer to thread ID

   -------------------------------------------------------------
   -- Thread Creation, Activation, Suspension And Termination --
   -------------------------------------------------------------

   --  Note: <bsedos.h> defines the "Informations" and "param" parameter below
   --  as a ULONG, but everyone knows that in general an address will be passed
   --  to it. We declared it here with type PVOID (which it should have had)
   --  because Ada is a bit more sensitive to mixing integers and addresses.

   type PFNTHREAD is access procedure (Informations : System.Address);
   --  TBSL should use PVOID instead of Address as per above node ???

   function DosCreateThread
     (F_ptid  : PTID;
      pfn     : PFNTHREAD;
      param   : PVOID;
      flag    : ULONG;
      cbStack : ULONG) return APIRET;
   pragma Import (C, DosCreateThread, "DosCreateThread");

   Block_Child     : constant := 1;
   No_Block_Child  : constant := 0;
   Commit_Stack    : constant := 2;
   No_Commit_Stack : constant := 0;
   --  Values for "flag" parameter in DosCreateThread call

   procedure DosExit (Action : ULONG; Result : ULONG);
   pragma Import (C, DosExit, "DosExit");

   EXIT_THREAD  : constant := 0;
   EXIT_PROCESS : constant := 1;
   --  Values for "Action" parameter in Dos_Exit call

   function DosResumeThread (Id : TID) return APIRET;
   pragma Import (C, DosResumeThread, "DosResumeThread");

   function DosSuspendThread (Id : TID) return APIRET;
   pragma Import (C, DosSuspendThread, "DosSuspendThread");

   procedure DosWaitThread (Thread_Ptr : PTID; Option : ULONG);
   pragma Import (C, DosWaitThread, "DosWaitThread");

   function DosKillThread (Id : TID) return APIRET;
   pragma Import (C, DosKillThread, "DosKillThread");

   DCWW_WAIT   : constant := 0;
   DCWW_NOWAIT : constant := 1;
   --  Values for "Option" parameter in DosWaitThread call

   ---------------------------------------------------
   -- Accessing properties of Threads and Processes --
   ---------------------------------------------------

   --  Structures translated from BSETIB.H

   --  Thread Information Block (TIB)
   --  Need documentation clarifying distinction between TIB, TIB2 ???

   --  GB970409: Changed TIB2 structure, because the tib2_ulprio field
   --            is not the actual priority but contains two byte fields
   --            that hold the priority class and rank respectively.
   --            A proper Ada style record with explicit representation
   --            avoids this kind of errors.

   type TIB2 is record
      Thread_ID           : TID;
      Prio_Rank           : UCHAR;
      Prio_Class          : UCHAR;
      Version             : ULONG;  -- Version number for this structure
      Must_Complete_Count : USHORT; -- Must Complete count
      Must_Complete_Force : USHORT; -- Must Complete force flag
   end record;

   type PTIB2 is access all TIB2;

   --  Thread Information Block (TIB)

   type TIB is record
      tib_pexchain      : PVOID;  -- Head of exception handler chain
      tib_pstack        : PVOID;  -- Pointer to base of stack
      tib_pstacklimit   : PVOID;  -- Pointer to end of stack
      System            : PTIB2;  -- Pointer to system specific TIB
      tib_version       : ULONG;  -- Version number for this TIB structure
      tib_ordinal       : ULONG;  -- Thread ordinal number
   end record;

   type PTIB is access all TIB;

   --  Process Information Block (PIB)

   type PIB is record
      pib_ulpid         : ULONG;   -- Process I.D.
      pib_ulppid        : ULONG;   -- Parent process I.D.
      pib_hmte          : ULONG;   -- Program (.EXE) module handle
      pib_pchcmd        : PCHAR;   -- Command line pointer
      pib_pchenv        : PCHAR;   -- Environment pointer
      pib_flstatus      : ULONG;   -- Process' status bits
      pib_ultype        : ULONG;   -- Process' type code
   end record;

   type PPIB is access all PIB;

   function DosGetInfoBlocks
     (Pptib : access PTIB;
      Pppib : access PPIB) return APIRET;
   pragma Import (C, DosGetInfoBlocks, "DosGetInfoBlocks");

   --  Thread local memory

   --  This function allocates a block of memory that is unique, or local, to
   --  a thread.

   function DosAllocThreadLocalMemory
     (cb : ULONG;               -- Number of 4-byte DWORDs to allocate
      p  : access PVOID)        -- Address of the memory block
      return APIRET;                   -- Return Code (rc)
   pragma Import
     (Convention => C,
      Entity     => DosAllocThreadLocalMemory,
      Link_Name  => "_DosAllocThreadLocalMemory");

   ----------------
   -- Priorities --
   ----------------

   function DosSetPriority
     (Scope   : ULONG;
      Class   : ULONG;
      Delta_P : IC.long;
      PorTid  : TID) return APIRET;
   pragma Import (C, DosSetPriority, "DosSetPriority");

   PRTYS_PROCESS     : constant := 0;
   PRTYS_PROCESSTREE : constant := 1;
   PRTYS_THREAD      : constant := 2;
   --  Values for "Scope" parameter in DosSetPriority call

   PRTYC_NOCHANGE         : constant := 0;
   PRTYC_IDLETIME         : constant := 1;
   PRTYC_REGULAR          : constant := 2;
   PRTYC_TIMECRITICAL     : constant := 3;
   PRTYC_FOREGROUNDSERVER : constant := 4;
   --  Values for "class" parameter in DosSetPriority call

end Interfaces.OS2Lib.Threads;
