------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a NT (native) version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl). For non tasking
--  oriented services consider declaring them into system-win32.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Ada.Unchecked_Conversion;

with Interfaces.C;
with Interfaces.C.Strings;
with System.Win32;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-mthreads");

   subtype int  is Interfaces.C.int;
   subtype long is Interfaces.C.long;

   -------------------
   -- General Types --
   -------------------

   subtype PSZ   is Interfaces.C.Strings.chars_ptr;

   Null_Void : constant Win32.PVOID := System.Null_Address;

   -------------------------
   -- Handles for objects --
   -------------------------

   subtype Thread_Id is Win32.HANDLE;

   -----------
   -- Errno --
   -----------

   NO_ERROR : constant := 0;
   FUNC_ERR : constant := -1;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 31;
   type Signal is new int range 0 .. Max_Interrupt;
   for Signal'Size use int'Size;

   SIGINT     : constant := 2; --  interrupt (Ctrl-C)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGFPE     : constant := 8; --  floating point exception
   SIGSEGV    : constant := 11; -- segmentation violation
   SIGTERM    : constant := 15; -- software termination signal from kill
   SIGBREAK   : constant := 21; -- break (Ctrl-Break)
   SIGABRT    : constant := 22; -- used by abort, replace SIGIOT in the future

   type sigset_t is private;

   type isr_address is access procedure (sig : int);
   pragma Convention (C, isr_address);

   function intr_attach (sig : int; handler : isr_address) return long;
   pragma Import (C, intr_attach, "signal");

   Intr_Attach_Reset : constant Boolean := True;
   --  True if intr_attach is reset after an interrupt handler is called

   procedure kill (sig : Signal);
   pragma Import (C, kill, "raise");

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   function Thread_Body_Access is new
     Ada.Unchecked_Conversion (System.Address, Thread_Body);

   procedure SwitchToThread;
   pragma Import (Stdcall, SwitchToThread, "SwitchToThread");

   function GetThreadTimes
     (hThread        : Win32.HANDLE;
      lpCreationTime : access Long_Long_Integer;
      lpExitTime     : access Long_Long_Integer;
      lpKernelTime   : access Long_Long_Integer;
      lpUserTime     : access Long_Long_Integer) return Win32.BOOL;
   pragma Import (Stdcall, GetThreadTimes, "GetThreadTimes");

   -----------------------
   -- Critical sections --
   -----------------------

   type CRITICAL_SECTION is private;

   -------------------------------------------------------------
   -- Thread Creation, Activation, Suspension And Termination --
   -------------------------------------------------------------

   type PTHREAD_START_ROUTINE is access function
     (pThreadParameter : Win32.PVOID) return Win32.DWORD;
   pragma Convention (Stdcall, PTHREAD_START_ROUTINE);

   function To_PTHREAD_START_ROUTINE is new
     Ada.Unchecked_Conversion (System.Address, PTHREAD_START_ROUTINE);

   function CreateThread
     (pThreadAttributes : access Win32.SECURITY_ATTRIBUTES;
      dwStackSize       : Win32.DWORD;
      pStartAddress     : PTHREAD_START_ROUTINE;
      pParameter        : Win32.PVOID;
      dwCreationFlags   : Win32.DWORD;
      pThreadId         : access Win32.DWORD) return Win32.HANDLE;
   pragma Import (Stdcall, CreateThread, "CreateThread");

   function BeginThreadEx
     (pThreadAttributes : access Win32.SECURITY_ATTRIBUTES;
      dwStackSize       : Win32.DWORD;
      pStartAddress     : PTHREAD_START_ROUTINE;
      pParameter        : Win32.PVOID;
      dwCreationFlags   : Win32.DWORD;
      pThreadId         : not null access Win32.DWORD) return Win32.HANDLE;
   pragma Import (C, BeginThreadEx, "_beginthreadex");

   Debug_Process                     : constant := 16#00000001#;
   Debug_Only_This_Process           : constant := 16#00000002#;
   Create_Suspended                  : constant := 16#00000004#;
   Detached_Process                  : constant := 16#00000008#;
   Create_New_Console                : constant := 16#00000010#;

   Create_New_Process_Group          : constant := 16#00000200#;

   Create_No_window                  : constant := 16#08000000#;

   Profile_User                      : constant := 16#10000000#;
   Profile_Kernel                    : constant := 16#20000000#;
   Profile_Server                    : constant := 16#40000000#;

   Stack_Size_Param_Is_A_Reservation : constant := 16#00010000#;

   function GetExitCodeThread
     (hThread   : Win32.HANDLE;
      pExitCode : not null access Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, GetExitCodeThread, "GetExitCodeThread");

   function ResumeThread (hThread : Win32.HANDLE) return Win32.DWORD;
   pragma Import (Stdcall, ResumeThread, "ResumeThread");

   function SuspendThread (hThread : Win32.HANDLE) return Win32.DWORD;
   pragma Import (Stdcall, SuspendThread, "SuspendThread");

   procedure ExitThread (dwExitCode : Win32.DWORD);
   pragma Import (Stdcall, ExitThread, "ExitThread");

   procedure EndThreadEx (dwExitCode : Win32.DWORD);
   pragma Import (C, EndThreadEx, "_endthreadex");

   function TerminateThread
     (hThread    : Win32.HANDLE;
      dwExitCode : Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, TerminateThread, "TerminateThread");

   function GetCurrentThread return Win32.HANDLE;
   pragma Import (Stdcall, GetCurrentThread, "GetCurrentThread");

   function GetCurrentProcess return Win32.HANDLE;
   pragma Import (Stdcall, GetCurrentProcess, "GetCurrentProcess");

   function GetCurrentThreadId return Win32.DWORD;
   pragma Import (Stdcall, GetCurrentThreadId, "GetCurrentThreadId");

   function TlsAlloc return Win32.DWORD;
   pragma Import (Stdcall, TlsAlloc, "TlsAlloc");

   function TlsGetValue (dwTlsIndex : Win32.DWORD) return Win32.PVOID;
   pragma Import (Stdcall, TlsGetValue, "TlsGetValue");

   function TlsSetValue
     (dwTlsIndex : Win32.DWORD; pTlsValue : Win32.PVOID) return Win32.BOOL;
   pragma Import (Stdcall, TlsSetValue, "TlsSetValue");

   function TlsFree (dwTlsIndex : Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, TlsFree, "TlsFree");

   TLS_Nothing : constant := Win32.DWORD'Last;

   procedure ExitProcess (uExitCode : Interfaces.C.unsigned);
   pragma Import (Stdcall, ExitProcess, "ExitProcess");

   function WaitForSingleObject
     (hHandle        : Win32.HANDLE;
      dwMilliseconds : Win32.DWORD) return Win32.DWORD;
   pragma Import (Stdcall, WaitForSingleObject, "WaitForSingleObject");

   function WaitForSingleObjectEx
     (hHandle        : Win32.HANDLE;
      dwMilliseconds : Win32.DWORD;
      fAlertable     : Win32.BOOL) return Win32.DWORD;
   pragma Import (Stdcall, WaitForSingleObjectEx, "WaitForSingleObjectEx");

   Wait_Infinite : constant := Win32.DWORD'Last;
   WAIT_TIMEOUT  : constant := 16#0000_0102#;
   WAIT_FAILED   : constant := 16#FFFF_FFFF#;

   ------------------------------------
   -- Semaphores, Events and Mutexes --
   ------------------------------------

   function CreateSemaphore
     (pSemaphoreAttributes : access Win32.SECURITY_ATTRIBUTES;
      lInitialCount        : Interfaces.C.long;
      lMaximumCount        : Interfaces.C.long;
      pName                : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, CreateSemaphore, "CreateSemaphoreA");

   function OpenSemaphore
     (dwDesiredAccess : Win32.DWORD;
      bInheritHandle  : Win32.BOOL;
      pName           : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, OpenSemaphore, "OpenSemaphoreA");

   function ReleaseSemaphore
     (hSemaphore     : Win32.HANDLE;
      lReleaseCount  : Interfaces.C.long;
      pPreviousCount : access Win32.LONG) return Win32.BOOL;
   pragma Import (Stdcall, ReleaseSemaphore, "ReleaseSemaphore");

   function CreateEvent
     (pEventAttributes : access Win32.SECURITY_ATTRIBUTES;
      bManualReset     : Win32.BOOL;
      bInitialState    : Win32.BOOL;
      pName            : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, CreateEvent, "CreateEventA");

   function OpenEvent
     (dwDesiredAccess : Win32.DWORD;
      bInheritHandle  : Win32.BOOL;
      pName           : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, OpenEvent, "OpenEventA");

   function SetEvent (hEvent : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, SetEvent, "SetEvent");

   function ResetEvent (hEvent : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, ResetEvent, "ResetEvent");

   function PulseEvent (hEvent : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, PulseEvent, "PulseEvent");

   function CreateMutex
     (pMutexAttributes : access Win32.SECURITY_ATTRIBUTES;
      bInitialOwner    : Win32.BOOL;
      pName            : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, CreateMutex, "CreateMutexA");

   function OpenMutex
     (dwDesiredAccess : Win32.DWORD;
      bInheritHandle  : Win32.BOOL;
      pName           : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, OpenMutex, "OpenMutexA");

   function ReleaseMutex (hMutex : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, ReleaseMutex, "ReleaseMutex");

   ---------------------------------------------------
   -- Accessing properties of Threads and Processes --
   ---------------------------------------------------

   -----------------
   --  Priorities --
   -----------------

   function SetThreadPriority
     (hThread   : Win32.HANDLE;
      nPriority : Interfaces.C.int) return Win32.BOOL;
   pragma Import (Stdcall, SetThreadPriority, "SetThreadPriority");

   function GetThreadPriority (hThread : Win32.HANDLE) return Interfaces.C.int;
   pragma Import (Stdcall, GetThreadPriority, "GetThreadPriority");

   function SetPriorityClass
     (hProcess        : Win32.HANDLE;
      dwPriorityClass : Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, SetPriorityClass, "SetPriorityClass");

   procedure SetThreadPriorityBoost
     (hThread              : Win32.HANDLE;
      DisablePriorityBoost : Win32.BOOL);
   pragma Import (Stdcall, SetThreadPriorityBoost, "SetThreadPriorityBoost");

   Normal_Priority_Class   : constant := 16#00000020#;
   Idle_Priority_Class     : constant := 16#00000040#;
   High_Priority_Class     : constant := 16#00000080#;
   Realtime_Priority_Class : constant := 16#00000100#;

   Thread_Priority_Idle          : constant := -15;
   Thread_Priority_Lowest        : constant := -2;
   Thread_Priority_Below_Normal  : constant := -1;
   Thread_Priority_Normal        : constant := 0;
   Thread_Priority_Above_Normal  : constant := 1;
   Thread_Priority_Highest       : constant := 2;
   Thread_Priority_Time_Critical : constant := 15;
   Thread_Priority_Error_Return  : constant := Interfaces.C.long'Last;

private

   type sigset_t is new Interfaces.C.unsigned_long;

   type CRITICAL_SECTION is record
      DebugInfo : System.Address;

      LockCount      : Long_Integer;
      RecursionCount : Long_Integer;
      OwningThread   : Win32.HANDLE;
      --  The above three fields control entering and exiting the critical
      --  section for the resource.

      LockSemaphore : Win32.HANDLE;
      Reserved      : Win32.DWORD;
   end record;

end System.OS_Interface;
