------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--         Copyright (C) 1997-2001, Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a NT (native) version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
with Interfaces.C.Strings;

package System.OS_Interface is
pragma Preelaborate;

   subtype int  is Interfaces.C.int;
   subtype long is Interfaces.C.long;

   -------------------
   -- General Types --
   -------------------

   type DWORD is new Interfaces.C.unsigned_long;
   type WORD  is new Interfaces.C.unsigned_short;

   --  The LARGE_INTEGER type is actually a fixed point type
   --  that only can represent integers. The reason for this is
   --  easier conversion to Duration or other fixed point types.
   --  (See Operations.Clock)

   type LARGE_INTEGER is delta 1.0 range -2.0**63 .. 2.0**63 - 1.0;
   for LARGE_INTEGER'Alignment use 4;

   subtype PSZ   is Interfaces.C.Strings.chars_ptr;
   subtype PCHAR is Interfaces.C.Strings.chars_ptr;
   subtype PVOID is System.Address;
   Null_Void   : constant PVOID := System.Null_Address;

   type PLONG  is access all Interfaces.C.long;
   type PDWORD is access all DWORD;

   type BOOL is new Boolean;
   for BOOL'Size use Interfaces.C.unsigned_long'Size;

   -------------------------
   -- Handles for objects --
   -------------------------

   type HANDLE is new Interfaces.C.long;
   type PHANDLE is access all HANDLE;

   subtype Thread_Id is HANDLE;

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

   function intr_attach (sig : int; handler : isr_address) return long;
   pragma Import (C, intr_attach, "signal");

   Intr_Attach_Reset : constant Boolean := True;
   --  True if intr_attach is reset after an interrupt handler is called

   procedure kill (sig : Signal);
   pragma Import (C, kill, "raise");

   ---------------------
   -- Time Management --
   ---------------------

   procedure Sleep (dwMilliseconds : DWORD);
   pragma Import (Stdcall, Sleep, External_Name => "Sleep");

   type SYSTEMTIME is record
      wYear         : WORD;
      wMonth        : WORD;
      wDayOfWeek    : WORD;
      wDay          : WORD;
      wHour         : WORD;
      wMinute       : WORD;
      wSecond       : WORD;
      wMilliseconds : WORD;
   end record;

   procedure GetSystemTime (pSystemTime : access SYSTEMTIME);
   pragma Import (Stdcall, GetSystemTime, "GetSystemTime");

   procedure GetSystemTimeAsFileTime (lpFileTime : access Long_Long_Integer);
   pragma Import (Stdcall, GetSystemTimeAsFileTime, "GetSystemTimeAsFileTime");

   function SetSystemTime (pSystemTime : access SYSTEMTIME) return BOOL;
   pragma Import (Stdcall, SetSystemTime, "SetSystemTime");

   function FileTimeToSystemTime
     (lpFileTime   : access Long_Long_Integer;
      lpSystemTime : access SYSTEMTIME) return BOOL;
   pragma Import (Stdcall, FileTimeToSystemTime, "FileTimeToSystemTime");

   function SystemTimeToFileTime
     (lpSystemTime : access SYSTEMTIME;
      lpFileTime   : access Long_Long_Integer) return BOOL;
   pragma Import (Stdcall, SystemTimeToFileTime, "SystemTimeToFileTime");

   function FileTimeToLocalFileTime
     (lpFileTime      : access Long_Long_Integer;
      lpLocalFileTime : access Long_Long_Integer) return BOOL;
   pragma Import (Stdcall, FileTimeToLocalFileTime, "FileTimeToLocalFileTime");

   function LocalFileTimeToFileTime
     (lpFileTime      : access Long_Long_Integer;
      lpLocalFileTime : access Long_Long_Integer) return BOOL;
   pragma Import (Stdcall, LocalFileTimeToFileTime, "LocalFileTimeToFileTime");

   function QueryPerformanceCounter
     (lpPerformanceCount : access LARGE_INTEGER) return BOOL;
   pragma Import
     (Stdcall, QueryPerformanceCounter, "QueryPerformanceCounter");

   function QueryPerformanceFrequency
     (lpFrequency : access LARGE_INTEGER) return BOOL;
   pragma Import
     (Stdcall, QueryPerformanceFrequency, "QueryPerformanceFrequency");

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;

   -----------------------
   -- Critical sections --
   -----------------------

   type CRITICAL_SECTION is private;
   type PCRITICAL_SECTION is access all CRITICAL_SECTION;

   procedure InitializeCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import
     (Stdcall, InitializeCriticalSection, "InitializeCriticalSection");

   procedure EnterCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import (Stdcall, EnterCriticalSection, "EnterCriticalSection");

   procedure LeaveCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import (Stdcall, LeaveCriticalSection, "LeaveCriticalSection");

   procedure DeleteCriticalSection (pCriticalSection : PCRITICAL_SECTION);
   pragma Import (Stdcall, DeleteCriticalSection, "DeleteCriticalSection");

   -------------------------------------------------------------
   -- Thread Creation, Activation, Suspension And Termination --
   -------------------------------------------------------------

   type PTHREAD_START_ROUTINE is access function
     (pThreadParameter : PVOID) return DWORD;
   pragma Convention (Stdcall, PTHREAD_START_ROUTINE);

   type SECURITY_ATTRIBUTES is record
      nLength              : DWORD;
      pSecurityDescriptor  : PVOID;
      bInheritHandle       : BOOL;
   end record;

   type PSECURITY_ATTRIBUTES is access all SECURITY_ATTRIBUTES;

   function CreateThread
     (pThreadAttributes    : PSECURITY_ATTRIBUTES;
      dwStackSize          : DWORD;
      pStartAddress        : PTHREAD_START_ROUTINE;
      pParameter           : PVOID;
      dwCreationFlags      : DWORD;
      pThreadId            : PDWORD) return HANDLE;
   pragma Import (Stdcall, CreateThread, "CreateThread");

   function BeginThreadEx
     (pThreadAttributes    : PSECURITY_ATTRIBUTES;
      dwStackSize          : DWORD;
      pStartAddress        : PTHREAD_START_ROUTINE;
      pParameter           : PVOID;
      dwCreationFlags      : DWORD;
      pThreadId            : PDWORD) return HANDLE;
   pragma Import (C, BeginThreadEx, "_beginthreadex");

   Debug_Process              : constant := 16#00000001#;
   Debug_Only_This_Process    : constant := 16#00000002#;
   Create_Suspended           : constant := 16#00000004#;
   Detached_Process           : constant := 16#00000008#;
   Create_New_Console         : constant := 16#00000010#;

   Create_New_Process_Group   : constant := 16#00000200#;

   Create_No_window           : constant := 16#08000000#;

   Profile_User               : constant := 16#10000000#;
   Profile_Kernel             : constant := 16#20000000#;
   Profile_Server             : constant := 16#40000000#;

   function GetExitCodeThread
     (hThread   : HANDLE;
      pExitCode : PDWORD) return BOOL;
   pragma Import (Stdcall, GetExitCodeThread, "GetExitCodeThread");

   function ResumeThread (hThread : HANDLE) return DWORD;
   pragma Import (Stdcall, ResumeThread, "ResumeThread");

   function SuspendThread (hThread : HANDLE) return DWORD;
   pragma Import (Stdcall, SuspendThread, "SuspendThread");

   procedure ExitThread (dwExitCode : DWORD);
   pragma Import (Stdcall, ExitThread, "ExitThread");

   procedure EndThreadEx (dwExitCode : DWORD);
   pragma Import (C, EndThreadEx, "_endthreadex");

   function TerminateThread
     (hThread    : HANDLE;
      dwExitCode : DWORD) return BOOL;
   pragma Import (Stdcall, TerminateThread, "TerminateThread");

   function GetCurrentThread return HANDLE;
   pragma Import (Stdcall, GetCurrentThread, "GetCurrentThread");

   function GetCurrentProcess return HANDLE;
   pragma Import (Stdcall, GetCurrentProcess, "GetCurrentProcess");

   function GetCurrentThreadId return DWORD;
   pragma Import (Stdcall, GetCurrentThreadId, "GetCurrentThreadId");

   function TlsAlloc return DWORD;
   pragma Import (Stdcall, TlsAlloc, "TlsAlloc");

   function TlsGetValue (dwTlsIndex : DWORD) return PVOID;
   pragma Import (Stdcall, TlsGetValue, "TlsGetValue");

   function TlsSetValue (dwTlsIndex : DWORD; pTlsValue : PVOID) return BOOL;
   pragma Import (Stdcall, TlsSetValue, "TlsSetValue");

   function TlsFree (dwTlsIndex : DWORD) return BOOL;
   pragma Import (Stdcall, TlsFree, "TlsFree");

   TLS_Nothing : constant := DWORD'Last;

   procedure ExitProcess (uExitCode : Interfaces.C.unsigned);
   pragma Import (Stdcall, ExitProcess, "ExitProcess");

   function WaitForSingleObject
     (hHandle        : HANDLE;
      dwMilliseconds : DWORD) return DWORD;
   pragma Import (Stdcall, WaitForSingleObject, "WaitForSingleObject");

   function WaitForSingleObjectEx
     (hHandle        : HANDLE;
      dwMilliseconds : DWORD;
      fAlertable     : BOOL) return DWORD;
   pragma Import (Stdcall, WaitForSingleObjectEx, "WaitForSingleObjectEx");

   Wait_Infinite : constant := DWORD'Last;
   WAIT_TIMEOUT  : constant := 16#0000_0102#;
   WAIT_FAILED   : constant := 16#FFFF_FFFF#;

   ------------------------------------
   -- Semaphores, Events and Mutexes --
   ------------------------------------

   function CloseHandle (hObject : HANDLE) return BOOL;
   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   function CreateSemaphore
     (pSemaphoreAttributes : PSECURITY_ATTRIBUTES;
      lInitialCount        : Interfaces.C.long;
      lMaximumCount        : Interfaces.C.long;
      pName                : PSZ) return HANDLE;
   pragma Import (Stdcall, CreateSemaphore, "CreateSemaphoreA");

   function OpenSemaphore
     (dwDesiredAccess : DWORD;
      bInheritHandle  : BOOL;
      pName           : PSZ) return HANDLE;
   pragma Import (Stdcall, OpenSemaphore, "OpenSemaphoreA");

   function ReleaseSemaphore
     (hSemaphore     : HANDLE;
      lReleaseCount  : Interfaces.C.long;
      pPreviousCount : PLONG) return BOOL;
   pragma Import (Stdcall, ReleaseSemaphore, "ReleaseSemaphore");

   function CreateEvent
     (pEventAttributes : PSECURITY_ATTRIBUTES;
      bManualReset     : BOOL;
      bInitialState    : BOOL;
      pName            : PSZ) return HANDLE;
   pragma Import (Stdcall, CreateEvent, "CreateEventA");

   function OpenEvent
     (dwDesiredAccess : DWORD;
      bInheritHandle  : BOOL;
      pName           : PSZ) return HANDLE;
   pragma Import (Stdcall, OpenEvent, "OpenEventA");

   function SetEvent (hEvent : HANDLE) return BOOL;
   pragma Import (Stdcall, SetEvent, "SetEvent");

   function ResetEvent (hEvent : HANDLE) return BOOL;
   pragma Import (Stdcall, ResetEvent, "ResetEvent");

   function PulseEvent (hEvent : HANDLE) return BOOL;
   pragma Import (Stdcall, PulseEvent, "PulseEvent");

   function CreateMutex
     (pMutexAttributes : PSECURITY_ATTRIBUTES;
      bInitialOwner    : BOOL;
      pName            : PSZ) return HANDLE;
   pragma Import (Stdcall, CreateMutex, "CreateMutexA");

   function OpenMutex
     (dwDesiredAccess : DWORD;
      bInheritHandle  : BOOL;
      pName           : PSZ) return HANDLE;
   pragma Import (Stdcall, OpenMutex, "OpenMutexA");

   function ReleaseMutex (hMutex : HANDLE) return BOOL;
   pragma Import (Stdcall, ReleaseMutex, "ReleaseMutex");

   ---------------------------------------------------
   -- Accessing properties of Threads and Processes --
   ---------------------------------------------------

   -----------------
   --  Priorities --
   -----------------

   function SetThreadPriority
     (hThread   : HANDLE;
      nPriority : Interfaces.C.int) return BOOL;
   pragma Import (Stdcall, SetThreadPriority, "SetThreadPriority");

   function GetThreadPriority (hThread : HANDLE) return Interfaces.C.int;
   pragma Import (Stdcall, GetThreadPriority, "GetThreadPriority");

   function SetPriorityClass
     (hProcess        : HANDLE;
      dwPriorityClass : DWORD) return BOOL;
   pragma Import (Stdcall, SetPriorityClass, "SetPriorityClass");

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

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

private

   type sigset_t is new Interfaces.C.unsigned_long;

   type CRITICAL_SECTION is record
      DebugInfo      : System.Address;
      --  The following three fields control entering and
      --  exiting the critical section for the resource
      LockCount      : Long_Integer;
      RecursionCount : Long_Integer;
      OwningThread   : HANDLE;
      LockSemaphore  : HANDLE;
      Reserved       : DWORD;
   end record;

end System.OS_Interface;
