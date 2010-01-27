/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              S E H - I N I T                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 2005-2009, Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This unit contains support for SEH (Structured Exception Handling).
    Right now the only implementation is for Win32.  */

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"

/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)

#else
#include "config.h"
#include "system.h"
#endif

#include "raise.h"

/* Addresses of exception data blocks for predefined exceptions. */
extern struct Exception_Data constraint_error;
extern struct Exception_Data numeric_error;
extern struct Exception_Data program_error;
extern struct Exception_Data storage_error;
extern struct Exception_Data tasking_error;
extern struct Exception_Data _abort_signal;

#define Raise_From_Signal_Handler \
                      ada__exceptions__raise_from_signal_handler
extern void Raise_From_Signal_Handler (struct Exception_Data *, const char *);


#if defined (_WIN32)

#include <windows.h>
#include <excpt.h>

extern void _global_unwind2 (void *);

EXCEPTION_DISPOSITION __gnat_SEH_error_handler
(struct _EXCEPTION_RECORD*, void*, struct _CONTEXT*, void*);

EXCEPTION_DISPOSITION
__gnat_SEH_error_handler (struct _EXCEPTION_RECORD* ExceptionRecord,
			  void *EstablisherFrame,
			  struct _CONTEXT* ContextRecord ATTRIBUTE_UNUSED,
			  void *DispatcherContext ATTRIBUTE_UNUSED)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (ExceptionRecord->ExceptionCode)
    {
    case EXCEPTION_ACCESS_VIOLATION:
      /* If the failing address isn't maximally-aligned or if the page
	 before the faulting page is not accessible, this is a program error.
      */
      if ((ExceptionRecord->ExceptionInformation[1] & 3) != 0
	  || IsBadCodePtr
	  ((void *)(ExceptionRecord->ExceptionInformation[1] + 4096)))
	{
	  exception = &program_error;
	  msg = "EXCEPTION_ACCESS_VIOLATION";
	}
      else
	{
	  /* otherwise it is a stack overflow  */
	  exception = &storage_error;
	  msg = "stack overflow (or erroneous memory access)";
	}
      break;

    case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
      exception = &constraint_error;
      msg = "EXCEPTION_ARRAY_BOUNDS_EXCEEDED";
      break;

    case EXCEPTION_DATATYPE_MISALIGNMENT:
      exception = &constraint_error;
      msg = "EXCEPTION_DATATYPE_MISALIGNMENT";
      break;

    case EXCEPTION_FLT_DENORMAL_OPERAND:
      exception = &constraint_error;
      msg = "EXCEPTION_FLT_DENORMAL_OPERAND";
      break;

    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
      exception = &constraint_error;
      msg = "EXCEPTION_FLT_DENORMAL_OPERAND";
      break;

    case EXCEPTION_FLT_INVALID_OPERATION:
      exception = &constraint_error;
      msg = "EXCEPTION_FLT_INVALID_OPERATION";
      break;

    case EXCEPTION_FLT_OVERFLOW:
      exception = &constraint_error;
      msg = "EXCEPTION_FLT_OVERFLOW";
      break;

    case EXCEPTION_FLT_STACK_CHECK:
      exception = &program_error;
      msg = "EXCEPTION_FLT_STACK_CHECK";
      break;

    case EXCEPTION_FLT_UNDERFLOW:
      exception = &constraint_error;
      msg = "EXCEPTION_FLT_UNDERFLOW";
      break;

    case EXCEPTION_INT_DIVIDE_BY_ZERO:
      exception = &constraint_error;
      msg = "EXCEPTION_INT_DIVIDE_BY_ZERO";
      break;

    case EXCEPTION_INT_OVERFLOW:
      exception = &constraint_error;
      msg = "EXCEPTION_INT_OVERFLOW";
      break;

    case EXCEPTION_INVALID_DISPOSITION:
      exception = &program_error;
      msg = "EXCEPTION_INVALID_DISPOSITION";
      break;

    case EXCEPTION_NONCONTINUABLE_EXCEPTION:
      exception = &program_error;
      msg = "EXCEPTION_NONCONTINUABLE_EXCEPTION";
      break;

    case EXCEPTION_PRIV_INSTRUCTION:
      exception = &program_error;
      msg = "EXCEPTION_PRIV_INSTRUCTION";
      break;

    case EXCEPTION_SINGLE_STEP:
      exception = &program_error;
      msg = "EXCEPTION_SINGLE_STEP";
      break;

    case EXCEPTION_STACK_OVERFLOW:
      exception = &storage_error;
      msg = "EXCEPTION_STACK_OVERFLOW";
      break;

   default:
      exception = &program_error;
      msg = "unhandled signal";
    }

#if ! defined (_WIN64)
  /* This call is important as it avoids locking the second time we catch a
     signal. Note that this routine is documented as internal to Windows and
     should not be used.  */

  _global_unwind2 (EstablisherFrame);
  /* Call equivalent to RtlUnwind (EstablisherFrame, NULL, NULL, 0); */
#endif

  Raise_From_Signal_Handler (exception, msg);
  return 0; /* This is never reached, avoid compiler warning  */
}

#if defined (_WIN64)
/*  On x86_64 windows exception mechanism is no more based on a chained list
    of handlers addresses on the stack. Instead unwinding information is used
    to retrieve the exception handler (similar to ZCX GCC mechanism). So in
    order to register an exception handler we need to put in the final
    executable some unwinding information. This information might be present
    statically in the image file inside the .pdata section or registered
    through RtlAddFunctionTable API. Currently the GCC toolchain does not
    generate the .pdata information for each function. As we don't need to
    handle SEH exceptions except for signal handling we are registering a
    "fake" unwinding data that associate a SEH exception handler to the
    complete .text section. As we never return from the handler, the system
    does not try to do the final unwinding using the pdata information. The
    unwinding is handled by the runtime using either the GNAT SJLJ mechanism
    or the ZCX GCC mechanism.

    The current implementation is using the RtlAddFunctionTable. Here is for
    information purposes the equivalent using a static .pdata section:

         .section .rdata,"dr"
         .align 4
      Lunwind_info:
         .byte 9,0,0,0
         .rva ___gnat_SEH_error_handler
         .section .pdata,"dr"
         .align 4
         .long 0
         .rva etext
         .rva Lunwind_info

    Solutions based on SetUnhandledExceptionFilter have been discarded as this
    function is mostly disabled on last Windows versions.
    Using AddVectoredExceptionHandler should also be discarded as it overrides
    all SEH exception handlers that might be present in the program itself and
    the loaded DLL (for example it results in unexpected behaviors in the
    Win32 subsystem.  */

typedef struct _UNWIND_INFO {
  BYTE VersionAndFlags;
  BYTE PrologSize;
  BYTE CountOfUnwindCodes;
  BYTE FrameRegisterAndOffset;
  ULONG AddressOfExceptionHandler;
} UNWIND_INFO,*PUNWIND_INFO;

static RUNTIME_FUNCTION Table[1];
static UNWIND_INFO unwind_info[1];

#define UNW_VERSION 0x01
#define UNW_FLAG_EHANDLER 0x08

void __gnat_install_SEH_handler (void *eh ATTRIBUTE_UNUSED)
{
  /* Get the end of the text section.  */
  extern char etext[] asm("etext");
  /* Get the base of the module.  */
  extern char __ImageBase[];

  /* Current version is always 1 and we are registering an
     exception handler.  */
  unwind_info[0].VersionAndFlags = UNW_FLAG_EHANDLER | UNW_VERSION;

  /* We don't use the unwinding info so fill the structure with 0 values.  */
  unwind_info[0].PrologSize = 0;
  unwind_info[0].CountOfUnwindCodes = 0;
  unwind_info[0].FrameRegisterAndOffset = 0;

  /* Add the exception handler.  */
  unwind_info[0].AddressOfExceptionHandler =
    (DWORD)((char *)__gnat_SEH_error_handler - __ImageBase);

  /* Set its scope to the entire program.  */
  Table[0].BeginAddress = 0;
  Table[0].EndAddress = (DWORD)(etext - __ImageBase);
  Table[0].UnwindData = (DWORD)((char *)unwind_info - __ImageBase);

  /* Register the unwind information.  */
  RtlAddFunctionTable (Table, 1, (DWORD64)__ImageBase);
}

#else /* defined (_WIN64) */
/*  Install the Win32 SEH exception handler. Note that the caller must have
    allocated 8 bytes on the stack and pass the pointer to this stack
    space. This is needed as the SEH exception handler must be on the stack of
    the thread.

       int buf[2];

       __gnat_install_SEH_handler ((void*)buf);

       main();

   This call must be done before calling the main procedure or the thread
   entry. The stack space must exists during all the main run.  */

void
__gnat_install_SEH_handler (void *ER)
{
  int *ptr;

  /* put current handler in ptr */

  asm ("mov %%fs:(0),%0" : "=r" (ptr));

  ((int *)ER)[0] = (int)ptr;                       /* previous handler */
  ((int *)ER)[1] = (int)__gnat_SEH_error_handler;  /* new handler */

  /* ER is the new handler, set fs:(0) with this value */

  asm volatile ("mov %0,%%fs:(0)": : "r" (ER));
}
#endif

#else /* defined (_WIN32) */
/* For all non Windows targets we provide a dummy SEH install handler.  */
void __gnat_install_SEH_handler (void *eh ATTRIBUTE_UNUSED)
{
}
#endif
