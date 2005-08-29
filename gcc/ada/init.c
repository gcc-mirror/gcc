/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 I N I T                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2005, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This unit contains initialization circuits that are system dependent. A
    major part of the functionality involved involves stack overflow checking.
    The GCC backend generates probe instructions to test for stack overflow.
    For details on the exact approach used to generate these probes, see the
    "Using and Porting GCC" manual, in particular the "Stack Checking" section
    and the subsection "Specifying How Stack Checking is Done". The handlers
    installed by this file are used to handle resulting signals that come
    from these probes failing (i.e. touching protected pages) */

/* This file should be kept synchronized with 2sinit.ads, 2sinit.adb, and
   5zinit.adb. All these files implement the required functionality for
   different targets. */

/* The following include is here to meet the published VxWorks requirement
   that the __vxworks header appear before any other include. */
#ifdef __vxworks
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>

/* We don't have libiberty, so us malloc.  */
#define xmalloc(S) malloc (S)
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"
#include "raise.h"

extern void __gnat_raise_program_error (const char *, int);

/* Addresses of exception data blocks for predefined exceptions. */
extern struct Exception_Data constraint_error;
extern struct Exception_Data numeric_error;
extern struct Exception_Data program_error;
extern struct Exception_Data storage_error;
extern struct Exception_Data tasking_error;
extern struct Exception_Data _abort_signal;

#define Lock_Task system__soft_links__lock_task
extern void (*Lock_Task) (void);

#define Unlock_Task system__soft_links__unlock_task
extern void (*Unlock_Task) (void);

#define Check_Abort_Status     \
                      system__soft_links__check_abort_status
extern int (*Check_Abort_Status) (void);

#define Raise_From_Signal_Handler \
                      ada__exceptions__raise_from_signal_handler
extern void Raise_From_Signal_Handler (struct Exception_Data *, const char *);

/* Copies of global values computed by the binder */
int   __gl_main_priority            = -1;
int   __gl_time_slice_val           = -1;
char  __gl_wc_encoding              = 'n';
char  __gl_locking_policy           = ' ';
char  __gl_queuing_policy           = ' ';
char  __gl_task_dispatching_policy  = ' ';
char *__gl_restrictions             = 0;
char *__gl_interrupt_states         = 0;
int   __gl_num_interrupt_states     = 0;
int   __gl_unreserve_all_interrupts = 0;
int   __gl_exception_tracebacks     = 0;
int   __gl_zero_cost_exceptions     = 0;
int   __gl_detect_blocking          = 0;

/* Indication of whether synchronous signal handler has already been
   installed by a previous call to adainit */
int  __gnat_handler_installed      = 0;

#ifndef IN_RTS
int __gnat_inside_elab_final_code = 0;
/* ??? This variable is obsolete since 2001-08-29 but is kept to allow
   bootstrap from old GNAT versions (< 3.15). */
#endif

/* HAVE_GNAT_INIT_FLOAT must be set on every targets where a __gnat_init_float
   is defined. If this is not set them a void implementation will be defined
   at the end of this unit. */
#undef HAVE_GNAT_INIT_FLOAT

/******************************/
/* __gnat_get_interrupt_state */
/******************************/

char __gnat_get_interrupt_state (int);

/* This routine is called from the runtime as needed to determine the state
   of an interrupt, as set by an Interrupt_State pragma appearing anywhere
   in the current partition. The input argument is the interrupt number,
   and the result is one of the following:

       'n'   this interrupt not set by any Interrupt_State pragma
       'u'   Interrupt_State pragma set state to User
       'r'   Interrupt_State pragma set state to Runtime
       's'   Interrupt_State pragma set state to System */

char
__gnat_get_interrupt_state (int intrup)
{
  if (intrup >= __gl_num_interrupt_states)
    return 'n';
  else
    return __gl_interrupt_states [intrup];
}

/**********************/
/* __gnat_set_globals */
/**********************/

/* This routine is called from the binder generated main program.  It copies
   the values for global quantities computed by the binder into the following
   global locations. The reason that we go through this copy, rather than just
   define the global locations in the binder generated file, is that they are
   referenced from the runtime, which may be in a shared library, and the
   binder file is not in the shared library. Global references across library
   boundaries like this are not handled correctly in all systems.  */

/* For detailed description of the parameters to this routine, see the
   section titled Run-Time Globals in package Bindgen (bindgen.adb) */

void
__gnat_set_globals (int main_priority,
                    int time_slice_val,
                    char wc_encoding,
                    char locking_policy,
                    char queuing_policy,
                    char task_dispatching_policy,
                    char *restrictions,
                    char *interrupt_states,
                    int num_interrupt_states,
                    int unreserve_all_interrupts,
                    int exception_tracebacks,
                    int zero_cost_exceptions,
                    int detect_blocking)
{
  static int already_called = 0;

  /* If this procedure has been already called once, check that the
     arguments in this call are consistent with the ones in the previous
     calls. Otherwise, raise a Program_Error exception.

     We do not check for consistency of the wide character encoding
     method. This default affects only Wide_Text_IO where no explicit
     coding method is given, and there is no particular reason to let
     this default be affected by the source representation of a library
     in any case.

     We do not check either for the consistency of exception tracebacks,
     because exception tracebacks are not normally set in Stand-Alone
     libraries. If a library or the main program set the exception
     tracebacks, then they are never reset afterwards (see below).

     The value of main_priority is meaningful only when we are invoked
     from the main program elaboration routine of an Ada application.
     Checking the consistency of this parameter should therefore not be
     done. Since it is assured that the main program elaboration will
     always invoke this procedure before any library elaboration
     routine, only the value of main_priority during the first call
     should be taken into account and all the subsequent ones should be
     ignored. Note that the case where the main program is not written
     in Ada is also properly handled, since the default value will then
     be used for this parameter.

     For identical reasons, the consistency of time_slice_val should not
     be checked. */

  if (already_called)
    {
      if (__gl_locking_policy		   != locking_policy
	  || __gl_queuing_policy           != queuing_policy
	  || __gl_task_dispatching_policy  != task_dispatching_policy
	  || __gl_unreserve_all_interrupts != unreserve_all_interrupts
	  || __gl_zero_cost_exceptions     != zero_cost_exceptions)
	__gnat_raise_program_error (__FILE__, __LINE__);

      /* If either a library or the main program set the exception traceback
         flag, it is never reset later */

      if (exception_tracebacks != 0)
         __gl_exception_tracebacks = exception_tracebacks;

      return;
    }
  already_called = 1;

  __gl_main_priority            = main_priority;
  __gl_time_slice_val           = time_slice_val;
  __gl_wc_encoding              = wc_encoding;
  __gl_locking_policy           = locking_policy;
  __gl_queuing_policy           = queuing_policy;
  __gl_restrictions             = restrictions;
  __gl_interrupt_states         = interrupt_states;
  __gl_num_interrupt_states     = num_interrupt_states;
  __gl_task_dispatching_policy  = task_dispatching_policy;
  __gl_unreserve_all_interrupts = unreserve_all_interrupts;
  __gl_exception_tracebacks     = exception_tracebacks;
  __gl_detect_blocking          = detect_blocking;

  /* ??? __gl_zero_cost_exceptions is new in 3.15 and is referenced from
     a-except.adb, which is also part of the compiler sources. Since the
     compiler is built with an older release of GNAT, the call generated by
     the old binder to this function does not provide any value for the
     corresponding argument, so the global has to be initialized in some
     reasonable other way. This could be removed as soon as the next major
     release is out.  */

#ifdef IN_RTS
  __gl_zero_cost_exceptions = zero_cost_exceptions;
#else
  __gl_zero_cost_exceptions = 0;
  /* We never build the compiler to run in ZCX mode currently anyway.  */
#endif
}

/* Notes on the Zero Cost Exceptions scheme and its impact on the signal
   handlers implemented below :

   What we call Zero Cost Exceptions is implemented using the GCC eh
   circuitry, even if the underlying implementation is setjmp/longjmp
   based. In any case ...

   The GCC unwinder expects to be dealing with call return addresses, since
   this is the "nominal" case of what we retrieve while unwinding a regular
   call chain. To evaluate if a handler applies at some point in this chain,
   the propagation engine needs to determine what region the corresponding
   call instruction pertains to. The return address may not be attached to the
   same region as the call, so the unwinder unconditionally subtracts "some"
   amount to the return addresses it gets to search the region tables. The
   exact amount is computed to ensure that the resulting address is inside the
   call instruction, and is thus target dependent (think about delay slots for
   instance).

   When we raise an exception from a signal handler, e.g. to transform a
   SIGSEGV into Storage_Error, things need to appear as if the signal handler
   had been "called" by the instruction which triggered the signal, so that
   exception handlers that apply there are considered. What the unwinder will
   retrieve as the return address from the signal handler is what it will find
   as the faulting instruction address in the corresponding signal context
   pushed by the kernel. Leaving this address untouched may loose, because if
   the triggering instruction happens to be the very first of a region, the
   later adjustments performed by the unwinder would yield an address outside
   that region. We need to compensate for those adjustments at some point,
   which we currently do in the GCC unwinding fallback macro.

   The thread at http://gcc.gnu.org/ml/gcc-patches/2004-05/msg00343.html
   describes a couple of issues with our current approach. Basically: on some
   targets the adjustment to apply depends on the triggering signal, which is
   not easily accessible from the macro, and we actually do not tackle this as
   of today. Besides, other languages, e.g. Java, deal with this by performing
   the adjustment in the signal handler before the raise, so our adjustments
   may break those front-ends.

   To have it all right, we should either find a way to deal with the signal
   variants from the macro and convert Java on all targets (ugh), or remove
   our macro adjustments and update our signal handlers a-la-java way.  The
   latter option appears the simplest, although some targets have their share
   of subtleties to account for.  See for instance the syscall(SYS_sigaction)
   story in libjava/include/i386-signal.h.  */

/***************/
/* AIX Section */
/***************/

#if defined (_AIX)

#include <signal.h>
#include <sys/time.h>

/* Some versions of AIX don't define SA_NODEFER. */

#ifndef SA_NODEFER
#define SA_NODEFER 0
#endif /* SA_NODEFER */

/* Versions of AIX before 4.3 don't have nanosleep but provide
   nsleep instead. */

#ifndef _AIXVERSION_430

extern int nanosleep (struct timestruc_t *, struct timestruc_t *);

int
nanosleep (struct timestruc_t *Rqtp, struct timestruc_t *Rmtp)
{
  return nsleep (Rqtp, Rmtp);
}

#endif /* _AIXVERSION_430 */

static void __gnat_error_handler (int);

static void
__gnat_error_handler (int sig)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGSEGV:
      /* FIXME: we need to detect the case of a *real* SIGSEGV */
      exception = &storage_error;
      msg = "stack overflow or erroneous memory access";
      break;

    case SIGBUS:
      exception = &constraint_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

/*****************/
/* Tru64 section */
/*****************/

#elif defined(__alpha__) && defined(__osf__)

#include <signal.h>
#include <sys/siginfo.h>

static void __gnat_error_handler (int, siginfo_t *, struct sigcontext *);
extern char *__gnat_get_code_loc (struct sigcontext *);
extern void __gnat_set_code_loc (struct sigcontext *, char *);
extern size_t __gnat_machine_state_length (void);

static void
__gnat_error_handler
  (int sig, siginfo_t *sip, struct sigcontext *context ATTRIBUTE_UNUSED)
{
  struct Exception_Data *exception;
  static int recurse = 0;
  const char *msg;

  /* If this was an explicit signal from a "kill", just resignal it.  */
  if (SI_FROMUSER (sip))
    {
      signal (sig, SIG_DFL);
      kill (getpid(), sig);
    }

  /* Otherwise, treat it as something we handle.  */
  switch (sig)
    {
    case SIGSEGV:
      /* If the problem was permissions, this is a constraint error.
	 Likewise if the failing address isn't maximally aligned or if
	 we've recursed.

	 ??? Using a static variable here isn't task-safe, but it's
	 much too hard to do anything else and we're just determining
	 which exception to raise.  */
      if (sip->si_code == SEGV_ACCERR
	  || (((long) sip->si_addr) & 3) != 0
	  || recurse)
	{
	  exception = &constraint_error;
	  msg = "SIGSEGV";
	}
      else
	{
	  /* See if the page before the faulting page is accessible.  Do that
	     by trying to access it.  We'd like to simply try to access
	     4096 + the faulting address, but it's not guaranteed to be
	     the actual address, just to be on the same page.  */
	  recurse++;
	  ((volatile char *)
	   ((long) sip->si_addr & - getpagesize ()))[getpagesize ()];
	  msg = "stack overflow (or erroneous memory access)";
	  exception = &storage_error;
	}
      break;

    case SIGBUS:
      exception = &program_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  recurse = 0;
  Raise_From_Signal_Handler (exception, (char *) msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Setup signal handler to map synchronous signals to appropriate
     exceptions. Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = (void (*) (int)) __gnat_error_handler;
  act.sa_flags = SA_RESTART | SA_NODEFER | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

/* Routines called by s-mastop-tru64.adb.  */

#define SC_GP 29

char *
__gnat_get_code_loc (struct sigcontext *context)
{
  return (char *) context->sc_pc;
}

void
__gnat_set_code_loc (struct sigcontext *context, char *pc)
{
  context->sc_pc = (long) pc;
}


size_t
__gnat_machine_state_length (void)
{
  return sizeof (struct sigcontext);
}

/********************/
/* PA HP-UX section */
/********************/

#elif defined (__hppa__) && defined (__hpux__)

#include <signal.h>
#include <sys/ucontext.h>

static void
__gnat_error_handler (int sig, siginfo_t *siginfo, void *ucontext);

/* __gnat_adjust_context_for_raise - see comments along with the default
   version later in this file.  */

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED, void *ucontext)
{
  mcontext_t *mcontext = &((ucontext_t *) ucontext)->uc_mcontext;

  if (UseWideRegs (mcontext))
    mcontext->ss_wide.ss_32.ss_pcoq_head_lo ++;
  else
    mcontext->ss_narrow.ss_pcoq_head ++;
}

static void
__gnat_error_handler
  (int sig, siginfo_t *siginfo ATTRIBUTE_UNUSED, void *ucontext)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGSEGV:
      /* FIXME: we need to detect the case of a *real* SIGSEGV */
      exception = &storage_error;
      msg = "stack overflow or erroneous memory access";
      break;

    case SIGBUS:
      exception = &constraint_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  __gnat_adjust_context_for_raise (sig, ucontext);

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! Also setup an alternate
     stack region for the handler execution so that stack overflows can be
     handled properly, avoiding a SEGV generation from stack usage by the
     handler itself. */

  static char handler_stack[SIGSTKSZ*2];
  /* SIGSTKSZ appeared to be "short" for the needs in some contexts
     (e.g. experiments with GCC ZCX exceptions).  */

  stack_t stack;

  stack.ss_sp    = handler_stack;
  stack.ss_size  = sizeof (handler_stack);
  stack.ss_flags = 0;

  sigaltstack (&stack, NULL);

  act.sa_sigaction = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_ONSTACK | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

/*********************/
/* GNU/Linux Section */
/*********************/

#elif defined (linux) && (defined (i386) || defined (__x86_64__))

#include <signal.h>

#define __USE_GNU 1 /* required to get REG_EIP/RIP from glibc's ucontext.h */
#include <sys/ucontext.h>

/* GNU/Linux, which uses glibc, does not define NULL in included
   header files */

#if !defined (NULL)
#define NULL ((void *) 0)
#endif

static void __gnat_error_handler (int, siginfo_t *siginfo, void *ucontext);

/* __gnat_adjust_context_for_raise - see comments along with the default
   version later in this file.  */

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED, void *ucontext)
{
  mcontext_t *mcontext = &((ucontext_t *) ucontext)->uc_mcontext;

#if defined (i386)
  mcontext->gregs[REG_EIP]++;
#elif defined (__x86_64__)
  mcontext->gregs[REG_RIP]++;
#endif
}

static void
__gnat_error_handler (int sig,
                      siginfo_t *siginfo ATTRIBUTE_UNUSED,
                      void *ucontext)
{
  struct Exception_Data *exception;
  const char *msg;
  static int recurse = 0;

  switch (sig)
    {
    case SIGSEGV:
      /* If the problem was permissions, this is a constraint error.
       Likewise if the failing address isn't maximally aligned or if
       we've recursed.

       ??? Using a static variable here isn't task-safe, but it's
       much too hard to do anything else and we're just determining
       which exception to raise.  */
      if (recurse)
      {
        exception = &constraint_error;
        msg = "SIGSEGV";
      }
      else
      {
        /* Here we would like a discrimination test to see whether the
           page before the faulting address is accessible. Unfortunately
           Linux seems to have no way of giving us the faulting address.

           In versions of a-init.c before 1.95, we had a test of the page
           before the stack pointer using:

            recurse++;
             ((volatile char *)
              ((long) info->esp_at_signal & - getpagesize ()))[getpagesize ()];

           but that's wrong, since it tests the stack pointer location, and
           the current stack probe code does not move the stack pointer
           until all probes succeed.

           For now we simply do not attempt any discrimination at all. Note
           that this is quite acceptable, since a "real" SIGSEGV can only
           occur as the result of an erroneous program */

        msg = "stack overflow (or erroneous memory access)";
        exception = &storage_error;
      }
      break;

    case SIGBUS:
      exception = &constraint_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }
  recurse = 0;

  /* We adjust the interrupted context here (and not in the
     MD_FALLBACK_FRAME_STATE_FOR macro) because recent versions of the Native
     POSIX Thread Library (NPTL) are compiled with DWARF 2 unwind information,
     and hence the later macro is never executed for signal frames. */

  __gnat_adjust_context_for_raise (sig, ucontext);

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_sigaction = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

/*******************/
/* Interix Section */
/*******************/

#elif defined (__INTERIX)

#include <signal.h>

static void __gnat_error_handler (int);

static void
__gnat_error_handler (int sig)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGSEGV:
      exception = &storage_error;
      msg = "stack overflow or erroneous memory access";
      break;

    case SIGBUS:
      exception = &constraint_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = 0;
  sigemptyset (&act.sa_mask);

  /* Handlers for signals besides SIGSEGV cause c974013 to hang */
/*  sigaction (SIGILL,  &act, NULL); */
/*  sigaction (SIGABRT, &act, NULL); */
/*  sigaction (SIGFPE,  &act, NULL); */
/*  sigaction (SIGBUS,  &act, NULL); */

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);

  __gnat_handler_installed = 1;
}

/****************/
/* IRIX Section */
/****************/

#elif defined (sgi)

#include <signal.h>
#include <siginfo.h>

#ifndef NULL
#define NULL 0
#endif

#define SIGADAABORT 48
#define SIGNAL_STACK_SIZE 4096
#define SIGNAL_STACK_ALIGNMENT 64

static void __gnat_error_handler (int, int, sigcontext_t *);

/* We are not setting the SA_SIGINFO bit in the sigaction flags when
   connecting that handler, with the effects described in the sigaction
   man page:

          SA_SIGINFO [...]
          If cleared and the signal is caught, the first argument is
          also the signal number but the second argument is the signal
          code identifying the cause of the signal. The third argument
          points to a sigcontext_t structure containing the receiving
	  process's context when the signal was delivered.
*/

static void
__gnat_error_handler (int sig, int code, sigcontext_t *sc ATTRIBUTE_UNUSED)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGSEGV:
      if (code == EFAULT)
	{
	  exception = &program_error;
	  msg = "SIGSEGV: (Invalid virtual address)";
	}
      else if (code == ENXIO)
	{
	  exception = &program_error;
	  msg = "SIGSEGV: (Read beyond mapped object)";
	}
      else if (code == ENOSPC)
	{
	  exception = &program_error; /* ??? storage_error ??? */
	  msg = "SIGSEGV: (Autogrow for file failed)";
	}
      else if (code == EACCES || code == EEXIST)
	{
	  /* ??? We handle stack overflows here, some of which do trigger
	         SIGSEGV + EEXIST on Irix 6.5 although EEXIST is not part of
	         the documented valid codes for SEGV in the signal(5) man
	         page.  */

	  /* ??? Re-add smarts to further verify that we launched
		 the stack into a guard page, not an attempt to
		 write to .text or something */
	  exception = &storage_error;
	  msg = "SIGSEGV: (stack overflow or erroneous memory access)";
	}
      else
	{
	  /* Just in case the OS guys did it to us again.  Sometimes
	     they fail to document all of the valid codes that are
	     passed to signal handlers, just in case someone depends
	     on knowing all the codes */
	  exception = &program_error;
	  msg = "SIGSEGV: (Undocumented reason)";
	}
      break;

    case SIGBUS:
      /* Map all bus errors to Program_Error.  */
      exception = &program_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      /* Map all fpe errors to Constraint_Error.  */
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    case SIGADAABORT:
      if ((*Check_Abort_Status) ())
	{
	  exception = &_abort_signal;
	  msg = "";
	}
      else
	return;

      break;

    default:
      /* Everything else is a Program_Error. */
      exception = &program_error;
      msg = "unhandled signal";
    }

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Setup signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_NODEFER + SA_RESTART;
  sigfillset (&act.sa_mask);
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGADAABORT) != 's')
    sigaction (SIGADAABORT,  &act, NULL);

  __gnat_handler_installed = 1;
}

/*******************/
/* Solaris Section */
/*******************/

#elif defined (sun) && defined (__SVR4) && !defined (__vxworks)

#include <signal.h>
#include <siginfo.h>

static void __gnat_error_handler (int, siginfo_t *);

static void
__gnat_error_handler (int sig, siginfo_t *sip)
{
  struct Exception_Data *exception;
  static int recurse = 0;
  const char *msg;

  /* If this was an explicit signal from a "kill", just resignal it.  */
  if (SI_FROMUSER (sip))
    {
      signal (sig, SIG_DFL);
      kill (getpid(), sig);
    }

  /* Otherwise, treat it as something we handle.  */
  switch (sig)
    {
    case SIGSEGV:
      /* If the problem was permissions, this is a constraint error.
	 Likewise if the failing address isn't maximally aligned or if
	 we've recursed.

	 ??? Using a static variable here isn't task-safe, but it's
	 much too hard to do anything else and we're just determining
	 which exception to raise.  */
      if (sip->si_code == SEGV_ACCERR
	  || (((long) sip->si_addr) & 3) != 0
	  || recurse)
	{
	  exception = &constraint_error;
	  msg = "SIGSEGV";
	}
      else
	{
	  /* See if the page before the faulting page is accessible.  Do that
	     by trying to access it.  We'd like to simply try to access
	     4096 + the faulting address, but it's not guaranteed to be
	     the actual address, just to be on the same page.  */
	  recurse++;
	  ((volatile char *)
	   ((long) sip->si_addr & - getpagesize ()))[getpagesize ()];
	  exception = &storage_error;
	  msg = "stack overflow (or erroneous memory access)";
	}
      break;

    case SIGBUS:
      exception = &program_error;
      msg = "SIGBUS";
      break;

    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  recurse = 0;

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

/***************/
/* VMS Section */
/***************/

#elif defined (VMS)

long __gnat_error_handler (int *, void *);

#ifdef __IA64
#define lib_get_curr_invo_context LIB$I64_GET_CURR_INVO_CONTEXT
#define lib_get_prev_invo_context LIB$I64_GET_PREV_INVO_CONTEXT
#define lib_get_invo_handle LIB$I64_GET_INVO_HANDLE
#else
#define lib_get_curr_invo_context LIB$GET_CURR_INVO_CONTEXT
#define lib_get_prev_invo_context LIB$GET_PREV_INVO_CONTEXT
#define lib_get_invo_handle LIB$GET_INVO_HANDLE
#endif

#if defined (IN_RTS) && !defined (__IA64)

/* The prehandler actually gets control first on a condition. It swaps the
   stack pointer and calls the handler (__gnat_error_handler). */
extern long __gnat_error_prehandler (void);

extern char *__gnat_error_prehandler_stack;   /* Alternate signal stack */
#endif

/* Define macro symbols for the VMS conditions that become Ada exceptions.
   Most of these are also defined in the header file ssdef.h which has not
   yet been converted to be recognized by Gnu C. */

/* Defining these as macros, as opposed to external addresses, allows
   them to be used in a case statement (below */
#define SS$_ACCVIO            12
#define SS$_HPARITH         1284
#define SS$_STKOVF          1364
#define SS$_RESIGNAL        2328

/* These codes are in standard message libraries */
extern int CMA$_EXIT_THREAD;
extern int SS$_DEBUG;
extern int SS$_INTDIV;
extern int LIB$_KEYNOTFOU;
extern int LIB$_ACTIMAGE;
extern int MTH$_FLOOVEMAT;       /* Some ACVC_21 CXA tests */

/* These codes are non standard, which is to say the author is
   not sure if they are defined in the standard message libraries
   so keep them as macros for now. */
#define RDB$_STREAM_EOF 20480426
#define FDL$_UNPRIKW 11829410

struct cond_except {
  const int *cond;
  const struct Exception_Data *except;
};

struct descriptor_s {unsigned short len, mbz; __char_ptr32 adr; };

/* Conditions that don't have an Ada exception counterpart must raise
   Non_Ada_Error.  Since this is defined in s-auxdec, it should only be
   referenced by user programs, not the compiler or tools. Hence the
   #ifdef IN_RTS. */

#ifdef IN_RTS

#define Status_Error ada__io_exceptions__status_error
extern struct Exception_Data Status_Error;

#define Mode_Error ada__io_exceptions__mode_error
extern struct Exception_Data Mode_Error;

#define Name_Error ada__io_exceptions__name_error
extern struct Exception_Data Name_Error;

#define Use_Error ada__io_exceptions__use_error
extern struct Exception_Data Use_Error;

#define Device_Error ada__io_exceptions__device_error
extern struct Exception_Data Device_Error;

#define End_Error ada__io_exceptions__end_error
extern struct Exception_Data End_Error;

#define Data_Error ada__io_exceptions__data_error
extern struct Exception_Data Data_Error;

#define Layout_Error ada__io_exceptions__layout_error
extern struct Exception_Data Layout_Error;

#define Non_Ada_Error system__aux_dec__non_ada_error
extern struct Exception_Data Non_Ada_Error;

#define Coded_Exception system__vms_exception_table__coded_exception
extern struct Exception_Data *Coded_Exception (Exception_Code);

#define Base_Code_In system__vms_exception_table__base_code_in
extern Exception_Code Base_Code_In (Exception_Code);

/* DEC Ada exceptions are not defined in a header file, so they
   must be declared as external addresses */

extern int ADA$_PROGRAM_ERROR __attribute__ ((weak));
extern int ADA$_LOCK_ERROR __attribute__ ((weak));
extern int ADA$_EXISTENCE_ERROR __attribute__ ((weak));
extern int ADA$_KEY_ERROR __attribute__ ((weak));
extern int ADA$_KEYSIZERR __attribute__ ((weak));
extern int ADA$_STAOVF __attribute__ ((weak));
extern int ADA$_CONSTRAINT_ERRO __attribute__ ((weak));
extern int ADA$_IOSYSFAILED __attribute__ ((weak));
extern int ADA$_LAYOUT_ERROR __attribute__ ((weak));
extern int ADA$_STORAGE_ERROR __attribute__ ((weak));
extern int ADA$_DATA_ERROR __attribute__ ((weak));
extern int ADA$_DEVICE_ERROR __attribute__ ((weak));
extern int ADA$_END_ERROR __attribute__ ((weak));
extern int ADA$_MODE_ERROR __attribute__ ((weak));
extern int ADA$_NAME_ERROR __attribute__ ((weak));
extern int ADA$_STATUS_ERROR __attribute__ ((weak));
extern int ADA$_NOT_OPEN __attribute__ ((weak));
extern int ADA$_ALREADY_OPEN __attribute__ ((weak));
extern int ADA$_USE_ERROR __attribute__ ((weak));
extern int ADA$_UNSUPPORTED __attribute__ ((weak));
extern int ADA$_FAC_MODE_MISMAT __attribute__ ((weak));
extern int ADA$_ORG_MISMATCH __attribute__ ((weak));
extern int ADA$_RFM_MISMATCH __attribute__ ((weak));
extern int ADA$_RAT_MISMATCH __attribute__ ((weak));
extern int ADA$_MRS_MISMATCH __attribute__ ((weak));
extern int ADA$_MRN_MISMATCH __attribute__ ((weak));
extern int ADA$_KEY_MISMATCH __attribute__ ((weak));
extern int ADA$_MAXLINEXC __attribute__ ((weak));
extern int ADA$_LINEXCMRS __attribute__ ((weak));

/* DEC Ada specific conditions */
static const struct cond_except dec_ada_cond_except_table [] = {
  {&ADA$_PROGRAM_ERROR,   &program_error},
  {&ADA$_USE_ERROR,       &Use_Error},
  {&ADA$_KEYSIZERR,       &program_error},
  {&ADA$_STAOVF,          &storage_error},
  {&ADA$_CONSTRAINT_ERRO, &constraint_error},
  {&ADA$_IOSYSFAILED,     &Device_Error},
  {&ADA$_LAYOUT_ERROR,    &Layout_Error},
  {&ADA$_STORAGE_ERROR,   &storage_error},
  {&ADA$_DATA_ERROR,      &Data_Error},
  {&ADA$_DEVICE_ERROR,    &Device_Error},
  {&ADA$_END_ERROR,       &End_Error},
  {&ADA$_MODE_ERROR,      &Mode_Error},
  {&ADA$_NAME_ERROR,      &Name_Error},
  {&ADA$_STATUS_ERROR,    &Status_Error},
  {&ADA$_NOT_OPEN,        &Use_Error},
  {&ADA$_ALREADY_OPEN,    &Use_Error},
  {&ADA$_USE_ERROR,       &Use_Error},
  {&ADA$_UNSUPPORTED,     &Use_Error},
  {&ADA$_FAC_MODE_MISMAT, &Use_Error},
  {&ADA$_ORG_MISMATCH,    &Use_Error},
  {&ADA$_RFM_MISMATCH,    &Use_Error},
  {&ADA$_RAT_MISMATCH,    &Use_Error},
  {&ADA$_MRS_MISMATCH,    &Use_Error},
  {&ADA$_MRN_MISMATCH,    &Use_Error},
  {&ADA$_KEY_MISMATCH,    &Use_Error},
  {&ADA$_MAXLINEXC,       &constraint_error},
  {&ADA$_LINEXCMRS,       &constraint_error},
  {0,                     0}
};

#if 0
   /* Already handled by a pragma Import_Exception
      in Aux_IO_Exceptions */
  {&ADA$_LOCK_ERROR,      &Lock_Error},
  {&ADA$_EXISTENCE_ERROR, &Existence_Error},
  {&ADA$_KEY_ERROR,       &Key_Error},
#endif

#endif /* IN_RTS */

/* Non DEC Ada specific conditions. We could probably also put
   SS$_HPARITH here and possibly SS$_ACCVIO, SS$_STKOVF. */
static const struct cond_except cond_except_table [] = {
  {&MTH$_FLOOVEMAT, &constraint_error},
  {&SS$_INTDIV,     &constraint_error},
  {0,               0}
};

/* To deal with VMS conditions and their mapping to Ada exceptions,
   the __gnat_error_handler routine below is installed as an exception
   vector having precedence over DEC frame handlers.  Some conditions
   still need to be handled by such handlers, however, in which case
   __gnat_error_handler needs to return SS$_RESIGNAL.  Consider for
   instance the use of a third party library compiled with DECAda and
   performing it's own exception handling internally.

   To allow some user-level flexibility, which conditions should be
   resignaled is controlled by a predicate function, provided with the
   condition value and returning a boolean indication stating whether
   this condition should be resignaled or not.

   That predicate function is called indirectly, via a function pointer,
   by __gnat_error_handler, and changing that pointer is allowed to the
   the user code by way of the __gnat_set_resignal_predicate interface.

   The user level function may then implement what it likes, including
   for instance the maintenance of a dynamic data structure if the set
   of to be resignalled conditions has to change over the program's
   lifetime.

   ??? This is not a perfect solution to deal with the possible
   interactions between the GNAT and the DECAda exception handling
   models and better (more general) schemes are studied.  This is so
   just provided as a convenient workaround in the meantime, and
   should be use with caution since the implementation has been kept
   very simple.  */

typedef int
resignal_predicate (int code);

const int *cond_resignal_table [] = {
  &CMA$_EXIT_THREAD,
  &SS$_DEBUG,
  &LIB$_KEYNOTFOU,
  &LIB$_ACTIMAGE,
  (int *) RDB$_STREAM_EOF,
  (int *) FDL$_UNPRIKW,
  0
};

/* Default GNAT predicate for resignaling conditions.  */

static int
__gnat_default_resignal_p (int code)
{
  int i, iexcept;

  for (i = 0, iexcept = 0;
       cond_resignal_table [i] &&
       !(iexcept = LIB$MATCH_COND (&code, &cond_resignal_table [i]));
       i++);

  return iexcept;
}

/* Static pointer to predicate that the __gnat_error_handler exception
   vector invokes to determine if it should resignal a condition.  */

static resignal_predicate * __gnat_resignal_p = __gnat_default_resignal_p;

/* User interface to change the predicate pointer to PREDICATE. Reset to
   the default if PREDICATE is null.  */

void
__gnat_set_resignal_predicate (resignal_predicate * predicate)
{
  if (predicate == 0)
    __gnat_resignal_p = __gnat_default_resignal_p;
  else
    __gnat_resignal_p = predicate;
}

/* Should match System.Parameters.Default_Exception_Msg_Max_Length */
#define Default_Exception_Msg_Max_Length 512

/* Action routine for SYS$PUTMSG. There may be
   multiple conditions, each with text to be appended to
   MESSAGE and separated by line termination. */

static int
copy_msg (msgdesc, message)
     struct descriptor_s *msgdesc;
     char *message;
{
  int len = strlen (message);
  int copy_len;

  /* Check for buffer overflow and skip */
  if (len > 0 && len <= Default_Exception_Msg_Max_Length - 3)
    {
      strcat (message, "\r\n");
      len += 2;
    }

  /* Check for buffer overflow and truncate if necessary */
  copy_len = (len + msgdesc->len <= Default_Exception_Msg_Max_Length - 1 ?
	      msgdesc->len :
	      Default_Exception_Msg_Max_Length - 1 - len);
  strncpy (&message [len], msgdesc->adr, copy_len);
  message [len + copy_len] = 0;

  return 0;
}

long
__gnat_error_handler (int *sigargs, void *mechargs)
{
  struct Exception_Data *exception = 0;
  Exception_Code base_code;
  struct descriptor_s gnat_facility = {4,0,"GNAT"};
  char message [Default_Exception_Msg_Max_Length];

  const char *msg = "";
  char curr_icb[544];
  long curr_invo_handle;

  /* Check for conditions to resignal which aren't effected by pragma
     Import_Exception.  */
  if (__gnat_resignal_p (sigargs [1]))
    return SS$_RESIGNAL;

#ifdef IN_RTS
  /* See if it's an imported exception. Beware that registered exceptions
     are bound to their base code, with the severity bits masked off.  */
  base_code = Base_Code_In ((Exception_Code) sigargs [1]);
  exception = Coded_Exception (base_code);

  if (exception)
    {
      message [0] = 0;

      /* Subtract PC & PSL fields which messes with PUTMSG */
      sigargs [0] -= 2;
      SYS$PUTMSG (sigargs, copy_msg, &gnat_facility, message);
      sigargs [0] += 2;
      msg = message;

      exception->Name_Length = 19;
      /* The full name really should be get sys$getmsg returns. ??? */
      exception->Full_Name = "IMPORTED_EXCEPTION";
      exception->Import_Code = base_code;
    }
#endif

  if (exception == 0)
    switch (sigargs[1])
      {
      case SS$_ACCVIO:
        if (sigargs[3] == 0)
	  {
	    exception = &constraint_error;
	    msg = "access zero";
	  }
	else
	  {
	    exception = &storage_error;
	    msg = "stack overflow (or erroneous memory access)";
	  }
	break;

      case SS$_STKOVF:
	exception = &storage_error;
	msg = "stack overflow";
	break;

      case SS$_HPARITH:
#ifndef IN_RTS
	return SS$_RESIGNAL; /* toplev.c handles for compiler */
#else
	{
	  exception = &constraint_error;
	  msg = "arithmetic error";
	}
#endif
	break;

      default:
#ifdef IN_RTS
	{
	  int i;

	  /* Scan the DEC Ada exception condition table for a match and fetch
	     the associated GNAT exception pointer */
	  for (i = 0;
	       dec_ada_cond_except_table [i].cond &&
	       !LIB$MATCH_COND (&sigargs [1],
			        &dec_ada_cond_except_table [i].cond);
	       i++);
	  exception = (struct Exception_Data *)
	    dec_ada_cond_except_table [i].except;

	  if (!exception)
	    {
	      /* Scan the VMS standard condition table for a match and fetch
		 the associated GNAT exception pointer */
	      for (i = 0;
		   cond_except_table [i].cond &&
		   !LIB$MATCH_COND (&sigargs [1], &cond_except_table [i].cond);
		   i++);
	      exception =(struct Exception_Data *) cond_except_table [i].except;

	      if (!exception)
		/* User programs expect Non_Ada_Error to be raised, reference
		   DEC Ada test CXCONDHAN. */
		exception = &Non_Ada_Error;
	    }
	}
#else
	exception = &program_error;
#endif
	message [0] = 0;
	/* Subtract PC & PSL fields which messes with PUTMSG */
	sigargs [0] -= 2;
	SYS$PUTMSG (sigargs, copy_msg, &gnat_facility, message);
	sigargs [0] += 2;
	msg = message;
	break;
      }

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler (void)
{
  long prvhnd;
#if defined (IN_RTS) && !defined (__IA64)
  char *c;

  c = (char *) xmalloc (2049);

  __gnat_error_prehandler_stack = &c[2048];

  /* __gnat_error_prehandler is an assembly function.  */
  SYS$SETEXV (1, __gnat_error_prehandler, 3, &prvhnd);
#else
  SYS$SETEXV (1, __gnat_error_handler, 3, &prvhnd);
#endif

  __gnat_handler_installed = 1;
}

/*******************/
/* FreeBSD Section */
/*******************/

#elif defined (__FreeBSD__)

#include <signal.h>
#include <unistd.h>

static void __gnat_error_handler (int, int, struct sigcontext *);

static void
__gnat_error_handler (int sig, int code __attribute__ ((unused)),
		      struct sigcontext *sc __attribute__ ((unused)))
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;

    case SIGILL:
      exception = &constraint_error;
      msg = "SIGILL";
      break;

    case SIGSEGV:
      exception = &storage_error;
      msg = "stack overflow or erroneous memory access";
      break;

    case SIGBUS:
      exception = &constraint_error;
      msg = "SIGBUS";
      break;

    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  Raise_From_Signal_Handler (exception, msg);
}

void
__gnat_install_handler ()
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART;
  (void) sigemptyset (&act.sa_mask);

  (void) sigaction (SIGILL,  &act, NULL);
  (void) sigaction (SIGFPE,  &act, NULL);
  (void) sigaction (SIGSEGV, &act, NULL);
  (void) sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

/*******************/
/* VxWorks Section */
/*******************/

#elif defined(__vxworks)

#include <signal.h>
#include <taskLib.h>

#ifndef __RTP__
#include <intLib.h>
#include <iv.h>
#endif

#ifdef VTHREADS
#include "private/vThreadsP.h"
#endif

static void __gnat_error_handler (int, int, struct sigcontext *);
void __gnat_map_signal (int);

#ifndef __RTP__

/* Directly vectored Interrupt routines are not supported when using RTPs */

extern int __gnat_inum_to_ivec (int);

/* This is needed by the GNAT run time to handle Vxworks interrupts */
int
__gnat_inum_to_ivec (int num)
{
  return INUM_TO_IVEC (num);
}
#endif

#if !defined(__alpha_vxworks) && (_WRS_VXWORKS_MAJOR != 6) && !defined(__RTP__)

/* getpid is used by s-parint.adb, but is not defined by VxWorks, except
   on Alpha VxWorks and VxWorks 6.x (including RTPs). */

extern long getpid (void);

long
getpid (void)
{
  return taskIdSelf ();
}
#endif

/* VxWorks expects the field excCnt to be zeroed when a signal is handled.
   The VxWorks version of longjmp does this; gcc's builtin_longjmp does not */
void
__gnat_clear_exception_count (void)
{
#ifdef VTHREADS
  WIND_TCB *currentTask = (WIND_TCB *) taskIdSelf();

  currentTask->vThreads.excCnt = 0;
#endif
}

/* Exported to s-intman-vxworks.adb in order to handle different signal
   to exception mappings in different VxWorks versions */
void
__gnat_map_signal (int sig)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;
#ifdef VTHREADS
    case SIGILL:
      exception = &constraint_error;
      msg = "Floating point exception or SIGILL";
      break;
    case SIGSEGV:
      exception = &storage_error;
      msg = "SIGSEGV: possible stack overflow";
      break;
    case SIGBUS:
      exception = &storage_error;
      msg = "SIGBUS: possible stack overflow";
      break;
#else
    case SIGILL:
      exception = &constraint_error;
      msg = "SIGILL";
      break;
    case SIGSEGV:
      exception = &program_error;
      msg = "SIGSEGV";
      break;
    case SIGBUS:
      exception = &program_error;
      msg = "SIGBUS";
      break;
#endif
    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

  __gnat_clear_exception_count ();
  Raise_From_Signal_Handler (exception, msg);
}

static void
__gnat_error_handler (int sig, int code, struct sigcontext *sc)
{
  sigset_t mask;
  int result;

  /* VxWorks will always mask out the signal during the signal handler and
     will reenable it on a longjmp.  GNAT does not generate a longjmp to
     return from a signal handler so the signal will still be masked unless
     we unmask it. */
  sigprocmask (SIG_SETMASK, NULL, &mask);
  sigdelset (&mask, sig);
  sigprocmask (SIG_SETMASK, &mask, NULL);

  __gnat_map_signal (sig);

}

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Setup signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event! */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sigemptyset (&act.sa_mask);

  /* For VxWorks, install all signal handlers, since pragma Interrupt_State
     applies to vectored hardware interrupts, not signals */
  sigaction (SIGFPE,  &act, NULL);
  sigaction (SIGILL,  &act, NULL);
  sigaction (SIGSEGV, &act, NULL);
  sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

#define HAVE_GNAT_INIT_FLOAT

void
__gnat_init_float (void)
{
  /* Disable overflow/underflow exceptions on the PPC processor, this is needed
     to get correct Ada semantics.  Note that for AE653 vThreads, the HW
     overflow settings are an OS configuration issue.  The instructions
     below have no effect */
#if defined (_ARCH_PPC) && !defined (_SOFT_FLOAT) && !defined (VTHREADS)
  asm ("mtfsb0 25");
  asm ("mtfsb0 26");
#endif

  /* Similarly for sparc64. Achieved by masking bits in the Trap Enable Mask
     field of the Floating-point Status Register (see the Sparc Architecture
     Manual Version 9, p 48).  */
#if defined (sparc64)

#define FSR_TEM_NVM (1 << 27)  /* Invalid operand  */
#define FSR_TEM_OFM (1 << 26)  /* Overflow  */
#define FSR_TEM_UFM (1 << 25)  /* Underflow  */
#define FSR_TEM_DZM (1 << 24)  /* Division by Zero  */
#define FSR_TEM_NXM (1 << 23)  /* Inexact result  */
  {
    unsigned int fsr;

    __asm__("st %%fsr, %0" : "=m" (fsr));
    fsr &= ~(FSR_TEM_OFM | FSR_TEM_UFM);
    __asm__("ld %0, %%fsr" : : "m" (fsr));
  }
#endif
}

/******************/
/* NetBSD Section */
/******************/

#elif defined(__NetBSD__)

#include <signal.h>
#include <unistd.h>

static void
__gnat_error_handler (int sig)
{
  struct Exception_Data *exception;
  const char *msg;

  switch(sig)
  {
    case SIGFPE:
      exception = &constraint_error;
      msg = "SIGFPE";
      break;
    case SIGILL:
      exception = &constraint_error;
      msg = "SIGILL";
      break;
    case SIGSEGV:
      exception = &storage_error;
      msg = "stack overflow or erroneous memory access";
      break;
    case SIGBUS:
      exception = &constraint_error;
      msg = "SIGBUS";
      break;
    default:
      exception = &program_error;
      msg = "unhandled signal";
    }

    Raise_From_Signal_Handler(exception, msg);
}

void
__gnat_install_handler(void)
{
  struct sigaction act;

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System" */
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

#else

/* For all other versions of GNAT, the handler does nothing */

/*******************/
/* Default Section */
/*******************/

void
__gnat_install_handler (void)
{
  __gnat_handler_installed = 1;
}

#endif

/*********************/
/* __gnat_init_float */
/*********************/

/* This routine is called as each process thread is created, for possible
   initialization of the FP processor. This version is used under INTERIX,
   WIN32 and could be used under OS/2 */

#if defined (_WIN32) || defined (__INTERIX) || defined (__EMX__) \
  || defined (__Lynx__) || defined(__NetBSD__) || defined(__FreeBSD__)

#define HAVE_GNAT_INIT_FLOAT

void
__gnat_init_float (void)
{
#if defined (__i386__) || defined (i386)

  /* This is used to properly initialize the FPU on an x86 for each
     process thread. */

  asm ("finit");

#endif  /* Defined __i386__ */
}
#endif

#ifndef HAVE_GNAT_INIT_FLOAT

/* All targets without a specific __gnat_init_float will use an empty one */
void
__gnat_init_float (void)
{
}
#endif

/***********************************/
/* __gnat_adjust_context_for_raise */
/***********************************/

#ifndef HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

/* All targets without a specific version will use an empty one */

/* UCONTEXT is a pointer to a context structure received by a signal handler
   about to propagate an exception. Adjust it to compensate the fact that the
   generic unwinder thinks the corresponding PC is a call return address.  */

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED,
				 void *ucontext ATTRIBUTE_UNUSED)
{
  /* The point is that the interrupted context PC typically is the address
     that we should search an EH region for, which is different from the call
     return address case. The target independent part of the GCC unwinder
     don't differentiate the two situations, so we compensate here for the
     adjustments it will blindly make.

     signo is passed because on some targets for some signals the PC in
     context points to the instruction after the faulting one, in which case
     the unwinder adjustment is still desired.  */

  /* On a number of targets, we have arranged for the adjustment to be
     performed by the MD_FALLBACK_FRAME_STATE circuitry, so we don't provide a
     specific instance of this routine.  The MD_FALLBACK doesn't have access
     to the signal number, though, so the compensation is systematic there and
     might be wrong in some cases.  */

  /* Having the compensation wrong leads to potential failures.  A very
     typical case is what happens when there is no compensation and a signal
     triggers for the first instruction in a region : the unwinder adjustment
     has it search in the wrong EH region.  */
}

#endif
