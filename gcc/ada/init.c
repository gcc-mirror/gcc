/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 I N I T                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2013, Free Software Foundation, Inc.         *
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

/*  This unit contains initialization circuits that are system dependent.
    A major part of the functionality involves stack overflow checking.
    The GCC backend generates probe instructions to test for stack overflow.
    For details on the exact approach used to generate these probes, see the
    "Using and Porting GCC" manual, in particular the "Stack Checking" section
    and the subsection "Specifying How Stack Checking is Done".  The handlers
    installed by this file are used to catch the resulting signals that come
    from these probes failing (i.e. touching protected pages).  */

/* This file should be kept synchronized with 2sinit.ads, 2sinit.adb,
   s-init-ae653-cert.adb and s-init-xi-sparc.adb.  All these files implement
   the required functionality for different targets.  */

/* The following include is here to meet the published VxWorks requirement
   that the __vxworks header appear before any other include.  */
#ifdef __vxworks
#include "vxWorks.h"
#endif

#ifdef __ANDROID__
#undef linux
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>

/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"
#include "raise.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void __gnat_raise_program_error (const char *, int);

/* Addresses of exception data blocks for predefined exceptions.  Tasking_Error
   is not used in this unit, and the abort signal is only used on IRIX.
   ??? Revisit this part since IRIX is no longer supported.  */
extern struct Exception_Data constraint_error;
extern struct Exception_Data numeric_error;
extern struct Exception_Data program_error;
extern struct Exception_Data storage_error;

/* For the Cert run time we use the regular raise exception routine because
   Raise_From_Signal_Handler is not available.  */
#ifdef CERT
#define Raise_From_Signal_Handler \
                      __gnat_raise_exception
extern void Raise_From_Signal_Handler (struct Exception_Data *, const char *);
#else
#define Raise_From_Signal_Handler \
                      ada__exceptions__raise_from_signal_handler
extern void Raise_From_Signal_Handler (struct Exception_Data *, const char *);
#endif

/* Global values computed by the binder.  */
int   __gl_main_priority                 = -1;
int   __gl_main_cpu                      = -1;
int   __gl_time_slice_val                = -1;
char  __gl_wc_encoding                   = 'n';
char  __gl_locking_policy                = ' ';
char  __gl_queuing_policy                = ' ';
char  __gl_task_dispatching_policy       = ' ';
char *__gl_priority_specific_dispatching = 0;
int   __gl_num_specific_dispatching      = 0;
char *__gl_interrupt_states              = 0;
int   __gl_num_interrupt_states          = 0;
int   __gl_unreserve_all_interrupts      = 0;
int   __gl_exception_tracebacks          = 0;
int   __gl_detect_blocking               = 0;
int   __gl_default_stack_size            = -1;
int   __gl_leap_seconds_support          = 0;
int   __gl_canonical_streams             = 0;

/* This value is not used anymore, but kept for bootstrapping purpose.  */
int   __gl_zero_cost_exceptions          = 0;

/* Indication of whether synchronous signal handler has already been
   installed by a previous call to adainit.  */
int  __gnat_handler_installed      = 0;

#ifndef IN_RTS
int __gnat_inside_elab_final_code = 0;
/* ??? This variable is obsolete since 2001-08-29 but is kept to allow
   bootstrap from old GNAT versions (< 3.15).  */
#endif

/* HAVE_GNAT_INIT_FLOAT must be set on every targets where a __gnat_init_float
   is defined.  If this is not set then a void implementation will be defined
   at the end of this unit.  */
#undef HAVE_GNAT_INIT_FLOAT

/******************************/
/* __gnat_get_interrupt_state */
/******************************/

char __gnat_get_interrupt_state (int);

/* This routine is called from the runtime as needed to determine the state
   of an interrupt, as set by an Interrupt_State pragma appearing anywhere
   in the current partition.  The input argument is the interrupt number,
   and the result is one of the following:

       'n'   this interrupt not set by any Interrupt_State pragma
       'u'   Interrupt_State pragma set state to User
       'r'   Interrupt_State pragma set state to Runtime
       's'   Interrupt_State pragma set state to System  */

char
__gnat_get_interrupt_state (int intrup)
{
  if (intrup >= __gl_num_interrupt_states)
    return 'n';
  else
    return __gl_interrupt_states [intrup];
}

/***********************************/
/* __gnat_get_specific_dispatching */
/***********************************/

char __gnat_get_specific_dispatching (int);

/* This routine is called from the runtime as needed to determine the
   priority specific dispatching policy, as set by a
   Priority_Specific_Dispatching pragma appearing anywhere in the current
   partition.  The input argument is the priority number, and the result
   is the upper case first character of the policy name, e.g. 'F' for
   FIFO_Within_Priorities. A space ' ' is returned if no
   Priority_Specific_Dispatching pragma is used in the partition.  */

char
__gnat_get_specific_dispatching (int priority)
{
  if (__gl_num_specific_dispatching == 0)
    return ' ';
  else if (priority >= __gl_num_specific_dispatching)
    return 'F';
  else
    return __gl_priority_specific_dispatching [priority];
}

#ifndef IN_RTS

/**********************/
/* __gnat_set_globals */
/**********************/

/* This routine is kept for bootstrapping purposes, since the binder generated
   file now sets the __gl_* variables directly.  */

void
__gnat_set_globals (void)
{
}

#endif

/***************/
/* AIX Section */
/***************/

#if defined (_AIX)

#include <signal.h>
#include <sys/time.h>

/* Some versions of AIX don't define SA_NODEFER.  */

#ifndef SA_NODEFER
#define SA_NODEFER 0
#endif /* SA_NODEFER */

/* Versions of AIX before 4.3 don't have nanosleep but provide
   nsleep instead.  */

#ifndef _AIXVERSION_430

extern int nanosleep (struct timestruc_t *, struct timestruc_t *);

int
nanosleep (struct timestruc_t *Rqtp, struct timestruc_t *Rmtp)
{
  return nsleep (Rqtp, Rmtp);
}

#endif /* _AIXVERSION_430 */

static void
__gnat_error_handler (int sig,
		      siginfo_t *si ATTRIBUTE_UNUSED,
		      void *ucontext ATTRIBUTE_UNUSED)
{
  struct Exception_Data *exception;
  const char *msg;

  switch (sig)
    {
    case SIGSEGV:
      /* FIXME: we need to detect the case of a *real* SIGSEGV.  */
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
     signal that might cause a scheduling event!  */

  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  act.sa_sigaction = __gnat_error_handler;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System".  */
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
/* HP-UX section */
/*****************/

#elif defined (__hpux__)

#include <signal.h>
#include <sys/ucontext.h>

#if defined (IN_RTS) && defined (__ia64__)

#include <sys/uc_access.h>

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED, void *ucontext)
{
  ucontext_t *uc = (ucontext_t *) ucontext;
  uint64_t ip;

  /* Adjust on itanium, as GetIPInfo is not supported.  */
  __uc_get_ip (uc, &ip);
  __uc_set_ip (uc, ip + 1);
}
#endif /* IN_RTS && __ia64__ */

/* Tasking and Non-tasking signal handler.  Map SIGnal to Ada exception
   propagation after the required low level adjustments.  */

static void
__gnat_error_handler (int sig,
		      siginfo_t *si ATTRIBUTE_UNUSED,
		      void *ucontext ATTRIBUTE_UNUSED)
{
  struct Exception_Data *exception;
  const char *msg;

  __gnat_adjust_context_for_raise (sig, ucontext);

  switch (sig)
    {
    case SIGSEGV:
      /* FIXME: we need to detect the case of a *real* SIGSEGV.  */
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

/* This must be in keeping with System.OS_Interface.Alternate_Stack_Size.  */
#if defined (__hppa__)
char __gnat_alternate_stack[16 * 1024]; /* 2 * SIGSTKSZ */
#else
char __gnat_alternate_stack[128 * 1024]; /* MINSIGSTKSZ */
#endif

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event!  Also setup an alternate
     stack region for the handler execution so that stack overflows can be
     handled properly, avoiding a SEGV generation from stack usage by the
     handler itself.  */

  stack_t stack;
  stack.ss_sp = __gnat_alternate_stack;
  stack.ss_size = sizeof (__gnat_alternate_stack);
  stack.ss_flags = 0;
  sigaltstack (&stack, NULL);

  act.sa_sigaction = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System".  */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);
  act.sa_flags |= SA_ONSTACK;
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);

  __gnat_handler_installed = 1;
}

/*********************/
/* GNU/Linux Section */
/*********************/

#elif defined (linux)

#include <signal.h>

#define __USE_GNU 1 /* required to get REG_EIP/RIP from glibc's ucontext.h */
#include <sys/ucontext.h>

/* GNU/Linux, which uses glibc, does not define NULL in included
   header files.  */

#if !defined (NULL)
#define NULL ((void *) 0)
#endif

#if defined (MaRTE)

/* MaRTE OS provides its own version of sigaction, sigfillset, and
   sigemptyset (overriding these symbol names).  We want to make sure that
   the versions provided by the underlying C library are used here (these
   versions are renamed by MaRTE to linux_sigaction, fake_linux_sigfillset,
   and fake_linux_sigemptyset, respectively).  The MaRTE library will not
   always be present (it will not be linked if no tasking constructs are
   used), so we use the weak symbol mechanism to point always to the symbols
   defined within the C library.  */

#pragma weak linux_sigaction
int linux_sigaction (int signum, const struct sigaction *act,
		     struct sigaction *oldact) {
  return sigaction (signum, act, oldact);
}
#define sigaction(signum, act, oldact) linux_sigaction (signum, act, oldact)

#pragma weak fake_linux_sigfillset
void fake_linux_sigfillset (sigset_t *set) {
  sigfillset (set);
}
#define sigfillset(set) fake_linux_sigfillset (set)

#pragma weak fake_linux_sigemptyset
void fake_linux_sigemptyset (sigset_t *set) {
  sigemptyset (set);
}
#define sigemptyset(set) fake_linux_sigemptyset (set)

#endif

#if defined (i386) || defined (__x86_64__) || defined (__ia64__)

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED, void *ucontext)
{
  mcontext_t *mcontext = &((ucontext_t *) ucontext)->uc_mcontext;

  /* On the i386 and x86-64 architectures, stack checking is performed by
     means of probes with moving stack pointer, that is to say the probed
     address is always the value of the stack pointer.  Upon hitting the
     guard page, the stack pointer therefore points to an inaccessible
     address and an alternate signal stack is needed to run the handler.
     But there is an additional twist: on these architectures, the EH
     return code writes the address of the handler at the target CFA's
     value on the stack before doing the jump.  As a consequence, if
     there is an active handler in the frame whose stack has overflowed,
     the stack pointer must nevertheless point to an accessible address
     by the time the EH return is executed.

     We therefore adjust the saved value of the stack pointer by the size
     of one page + a small dope of 4 words, in order to make sure that it
     points to an accessible address in case it's used as the target CFA.
     The stack checking code guarantees that this address is unused by the
     time this happens.  */

#if defined (i386)
  unsigned long *pc = (unsigned long *)mcontext->gregs[REG_EIP];
  /* The pattern is "orl $0x0,(%esp)" for a probe in 32-bit mode.  */
  if (signo == SIGSEGV && pc && *pc == 0x00240c83)
    mcontext->gregs[REG_ESP] += 4096 + 4 * sizeof (unsigned long);
#elif defined (__x86_64__)
  unsigned long long *pc = (unsigned long long *)mcontext->gregs[REG_RIP];
  if (signo == SIGSEGV && pc
      /* The pattern is "orq $0x0,(%rsp)" for a probe in 64-bit mode.  */
      && ((*pc & 0xffffffffffLL) == 0x00240c8348LL
	  /* The pattern may also be "orl $0x0,(%esp)" for a probe in
	     x32 mode.  */
	  || (*pc & 0xffffffffLL) == 0x00240c83LL))
    mcontext->gregs[REG_RSP] += 4096 + 4 * sizeof (unsigned long);
#elif defined (__ia64__)
  /* ??? The IA-64 unwinder doesn't compensate for signals.  */
  mcontext->sc_ip++;
#endif
}

#endif

static void
__gnat_error_handler (int sig, siginfo_t *si ATTRIBUTE_UNUSED, void *ucontext)
{
  struct Exception_Data *exception;
  const char *msg;

  /* Adjusting is required for every fault context, so adjust for this one
     now, before we possibly trigger a recursive fault below.  */
  __gnat_adjust_context_for_raise (sig, ucontext);

  switch (sig)
    {
    case SIGSEGV:
      /* Here we would like a discrimination test to see whether the page
	 before the faulting address is accessible.  Unfortunately, Linux
	 seems to have no way of giving us the faulting address.

	 In old versions of init.c, we had a test of the page before the
	 stack pointer:

	   ((volatile char *)
	    ((long) si->esp_at_signal & - getpagesize ()))[getpagesize ()];

	 but that's wrong since it tests the stack pointer location and the
	 stack probing code may not move it until all probes succeed.

	 For now we simply do not attempt any discrimination at all. Note
	 that this is quite acceptable, since a "real" SIGSEGV can only
	 occur as the result of an erroneous program.  */
      exception = &storage_error;
      msg = "stack overflow or erroneous memory access";
      break;

    case SIGBUS:
      exception = &storage_error;
      msg = "SIGBUS: possible stack overflow";
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

#if defined (i386) || defined (__x86_64__) || defined (__powerpc__)
/* This must be in keeping with System.OS_Interface.Alternate_Stack_Size.  */
char __gnat_alternate_stack[16 * 1024]; /* 2 * SIGSTKSZ */
#endif

#ifdef __XENO__
#include <sys/mman.h>
#include <native/task.h>

RT_TASK main_task;
#endif

void
__gnat_install_handler (void)
{
  struct sigaction act;

#ifdef __XENO__
  int prio;

  if (__gl_main_priority == -1)
    prio = 49;
  else
    prio = __gl_main_priority;

  /* Avoid memory swapping for this program */

  mlockall (MCL_CURRENT|MCL_FUTURE);

  /* Turn the current Linux task into a native Xenomai task */

  rt_task_shadow(&main_task, "environment_task", prio, T_FPU);
#endif

  /* Set up signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event!  Also setup an alternate
     stack region for the handler execution so that stack overflows can be
     handled properly, avoiding a SEGV generation from stack usage by the
     handler itself.  */

  act.sa_sigaction = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System".  */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    {
#if defined (i386) || defined (__x86_64__) || defined (__powerpc__)
      /* Setup an alternate stack region for the handler execution so that
	 stack overflows can be handled properly, avoiding a SEGV generation
	 from stack usage by the handler itself.  */
      stack_t stack;

      stack.ss_sp = __gnat_alternate_stack;
      stack.ss_size = sizeof (__gnat_alternate_stack);
      stack.ss_flags = 0;
      sigaltstack (&stack, NULL);

      act.sa_flags |= SA_ONSTACK;
#endif
      sigaction (SIGSEGV, &act, NULL);
    }

  __gnat_handler_installed = 1;
}

/*******************/
/* LynxOS Section */
/*******************/

#elif defined (__Lynx__)

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
  act.sa_flags = 0x0;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System".  */
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
/* Solaris Section */
/*******************/

#elif defined (sun) && defined (__SVR4) && !defined (__vxworks)

#include <signal.h>
#include <siginfo.h>
#include <sys/ucontext.h>
#include <sys/regset.h>

static void
__gnat_error_handler (int sig, siginfo_t *si, void *ucontext ATTRIBUTE_UNUSED)
{
  struct Exception_Data *exception;
  static int recurse = 0;
  const char *msg;

  switch (sig)
    {
    case SIGSEGV:
      /* If the problem was permissions, this is a constraint error.
	 Likewise if the failing address isn't maximally aligned or if
	 we've recursed.

	 ??? Using a static variable here isn't task-safe, but it's
	 much too hard to do anything else and we're just determining
	 which exception to raise.  */
      if (si->si_code == SEGV_ACCERR
	  || (long) si->si_addr == 0
	  || (((long) si->si_addr) & 3) != 0
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
	   ((long) si->si_addr & - getpagesize ()))[getpagesize ()];
	  exception = &storage_error;
	  msg = "stack overflow or erroneous memory access";
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
     signal that might cause a scheduling event!  */

  act.sa_sigaction = __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System".  */
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

/* Routine called from binder to override default feature values. */
void __gnat_set_features (void);
int __gnat_features_set = 0;
void (*__gnat_ctrl_c_handler) (void) = 0;

#ifdef __IA64
#define lib_get_curr_invo_context LIB$I64_GET_CURR_INVO_CONTEXT
#define lib_get_prev_invo_context LIB$I64_GET_PREV_INVO_CONTEXT
#define lib_get_invo_handle LIB$I64_GET_INVO_HANDLE
#else
#define lib_get_curr_invo_context LIB$GET_CURR_INVO_CONTEXT
#define lib_get_prev_invo_context LIB$GET_PREV_INVO_CONTEXT
#define lib_get_invo_handle LIB$GET_INVO_HANDLE
#endif

/* Masks for facility identification. */
#define FAC_MASK  		0x0fff0000
#define DECADA_M_FACILITY	0x00310000

/* Define macro symbols for the VMS conditions that become Ada exceptions.
   It would be better to just include <ssdef.h> */

#define SS$_CONTINUE           1
#define SS$_ACCVIO            12
#define SS$_HPARITH         1284
#define SS$_INTDIV          1156
#define SS$_STKOVF          1364
#define SS$_CONTROLC        1617
#define SS$_RESIGNAL        2328

#define MTH$_FLOOVEMAT   1475268       /* Some ACVC_21 CXA tests */

/* The following codes must be resignalled, and not handled here. */

/* These codes are in standard message libraries.  */
extern int C$_SIGKILL;
extern int C$_SIGINT;
extern int SS$_DEBUG;
extern int LIB$_KEYNOTFOU;
extern int LIB$_ACTIMAGE;

/* These codes are non standard, which is to say the author is
   not sure if they are defined in the standard message libraries
   so keep them as macros for now.  */
#define RDB$_STREAM_EOF 20480426
#define FDL$_UNPRIKW 11829410
#define CMA$_EXIT_THREAD 4227492

struct cond_sigargs
{
  unsigned int sigarg;
  unsigned int sigargval;
};

struct cond_subtests
{
  unsigned int num;
  const struct cond_sigargs sigargs[];
};

struct cond_except
{
  unsigned int cond;
  const struct Exception_Data *except;
  unsigned int needs_adjust;  /* 1 = adjust PC,  0 = no adjust */
  const struct cond_subtests *subtests;
};

struct descriptor_s
{
  unsigned short len, mbz;
  __char_ptr32 adr;
};

/* Conditions that don't have an Ada exception counterpart must raise
   Non_Ada_Error.  Since this is defined in s-auxdec, it should only be
   referenced by user programs, not the compiler or tools.  Hence the
   #ifdef IN_RTS.  */

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
   must be declared.  */

#define ADA$_ALREADY_OPEN	0x0031a594
#define ADA$_CONSTRAINT_ERRO	0x00318324
#define ADA$_DATA_ERROR		0x003192c4
#define ADA$_DEVICE_ERROR	0x003195e4
#define ADA$_END_ERROR		0x00319904
#define ADA$_FAC_MODE_MISMAT	0x0031a8b3
#define ADA$_IOSYSFAILED	0x0031af04
#define ADA$_KEYSIZERR		0x0031aa3c
#define ADA$_KEY_MISMATCH	0x0031a8e3
#define ADA$_LAYOUT_ERROR	0x00319c24
#define ADA$_LINEXCMRS		0x0031a8f3
#define ADA$_MAXLINEXC		0x0031a8eb
#define ADA$_MODE_ERROR		0x00319f44
#define ADA$_MRN_MISMATCH	0x0031a8db
#define ADA$_MRS_MISMATCH	0x0031a8d3
#define ADA$_NAME_ERROR		0x0031a264
#define ADA$_NOT_OPEN		0x0031a58c
#define ADA$_ORG_MISMATCH	0x0031a8bb
#define ADA$_PROGRAM_ERROR	0x00318964
#define ADA$_RAT_MISMATCH	0x0031a8cb
#define ADA$_RFM_MISMATCH	0x0031a8c3
#define ADA$_STAOVF		0x00318cac
#define ADA$_STATUS_ERROR	0x0031a584
#define ADA$_STORAGE_ERROR	0x00318c84
#define ADA$_UNSUPPORTED	0x0031a8ab
#define ADA$_USE_ERROR		0x0031a8a4

/* DEC Ada specific conditions.  */
static const struct cond_except dec_ada_cond_except_table [] =
{
  {ADA$_PROGRAM_ERROR,   &program_error, 0, 0},
  {ADA$_USE_ERROR,       &Use_Error, 0, 0},
  {ADA$_KEYSIZERR,       &program_error, 0, 0},
  {ADA$_STAOVF,          &storage_error, 0, 0},
  {ADA$_CONSTRAINT_ERRO, &constraint_error, 0, 0},
  {ADA$_IOSYSFAILED,     &Device_Error, 0, 0},
  {ADA$_LAYOUT_ERROR,    &Layout_Error, 0, 0},
  {ADA$_STORAGE_ERROR,   &storage_error, 0, 0},
  {ADA$_DATA_ERROR,      &Data_Error, 0, 0},
  {ADA$_DEVICE_ERROR,    &Device_Error, 0, 0},
  {ADA$_END_ERROR,       &End_Error, 0, 0},
  {ADA$_MODE_ERROR,      &Mode_Error, 0, 0},
  {ADA$_NAME_ERROR,      &Name_Error, 0, 0},
  {ADA$_STATUS_ERROR,    &Status_Error, 0, 0},
  {ADA$_NOT_OPEN,        &Use_Error, 0, 0},
  {ADA$_ALREADY_OPEN,    &Use_Error, 0, 0},
  {ADA$_USE_ERROR,       &Use_Error, 0, 0},
  {ADA$_UNSUPPORTED,     &Use_Error, 0, 0},
  {ADA$_FAC_MODE_MISMAT, &Use_Error, 0, 0},
  {ADA$_ORG_MISMATCH,    &Use_Error, 0, 0},
  {ADA$_RFM_MISMATCH,    &Use_Error, 0, 0},
  {ADA$_RAT_MISMATCH,    &Use_Error, 0, 0},
  {ADA$_MRS_MISMATCH,    &Use_Error, 0, 0},
  {ADA$_MRN_MISMATCH,    &Use_Error, 0, 0},
  {ADA$_KEY_MISMATCH,    &Use_Error, 0, 0},
  {ADA$_MAXLINEXC,       &constraint_error, 0, 0},
  {ADA$_LINEXCMRS,       &constraint_error, 0, 0},

#if 0
   /* Already handled by a pragma Import_Exception
      in Aux_IO_Exceptions */
  {ADA$_LOCK_ERROR,      &Lock_Error, 0, 0},
  {ADA$_EXISTENCE_ERROR, &Existence_Error, 0, 0},
  {ADA$_KEY_ERROR,       &Key_Error, 0, 0},
#endif

  {0,                    0, 0, 0}
};

#endif /* IN_RTS */

/* Non-DEC Ada specific conditions that map to Ada exceptions.  */

/* Subtest for ACCVIO Constraint_Error, kept for compatibility,
   in hindsight should have just made ACCVIO == Storage_Error.  */
#define ACCVIO_VIRTUAL_ADDR 3
static const struct cond_subtests accvio_c_e =
{1,  /* number of subtests below */
  {
     { ACCVIO_VIRTUAL_ADDR, 0 }
   }
};

/* Macro flag to adjust PC which gets off by one for some conditions,
   not sure if this is reliably true, PC could be off by more for
   HPARITH for example, unless a trapb is inserted. */
#define NEEDS_ADJUST 1

static const struct cond_except system_cond_except_table [] =
{
  {MTH$_FLOOVEMAT, &constraint_error, 0, 0},
  {SS$_INTDIV,     &constraint_error, 0, 0},
  {SS$_HPARITH,    &constraint_error, NEEDS_ADJUST, 0},
  {SS$_ACCVIO,     &constraint_error, NEEDS_ADJUST, &accvio_c_e},
  {SS$_ACCVIO,     &storage_error,    NEEDS_ADJUST, 0},
  {SS$_STKOVF,     &storage_error,    NEEDS_ADJUST, 0},
  {0,               0, 0, 0}
};

/* To deal with VMS conditions and their mapping to Ada exceptions,
   the __gnat_error_handler routine below is installed as an exception
   vector having precedence over DEC frame handlers.  Some conditions
   still need to be handled by such handlers, however, in which case
   __gnat_error_handler needs to return SS$_RESIGNAL.  Consider for
   instance the use of a third party library compiled with DECAda and
   performing its own exception handling internally.

   To allow some user-level flexibility, which conditions should be
   resignaled is controlled by a predicate function, provided with the
   condition value and returning a boolean indication stating whether
   this condition should be resignaled or not.

   That predicate function is called indirectly, via a function pointer,
   by __gnat_error_handler, and changing that pointer is allowed to the
   user code by way of the __gnat_set_resignal_predicate interface.

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

static const int * const cond_resignal_table [] =
{
  &C$_SIGKILL,
  (int *)CMA$_EXIT_THREAD,
  &SS$_DEBUG,
  &LIB$_KEYNOTFOU,
  &LIB$_ACTIMAGE,
  (int *) RDB$_STREAM_EOF,
  (int *) FDL$_UNPRIKW,
  0
};

static const int facility_resignal_table [] =
{
  0x1380000, /* RDB */
  0x2220000, /* SQL */
  0
};

/* Default GNAT predicate for resignaling conditions.  */

static int
__gnat_default_resignal_p (int code)
{
  int i, iexcept;

  for (i = 0; facility_resignal_table [i]; i++)
    if ((code & FAC_MASK) == facility_resignal_table [i])
      return 1;

  for (i = 0, iexcept = 0;
       cond_resignal_table [i]
	&& !(iexcept = LIB$MATCH_COND (&code, &cond_resignal_table [i]));
       i++);

  return iexcept;
}

/* Static pointer to predicate that the __gnat_error_handler exception
   vector invokes to determine if it should resignal a condition.  */

static resignal_predicate *__gnat_resignal_p = __gnat_default_resignal_p;

/* User interface to change the predicate pointer to PREDICATE. Reset to
   the default if PREDICATE is null.  */

void
__gnat_set_resignal_predicate (resignal_predicate *predicate)
{
  if (predicate == NULL)
    __gnat_resignal_p = __gnat_default_resignal_p;
  else
    __gnat_resignal_p = predicate;
}

/* Should match System.Parameters.Default_Exception_Msg_Max_Length.  */
#define Default_Exception_Msg_Max_Length 512

/* Action routine for SYS$PUTMSG. There may be multiple
   conditions, each with text to be appended to MESSAGE
   and separated by line termination.  */
static int
copy_msg (struct descriptor_s *msgdesc, char *message)
{
  int len = strlen (message);
  int copy_len;

  /* Check for buffer overflow and skip.  */
  if (len > 0 && len <= Default_Exception_Msg_Max_Length - 3)
    {
      strcat (message, "\r\n");
      len += 2;
    }

  /* Check for buffer overflow and truncate if necessary.  */
  copy_len = (len + msgdesc->len <= Default_Exception_Msg_Max_Length - 1 ?
	      msgdesc->len :
	      Default_Exception_Msg_Max_Length - 1 - len);
  strncpy (&message [len], msgdesc->adr, copy_len);
  message [len + copy_len] = 0;

  return 0;
}

/* Scan TABLE for a match for the condition contained in SIGARGS,
   and return the entry, or the empty entry if no match found.  */
static const struct cond_except *
  scan_conditions ( int *sigargs, const struct cond_except *table [])
{
  int i;
  struct cond_except entry;

  /* Scan the exception condition table for a match and fetch
     the associated GNAT exception pointer.  */
  for (i = 0; (*table) [i].cond; i++)
    {
      unsigned int match = LIB$MATCH_COND (&sigargs [1], &(*table) [i].cond);
      const struct cond_subtests *subtests  = (*table) [i].subtests;

      if (match)
	{
	  if (!subtests)
	    {
	      return &(*table) [i];
	    }
	  else
	    {
	      unsigned int ii;
	      int num = (*subtests).num;

	      /* Perform subtests to differentiate exception.  */
	      for (ii = 0; ii < num; ii++)
		{
		  unsigned int arg = (*subtests).sigargs [ii].sigarg;
		  unsigned int argval = (*subtests).sigargs [ii].sigargval;

		  if (sigargs [arg] != argval)
		    {
		      num = 0;
		      break;
		    }
		}

	      /* All subtests passed.  */
	      if (num == (*subtests).num)
	        return &(*table) [i];
	    }
	}
    }

    /* No match, return the null terminating entry.  */
    return &(*table) [i];
}

/* __gnat_handle_vms_condtition is both a frame based handler
   for the runtime, and an exception vector for the compiler.  */
long
__gnat_handle_vms_condition (int *sigargs, void *mechargs)
{
  struct Exception_Data *exception = 0;
  unsigned int needs_adjust = 0;
  Exception_Code base_code;
  struct descriptor_s gnat_facility = {4, 0, "GNAT"};
  char message [Default_Exception_Msg_Max_Length];

  const char *msg = "";

  /* Check for conditions to resignal which aren't effected by pragma
     Import_Exception.  */
  if (__gnat_resignal_p (sigargs [1]))
    return SS$_RESIGNAL;
#ifndef IN_RTS
  /* toplev.c handles this for compiler.  */
  if (sigargs [1] == SS$_HPARITH)
    return SS$_RESIGNAL;
#endif

#ifdef IN_RTS
  /* See if it's an imported exception.  Beware that registered exceptions
     are bound to their base code, with the severity bits masked off.  */
  base_code = Base_Code_In ((Exception_Code) sigargs[1]);
  exception = Coded_Exception (base_code);
#endif

  if (exception == 0)
#ifdef IN_RTS
    {
      int i;
      struct cond_except cond;
      const struct cond_except *cond_table;
      const struct cond_except *cond_tables [] = {dec_ada_cond_except_table,
					          system_cond_except_table,
					          0};
      unsigned int ctrlc = SS$_CONTROLC;
      unsigned int *sigint = &C$_SIGINT;
      int ctrlc_match = LIB$MATCH_COND (&sigargs [1], &ctrlc);
      int sigint_match = LIB$MATCH_COND (&sigargs [1], &sigint);

      extern int SYS$DCLAST (void (*astadr)(), unsigned long long astprm,
	                     unsigned int acmode);

      /* If SS$_CONTROLC has been imported as an exception, it will take
	 priority over a a Ctrl/C handler.  See above.  SIGINT has a
	 different condition value due to it's DECCCRTL roots and it's
	 the condition that gets raised for a "kill -INT".  */
      if ((ctrlc_match || sigint_match) && __gnat_ctrl_c_handler)
	{
	  SYS$DCLAST (__gnat_ctrl_c_handler, 0, 0);
	  return SS$_CONTINUE;
	}

      i = 0;
      while ((cond_table = cond_tables[i++]) && !exception)
	{
	  cond = *scan_conditions (sigargs, &cond_table);
	  exception = (struct Exception_Data *) cond.except;
	}

      if (exception)
	needs_adjust = cond.needs_adjust;
      else
	/* User programs expect Non_Ada_Error to be raised if no match,
	   reference DEC Ada test CXCONDHAN.  */
	exception = &Non_Ada_Error;
      }
#else
    {
      /* Pretty much everything is just a program error in the compiler */
      exception = &program_error;
    }
#endif

  message[0] = 0;
  /* Subtract PC & PSL fields as per ABI for SYS$PUTMSG.  */
  sigargs[0] -= 2;

  extern int SYS$PUTMSG (void *, int (*)(), void *, unsigned long long);

  /* If it was a DEC Ada specific condtiion, make it GNAT otherwise
     keep the old facility.  */
  if (sigargs [1] & FAC_MASK == DECADA_M_FACILITY)
    SYS$PUTMSG (sigargs, copy_msg, &gnat_facility,
	        (unsigned long long ) message);
  else
    SYS$PUTMSG (sigargs, copy_msg, 0,
	        (unsigned long long ) message);

  /* Add back PC & PSL fields as per ABI for SYS$PUTMSG.  */
  sigargs[0] += 2;
  msg = message;

  if (needs_adjust)
    __gnat_adjust_context_for_raise (sigargs [1], (void *)mechargs);

  Raise_From_Signal_Handler (exception, msg);
}

#if defined (IN_RTS) && defined (__IA64)
/* Called only from adasigio.b32.  This is a band aid to avoid going
   through the VMS signal handling code which results in a 0x8000 per
   handled exception memory leak in P2 space (see VMS source listing
   sys/lis/exception.lis) due to the allocation of working space that
   is expected to be deallocated upon return from the condition handler,
   which doesn't return in GNAT compiled code.  */
void
GNAT$STOP (int *sigargs)
{
   /* Note that there are no mechargs. We rely on the fact that condtions
      raised from DEClib I/O do not require an "adjust".  Also the count
      will be off by 2, since LIB$STOP didn't get a chance to add the
      PC and PSL fields, so we bump it so PUTMSG comes out right.  */
   sigargs [0] += 2;
   __gnat_handle_vms_condition (sigargs, 0);
}
#endif

void
__gnat_install_handler (void)
{
  long prvhnd ATTRIBUTE_UNUSED;

#if !defined (IN_RTS)
  extern int SYS$SETEXV (unsigned int vector, int (*addres)(),
	                 unsigned int accmode, void *(*(prvhnd)));
  SYS$SETEXV (1, __gnat_handle_vms_condition, 3, &prvhnd);
#endif

  __gnat_handler_installed = 1;
}

/* __gnat_adjust_context_for_raise for Alpha - see comments along with the
   default version later in this file.  */

#if defined (IN_RTS) && defined (__alpha__)

#include <vms/chfctxdef.h>
#include <vms/chfdef.h>

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED, void *ucontext)
{
  if (signo == SS$_HPARITH)
    {
      /* Sub one to the address of the instruction signaling the condition,
	 located in the sigargs array.  */

      CHF$MECH_ARRAY * mechargs = (CHF$MECH_ARRAY *) ucontext;
      CHF$SIGNAL_ARRAY * sigargs
	= (CHF$SIGNAL_ARRAY *) mechargs->chf$q_mch_sig_addr;

      int vcount = sigargs->chf$is_sig_args;
      int * pc_slot = & (&sigargs->chf$l_sig_name)[vcount-2];

      (*pc_slot)--;
    }
}

#endif

/* __gnat_adjust_context_for_raise for ia64.  */

#if defined (IN_RTS) && defined (__IA64)

#include <vms/chfctxdef.h>
#include <vms/chfdef.h>

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

typedef unsigned long long u64;

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED, void *ucontext)
{
  /* Add one to the address of the instruction signaling the condition,
     located in the 64bits sigargs array.  */

  CHF$MECH_ARRAY * mechargs = (CHF$MECH_ARRAY *) ucontext;

  CHF64$SIGNAL_ARRAY *chfsig64
    = (CHF64$SIGNAL_ARRAY *) mechargs->chf$ph_mch_sig64_addr;

  u64 * post_sigarray
    = (u64 *)chfsig64 + 1 + chfsig64->chf64$l_sig_args;

  u64 * ih_pc_loc = post_sigarray - 2;

  (*ih_pc_loc) ++;
}

#endif

/* Easier interface for LIB$GET_LOGICAL: put the equivalence of NAME into BUF,
   always NUL terminated.  In case of error or if the result is longer than
   LEN (length of BUF) an empty string is written info BUF.  */

static void
__gnat_vms_get_logical (const char *name, char *buf, int len)
{
  struct descriptor_s name_desc, result_desc;
  int status;
  unsigned short rlen;

  /* Build the descriptor for NAME.  */
  name_desc.len = strlen (name);
  name_desc.mbz = 0;
  name_desc.adr = (char *)name;

  /* Build the descriptor for the result.  */
  result_desc.len = len;
  result_desc.mbz = 0;
  result_desc.adr = buf;

  status = LIB$GET_LOGICAL (&name_desc, &result_desc, &rlen);

  if ((status & 1) == 1 && rlen < len)
    buf[rlen] = 0;
  else
    buf[0] = 0;
}

/* Size of a page on ia64 and alpha VMS.  */
#define VMS_PAGESIZE 8192

/* User mode.  */
#define PSL__C_USER 3

/* No access.  */
#define PRT__C_NA 0

/* Descending region.  */
#define VA__M_DESCEND 1

/* Get by virtual address.  */
#define VA___REGSUM_BY_VA 1

/* Memory region summary.  */
struct regsum
{
  unsigned long long q_region_id;
  unsigned int l_flags;
  unsigned int l_region_protection;
  void *pq_start_va;
  unsigned long long q_region_size;
  void *pq_first_free_va;
};

extern int SYS$GET_REGION_INFO (unsigned int, unsigned long long *,
	                        void *, void *, unsigned int,
	                        void *, unsigned int *);
extern int SYS$EXPREG_64 (unsigned long long *, unsigned long long,
	                  unsigned int, unsigned int, void **,
	                  unsigned long long *);
extern int SYS$SETPRT_64 (void *, unsigned long long, unsigned int,
	                  unsigned int, void **, unsigned long long *,
	                  unsigned int *);

/* Add a guard page in the memory region containing ADDR at ADDR +/- SIZE.
   (The sign depends on the kind of the memory region).  */

static int
__gnat_set_stack_guard_page (void *addr, unsigned long size)
{
  int status;
  void *ret_va;
  unsigned long long ret_len;
  unsigned int ret_prot;
  void *start_va;
  unsigned long long length;
  unsigned int retlen;
  struct regsum buffer;

  /* Get the region for ADDR.  */
  status = SYS$GET_REGION_INFO
    (VA___REGSUM_BY_VA, NULL, addr, NULL, sizeof (buffer), &buffer, &retlen);

  if ((status & 1) != 1)
    return -1;

  /* Extend the region.  */
  status = SYS$EXPREG_64 (&buffer.q_region_id,
	                  size, 0, 0, &start_va, &length);

  if ((status & 1) != 1)
    return -1;

  /* Create a guard page.  */
  if (!(buffer.l_flags & VA__M_DESCEND))
    start_va = (void *)((unsigned long long)start_va + length - VMS_PAGESIZE);

  status = SYS$SETPRT_64 (start_va, VMS_PAGESIZE, PSL__C_USER, PRT__C_NA,
	                  &ret_va, &ret_len, &ret_prot);

  if ((status & 1) != 1)
    return -1;
  return 0;
}

/* Read logicals to limit the stack(s) size.  */

static void
__gnat_set_stack_limit (void)
{
#ifdef __ia64__
  void *sp;
  unsigned long size;
  char value[16];
  char *e;

  /* The main stack.  */
  __gnat_vms_get_logical ("GNAT_STACK_SIZE", value, sizeof (value));
  size = strtoul (value, &e, 0);
  if (e > value && *e == 0)
    {
      asm ("mov %0=sp" : "=r" (sp));
      __gnat_set_stack_guard_page (sp, size * 1024);
    }

  /* The register stack.  */
  __gnat_vms_get_logical ("GNAT_RBS_SIZE", value, sizeof (value));
  size = strtoul (value, &e, 0);
  if (e > value && *e == 0)
    {
      asm ("mov %0=ar.bsp" : "=r" (sp));
      __gnat_set_stack_guard_page (sp, size * 1024);
    }
#endif
}

/* Feature logical name and global variable address pair.
   If we ever add another feature logical to this list, the
   feature struct will need to be enhanced to take into account
   possible values for *gl_addr.  */
struct feature {
  const char *name;
  int *gl_addr;
};

/* Default values for GNAT features set by environment.  */
int __gl_heap_size = 64;

/* Array feature logical names and global variable addresses.  */
static const struct feature features[] =
{
  {"GNAT$NO_MALLOC_64", &__gl_heap_size},
  {0, 0}
};

void
__gnat_set_features (void)
{
  int i;
  char buff[16];

  /* Loop through features array and test name for enable/disable.  */
  for (i = 0; features[i].name; i++)
    {
      __gnat_vms_get_logical (features[i].name, buff, sizeof (buff));

      if (strcmp (buff, "ENABLE") == 0
	  || strcmp (buff, "TRUE") == 0
	  || strcmp (buff, "1") == 0)
	*features[i].gl_addr = 32;
      else if (strcmp (buff, "DISABLE") == 0
	       || strcmp (buff, "FALSE") == 0
	       || strcmp (buff, "0") == 0)
	*features[i].gl_addr = 64;
    }

  /* Features to artificially limit the stack size.  */
  __gnat_set_stack_limit ();

  __gnat_features_set = 1;
}

/* Return true if the VMS version is 7.x.  */

extern unsigned int LIB$GETSYI (int *, ...);

#define SYI$_VERSION 0x1000

int
__gnat_is_vms_v7 (void)
{
  struct descriptor_s desc;
  char version[8];
  int status;
  int code = SYI$_VERSION;

  desc.len = sizeof (version);
  desc.mbz = 0;
  desc.adr = version;

  status = LIB$GETSYI (&code, 0, &desc);
  if ((status & 1) == 1 && version[1] == '7' && version[2] == '.')
    return 1;
  else
    return 0;
}

/*******************/
/* FreeBSD Section */
/*******************/

#elif defined (__FreeBSD__)

#include <signal.h>
#include <sys/ucontext.h>
#include <unistd.h>

static void
__gnat_error_handler (int sig,
		      siginfo_t *si ATTRIBUTE_UNUSED,
		      void *ucontext ATTRIBUTE_UNUSED)
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
      exception = &storage_error;
      msg = "SIGBUS: possible stack overflow";
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
     signal that might cause a scheduling event!  */

  act.sa_sigaction
    = (void (*)(int, struct __siginfo *, void*)) __gnat_error_handler;
  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
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

void __gnat_error_handler (int, void *, struct sigcontext *);

#ifndef __RTP__

/* Directly vectored Interrupt routines are not supported when using RTPs.  */

extern int __gnat_inum_to_ivec (int);

/* This is needed by the GNAT run time to handle Vxworks interrupts.  */
int
__gnat_inum_to_ivec (int num)
{
  return INUM_TO_IVEC (num);
}
#endif

#if !defined(__alpha_vxworks) && (_WRS_VXWORKS_MAJOR != 6) && !defined(__RTP__)

/* getpid is used by s-parint.adb, but is not defined by VxWorks, except
   on Alpha VxWorks and VxWorks 6.x (including RTPs).  */

extern long getpid (void);

long
getpid (void)
{
  return taskIdSelf ();
}
#endif

/* VxWorks 653 vThreads expects the field excCnt to be zeroed when a signal is.
   handled. The VxWorks version of longjmp does this; GCC's builtin_longjmp
   doesn't.  */
void
__gnat_clear_exception_count (void)
{
#ifdef VTHREADS
  WIND_TCB *currentTask = (WIND_TCB *) taskIdSelf();

  currentTask->vThreads.excCnt = 0;
#endif
}

/* Handle different SIGnal to exception mappings in different VxWorks
   versions.   */
static void
__gnat_map_signal (int sig, void *si ATTRIBUTE_UNUSED,
		   struct sigcontext *sc ATTRIBUTE_UNUSED)
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
#ifdef __VXWORKSMILS__
    case SIGILL:
      exception = &storage_error;
      msg = "SIGILL: possible stack overflow";
      break;
    case SIGSEGV:
      exception = &storage_error;
      msg = "SIGSEGV";
      break;
    case SIGBUS:
      exception = &program_error;
      msg = "SIGBUS";
      break;
#else
    case SIGILL:
      exception = &constraint_error;
      msg = "Floating point exception or SIGILL";
      break;
    case SIGSEGV:
      exception = &storage_error;
      msg = "SIGSEGV";
      break;
    case SIGBUS:
      exception = &storage_error;
      msg = "SIGBUS: possible stack overflow";
      break;
#endif
#elif (_WRS_VXWORKS_MAJOR == 6)
    case SIGILL:
      exception = &constraint_error;
      msg = "SIGILL";
      break;
#ifdef __RTP__
    /* In RTP mode a SIGSEGV is most likely due to a stack overflow,
       since stack checking uses the probing mechanism.  */
    case SIGSEGV:
      exception = &storage_error;
      msg = "SIGSEGV: possible stack overflow";
      break;
    case SIGBUS:
      exception = &program_error;
      msg = "SIGBUS";
      break;
#else
      /* VxWorks 6 kernel mode with probing. SIGBUS for guard page hit */
    case SIGSEGV:
      exception = &storage_error;
      msg = "SIGSEGV";
      break;
    case SIGBUS:
      exception = &storage_error;
      msg = "SIGBUS: possible stack overflow";
      break;
#endif
#else
    /* VxWorks 5: a SIGILL is most likely due to a stack overflow,
       since stack checking uses the stack limit mechanism.  */
    case SIGILL:
      exception = &storage_error;
      msg = "SIGILL: possible stack overflow";
      break;
    case SIGSEGV:
      exception = &storage_error;
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

/* Tasking and Non-tasking signal handler.  Map SIGnal to Ada exception
   propagation after the required low level adjustments.  */

void
__gnat_error_handler (int sig, void *si, struct sigcontext *sc)
{
  sigset_t mask;

  /* VxWorks will always mask out the signal during the signal handler and
     will reenable it on a longjmp.  GNAT does not generate a longjmp to
     return from a signal handler so the signal will still be masked unless
     we unmask it.  */
  sigprocmask (SIG_SETMASK, NULL, &mask);
  sigdelset (&mask, sig);
  sigprocmask (SIG_SETMASK, &mask, NULL);

#if defined (__PPC__) && defined(_WRS_KERNEL)
  /* On PowerPC, kernel mode, we process signals through a Call Frame Info
     trampoline, voiding the need for myriads of fallback_frame_state
     variants in the ZCX runtime.  We have no simple way to distinguish ZCX
     from SJLJ here, so we do this for SJLJ as well even though this is not
     necessary.  This only incurs a few extra instructions and a tiny
     amount of extra stack usage.  */

  #include "sigtramp.h"

  __gnat_sigtramp (sig, (void *)si, (void *)sc,
		   (sighandler_t *)&__gnat_map_signal);

#else
  __gnat_map_signal (sig, si, sc);
#endif
}

#if defined(__leon__) && defined(_WRS_KERNEL)
/* For LEON VxWorks we need to install a trap handler for stack overflow */

extern void excEnt (void);
/* VxWorks exception handler entry */

struct trap_entry {
   unsigned long inst_first;
   unsigned long inst_second;
   unsigned long inst_third;
   unsigned long inst_fourth;
};
/* Four instructions representing entries in the trap table */

struct trap_entry *trap_0_entry;
/* We will set the location of the entry for software trap 0 in the trap
   table. */
#endif

void
__gnat_install_handler (void)
{
  struct sigaction act;

  /* Setup signal handler to map synchronous signals to appropriate
     exceptions.  Make sure that the handler isn't interrupted by another
     signal that might cause a scheduling event!  */

  act.sa_handler = __gnat_error_handler;
  act.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sigemptyset (&act.sa_mask);

  /* For VxWorks, install all signal handlers, since pragma Interrupt_State
     applies to vectored hardware interrupts, not signals.  */
  sigaction (SIGFPE,  &act, NULL);
  sigaction (SIGILL,  &act, NULL);
  sigaction (SIGSEGV, &act, NULL);
  sigaction (SIGBUS,  &act, NULL);

#if defined(__leon__) && defined(_WRS_KERNEL)
  /* Specific to the LEON VxWorks kernel run-time library */

  /* For stack checking the compiler triggers a software trap 0 (ta 0) in
     case of overflow (we use the stack limit mechanism). We need to install
     the trap handler here for this software trap (the OS does not handle
     it) as if it were a data_access_exception (trap 9). We do the same as
     if we put in the trap table a VXSPARC_BAD_TRAP(9). Software trap 0 is
     located at vector 0x80, and each entry takes 4 words. */

  trap_0_entry = (struct trap_entry *)(intVecBaseGet () + 0x80 * 4);

  /* mov 0x9, %l7 */

  trap_0_entry->inst_first = 0xae102000 + 9;

  /* sethi %hi(excEnt), %l6 */

  /* The 22 most significant bits of excEnt are obtained shifting 10 times
     to the right.  */

  trap_0_entry->inst_second = 0x2d000000 + ((unsigned long)excEnt >> 10);

  /* jmp %l6+%lo(excEnt) */

  /* The 10 least significant bits of excEnt are obtained by masking */

  trap_0_entry->inst_third = 0x81c5a000 + ((unsigned long)excEnt & 0x3ff);

  /* rd %psr, %l0 */

  trap_0_entry->inst_fourth = 0xa1480000;
#endif

  __gnat_handler_installed = 1;
}

#define HAVE_GNAT_INIT_FLOAT

void
__gnat_init_float (void)
{
  /* Disable overflow/underflow exceptions on the PPC processor, needed
     to get correct Ada semantics.  Note that for AE653 vThreads, the HW
     overflow settings are an OS configuration issue.  The instructions
     below have no effect.  */
#if defined (_ARCH_PPC) && !defined (_SOFT_FLOAT) && (!defined (VTHREADS) || defined (__VXWORKSMILS__))
#if defined (__SPE__)
  {
     const unsigned long spefscr_mask = 0xfffffff3;
     unsigned long spefscr;
     asm ("mfspr  %0, 512" : "=r" (spefscr));
     spefscr = spefscr & spefscr_mask;
     asm ("mtspr 512, %0\n\tisync" : : "r" (spefscr));
  }
#else
  asm ("mtfsb0 25");
  asm ("mtfsb0 26");
#endif
#endif

#if (defined (__i386__) || defined (i386)) && !defined (VTHREADS)
  /* This is used to properly initialize the FPU on an x86 for each
     process thread.  */
  asm ("finit");
#endif

  /* Similarly for SPARC64.  Achieved by masking bits in the Trap Enable Mask
     field of the Floating-point Status Register (see the SPARC Architecture
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

/* This subprogram is called by System.Task_Primitives.Operations.Enter_Task
   (if not null) when a new task is created.  It is initialized by
   System.Stack_Checking.Operations.Initialize_Stack_Limit.
   The use of a hook avoids to drag stack checking subprograms if stack
   checking is not used.  */
void (*__gnat_set_stack_limit_hook)(void) = (void (*)(void))0;

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

  /* Do not install handlers if interrupt state is "System".  */
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
/* OpenBSD Section */
/*******************/

#elif defined(__OpenBSD__)

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

/******************/
/* Darwin Section */
/******************/

#elif defined(__APPLE__)

#include <signal.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/sysctl.h>
#include <mach/mach_vm.h>
#include <mach/mach_init.h>
#include <mach/vm_statistics.h>

/* This must be in keeping with System.OS_Interface.Alternate_Stack_Size.  */
char __gnat_alternate_stack[32 * 1024]; /* 1 * MINSIGSTKSZ */

/* Defined in xnu unix_signal.c.
   Tell the kernel to re-use alt stack when delivering a signal.  */
#define	UC_RESET_ALT_STACK	0x80000000

/* Return true if ADDR is within a stack guard area.  */
static int
__gnat_is_stack_guard (mach_vm_address_t addr)
{
  kern_return_t kret;
  vm_region_submap_info_data_64_t info;
  mach_vm_address_t start;
  mach_vm_size_t size;
  natural_t depth;
  mach_msg_type_number_t count;

  count = VM_REGION_SUBMAP_INFO_COUNT_64;
  start = addr;
  size = -1;
  depth = 9999;
  kret = mach_vm_region_recurse (mach_task_self (), &start, &size, &depth,
				 (vm_region_recurse_info_t) &info, &count);
  if (kret == KERN_SUCCESS
      && addr >= start && addr < (start + size)
      && info.protection == VM_PROT_NONE
      && info.user_tag == VM_MEMORY_STACK)
    return 1;
  return 0;
}

#define HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

#if defined (__x86_64__)
static int
__darwin_major_version (void)
{
  static int cache = -1;
  if (cache < 0)
    {
      int mib[2] = {CTL_KERN, KERN_OSRELEASE};
      size_t len;

      /* Find out how big the buffer needs to be (and set cache to 0
         on failure).  */
      if (sysctl (mib, 2, NULL, &len, NULL, 0) == 0)
        {
          char release[len];
          sysctl (mib, 2, release, &len, NULL, 0);
          /* Darwin releases are of the form L.M.N where L is the major
             version, so strtol will return L.  */
          cache = (int) strtol (release, NULL, 10);
        }
      else
        {
          cache = 0;
        }
    }
  return cache;
}
#endif

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED,
				 void *ucontext ATTRIBUTE_UNUSED)
{
#if defined (__x86_64__)
  if (__darwin_major_version () < 12)
    {
      /* Work around radar #10302855, where the unwinders (libunwind or
	 libgcc_s depending on the system revision) and the DWARF unwind
	 data for sigtramp have different ideas about register numbering,
	 causing rbx and rdx to be transposed.  */
      ucontext_t *uc = (ucontext_t *)ucontext;
      unsigned long t = uc->uc_mcontext->__ss.__rbx;

      uc->uc_mcontext->__ss.__rbx = uc->uc_mcontext->__ss.__rdx;
      uc->uc_mcontext->__ss.__rdx = t;
    }
#endif
}

static void
__gnat_error_handler (int sig, siginfo_t *si, void *ucontext)
{
  struct Exception_Data *exception;
  const char *msg;

  __gnat_adjust_context_for_raise (sig, ucontext);

  switch (sig)
    {
    case SIGSEGV:
    case SIGBUS:
      if (__gnat_is_stack_guard ((unsigned long)si->si_addr))
	{
	  exception = &storage_error;
	  msg = "stack overflow";
	}
      else
	{
	  exception = &constraint_error;
	  msg = "erroneous memory access";
	}
      /* Reset the use of alt stack, so that the alt stack will be used
	 for the next signal delivery.
         The stack can't be used in case of stack checking.  */
      syscall (SYS_sigreturn, NULL, UC_RESET_ALT_STACK);
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
     signal that might cause a scheduling event!  Also setup an alternate
     stack region for the handler execution so that stack overflows can be
     handled properly, avoiding a SEGV generation from stack usage by the
     handler itself (and it is required by Darwin).  */

  stack_t stack;
  stack.ss_sp = __gnat_alternate_stack;
  stack.ss_size = sizeof (__gnat_alternate_stack);
  stack.ss_flags = 0;
  sigaltstack (&stack, NULL);

  act.sa_flags = SA_NODEFER | SA_RESTART | SA_SIGINFO;
  act.sa_sigaction = __gnat_error_handler;
  sigemptyset (&act.sa_mask);

  /* Do not install handlers if interrupt state is "System".  */
  if (__gnat_get_interrupt_state (SIGABRT) != 's')
    sigaction (SIGABRT, &act, NULL);
  if (__gnat_get_interrupt_state (SIGFPE) != 's')
    sigaction (SIGFPE,  &act, NULL);
  if (__gnat_get_interrupt_state (SIGILL) != 's')
    sigaction (SIGILL,  &act, NULL);

  act.sa_flags |= SA_ONSTACK;
  if (__gnat_get_interrupt_state (SIGSEGV) != 's')
    sigaction (SIGSEGV, &act, NULL);
  if (__gnat_get_interrupt_state (SIGBUS) != 's')
    sigaction (SIGBUS,  &act, NULL);

  __gnat_handler_installed = 1;
}

#else

/* For all other versions of GNAT, the handler does nothing.  */

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
   initialization of the FP processor.  This version is used under INTERIX
   and WIN32.  */

#if defined (_WIN32) || defined (__INTERIX) \
  || defined (__Lynx__) || defined(__NetBSD__) || defined(__FreeBSD__) \
  || defined (__OpenBSD__)

#define HAVE_GNAT_INIT_FLOAT

void
__gnat_init_float (void)
{
#if defined (__i386__) || defined (i386) || defined (__x86_64)

  /* This is used to properly initialize the FPU on an x86 for each
     process thread.  */

  asm ("finit");

#endif  /* Defined __i386__ */
}
#endif

#ifndef HAVE_GNAT_INIT_FLOAT

/* All targets without a specific __gnat_init_float will use an empty one.  */
void
__gnat_init_float (void)
{
}
#endif

/***********************************/
/* __gnat_adjust_context_for_raise */
/***********************************/

#ifndef HAVE_GNAT_ADJUST_CONTEXT_FOR_RAISE

/* All targets without a specific version will use an empty one.  */

/* Given UCONTEXT a pointer to a context structure received by a signal
   handler for SIGNO, perform the necessary adjustments to let the handler
   raise an exception.  Calls to this routine are not conditioned by the
   propagation scheme in use.  */

void
__gnat_adjust_context_for_raise (int signo ATTRIBUTE_UNUSED,
				 void *ucontext ATTRIBUTE_UNUSED)
{
  /* We used to compensate here for the raised from call vs raised from signal
     exception discrepancy with the GCC ZCX scheme, but this now can be dealt
     with generically in the unwinder (see GCC PR other/26208).  This however
     requires the use of the _Unwind_GetIPInfo routine in raise-gcc.c, which
     is predicated on the definition of HAVE_GETIPINFO at compile time.  Only
     the VMS ports still do the compensation described in the few lines below.

     *** Call vs signal exception discrepancy with GCC ZCX scheme ***

     The GCC unwinder expects to be dealing with call return addresses, since
     this is the "nominal" case of what we retrieve while unwinding a regular
     call chain.

     To evaluate if a handler applies at some point identified by a return
     address, the propagation engine needs to determine what region the
     corresponding call instruction pertains to.  Because the return address
     may not be attached to the same region as the call, the unwinder always
     subtracts "some" amount from a return address to search the region
     tables, amount chosen to ensure that the resulting address is inside the
     call instruction.

     When we raise an exception from a signal handler, e.g. to transform a
     SIGSEGV into Storage_Error, things need to appear as if the signal
     handler had been "called" by the instruction which triggered the signal,
     so that exception handlers that apply there are considered.  What the
     unwinder will retrieve as the return address from the signal handler is
     what it will find as the faulting instruction address in the signal
     context pushed by the kernel.  Leaving this address untouched looses, if
     the triggering instruction happens to be the very first of a region, as
     the later adjustments performed by the unwinder would yield an address
     outside that region.  We need to compensate for the unwinder adjustments
     at some point, and this is what this routine is expected to do.

     signo is passed because on some targets for some signals the PC in
     context points to the instruction after the faulting one, in which case
     the unwinder adjustment is still desired.  */
}

#endif

#ifdef __cplusplus
}
#endif
