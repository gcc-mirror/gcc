/* SysExceptions.c connect signal handlers to exceptions.

Copyright (C) 2010-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "gm2-libs-host.h"

#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if 0
/* Signals.  */
#define SIGHUP 1       /* Hangup (POSIX).  */
#define SIGINT 2       /* Interrupt (ANSI).  */
#define SIGQUIT 3      /* Quit (POSIX).  */
#define SIGILL 4       /* Illegal instruction (ANSI).  */
#define SIGTRAP 5      /* Trace trap (POSIX).  */
#define SIGABRT 6      /* Abort (ANSI).  */
#define SIGIOT 6       /* IOT trap (4.2 BSD).  */
#define SIGBUS 7       /* BUS error (4.2 BSD).  */
#define SIGFPE 8       /* Floating-point exception (ANSI).  */
#define SIGKILL 9      /* Kill, unblockable (POSIX).  */
#define SIGUSR1 10     /* User-defined signal 1 (POSIX).  */
#define SIGSEGV 11     /* Segmentation violation (ANSI).  */
#define SIGUSR2 12     /* User-defined signal 2 (POSIX).  */
#define SIGPIPE 13     /* Broken pipe (POSIX).  */
#define SIGALRM 14     /* Alarm clock (POSIX).  */
#define SIGTERM 15     /* Termination (ANSI).  */
#define SIGSTKFLT 16   /* Stack fault.  */
#define SIGCLD SIGCHLD /* Same as SIGCHLD (System V).  */
#define SIGCHLD 17     /* Child status has changed (POSIX).  */
#define SIGCONT 18     /* Continue (POSIX).  */
#define SIGSTOP 19     /* Stop, unblockable (POSIX).  */
#define SIGTSTP 20     /* Keyboard stop (POSIX).  */
#define SIGTTIN 21     /* Background read from tty (POSIX).  */
#define SIGTTOU 22     /* Background write to tty (POSIX).  */
#define SIGURG 23      /* Urgent condition on socket (4.2 BSD).  */
#define SIGXCPU 24     /* CPU limit exceeded (4.2 BSD).  */
#define SIGXFSZ 25     /* File size limit exceeded (4.2 BSD).  */
#define SIGVTALRM 26   /* Virtual alarm clock (4.2 BSD).  */
#define SIGPROF 27     /* Profiling alarm clock (4.2 BSD).  */
#define SIGWINCH 28    /* Window size change (4.3 BSD, Sun).  */
#define SIGPOLL SIGIO  /* Pollable event occurred (System V).  */
#define SIGIO 29       /* I/O now possible (4.2 BSD).  */
#define SIGPWR 30      /* Power failure restart (System V).  */
#define SIGSYS 31      /* Bad system call.  */
#define SIGUNUSED 31


    (indexException,     rangeException,         caseSelectException,  invalidLocation,
     functionException,  wholeValueException,    wholeDivException,    realValueException,
     realDivException,   complexValueException,  complexDivException,  protException,
     sysException,       coException,            exException
    );

#endif

/* note wholeDivException and realDivException are caught by SIGFPE
   and depatched to the appropriate Modula-2 runtime routine upon
   testing FPE_INTDIV or FPE_FLTDIV.  realValueException is also
   caught by SIGFPE and dispatched by testing FFE_FLTOVF or
   FPE_FLTUND or FPE_FLTRES or FPE_FLTINV.  indexException is
   caught by SIGFPE and dispatched by FPE_FLTSUB.  */

#if defined(HAVE_SIGNAL_H)
static struct sigaction sigbus;
static struct sigaction sigfpe_;
static struct sigaction sigsegv;

static void (*indexProc) (void *);
static void (*rangeProc) (void *);
static void (*assignmentrangeProc) (void *);
static void (*caseProc) (void *);
static void (*invalidlocProc) (void *);
static void (*functionProc) (void *);
static void (*wholevalueProc) (void *);
static void (*wholedivProc) (void *);
static void (*realvalueProc) (void *);
static void (*realdivProc) (void *);
static void (*complexvalueProc) (void *);
static void (*complexdivProc) (void *);
static void (*protectionProc) (void *);
static void (*systemProc) (void *);
static void (*coroutineProc) (void *);
static void (*exceptionProc) (void *);

static void
sigbusDespatcher (int signum, siginfo_t *info, void *ucontext)
{
  switch (signum)
    {

    case SIGSEGV:
    case SIGBUS:
      if (info)
        (*invalidlocProc) (info->si_addr);
      break;
    default:
      perror ("not expecting to arrive here with this signal");
    }
}

static void
sigfpeDespatcher (int signum, siginfo_t *info, void *ucontext)
{
  switch (signum)
    {

    case SIGFPE:
      if (info)
        {
          if (info->si_code | FPE_INTDIV)
            (*wholedivProc) (info->si_addr); /* integer divide by zero.  */
          if (info->si_code | FPE_INTOVF)
            (*wholevalueProc) (info->si_addr); /* integer overflow.  */
          if (info->si_code | FPE_FLTDIV)
            (*realdivProc) (
                info->si_addr); /* floating-point divide by zero.  */
          if (info->si_code | FPE_FLTOVF)
            (*realvalueProc) (info->si_addr); /* floating-point overflow.  */
          if (info->si_code | FPE_FLTUND)
            (*realvalueProc) (info->si_addr); /* floating-point underflow.  */
          if (info->si_code | FPE_FLTRES)
            (*realvalueProc) (
                info->si_addr); /* floating-point inexact result.  */
          if (info->si_code | FPE_FLTINV)
            (*realvalueProc) (
                info->si_addr); /* floating-point invalid result.  */
          if (info->si_code | FPE_FLTSUB)
            (*indexProc) (info->si_addr); /* subscript out of range.  */
        }
      break;
    default:
      perror ("not expecting to arrive here with this signal");
    }
}

void
SysExceptions_InitExceptionHandlers (
    void (*indexf) (void *), void (*range) (void *), void (*casef) (void *),
    void (*invalidloc) (void *), void (*function) (void *),
    void (*wholevalue) (void *), void (*wholediv) (void *),
    void (*realvalue) (void *), void (*realdiv) (void *),
    void (*complexvalue) (void *), void (*complexdiv) (void *),
    void (*protection) (void *), void (*systemf) (void *),
    void (*coroutine) (void *), void (*exception) (void *))
{
  struct sigaction old;

  indexProc = indexf;
  rangeProc = range;
  caseProc = casef;
  invalidlocProc = invalidloc;
  functionProc = function;
  wholevalueProc = wholevalue;
  wholedivProc = wholediv;
  realvalueProc = realvalue;
  realdivProc = realdiv;
  complexvalueProc = complexvalue;
  complexdivProc = complexdiv;
  protectionProc = protection;
  systemProc = systemf;
  coroutineProc = coroutine;
  exceptionProc = exception;

  sigbus.sa_sigaction = sigbusDespatcher;
  sigbus.sa_flags = (SA_SIGINFO);
  sigemptyset (&sigbus.sa_mask);

  if (sigaction (SIGBUS, &sigbus, &old) != 0)
    perror ("unable to install the sigbus signal handler");

  sigsegv.sa_sigaction = sigbusDespatcher;
  sigsegv.sa_flags = (SA_SIGINFO);
  sigemptyset (&sigsegv.sa_mask);

  if (sigaction (SIGSEGV, &sigsegv, &old) != 0)
    perror ("unable to install the sigsegv signal handler");

  sigfpe_.sa_sigaction = sigfpeDespatcher;
  sigfpe_.sa_flags = (SA_SIGINFO);
  sigemptyset (&sigfpe_.sa_mask);

  if (sigaction (SIGFPE, &sigfpe_, &old) != 0)
    perror ("unable to install the sigfpe signal handler");
}

#else
void
SysExceptions_InitExceptionHandlers (void *indexf, void *range, void *casef,
                                     void *invalidloc, void *function,
                                     void *wholevalue, void *wholediv,
                                     void *realvalue, void *realdiv,
                                     void *complexvalue, void *complexdiv,
                                     void *protection, void *systemf,
                                     void *coroutine, void *exception)
{
}
#endif

/* GNU Modula-2 linking fodder.  */

void
_M2_SysExceptions_init (int argc, char *argv[], char *envp[])
{
}

void
_M2_SysExceptions_finish (void)
{
}
#ifdef __cplusplus
}
#endif
