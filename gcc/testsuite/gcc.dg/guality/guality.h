/* Infrastructure to test the quality of debug information.
   Copyright (C) 2008, 2009 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* This is a first cut at checking that debug information matches
   run-time.  The idea is to annotate programs with GUALCHK* macros
   that guide the tests.

   In the current implementation, all of the macros expand to function
   calls.  On the one hand, this interferes with optimizations; on the
   other hand, it establishes an optimization barrier and a clear
   inspection point, where previous operations (as in the abstract
   machine) should have been completed and have their effects visible,
   and future operations shouldn't have started yet.

   In the current implementation of guality_check(), we fork a child
   process that runs gdb, attaches to the parent process (the one that
   called guality_check), moves up one stack frame (to the caller of
   guality_check) and then examines the given expression.

   If it matches the expected value, we have a PASS.  If it differs,
   we have a FAILure.  If it is missing, we'll have a FAIL or an
   UNRESOLVED depending on whether the variable or expression might be
   unavailable at that point, as indicated by the third argument.

   We envision a future alternate implementation with two compilation
   and execution cycles, say one that runs the program and uses the
   macros to log expressions and expected values, another in which the
   macros expand to nothing and the logs are used to guide a debug
   session that tests the values.  How to identify the inspection
   points in the second case is yet to be determined.  It is
   recommended that GUALCHK* macros be by themselves in source lines,
   so that __FILE__ and __LINE__ will be usable to identify them.
*/

/* This is the type we use to pass values to guality_check.  */

typedef intmax_t gualchk_t;

/* Convert a pointer or integral type to the widest integral type,
   as expected by guality_check.  */

#define GUALCVT(val)						\
  ((gualchk_t)__builtin_choose_expr				\
   (__builtin_types_compatible_p (__typeof (val), gualchk_t),	\
    (val), (intptr_t)(val)))

/* Attach a debugger to the current process and verify that the string
   EXPR, evaluated by the debugger, yields the gualchk_t number VAL.
   If the debugger cannot compute the expression, say because the
   variable is unavailable, this will count as an error, unless unkok
   is nonzero.  */

#define GUALCHKXPRVAL(expr, val, unkok) \
  guality_check ((expr), (val), (unkok))

/* Check that a debugger knows that EXPR evaluates to the run-time
   value of EXPR.  Unknown values are marked as acceptable,
   considering that EXPR may die right after this call.  This will
   affect the generated code in that EXPR will be evaluated and forced
   to remain live at least until right before the call to
   guality_check, although not necessarily after the call.  */

#define GUALCHKXPR(expr) \
  GUALCHKXPRVAL (#expr, GUALCVT (expr), 1)

/* Same as GUALCHKXPR, but issue an error if the variable is optimized
   away.  */

#define GUALCHKVAL(expr) \
  GUALCHKXPRVAL (#expr, GUALCVT (expr), 0)

/* Check that a debugger knows that EXPR evaluates to the run-time
   value of EXPR.  Unknown values are marked as errors, because the
   value of EXPR is forced to be available right after the call, for a
   range of at least one instruction.  This will affect the generated
   code, in that EXPR *will* be evaluated before and preserved until
   after the call to guality_check.  */

#define GUALCHKFLA(expr) do {					\
    __typeof(expr) volatile __preserve_after;			\
    __typeof(expr) __preserve_before = (expr);			\
    GUALCHKXPRVAL (#expr, GUALCVT (__preserve_before), 0);	\
    __preserve_after = __preserve_before;			\
    asm ("" : : "m" (__preserve_after));			\
  } while (0)

/* GUALCHK is the simplest way to assert that debug information for an
   expression matches its run-time value.  Whether to force the
   expression live after the call, so as to flag incompleteness
   errors, can be disabled by defining GUALITY_DONT_FORCE_LIVE_AFTER.
   Setting it to -1, an error is issued for optimized out variables,
   even though they are not forced live.  */

#if ! GUALITY_DONT_FORCE_LIVE_AFTER
#define GUALCHK(var) GUALCHKFLA(var)
#elif GUALITY_DONT_FORCE_LIVE_AFTER < 0
#define GUALCHK(var) GUALCHKVAL(var)
#else
#define GUALCHK(var) GUALCHKXPR(var)
#endif

/* The name of the GDB program, with arguments to make it quiet.  This
   is GUALITY_GDB_DEFAULT GUALITY_GDB_ARGS by default, but it can be
   overridden by setting the GUALITY_GDB environment variable, whereas
   GUALITY_GDB_DEFAULT can be overridden by setting the
   GUALITY_GDB_NAME environment variable.  */

static const char *guality_gdb_command;
#define GUALITY_GDB_DEFAULT "gdb"
#if defined(__unix)
# define GUALITY_GDB_REDIRECT " > /dev/null 2>&1"
#elif defined (_WIN32) || defined (MSDOS)
# define GUALITY_GDB_REDIRECT " > nul"
#else
# define GUALITY_GDB_REDIRECT ""
#endif
#define GUALITY_GDB_ARGS " -nx -nw --quiet" GUALITY_GDB_REDIRECT

/* Kinds of results communicated as exit status from child process
   that runs gdb to the parent process that's being monitored.  */

enum guality_counter { PASS, INCORRECT, INCOMPLETE };

/* Count of passes and errors.  */

static int guality_count[INCOMPLETE+1];

/* If --guality-skip is given in the command line, all the monitoring,
   forking and debugger-attaching action will be disabled.  This is
   useful to run the monitor program within a debugger.  */

static int guality_skip;

/* This is a file descriptor to which we'll issue gdb commands to
   probe and test.  */
FILE *guality_gdb_input;

/* This holds the line number where we're supposed to set a
   breakpoint.  */
int guality_breakpoint_line;

/* GDB should set this to true once it's connected.  */
int volatile guality_attached;

/* This function is the main guality program.  It may actually be
   defined as main, because we #define main to it afterwards.  Because
   of this wrapping, guality_main may not have an empty argument
   list.  */

extern int guality_main (int argc, char *argv[]);

static void __attribute__((noinline))
guality_check (const char *name, gualchk_t value, int unknown_ok);

/* Set things up, run guality_main, then print a summary and quit.  */

int
main (int argc, char *argv[])
{
  int i;
  char *argv0 = argv[0];

  guality_gdb_command = getenv ("GUALITY_GDB");
  if (!guality_gdb_command)
    {
      guality_gdb_command = getenv ("GUALITY_GDB_NAME");
      if (!guality_gdb_command)
	guality_gdb_command = GUALITY_GDB_DEFAULT GUALITY_GDB_ARGS;
      else
	{
	  int len = strlen (guality_gdb_command) + sizeof (GUALITY_GDB_ARGS);
	  char *buf = __builtin_alloca (len);
	  strcpy (buf, guality_gdb_command);
	  strcat (buf, GUALITY_GDB_ARGS);
	  guality_gdb_command = buf;
	}
    }

  for (i = 1; i < argc; i++)
    if (strcmp (argv[i], "--guality-skip") == 0)
      guality_skip = 1;
    else
      break;

  if (!guality_skip)
    {
      guality_gdb_input = popen (guality_gdb_command, "w");
      /* This call sets guality_breakpoint_line.  */
      guality_check (NULL, 0, 0);
      if (!guality_gdb_input
	  || fprintf (guality_gdb_input, "\
set height 0\n\
attach %i\n\
set guality_attached = 1\n\
b %i\n\
continue\n\
", (int)getpid (), guality_breakpoint_line) <= 0
	  || fflush (guality_gdb_input))
	{
	  perror ("gdb");
	  abort ();
	}
    }

  argv[--i] = argv0;

  guality_main (argc - i, argv + i);

  i = guality_count[INCORRECT];

  fprintf (stderr, "%s: %i PASS, %i FAIL, %i UNRESOLVED\n",
	   i ? "FAIL" : "PASS",
	   guality_count[PASS], guality_count[INCORRECT],
	   guality_count[INCOMPLETE]);

  return i;
}

#define main guality_main

/* Tell the GDB child process to evaluate NAME in the caller.  If it
   matches VALUE, we have a PASS; if it's unknown and UNKNOWN_OK, we
   have an UNRESOLVED.  Otherwise, it's a FAIL.  */

static void __attribute__((noinline))
guality_check (const char *name, gualchk_t value, int unknown_ok)
{
  int result;

  if (guality_skip)
    return;

  {
    volatile gualchk_t xvalue = -1;
    volatile int unavailable = 0;
    if (name)
      {
	/* The sequence below cannot distinguish an optimized away
	   variable from one mapped to a non-lvalue zero.  */
	if (fprintf (guality_gdb_input, "\
up\n\
set $value1 = 0\n\
set $value1 = (%s)\n\
set $value2 = -1\n\
set $value2 = (%s)\n\
set $value3 = $value1 - 1\n\
set $value4 = $value1 + 1\n\
set $value3 = (%s)++\n\
set $value4 = --(%s)\n\
down\n\
set xvalue = $value1\n\
set unavailable = $value1 != $value2 ? -1 : $value3 != $value4 ? 1 : 0\n\
continue\n\
", name, name, name, name) <= 0
	    || fflush (guality_gdb_input))
	  {
	    perror ("gdb");
	    abort ();
	  }
	else if (!guality_attached)
	  {
	    unsigned int timeout = 0;

	    /* Give GDB some more time to attach.  Wrapping around a
	       32-bit counter takes some seconds, it should be plenty
	       of time for GDB to get a chance to start up and attach,
	       but not long enough that, if GDB is unavailable or
	       broken, we'll take far too long to give up.  */
	    while (--timeout && !guality_attached)
	      ;
	    if (!timeout && !guality_attached)
	      {
		fprintf (stderr, "gdb: took too long to attach\n");
		abort ();
	      }
	  }
      }
    else
      {
	guality_breakpoint_line = __LINE__ + 5;
	return;
      }
    /* Do NOT add lines between the __LINE__ above and the line below,
       without also adjusting the added constant to match.  */
    if (!unavailable || (unavailable > 0 && xvalue))
      {
	if (xvalue == value)
	  result = PASS;
	else
	  result = INCORRECT;
      }
    else
      result = INCOMPLETE;
    asm ("" : : "X" (name), "X" (value), "X" (unknown_ok), "m" (xvalue));
    switch (result)
      {
      case PASS:
	fprintf (stderr, "PASS: %s is %lli\n", name, value);
	break;
      case INCORRECT:
	fprintf (stderr, "FAIL: %s is %lli, not %lli\n", name, xvalue, value);
	break;
      case INCOMPLETE:
	fprintf (stderr, "%s: %s is %s, expected %lli\n",
		 unknown_ok ? "UNRESOLVED" : "FAIL", name,
		 unavailable < 0 ? "not computable" : "optimized away", value);
	result = unknown_ok ? INCOMPLETE : INCORRECT;
	break;
      default:
	abort ();
      }
  }

  switch (result)
    {
    case PASS:
    case INCORRECT:
    case INCOMPLETE:
      ++guality_count[result];
      break;

    default:
      abort ();
    }
}
