/* Timing variables for measuring compiler performance.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Alex Samuel <samuel@codesourcery.com>

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include <sys/resource.h>

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#include "flags.h"
#include "timevar.h"

/* See timevar.h for an explanation of timing variables.  */

/* This macro evaluates to non-zero if timing variables are enabled. */
#define TIMEVAR_ENABLE (!quiet_flag)

/* A timing variable.  */

struct timevar_def
{
  /* Elapsed time for this variable.  */
  struct timevar_time_def elapsed;

  /* If this variable is timed independently of the timing stack,
     using timevar_start, this contains the start time.  */
  struct timevar_time_def start_time;

  /* The name of this timing variable.  */
  const char *name;

  /* Non-zero if this timing variable is running as a standalone
     timer.  */
  unsigned standalone : 1;

  /* Non-zero if this timing variable was ever started or pushed onto
     the timing stack.  */
  unsigned used : 1;
};

/* An element on the timing stack.  Elapsed time is attributed to the
   topmost timing variable on the stack.  */

struct timevar_stack_def
{
  /* The timing variable at this stack level.  */
  struct timevar_def *timevar;

  /* The next lower timing variable context in the stack.  */
  struct timevar_stack_def *next;
};

/* Declared timing variables.  Constructed from the contents of
   timevar.def.  */
static struct timevar_def timevars[TIMEVAR_LAST];

/* The top of the timing stack.  */
static struct timevar_stack_def *stack;

/* A list of unused (i.e. allocated and subsequently popped)
   timevar_stack_def instances.  */
static struct timevar_stack_def *unused_stack_instances;

/* The time at which the topmost element on the timing stack was
   pushed.  Time elapsed since then is attributed to the topmost
   element.  */
static struct timevar_time_def start_time;

static void get_time
  PARAMS ((struct timevar_time_def *));
static void timevar_add
  PARAMS ((struct timevar_time_def *, struct timevar_time_def *));
static void timevar_accumulate
  PARAMS ((struct timevar_time_def *, struct timevar_time_def *, 
	   struct timevar_time_def *));

/* Fill the current times into TIME.  The definition of this function
   also defines any or all of the HAVE_USER_TIME, HAVE_SYS_TIME, and
   HAVA_WALL_TIME macros.  */

static void
get_time (now)
     struct timevar_time_def *now;
{
  now->user = 0;
  now->sys  = 0;
  now->wall = 0;

  if (!TIMEVAR_ENABLE)
    return;

#ifdef __BEOS__
  /* Nothing.  */
#else /* not BeOS */
#if defined (_WIN32) && !defined (__CYGWIN__)
  if (clock () >= 0)
    now->user = clock () * 1000;
#define HAVE_USER_TIME

#else /* not _WIN32 */
#ifdef _SC_CLK_TCK
  {
    static int tick;
    struct tms tms;
    if (tick == 0)
      tick = 1000000 / sysconf (_SC_CLK_TCK);
    now->wall = times (&tms) * tick;
    now->user = tms.tms_utime * tick;
    now->sys = tms.tms_stime * tick;
  }
#define HAVE_USER_TIME
#define HAVE_SYS_TIME
#define HAVE_WALL_TIME

#else
#ifdef USG
  {
    struct tms tms;
#   if HAVE_SYSCONF && defined _SC_CLK_TCK
#    define TICKS_PER_SECOND sysconf (_SC_CLK_TCK) /* POSIX 1003.1-1996 */
#   else
#    ifdef CLK_TCK
#     define TICKS_PER_SECOND CLK_TCK /* POSIX 1003.1-1988; obsolescent */
#    else
#     define TICKS_PER_SECOND HZ /* traditional UNIX */
#    endif
#   endif
    now->wall = times (&tms) * (1000000 / TICKS_PER_SECOND);
    now->user = tms.tms_utime * (1000000 / TICKS_PER_SECOND);
    now->sys = tms.tms_stime * (1000000 / TICKS_PER_SECOND);
  }
#define HAVE_USER_TIME
#define HAVE_SYS_TIME
#define HAVE_WALL_TIME

#else
#ifndef VMS
  {
    struct rusage rusage;
    getrusage (0, &rusage);
    now->user 
      = rusage.ru_utime.tv_sec * 1000000 + rusage.ru_utime.tv_usec;
    now->sys 
      = rusage.ru_stime.tv_sec * 1000000 + rusage.ru_stime.tv_usec;
  }
#define HAVE_USER_TIME
#define HAVE_SYS_TIME

#else /* VMS */
  {
    struct
      {
        int proc_user_time;
        int proc_system_time;
        int child_user_time;
        int child_system_time;
      } vms_times;
    now->wall = times ((void *) &vms_times) * 10000;
    now->user = vms_times.proc_user_time * 10000;
    now->sys = vms_times.proc_system_time * 10000;
  }
#define HAVE_USER_TIME
#define HAVE_SYS_TIME
#define HAVE_WALL_TIME

#endif	/* VMS */
#endif	/* USG */
#endif  /* _SC_CLK_TCK */
#endif	/* _WIN32 */
#endif	/* __BEOS__ */
}  

/* Add ELAPSED to TIMER.  */

static void
timevar_add (timer, elapsed)
     struct timevar_time_def *timer;
     struct timevar_time_def *elapsed;
{
  timer->user += elapsed->user;
  timer->sys += elapsed->sys;
  timer->wall += elapsed->wall;
}

/* Add the difference between STOP_TIME and START_TIME to TIMER.  */

static void 
timevar_accumulate (timer, start_time, stop_time)
  struct timevar_time_def *timer;
  struct timevar_time_def *start_time;
  struct timevar_time_def *stop_time;
{
  timer->user += stop_time->user - start_time->user;
  timer->sys += stop_time->sys - start_time->sys;
  timer->wall += stop_time->wall - start_time->wall;
}

/* Initialize timing variables.  */

void
init_timevar ()
{
  if (!TIMEVAR_ENABLE)
    return;

  /* Zero all elapsed times.  */
  memset ((void *) timevars, 0, sizeof (timevars));

  /* Initialize the names of timing variables.  */
#define DEFTIMEVAR(identifer__, name__) \
  timevars[identifer__].name = name__;
#include "timevar.def"
#undef DEFTIMEVAR
}

/* Push TIMEVAR onto the timing stack.  No further elapsed time is
   attributed to the previous topmost timing variable on the stack;
   subsequent elapsed time is attributed to TIMEVAR, until it is
   popped or another element is pushed on top. 

   TIMEVAR cannot be running as a standalone timer.  */

void
timevar_push (timevar)
     timevar_id_t timevar;
{
  struct timevar_def *tv = &timevars[timevar];
  struct timevar_stack_def *context;
  struct timevar_time_def now;

  if (!TIMEVAR_ENABLE)
    return;

  /* Mark this timing variable as used.  */
  tv->used = 1;

  /* Can't push a standalone timer.  */
  if (tv->standalone)
    abort ();

  /* What time is it?  */
  get_time (&now);

  /* If the stack isn't empty, attribute the current elapsed time to
     the old topmost element.  */
  if (stack)
    timevar_accumulate (&stack->timevar->elapsed, &start_time, &now);

  /* Reset the start time; from now on, time is attributed to
     TIMEVAR. */
  start_time = now;

  /* See if we have a previously-allocated stack instance.  If so,
     take it off the list.  If not, malloc a new one.  */
  if (unused_stack_instances != NULL) 
    {
      context = unused_stack_instances;
      unused_stack_instances = unused_stack_instances->next;
    }
  else
    context = (struct timevar_stack_def *) 
      xmalloc (sizeof (struct timevar_stack_def));

  /* Fill it in and put it on the stack.  */
  context->timevar = tv;
  context->next = stack;
  stack = context;
}

/* Pop the topmost timing variable element off the timing stack.  The
   popped variable must be TIMEVAR.  Elapsed time since the that
   element was pushed on, or since it was last exposed on top of the
   stack when the element above it was popped off, is credited to that
   timing variable.  */

void
timevar_pop (timevar)
     timevar_id_t timevar;
{
  struct timevar_time_def now;
  struct timevar_stack_def *popped = stack;

  if (!TIMEVAR_ENABLE)
    return;

  if (&timevars[timevar] != stack->timevar)
    abort ();

  /* What time is it?  */
  get_time (&now);

  /* Attribute the elapsed time to the element we're popping.  */
  timevar_accumulate (&popped->timevar->elapsed, &start_time, &now);

  /* Reset the start time; from now on, time is attributed to the
     element just exposed on the stack.  */
  start_time = now;

  /* Take the item off the stack.  */
  stack = stack->next;

  /* Don't delete the stack element; instead, add it to the list of
     unused elements for later use.  */
  popped->next = unused_stack_instances;
  unused_stack_instances = popped;
}

/* Start timing TIMEVAR independently of the timing stack.  Elapsed
   time until timevar_stop is called for the same timing variable is
   attributed to TIMEVAR.  */

void
timevar_start (timevar)
     timevar_id_t timevar;
{
  struct timevar_def *tv = &timevars[timevar];

  if (!TIMEVAR_ENABLE)
    return;

  /* Mark this timing variable as used.  */
  tv->used = 1;

  /* Don't allow the same timing variable to be started more than
     once.  */
  if (tv->standalone)
    abort ();
  tv->standalone = 1;

  get_time (&tv->start_time);
}

/* Stop timing TIMEVAR.  Time elapsed since timevar_start was called
   is attributed to it.  */

void
timevar_stop (timevar)
     timevar_id_t timevar;
{
  struct timevar_def *tv = &timevars[timevar];
  struct timevar_time_def now;

  if (!TIMEVAR_ENABLE)
    return;

  /* TIMEVAR must have been started via timevar_start.  */
  if (!tv->standalone)
    abort ();

  get_time (&now);
  timevar_accumulate (&tv->elapsed, &tv->start_time, &now);
}

/* Fill the elapsed time for TIMEVAR into ELAPSED.  Returns
   update-to-date information even if TIMEVAR is currently running.  */

void
timevar_get (timevar, elapsed)
     timevar_id_t timevar;
     struct timevar_time_def *elapsed;
{
  struct timevar_def *tv = &timevars[timevar];

  *elapsed = tv->elapsed;

  /* Is TIMEVAR currently running as a standalone timer?  */
  if (tv->standalone)
    /* Add the time elapsed since the it was started.  */
    timevar_add (elapsed, &tv->start_time);

  /* Is TIMEVAR at the top of the timer stack?  */
  if (stack->timevar == tv)
    /* Add the elapsed time since it was pushed.  */
    timevar_add (elapsed, &start_time);
}

/* Summarize timing variables to FP.  The timing variable TV_TOTAL has
   a special meaning -- it's considered to be the total elapsed time,
   for normalizing the others, and is displayed last.  */

void
timevar_print (fp)
     FILE *fp;
{
  /* Only print stuff if we have some sort of time information.  */
#if defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME) || defined (HAVE_WALL_TIME)
  timevar_id_t id;
  struct timevar_time_def *total = &timevars[TV_TOTAL].elapsed;

  if (!TIMEVAR_ENABLE)
    return;

  fprintf (fp, "\nExecution times (seconds)\n");
  for (id = 0; id < TIMEVAR_LAST; ++id)
    {
      struct timevar_def *tv = &timevars[id];

      /* Don't print the total execution time here; that goes at the
	 end.  */
      if (id == TV_TOTAL)
	continue;

      /* Don't print timing variables that were never used.  */
      if (!tv->used)
	continue;

      /* The timing variable name.  */
      fprintf (fp, " %-22s:", tv->name);

#ifdef HAVE_USER_TIME
      /* Print user-mode time for this process.  */
      fprintf (fp, "%4ld.%02ld (%2.0f%%) usr", 
	       tv->elapsed.user / 1000000, 
	       (tv->elapsed.user % 1000000) / 10000,
	       (total->user == 0) ? 0.0 
	       : (100.0 * tv->elapsed.user / (double) total->user));
#endif /* HAVE_USER_TIME */

#ifdef HAVE_SYS_TIME
      /* Print system-mode time for this process.  */
      fprintf (fp, "%4ld.%02ld (%2.0f%%) sys", 
	       tv->elapsed.sys / 1000000, 
	       (tv->elapsed.sys % 1000000) / 10000,
	       (total->sys == 0) ? 0.0 
	       : (100.0 * tv->elapsed.sys / (double) total->sys));
#endif /* HAVE_SYS_TIME */

#ifdef HAVE_WALL_TIME
      /* Print wall clock time elapsed.  */
      fprintf (fp, "%4ld.%02ld (%2.0f%%) wall", 
	       tv->elapsed.wall / 1000000, 
	       (tv->elapsed.wall % 1000000) / 10000,
	       (total->wall == 0) ? 0.0 
	       : (100.0 * tv->elapsed.wall / (double) total->wall));
#endif /* HAVE_WALL_TIME */

      fprintf (fp, "\n");
    }

  /* Print total time.  */
  fprintf (fp, " TOTAL                 :");
#ifdef HAVE_USER_TIME
  fprintf (fp, "%4ld.%02ld          ", 
	   total->user / 1000000, (total->user % 1000000) / 10000);
#endif 
#ifdef HAVE_SYS_TIME
  fprintf (fp, "%4ld.%02ld          ", 
	   total->sys  / 1000000, (total->sys  % 1000000) / 10000);
#endif
#ifdef HAVE_WALL_TIME
  fprintf (fp, "%4ld.%02ld\n",
	   total->wall / 1000000, (total->wall % 1000000) / 10000);
#endif
  
#endif /* defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME) 
	  || defined (HAVE_WALL_TIME) */
}

/* Returns time (user + system) used so far by the compiler process,
   in microseconds.  */

long
get_run_time ()
{
  struct timevar_time_def total_elapsed;
  timevar_get (TV_TOTAL, &total_elapsed);
  return total_elapsed.user + total_elapsed.sys;
}

/* Prints a message to stderr stating that time elapsed in STR is
   TOTAL (given in microseconds).  */

void
print_time (str, total)
     const char *str;
     long total;
{
  long all_time = get_run_time ();
  fprintf (stderr,
	   "time in %s: %ld.%06ld (%ld%%)\n",
	   str, total / 1000000, total % 1000000,
 	   all_time == 0 ? 0
 	   : (long) (((100.0 * (double) total) / (double) all_time) + .5));
}
