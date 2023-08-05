/* Timing variables for measuring compiler performance.
   Copyright (C) 2000-2023 Free Software Foundation, Inc.
   Contributed by Alex Samuel <samuel@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "timevar.h"
#include "options.h"
#include "json.h"

#ifndef HAVE_CLOCK_T
typedef int clock_t;
#endif

#ifndef HAVE_STRUCT_TMS
struct tms
{
  clock_t tms_utime;
  clock_t tms_stime;
  clock_t tms_cutime;
  clock_t tms_cstime;
};
#endif

#ifndef RUSAGE_SELF
# define RUSAGE_SELF 0
#endif

/* Calculation of scale factor to convert ticks to seconds.
   We mustn't use CLOCKS_PER_SEC except with clock().  */
#if HAVE_SYSCONF && defined _SC_CLK_TCK
# define TICKS_PER_SECOND sysconf (_SC_CLK_TCK) /* POSIX 1003.1-1996 */
#else
# ifdef CLK_TCK
#  define TICKS_PER_SECOND CLK_TCK /* POSIX 1003.1-1988; obsolescent */
# else
#  ifdef HZ
#   define TICKS_PER_SECOND HZ  /* traditional UNIX */
#  else
#   define TICKS_PER_SECOND 100 /* often the correct value */
#  endif
# endif
#endif

/* Prefer times to getrusage to clock (each gives successively less
   information).  */
#ifdef HAVE_TIMES
# if defined HAVE_DECL_TIMES && !HAVE_DECL_TIMES
  extern clock_t times (struct tms *);
# endif
# define USE_TIMES
# define HAVE_USER_TIME
# define HAVE_SYS_TIME
# define HAVE_WALL_TIME
#else
#ifdef HAVE_GETRUSAGE
# if defined HAVE_DECL_GETRUSAGE && !HAVE_DECL_GETRUSAGE
  extern int getrusage (int, struct rusage *);
# endif
# define USE_GETRUSAGE
# define HAVE_USER_TIME
# define HAVE_SYS_TIME
#else
#ifdef HAVE_CLOCK
# if defined HAVE_DECL_CLOCK && !HAVE_DECL_CLOCK
  extern clock_t clock (void);
# endif
# define USE_CLOCK
# define HAVE_USER_TIME
#endif
#endif
#endif

/* libc is very likely to have snuck a call to sysconf() into one of
   the underlying constants, and that can be very slow, so we have to
   precompute them.  Whose wonderful idea was it to make all those
   _constants_ variable at run time, anyway?  */
#define NANOSEC_PER_SEC 1000000000
#ifdef USE_TIMES
static uint64_t ticks_to_nanosec;
#define TICKS_TO_NANOSEC (NANOSEC_PER_SEC / TICKS_PER_SECOND)
#endif

#ifdef USE_CLOCK
static uint64_t clocks_to_nanosec;
#define CLOCKS_TO_NANOSEC (NANOSEC_PER_SEC / CLOCKS_PER_SEC)
#endif

/* Non-NULL if timevars should be used.  In GCC, this happens with
   the -ftime-report flag.  */

timer *g_timer;

/* Total amount of memory allocated by garbage collector.  */

size_t timevar_ggc_mem_total;

/* The amount of memory that will cause us to report the timevar even
   if the time spent is not significant.  */

#define GGC_MEM_BOUND (1 << 20)

/* See timevar.h for an explanation of timing variables.  */

static void get_time (struct timevar_time_def *);
static void timevar_accumulate (struct timevar_time_def *,
				struct timevar_time_def *,
				struct timevar_time_def *);

/* The implementation of timing events for jit client code, allowing
   arbitrary named items to appear on the timing stack.  */

class timer::named_items
{
 public:
  named_items (timer *t);
  ~named_items ();

  void push (const char *item_name);
  void pop ();
  void print (FILE *fp, const timevar_time_def *total);

  json::value *make_json () const;

 private:
  /* Which timer instance does this relate to?  */
  timer *m_timer;

  /* Dictionary, mapping from item names to timevar_def.
     Note that currently we merely store/compare the raw string
     pointers provided by client code; we don't take a copy,
     or use strcmp.  */
  typedef hash_map <const char *, timer::timevar_def> hash_map_t;
  hash_map_t m_hash_map;

  /* The order in which items were originally inserted.  */
  auto_vec <const char *> m_names;
};

/* The constructor for class timer::named_items.  */

timer::named_items::named_items (timer *t)
: m_timer (t),
  m_hash_map (),
  m_names ()
{
}

/* The destructor for class timer::named_items.  */

timer::named_items::~named_items ()
{
}

/* Push the named item onto the timer stack.  */

void
timer::named_items::push (const char *item_name)
{
  gcc_assert (item_name);

  bool existed;
  timer::timevar_def *def = &m_hash_map.get_or_insert (item_name, &existed);
  if (!existed)
    {
      def->elapsed.user = 0;
      def->elapsed.sys = 0;
      def->elapsed.wall = 0;
      def->name = item_name;
      def->standalone = 0;
      m_names.safe_push (item_name);
    }
  m_timer->push_internal (def);
}

/* Pop the top item from the timer stack.  */

void
timer::named_items::pop ()
{
  m_timer->pop_internal ();
}

/* Print the given client item.  Helper function for timer::print.  */

void
timer::named_items::print (FILE *fp, const timevar_time_def *total)
{
  fprintf (fp, "Client items:\n");
  for (const char *item_name : m_names)
    {
      timer::timevar_def *def = m_hash_map.get (item_name);
      gcc_assert (def);
      m_timer->print_row (fp, total, def->name, def->elapsed);
    }
}

/* Create a json value representing this object, suitable for use
   in SARIF output.  */

json::value *
timer::named_items::make_json () const
{
  json::array *arr = new json::array ();
  for (const char *item_name : m_names)
    {
      hash_map_t &mut_map = const_cast <hash_map_t &> (m_hash_map);
      timer::timevar_def *def = mut_map.get (item_name);
      gcc_assert (def);
      arr->append (def->make_json ());
    }
  return arr;
}

/* Fill the current times into TIME.  The definition of this function
   also defines any or all of the HAVE_USER_TIME, HAVE_SYS_TIME, and
   HAVE_WALL_TIME macros.  */

static void
get_time (struct timevar_time_def *now)
{
  now->user = 0;
  now->sys  = 0;
  now->wall = 0;
  now->ggc_mem = timevar_ggc_mem_total;

  {
#ifdef USE_TIMES
    struct tms tms;
    now->wall = times (&tms)  * ticks_to_nanosec;
    now->user = tms.tms_utime * ticks_to_nanosec;
    now->sys  = tms.tms_stime * ticks_to_nanosec;
#endif
#ifdef USE_GETRUSAGE
    struct rusage rusage;
    getrusage (RUSAGE_SELF, &rusage);
    now->user = rusage.ru_utime.tv_sec * NANOSEC_PER_SEC
		+ rusage.ru_utime.tv_usec * 1000;
    now->sys  = rusage.ru_stime.tv_sec * NANOSEC_PER_SEC
		+ rusage.ru_stime.tv_usec * 1000;
#endif
#ifdef USE_CLOCK
    now->user = clock () * clocks_to_nanosec;
#endif
  }
}

/* Add the difference between STOP_TIME and START_TIME to TIMER.  */

static void
timevar_accumulate (struct timevar_time_def *timer,
		    struct timevar_time_def *start_time,
		    struct timevar_time_def *stop_time)
{
  timer->user += stop_time->user - start_time->user;
  timer->sys += stop_time->sys - start_time->sys;
  timer->wall += stop_time->wall - start_time->wall;
  timer->ggc_mem += stop_time->ggc_mem - start_time->ggc_mem;
}

/* Get the difference between STOP_TIME and START_TIME.  */

static void
timevar_diff (struct timevar_time_def *out,
	      const timevar_time_def &start_time,
	      const timevar_time_def &stop_time)
{
  out->user = stop_time.user - start_time.user;
  out->sys = stop_time.sys - start_time.sys;
  out->wall = stop_time.wall - start_time.wall;
  out->ggc_mem = stop_time.ggc_mem - start_time.ggc_mem;
}

/* Class timer's constructor.  */

timer::timer () :
  m_stack (NULL),
  m_unused_stack_instances (NULL),
  m_start_time (),
  m_jit_client_items (NULL)
{
  /* Zero all elapsed times.  */
  memset (m_timevars, 0, sizeof (m_timevars));

  /* Initialize the names of timing variables.  */
#define DEFTIMEVAR(identifier__, name__) \
  m_timevars[identifier__].name = name__;
#include "timevar.def"
#undef DEFTIMEVAR

  /* Initialize configuration-specific state.
     Ideally this would be one-time initialization.  */
#ifdef USE_TIMES
  ticks_to_nanosec = TICKS_TO_NANOSEC;
#endif
#ifdef USE_CLOCK
  clocks_to_nanosec = CLOCKS_TO_NANOSEC;
#endif
}

/* Class timer's destructor.  */

timer::~timer ()
{
  timevar_stack_def *iter, *next;

  for (iter = m_stack; iter; iter = next)
    {
      next = iter->next;
      free (iter);
    }
  for (iter = m_unused_stack_instances; iter; iter = next)
    {
      next = iter->next;
      free (iter);
    }
  for (unsigned i = 0; i < TIMEVAR_LAST; ++i)
    delete m_timevars[i].children;

  delete m_jit_client_items;
}

/* Initialize timing variables.  */

void
timevar_init (void)
{
  if (g_timer)
    return;

  g_timer = new timer ();
}

/* Push TIMEVAR onto the timing stack.  No further elapsed time is
   attributed to the previous topmost timing variable on the stack;
   subsequent elapsed time is attributed to TIMEVAR, until it is
   popped or another element is pushed on top.

   TIMEVAR cannot be running as a standalone timer.  */

void
timer::push (timevar_id_t timevar)
{
  struct timevar_def *tv = &m_timevars[timevar];
  push_internal (tv);
}

/* Push TV onto the timing stack, either one of the builtin ones
   for a timevar_id_t, or one provided by client code to libgccjit.  */

void
timer::push_internal (struct timevar_def *tv)
{
  struct timevar_stack_def *context;
  struct timevar_time_def now;

  gcc_assert (tv);

  /* Mark this timing variable as used.  */
  tv->used = 1;

  /* Can't push a standalone timer.  */
  gcc_assert (!tv->standalone);

  /* What time is it?  */
  get_time (&now);

  /* If the stack isn't empty, attribute the current elapsed time to
     the old topmost element.  */
  if (m_stack)
    timevar_accumulate (&m_stack->timevar->elapsed, &m_start_time, &now);

  /* Reset the start time; from now on, time is attributed to
     TIMEVAR.  */
  m_start_time = now;

  /* See if we have a previously-allocated stack instance.  If so,
     take it off the list.  If not, malloc a new one.  */
  if (m_unused_stack_instances != NULL)
    {
      context = m_unused_stack_instances;
      m_unused_stack_instances = m_unused_stack_instances->next;
    }
  else
    context = XNEW (struct timevar_stack_def);

  /* Fill it in and put it on the stack.  */
  context->timevar = tv;
  context->next = m_stack;
  m_stack = context;
}

/* Pop the topmost timing variable element off the timing stack.  The
   popped variable must be TIMEVAR.  Elapsed time since the that
   element was pushed on, or since it was last exposed on top of the
   stack when the element above it was popped off, is credited to that
   timing variable.  */

void
timer::pop (timevar_id_t timevar)
{
  gcc_assert (&m_timevars[timevar] == m_stack->timevar);

  pop_internal ();
}

/* Pop the topmost item from the stack, either one of the builtin ones
   for a timevar_id_t, or one provided by client code to libgccjit.  */

void
timer::pop_internal ()
{
  struct timevar_time_def now;
  struct timevar_stack_def *popped = m_stack;

  /* What time is it?  */
  get_time (&now);

  /* Attribute the elapsed time to the element we're popping.  */
  timevar_accumulate (&popped->timevar->elapsed, &m_start_time, &now);

  /* Take the item off the stack.  */
  m_stack = m_stack->next;

  /* Record the elapsed sub-time to the parent as well.  */
  if (m_stack && time_report_details)
    {
      if (! m_stack->timevar->children)
	m_stack->timevar->children = new child_map_t (5);
      bool existed_p;
      timevar_time_def &time
	= m_stack->timevar->children->get_or_insert (popped->timevar, &existed_p);
      if (! existed_p)
	memset (&time, 0, sizeof (timevar_time_def));
      timevar_accumulate (&time, &m_start_time, &now);
    }

  /* Reset the start time; from now on, time is attributed to the
     element just exposed on the stack.  */
  m_start_time = now;

  /* Don't delete the stack element; instead, add it to the list of
     unused elements for later use.  */
  popped->next = m_unused_stack_instances;
  m_unused_stack_instances = popped;
}

/* Start timing TIMEVAR independently of the timing stack.  Elapsed
   time until timevar_stop is called for the same timing variable is
   attributed to TIMEVAR.  */

void
timevar_start (timevar_id_t timevar)
{
  if (!g_timer)
    return;

  g_timer->start (timevar);
}

/* See timevar_start above.  */

void
timer::start (timevar_id_t timevar)
{
  struct timevar_def *tv = &m_timevars[timevar];

  /* Mark this timing variable as used.  */
  tv->used = 1;

  /* Don't allow the same timing variable to be started more than
     once.  */
  gcc_assert (!tv->standalone);
  tv->standalone = 1;

  get_time (&tv->start_time);
}

/* Stop timing TIMEVAR.  Time elapsed since timevar_start was called
   is attributed to it.  */

void
timevar_stop (timevar_id_t timevar)
{
  if (!g_timer)
    return;

  g_timer->stop (timevar);
}

/* See timevar_stop above.  */

void
timer::stop (timevar_id_t timevar)
{
  struct timevar_def *tv = &m_timevars[timevar];
  struct timevar_time_def now;

  /* TIMEVAR must have been started via timevar_start.  */
  gcc_assert (tv->standalone);
  tv->standalone = 0; /* Enable a restart.  */

  get_time (&now);
  timevar_accumulate (&tv->elapsed, &tv->start_time, &now);
}


/* Conditionally start timing TIMEVAR independently of the timing stack.
   If the timer is already running, leave it running and return true.
   Otherwise, start the timer and return false.
   Elapsed time until the corresponding timevar_cond_stop
   is called for the same timing variable is attributed to TIMEVAR.  */

bool
timevar_cond_start (timevar_id_t timevar)
{
  if (!g_timer)
    return false;

  return g_timer->cond_start (timevar);
}

/* See timevar_cond_start above.  */

bool
timer::cond_start (timevar_id_t timevar)
{
  struct timevar_def *tv = &m_timevars[timevar];

  /* Mark this timing variable as used.  */
  tv->used = 1;

  if (tv->standalone)
    return true;  /* The timevar is already running.  */

  /* Don't allow the same timing variable
     to be unconditionally started more than once.  */
  tv->standalone = 1;

  get_time (&tv->start_time);
  return false;  /* The timevar was not already running.  */
}

/* Conditionally stop timing TIMEVAR.  The RUNNING parameter must come
   from the return value of a dynamically matching timevar_cond_start.
   If the timer had already been RUNNING, do nothing.  Otherwise, time
   elapsed since timevar_cond_start was called is attributed to it.  */

void
timevar_cond_stop (timevar_id_t timevar, bool running)
{
  if (!g_timer || running)
    return;

  g_timer->cond_stop (timevar);
}

/* See timevar_cond_stop above.  */

void
timer::cond_stop (timevar_id_t timevar)
{
  struct timevar_def *tv;
  struct timevar_time_def now;

  tv = &m_timevars[timevar];

  /* TIMEVAR must have been started via timevar_cond_start.  */
  gcc_assert (tv->standalone);
  tv->standalone = 0; /* Enable a restart.  */

  get_time (&now);
  timevar_accumulate (&tv->elapsed, &tv->start_time, &now);
}

/* Push the named item onto the timing stack.  */

void
timer::push_client_item (const char *item_name)
{
  gcc_assert (item_name);

  /* Lazily create the named_items instance.  */
  if (!m_jit_client_items)
    m_jit_client_items = new named_items (this);

  m_jit_client_items->push (item_name);
}

/* Pop the top-most client item from the timing stack.  */

void
timer::pop_client_item ()
{
  gcc_assert (m_jit_client_items);
  m_jit_client_items->pop ();
}

/* Validate that phase times are consistent.  */

void
timer::validate_phases (FILE *fp) const
{
  unsigned int /* timevar_id_t */ id;
  const timevar_time_def *total = &m_timevars[TV_TOTAL].elapsed;
  uint64_t phase_user = 0;
  uint64_t phase_sys = 0;
  uint64_t phase_wall = 0;
  size_t phase_ggc_mem = 0;
  static char phase_prefix[] = "phase ";

  for (id = 0; id < (unsigned int) TIMEVAR_LAST; ++id)
    {
      const timevar_def *tv = &m_timevars[(timevar_id_t) id];

      /* Don't evaluate timing variables that were never used.  */
      if (!tv->used)
	continue;

      if (startswith (tv->name, phase_prefix))
	{
	  phase_user += tv->elapsed.user;
	  phase_sys += tv->elapsed.sys;
	  phase_wall += tv->elapsed.wall;
	  phase_ggc_mem += tv->elapsed.ggc_mem;
	}
    }

  if (phase_user > total->user
      || phase_sys > total->sys
      || phase_wall > total->wall
      || phase_ggc_mem > total->ggc_mem)
    {

      fprintf (fp, "Timing error: total of phase timers exceeds total time.\n");
      if (phase_user > total->user)
	fprintf (fp, "user    %13" PRIu64 " > %13" PRIu64 "\n",
		 phase_user, total->user);
      if (phase_sys > total->sys)
	fprintf (fp, "sys     %13" PRIu64 " > %13" PRIu64 "\n",
		 phase_sys, total->sys);
      if (phase_wall > total->wall)
	fprintf (fp, "wall    %13" PRIu64 " > %13" PRIu64 "\n",
		 phase_wall, total->wall);
      if (phase_ggc_mem > total->ggc_mem)
	fprintf (fp, "ggc_mem %13lu > %13lu\n", (unsigned long)phase_ggc_mem,
		 (unsigned long)total->ggc_mem);
      gcc_unreachable ();
    }
}

#define nanosec_to_floating_sec(NANO) ((double)(NANO) * 1e-9)
#define percent_of(TOTAL, SUBTOTAL) \
  ((TOTAL) == 0 ? 0 : ((double)SUBTOTAL / TOTAL) * 100)
/* Helper function for timer::print.  */

void
timer::print_row (FILE *fp,
		  const timevar_time_def *total,
		  const char *name, const timevar_time_def &elapsed)
{
  /* The timing variable name.  */
  fprintf (fp, " %-35s:", name);

#ifdef HAVE_USER_TIME
  /* Print user-mode time for this process.  */
  fprintf (fp, "%7.2f (%3.0f%%)",
	   nanosec_to_floating_sec (elapsed.user),
	   percent_of (total->user, elapsed.user));
#endif /* HAVE_USER_TIME */

#ifdef HAVE_SYS_TIME
  /* Print system-mode time for this process.  */
  fprintf (fp, "%7.2f (%3.0f%%)",
	   nanosec_to_floating_sec (elapsed.sys),
	   percent_of (total->sys, elapsed.sys));
#endif /* HAVE_SYS_TIME */

#ifdef HAVE_WALL_TIME
  /* Print wall clock time elapsed.  */
  fprintf (fp, "%7.2f (%3.0f%%)",
	   nanosec_to_floating_sec (elapsed.wall),
	   percent_of (total->wall, elapsed.wall));
#endif /* HAVE_WALL_TIME */

  /* Print the amount of ggc memory allocated.  */
  fprintf (fp, PRsa (6) " (%3.0f%%)",
	   SIZE_AMOUNT (elapsed.ggc_mem),
	   (total->ggc_mem == 0
	    ? 0
	    : (float) elapsed.ggc_mem / total->ggc_mem) * 100);

  putc ('\n', fp);
}

/* Return whether ELAPSED is all zero.  */

bool
timer::all_zero (const timevar_time_def &elapsed)
{
  /* 5000000 nanosec == 5e-3 seconds.  */
  uint64_t tiny = 5000000;
  return (elapsed.user < tiny
	  && elapsed.sys < tiny
	  && elapsed.wall < tiny
	  && elapsed.ggc_mem < GGC_MEM_BOUND);
}

/* Summarize timing variables to FP.  The timing variable TV_TOTAL has
   a special meaning -- it's considered to be the total elapsed time,
   for normalizing the others, and is displayed last.  */

void
timer::print (FILE *fp)
{
  /* Only print stuff if we have some sort of time information.  */
#if defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME) || defined (HAVE_WALL_TIME)
  unsigned int /* timevar_id_t */ id;
  const timevar_time_def *total = &m_timevars[TV_TOTAL].elapsed;
  struct timevar_time_def now;

  /* Update timing information in case we're calling this from GDB.  */

  if (fp == 0)
    fp = stderr;

  /* What time is it?  */
  get_time (&now);

  /* If the stack isn't empty, attribute the current elapsed time to
     the old topmost element.  */
  if (m_stack)
    timevar_accumulate (&m_stack->timevar->elapsed, &m_start_time, &now);

  /* Reset the start time; from now on, time is attributed to
     TIMEVAR.  */
  m_start_time = now;

  fprintf (fp, "\n%-35s%16s%14s%14s%14s\n", "Time variable", "usr", "sys",
	   "wall", "GGC");
  if (m_jit_client_items)
    fputs ("GCC items:\n", fp);
  for (id = 0; id < (unsigned int) TIMEVAR_LAST; ++id)
    {
      const timevar_def *tv = &m_timevars[(timevar_id_t) id];

      /* Don't print the total execution time here; that goes at the
	 end.  */
      if ((timevar_id_t) id == TV_TOTAL)
	continue;

      /* Don't print timing variables that were never used.  */
      if (!tv->used)
	continue;

      bool any_children_with_time = false;
      if (tv->children)
	for (child_map_t::iterator i = tv->children->begin ();
	     i != tv->children->end (); ++i)
	  if (! all_zero ((*i).second))
	    {
	      any_children_with_time = true;
	      break;
	    }

      /* Don't print timing variables if we're going to get a row of
         zeroes.  Unless there are children with non-zero time.  */
      if (! any_children_with_time
	  && all_zero (tv->elapsed))
	continue;

      print_row (fp, total, tv->name, tv->elapsed);

      if (tv->children)
	for (child_map_t::iterator i = tv->children->begin ();
	     i != tv->children->end (); ++i)
	  {
	    timevar_def *tv2 = (*i).first;
	    /* Don't print timing variables if we're going to get a row of
	       zeroes.  */
	    if (! all_zero ((*i).second))
	      {
		char lname[256];
		snprintf (lname, 256, "`- %s", tv2->name);
		print_row (fp, total, lname, (*i).second);
	      }
	  }
    }
  if (m_jit_client_items)
    m_jit_client_items->print (fp, total);

  /* Print total time.  */
  fprintf (fp, " %-35s:", "TOTAL");
#ifdef HAVE_USER_TIME
  fprintf (fp, "%7.2f      ", nanosec_to_floating_sec (total->user));
#endif
#ifdef HAVE_SYS_TIME
  fprintf (fp, "%8.2f      ", nanosec_to_floating_sec (total->sys));
#endif
#ifdef HAVE_WALL_TIME
  fprintf (fp, "%8.2f      ", nanosec_to_floating_sec (total->wall));
#endif
  fprintf (fp, PRsa (7) "\n", SIZE_AMOUNT (total->ggc_mem));

  if (CHECKING_P || flag_checking)
    fprintf (fp, "Extra diagnostic checks enabled; compiler may run slowly.\n");
  if (CHECKING_P)
    fprintf (fp, "Configure with --enable-checking=release to disable checks.\n");
#ifndef ENABLE_ASSERT_CHECKING
  fprintf (fp, "Internal checks disabled; compiler is not suited for release.\n");
  fprintf (fp, "Configure with --enable-checking=release to enable checks.\n");
#endif

#endif /* defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME)
	  || defined (HAVE_WALL_TIME) */

  validate_phases (fp);
}

/* Create a json value representing this object, suitable for use
   in SARIF output.  */

json::object *
make_json_for_timevar_time_def (const timevar_time_def &ttd)
{
  json::object *obj = new json::object ();
  obj->set ("user",
	    new json::float_number (nanosec_to_floating_sec (ttd.user)));
  obj->set ("sys", new json::float_number (nanosec_to_floating_sec (ttd.sys)));
  obj->set ("wall",
	    new json::float_number (nanosec_to_floating_sec (ttd.wall)));
  obj->set ("ggc_mem", new json::integer_number (ttd.ggc_mem));
  return obj;
}
#undef nanosec_to_floating_sec
#undef percent_of

/* Create a json value representing this object, suitable for use
   in SARIF output.  */

json::value *
timer::timevar_def::make_json () const
{
  json::object *timevar_obj = new json::object ();
  timevar_obj->set ("name", new json::string (name));
  timevar_obj->set ("elapsed", make_json_for_timevar_time_def (elapsed));

  if (children)
    {
      bool any_children_with_time = false;
      for (auto i : *children)
	if (!all_zero (i.second))
	  {
	    any_children_with_time = true;
	    break;
	  }
      if (any_children_with_time)
	{
	  json::array *children_arr = new json::array ();
	  timevar_obj->set ("children", children_arr);
	  for (auto i : *children)
	    {
	      /* Don't emit timing variables if we're going to get a row of
		 zeroes.  */
	      if (all_zero (i.second))
		continue;
	      json::object *child_obj = new json::object;
	      children_arr->append (child_obj);
	      child_obj->set ("name", new json::string (i.first->name));
	      child_obj->set ("elapsed",
			      make_json_for_timevar_time_def (i.second));
	    }
	}
    }

  return timevar_obj;
}

/* Create a json value representing this object, suitable for use
   in SARIF output.  */

json::value *
timer::make_json () const
{
  /* Only generate stuff if we have some sort of time information.  */
#if defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME) || defined (HAVE_WALL_TIME)
  json::object *report_obj = new json::object ();
  json::array *json_arr = new json::array ();
  report_obj->set ("timevars", json_arr);

  for (unsigned id = 0; id < (unsigned int) TIMEVAR_LAST; ++id)
    {
      const timevar_def *tv = &m_timevars[(timevar_id_t) id];

      /* Don't print the total execution time here; this isn't initialized
	 by the time the sarif output runs.  */
      if ((timevar_id_t) id == TV_TOTAL)
	continue;

      /* Don't emit timing variables that were never used.  */
      if (!tv->used)
	continue;

      bool any_children_with_time = false;
      if (tv->children)
	for (child_map_t::iterator i = tv->children->begin ();
	     i != tv->children->end (); ++i)
	  if (! all_zero ((*i).second))
	    {
	      any_children_with_time = true;
	      break;
	    }

      /* Don't emit timing variables if we're going to get a row of
	 zeroes.  Unless there are children with non-zero time.  */
      if (! any_children_with_time
	  && all_zero (tv->elapsed))
	continue;

      json_arr->append (tv->make_json ());
    }

  /* Special-case for total.  */
  {
    /* Get our own total up till now, without affecting TV_TOTAL.  */
    struct timevar_time_def total_now;
    struct timevar_time_def total_elapsed;
    get_time (&total_now);
    timevar_diff (&total_elapsed, m_timevars[TV_TOTAL].start_time, total_now);

    json::object *total_obj = new json::object();
    json_arr->append (total_obj);
    total_obj->set ("name", new json::string ("TOTAL"));
    total_obj->set ("elapsed", make_json_for_timevar_time_def (total_elapsed));
  }

  if (m_jit_client_items)
    report_obj->set ("client_items", m_jit_client_items->make_json ());

  report_obj->set ("CHECKING_P", new json::literal ((bool)CHECKING_P));
  report_obj->set ("flag_checking", new json::literal ((bool)flag_checking));

  return report_obj;

#else /* defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME)
	  || defined (HAVE_WALL_TIME) */
  return NULL;
#endif /* defined (HAVE_USER_TIME) || defined (HAVE_SYS_TIME)
	  || defined (HAVE_WALL_TIME) */
}

/* Get the name of the topmost item.  For use by jit for validating
   inputs to gcc_jit_timer_pop.  */
const char *
timer::get_topmost_item_name () const
{
  if (m_stack)
    return m_stack->timevar->name;
  else
    return NULL;
}

/* Prints a message to stderr stating that time elapsed in STR is
   TOTAL (given in microseconds).  */

void
print_time (const char *str, long total)
{
  long all_time = get_run_time ();
  fprintf (stderr,
	   "time in %s: %ld.%06ld (%ld%%)\n",
	   str, total / 1000000, total % 1000000,
	   all_time == 0 ? 0
	   : (long) (((100.0 * (double) total) / (double) all_time) + .5));
}
