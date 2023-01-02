/* Debug counter for debugging support
   Copyright (C) 2006-2023 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.

See dbgcnt.def for usage information.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "dumpfile.h"
#include "selftest.h"
#include "intl.h"

#include "dbgcnt.h"

struct string2counter_map {
  const char *name;
  enum debug_counter counter;
};

#define DEBUG_COUNTER(a) { #a , a },

static struct string2counter_map map[debug_counter_number_of_counters] =
{
#include "dbgcnt.def"
};
#undef DEBUG_COUNTER

typedef std::pair<unsigned int, unsigned int> limit_tuple;

static vec<limit_tuple> limits[debug_counter_number_of_counters];
static vec<limit_tuple> original_limits[debug_counter_number_of_counters];

static unsigned int count[debug_counter_number_of_counters];

static void
print_limit_reach (const char *counter, int limit, bool upper_p)
{
  char buffer[128];
  sprintf (buffer, "***dbgcnt: %s limit %d reached for %s.***\n",
	   upper_p ? "upper" : "lower", limit, counter);
  fputs (buffer, stderr);
  if (dump_file)
    fputs (buffer, dump_file);
}

bool
dbg_cnt (enum debug_counter index)
{
  unsigned v = ++count[index];

  if (!limits[index].exists ())
    return true;
  else if (limits[index].is_empty ())
    return false;

  unsigned last = limits[index].length () - 1;
  unsigned int min = limits[index][last].first;
  unsigned int max = limits[index][last].second;

  if (v < min)
    return false;
  else if (v == min)
    {
      print_limit_reach (map[index].name, v, false);
      if (min == max)
	{
	  print_limit_reach (map[index].name, v, true);
	  limits[index].pop ();
	}
      return true;
    }
  else if (v < max)
    return true;
  else if (v == max)
    {
      print_limit_reach (map[index].name, v, true);
      limits[index].pop ();
      return true;
    }
  else
    return false;
}

/* Return the counter for INDEX.  */

unsigned
dbg_cnt_counter (enum debug_counter index)
{
  return count[index];
}

/* Compare limit_tuple intervals by first item in descending order.  */

static int
cmp_tuples (const void *ptr1, const void *ptr2)
{
  const limit_tuple *p1 = (const limit_tuple *)ptr1;
  const limit_tuple *p2 = (const limit_tuple *)ptr2;

  if (p1->first < p2->first)
    return 1;
  else if (p1->first > p2->first)
    return -1;
  return 0;
}

static bool
dbg_cnt_set_limit_by_index (enum debug_counter index, const char *name,
			    unsigned int low, unsigned int high)
{
  if (!limits[index].exists ())
    limits[index].create (1);

  limits[index].safe_push (limit_tuple (low, high));
  limits[index].qsort (cmp_tuples);

  for (unsigned i = 0; i < limits[index].length () - 1; i++)
    {
      limit_tuple t1 = limits[index][i];
      limit_tuple t2 = limits[index][i + 1];
      if (t1.first <= t2.second)
	{
	  error ("Interval overlap of %<-fdbg-cnt=%s%>: [%u, %u] and "
		 "[%u, %u]", name, t2.first, t2.second, t1.first, t1.second);
	  return false;
	}
    }

  original_limits[index] = limits[index].copy ();

  return true;
}

static bool
dbg_cnt_set_limit_by_name (const char *name, unsigned int low,
			   unsigned int high)
{
  if (high < low)
    {
      error ("%<-fdbg-cnt=%s:%d-%d%> has smaller upper limit than the lower",
	     name, low, high);
      return false;
    }

  int i;
  for (i = debug_counter_number_of_counters - 1; i >= 0; i--)
    if (strcmp (map[i].name, name) == 0)
      break;

  if (i < 0)
    {
      error ("cannot find a valid counter name %qs of %<-fdbg-cnt=%> option",
	     name);
      return false;
    }

  return dbg_cnt_set_limit_by_index ((enum debug_counter) i, name, low, high);
}

/* Process a single "low:high" pair.
   Returns NULL if there's no valid pair is found.
   Otherwise returns a pointer to the end of the pair. */

static bool
dbg_cnt_process_single_pair (char *name, char *str)
{
  char *value1 = strtok (str, "-");
  char *value2 = strtok (NULL, "-");

  unsigned int high, low;

  if (value1 == NULL)
    return false;

  if (value2 == NULL)
    {
      high = strtol (value1, NULL, 10);
      /* Let's allow 0:0.  */
      low = high == 0 ? 0 : 1;
    }
  else
    {
      low = strtol (value1, NULL, 10);
      high = strtol (value2, NULL, 10);
    }

  return dbg_cnt_set_limit_by_name (name, low, high);
}

void
dbg_cnt_process_opt (const char *arg)
{
  char *str = xstrdup (arg);

  auto_vec<char *> tokens;
  for (char *next = strtok (str, ","); next != NULL; next = strtok (NULL, ","))
    tokens.safe_push (next);

  unsigned i;
  for (i = 0; i < tokens.length (); i++)
    {
      auto_vec<char *> ranges;
      char *name = strtok (tokens[i], ":");
      for (char *part = strtok (NULL, ":"); part; part = strtok (NULL, ":"))
	ranges.safe_push (part);

      for (unsigned j = 0; j < ranges.length (); j++)
	{
	  if (!dbg_cnt_process_single_pair (name, ranges[j]))
	    break;
	}
    }
}

/* Print name, limit and count of all counters.   */

void
dbg_cnt_list_all_counters (void)
{
  int i;
  fprintf (stderr, "  %-30s%-15s   %s\n", G_("counter name"),
	   G_("counter value"), G_("closed intervals"));
  fprintf (stderr, "-----------------------------------------------------------------\n");
  for (i = 0; i < debug_counter_number_of_counters; i++)
    {
      fprintf (stderr, "  %-30s%-15d   ", map[i].name, count[i]);
      if (original_limits[i].exists ())
	{
	  for (int j = original_limits[i].length () - 1; j >= 0; j--)
	    {
	      fprintf (stderr, "[%u, %u]", original_limits[i][j].first,
		       original_limits[i][j].second);
	      if (j > 0)
		fprintf (stderr, ", ");
	    }
	  fprintf (stderr, "\n");
	}
      else
	fprintf (stderr, "unset\n");
    }
  fprintf (stderr, "\n");
}

#if CHECKING_P

namespace selftest {

/* Selftests.  */

static void
test_sorted_dbg_counters ()
{
  for (unsigned i = 0; i < debug_counter_number_of_counters - 1; i++)
    ASSERT_LT (strcmp (map[i].name, map[i + 1].name), 0);
}

void
dbgcnt_cc_tests ()
{
  test_sorted_dbg_counters ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
