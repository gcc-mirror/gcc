/* Debug counter for debugging support
   Copyright (C) 2006-2018 Free Software Foundation, Inc.

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

#define DEBUG_COUNTER(a) UINT_MAX,
static unsigned int limit_high[debug_counter_number_of_counters] =
{
#include "dbgcnt.def"
};
#undef DEBUG_COUNTER

static unsigned int limit_low[debug_counter_number_of_counters];

static unsigned int count[debug_counter_number_of_counters];

bool
dbg_cnt_is_enabled (enum debug_counter index)
{
  unsigned v = count[index];
  return v > limit_low[index] && v <= limit_high[index];
}

bool
dbg_cnt (enum debug_counter index)
{
  count[index]++;

  if (dump_file)
    {
      /* Do not print the info for default lower limit.  */
      if (count[index] == limit_low[index] && limit_low[index] > 0)
	fprintf (dump_file, "***dbgcnt: lower limit %d reached for %s.***\n",
		 limit_low[index], map[index].name);
      else if (count[index] == limit_high[index])
	fprintf (dump_file, "***dbgcnt: upper limit %d reached for %s.***\n",
		 limit_high[index], map[index].name);
    }

  return dbg_cnt_is_enabled (index);
}

static void
dbg_cnt_set_limit_by_index (enum debug_counter index, int low, int high)
{
  limit_low[index] = low;
  limit_high[index] = high;

  fprintf (stderr, "dbg_cnt '%s' set to %d-%d\n", map[index].name, low, high);
}

static bool
dbg_cnt_set_limit_by_name (const char *name, int low, int high)
{
  if (high < low)
    {
      error ("-fdbg-cnt=%s:%d:%d has smaller upper limit than the lower",
	     name, low, high);
      return false;
    }

  if (low < 0)
    {
      error ("Lower limit %d of -fdbg-cnt=%s must be a non-negative number", low,
	     name);
      return false;
    }

  if (high < 0)
    {
      error ("Upper limit %d of -fdbg-cnt=%s must be a non-negative number", high,
	     name);
      return false;
    }

  int i;
  for (i = debug_counter_number_of_counters - 1; i >= 0; i--)
    if (strcmp (map[i].name, name) == 0)
      break;

  if (i < 0)
    return false;

  dbg_cnt_set_limit_by_index ((enum debug_counter) i, low, high);
  return true;
}


/* Process a single "name:value" pair.
   Returns NULL if there's no valid pair is found.
   Otherwise returns a pointer to the end of the pair. */

static bool
dbg_cnt_process_single_pair (const char *arg)
{
  char *str = xstrdup (arg);
  char *name = strtok (str, ":");
  char *value1 = strtok (NULL, ":");
  char *value2 = strtok (NULL, ":");

  int high, low;

  if (value1 == NULL)
    return NULL;

  if (value2 == NULL)
    {
      low = 0;
      high = strtol (value1, NULL, 10);
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
  const char *next = strtok (str, ",");
  unsigned int start = 0;

   do {
     if (!dbg_cnt_process_single_pair (arg))
       break;
     start += strlen (arg) + 1;
     next = strtok (NULL, ",");
   } while (next != NULL);

   if (next != NULL)
     {
       char *buffer = XALLOCAVEC (char, start + 2);
       sprintf (buffer, "%*c", start + 1, '^');
       error ("cannot find a valid counter:value pair:");
       error ("-fdbg-cnt=%s", next);
       error ("          %s", buffer);
     }
}

/* Print name, limit and count of all counters.   */

void
dbg_cnt_list_all_counters (void)
{
  int i;
  printf ("  %-32s %-11s %-12s\n", "counter name",  "low limit",
	  "high limit");
  printf ("-----------------------------------------------------------------\n");
  for (i = 0; i < debug_counter_number_of_counters; i++)
    printf ("  %-30s %11u %12u\n",
	    map[i].name, limit_low[map[i].counter], limit_high[map[i].counter]);
  printf ("\n");
}
