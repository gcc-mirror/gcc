/* Debug counter for debugging support
   Copyright (C) 2006-2016 Free Software Foundation, Inc.

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
static unsigned int limit[debug_counter_number_of_counters] =
{
#include "dbgcnt.def"
};
#undef DEBUG_COUNTER

static unsigned int count[debug_counter_number_of_counters];

bool
dbg_cnt_is_enabled (enum debug_counter index)
{
  return count[index] <= limit[index];
}

bool
dbg_cnt (enum debug_counter index)
{
  count[index]++;
  if (dump_file && count[index] == limit[index])
    fprintf (dump_file, "***dbgcnt: limit reached for %s.***\n",
	     map[index].name);

  return dbg_cnt_is_enabled (index);
}


static void
dbg_cnt_set_limit_by_index (enum debug_counter index, int value)
{
  limit[index] = value;

  fprintf (stderr, "dbg_cnt '%s' set to %d\n", map[index].name, value);
}

static bool
dbg_cnt_set_limit_by_name (const char *name, int len, int value)
{
  int i;
  for (i = debug_counter_number_of_counters - 1; i >= 0; i--)
    if (strncmp (map[i].name, name, len) == 0
        && map[i].name[len] == '\0')
      break;

  if (i < 0)
    return false;

  dbg_cnt_set_limit_by_index ((enum debug_counter) i, value);
  return true;
}


/* Process a single "name:value" pair.
   Returns NULL if there's no valid pair is found.
   Otherwise returns a pointer to the end of the pair. */

static const char *
dbg_cnt_process_single_pair (const char *arg)
{
   const char *colon = strchr (arg, ':');
   char *endptr = NULL;
   int value;

   if (colon == NULL)
     return NULL;

   value = strtol (colon + 1, &endptr, 10);

   if (endptr != NULL && endptr != colon + 1
       && dbg_cnt_set_limit_by_name (arg, colon - arg, value))
     return endptr;

   return NULL;
}

void
dbg_cnt_process_opt (const char *arg)
{
   const char *start = arg;
   const char *next;
   do {
     next = dbg_cnt_process_single_pair (arg);
     if (next == NULL)
       break;
   } while (*next == ',' && (arg = next + 1));

   if (next == NULL || *next != 0)
     {
       char *buffer = XALLOCAVEC (char, arg - start + 2);
       sprintf (buffer, "%*c", (int)(1 + (arg - start)), '^');
       error ("cannot find a valid counter:value pair:");
       error ("-fdbg-cnt=%s", start);
       error ("          %s", buffer);
     }
}

/* Print name, limit and count of all counters.   */

void
dbg_cnt_list_all_counters (void)
{
  int i;
  printf ("  %-30s %-5s %-5s\n", "counter name",  "limit", "value");
  printf ("----------------------------------------------\n");
  for (i = 0; i < debug_counter_number_of_counters; i++)
    printf ("  %-30s %5d %5u\n",
            map[i].name, limit[map[i].counter], count[map[i].counter]);
  printf ("\n");
}
