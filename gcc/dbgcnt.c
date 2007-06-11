/* Debug counter for debugging support
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  

See dbgcnt.def for usage information.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

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
  return dbg_cnt_is_enabled (index);
}


static void
dbg_cnt_set_limit_by_index (enum debug_counter index, int value)
{
  limit[index] = value;

  fprintf (stderr, "dbg_cnt '%s' set to %d\n", map[index].name, value);
}

static void
dbg_cnt_set_limit_by_name (const char *name, int len, int value)
{
  int i;
  for (i = debug_counter_number_of_counters - 1; i >= 0; i--)
    if (!strncmp (map[i].name, name, len))
      break;

  if (i < 0)
    return;

  dbg_cnt_set_limit_by_index (i, value);
}

void
dbg_cnt_process_opt (const char *arg)
{
   char *colon = strchr (arg, ':');
   char *comma;
   
   if (colon == NULL)
     return;

   dbg_cnt_set_limit_by_name (arg, colon - arg, atoi (colon + 1));

   comma = strchr (colon + 1, ',');
   while (comma)
     {
       colon = strchr (comma + 1, ':');
       if (colon == NULL || !(colon[1] >= '0' && colon[1] <= '9'))
         return;
       dbg_cnt_set_limit_by_name (comma + 1, colon - (comma + 1), atoi (colon + 1));
       comma = strchr (colon + 1, ',');
     }
}
