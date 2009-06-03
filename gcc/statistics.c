/* Optimization statistics functions.
   Copyright (C) 2008
   Free Software Foundation, Inc.
   Contributed by Richard Guenther  <rguenther@suse.de>

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
#include "tree-pass.h"
#include "tree-dump.h"
#include "statistics.h"
#include "hashtab.h"
#include "tm.h"
#include "function.h"

static int statistics_dump_nr;
static int statistics_dump_flags;
static FILE *statistics_dump_file;

/* Statistics entry.  A integer counter associated to a string ID
   and value.  */

typedef struct statistics_counter_s {
  const char *id;
  int val;
  bool histogram_p;
  unsigned HOST_WIDE_INT count;
  unsigned HOST_WIDE_INT prev_dumped_count;
} statistics_counter_t;

/* Array of statistic hashes, indexed by pass id.  */
static htab_t *statistics_hashes;
static unsigned nr_statistics_hashes;

/* Hash a statistic counter by its string ID.  */

static hashval_t
hash_statistics_hash (const void *p)
{
  const statistics_counter_t *const c = (const statistics_counter_t *)p;
  return htab_hash_string (c->id) + c->val;
}

/* Compare two statistic counters by their string IDs.  */

static int
hash_statistics_eq (const void *p, const void *q)
{
  const statistics_counter_t *const c1 = (const statistics_counter_t *)p;
  const statistics_counter_t *const c2 = (const statistics_counter_t *)q;
  return c1->val == c2->val && strcmp (c1->id, c2->id) == 0;
}

/* Free a statistics entry.  */

static void
hash_statistics_free (void *p)
{
  free (CONST_CAST(char *, ((statistics_counter_t *)p)->id));
  free (p);
}

/* Return the current hashtable to be used for recording or printing
   statistics.  */

static htab_t
curr_statistics_hash (void)
{
  unsigned idx;

  gcc_assert (current_pass->static_pass_number >= 0);
  idx = current_pass->static_pass_number;

  if (idx < nr_statistics_hashes
      && statistics_hashes[idx] != NULL)
    return statistics_hashes[idx];

  if (idx >= nr_statistics_hashes)
    {
      statistics_hashes = XRESIZEVEC (struct htab *, statistics_hashes, idx+1);
      memset (statistics_hashes + nr_statistics_hashes, 0,
	      (idx + 1 - nr_statistics_hashes) * sizeof (htab_t));
      nr_statistics_hashes = idx + 1;
    }

  statistics_hashes[idx] = htab_create (15, hash_statistics_hash,
					hash_statistics_eq,
					hash_statistics_free);

  return statistics_hashes[idx];
}

/* Helper for statistics_fini_pass.  Print the counter difference
   since the last dump for the pass dump files.  */

static int
statistics_fini_pass_1 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  statistics_counter_t *counter = (statistics_counter_t *)*slot;
  unsigned HOST_WIDE_INT count = counter->count - counter->prev_dumped_count;
  if (count == 0)
    return 1;
  if (counter->histogram_p)
    fprintf (dump_file, "%s == %d: " HOST_WIDE_INT_PRINT_DEC "\n",
	     counter->id, counter->val, count);
  else
    fprintf (dump_file, "%s: " HOST_WIDE_INT_PRINT_DEC "\n",
	     counter->id, count);
  counter->prev_dumped_count = counter->count;
  return 1;
}

/* Helper for statistics_fini_pass.  Print the counter difference
   since the last dump for the statistics dump.  */

static int
statistics_fini_pass_2 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  statistics_counter_t *counter = (statistics_counter_t *)*slot;
  unsigned HOST_WIDE_INT count = counter->count - counter->prev_dumped_count;
  if (count == 0)
    return 1;
  counter->prev_dumped_count = counter->count;
  if (counter->histogram_p)
    fprintf (statistics_dump_file,
	     "%d %s \"%s == %d\" \"%s\" " HOST_WIDE_INT_PRINT_DEC "\n",
	     current_pass->static_pass_number,
	     current_pass->name,
	     counter->id, counter->val,
	     cfun ? IDENTIFIER_POINTER (DECL_NAME (cfun->decl)) : "(nofn)",
	     count);
  else
    fprintf (statistics_dump_file,
	     "%d %s \"%s\" \"%s\" " HOST_WIDE_INT_PRINT_DEC "\n",
	     current_pass->static_pass_number,
	     current_pass->name,
	     counter->id,
	     cfun ? IDENTIFIER_POINTER (DECL_NAME (cfun->decl)) : "(nofn)",
	     count);
  counter->prev_dumped_count = counter->count;
  return 1;
}

/* Helper for statistics_fini_pass, reset the counters.  */

static int
statistics_fini_pass_3 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  statistics_counter_t *counter = (statistics_counter_t *)*slot;
  counter->prev_dumped_count = counter->count;
  return 1;
}

/* Dump the current statistics incrementally.  */

void
statistics_fini_pass (void)
{
  if (current_pass->static_pass_number == -1)
    return;

  if (dump_file
      && dump_flags & TDF_STATS)
    {
      fprintf (dump_file, "\n");
      fprintf (dump_file, "Pass statistics:\n");
      fprintf (dump_file, "----------------\n");
      htab_traverse_noresize (curr_statistics_hash (),
			      statistics_fini_pass_1, NULL);
      fprintf (dump_file, "\n");
    }
  if (statistics_dump_file
      && !(statistics_dump_flags & TDF_STATS
	   || statistics_dump_flags & TDF_DETAILS))
    htab_traverse_noresize (curr_statistics_hash (),
			    statistics_fini_pass_2, NULL);
  htab_traverse_noresize (curr_statistics_hash (),
			  statistics_fini_pass_3, NULL);
}

/* Helper for printing summary information.  */

static int
statistics_fini_1 (void **slot, void *data)
{
  struct opt_pass *pass = (struct opt_pass *)data;
  statistics_counter_t *counter = (statistics_counter_t *)*slot;
  if (counter->count == 0)
    return 1;
  if (counter->histogram_p)
    fprintf (statistics_dump_file,
	     "%d %s \"%s == %d\" " HOST_WIDE_INT_PRINT_DEC "\n",
	     pass->static_pass_number,
	     pass->name,
	     counter->id, counter->val,
	     counter->count);
  else
    fprintf (statistics_dump_file,
	     "%d %s \"%s\" " HOST_WIDE_INT_PRINT_DEC "\n",
	     pass->static_pass_number,
	     pass->name,
	     counter->id,
	     counter->count);
  return 1;
}

/* Finish the statistics and dump summary information.  */

void
statistics_fini (void)
{
  if (!statistics_dump_file)
    return;

  if (statistics_dump_flags & TDF_STATS)
    {
      unsigned i;
      for (i = 0; i < nr_statistics_hashes; ++i)
	if (statistics_hashes[i] != NULL
	    && get_pass_for_id (i) != NULL)
	  htab_traverse_noresize (statistics_hashes[i],
				  statistics_fini_1, get_pass_for_id (i));
    }

  dump_end (statistics_dump_nr, statistics_dump_file);
}

/* Register the statistics dump file.  */

void
statistics_early_init (void)
{
  statistics_dump_nr = dump_register (".statistics", "statistics",
				      "statistics", TDF_TREE);
}

/* Init the statistics.  */

void
statistics_init (void)
{
  statistics_dump_file = dump_begin (statistics_dump_nr, NULL);
  statistics_dump_flags = get_dump_file_info (statistics_dump_nr)->flags;
}

/* Lookup or add a statistics counter in the hashtable HASH with ID, VAL
   and HISTOGRAM_P.  */

static statistics_counter_t *
lookup_or_add_counter (htab_t hash, const char *id, int val,
		       bool histogram_p)
{
  statistics_counter_t **counter;
  statistics_counter_t c;
  c.id = id;
  c.val = val;
  counter = (statistics_counter_t **) htab_find_slot (hash, &c, INSERT);
  if (!*counter)
    {
      *counter = XNEW (struct statistics_counter_s);
      (*counter)->id = xstrdup (id);
      (*counter)->val = val;
      (*counter)->histogram_p = histogram_p;
      (*counter)->prev_dumped_count = 0;
      (*counter)->count = 0;
    }
  return *counter;
}

/* Add statistics information about event ID in function FN.
   This will increment the counter associated with ID by INCR.
   It will also dump the event to the global statistics file if requested.  */

void
statistics_counter_event (struct function *fn, const char *id, int incr)
{
  statistics_counter_t *counter;

  if ((!(dump_flags & TDF_STATS)
       && !statistics_dump_file)
      || incr == 0)
    return;

  if (current_pass->static_pass_number != -1)
    {
      counter = lookup_or_add_counter (curr_statistics_hash (), id, 0, false);
      gcc_assert (!counter->histogram_p);
      counter->count += incr;
    }

  if (!statistics_dump_file
      || !(statistics_dump_flags & TDF_DETAILS))
    return;

  fprintf (statistics_dump_file,
	   "%d %s \"%s\" \"%s\" %d\n",
	   current_pass->static_pass_number,
	   current_pass->name,
	   id,
	   fn ? IDENTIFIER_POINTER (DECL_NAME (fn->decl)) : "(nofn)",
	   incr);
}

/* Add statistics information about event ID in function FN with the
   histogram value VAL.
   It will dump the event to the global statistics file if requested.  */

void
statistics_histogram_event (struct function *fn, const char *id, int val)
{
  statistics_counter_t *counter;

  if (!(dump_flags & TDF_STATS)
      && !statistics_dump_file)
    return;

  counter = lookup_or_add_counter (curr_statistics_hash (), id, val, true);
  gcc_assert (counter->histogram_p);
  counter->count += 1;

  if (!statistics_dump_file
      || !(statistics_dump_flags & TDF_DETAILS))
    return;

  fprintf (statistics_dump_file,
	   "%d %s \"%s == %d\" \"%s\" 1\n",
	   current_pass->static_pass_number,
	   current_pass->name,
	   id, val,
	   fn ? IDENTIFIER_POINTER (DECL_NAME (fn->decl)) : "(nofn)");
}
