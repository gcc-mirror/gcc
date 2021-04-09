/* Scheduler hooks for IA-32 which implement bdver1-4 specific logic.
   Copyright (C) 1988-2021 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "tm_p.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "target.h"
#include "rtl-iter.h"
#include "regset.h"
#include "sched-int.h"


/* Model decoder of Core 2/i7.
   Below hooks for multipass scheduling (see haifa-sched.c:max_issue)
   track the instruction fetch block boundaries and make sure that long
   (9+ bytes) instructions are assigned to D0.  */

/* Maximum length of an insn that can be handled by
   a secondary decoder unit.  '8' for Core 2/i7.  */
static int core2i7_secondary_decoder_max_insn_size;

/* Ifetch block size, i.e., number of bytes decoder reads per cycle.
   '16' for Core 2/i7.  */
static int core2i7_ifetch_block_size;

/* Maximum number of instructions decoder can handle per cycle.
   '6' for Core 2/i7.  */
static int core2i7_ifetch_block_max_insns;

typedef struct ix86_first_cycle_multipass_data_ *
  ix86_first_cycle_multipass_data_t;
typedef const struct ix86_first_cycle_multipass_data_ *
  const_ix86_first_cycle_multipass_data_t;

/* A variable to store target state across calls to max_issue within
   one cycle.  */
static struct ix86_first_cycle_multipass_data_ _ix86_first_cycle_multipass_data,
  *ix86_first_cycle_multipass_data = &_ix86_first_cycle_multipass_data;

/* Initialize DATA.  */
static void
core2i7_first_cycle_multipass_init (void *_data)
{
  ix86_first_cycle_multipass_data_t data
    = (ix86_first_cycle_multipass_data_t) _data;

  data->ifetch_block_len = 0;
  data->ifetch_block_n_insns = 0;
  data->ready_try_change = NULL;
  data->ready_try_change_size = 0;
}

/* Advancing the cycle; reset ifetch block counts.  */
static void
core2i7_dfa_post_advance_cycle (void)
{
  ix86_first_cycle_multipass_data_t data = ix86_first_cycle_multipass_data;

  gcc_assert (data->ifetch_block_n_insns <= core2i7_ifetch_block_max_insns);

  data->ifetch_block_len = 0;
  data->ifetch_block_n_insns = 0;
}

/* Filter out insns from ready_try that the core will not be able to issue
   on current cycle due to decoder.  */
static void
core2i7_first_cycle_multipass_filter_ready_try
(const_ix86_first_cycle_multipass_data_t data,
 signed char *ready_try, int n_ready, bool first_cycle_insn_p)
{
  while (n_ready--)
    {
      rtx_insn *insn;
      int insn_size;

      if (ready_try[n_ready])
	continue;

      insn = get_ready_element (n_ready);
      insn_size = ix86_min_insn_size (insn);

      if (/* If this is a too long an insn for a secondary decoder ...  */
	  (!first_cycle_insn_p
	   && insn_size > core2i7_secondary_decoder_max_insn_size)
	  /* ... or it would not fit into the ifetch block ...  */
	  || data->ifetch_block_len + insn_size > core2i7_ifetch_block_size
	  /* ... or the decoder is full already ...  */
	  || data->ifetch_block_n_insns + 1 > core2i7_ifetch_block_max_insns)
	/* ... mask the insn out.  */
	{
	  ready_try[n_ready] = 1;

	  if (data->ready_try_change)
	    bitmap_set_bit (data->ready_try_change, n_ready);
	}
    }
}

/* Prepare for a new round of multipass lookahead scheduling.  */
static void
core2i7_first_cycle_multipass_begin (void *_data,
				     signed char *ready_try, int n_ready,
				     bool first_cycle_insn_p)
{
  ix86_first_cycle_multipass_data_t data
    = (ix86_first_cycle_multipass_data_t) _data;
  const_ix86_first_cycle_multipass_data_t prev_data
    = ix86_first_cycle_multipass_data;

  /* Restore the state from the end of the previous round.  */
  data->ifetch_block_len = prev_data->ifetch_block_len;
  data->ifetch_block_n_insns = prev_data->ifetch_block_n_insns;

  /* Filter instructions that cannot be issued on current cycle due to
     decoder restrictions.  */
  core2i7_first_cycle_multipass_filter_ready_try (data, ready_try, n_ready,
						  first_cycle_insn_p);
}

/* INSN is being issued in current solution.  Account for its impact on
   the decoder model.  */
static void
core2i7_first_cycle_multipass_issue (void *_data,
				     signed char *ready_try, int n_ready,
				     rtx_insn *insn, const void *_prev_data)
{
  ix86_first_cycle_multipass_data_t data
    = (ix86_first_cycle_multipass_data_t) _data;
  const_ix86_first_cycle_multipass_data_t prev_data
    = (const_ix86_first_cycle_multipass_data_t) _prev_data;

  int insn_size = ix86_min_insn_size (insn);

  data->ifetch_block_len = prev_data->ifetch_block_len + insn_size;
  data->ifetch_block_n_insns = prev_data->ifetch_block_n_insns + 1;
  gcc_assert (data->ifetch_block_len <= core2i7_ifetch_block_size
	      && data->ifetch_block_n_insns <= core2i7_ifetch_block_max_insns);

  /* Allocate or resize the bitmap for storing INSN's effect on ready_try.  */
  if (!data->ready_try_change)
    {
      data->ready_try_change = sbitmap_alloc (n_ready);
      data->ready_try_change_size = n_ready;
    }
  else if (data->ready_try_change_size < n_ready)
    {
      data->ready_try_change = sbitmap_resize (data->ready_try_change,
					       n_ready, 0);
      data->ready_try_change_size = n_ready;
    }
  bitmap_clear (data->ready_try_change);

  /* Filter out insns from ready_try that the core will not be able to issue
     on current cycle due to decoder.  */
  core2i7_first_cycle_multipass_filter_ready_try (data, ready_try, n_ready,
						  false);
}

/* Revert the effect on ready_try.  */
static void
core2i7_first_cycle_multipass_backtrack (const void *_data,
					 signed char *ready_try,
					 int n_ready ATTRIBUTE_UNUSED)
{
  const_ix86_first_cycle_multipass_data_t data
    = (const_ix86_first_cycle_multipass_data_t) _data;
  unsigned int i = 0;
  sbitmap_iterator sbi;

  gcc_assert (bitmap_last_set_bit (data->ready_try_change) < n_ready);
  EXECUTE_IF_SET_IN_BITMAP (data->ready_try_change, 0, i, sbi)
    {
      ready_try[i] = 0;
    }
}

/* Save the result of multipass lookahead scheduling for the next round.  */
static void
core2i7_first_cycle_multipass_end (const void *_data)
{
  const_ix86_first_cycle_multipass_data_t data
    = (const_ix86_first_cycle_multipass_data_t) _data;
  ix86_first_cycle_multipass_data_t next_data
    = ix86_first_cycle_multipass_data;

  if (data != NULL)
    {
      next_data->ifetch_block_len = data->ifetch_block_len;
      next_data->ifetch_block_n_insns = data->ifetch_block_n_insns;
    }
}

/* Deallocate target data.  */
static void
core2i7_first_cycle_multipass_fini (void *_data)
{
  ix86_first_cycle_multipass_data_t data
    = (ix86_first_cycle_multipass_data_t) _data;

  if (data->ready_try_change)
    {
      sbitmap_free (data->ready_try_change);
      data->ready_try_change = NULL;
      data->ready_try_change_size = 0;
    }
}

void
ix86_core2i7_init_hooks (void)
{
  targetm.sched.dfa_post_advance_cycle
    = core2i7_dfa_post_advance_cycle;
  targetm.sched.first_cycle_multipass_init
    = core2i7_first_cycle_multipass_init;
  targetm.sched.first_cycle_multipass_begin
    = core2i7_first_cycle_multipass_begin;
  targetm.sched.first_cycle_multipass_issue
    = core2i7_first_cycle_multipass_issue;
  targetm.sched.first_cycle_multipass_backtrack
    = core2i7_first_cycle_multipass_backtrack;
  targetm.sched.first_cycle_multipass_end
    = core2i7_first_cycle_multipass_end;
  targetm.sched.first_cycle_multipass_fini
    = core2i7_first_cycle_multipass_fini;

  /* Set decoder parameters.  */
  core2i7_secondary_decoder_max_insn_size = 8;
  core2i7_ifetch_block_size = 16;
  core2i7_ifetch_block_max_insns = 6;
}
