/* Instruction scheduling pass.  This file contains definitions used
   internally in the scheduler.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to the Free
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Forward declaration.  */
struct ready_list;

/* This structure holds some state of the current scheduling pass, and
   contains some function pointers that abstract out some of the non-generic
   functionality from functions such as schedule_block or schedule_insn.
   There is one global variable, current_sched_info, which points to the
   sched_info structure currently in use.  */
struct sched_info
{
  /* Add all insns that are initially ready to the ready list.  Called once
     before scheduling a set of insns.  */
  void (*init_ready_list) PARAMS ((struct ready_list *));
  /* Called after taking an insn from the ready list.  Returns nonzero if
     this insn can be scheduled, nonzero if we should silently discard it.  */
  int (*can_schedule_ready_p) PARAMS ((rtx));
  /* Return nonzero if there are more insns that should be scheduled.  */
  int (*schedule_more_p) PARAMS ((void));
  /* Called after an insn has all its dependencies resolved.  Return nonzero
     if it should be moved to the ready list or the queue, or zero if we
     should silently discard it.  */
  int (*new_ready) PARAMS ((rtx));
  /* Compare priority of two insns.  Return a positive number if the second
     insn is to be preferred for scheduling, and a negative one if the first
     is to be preferred.  Zero if they are equally good.  */
  int (*rank) PARAMS ((rtx, rtx));
  /* Return a string that contains the insn uid and optionally anything else
     necessary to identify this insn in an output.  It's valid to use a
     static buffer for this.  The ALIGNED parameter should cause the string
     to be formatted so that multiple output lines will line up nicely.  */
  const char *(*print_insn) PARAMS ((rtx, int));

  /* The boundaries of the set of insns to be scheduled.  */
  rtx prev_head, next_tail;

  /* Filled in after the schedule is finished; the first and last scheduled
     insns.  */
  rtx head, tail;

  /* If nonzero, enables an additional sanity check in schedule_block.  */
  int queue_must_finish_empty;
};

#ifndef __GNUC__
#define __inline
#endif

#ifndef HAIFA_INLINE
#define HAIFA_INLINE __inline
#endif
