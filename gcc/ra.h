/* Define per-register tables for data flow info and register allocation.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

#ifndef GCC_RA_H
#define GCC_RA_H

#include "regs.h"

struct allocno
{
  int reg;
  /* Gives the number of consecutive hard registers needed by that
     pseudo reg.  */
  int size;

  /* Number of calls crossed by each allocno.  */
  int calls_crossed;

  /* Number of calls that might throw crossed by each allocno.  */
  int throwing_calls_crossed;

  /* Number of refs to each allocno.  */
  int n_refs;

  /* Frequency of uses of each allocno.  */
  int freq;

  /* Guess at live length of each allocno.
     This is actually the max of the live lengths of the regs.  */
  int live_length;

  /* Set of hard regs conflicting with allocno N.  */

  HARD_REG_SET hard_reg_conflicts;

  /* Set of hard regs preferred by allocno N.
     This is used to make allocnos go into regs that are copied to or from them,
     when possible, to reduce register shuffling.  */

  HARD_REG_SET hard_reg_preferences;

  /* Similar, but just counts register preferences made in simple copy
     operations, rather than arithmetic.  These are given priority because
     we can always eliminate an insn by using these, but using a register
     in the above list won't always eliminate an insn.  */

  HARD_REG_SET hard_reg_copy_preferences;

  /* Similar to hard_reg_preferences, but includes bits for subsequent
     registers when an allocno is multi-word.  The above variable is used for
     allocation while this is used to build reg_someone_prefers, below.  */

  HARD_REG_SET hard_reg_full_preferences;

  /* Set of hard registers that some later allocno has a preference for.  */

  HARD_REG_SET regs_someone_prefers;

#ifdef STACK_REGS
  /* Set to true if allocno can't be allocated in the stack register.  */
  bool no_stack_reg;
#endif
};
extern struct allocno *allocno;

/* In ra-conflict.c  */

/* Number of pseudo-registers which are candidates for allocation.  */

extern int max_allocno;

/* max_allocno by max_allocno array of bits, recording whether two
   allocno's conflict (can't go in the same hardware register).

   `conflicts' is symmetric after the call to mirror_conflicts.  */

extern HOST_WIDE_INT *conflicts;

/* Number of ints required to hold max_allocno bits.
   This is the length of a row in `conflicts'.  */

extern int allocno_row_words;

/* Indexed by (pseudo) reg number, gives the allocno, or -1
   for pseudo registers which are not to be allocated.  */

extern int *reg_allocno;

extern void global_conflicts (void);

/* In global.c  */

/* For any allocno set in ALLOCNO_SET, set ALLOCNO to that allocno,
   and execute CODE.  */
#define EXECUTE_IF_SET_IN_ALLOCNO_SET(ALLOCNO_SET, ALLOCNO, CODE)	\
do {									\
  int i_;								\
  int allocno_;								\
  HOST_WIDE_INT *p_ = (ALLOCNO_SET);					\
									\
  for (i_ = allocno_row_words - 1, allocno_ = 0; i_ >= 0;		\
       i_--, allocno_ += HOST_BITS_PER_WIDE_INT)			\
    {									\
      unsigned HOST_WIDE_INT word_ = (unsigned HOST_WIDE_INT) *p_++;	\
									\
      for ((ALLOCNO) = allocno_; word_; word_ >>= 1, (ALLOCNO)++)	\
	{								\
	  if (word_ & 1)						\
	    {CODE;}							\
	}								\
    }									\
} while (0)

extern void ra_init_live_subregs (bool, sbitmap *, int *, int, rtx reg);


#endif /* GCC_RA_H */
