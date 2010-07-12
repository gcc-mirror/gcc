/* Target-dependent costs for expmed.c.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option; any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef EXPMED_H
#define EXPMED_H 1

/* Target-dependent globals.  */
struct target_expmed {
  /* Nonzero means divides or modulus operations are relatively cheap for
     powers of two, so don't use branches; emit the operation instead.
     Usually, this will mean that the MD file will emit non-branch
     sequences.  */
  bool x_sdiv_pow2_cheap[2][NUM_MACHINE_MODES];
  bool x_smod_pow2_cheap[2][NUM_MACHINE_MODES];

  /* Cost of various pieces of RTL.  Note that some of these are indexed by
     shift count and some by mode.  */
  int x_zero_cost[2];
  int x_add_cost[2][NUM_MACHINE_MODES];
  int x_neg_cost[2][NUM_MACHINE_MODES];
  int x_shift_cost[2][NUM_MACHINE_MODES][MAX_BITS_PER_WORD];
  int x_shiftadd_cost[2][NUM_MACHINE_MODES][MAX_BITS_PER_WORD];
  int x_shiftsub0_cost[2][NUM_MACHINE_MODES][MAX_BITS_PER_WORD];
  int x_shiftsub1_cost[2][NUM_MACHINE_MODES][MAX_BITS_PER_WORD];
  int x_mul_cost[2][NUM_MACHINE_MODES];
  int x_sdiv_cost[2][NUM_MACHINE_MODES];
  int x_udiv_cost[2][NUM_MACHINE_MODES];
  int x_mul_widen_cost[2][NUM_MACHINE_MODES];
  int x_mul_highpart_cost[2][NUM_MACHINE_MODES];
};

extern struct target_expmed default_target_expmed;
#if SWITCHABLE_TARGET
extern struct target_expmed *this_target_expmed;
#else
#define this_target_expmed (&default_target_expmed)
#endif

#define sdiv_pow2_cheap \
  (this_target_expmed->x_sdiv_pow2_cheap)
#define smod_pow2_cheap \
  (this_target_expmed->x_smod_pow2_cheap)
#define zero_cost \
  (this_target_expmed->x_zero_cost)
#define add_cost \
  (this_target_expmed->x_add_cost)
#define neg_cost \
  (this_target_expmed->x_neg_cost)
#define shift_cost \
  (this_target_expmed->x_shift_cost)
#define shiftadd_cost \
  (this_target_expmed->x_shiftadd_cost)
#define shiftsub0_cost \
  (this_target_expmed->x_shiftsub0_cost)
#define shiftsub1_cost \
  (this_target_expmed->x_shiftsub1_cost)
#define mul_cost \
  (this_target_expmed->x_mul_cost)
#define sdiv_cost \
  (this_target_expmed->x_sdiv_cost)
#define udiv_cost \
  (this_target_expmed->x_udiv_cost)
#define mul_widen_cost \
  (this_target_expmed->x_mul_widen_cost)
#define mul_highpart_cost \
  (this_target_expmed->x_mul_highpart_cost)

#endif
