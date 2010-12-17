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

enum alg_code {
  alg_unknown,
  alg_zero,
  alg_m, alg_shift,
  alg_add_t_m2,
  alg_sub_t_m2,
  alg_add_factor,
  alg_sub_factor,
  alg_add_t2_m,
  alg_sub_t2_m,
  alg_impossible
};

/* This structure holds the "cost" of a multiply sequence.  The
   "cost" field holds the total rtx_cost of every operator in the
   synthetic multiplication sequence, hence cost(a op b) is defined
   as rtx_cost(op) + cost(a) + cost(b), where cost(leaf) is zero.
   The "latency" field holds the minimum possible latency of the
   synthetic multiply, on a hypothetical infinitely parallel CPU.
   This is the critical path, or the maximum height, of the expression
   tree which is the sum of rtx_costs on the most expensive path from
   any leaf to the root.  Hence latency(a op b) is defined as zero for
   leaves and rtx_cost(op) + max(latency(a), latency(b)) otherwise.  */

struct mult_cost {
  short cost;     /* Total rtx_cost of the multiplication sequence.  */
  short latency;  /* The latency of the multiplication sequence.  */
};

/* This macro is used to compare a pointer to a mult_cost against an
   single integer "rtx_cost" value.  This is equivalent to the macro
   CHEAPER_MULT_COST(X,Z) where Z = {Y,Y}.  */
#define MULT_COST_LESS(X,Y) ((X)->cost < (Y)	\
			     || ((X)->cost == (Y) && (X)->latency < (Y)))

/* This macro is used to compare two pointers to mult_costs against
   each other.  The macro returns true if X is cheaper than Y.
   Currently, the cheaper of two mult_costs is the one with the
   lower "cost".  If "cost"s are tied, the lower latency is cheaper.  */
#define CHEAPER_MULT_COST(X,Y)  ((X)->cost < (Y)->cost		\
				 || ((X)->cost == (Y)->cost	\
				     && (X)->latency < (Y)->latency))

/* This structure records a sequence of operations.
   `ops' is the number of operations recorded.
   `cost' is their total cost.
   The operations are stored in `op' and the corresponding
   logarithms of the integer coefficients in `log'.

   These are the operations:
   alg_zero		total := 0;
   alg_m		total := multiplicand;
   alg_shift		total := total * coeff
   alg_add_t_m2		total := total + multiplicand * coeff;
   alg_sub_t_m2		total := total - multiplicand * coeff;
   alg_add_factor	total := total * coeff + total;
   alg_sub_factor	total := total * coeff - total;
   alg_add_t2_m		total := total * coeff + multiplicand;
   alg_sub_t2_m		total := total * coeff - multiplicand;

   The first operand must be either alg_zero or alg_m.  */

struct algorithm
{
  struct mult_cost cost;
  short ops;
  /* The size of the OP and LOG fields are not directly related to the
     word size, but the worst-case algorithms will be if we have few
     consecutive ones or zeros, i.e., a multiplicand like 10101010101...
     In that case we will generate shift-by-2, add, shift-by-2, add,...,
     in total wordsize operations.  */
  enum alg_code op[MAX_BITS_PER_WORD];
  char log[MAX_BITS_PER_WORD];
};

/* The entry for our multiplication cache/hash table.  */
struct alg_hash_entry {
  /* The number we are multiplying by.  */
  unsigned HOST_WIDE_INT t;

  /* The mode in which we are multiplying something by T.  */
  enum machine_mode mode;

  /* The best multiplication algorithm for t.  */
  enum alg_code alg;

  /* The cost of multiplication if ALG_CODE is not alg_impossible.
     Otherwise, the cost within which multiplication by T is
     impossible.  */
  struct mult_cost cost;

  /* Optimized for speed? */
  bool speed;
};

/* The number of cache/hash entries.  */
#if HOST_BITS_PER_WIDE_INT == 64
#define NUM_ALG_HASH_ENTRIES 1031
#else
#define NUM_ALG_HASH_ENTRIES 307
#endif

/* Target-dependent globals.  */
struct target_expmed {
  /* Each entry of ALG_HASH caches alg_code for some integer.  This is
     actually a hash table.  If we have a collision, that the older
     entry is kicked out.  */
  struct alg_hash_entry x_alg_hash[NUM_ALG_HASH_ENTRIES];

  /* True if x_alg_hash might already have been used.  */
  bool x_alg_hash_used_p;

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

#define alg_hash \
  (this_target_expmed->x_alg_hash)
#define alg_hash_used_p \
  (this_target_expmed->x_alg_hash_used_p)
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
