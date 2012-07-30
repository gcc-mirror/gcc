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

#define NUM_MODE_INT (MAX_MODE_INT - MIN_MODE_INT + 1)

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

  /* Conversion costs are only defined between two scalar integer modes
     of different sizes.  The first machine mode is the destination mode,
     and the second is the source mode.  */
  int x_convert_cost[2][NUM_MODE_INT][NUM_MODE_INT];
};

extern struct target_expmed default_target_expmed;
#if SWITCHABLE_TARGET
extern struct target_expmed *this_target_expmed;
#else
#define this_target_expmed (&default_target_expmed)
#endif

/* Return a pointer to the alg_hash_entry at IDX.  */

static inline struct alg_hash_entry *
alg_hash_entry_ptr (int idx)
{
  return &this_target_expmed->x_alg_hash[idx];
}

/* Return true if the x_alg_hash field might have been used.  */

static inline bool
alg_hash_used_p (void)
{
  return this_target_expmed->x_alg_hash_used_p;
}

/* Set whether the x_alg_hash field might have been used.  */

static inline void
set_alg_hash_used_p (bool usedp)
{
  this_target_expmed->x_alg_hash_used_p = usedp;
}

/* Subroutine of {set_,}sdiv_pow2_cheap.  Not to be used otherwise.  */

static inline bool *
sdiv_pow2_cheap_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_sdiv_pow2_cheap[speed][mode];
}

/* Set whether a signed division by a power of 2 is cheap in MODE
   when optimizing for SPEED.  */

static inline void
set_sdiv_pow2_cheap (bool speed, enum machine_mode mode, bool cheap_p)
{
  *sdiv_pow2_cheap_ptr (speed, mode) = cheap_p;
}

/* Return whether a signed division by a power of 2 is cheap in MODE
   when optimizing for SPEED.  */

static inline bool
sdiv_pow2_cheap (bool speed, enum machine_mode mode)
{
  return *sdiv_pow2_cheap_ptr (speed, mode);
}

/* Subroutine of {set_,}smod_pow2_cheap.  Not to be used otherwise.  */

static inline bool *
smod_pow2_cheap_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_smod_pow2_cheap[speed][mode];
}

/* Set whether a signed modulo by a power of 2 is CHEAP in MODE when
   optimizing for SPEED.  */

static inline void
set_smod_pow2_cheap (bool speed, enum machine_mode mode, bool cheap)
{
  *smod_pow2_cheap_ptr (speed, mode) = cheap;
}

/* Return whether a signed modulo by a power of 2 is cheap in MODE
   when optimizing for SPEED.  */

static inline bool
smod_pow2_cheap (bool speed, enum machine_mode mode)
{
  return *smod_pow2_cheap_ptr (speed, mode);
}

/* Subroutine of {set_,}zero_cost.  Not to be used otherwise.  */

static inline int *
zero_cost_ptr (bool speed)
{
  return &this_target_expmed->x_zero_cost[speed];
}

/* Set the COST of loading zero when optimizing for SPEED.  */

static inline void
set_zero_cost (bool speed, int cost)
{
  *zero_cost_ptr (speed) = cost;
}

/* Return the COST of loading zero when optimizing for SPEED.  */

static inline int
zero_cost (bool speed)
{
  return *zero_cost_ptr (speed);
}

/* Subroutine of {set_,}add_cost.  Not to be used otherwise.  */

static inline int *
add_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_add_cost[speed][mode];
}

/* Set the COST of computing an add in MODE when optimizing for SPEED.  */

static inline void
set_add_cost (bool speed, enum machine_mode mode, int cost)
{
  *add_cost_ptr (speed, mode) = cost;
}

/* Return the cost of computing an add in MODE when optimizing for SPEED.  */

static inline int
add_cost (bool speed, enum machine_mode mode)
{
  return *add_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}neg_cost.  Not to be used otherwise.  */

static inline int *
neg_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_neg_cost[speed][mode];
}

/* Set the COST of computing a negation in MODE when optimizing for SPEED.  */

static inline void
set_neg_cost (bool speed, enum machine_mode mode, int cost)
{
  *neg_cost_ptr (speed, mode) = cost;
}

/* Return the cost of computing a negation in MODE when optimizing for
   SPEED.  */

static inline int
neg_cost (bool speed, enum machine_mode mode)
{
  return *neg_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}shift_cost.  Not to be used otherwise.  */

static inline int *
shift_cost_ptr (bool speed, enum machine_mode mode, int bits)
{
  return &this_target_expmed->x_shift_cost[speed][mode][bits];
}

/* Set the COST of doing a shift in MODE by BITS when optimizing for SPEED.  */

static inline void
set_shift_cost (bool speed, enum machine_mode mode, int bits, int cost)
{
  *shift_cost_ptr (speed, mode, bits) = cost;
}

/* Return the cost of doing a shift in MODE by BITS when optimizing for
   SPEED.  */

static inline int
shift_cost (bool speed, enum machine_mode mode, int bits)
{
  return *shift_cost_ptr (speed, mode, bits);
}

/* Subroutine of {set_,}shiftadd_cost.  Not to be used otherwise.  */

static inline int *
shiftadd_cost_ptr (bool speed, enum machine_mode mode, int bits)
{
  return &this_target_expmed->x_shiftadd_cost[speed][mode][bits];
}

/* Set the COST of doing a shift in MODE by BITS followed by an add when
   optimizing for SPEED.  */

static inline void
set_shiftadd_cost (bool speed, enum machine_mode mode, int bits, int cost)
{
  *shiftadd_cost_ptr (speed, mode, bits) = cost;
}

/* Return the cost of doing a shift in MODE by BITS followed by an add
   when optimizing for SPEED.  */

static inline int
shiftadd_cost (bool speed, enum machine_mode mode, int bits)
{
  return *shiftadd_cost_ptr (speed, mode, bits);
}

/* Subroutine of {set_,}shiftsub0_cost.  Not to be used otherwise.  */

static inline int *
shiftsub0_cost_ptr (bool speed, enum machine_mode mode, int bits)
{
  return &this_target_expmed->x_shiftsub0_cost[speed][mode][bits];
}

/* Set the COST of doing a shift in MODE by BITS and then subtracting a
   value when optimizing for SPEED.  */

static inline void
set_shiftsub0_cost (bool speed, enum machine_mode mode, int bits, int cost)
{
  *shiftsub0_cost_ptr (speed, mode, bits) = cost;
}

/* Return the cost of doing a shift in MODE by BITS and then subtracting
   a value when optimizing for SPEED.  */

static inline int
shiftsub0_cost (bool speed, enum machine_mode mode, int bits)
{
  return *shiftsub0_cost_ptr (speed, mode, bits);
}

/* Subroutine of {set_,}shiftsub1_cost.  Not to be used otherwise.  */

static inline int *
shiftsub1_cost_ptr (bool speed, enum machine_mode mode, int bits)
{
  return &this_target_expmed->x_shiftsub1_cost[speed][mode][bits];
}

/* Set the COST of subtracting a shift in MODE by BITS from a value when
   optimizing for SPEED.  */

static inline void
set_shiftsub1_cost (bool speed, enum machine_mode mode, int bits, int cost)
{
  *shiftsub1_cost_ptr (speed, mode, bits) = cost;
}

/* Return the cost of subtracting a shift in MODE by BITS from a value
   when optimizing for SPEED.  */

static inline int
shiftsub1_cost (bool speed, enum machine_mode mode, int bits)
{
  return *shiftsub1_cost_ptr (speed, mode, bits);
}

/* Subroutine of {set_,}mul_cost.  Not to be used otherwise.  */

static inline int *
mul_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_mul_cost[speed][mode];
}

/* Set the COST of doing a multiplication in MODE when optimizing for
   SPEED.  */

static inline void
set_mul_cost (bool speed, enum machine_mode mode, int cost)
{
  *mul_cost_ptr (speed, mode) = cost;
}

/* Return the cost of doing a multiplication in MODE when optimizing
   for SPEED.  */

static inline int
mul_cost (bool speed, enum machine_mode mode)
{
  return *mul_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}sdiv_cost.  Not to be used otherwise.  */

static inline int *
sdiv_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_sdiv_cost[speed][mode];
}

/* Set the COST of doing a signed division in MODE when optimizing
   for SPEED.  */

static inline void
set_sdiv_cost (bool speed, enum machine_mode mode, int cost)
{
  *sdiv_cost_ptr (speed, mode) = cost;
}

/* Return the cost of doing a signed division in MODE when optimizing
   for SPEED.  */

static inline int
sdiv_cost (bool speed, enum machine_mode mode)
{
  return *sdiv_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}udiv_cost.  Not to be used otherwise.  */

static inline int *
udiv_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_udiv_cost[speed][mode];
}

/* Set the COST of doing an unsigned division in MODE when optimizing
   for SPEED.  */

static inline void
set_udiv_cost (bool speed, enum machine_mode mode, int cost)
{
  *udiv_cost_ptr (speed, mode) = cost;
}

/* Return the cost of doing an unsigned division in MODE when
   optimizing for SPEED.  */

static inline int
udiv_cost (bool speed, enum machine_mode mode)
{
  return *udiv_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}mul_widen_cost.  Not to be used otherwise.  */

static inline int *
mul_widen_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_mul_widen_cost[speed][mode];
}

/* Set the COST for computing a widening multiplication in MODE when
   optimizing for SPEED.  */

static inline void
set_mul_widen_cost (bool speed, enum machine_mode mode, int cost)
{
  *mul_widen_cost_ptr (speed, mode) = cost;
}

/* Return the cost for computing a widening multiplication in MODE when
   optimizing for SPEED.  */

static inline int
mul_widen_cost (bool speed, enum machine_mode mode)
{
  return *mul_widen_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}mul_highpart_cost.  Not to be used otherwise.  */

static inline int *
mul_highpart_cost_ptr (bool speed, enum machine_mode mode)
{
  return &this_target_expmed->x_mul_highpart_cost[speed][mode];
}

/* Set the COST for computing the high part of a multiplication in MODE
   when optimizing for SPEED.  */

static inline void
set_mul_highpart_cost (bool speed, enum machine_mode mode, int cost)
{
  *mul_highpart_cost_ptr (speed, mode) = cost;
}

/* Return the cost for computing the high part of a multiplication in MODE
   when optimizing for SPEED.  */

static inline int
mul_highpart_cost (bool speed, enum machine_mode mode)
{
  return *mul_highpart_cost_ptr (speed, mode);
}

/* Subroutine of {set_,}convert_cost.  Not to be used otherwise.  */

static inline int *
convert_cost_ptr (enum machine_mode to_mode, enum machine_mode from_mode,
		  bool speed)
{
  int to_idx, from_idx;

  gcc_assert (to_mode >= MIN_MODE_INT
	      && to_mode <= MAX_MODE_INT
	      && from_mode >= MIN_MODE_INT
	      && from_mode <= MAX_MODE_INT);

  to_idx = to_mode - MIN_MODE_INT;
  from_idx = from_mode - MIN_MODE_INT;
  return &this_target_expmed->x_convert_cost[speed][to_idx][from_idx];
}

/* Set the COST for converting from FROM_MODE to TO_MODE when optimizing
   for SPEED.  */

static inline void
set_convert_cost (enum machine_mode to_mode, enum machine_mode from_mode,
		  bool speed, int cost)
{
  *convert_cost_ptr (to_mode, from_mode, speed) = cost;
}

/* Return the cost for converting from FROM_MODE to TO_MODE when optimizing
   for SPEED.  */

static inline int
convert_cost (enum machine_mode to_mode, enum machine_mode from_mode,
	      bool speed)
{
  return *convert_cost_ptr (to_mode, from_mode, speed);
}

extern int mult_by_coeff_cost (HOST_WIDE_INT, enum machine_mode, bool);
#endif
