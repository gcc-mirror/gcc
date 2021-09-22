/* Definitions for RTX-operation-cost-related data structures.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

#ifndef LOONGARCH_RTX_COST_H
#define LOONGARCH_RTX_COST_H

/* Costs of various operations on the different architectures.  */
struct loongarch_rtx_cost_data
{
  unsigned short fp_add;
  unsigned short fp_mult_sf;
  unsigned short fp_mult_df;
  unsigned short fp_div_sf;
  unsigned short fp_div_df;
  unsigned short int_mult_si;
  unsigned short int_mult_di;
  unsigned short int_div_si;
  unsigned short int_div_di;
  unsigned short branch_cost;
  unsigned short memory_latency;
};

/* Definition stolen from rtl.h */
#define COSTS_N_INSNS(N) ((N) * 4)

/* Default costs.  If these are used for a processor we should look
   up the actual costs.  */
#define DEFAULT_COSTS COSTS_N_INSNS (6),  /* fp_add */       \
		      COSTS_N_INSNS (7),  /* fp_mult_sf */   \
		      COSTS_N_INSNS (8),  /* fp_mult_df */   \
		      COSTS_N_INSNS (23), /* fp_div_sf */    \
		      COSTS_N_INSNS (36), /* fp_div_df */    \
		      COSTS_N_INSNS (10), /* int_mult_si */  \
		      COSTS_N_INSNS (10), /* int_mult_di */  \
		      COSTS_N_INSNS (69), /* int_div_si */   \
		      COSTS_N_INSNS (69), /* int_div_di */   \
		      2, /* branch_cost */  \
		      4  /* memory_latency */

/* Floating-point costs for processors without an FPU.  Just assume that
   all floating-point libcalls are very expensive.  */
#define SOFT_FP_COSTS COSTS_N_INSNS (256), /* fp_add */       \
		      COSTS_N_INSNS (256), /* fp_mult_sf */   \
		      COSTS_N_INSNS (256), /* fp_mult_df */   \
		      COSTS_N_INSNS (256), /* fp_div_sf */    \
		      COSTS_N_INSNS (256)  /* fp_div_df */

/* Costs to use when optimizing for size.  */
static const struct loongarch_rtx_cost_data loongarch_rtx_cost_optimize_size = {
    COSTS_N_INSNS (1),		    /* fp_add */
    COSTS_N_INSNS (1),		    /* fp_mult_sf */
    COSTS_N_INSNS (1),		    /* fp_mult_df */
    COSTS_N_INSNS (1),		    /* fp_div_sf */
    COSTS_N_INSNS (1),		    /* fp_div_df */
    COSTS_N_INSNS (1),		    /* int_mult_si */
    COSTS_N_INSNS (1),		    /* int_mult_di */
    COSTS_N_INSNS (1),		    /* int_div_si */
    COSTS_N_INSNS (1),		    /* int_div_di */
    2,				    /* branch_cost */
    4				    /* memory_latency */
};

#endif /* LOONGARCH_RTX_COST_H */
