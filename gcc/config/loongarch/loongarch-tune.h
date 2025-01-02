/* Definitions for microarchitecture-related data structures.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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

#ifndef LOONGARCH_TUNE_H
#define LOONGARCH_TUNE_H

#include "loongarch-def-array.h"

/* RTX costs of various operations on the different architectures.  */
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
  unsigned short movcf2gr;
  unsigned short movgr2cf;
  unsigned short branch_cost;
  unsigned short memory_latency;

  /* Default RTX cost initializer, implemented in loongarch-def.cc.  */
  loongarch_rtx_cost_data ();

  loongarch_rtx_cost_data fp_add_ (unsigned short _fp_add)
  {
    fp_add = _fp_add;
    return *this;
  }

  loongarch_rtx_cost_data fp_mult_sf_ (unsigned short _fp_mult_sf)
  {
    fp_mult_sf = _fp_mult_sf;
    return *this;
  }

  loongarch_rtx_cost_data fp_mult_df_ (unsigned short _fp_mult_df)
  {
    fp_mult_df = _fp_mult_df;
    return *this;
  }

  loongarch_rtx_cost_data fp_div_sf_ (unsigned short _fp_div_sf)
  {
    fp_div_sf = _fp_div_sf;
    return *this;
  }

  loongarch_rtx_cost_data fp_div_df_ (unsigned short _fp_div_df)
  {
    fp_div_df = _fp_div_df;
    return *this;
  }

  loongarch_rtx_cost_data int_mult_si_ (unsigned short _int_mult_si)
  {
    int_mult_si = _int_mult_si;
    return *this;
  }

  loongarch_rtx_cost_data int_mult_di_ (unsigned short _int_mult_di)
  {
    int_mult_di = _int_mult_di;
    return *this;
  }

  loongarch_rtx_cost_data int_div_si_ (unsigned short _int_div_si)
  {
    int_div_si = _int_div_si;
    return *this;
  }

  loongarch_rtx_cost_data int_div_di_ (unsigned short _int_div_di)
  {
    int_div_di = _int_div_di;
    return *this;
  }

  loongarch_rtx_cost_data movcf2gr_ (unsigned short _movcf2gr)
  {
    movcf2gr = _movcf2gr;
    return *this;
  }

  loongarch_rtx_cost_data movgr2cf_ (unsigned short _movgr2cf)
  {
    movgr2cf = _movgr2cf;
    return *this;
  }

  loongarch_rtx_cost_data branch_cost_ (unsigned short _branch_cost)
  {
    branch_cost = _branch_cost;
    return *this;
  }

  loongarch_rtx_cost_data memory_latency_ (unsigned short _memory_latency)
  {
    memory_latency = _memory_latency;
    return *this;
  }
};

/* Costs to use when optimizing for size.  */
extern const struct loongarch_rtx_cost_data loongarch_rtx_cost_optimize_size;

/* Cache size record of known processor models.  */
struct loongarch_cache {
  int l1d_line_size;  /* bytes */
  int l1d_size;       /* KiB */
  int l2d_size;       /* kiB */
  int simultaneous_prefetches; /* number of parallel prefetch */

  loongarch_cache () : l1d_line_size (0),
		       l1d_size (0),
		       l2d_size (0),
		       simultaneous_prefetches (0) {}

  loongarch_cache l1d_line_size_ (int _l1d_line_size)
  {
    l1d_line_size = _l1d_line_size;
    return *this;
  }

  loongarch_cache l1d_size_ (int _l1d_size)
  {
    l1d_size = _l1d_size;
    return *this;
  }

  loongarch_cache l2d_size_ (int _l2d_size)
  {
    l2d_size = _l2d_size;
    return *this;
  }

  loongarch_cache simultaneous_prefetches_ (int _simultaneous_prefetches)
  {
    simultaneous_prefetches = _simultaneous_prefetches;
    return *this;
  }
};

/* Alignment for functions loops and jumps for best performance.  For new
   uarchs the value should be measured via benchmarking.  See the
   documentation for -falign-functions, -falign-loops, and -falign-jumps in
   invoke.texi for the format.  */
struct loongarch_align {
  const char *function;	/* default value for -falign-functions */
  const char *loop;	/* default value for -falign-loops */
  const char *jump;	/* default value for -falign-jumps */

  loongarch_align () : function (nullptr), loop (nullptr), jump (nullptr) {}

  loongarch_align function_ (const char *_function)
  {
    function = _function;
    return *this;
  }

  loongarch_align loop_ (const char *_loop)
  {
    loop = _loop;
    return *this;
  }

  loongarch_align jump_ (const char *_jump)
  {
    jump = _jump;
    return *this;
  }
};

#endif /* LOONGARCH_TUNE_H */
