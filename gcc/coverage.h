/* coverage.h - Defines data exported from coverage.c
   Copyright (C) 1998, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_COVERAGE_H
#define GCC_COVERAGE_H

#include "gcov-io.h"

extern void coverage_init (const char *);
extern void coverage_finish (void);
extern void coverage_read_counts_file (void);

/* Complete the coverage information for the current function. Once
   per function.  */
extern void coverage_end_function (void);

/* Start outputting coverage information for the current
   function. Repeatable per function.  */
extern int coverage_begin_output (void);

/* Allocate some counters. Repeatable per function.  */
extern int coverage_counter_alloc (unsigned /*counter*/, unsigned/*num*/);
/* Use a counter from the most recent allocation.  */
extern rtx coverage_counter_ref (unsigned /*counter*/, unsigned/*num*/);

/* Get all the counters for the current function.  */
extern gcov_type *get_coverage_counts (unsigned /*counter*/,
				       unsigned /*expected*/,
				       const struct gcov_ctr_summary **);

#endif
