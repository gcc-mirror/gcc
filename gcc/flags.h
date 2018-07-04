/* Compilation switch flag definitions for GCC.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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

#ifndef GCC_FLAGS_H
#define GCC_FLAGS_H

#if !defined(IN_LIBGCC2) && !defined(IN_TARGET_LIBS) && !defined(IN_RTS)

/* Names of debug_info_type, for error messages.  */
extern const char *const debug_type_names[];

extern void strip_off_ending (char *, int);
extern int base_of_path (const char *path, const char **base_out);

/* Return true iff flags are set as if -ffast-math.  */
extern bool fast_math_flags_set_p (const struct gcc_options *);
extern bool fast_math_flags_struct_set_p (struct cl_optimization *);


/* Now the symbols that are set with `-f' switches.  */

/* True if printing into -fdump-final-insns= dump.  */

extern bool final_insns_dump_p;


/* Other basic status info about current function.  */

/* Align flags tuple with alignment in log form and with a maximum skip.  */

struct align_flags_tuple
{
  /* Values of the -falign-* flags: how much to align labels in code.
     log is "align to 2^log" (so 0 means no alignment).
     maxskip is the maximum allowed amount of padding to insert.  */
  int log;
  int maxskip;
};

/* Target-dependent global state.  */

struct align_flags
{
  align_flags_tuple levels[2];
};

struct target_flag_state
{
  /* Each falign-foo can generate up to two levels of alignment:
     -falign-foo=N:M[:N2:M2] */
  align_flags x_align_loops;
  align_flags x_align_jumps;
  align_flags x_align_labels;
  align_flags x_align_functions;

  /* The excess precision currently in effect.  */
  enum excess_precision x_flag_excess_precision;
};

extern struct target_flag_state default_target_flag_state;
#if SWITCHABLE_TARGET
extern struct target_flag_state *this_target_flag_state;
#else
#define this_target_flag_state (&default_target_flag_state)
#endif

#define state_align_loops	 (this_target_flag_state->x_align_loops)
#define state_align_jumps	 (this_target_flag_state->x_align_jumps)
#define state_align_labels	 (this_target_flag_state->x_align_labels)
#define state_align_functions	 (this_target_flag_state->x_align_functions)
#define align_loops_log		 (state_align_loops.levels[0].log)
#define align_jumps_log		 (state_align_jumps.levels[0].log)
#define align_labels_log	 (state_align_labels.levels[0].log)
#define align_functions_log      (state_align_functions.levels[0].log)
#define align_loops_max_skip     (state_align_loops.levels[0].maxskip)
#define align_jumps_max_skip     (state_align_jumps.levels[0].maxskip)
#define align_labels_max_skip    (state_align_labels.levels[0].maxskip)
#define align_functions_max_skip (state_align_functions.levels[0].maxskip)
#define align_loops_value	 (align_loops_max_skip + 1)
#define align_jumps_value	 (align_jumps_max_skip + 1)
#define align_labels_value	 (align_labels_max_skip + 1)
#define align_functions_value	 (align_functions_max_skip + 1)

/* String representaions of the above options are available in
   const char *str_align_foo.  NULL if not set.  */

#define flag_excess_precision \
  (this_target_flag_state->x_flag_excess_precision)

/* Returns TRUE if generated code should match ABI version N or
   greater is in use.  */

#define abi_version_at_least(N) \
  (flag_abi_version == 0 || flag_abi_version >= (N))

/* Whether to emit an overflow warning whose code is C.  */
#define issue_strict_overflow_warning(c) (warn_strict_overflow >= (int) (c))

#endif

#endif /* ! GCC_FLAGS_H */
