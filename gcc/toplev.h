/* toplev.h - Various declarations for functions found in toplev.c
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2006, 2007,
   2008, 2009, 2010
   Free Software Foundation, Inc.

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

#ifndef GCC_TOPLEV_H
#define GCC_TOPLEV_H
#include "input.h"
#include "bversion.h"

/* If non-NULL, return one past-the-end of the matching SUBPART of
   the WHOLE string.  */
#define skip_leading_substring(whole,  part) \
   (strncmp (whole, part, strlen (part)) ? NULL : whole + strlen (part))

/* Decoded options, and number of such options.  */
extern struct cl_decoded_option *save_decoded_options;
extern unsigned int save_decoded_options_count;

extern int toplev_main (int, char **);
extern void strip_off_ending (char *, int);
extern void rest_of_decl_compilation (tree, int, int);
extern void rest_of_type_compilation (tree, int);
extern void tree_rest_of_compilation (tree);
extern void init_optimization_passes (void);
extern void finish_optimization_passes (void);
extern bool enable_rtl_dump_file (void);

/* In except.c.  Initialize exception handling.  This is used by the Ada
   and LTO front ends to initialize EH "on demand".  See lto-streamer-in.c
   and ada/gcc-interface/misc.c.  */
extern void init_eh (void);

extern void announce_function (tree);

extern void warn_deprecated_use (tree, tree);
extern bool parse_optimize_options (tree, bool);

#ifdef BUFSIZ
extern void output_quoted_string	(FILE *, const char *);
#endif

extern void wrapup_global_declaration_1 (tree);
extern bool wrapup_global_declaration_2 (tree);
extern bool wrapup_global_declarations (tree *, int);
extern void check_global_declaration_1 (tree);
extern void check_global_declarations (tree *, int);
extern void emit_debug_global_declarations (tree *, int);
extern void write_global_declarations (void);

extern void dump_memory_report (bool);

extern void target_reinit (void);

/* A unique local time stamp, might be zero if none is available.  */
extern unsigned local_tick;

/* Top-level source file.  */
extern const char *main_input_filename;
extern const char *main_input_basename;
extern int main_input_baselength;

extern const char *profile_data_prefix;
extern bool exit_after_options;

/* True if the user has tagged the function with the 'section'
   attribute.  */

extern bool user_defined_section_attribute;

/* See toplev.c.  */
extern int flag_rerun_cse_after_global_opts;

/* Things to do with target switches.  */
extern void print_version (FILE *, const char *);
extern void * default_get_pch_validity (size_t *);
extern const char * default_pch_valid_p (const void *, size_t);

/* The hashtable, so that the C front ends can pass it to cpplib.  */
extern struct ht *ident_hash;

/* Handle -d switch.  */
extern void decode_d_option		(const char *);

/* Return true iff flags are set as if -ffast-math.  */
extern bool fast_math_flags_set_p	(void);
extern bool fast_math_flags_struct_set_p (struct cl_optimization *);

/* Inline versions of the above for speed.  */
#if GCC_VERSION < 3004

extern int clz_hwi (unsigned HOST_WIDE_INT x);
extern int ctz_hwi (unsigned HOST_WIDE_INT x);
extern int ffs_hwi (unsigned HOST_WIDE_INT x);

/* Return log2, or -1 if not exact.  */
extern int exact_log2                  (unsigned HOST_WIDE_INT);

/* Return floor of log2, with -1 for zero.  */
extern int floor_log2                  (unsigned HOST_WIDE_INT);

#else /* GCC_VERSION >= 3004 */

/* For convenience, define 0 -> word_size.  */
static inline int
clz_hwi (unsigned HOST_WIDE_INT x)
{
  if (x == 0)
    return HOST_BITS_PER_WIDE_INT;
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
  return __builtin_clzl (x);
# elif HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONGLONG
  return __builtin_clzll (x);
# else
  return __builtin_clz (x);
# endif
}

static inline int
ctz_hwi (unsigned HOST_WIDE_INT x)
{
  if (x == 0)
    return HOST_BITS_PER_WIDE_INT;
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
  return __builtin_ctzl (x);
# elif HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONGLONG
  return __builtin_ctzll (x);
# else
  return __builtin_ctz (x);
# endif
}

static inline int
ffs_hwi (unsigned HOST_WIDE_INT x)
{
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
  return __builtin_ffsl (x);
# elif HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONGLONG
  return __builtin_ffsll (x);
# else
  return __builtin_ffs (x);
# endif
}

static inline int
floor_log2 (unsigned HOST_WIDE_INT x)
{
  return HOST_BITS_PER_WIDE_INT - 1 - clz_hwi (x);
}

static inline int
exact_log2 (unsigned HOST_WIDE_INT x)
{
  return x == (x & -x) && x ? ctz_hwi (x) : -1;
}

#endif /* GCC_VERSION >= 3004 */

/* Functions used to get and set GCC's notion of in what directory
   compilation was started.  */

extern const char *get_src_pwd	       (void);
extern bool set_src_pwd		       (const char *);

/* Functions used to manipulate the random seed.  */

extern const char *get_random_seed (bool);
extern const char *set_random_seed (const char *);

#endif /* ! GCC_TOPLEV_H */
