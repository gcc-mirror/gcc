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
#include "diagnostic-core.h"

/* If non-NULL, return one past-the-end of the matching SUBPART of
   the WHOLE string.  */
#define skip_leading_substring(whole,  part) \
   (strncmp (whole, part, strlen (part)) ? NULL : whole + strlen (part))

extern int toplev_main (int, char **);
extern void strip_off_ending (char *, int);
extern void _fatal_insn_not_found (const_rtx, const char *, int, const char *)
     ATTRIBUTE_NORETURN;
extern void _fatal_insn (const char *, const_rtx, const char *, int, const char *)
     ATTRIBUTE_NORETURN;

#define fatal_insn(msgid, insn) \
	_fatal_insn (msgid, insn, __FILE__, __LINE__, __FUNCTION__)
#define fatal_insn_not_found(insn) \
	_fatal_insn_not_found (insn, __FILE__, __LINE__, __FUNCTION__)

extern void rest_of_decl_compilation (tree, int, int);
extern void rest_of_type_compilation (tree, int);
extern void tree_rest_of_compilation (tree);
extern void init_optimization_passes (void);
extern void finish_optimization_passes (void);
extern bool enable_rtl_dump_file (void);

extern void announce_function (tree);

extern void error_for_asm (const_rtx, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern void warning_for_asm (const_rtx, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern void warn_deprecated_use (tree, tree);
extern bool parse_optimize_options (tree, bool);

#ifdef BUFSIZ
extern void output_quoted_string	(FILE *, const char *);
extern void output_file_directive	(FILE *, const char *);
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

extern const char *dump_base_name;
extern const char *dump_dir_name;
extern const char *aux_base_name;
extern const char *aux_info_file_name;
extern const char *profile_data_prefix;
extern const char *asm_file_name;
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

/* This function can be used by targets to set the flags originally
    implied by -ffast-math and -fno-fast-math.  */

extern void set_fast_math_flags         (int);

extern void set_unsafe_math_optimizations_flags (int);

/* Handle -d switch.  */
extern void decode_d_option		(const char *);

/* Return true iff flags are set as if -ffast-math.  */
extern bool fast_math_flags_set_p	(void);
extern bool fast_math_flags_struct_set_p (struct cl_optimization *);

/* Inline versions of the above for speed.  */
#if GCC_VERSION < 3004

/* Return log2, or -1 if not exact.  */
extern int exact_log2                  (unsigned HOST_WIDE_INT);

/* Return floor of log2, with -1 for zero.  */
extern int floor_log2                  (unsigned HOST_WIDE_INT);

#else /* GCC_VERSION >= 3004 */

# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#  define CLZ_HWI __builtin_clzl
#  define CTZ_HWI __builtin_ctzl
# elif HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONGLONG
#  define CLZ_HWI __builtin_clzll
#  define CTZ_HWI __builtin_ctzll
# else
#  define CLZ_HWI __builtin_clz
#  define CTZ_HWI __builtin_ctz
# endif

static inline int
floor_log2 (unsigned HOST_WIDE_INT x)
{
  return x ? HOST_BITS_PER_WIDE_INT - 1 - (int) CLZ_HWI (x) : -1;
}

static inline int
exact_log2 (unsigned HOST_WIDE_INT x)
{
  return x == (x & -x) && x ? (int) CTZ_HWI (x) : -1;
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
