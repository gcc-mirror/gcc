/* toplev.h - Various declarations for functions found in toplev.c
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#ifndef GCC_TOPLEV_H
#define GCC_TOPLEV_H

#ifdef ANSI_PROTOTYPES
union tree_node;
struct rtx_def;
#endif

/* If non-NULL, return one past-the-end of the matching SUBPART of
   the WHOLE string.  */
#define skip_leading_substring(whole,  part) \
   (strncmp (whole, part, strlen (part)) ? NULL : whole + strlen (part))

extern int toplev_main			PARAMS ((int argc, char **argv));
extern int read_integral_parameter	PARAMS ((const char *, const char *,
						const int));
extern int count_error			PARAMS ((int));
extern void strip_off_ending		PARAMS ((char *, int));
extern void print_time			PARAMS ((const char *, long));
extern const char *trim_filename	PARAMS ((const char *));
extern void internal_error		PARAMS ((const char *, ...))
					       ATTRIBUTE_NORETURN;
extern void fatal_io_error		PARAMS ((const char *, ...))
					       ATTRIBUTE_NORETURN;
extern void _fatal_insn_not_found	PARAMS ((struct rtx_def *,
						const char *, int,
						const char *))
					       ATTRIBUTE_NORETURN;
extern void _fatal_insn			PARAMS ((const char *,
						struct rtx_def *,
						const char *, int,
						const char *))
					      ATTRIBUTE_NORETURN;

#define fatal_insn(msgid, insn) \
	_fatal_insn (msgid, insn, __FILE__, __LINE__, __FUNCTION__)
#define fatal_insn_not_found(insn) \
	_fatal_insn_not_found (insn, __FILE__, __LINE__, __FUNCTION__)

/* None of these functions are suitable for ATTRIBUTE_PRINTF, because
   each language front end can extend them with its own set of format
   specifiers.  */
extern void warning			PARAMS ((const char *, ...));
extern void error			PARAMS ((const char *, ...));
extern void fatal_error			PARAMS ((const char *, ...))
					       ATTRIBUTE_NORETURN;
extern void pedwarn			PARAMS ((const char *, ...));
extern void pedwarn_with_file_and_line	PARAMS ((const char *, int,
						 const char *, ...));
extern void warning_with_file_and_line	PARAMS ((const char *, int,
						 const char *, ...));
extern void error_with_file_and_line	PARAMS ((const char *, int,
						 const char *, ...));
extern void sorry			PARAMS ((const char *, ...));
extern void report_error_function	PARAMS ((const char *));

extern void rest_of_decl_compilation	PARAMS ((union tree_node *,
						const char *, int, int));
extern void rest_of_type_compilation	PARAMS ((union tree_node *, int));
extern void rest_of_compilation		PARAMS ((union tree_node *));

extern void pedwarn_with_decl		PARAMS ((union tree_node *,
						 const char *, ...));
extern void warning_with_decl		PARAMS ((union tree_node *,
						 const char *, ...));
extern void error_with_decl		PARAMS ((union tree_node *,
						 const char *, ...));

extern void announce_function		PARAMS ((union tree_node *));

extern void error_for_asm		PARAMS ((struct rtx_def *,
						 const char *, ...));
extern void warning_for_asm		PARAMS ((struct rtx_def *,
						 const char *, ...));
extern int do_float_handler PARAMS ((void (*) (PTR), PTR));

#ifdef BUFSIZ
extern void output_quoted_string	PARAMS ((FILE *, const char *));
extern void output_file_directive	PARAMS ((FILE *, const char *));
#endif
extern void do_abort			PARAMS ((void)) ATTRIBUTE_NORETURN;
extern void botch			PARAMS ((const char *))
  ATTRIBUTE_NORETURN;

#ifdef BUFSIZ
  /* N.B. Unlike all the others, fnotice is just gettext+fprintf, and
     therefore it can have ATTRIBUTE_PRINTF.  */
extern void fnotice			PARAMS ((FILE *, const char *, ...))
					       ATTRIBUTE_PRINTF_2;
#endif

extern int wrapup_global_declarations   PARAMS ((union tree_node **, int));
extern void check_global_declarations   PARAMS ((union tree_node **, int));

extern const char *progname;
extern const char *dump_base_name;

/* The following hooks are documented in langhooks.c.  Must not be
   NULL.  */

struct lang_hooks_for_tree_inlining
{
  union tree_node *(*walk_subtrees) PARAMS ((union tree_node **, int *,
					     union tree_node *(*)
					     (union tree_node **,
					      int *, void *),
					     void *, void *));
  int (*cannot_inline_tree_fn) PARAMS ((union tree_node **));
  int (*disregard_inline_limits) PARAMS ((union tree_node *));
  union tree_node *(*add_pending_fn_decls) PARAMS ((void*, union tree_node *));
  int (*tree_chain_matters_p) PARAMS ((union tree_node *));
  int (*auto_var_in_fn_p) PARAMS ((union tree_node *, union tree_node *));
  union tree_node *(*copy_res_decl_for_inlining) PARAMS ((union tree_node *,
							  union tree_node *,
							  union tree_node *,
							  void *, int *,
							  void *));
  int (*anon_aggr_type_p) PARAMS ((union tree_node *));
};

/* Language-specific hooks.  Can be NULL unless otherwise specified.  */

struct lang_hooks
{
  /* Called first, to initialize the front end.  */
  void (*init) PARAMS ((void));

  /* Called last, as a finalizer.  */
  void (*finish) PARAMS ((void));

  /* Called to initialize options, before any calls to decode_option.  */
  void (*init_options) PARAMS ((void));

  /* Function called with an option vector as argument, to decode a
     single option (typically starting with -f or -W or +).  It should
     return the number of command-line arguments it uses if it handles
     the option, or 0 and not complain if it does not recognise the
     option.  If this function returns a negative number, then its
     absolute value is the number of command-line arguments used, but,
     in addition, no language-independent option processing should be
     done for this option.

     This hook cannot be NULL.  */
  int (*decode_option) PARAMS ((int, char **));

  /* Called when all command line options have been processed.  */
  void (*post_options) PARAMS ((void));

  struct lang_hooks_for_tree_inlining tree_inlining;

  /* Whenever you add entries here, make sure you adjust langhooks.h
     and langhooks.c accordingly.  */
};

/* Each front end provides its own.  */
extern struct lang_hooks lang_hooks;

/* The hashtable, so that the C front ends can pass it to cpplib.  */
extern struct ht *ident_hash;

/* These functions can be used by targets to set the flags originally
   implied by -ffast-math and -fno-fast-math.  */

extern void set_fast_math_flags         PARAMS ((void));
extern void set_no_fast_math_flags      PARAMS ((void));

/* The following functions accept a wide integer argument.  Rather
   than having to cast on every function call, we use a macro instead.  */

#ifndef exact_log2
#define exact_log2(N) exact_log2_wide ((unsigned HOST_WIDE_INT) (N))
#define floor_log2(N) floor_log2_wide ((unsigned HOST_WIDE_INT) (N))
#endif
extern int exact_log2_wide             PARAMS ((unsigned HOST_WIDE_INT));
extern int floor_log2_wide             PARAMS ((unsigned HOST_WIDE_INT));

#endif /* ! GCC_TOPLEV_H */
