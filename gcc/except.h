/* Exception Handling interface routines.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
   2007, 2008, 2009  Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@cygnus.com>.

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

#include "sbitmap.h"
#include "vecprim.h"

struct function;

/* The type of an exception region.  */
enum eh_region_type
{
  ERT_UNKNOWN = 0,
  ERT_CLEANUP,
  ERT_TRY,
  ERT_CATCH,
  ERT_ALLOWED_EXCEPTIONS,
  ERT_MUST_NOT_THROW,
  ERT_THROW
};

/* Describes one exception region.  */
struct GTY(()) eh_region_d
{
  /* The immediately surrounding region.  */
  struct eh_region_d *outer;

  /* The list of immediately contained regions.  */
  struct eh_region_d *inner;
  struct eh_region_d *next_peer;

  /* List of regions sharing label.  */
  struct eh_region_d *next_region_sharing_label;

  /* An identifier for this region.  */
  int region_number;

  /* When a region is deleted, its parents inherit the REG_EH_REGION
     numbers already assigned.  */
  bitmap aka;

  /* Each region does exactly one thing.  */
  enum eh_region_type type;

  /* Holds the action to perform based on the preceding type.  */
  union eh_region_u {
    /* A list of catch blocks, a surrounding try block,
       and the label for continuing after a catch.  */
    struct eh_region_u_try {
      struct eh_region_d *eh_catch;
      struct eh_region_d *last_catch;
    } GTY ((tag ("ERT_TRY"))) eh_try;

    /* The list through the catch handlers, the list of type objects
       matched, and the list of associated filters.  */
    struct eh_region_u_catch {
      struct eh_region_d *next_catch;
      struct eh_region_d *prev_catch;
      tree type_list;
      tree filter_list;
    } GTY ((tag ("ERT_CATCH"))) eh_catch;

    /* A tree_list of allowed types.  */
    struct eh_region_u_allowed {
      tree type_list;
      int filter;
    } GTY ((tag ("ERT_ALLOWED_EXCEPTIONS"))) allowed;

    /* The type given by a call to "throw foo();", or discovered
       for a throw.  */
    struct eh_region_u_throw {
      tree type;
    } GTY ((tag ("ERT_THROW"))) eh_throw;
  } GTY ((desc ("%0.type"))) u;

  /* Entry point for this region's handler before landing pads are built.  */
  rtx label;
  tree tree_label;

  /* Entry point for this region's handler from the runtime eh library.  */
  rtx landing_pad;

  /* Entry point for this region's handler from an inner region.  */
  rtx post_landing_pad;

  /* The RESX insn for handing off control to the next outermost handler,
     if appropriate.  */
  rtx resume;

  /* True if something in this region may throw.  */
  unsigned may_contain_throw : 1;
};

typedef struct eh_region_d *eh_region;
DEF_VEC_P(eh_region);
DEF_VEC_ALLOC_P(eh_region, gc);
DEF_VEC_ALLOC_P(eh_region, heap);

/* Per-function EH data.  Used to save exception status for each
   function.  */
struct GTY(()) eh_status
{
  /* The tree of all regions for this function.  */
  struct eh_region_d *region_tree;

  /* The same information as an indexable array.  */
  VEC(eh_region,gc) *region_array;
  int last_region_number;

  htab_t GTY((param_is (struct throw_stmt_node))) throw_stmt_table;
};


/* Test: is exception handling turned on?  */
extern int doing_eh (int);

/* Note that the current EH region (if any) may contain a throw, or a
   call to a function which itself may contain a throw.  */
extern void note_eh_region_may_contain_throw (struct eh_region_d *);

/* Invokes CALLBACK for every exception handler label.  Only used by old
   loop hackery; should not be used by new code.  */
extern void for_each_eh_label (void (*) (rtx));

/* Invokes CALLBACK for every exception region in the current function.  */
extern void for_each_eh_region (void (*) (struct eh_region_d *));

/* Determine if the given INSN can throw an exception.  */
extern bool can_throw_internal_1 (int, bool, bool);
extern bool can_throw_internal (const_rtx);
extern bool can_throw_external_1 (int, bool, bool);
extern bool can_throw_external (const_rtx);

/* Set TREE_NOTHROW and cfun->all_throwers_are_sibcalls.  */
extern unsigned int set_nothrow_function_flags (void);

extern void init_eh (void);
extern void init_eh_for_function (void);

extern rtx reachable_handlers (rtx);
extern void remove_eh_region (int);
extern void remove_eh_region_and_replace_by_outer_of (int, int);

extern void convert_from_eh_region_ranges (void);
extern unsigned int convert_to_eh_region_ranges (void);
extern void find_exception_handler_labels (void);
extern bool current_function_has_exception_handlers (void);
extern void output_function_exception_table (const char *);

extern void expand_builtin_unwind_init (void);
extern rtx expand_builtin_eh_return_data_regno (tree);
extern rtx expand_builtin_extract_return_addr (tree);
extern void expand_builtin_init_dwarf_reg_sizes (tree);
extern rtx expand_builtin_frob_return_addr (tree);
extern rtx expand_builtin_dwarf_sp_column (void);
extern void expand_builtin_eh_return (tree, tree);
extern void expand_eh_return (void);
extern rtx expand_builtin_extend_pointer (tree);
extern rtx get_exception_pointer (void);
extern rtx get_exception_filter (void);
typedef tree (*duplicate_eh_regions_map) (tree, void *);
extern int duplicate_eh_regions (struct function *, duplicate_eh_regions_map,
				 void *, int, int);

extern void sjlj_emit_function_exit_after (rtx);
extern void default_init_unwind_resume_libfunc (void);

extern struct eh_region_d *gen_eh_region_cleanup (struct eh_region_d *);
extern struct eh_region_d *gen_eh_region_try (struct eh_region_d *);
extern struct eh_region_d *gen_eh_region_catch (struct eh_region_d *, tree);
extern struct eh_region_d *gen_eh_region_allowed (struct eh_region_d *, tree);
extern struct eh_region_d *gen_eh_region_must_not_throw (struct eh_region_d *);
extern int get_eh_region_number (struct eh_region_d *);
extern bool get_eh_region_may_contain_throw (struct eh_region_d *);
extern tree get_eh_region_no_tree_label (int);
extern tree get_eh_region_tree_label (struct eh_region_d *);
extern void set_eh_region_tree_label (struct eh_region_d *, tree);

extern void foreach_reachable_handler (int, bool, bool,
				       void (*) (struct eh_region_d *, void *),
				       void *);

extern void collect_eh_region_array (void);
extern void expand_resx_expr (tree);
extern void verify_eh_tree (struct function *);
extern void dump_eh_tree (FILE *, struct function *);
void debug_eh_tree (struct function *);
extern int eh_region_outermost (struct function *, int, int);
extern void add_type_for_runtime (tree);
extern tree lookup_type_for_runtime (tree);

/* If non-NULL, this is a function that returns an expression to be
   executed if an unhandled exception is propagated out of a cleanup
   region.  For example, in C++, an exception thrown by a destructor
   during stack unwinding is required to result in a call to
   `std::terminate', so the C++ version of this function returns a
   CALL_EXPR for `std::terminate'.  */
extern gimple (*lang_protect_cleanup_actions) (void);

/* Return true if type A catches type B.  */
extern int (*lang_eh_type_covers) (tree a, tree b);

/* Map a type to a runtime object to match type.  */
extern tree (*lang_eh_runtime_type) (tree);


/* Just because the user configured --with-sjlj-exceptions=no doesn't
   mean that we can use call frame exceptions.  Detect that the target
   has appropriate support.  */

#ifndef MUST_USE_SJLJ_EXCEPTIONS
# if defined (EH_RETURN_DATA_REGNO)			\
       && (defined (TARGET_UNWIND_INFO)			\
	   || (DWARF2_UNWIND_INFO			\
	       && (defined (EH_RETURN_HANDLER_RTX)	\
		   || defined (HAVE_eh_return))))
#  define MUST_USE_SJLJ_EXCEPTIONS	0
# else
#  define MUST_USE_SJLJ_EXCEPTIONS	1
# endif
#endif

#ifdef CONFIG_SJLJ_EXCEPTIONS
# if CONFIG_SJLJ_EXCEPTIONS == 1
#  define USING_SJLJ_EXCEPTIONS		1
# endif
# if CONFIG_SJLJ_EXCEPTIONS == 0
#  define USING_SJLJ_EXCEPTIONS		0
#  if !defined(EH_RETURN_DATA_REGNO)
    #error "EH_RETURN_DATA_REGNO required"
#  endif
#  if ! (defined(TARGET_UNWIND_INFO) || DWARF2_UNWIND_INFO)
    #error "{DWARF2,TARGET}_UNWIND_INFO required"
#  endif
#  if !defined(TARGET_UNWIND_INFO) \
	&& !(defined(EH_RETURN_HANDLER_RTX) || defined(HAVE_eh_return))
    #error "EH_RETURN_HANDLER_RTX or eh_return required"
#  endif
/* Usually the above error checks will have already triggered an
   error, but backends may set MUST_USE_SJLJ_EXCEPTIONS for their own
   reasons.  */
#  if MUST_USE_SJLJ_EXCEPTIONS
    #error "Must use SJLJ exceptions but configured not to"
#  endif
# endif
#else
# define USING_SJLJ_EXCEPTIONS		MUST_USE_SJLJ_EXCEPTIONS
#endif

struct GTY(()) throw_stmt_node {
  gimple stmt;
  int region_nr;
};

extern struct htab *get_eh_throw_stmt_table (struct function *);
extern void set_eh_throw_stmt_table (struct function *, struct htab *);
extern void remove_unreachable_regions (sbitmap, sbitmap);
extern VEC(int,heap) * label_to_region_map (void);
extern int num_eh_regions (void);
extern bitmap must_not_throw_labels (void);
extern struct eh_region_d *redirect_eh_edge_to_label (struct edge_def *, tree, bool, bool, int);
extern int get_next_region_sharing_label (int);
