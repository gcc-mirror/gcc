/* Exception Handling interface routines.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@cygnus.com>.

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


#ifndef GCC_VARRAY_H
struct varray_head_tag;
#define varray_type struct varray_head_tag *
#endif

struct function;

struct inline_remap;

/* Per-function EH data.  Used only in except.c, but GC and others
   manipulate pointers to the opaque type.  */
struct eh_status;

/* Internal structure describing a region.  */
struct eh_region;

/* Test: is exception handling turned on?  */
extern int doing_eh			        PARAMS ((int));

/* Start an exception handling region.  All instructions emitted after
   this point are considered to be part of the region until an
   expand_eh_region_end variant is invoked.  */
extern void expand_eh_region_start		PARAMS ((void));

/* End an exception handling region for a cleanup.  HANDLER is an
   expression to expand for the cleanup.  */
extern void expand_eh_region_end_cleanup	PARAMS ((tree));

/* End an exception handling region for a try block, and prepares
   for subsequent calls to expand_start_catch.  */
extern void expand_start_all_catch		PARAMS ((void));

/* Begin a catch clause.  TYPE is an object to be matched by the
   runtime, or a list of such objects, or null if this is a catch-all
   clause.  */
extern void expand_start_catch			PARAMS ((tree));

/* End a catch clause.  Control will resume after the try/catch block.  */
extern void expand_end_catch			PARAMS ((void));

/* End a sequence of catch handlers for a try block.  */
extern void expand_end_all_catch		PARAMS ((void));

/* End an exception region for an exception type filter.  ALLOWED is a
   TREE_LIST of TREE_VALUE objects to be matched by the runtime.
   FAILURE is a function to invoke if a mismatch occurs.  */
extern void expand_eh_region_end_allowed	PARAMS ((tree, tree));

/* End an exception region for a must-not-throw filter.  FAILURE is a
   function to invoke if an uncaught exception propagates this far.  */
extern void expand_eh_region_end_must_not_throw	PARAMS ((tree));

/* End an exception region for a throw.  No handling goes on here,
   but it's the easiest way for the front-end to indicate what type
   is being thrown.  */
extern void expand_eh_region_end_throw		PARAMS ((tree));

/* End a fixup region.  Within this region the cleanups for the immediately
   enclosing region are _not_ run.  This is used for goto cleanup to avoid
   destroying an object twice.  */
extern void expand_eh_region_end_fixup		PARAMS ((tree));

/* Begin a region that will contain entries created with
   add_partial_entry.  */
extern void begin_protect_partials              PARAMS ((void));

/* Create a new exception region and add the handler for the region
   onto a list. These regions will be ended (and their handlers emitted)
   when end_protect_partials is invoked.  */
extern void add_partial_entry			PARAMS ((tree));

/* End all of the pending exception regions that have handlers added with
   add_partial_entry.  */
extern void end_protect_partials		PARAMS ((void));

/* Invokes CALLBACK for every exception handler label.  Only used by old
   loop hackery; should not be used by new code.  */
extern void for_each_eh_label			PARAMS ((void (*) (rtx)));

/* Determine if the given INSN can throw an exception.  */
extern bool can_throw_internal			PARAMS ((rtx));
extern bool can_throw_external			PARAMS ((rtx));

/* Return nonzero if nothing in this function can throw.  */
extern bool nothrow_function_p			PARAMS ((void));

/* After initial rtl generation, call back to finish generating
   exception support code.  */
extern void finish_eh_generation		PARAMS ((void));

extern void init_eh				PARAMS ((void));
extern void init_eh_for_function		PARAMS ((void));

extern rtx reachable_handlers			PARAMS ((rtx));
extern void maybe_remove_eh_handler		PARAMS ((rtx));

extern void convert_from_eh_region_ranges	PARAMS ((void));
extern void convert_to_eh_region_ranges		PARAMS ((void));
extern void find_exception_handler_labels	PARAMS ((void));
extern bool current_function_has_exception_handlers PARAMS ((void));
extern void output_function_exception_table	PARAMS ((void));

extern void expand_builtin_unwind_init		PARAMS ((void));
extern rtx expand_builtin_eh_return_data_regno	PARAMS ((tree));
extern rtx expand_builtin_extract_return_addr	PARAMS ((tree));
extern void expand_builtin_init_dwarf_reg_sizes PARAMS ((tree));
extern rtx expand_builtin_frob_return_addr	PARAMS ((tree));
extern rtx expand_builtin_dwarf_fp_regnum	PARAMS ((void));
extern void expand_builtin_eh_return		PARAMS ((tree, tree));
extern void expand_eh_return			PARAMS ((void));
extern rtx get_exception_pointer		PARAMS ((struct function *));
extern int duplicate_eh_regions			PARAMS ((struct function *,
						 struct inline_remap *));

extern void sjlj_emit_function_exit_after	PARAMS ((rtx));


/* If non-NULL, this is a function that returns an expression to be
   executed if an unhandled exception is propagated out of a cleanup
   region.  For example, in C++, an exception thrown by a destructor
   during stack unwinding is required to result in a call to
   `std::terminate', so the C++ version of this function returns a
   CALL_EXPR for `std::terminate'.  */
extern tree (*lang_protect_cleanup_actions) PARAMS ((void));

/* Return true if type A catches type B.  */
extern int (*lang_eh_type_covers) PARAMS ((tree a, tree b));

/* Map a type to a runtime object to match type.  */
extern tree (*lang_eh_runtime_type) PARAMS ((tree));

#ifndef GCC_VARRAY_H
#undef varray_type
#endif


/* Just because the user configured --with-sjlj-exceptions=no doesn't
   mean that we can use call frame exceptions.  Detect that the target
   has appropriate support.  */

#if ! (defined (EH_RETURN_DATA_REGNO)			\
       && (defined (IA64_UNWIND_INFO)			\
	   || (DWARF2_UNWIND_INFO			\
	       && defined (EH_RETURN_STACKADJ_RTX)	\
	       && (defined (EH_RETURN_HANDLER_RTX)	\
		   || defined (HAVE_eh_return)))))
#define MUST_USE_SJLJ_EXCEPTIONS	1
#else
#define MUST_USE_SJLJ_EXCEPTIONS	0
#endif

#ifdef CONFIG_SJLJ_EXCEPTIONS
# if CONFIG_SJLJ_EXCEPTIONS == 1
#  define USING_SJLJ_EXCEPTIONS		1
# endif
# if CONFIG_SJLJ_EXCEPTIONS == 0
#  define USING_SJLJ_EXCEPTIONS		0
#  ifndef EH_RETURN_DATA_REGNO
    #error "EH_RETURN_DATA_REGNO required"
#  endif
#  ifndef EH_RETURN_STACKADJ_RTX
    #error "EH_RETURN_STACKADJ_RTX required"
#  endif
#  if !defined(EH_RETURN_HANDLER_RTX) && !defined(HAVE_eh_return)
    #error "EH_RETURN_HANDLER_RTX or eh_return required"
#  endif
#  if !defined(DWARF2_UNWIND_INFO) && !defined(IA64_UNWIND_INFO)
    #error "{DWARF2,IA64}_UNWIND_INFO required"
#  endif
# endif
#else
# define USING_SJLJ_EXCEPTIONS		MUST_USE_SJLJ_EXCEPTIONS
#endif
