/* Configuration for GNU C-compiler for Vax.
   Copyright (C) 1987, 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

/* Other configurations get these via autoconfig.  */
#define STDC_HEADERS 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#ifdef __DECC
#define HAVE_UNISTD_H 1
#endif

#if defined(VAXC) || defined(__DECC)
/* if compiling with VAXC, need to fix problem with <stdio.h>
   which defines a macro called FILE_TYPE that breaks "tree.h".
   Fortunately it uses #ifndef to suppress multiple inclusions.
   Three possible cases:
	1) <stdio.h> has already been included -- ours will be no-op;
	2) <stdio.h> will be included after us -- "theirs" will be no-op;
	3) <stdio.h> isn't needed -- including it here shouldn't hurt.
   In all three cases, the problem macro will be removed here.  */
#include <stdio.h>
#undef FILE_TYPE
#endif

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR 8
#define HOST_BITS_PER_SHORT 16
#define HOST_BITS_PER_INT 32
#define HOST_BITS_PER_LONG 32
#define HOST_BITS_PER_LONGLONG 64

#define HOST_FLOAT_FORMAT VAX_FLOAT_FORMAT

#define SUCCESS_EXIT_CODE 1
#define FATAL_EXIT_CODE (44 | 0x10000000)  /* Abort, and no DCL message.  */

/* A couple of conditionals for execution machine are controlled here.  */
#ifndef VMS
#define VMS
#endif

#ifndef __GNUC__
/* not present, at least in VAX-11 C (VMS) v2.2 */
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif

#define GCC_INCLUDE_DIR "///not used with VMS///"	/* nonsense string for now */

/* and define a local equivalent (sort of) for unlink */
#define unlink remove

/* Used by the preprocessor to limit size of disk I/O chunks.
   64K - 1 is the maximum supported by VAXCRTL.  Amounts in excess
   of 35 blocks will bypass the VMS V6.x VIOC [Virtual I/O Cache],
   so we'll pick a limit of 16K (32 blocks).  */
#define MAX_READ_LEN	(32 * 512)
#define MAX_WRITE_LEN	(32 * 512)

/* Under VMS a directory specification can be enclosed either in square
   brackets or in angle brackets.  Thus we need to check both.  This
   macro is used to help compare filenames in cp-lex.c.

   We also need to make sure that the names are all lower case, because
   we must be able to compare filenames to determine if a file implements
   a class.  */

#define FILE_NAME_NONDIRECTORY(C)				\
({								\
   char * pnt_ = (C), * pnt1_;					\
   pnt1_ = pnt_ - 1;						\
   while (*++pnt1_)						\
     if ((*pnt1_ >= 'A' && *pnt1_ <= 'Z')) *pnt1_ |= 0x20;	\
   pnt1_ = rindex (pnt_, ']'); 					\
   pnt1_ = (pnt1_ == 0 ? rindex (pnt_, '>') : pnt1_);		\
   pnt1_ = (pnt1_ == 0 ? rindex (pnt_, ':') : pnt1_);		\
   (pnt1_ == 0 ? pnt_ : pnt1_ + 1);				\
 })

/* Macro to generate the name of the cross reference file.  The standard
   one does not work, since it was written assuming that the conventions
   of a unix style filesystem will work on the host system.  */

#define XREF_FILE_NAME(BUFF, NAME)	\
  s = FILE_NAME_NONDIRECTORY (NAME);			\
  if (s == NAME) sprintf(BUFF, "%s_gxref", NAME);	\
  else {						\
    strcpy(BUFF, NAME);					\
    strcat(BUFF, "_gxref");				\
  }

/* Macro that is used in cp-xref.c to determine whether a file name is
   absolute or not.  */

#define FILE_NAME_ABSOLUTE_P(NAME)	\
	(FILE_NAME_NONDIRECTORY (NAME) != (&NAME[1]))

/* FILE_NAME_JOINER is defined to be the characters that are inserted between 
   a directory name and a filename in order to make an absolute file
   specification.  Under VMS the directory specification contains all of the
   required characters, so we define this to be a null string.  */

#define FILE_NAME_JOINER ""

/* vprintf() has been available since VMS V4.6.  */

#define HAVE_VPRINTF

#if defined(VAXC) || defined(__DECC)

/* Customizations/kludges for building with DEC's VAX C compiler
   rather than GCC.  */

#define NO_SYS_PARAMS_H		/* don't have <sys/params.h> */
#define USE_C_ALLOCA		/* using alloca.c */
#define QSORT_WORKAROUND	/* do not use VAXCRTL's qsort */

/* use ANSI/SYSV style byte manipulation routines instead of BSD ones */
/* rename all too-long external symbol names to avoid warnings */
#define check_for_full_enumeration_handling	check_for_full_enum_handling
#define current_function_contains_functions	curfunc_contains_functions
#define current_function_epilogue_delay_list	curfunc_epilogue_delay_list
#define current_function_has_nonlocal_goto	curfunc_has_nonlocal_goto
#define current_function_has_nonlocal_label	curfunc_has_nonlocal_label
#define current_function_internal_arg_pointer	curfunc_internal_arg_pointer
#define current_function_outgoing_args_size	curfunc_outgoing_args_size
#define current_function_pretend_args_size	curfunc_pretend_args_size
#define current_function_returns_pcc_struct	curfunc_returns_pcc_struct
#define current_function_returns_pointer	curfunc_returns_pointer
#define current_function_uses_const_pool	curfunc_uses_const_pool
#define current_function_uses_pic_offset_table	curfunc_uses_pic_offset_table
#define dbxout_resume_previous_source_file	dbxout_resume_previous_src_file
#define expand_builtin_extract_return_addr	expand_builtin_extract_ret_addr
#define expand_builtin_set_return_addr_reg	expand_builtin_set_ret_addr_reg
#define expand_start_loop_continue_elsewhere	expnd_start_loop_cont_elsewhere
#define flag_schedule_insns_after_reload	flag_sched_insns_after_reload
#define get_dynamic_handler_chain_libfunc	get_dynamic_hndlr_chain_libfunc
#define lookup_name_current_level_global	lookup_name_current_level_gbl
#define maybe_building_objc_message_expr	maybe_building_objc_msg_expr
#define mesg_implicit_function_declaration	mesg_implicit_func_declaration
#define output_deferred_addressed_constants	output_deferred_addr_constants
#define protect_cleanup_actions_with_terminate  protect_cleanup_act_w_terminate
#define reg_overlap_mentioned_for_reload_p	reg_overlap_mtnd_for_reload_p
#define reposition_prologue_and_epilogue_notes	repos_prolog_and_epilog_notes
#define rtx_equal_function_value_matters	rtx_equal_func_value_matters
#define set_new_first_and_last_label_num	set_new_first_and_last_lbl_num
#define thread_prologue_and_epilogue_insns	thread_prolog_and_epilog_insns
#endif

/* We need to avoid the library qsort routine, due to a serious bug
   in VAXCRTL.  (Sorting anything with size that's not a multiple of 4
   after having previously sorted something that was a multiple of 4
   can produce wrong results and result in data corruption.)  We'll
   use our own substitute (in vax.c) instead.  */
/* #define QSORT_WORKAROUND */
#ifdef QSORT_WORKAROUND
#define qsort not_qsort
#endif

#ifdef __DECC
/* DECC$SHR doesn't have VAXCRTL's bugs.  */
#undef QSORT_WORKAROUND
#undef qsort
/* Avoid a lot of informational level diagnostics about implicitly
   declared functions.  */
#include <stdlib.h>
#include <string.h>
/* this is for genopinit.c */
 #pragma message disable (undefescap)
#endif

#if defined(USE_C_ALLOCA) && !defined(alloca)
/* Declare alloca() using similar logic to that in alloca.c.  */
#ifdef __STDC__
extern void *alloca(unsigned);
#else
extern char *alloca();
#endif
#endif
