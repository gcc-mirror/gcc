/* Configuration for GNU C-compiler for Vax.
   Copyright (C) 1987 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

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
#define FATAL_EXIT_CODE (44 | 0x10000000)  /* Failure, and no DCL message.  */


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

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
#define alloca __builtin_alloca
#endif

#define GCC_INCLUDE_DIR "///not used with VMS///"	/* nonsense string for now */

/* and define a local equivalent (sort of) for unlink */
#define unlink remove

/* Specify the list of include file directories.  */
#define INCLUDE_DEFAULTS \
{									\
  { "GNU_GXX_INCLUDE:", 1},						\
  { "GNU_CC_INCLUDE:", 0},	/* GNU includes */			\
  { "SYS$SYSROOT:[SYSLIB.]", 0}, /* VAX-11 "C" includes */		\
  { ".", 0},			/* Make normal VMS filespecs work.  */	\
  { 0, 0}								\
}

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

#ifdef VAXC
/* Customizations/kludges for building with DEC's VAX C compiler
   rather than GCC.  */
#define NO_SYS_PARAMS_H		/* don't have <sys/params.h> */
#define NO_STAB_H		/* don't have <stab.h> */
#define USE_C_ALLOCA		/* using alloca.c */
#define QSORT_WORKAROUND	/* do not use VAXCRTL's qsort */

/* use ANSI/SYSV style byte manipulation routines instead of BSD ones */
#define bcopy(s,d,n)	memcpy((d),(s),(n))
#define bzero(d,n)	memset((d),0,(n))
#define bcmp(l,r,n)	memcmp((l),(r),(n))
#define index	strchr
#define rindex	strrchr

/* rename all too-long external symbol names to avoid warnings */
#define check_for_full_enumeration_handling	check_for_full_enum_handling
#define current_function_contains_functions	curfunc_contains_functions
#define current_function_epilogue_delay_list	curfunc_epilogue_delay_list
#define current_function_has_nonlocal_label	curfunc_has_nonlocal_label
#define current_function_internal_arg_pointer	curfunc_internal_arg_pointer
#define current_function_outgoing_args_size	curfunc_outgoing_args_size
#define current_function_pretend_args_size	curfunc_pretend_args_size
#define current_function_returns_pcc_struct	curfunc_returns_pcc_struct
#define current_function_returns_pointer	curfunc_returns_pointer
#define current_function_uses_const_pool	curfunc_uses_const_pool
#define current_function_uses_pic_offset_table	curfunc_uses_pic_offset_table
#define expand_start_loop_continue_elsewhere	expnd_start_loop_cont_elsewhere
#define flag_schedule_insns_after_reload	flag_sched_insns_after_reload
#define maybe_building_objc_message_expr	maybe_building_objc_msg_expr
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
