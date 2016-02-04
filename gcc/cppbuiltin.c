/* Define builtin-in macros for all front ends that perform preprocessing
   Copyright (C) 2010-2016 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "version.h"
#include "flags.h"
#include "cpp-id-data.h"
#include "cppbuiltin.h"


/* Parse a BASEVER version string of the format "major.minor.patchlevel"
   or "major.minor" to extract its components.  */
void
parse_basever (int *major, int *minor, int *patchlevel)
{
  static int s_major = -1, s_minor, s_patchlevel;

  if (s_major == -1)
    if (sscanf (BASEVER, "%d.%d.%d", &s_major, &s_minor, &s_patchlevel) != 3)
      {
	sscanf (BASEVER, "%d.%d", &s_major, &s_minor);
	s_patchlevel = 0;
      }

  if (major)
    *major = s_major;

  if (minor)
    *minor = s_minor;

  if (patchlevel)
    *patchlevel = s_patchlevel;
}


/* Define __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__ and __VERSION__.  */
static void
define__GNUC__ (cpp_reader *pfile)
{
  int major, minor, patchlevel;

  parse_basever (&major, &minor, &patchlevel);
  cpp_define_formatted (pfile, "__GNUC__=%d", major);
  cpp_define_formatted (pfile, "__GNUC_MINOR__=%d", minor);
  cpp_define_formatted (pfile, "__GNUC_PATCHLEVEL__=%d", patchlevel);
  cpp_define_formatted (pfile, "__VERSION__=\"%s\"", version_string);
  cpp_define_formatted (pfile, "__ATOMIC_RELAXED=%d", MEMMODEL_RELAXED);
  cpp_define_formatted (pfile, "__ATOMIC_SEQ_CST=%d", MEMMODEL_SEQ_CST);
  cpp_define_formatted (pfile, "__ATOMIC_ACQUIRE=%d", MEMMODEL_ACQUIRE);
  cpp_define_formatted (pfile, "__ATOMIC_RELEASE=%d", MEMMODEL_RELEASE);
  cpp_define_formatted (pfile, "__ATOMIC_ACQ_REL=%d", MEMMODEL_ACQ_REL);
  cpp_define_formatted (pfile, "__ATOMIC_CONSUME=%d", MEMMODEL_CONSUME);
}


/* Define various built-in CPP macros that depend on language-independent
   compilation flags.  */
static void
define_builtin_macros_for_compilation_flags (cpp_reader *pfile)
{
  if (flag_pic)
    {
      cpp_define_formatted (pfile, "__pic__=%d", flag_pic);
      cpp_define_formatted (pfile, "__PIC__=%d", flag_pic);
    }
  if (flag_pie)
    {
      cpp_define_formatted (pfile, "__pie__=%d", flag_pie);
      cpp_define_formatted (pfile, "__PIE__=%d", flag_pie);
    }

  if (flag_sanitize & SANITIZE_ADDRESS)
    cpp_define (pfile, "__SANITIZE_ADDRESS__");

  if (optimize_size)
    cpp_define (pfile, "__OPTIMIZE_SIZE__");
  if (optimize)
    cpp_define (pfile, "__OPTIMIZE__");

  if (fast_math_flags_set_p (&global_options))
    cpp_define (pfile, "__FAST_MATH__");
  if (flag_signaling_nans)
    cpp_define (pfile, "__SUPPORT_SNAN__");
  if (!flag_errno_math)
    cpp_define (pfile, "__NO_MATH_ERRNO__");

  cpp_define_formatted (pfile, "__FINITE_MATH_ONLY__=%d",
			flag_finite_math_only);
  if (flag_cilkplus)
    cpp_define (pfile, "__cilk=200");

  if (flag_check_pointer_bounds)
    cpp_define (pfile, "__CHKP__");
}


/* Define built-in macros for LP64 targets. */
static void
define_builtin_macros_for_lp64 (cpp_reader *pfile)
{
  if (TYPE_PRECISION (long_integer_type_node) == 64
      && POINTER_SIZE == 64
      && TYPE_PRECISION (integer_type_node) == 32)
    {
      cpp_define (pfile, "_LP64");
      cpp_define (pfile, "__LP64__");
    }
}


/* Define macros for size of basic C types.  */
static void
define_builtin_macros_for_type_sizes (cpp_reader *pfile)
{
#define define_type_sizeof(NAME, TYPE)                             \
    cpp_define_formatted (pfile, NAME"=" HOST_WIDE_INT_PRINT_DEC,   \
                          tree_to_uhwi (TYPE_SIZE_UNIT (TYPE)))

  define_type_sizeof ("__SIZEOF_INT__", integer_type_node);
  define_type_sizeof ("__SIZEOF_LONG__", long_integer_type_node);
  define_type_sizeof ("__SIZEOF_LONG_LONG__", long_long_integer_type_node);
  define_type_sizeof ("__SIZEOF_SHORT__", short_integer_type_node);
  define_type_sizeof ("__SIZEOF_FLOAT__", float_type_node);
  define_type_sizeof ("__SIZEOF_DOUBLE__", double_type_node);
  define_type_sizeof ("__SIZEOF_LONG_DOUBLE__", long_double_type_node);
  define_type_sizeof ("__SIZEOF_SIZE_T__", size_type_node);

#undef define_type_sizeof

  cpp_define_formatted (pfile, "__CHAR_BIT__=%u",
			TYPE_PRECISION (char_type_node));
  cpp_define_formatted (pfile, "__BIGGEST_ALIGNMENT__=%d",
			BIGGEST_ALIGNMENT / BITS_PER_UNIT);

  /* Define constants useful for implementing endian.h.  */
  cpp_define (pfile, "__ORDER_LITTLE_ENDIAN__=1234");
  cpp_define (pfile, "__ORDER_BIG_ENDIAN__=4321");
  cpp_define (pfile, "__ORDER_PDP_ENDIAN__=3412");

  if (WORDS_BIG_ENDIAN == BYTES_BIG_ENDIAN)
    cpp_define_formatted (pfile, "__BYTE_ORDER__=%s",
			  (WORDS_BIG_ENDIAN
			   ? "__ORDER_BIG_ENDIAN__"
			   : "__ORDER_LITTLE_ENDIAN__"));
  else
    {
      /* Assert that we're only dealing with the PDP11 case.  */
      gcc_assert (!BYTES_BIG_ENDIAN);
      gcc_assert (WORDS_BIG_ENDIAN);

      cpp_define (pfile, "__BYTE_ORDER__=__ORDER_PDP_ENDIAN__");
    }

  cpp_define_formatted (pfile, "__FLOAT_WORD_ORDER__=%s",
                        (targetm.float_words_big_endian ()
                         ? "__ORDER_BIG_ENDIAN__"
                         : "__ORDER_LITTLE_ENDIAN__"));

  /* ptr_type_node can't be used here since ptr_mode is only set when
     toplev calls backend_init which is not done with -E switch.  */
  cpp_define_formatted (pfile, "__SIZEOF_POINTER__=%d",
			1 << ceil_log2 ((POINTER_SIZE + BITS_PER_UNIT - 1) / BITS_PER_UNIT));
}


/* Define macros builtins common to all language performing CPP
   preprocessing.  */
void
define_language_independent_builtin_macros (cpp_reader *pfile)
{
  define__GNUC__ (pfile);
  define_builtin_macros_for_compilation_flags (pfile);
  define_builtin_macros_for_lp64 (pfile);
  define_builtin_macros_for_type_sizes (pfile);
}
