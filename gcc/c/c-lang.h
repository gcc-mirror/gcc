/* Definitions for C language specific types.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

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

#ifndef GCC_C_LANG_H
#define GCC_C_LANG_H

#include "c-family/c-common.h"

/* In a RECORD_TYPE, a sorted array of the fields of the type, not a
   tree for size reasons.  */
struct GTY(()) sorted_fields_type {
  int len;
  tree GTY((length ("%h.len"))) elts[1];
};

struct GTY(()) lang_type {
  /* In a RECORD_TYPE, a sorted array of the fields of the type.  */
  struct sorted_fields_type * GTY ((reorder ("resort_sorted_fields"))) s;
  /* In an ENUMERAL_TYPE, the min and max values.  */
  tree enum_min;
  tree enum_max;
  /* In a RECORD_TYPE, information specific to Objective-C, such
     as a list of adopted protocols or a pointer to a corresponding
     @interface.  See objc/objc-act.h for details.  */
  tree objc_info;
};

struct GTY(()) lang_decl {
  char dummy;
};

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct GTY(()) language_function {
  struct c_language_function base;
  unsigned char x_in_statement;
  struct c_switch * GTY((skip)) x_switch_stack;
  struct c_arg_info * GTY((skip)) arg_info;
  int returns_value;
  int returns_null;
  int returns_abnormally;
  int warn_about_return_type;
};

struct GTY(()) c_omp_declare_target_attr {
  bool attr_syntax;
  int device_type;
  int indirect;
};

struct GTY(()) c_omp_begin_assumes_data {
  bool attr_syntax;
};

/* If non-empty, implicit "omp declare target" attribute is added into the
   attribute lists.  */
extern GTY(()) vec<c_omp_declare_target_attr, va_gc>
  *current_omp_declare_target_attribute;
/* Similarly whether we are in between #pragma omp begin assumes and
   #pragma omp end assumes (and how many times when nested).  */
extern GTY(()) vec<c_omp_begin_assumes_data, va_gc>
  *current_omp_begin_assumes;

#endif /* ! GCC_C_LANG_H */
