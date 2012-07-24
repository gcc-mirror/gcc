/* Definitions for C language specific types.
   Copyright (C) 2009, 2010
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

#ifndef GCC_C_LANG_H
#define GCC_C_LANG_H

#include "c-family/c-common.h"
#include "ggc.h"

struct GTY((variable_size)) lang_type {
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

struct GTY((variable_size)) lang_decl {
  char dummy;
};

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct GTY(()) language_function {
  struct c_language_function base;
  tree x_break_label;
  tree x_cont_label;
  struct c_switch * GTY((skip)) x_switch_stack;
  struct c_arg_info * GTY((skip)) arg_info;
  int returns_value;
  int returns_null;
  int returns_abnormally;
  int warn_about_return_type;
};


#endif /* ! GCC_C_LANG_H */
