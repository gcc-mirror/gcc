/* Interface for -fdump-ada-spec capability.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.

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

#ifndef C_ADA_SPEC_H
#define C_ADA_SPEC_H

#include "pretty-print.h"

/* In c-ada-spec.c  */

enum cpp_operation {
  HAS_DEPENDENT_TEMPLATE_ARGS,
  IS_ABSTRACT,
  IS_CONSTRUCTOR,
  IS_DESTRUCTOR,
  IS_COPY_CONSTRUCTOR,
  IS_MOVE_CONSTRUCTOR,
  IS_TEMPLATE,
  IS_TRIVIAL
};

extern location_t decl_sloc (const_tree, bool);
extern void collect_ada_nodes (tree, const char *);
extern void collect_source_ref (const char *);
extern void dump_ada_specs (void (*)(const char *),
			    int (*)(tree, cpp_operation));

#endif /* ! C_ADA_SPEC_H */
