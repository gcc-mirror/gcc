/* Declarations anda data types for RTL call insn generation.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.

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

#ifndef GCC_CALLS_H
#define GCC_CALLS_H

extern int flags_from_decl_or_type (const_tree);
extern int call_expr_flags (const_tree);
extern int setjmp_call_p (const_tree);
extern bool gimple_alloca_call_p (const_gimple);
extern bool alloca_call_p (const_tree);
extern bool must_pass_in_stack_var_size (enum machine_mode, const_tree);
extern bool must_pass_in_stack_var_size_or_pad (enum machine_mode, const_tree);

#endif // GCC_CALLS_H
