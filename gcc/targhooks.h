/* Default target hook functions.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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

extern void default_external_libcall (rtx);

extern enum machine_mode default_cc_modes_compatible (enum machine_mode,
						      enum machine_mode);

extern bool default_promote_function_args (tree);
extern bool default_promote_function_return (tree);
extern bool default_promote_prototypes (tree);

extern rtx default_struct_value_rtx (tree, int);
extern bool default_return_in_memory (tree, tree);

extern rtx default_expand_builtin_saveregs (void);
extern void default_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode, tree, int *, int);
extern bool default_strict_argument_naming (CUMULATIVE_ARGS *);
extern bool default_pretend_outgoing_varargs_named (CUMULATIVE_ARGS *);

extern bool hook_bool_CUMULATIVE_ARGS_true (CUMULATIVE_ARGS *);
extern bool hook_bool_machine_mode_true (enum machine_mode);
