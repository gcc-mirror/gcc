/* Expand builtin functions.
   Copyright (C) 1988-2014 Free Software Foundation, Inc.

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

#ifndef GCC_BUILTINS_H
#define GCC_BUILTINS_H

#include <mpc.h>

/* Target-dependent globals.  */
struct target_builtins {
  /* For each register that may be used for calling a function, this
     gives a mode used to copy the register's value.  VOIDmode indicates
     the register is not used for calling a function.  If the machine
     has register windows, this gives only the outbound registers.
     INCOMING_REGNO gives the corresponding inbound register.  */
  enum machine_mode x_apply_args_mode[FIRST_PSEUDO_REGISTER];

  /* For each register that may be used for returning values, this gives
     a mode used to copy the register's value.  VOIDmode indicates the
     register is not used for returning values.  If the machine has
     register windows, this gives only the outbound registers.
     INCOMING_REGNO gives the corresponding inbound register.  */
  enum machine_mode x_apply_result_mode[FIRST_PSEUDO_REGISTER];
};

extern GTY(()) struct target_builtins default_target_builtins;
#if SWITCHABLE_TARGET
extern struct target_builtins *this_target_builtins;
#else
#define this_target_builtins (&default_target_builtins)
#endif

/* Non-zero if __builtin_constant_p should be folded right away.  */
extern bool force_folding_builtin_constant_p;

extern bool is_builtin_fn (tree);
extern bool get_object_alignment_1 (tree, unsigned int *,
				    unsigned HOST_WIDE_INT *);
extern unsigned int get_object_alignment (tree);
extern bool get_pointer_alignment_1 (tree, unsigned int *,
				     unsigned HOST_WIDE_INT *);
extern unsigned int get_pointer_alignment (tree);
extern tree c_strlen (tree, int);
extern void expand_builtin_setjmp_setup (rtx, rtx);
extern void expand_builtin_setjmp_receiver (rtx);
extern tree mathfn_built_in (tree, enum built_in_function fn);
extern rtx builtin_strncpy_read_str (void *, HOST_WIDE_INT, enum machine_mode);
extern rtx builtin_memset_read_str (void *, HOST_WIDE_INT, enum machine_mode);
extern rtx expand_builtin_saveregs (void);
extern tree std_build_builtin_va_list (void);
extern tree std_fn_abi_va_list (tree);
extern tree std_canonical_va_list_type (tree);
extern void std_expand_builtin_va_start (tree, rtx);
extern void expand_builtin_trap (void);
extern rtx expand_builtin (tree, rtx, rtx, enum machine_mode, int);
extern enum built_in_function builtin_mathfn_code (const_tree);
extern tree fold_builtin_expect (location_t, tree, tree, tree);
extern tree fold_builtin_strcpy (location_t, tree, tree, tree, tree);
extern tree fold_builtin_strncpy (location_t, tree, tree, tree, tree, tree);
extern tree fold_fma (location_t, tree, tree, tree, tree);
extern bool avoid_folding_inline_builtin (tree);
extern tree fold_call_expr (location_t, tree, bool);
extern tree fold_builtin_call_array (location_t, tree, tree, int, tree *);
extern bool validate_gimple_arglist (const_gimple, ...);
extern rtx default_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
extern tree fold_builtin_strcat (location_t, tree, tree, tree);
extern tree fold_builtin_fputs (location_t, tree, tree, bool, bool, tree);
extern bool fold_builtin_next_arg (tree, bool);
extern tree fold_builtin_memory_chk (location_t, tree, tree, tree, tree, tree,
				     tree, bool, enum built_in_function);
extern tree fold_builtin_stxcpy_chk (location_t, tree, tree, tree, tree, tree,
				     bool, enum built_in_function);
extern tree fold_builtin_stxncpy_chk (location_t, tree, tree, tree, tree, tree,
				      bool, enum built_in_function);
extern tree gimple_fold_builtin_snprintf_chk (gimple, tree,
					      enum built_in_function);
extern tree do_mpc_arg2 (tree, tree, tree, int, int (*)(mpc_ptr, mpc_srcptr, mpc_srcptr, mpc_rnd_t));
extern tree fold_call_stmt (gimple, bool);
extern void set_builtin_user_assembler_name (tree decl, const char *asmspec);
extern bool is_simple_builtin (tree);
extern bool is_inexpensive_builtin (tree);

#endif
