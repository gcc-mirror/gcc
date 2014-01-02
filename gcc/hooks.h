/* General-purpose hooks.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   In other words, you are welcome to use, share and improve this program.
   You are forbidden to forbid anyone else to use, share and improve
   what you give them.   Help stamp out software-hoarding!  */

#ifndef GCC_HOOKS_H
#define GCC_HOOKS_H

#include "machmode.h"
#include "double-int.h"

extern bool hook_bool_void_false (void);
extern bool hook_bool_void_true (void);
extern bool hook_bool_bool_false (bool);
extern bool hook_bool_bool_gcc_optionsp_false (bool, struct gcc_options *);
extern bool hook_bool_const_int_const_int_true (const int, const int);
extern bool hook_bool_mode_false (enum machine_mode);
extern bool hook_bool_mode_true (enum machine_mode);
extern bool hook_bool_mode_const_rtx_false (enum machine_mode, const_rtx);
extern bool hook_bool_mode_const_rtx_true (enum machine_mode, const_rtx);
extern bool hook_bool_mode_rtx_false (enum machine_mode, rtx);
extern bool hook_bool_mode_rtx_true (enum machine_mode, rtx);
extern bool hook_bool_const_rtx_const_rtx_true (const_rtx, const_rtx);
extern bool hook_bool_mode_uhwi_false (enum machine_mode,
				       unsigned HOST_WIDE_INT);
extern bool hook_bool_tree_false (tree);
extern bool hook_bool_const_tree_false (const_tree);
extern bool hook_bool_tree_true (tree);
extern bool hook_bool_const_tree_true (const_tree);
extern bool hook_bool_gsiptr_false (gimple_stmt_iterator *);
extern bool hook_bool_const_tree_hwi_hwi_const_tree_false (const_tree,
							   HOST_WIDE_INT,
							   HOST_WIDE_INT,
							   const_tree);
extern bool hook_bool_const_tree_hwi_hwi_const_tree_true (const_tree,
							  HOST_WIDE_INT,
							  HOST_WIDE_INT,
							  const_tree);
extern bool hook_bool_rtx_true (rtx);
extern bool hook_bool_rtx_false (rtx);
extern bool hook_bool_rtx_int_false (rtx, int);
extern bool hook_bool_uintp_uintp_false (unsigned int *, unsigned int *);
extern bool hook_bool_rtx_int_int_int_intp_bool_false (rtx, int, int, int,
						       int *, bool);
extern bool hook_bool_tree_tree_false (tree, tree);
extern bool hook_bool_tree_tree_true (tree, tree);
extern bool hook_bool_tree_bool_false (tree, bool);
extern bool hook_bool_dint_dint_uint_bool_true (double_int, double_int,
						unsigned int, bool);

extern void hook_void_void (void);
extern void hook_void_constcharptr (const char *);
extern void hook_void_rtx_int (rtx, int);
extern void hook_void_FILEptr_constcharptr (FILE *, const char *);
extern bool hook_bool_FILEptr_rtx_false (FILE *, rtx);
extern void hook_void_tree (tree);
extern void hook_void_tree_treeptr (tree, tree *);
extern void hook_void_int_int (int, int);
extern void hook_void_gcc_optionsp (struct gcc_options *);

extern int hook_int_uint_mode_1 (unsigned int, enum machine_mode);
extern int hook_int_const_tree_0 (const_tree);
extern int hook_int_const_tree_const_tree_1 (const_tree, const_tree);
extern int hook_int_rtx_0 (rtx);
extern int hook_int_rtx_1 (rtx);
extern int hook_int_rtx_unreachable (rtx);
extern int hook_int_rtx_bool_0 (rtx, bool);
extern int hook_int_rtx_mode_as_bool_0 (rtx, enum machine_mode, addr_space_t,
					bool);

extern tree hook_tree_const_tree_null (const_tree);

extern tree hook_tree_tree_tree_null (tree, tree);
extern tree hook_tree_tree_tree_tree_null (tree, tree, tree);
extern tree hook_tree_tree_tree_tree_3rd_identity (tree, tree, tree);
extern tree hook_tree_tree_int_treep_bool_null (tree, int, tree *, bool);

extern unsigned hook_uint_void_0 (void);
extern unsigned int hook_uint_mode_0 (enum machine_mode);

extern bool default_can_output_mi_thunk_no_vcall (const_tree, HOST_WIDE_INT,
						  HOST_WIDE_INT, const_tree);

extern rtx hook_rtx_rtx_identity (rtx);
extern rtx hook_rtx_rtx_null (rtx);
extern rtx hook_rtx_tree_int_null (tree, int);

extern const char *hook_constcharptr_void_null (void);
extern const char *hook_constcharptr_const_tree_null (const_tree);
extern const char *hook_constcharptr_const_rtx_null (const_rtx);
extern const char *hook_constcharptr_const_tree_const_tree_null (const_tree, const_tree);
extern const char *hook_constcharptr_int_const_tree_null (int, const_tree);
extern const char *hook_constcharptr_int_const_tree_const_tree_null (int, const_tree, const_tree);
#endif
