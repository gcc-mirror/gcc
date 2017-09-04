/* General-purpose hooks.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.

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

/* This file contains generic hooks that can be used as defaults for
   target or language-dependent hook initializers.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hooks.h"

/* Generic hook that does absolutely zappo.  */
void
hook_void_void (void)
{
}

/* Generic hook that takes no arguments and returns false.  */
bool
hook_bool_void_false (void)
{
  return false;
}

/* Generic hook that takes no arguments and returns true.  */
bool
hook_bool_void_true (void)
{
  return true;
}

/* Generic hook that takes (bool) and returns false.  */
bool
hook_bool_bool_false (bool)
{
  return false;
}

/* Generic hook that takes (bool, struct gcc_options *) and returns false.  */
bool
hook_bool_bool_gcc_optionsp_false (bool, struct gcc_options *)
{
  return false;
}

/* Generic hook that takes const int, const int) and returns true.  */
bool hook_bool_const_int_const_int_true (const int, const int)
{
  return true;
}

/* Generic hook that takes (machine_mode) and returns false.  */
bool
hook_bool_mode_false (machine_mode)
{
  return false;
}

/* Generic hook that takes (machine_mode) and returns true.  */
bool
hook_bool_mode_true (machine_mode)
{
  return true;
}

/* Generic hook that takes (machine_mode, const_rtx) and returns false.  */
bool
hook_bool_mode_const_rtx_false (machine_mode, const_rtx)
{
  return false;
}

/* Generic hook that takes (machine_mode, const_rtx) and returns true.  */
bool
hook_bool_mode_const_rtx_true (machine_mode, const_rtx)
{
  return true;
}

/* Generic hook that takes (machine_mode, rtx) and returns false.  */
bool
hook_bool_mode_rtx_false (machine_mode, rtx)
{
  return false;
}

/* Generic hook that takes (machine_mode, rtx) and returns true.  */
bool
hook_bool_mode_rtx_true (machine_mode, rtx)
{
  return true;
}

/* Generic hook that takes (const rtx_insn *, const rtx_insn *) and returns true.  */
bool
hook_bool_const_rtx_insn_const_rtx_insn_true (const rtx_insn *,
					      const rtx_insn *)
{
  return true;
}

/* Generic hook that takes (machine_mode, unsigned HOST_WIDE_INT)
   and returns false.  */
bool
hook_bool_mode_uhwi_false (machine_mode, unsigned HOST_WIDE_INT)
{
  return false;
}

/* Generic hook that takes (unsigned int, machine_mode) and returns false.  */
bool
hook_bool_uint_mode_false (unsigned int, machine_mode)
{
  return false;
}

/* Generic hook that takes (FILE *, const char *) and does nothing.  */
void
hook_void_FILEptr_constcharptr (FILE *, const char *)
{
}

/* Generic hook that takes (FILE *, const char *, constr_tree *) and does
   nothing.  */
void
hook_void_FILEptr_constcharptr_const_tree (FILE *, const char *, const_tree)
{
}

/* Generic hook that takes (FILE *, rtx) and returns false.  */
bool
hook_bool_FILEptr_rtx_false (FILE *, rtx)
{
  return false;
}

/* Generic hook that takes (gimple_stmt_iterator *) and returns
   false.  */
bool
hook_bool_gsiptr_false (gimple_stmt_iterator *)
{
  return false;
}

/* Used for the TARGET_ASM_CAN_OUTPUT_MI_THUNK hook.  */
bool
hook_bool_const_tree_hwi_hwi_const_tree_false (const_tree, HOST_WIDE_INT,
					       HOST_WIDE_INT, const_tree)
{
  return false;
}

bool
hook_bool_const_tree_hwi_hwi_const_tree_true (const_tree, HOST_WIDE_INT,
					      HOST_WIDE_INT, const_tree)
{
  return true;
}

bool
default_can_output_mi_thunk_no_vcall (const_tree, HOST_WIDE_INT,
				      HOST_WIDE_INT c, const_tree)
{
  return c == 0;
}

int
hook_int_uint_mode_1 (unsigned int, machine_mode)
{
  return 1;
}

int
hook_int_const_tree_0 (const_tree)
{
  return 0;
}

/* ??? Used for comp_type_attributes, which ought to return bool.  */
int
hook_int_const_tree_const_tree_1 (const_tree, const_tree)
{
  return 1;
}

int
hook_int_rtx_0 (rtx)
{
  return 0;
}

int
hook_int_rtx_1 (rtx)
{
  return 1;
}

int
hook_int_rtx_insn_unreachable (rtx_insn *)
{
  gcc_unreachable ();
}

int
hook_int_rtx_bool_0 (rtx, bool)
{
  return 0;
}

int
hook_int_rtx_mode_as_bool_0 (rtx, machine_mode, addr_space_t, bool)
{
  return 0;
}

unsigned int
hook_uint_void_0 (void)
{
  return 0;
}

void
hook_void_tree (tree)
{
}

void
hook_void_rtx_tree (rtx, tree)
{
}

void
hook_void_constcharptr (const char *)
{
}

void
hook_void_tree_treeptr (tree, tree *)
{
}

void
hook_void_int_int (int, int)
{
}

bool
hook_bool_tree_false (tree)
{
  return false;
}

bool
hook_bool_const_tree_false (const_tree)
{
  return false;
}

bool
hook_bool_tree_true (tree)
{
  return true;
}

bool
hook_bool_const_tree_true (const_tree)
{
  return true;
}

bool
hook_bool_tree_tree_false (tree, tree)
{
  return false;
}

bool
hook_bool_tree_tree_true (tree, tree)
{
  return true;
}

bool
hook_bool_tree_bool_false (tree, bool)
{
  return false;
}

bool
hook_bool_rtx_insn_true (rtx_insn *)
{
  return true;
}

bool
hook_bool_rtx_false (rtx)
{
  return false;
}

bool
hook_bool_uintp_uintp_false (unsigned int *, unsigned int *)
{
  return false;
}

bool
hook_bool_rtx_mode_int_int_intp_bool_false (rtx, machine_mode, int, int,
					    int *, bool)
{
  return false;
}

bool
hook_bool_wint_wint_uint_bool_true (const widest_int &, const widest_int &,
				    unsigned int, bool)
{
  return true;
}

/* Generic hook that takes an rtx and returns it.  */
rtx
hook_rtx_rtx_identity (rtx x)
{
  return x;
}

/* Generic hook that takes an rtx and returns NULL_RTX.  */
rtx
hook_rtx_rtx_null (rtx)
{
  return NULL;
}

/* Generic hook that takes a tree and an int and returns NULL_RTX.  */
rtx
hook_rtx_tree_int_null (tree, int)
{
  return NULL;
}

/* Generic hook that takes a machine mode and returns an unsigned int 0.  */
unsigned int
hook_uint_mode_0 (machine_mode)
{
  return 0;
}

/* Generic hook that takes no arguments and returns a NULL const string.  */
const char *
hook_constcharptr_void_null (void)
{
  return NULL;
}

/* Generic hook that takes no arguments and returns a NULL string.  */
char *
hook_charptr_void_null (void)
{
  return NULL;
}

/* Generic hook that takes a tree and returns a NULL string.  */
const char *
hook_constcharptr_const_tree_null (const_tree)
{
  return NULL;
}

tree
hook_tree_tree_int_treep_bool_null (tree, int, tree *, bool)
{
  return NULL;
}

tree
hook_tree_tree_tree_null (tree, tree)
{
  return NULL;
}

tree
hook_tree_tree_tree_tree_null (tree, tree, tree)
{
  return NULL;
}

/* Generic hook that takes an rtx_insn *and returns a NULL string.  */
const char *
hook_constcharptr_const_rtx_insn_null (const rtx_insn *)
{
  return NULL;
}

const char *
hook_constcharptr_const_tree_const_tree_null (const_tree, const_tree)
{
  return NULL;
}

const char *
hook_constcharptr_int_const_tree_null (int, const_tree)
{
  return NULL;
}

const char *
hook_constcharptr_int_const_tree_const_tree_null (int, const_tree, const_tree)
{
  return NULL;
}

/* Generic hook that takes a const_tree and returns NULL_TREE.  */
tree
hook_tree_const_tree_null (const_tree)
{
  return NULL;
}

/* Generic hook that takes no arguments and returns a NULL_TREE.  */
tree
hook_tree_void_null (void)
{
  return NULL;
}

/* Generic hook that takes a rtx_insn * and an int and returns a bool.  */

bool
hook_bool_rtx_insn_int_false (rtx_insn *, int)
{
  return false;
}

/* Generic hook that takes a rtx_insn * and an int and returns void.  */

void
hook_void_rtx_insn_int (rtx_insn *, int)
{
}

/* Generic hook that takes a struct gcc_options * and returns void.  */

void
hook_void_gcc_optionsp (struct gcc_options *)
{
}

/* Generic hook that takes an unsigned int, an unsigned int pointer and
   returns false.  */

bool
hook_bool_uint_uintp_false (unsigned int, unsigned int *)
{
  return false;
}

/* Generic hook that takes a register class and returns false.  */
bool
hook_bool_reg_class_t_false (reg_class_t regclass ATTRIBUTE_UNUSED)
{
  return false;
}

