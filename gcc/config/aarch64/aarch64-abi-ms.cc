/* Windows specific ABI for AArch64 architecture.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "regs.h"
#include "function-abi.h"
#include "builtins.h"
#include "aarch64-abi-ms-protos.h"

/* Iterate through the target-specific builtin types for va_list.
   IDX denotes the iterator, *PTREE is set to the result type of
   the va_list builtin, and *PNAME to its internal type.
   Returns zero if there is no element for this index, otherwise
   IDX should be increased upon the next call.
   Note, do not iterate a base builtin's name like __builtin_va_list.
   Used from c_common_nodes_and_builtins.  */

int
aarch64_ms_variadic_abi_enum_va_list (int idx, const char **pname, tree *ptree)
{
  switch (idx)
    {
    default:
      break;

    case 0:
      *ptree = ms_va_list_type_node;
      *pname = "__builtin_ms_va_list";
      return 1;
    }

  return 0;
}

/* This function returns the calling abi specific va_list type node.
   It returns  the FNDECL specific va_list type.  */

tree
aarch64_ms_variadic_abi_fn_abi_va_list (tree fndecl)
{
  gcc_assert (fndecl != NULL_TREE);

  arm_pcs pcs = (arm_pcs) fndecl_abi (fndecl).id ();
  if (pcs == ARM_PCS_MS_VARIADIC)
    return ms_va_list_type_node;

  return std_fn_abi_va_list (fndecl);
}

/* Returns the canonical va_list type specified by TYPE.
   If there is no valid TYPE provided, it return NULL_TREE.  */

tree
aarch64_ms_variadic_abi_canonical_va_list_type (tree type)
{
  if (lookup_attribute ("ms_abi va_list", TYPE_ATTRIBUTES (type)))
    return ms_va_list_type_node;

  return NULL_TREE;
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

int
aarch64_arg_partial_bytes (cumulative_args_t pcum_v,
			   const function_arg_info &arg ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);

  if (pcum->pcs_variant != ARM_PCS_MS_VARIADIC)
    return 0;

  /* Handle the case when argument is split between the last registers and
     the stack.  */
  if ((pcum->aapcs_reg != NULL_RTX) && (pcum->aapcs_stack_words != 0))
    return pcum->aapcs_stack_words * UNITS_PER_WORD;

  return 0;
}
