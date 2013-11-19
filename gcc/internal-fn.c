/* Internal functions.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.

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
#include "internal-fn.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "gimple.h"

/* The names of each internal function, indexed by function number.  */
const char *const internal_fn_name_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS) #CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  "<invalid-fn>"
};

/* The ECF_* flags of each internal function, indexed by function number.  */
const int internal_fn_flags_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS) FLAGS,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* ARRAY_TYPE is an array of vector modes.  Return the associated insn
   for load-lanes-style optab OPTAB.  The insn must exist.  */

static enum insn_code
get_multi_vector_move (tree array_type, convert_optab optab)
{
  enum insn_code icode;
  enum machine_mode imode;
  enum machine_mode vmode;

  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  imode = TYPE_MODE (array_type);
  vmode = TYPE_MODE (TREE_TYPE (array_type));

  icode = convert_optab_handler (optab, imode, vmode);
  gcc_assert (icode != CODE_FOR_nothing);
  return icode;
}

/* Expand LOAD_LANES call STMT.  */

static void
expand_LOAD_LANES (gimple stmt)
{
  struct expand_operand ops[2];
  tree type, lhs, rhs;
  rtx target, mem;

  lhs = gimple_call_lhs (stmt);
  rhs = gimple_call_arg (stmt, 0);
  type = TREE_TYPE (lhs);

  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  mem = expand_normal (rhs);

  gcc_assert (MEM_P (mem));
  PUT_MODE (mem, TYPE_MODE (type));

  create_output_operand (&ops[0], target, TYPE_MODE (type));
  create_fixed_operand (&ops[1], mem);
  expand_insn (get_multi_vector_move (type, vec_load_lanes_optab), 2, ops);
}

/* Expand STORE_LANES call STMT.  */

static void
expand_STORE_LANES (gimple stmt)
{
  struct expand_operand ops[2];
  tree type, lhs, rhs;
  rtx target, reg;

  lhs = gimple_call_lhs (stmt);
  rhs = gimple_call_arg (stmt, 0);
  type = TREE_TYPE (rhs);

  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  reg = expand_normal (rhs);

  gcc_assert (MEM_P (target));
  PUT_MODE (target, TYPE_MODE (type));

  create_fixed_operand (&ops[0], target);
  create_input_operand (&ops[1], reg, TYPE_MODE (type));
  expand_insn (get_multi_vector_move (type, vec_store_lanes_optab), 2, ops);
}

static void
expand_ANNOTATE (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LANE (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_VF (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LAST_LANE (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_NULL (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Routines to expand each internal function, indexed by function number.
   Each routine has the prototype:

       expand_<NAME> (gimple stmt)

   where STMT is the statement that performs the call. */
static void (*const internal_fn_expanders[]) (gimple) = {
#define DEF_INTERNAL_FN(CODE, FLAGS) expand_##CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* Expand STMT, which is a call to internal function FN.  */

void
expand_internal_call (gimple stmt)
{
  internal_fn_expanders[(int) gimple_call_internal_fn (stmt)] (stmt);
}
