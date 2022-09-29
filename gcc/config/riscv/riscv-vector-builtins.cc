/* Builtins implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2022 Free Software Foundation, Inc.
   Contributed by Ju-Zhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "recog.h"
#include "diagnostic.h"
#include "expr.h"
#include "function.h"
#include "fold-const.h"
#include "gimplify.h"
#include "explow.h"
#include "stor-layout.h"
#include "alias.h"
#include "langhooks.h"
#include "stringpool.h"
#include "attribs.h"
#include "targhooks.h"
#include "regs.h"
#include "riscv-vector-builtins.h"

namespace riscv_vector {

/* Information about each RVV type.  */
static CONSTEXPR const vector_type_info vector_types[] = {
#define DEF_RVV_TYPE(USER_NAME, NCHARS, ABI_NAME, ARGS...)    \
  {#USER_NAME, #ABI_NAME, "u" #NCHARS #ABI_NAME},
#include "riscv-vector-builtins.def"
};

/* The scalar type associated with each vector type.  */
static GTY (()) tree scalar_types[NUM_VECTOR_TYPES];
/* The machine mode associated with each vector type.  */
static GTY (()) machine_mode vector_modes[NUM_VECTOR_TYPES];
/* The RVV types, with their built-in
   "__rvv..._t" name.  Allow an index of NUM_VECTOR_TYPES, which always
   yields a null tree.  */
static GTY(()) tree abi_vector_types[NUM_VECTOR_TYPES + 1];

rvv_switcher::rvv_switcher ()
{
  /* Set have_regs_of_mode before targetm.init_builtins ().  */
  memcpy (m_old_have_regs_of_mode, have_regs_of_mode,
	  sizeof (have_regs_of_mode));
  for (int i = 0; i < NUM_MACHINE_MODES; ++i)
    if (riscv_v_ext_enabled_vector_mode_p ((machine_mode) i))
      have_regs_of_mode[i] = true;
}

rvv_switcher::~rvv_switcher ()
{
  /* Recover back have_regs_of_mode.  */
  memcpy (have_regs_of_mode, m_old_have_regs_of_mode,
	  sizeof (have_regs_of_mode));
}

/* Add type attributes to builtin type tree, currently only the mangled name. */
static void
add_vector_type_attribute (tree type, const char *mangled_name)
{
  tree mangled_name_tree = get_identifier (mangled_name);
  tree value = tree_cons (NULL_TREE, mangled_name_tree, NULL_TREE);
  TYPE_ATTRIBUTES (type) = tree_cons (get_identifier ("RVV type"), value,
				      TYPE_ATTRIBUTES (type));
}

/* Force TYPE to be a sizeless type.  */
static void
make_type_sizeless (tree type)
{
  TYPE_ATTRIBUTES (type) = tree_cons (get_identifier ("RVV sizeless type"),
				      NULL_TREE, TYPE_ATTRIBUTES (type));
}

/* Return true if TYPE is a sizeless type.  */
static bool
sizeless_type_p (const_tree type)
{
  if (type == error_mark_node)
    return NULL_TREE;
  return lookup_attribute ("RVV sizeless type", TYPE_ATTRIBUTES (type));
}

/* If TYPE is an ABI-defined RVV type, return its attribute descriptor,
   otherwise return null.  */
static tree
lookup_vector_type_attribute (const_tree type)
{
  if (type == error_mark_node)
    return NULL_TREE;
  return lookup_attribute ("RVV type", TYPE_ATTRIBUTES (type));
}

/* If TYPE is a built-in type defined by the RVV ABI, return the mangled name,
   otherwise return NULL.  */
const char *
mangle_builtin_type (const_tree type)
{
  if (TYPE_NAME (type) && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
    type = TREE_TYPE (TYPE_NAME (type));
  if (tree attr = lookup_vector_type_attribute (type))
    if (tree id = TREE_VALUE (chain_index (0, TREE_VALUE (attr))))
      return IDENTIFIER_POINTER (id);
  return NULL;
}

/* Register the built-in RVV ABI types, such as __rvv_int32m1_t.  */
static void
register_builtin_types ()
{
  /* int32_t/uint32_t defined as `long`/`unsigned long` in RV32,
     but intSI_type_node/unsigned_intSI_type_node is
     `int` and `unsigned int`, so use long_integer_type_node and
     long_unsigned_type_node here for type consistent.  */
  tree int32_type_node
    = TARGET_64BIT ? intSI_type_node : long_integer_type_node;
  tree unsigned_int32_type_node
    = TARGET_64BIT ? unsigned_intSI_type_node : long_unsigned_type_node;

  machine_mode mode;
#define DEF_RVV_TYPE(USER_NAME, NCHARS, ABI_NAME, SCALAR_TYPE, VECTOR_MODE,    \
		     VECTOR_MODE_MIN_VLEN_32)                                  \
  mode = TARGET_MIN_VLEN > 32 ? VECTOR_MODE##mode                              \
			      : VECTOR_MODE_MIN_VLEN_32##mode;                 \
  scalar_types[VECTOR_TYPE_##USER_NAME]                                        \
    = riscv_v_ext_enabled_vector_mode_p (mode) ? SCALAR_TYPE##_type_node       \
					       : NULL_TREE;                    \
  vector_modes[VECTOR_TYPE_##USER_NAME]                                        \
    = riscv_v_ext_enabled_vector_mode_p (mode) ? mode : VOIDmode;
#include "riscv-vector-builtins.def"

  for (unsigned int i = 0; i < NUM_VECTOR_TYPES; ++i)
    {
      tree eltype = scalar_types[i];
      mode = vector_modes[i];
      /* We disabled the datatypes according '-march'.  */
      if (!eltype)
	continue;

      tree vectype = build_vector_type_for_mode (eltype, mode);
      gcc_assert (
	VECTOR_MODE_P (TYPE_MODE (vectype)) && TYPE_MODE (vectype) == mode
	&& TYPE_MODE_RAW (vectype) == mode && TYPE_ALIGN (vectype) <= 128
	&& known_eq (tree_to_poly_uint64 (TYPE_SIZE (vectype)),
		     GET_MODE_BITSIZE (mode)));
      vectype = build_distinct_type_copy (vectype);
      gcc_assert (vectype == TYPE_MAIN_VARIANT (vectype));
      SET_TYPE_STRUCTURAL_EQUALITY (vectype);
      TYPE_ARTIFICIAL (vectype) = 1;
      TYPE_INDIVISIBLE_P (vectype) = 1;
      add_vector_type_attribute (vectype, vector_types[i].mangled_name);
      make_type_sizeless (vectype);
      abi_vector_types[i] = vectype;
      lang_hooks.types.register_builtin_type (vectype,
					      vector_types[i].abi_name);
    }
}

/* Initialize all compiler built-ins related to RVV that should be
   defined at start-up.  */
void
init_builtins ()
{
  rvv_switcher rvv;
  if (!TARGET_VECTOR)
    return;
  register_builtin_types ();
}

/* Implement TARGET_VERIFY_TYPE_CONTEXT for RVV types.  */
bool
verify_type_context (location_t loc, type_context_kind context, const_tree type,
		     bool silent_p)
{
  if (!sizeless_type_p (type))
    return true;

  switch (context)
    {
    case TCTX_SIZEOF:
    case TCTX_STATIC_STORAGE:
      if (!silent_p)
	error_at (loc, "RVV type %qT does not have a fixed size", type);

      return false;

    case TCTX_ALIGNOF:
      if (!silent_p)
	error_at (loc, "RVV type %qT does not have a defined alignment", type);

      return false;

    case TCTX_THREAD_STORAGE:
      if (!silent_p)
	error_at (loc,
		  "variables of type %qT cannot have thread-local"
		  " storage duration",
		  type);

      return false;

    case TCTX_POINTER_ARITH:
      if (!silent_p)
	error_at (loc, "arithmetic on pointer to RVV type %qT", type);

      return false;

    case TCTX_FIELD:
      if (silent_p)
	;
      else if (lang_GNU_CXX ())
	error_at (loc, "member variables cannot have RVV type %qT", type);
      else
	error_at (loc, "fields cannot have RVV type %qT", type);

      return false;

    case TCTX_ARRAY_ELEMENT:
      if (!silent_p)
	error_at (loc, "array elements cannot have RVV type %qT", type);

      return false;

    case TCTX_ALLOCATION:
      if (!silent_p)
	error_at (loc, "cannot allocate objects with RVV type %qT", type);

      return false;

    case TCTX_DEALLOCATION:
      if (!silent_p)
	error_at (loc, "cannot delete objects with RVV type %qT", type);

      return false;

    case TCTX_EXCEPTIONS:
      if (!silent_p)
	error_at (loc, "cannot throw or catch RVV type %qT", type);

      return false;

    case TCTX_CAPTURE_BY_COPY:
      if (!silent_p)
	error_at (loc, "capture by copy of RVV type %qT", type);

      return false;
    }

  gcc_unreachable ();
}

} // end namespace riscv_vector
