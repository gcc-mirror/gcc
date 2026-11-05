/* ACLE support for AArch64 NEON (function shapes)
   Copyright (C) 2026-2026 Free Software Foundation, Inc.

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

#define INCLUDE_ALGORITHM
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "basic-block.h"
#include "tree.h"
#include "function.h"
#include "gimple.h"
#include "rtl.h"
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "aarch64-acle-builtins.h"
#include "aarch64-sve-builtins-shapes.h"
#include "aarch64-builtins.h"

using namespace aarch64_acle;

/* Require that the parameter at PARAM_INDEX is in the range MIN to MAX
   (inclusive).  */
template <unsigned int PARAM_INDEX, int MIN, int MAX>
bool
range (function_checker &c)
{
  return c.require_immediate_range (PARAM_INDEX, MIN, MAX);
}

/* Require that the parameter at PARAM_INDEX is a valid lane index for the
   vector at PARAM_INDEX - 1.  */
template <unsigned int PARAM_INDEX>
bool
lane (function_checker &c)
{
  auto vec_param_index = PARAM_INDEX - 1;
  auto fn_type = TREE_TYPE (c.fndecl);
  auto args = TYPE_ARG_TYPES (fn_type);

  for (auto i = 0U; i < vec_param_index; i++)
    args = TREE_CHAIN (args);
  auto arg_type = TREE_VALUE (args);

  unsigned element_count;
  if (VECTOR_TYPE_P (arg_type))
    element_count = TYPE_VECTOR_SUBPARTS (arg_type).to_constant ();
  else
    {
      auto field = tuple_type_field (arg_type);
      auto array_type = TREE_TYPE (field);
      auto vec_type = TREE_TYPE (array_type);
      element_count = TYPE_VECTOR_SUBPARTS (vec_type).to_constant ();
    }
  return c.require_immediate_range (PARAM_INDEX, 0, element_count - 1);
}

/* Require that the parameter at PARAM_INDEX is a valid shift amount.  */
template <unsigned int PARAM_INDEX>
bool
shift (function_checker &c)
{
  auto bits = c.type_suffix (0).element_bits;
  return c.require_immediate_range (PARAM_INDEX, 0, bits - 1);
}

/* Require that the parameter at PARAM_INDEX is a valid right shift amount (1 to
   the element width).  */
template <unsigned int PARAM_INDEX>
bool
shift_right (function_checker &c)
{
  auto bits = c.type_suffix (0).element_bits;
  return c.require_immediate_range (PARAM_INDEX, 1, bits);
}

/* Require that the parameter at PARAM_INDEX is a valid right shift amount for
   a narrowing shift (whose result elements are half the width of the ones
   named by the type suffix).  */
template <unsigned int PARAM_INDEX>
bool
shift_right_narrow (function_checker &c)
{
  auto bits = c.type_suffix (0).element_bits / 2;
  return c.require_immediate_range (PARAM_INDEX, 1, bits);
}

/* Require that the parameter at PARAM_INDEX is a valid left shift amount for
   a widening shift.  Unlike the other left shifts, these may name the element
   width itself, which selects the SHLL form.  */
template <unsigned int PARAM_INDEX>
bool
shift_left_long (function_checker &c)
{
  auto bits = c.type_suffix (0).element_bits;
  return c.require_immediate_range (PARAM_INDEX, 0, bits);
}

/* A checker that always returns true.  */
bool
trivial (function_checker &)
{
  return true;
}

using check_fn_t = bool (*) (function_checker &);

/* All NEON functions are non-overloaded, so we don't need bespoke
  function shapes.  Instead, we can just use a single shape for all NEON
  functions, parameterised by a signature and optional validity checkers.  */
struct neon_shape : public function_shape
{
  constexpr neon_shape (const char *signature,
			check_fn_t check_fn1 = trivial,
			check_fn_t check_fn2 = trivial)
    : m_signature (signature),
      m_check_fn1 (check_fn1),
      m_check_fn2 (check_fn2)
  {}

  const char *m_signature;
  check_fn_t m_check_fn1;
  check_fn_t m_check_fn2;

  void build (function_builder &b,
	      const function_group_info &group) const override
  {
    aarch64_acle::build_all (b, this->m_signature, group, MODE_none);
  }

  bool check (function_checker &c) const override
  {
    /* Use strict `&` rather than short-circuiting `&&` so that we report errors
       from both checkers.  */
    return this->m_check_fn1 (c) & this->m_check_fn2 (c);
  }

  bool explicit_type_suffix_p (unsigned int) const override { return true; }
  tree resolve (function_resolver &) const override { gcc_unreachable (); }
};

namespace aarch64_acle {
  namespace shapes {
#define DEF_NEON_FUNCTION(NAME, TYPES, SHAPE_ARGS)			\
    static constexpr const neon_shape OBJ_NAME (NAME, TYPES) SHAPE_ARGS;\
    const aarch64_acle::function_shape *SHAPE_NAME (NAME, TYPES)	\
      = &OBJ_NAME (NAME, TYPES);
#include "aarch64-neon-builtins.def"
  }
}
