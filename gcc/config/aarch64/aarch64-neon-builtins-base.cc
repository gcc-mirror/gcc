/* ACLE support for AArch64 NEON (__ARM_FEATURE_SIMD intrinsics)
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
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "explow.h"
#include "tree-vector-builder.h"
#include "rtx-vector-builder.h"
#include "vec-perm-indices.h"
#include "aarch64-acle-builtins.h"
#include "aarch64-neon-builtins-base.h"
#include "aarch64-neon-builtins-functions.h"
#include "gimple-fold.h"

namespace aarch64_acle {
/* Build a cast expression, `(TYPE)EXPR`, if necessary to make an expression
   with type TYPE.  */
tree
build_cast (tree type, tree expr)
{
  return TREE_TYPE (expr) != type ? fold_build1 (VIEW_CONVERT_EXPR, type, expr)
				  : expr;
}

/* Convert a lane index to the correct index for the target endianness.
   For little-endian targets, this is a no-op.
   For big-endian targets, this is `len - index - 1`.  */
tree
convert_lane_index (poly_uint64 len, tree index)
{
  auto index_type = TREE_TYPE (index);
  return !BYTES_BIG_ENDIAN
	   ? index
	   : fold_build2 (MINUS_EXPR, index_type,
			  fold_build2 (MINUS_EXPR, index_type,
				       build_int_cst_type (index_type, len),
				       index),
			  build_one_cst (index_type));
}

/* Build a `VEC[INDEX]` expression.  */
tree
build_lane_get (tree vec, tree index)
{
  auto vec_type = TREE_TYPE (vec);
  auto vec_len = TYPE_VECTOR_SUBPARTS (vec_type);
  auto elem_type = TREE_TYPE (vec_type);
  auto elem_size = TYPE_SIZE (elem_type);
  auto offset = fold_build2 (MULT_EXPR, bitsizetype, elem_size,
			     convert_lane_index (vec_len, index));
  return fold_build3 (BIT_FIELD_REF, elem_type, vec, elem_size, offset);
}

/* Build a `VEC[INDEX] = ELEM;` statement.
   Returns an expression representing the updated VEC.  */
tree
build_lane_set (tree vec, tree index, tree elem)
{
  auto vec_type = TREE_TYPE (vec);
  auto vec_len = TYPE_VECTOR_SUBPARTS (vec_type);
  auto elem_type = TREE_TYPE (vec_type);
  auto elem_size = TYPE_SIZE (elem_type);
  auto offset = fold_build2 (MULT_EXPR, bitsizetype, elem_size,
			     convert_lane_index (vec_len, index));
  return fold_build3 (BIT_INSERT_EXPR, vec_type, vec, elem, offset);
}

/* Build an expression for a vector with all lanes set to `ELEM`.  */
tree
build_vec_dup (tree type, tree elem)
{
  return known_eq (TYPE_VECTOR_SUBPARTS (type), 1U)
	   ? fold_build1 (VIEW_CONVERT_EXPR, type, elem)
	   : fold_build1 (VEC_DUPLICATE_EXPR, type, elem);
}

/* Build a `TUPLE.val[INDEX]` expression.  */
tree
build_tuple_get (tree tuple, tree index)
{
  auto tuple_type = TREE_TYPE (tuple);
  auto field = tuple_type_field (tuple_type);
  auto array_type = TREE_TYPE (field);
  auto vec_type = TREE_TYPE (array_type);

  auto field_ref = fold_build3 (COMPONENT_REF, array_type, unshare_expr (tuple),
				field, NULL_TREE);
  auto array_ref
    = build4 (ARRAY_REF, vec_type, field_ref, index, NULL_TREE, NULL_TREE);

  return array_ref;
}

/* Build a `TUPLE.val[INDEX][LANE]` expression.  */
tree
build_tuple_get (gimple_folder &f, tree tuple, tree index, tree lane)
{
  auto vec = f.force_val (build_tuple_get (tuple, index));
  return build_lane_get (vec, lane);
}

/* Build a `TUPLE.val[INDEX] = ELEM;` statement.
   Returns an expression representing the updated tuple.  */
tree
build_tuple_set (gimple_folder &f, tree tuple, tree index, tree elem)
{
  f.assign (build_tuple_get (tuple, index), elem);
  return tuple;
}

/* Build a `TUPLE.val[INDEX][LANE] = ELEM;` statement.
   Returns an expression representing the updated tuple.  */
tree
build_tuple_set (gimple_folder &f, tree tuple, tree index, tree lane, tree elem)
{
  auto vec = f.force_val (build_tuple_get (tuple, index));
  vec = f.force_val (build_lane_set (vec, lane, elem));
  return build_tuple_set (f, tuple, index, vec);
}

/* Base class for all function expanders.
   At least one of `expand` or `fold` must be overriden by derived classes.  */
class gimple_function_base : public function_base
{
  rtx expand (function_expander &) const override { gcc_unreachable (); }
  gimple *fold (gimple_folder &) const override { gcc_unreachable (); }
};

template <insn_code insn> struct gimple_exact_insn : gimple_function_base
{
  gimple *fold (gimple_folder &) const override { return nullptr; }
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (insn);
  }
};

/* For intrinsics that map to a single GIMPLE expression with no argument
   preparation necessary.  */
class gimple_expr : public gimple_function_base
{
  tree_code m_int_code;
  tree_code m_float_code;
  tree_code m_poly_code;

public:
  constexpr gimple_expr (tree_code code)
    : m_int_code (code),
      m_float_code (code),
      m_poly_code (code)
    {}

  constexpr gimple_expr (tree_code int_code,
			 tree_code float_code,
			 tree_code poly_code)
    : m_int_code (int_code),
      m_float_code (float_code),
      m_poly_code (poly_code)
    {}

  gimple *fold (gimple_folder &f) const override
  {
    auto nargs = gimple_call_num_args (f.call);
    auto arg0 = nargs >= 1 ? gimple_call_arg (f.call, 0) : nullptr;
    auto arg1 = nargs >= 2 ? gimple_call_arg (f.call, 1) : nullptr;
    auto arg2 = nargs >= 3 ? gimple_call_arg (f.call, 2) : nullptr;

    auto type_class = f.type_suffix (0).tclass;
    switch (type_class)
      {
      case TYPE_signed:
      case TYPE_unsigned:
	return gimple_build_assign (f.lhs, m_int_code, arg0, arg1, arg2);
      case TYPE_float:
	return gimple_build_assign (f.lhs, m_float_code, arg0, arg1, arg2);
      case TYPE_poly:
	return gimple_build_assign (f.lhs, m_poly_code, arg0, arg1, arg2);
      default:
	gcc_unreachable ();
      }
  }
};

struct gimple_create : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto arg = gimple_call_arg (f.call, 0);
    return gimple_build_assign (f.lhs, fold_build1 (VIEW_CONVERT_EXPR,
						    TREE_TYPE (f.lhs), arg));
  }
};

struct gimple_combine : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto arg1 = gimple_call_arg (f.call, 0);
    auto arg2 = gimple_call_arg (f.call, 1);
    auto arg_type = TREE_TYPE (arg1);
    auto elem_type = TREE_TYPE (arg_type);
    auto ret_type = TREE_TYPE (f.lhs);

    if (known_eq (TYPE_VECTOR_SUBPARTS (arg_type), 1U))
      {
	arg1 = f.force_val (fold_build1 (VIEW_CONVERT_EXPR, elem_type, arg1));
	arg2 = f.force_val (fold_build1 (VIEW_CONVERT_EXPR, elem_type, arg2));
      }

    if (BYTES_BIG_ENDIAN)
      std::swap (arg1, arg2);

    return gimple_build_assign (f.lhs,
				build_constructor_va (ret_type, 2, NULL_TREE,
						      arg1, NULL_TREE, arg2));
  }
};

struct gimple_get_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto src = gimple_call_arg (f.call, 0);
    auto lane = gimple_call_arg (f.call, 1);
    return gimple_build_assign (f.lhs, build_lane_get (src, lane));
  }
};

template <bool high_half_p>
struct gimple_get_half : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto src = gimple_call_arg (f.call, 0);
    auto half
      = fold_build3 (BIT_FIELD_REF, TREE_TYPE (f.lhs), src, bitsize_int (64),
		     bitsize_int (high_half_p ^ BYTES_BIG_ENDIAN ? 64 : 0));
    return gimple_build_assign (f.lhs, half);
  }
};

struct gimple_set_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto elem = gimple_call_arg (f.call, 0);
    auto src = gimple_call_arg (f.call, 1);
    auto lane = gimple_call_arg (f.call, 2);
    return gimple_build_assign (f.lhs, build_lane_set (src, lane, elem));
  }
};

struct gimple_copy_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto vec1 = gimple_call_arg (f.call, 0);
    auto lane1 = gimple_call_arg (f.call, 1);
    auto vec2 = gimple_call_arg (f.call, 2);
    auto lane2 = gimple_call_arg (f.call, 3);

    // tmp1 = vec2[lane2]
    auto tmp1 = f.force_val (build_lane_get (vec2, lane2));

    // vec1[lane1] = tmp1
    // lhs = vec1
    return gimple_build_assign (f.lhs, build_lane_set (vec1, lane1, tmp1));
  }
};

struct gimple_dup : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto elem = gimple_call_arg (f.call, 0);
    auto ret_type = TREE_TYPE (f.lhs);
    return gimple_build_assign (f.lhs, build_vec_dup (ret_type, elem));
  }
};

// Vector creation
NEON_FUNCTION (vcreate,		gimple_create,)
NEON_FUNCTION (vcombine,	gimple_combine,)
NEON_FUNCTION (vdup_n,		gimple_dup,)
NEON_FUNCTION (vdupq_n,		gimple_dup,)
NEON_FUNCTION (vmov_n,		gimple_dup,)
NEON_FUNCTION (vmovq_n,		gimple_dup,)

struct gimple_dup_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto vec = gimple_call_arg (f.call, 0);
    auto lane = gimple_call_arg (f.call, 1);
    auto type = TREE_TYPE (f.lhs);

    // tmp1 = vec[lane]
    auto tmp1 = f.force_val (build_lane_get (vec, lane));

    // lhs = VEC_DUP (vec[lane])
    return gimple_build_assign (f.lhs, build_vec_dup (type, tmp1));
  }
};

/* For intrinsics that map to a GIMPLE expression with a `BIT_NOT` applied to
   the second argument.  */
class gimple_not_rhs : public gimple_function_base
{
  tree_code m_code;

public:
  constexpr gimple_not_rhs (tree_code code)
    : m_code (code)
    {}

  gimple *fold (gimple_folder &f) const override
  {
    auto lhs = gimple_call_arg (f.call, 0);
    auto rhs = gimple_call_arg (f.call, 1);
    auto type = TREE_TYPE (lhs);

    // tmp1 = ~rhs
    auto tmp1 = f.force_val (fold_build1 (BIT_NOT_EXPR, type, rhs));
    return gimple_build_assign (f.lhs, this->m_code, lhs, tmp1);
  }
};

/* BSL (d, n, m) == m ^ ((m ^ n) & d).  */
class gimple_bsl : public gimple_function_base
{
public:
  gimple *fold (gimple_folder &f) const override
  {
    auto d = gimple_call_arg (f.call, 0);
    auto n = gimple_call_arg (f.call, 1);
    auto m = gimple_call_arg (f.call, 2);

    auto uint_type = TREE_TYPE (d);
    auto ret_type = TREE_TYPE (f.lhs);

    // Cast to unsigned integer type if necessary.
    m = f.force_val (build_cast (uint_type, m));
    n = f.force_val (build_cast (uint_type, n));

    // tmp1 = m ^ n
    auto tmp1 = f.force_val (fold_build2 (BIT_XOR_EXPR, uint_type, m, n));

    // tmp2 = (m ^ n) & d
    auto tmp2 = f.force_val (fold_build2 (BIT_AND_EXPR, uint_type, tmp1, d));

    // tmp3 = m ^ ((m ^ n) & d)
    auto tmp3 = f.force_val (fold_build2 (BIT_XOR_EXPR, uint_type, m, tmp2));

    return gimple_build_assign (f.lhs, build_cast (ret_type, tmp3));
  }
};

/* SHA3 intrinsics expand to RTL at `-O0` because we want to emit one
   instruction for them even when optimizations are disabled.  */

/* EOR3 (n, m, a) = (n ^ m) ^ a.  */
class gimple_eor3 : public gimple_function_base
{
public:
  gimple *fold (gimple_folder &f) const override
  {
    if (optimize == 0)
      return nullptr;

    auto n = gimple_call_arg (f.call, 0);
    auto m = gimple_call_arg (f.call, 1);
    auto a = gimple_call_arg (f.call, 2);
    auto type = TREE_TYPE (f.lhs);

    // tmp1 = n ^ m
    auto tmp1 = f.force_val (fold_build2 (BIT_XOR_EXPR, type, n, m));

    // lhs = (n ^ m) ^ a
    return gimple_build_assign (f.lhs, BIT_XOR_EXPR, tmp1, a);
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_eor3q4 (e.args[0]->mode));
  }
};

/* BCAX (n, m, a) = n ^ (m & ~a).  */
class gimple_bcax : public gimple_function_base
{
public:
  gimple *fold (gimple_folder &f) const override
  {
    if (optimize == 0)
      return nullptr;

    auto n = gimple_call_arg (f.call, 0);
    auto m = gimple_call_arg (f.call, 1);
    auto a = gimple_call_arg (f.call, 2);
    auto arg_type = TREE_TYPE (n);

    // tmp1 = ~a
    auto tmp1 = f.force_val (fold_build1 (BIT_NOT_EXPR, arg_type, a));

    // tmp2 = m & ~a
    auto tmp2 = f.force_val (fold_build2 (BIT_AND_EXPR, arg_type, m, tmp1));

    // lhs = n ^ (m & ~a)
    return gimple_build_assign (f.lhs, BIT_XOR_EXPR, n, tmp2);
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_bcaxq4 (e.args[0]->mode));
  }
};

/* RAX1 (n, m) = n ^ rotl (m, splat (1)).  */
class gimple_rax1 : public gimple_function_base
{
public:
  gimple *fold (gimple_folder &f) const override
  {
    if (optimize == 0)
      return nullptr;

    auto n = gimple_call_arg (f.call, 0);
    auto m = gimple_call_arg (f.call, 1);
    auto arg_type = TREE_TYPE (n);

    // tmp1 = rotl (m, 1)
    auto tmp1 = f.force_val (
      fold_build2 (LROTATE_EXPR, arg_type, m, build_one_cst (arg_type)));

    // lhs = n ^ rotl (m, 1)
    return gimple_build_assign (f.lhs, BIT_XOR_EXPR, n, tmp1);
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (CODE_FOR_aarch64_rax1qv2di);
  }
};

/* XAR (n, m, imm6) = rotr (n ^ m, imm6).  */
class gimple_xar : public gimple_function_base
{
public:
  gimple *fold (gimple_folder &f) const override
  {
    if (optimize == 0)
      return nullptr;

    auto n = gimple_call_arg (f.call, 0);
    auto m = gimple_call_arg (f.call, 1);
    auto imm6 = gimple_call_arg (f.call, 2);
    auto type = TREE_TYPE (f.lhs);

    // tmp1 = n ^ m
    auto tmp1 = f.force_val (fold_build2 (BIT_XOR_EXPR, type, n, m));

    // lhs = rotr (n ^ m, imm6)
    return gimple_build_assign (f.lhs, RROTATE_EXPR, tmp1, imm6);
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (CODE_FOR_aarch64_xarqv2di);
  }
};

/* For intrinsics that map to a single GIMPLE IFN with no argument
   preparation necessary.  */
class gimple_ifn : public gimple_function_base
{
  internal_fn m_ifn;

public:
  constexpr gimple_ifn (internal_fn fn)
    : m_ifn (fn)
      {}

  gimple *fold (gimple_folder &f) const override
  {
    vec<tree> args{};
    for (unsigned i = 0; i < gimple_call_num_args (f.call); i++)
      args.safe_push (gimple_call_arg (f.call, i));

    auto call = gimple_build_call_internal_vec (this->m_ifn, args);
    gimple_call_set_lhs (call, f.lhs);
    return call;
  }
};

using mask_fn_t = tree (*) (gimple_folder &);

/* For intrinsics that map to a VEC_PERM (A, B, MASK) expression.
   A and B come the intrinsic's arguments, MASK is genereted by calling the
   provided MASK_FN with gimple_folder.  */
class gimple_permute : public gimple_function_base
{
  mask_fn_t m_mask_fn;

public:
  constexpr gimple_permute (mask_fn_t mask_fn)
    : m_mask_fn (mask_fn)
      {}

  gimple *fold (gimple_folder &f) const override
  {
    auto a = gimple_call_arg (f.call, 0);
    auto b
      = gimple_call_num_args (f.call) >= 2 ? gimple_call_arg (f.call, 1) : a;

    if (BYTES_BIG_ENDIAN)
      std::swap (a, b);

    auto mask = this->m_mask_fn (f);
    return gimple_build_assign (f.lhs, VEC_PERM_EXPR, a, b, mask);
  }
};

class gimple_permute_pair : public gimple_function_base
{
  mask_fn_t m_mask_fn_1;
  mask_fn_t m_mask_fn_2;

public:
  constexpr gimple_permute_pair (mask_fn_t mask_fn_1, mask_fn_t mask_fn_2)
    : m_mask_fn_1 (mask_fn_1), m_mask_fn_2 (mask_fn_2)
  {}

  gimple *fold (gimple_folder &f) const override
  {
    auto a = gimple_call_arg (f.call, 0);
    auto b = gimple_call_arg (f.call, 1);

    if (BYTES_BIG_ENDIAN)
      std::swap (a, b);

    auto arg_type = TREE_TYPE (a);
    gcc_assert (types_compatible_p (arg_type, TREE_TYPE (b)));

    auto tuple_type = TREE_TYPE (f.lhs);
    auto tuple = create_tmp_var (tuple_type);
    f.assign (tuple, build_clobber (tuple_type));
    for (auto i = 0; i < 2; i++)
      {
	auto mask = i == 0 ? this->m_mask_fn_1 (f) : this->m_mask_fn_2 (f);
	auto permuted
	  = f.force_val (fold_build3 (VEC_PERM_EXPR, arg_type, a, b, mask));
	build_tuple_set (f, tuple, size_int (i), permuted);
      }

    return gimple_build_assign (f.lhs, tuple);
  }
};

tree
ext_mask (gimple_folder &f)
{
  auto vec_type = TREE_TYPE (gimple_call_arg (f.call, 0));
  auto start = int_cst_value (gimple_call_arg (f.call, 2));
  auto len = TYPE_VECTOR_SUBPARTS (vec_type).to_constant ();
  auto mask_type = build_vector_type (sizetype, len);
  return build_vec_series (mask_type,
			   size_int (!BYTES_BIG_ENDIAN ? start : len - start),
			   size_int (1));
}

/* vrev16_u8  => {{1, 0}, {3, 2}, {5, 4}, {7, 6}}
   vrev16q_u8 => {{1, 0}, {3, 2}, {5, 4}, {7, 6},
		  {9, 8}, {11, 10}, {13, 12}, {15, 14}}

   vrev32_u8   => {{3, 2, 1, 0}, {7, 6, 5, 4}}
   vrev32q_u8  => {{3, 2, 1, 0}, {7, 6, 5, 4}, {11, 10, 9, 8}, {15, 14, 13, 12}}
   vrev32_u16  => {{1, 0}, {3, 2}}
   vrev32q_p16 => {{1, 0}, {3, 2}, {5, 4}, {7, 6}}

   rev64_u8   => {{7, 6, 5, 4, 3, 2, 1, 0}}
   rev64q_u8  => {{7, 6, 5, 4, 3, 2, 1, 0}, {15, 14, 13, 12, 11, 10, 9, 8}}
   rev64_u16  => {{3, 2, 1, 0}}
   rev64q_u16 => {{3, 2, 1, 0}, {7, 6, 5, 4}}
   rev64_u32  => {{1, 0}}
   rev64q_u32 => {{1, 0}, {3, 2}}
*/
template <unsigned int bits_per_word>
tree
rev_mask (gimple_folder &f)
{
  auto vec_type = TREE_TYPE (gimple_call_arg (f.call, 0));

  auto elem_type = TREE_TYPE (vec_type);
  auto len = TYPE_VECTOR_SUBPARTS (vec_type).to_constant ();
  auto mask_type = build_vector_type (sizetype, len);

  auto num_elems_per_word
    = bits_per_word / int_cst_value (TYPE_SIZE (elem_type));
  auto num_groups = len / num_elems_per_word;

  tree_vector_builder builder (mask_type, len, 1);

  for (auto i = 1U; i <= num_groups; i++)
    for (auto j = 1U; j <= num_elems_per_word; j++)
      builder.quick_push (size_int (i * num_elems_per_word - j));

  return builder.build ();
}

/* TRN1 ({a0, a1},	   {b0, b1})	     = {a0, b0}
					     = VEC_PERM (a, b, {0, 2})
   TRN1 ({a0, a1, a2, a3}, {b0, b1, b2, b3}) = {a0, b0, a2, b2}
					     = VEC_PERM (a, b, {0, 4, 2, 6})

   TRN2 ({a0, a1},	   {b0, b1})	     = {a1, b1}
					     = VEC_PERM (a, b, {1, 3})
   TRN2 ({a0, a1, a2, a3}, {b0, b1, b2, b3}) = {a1, b1, a3, b3}
					     = VEC_PERM (a, b, {1, 5, 3, 7})
*/
template <bool secondary_p>
tree
trn_mask (gimple_folder &f)
{
  auto vec_type = TREE_TYPE (gimple_call_arg (f.call, 0));
  auto len = TYPE_VECTOR_SUBPARTS (vec_type).to_constant ();
  auto mask_type = build_vector_type (sizetype, len);
  tree_vector_builder builder (mask_type, len, 1);

  for (auto i = 0U; i < len / 2; i++)
    {
      builder.quick_push (size_int (i * 2 + (secondary_p ^ BYTES_BIG_ENDIAN)));
      builder.quick_push (
	size_int (len + i * 2 + (secondary_p ^ BYTES_BIG_ENDIAN)));
    }

  return builder.build ();
}

/* UZP1 ({a0, a1},	   {b0, b1})	     = {a0, b0}
					     = VEC_PERM (a, b, {0, 2})
   UZP1 ({a0, a1, a2, a3}, {b0, b1, b2, b3}) = {a0, a2, b0, b2}
					     = VEC_PERM (a, b, {0, 2, 4, 6})

   UZP2 ({a0, a1},	   {b0, b1})	     = {a1, b1}
					     = VEC_PERM (a, b, {1, 3})
   UZP2 ({a0, a1, a2, a3}, {b0, b1, b2, b3}) = {a1, a3, b1, b3}
					     = VEC_PERM (a, b, {1, 3, 5, 7})
*/
template <bool secondary_p>
tree
uzp_mask (gimple_folder &f)
{
  auto vec_type = TREE_TYPE (gimple_call_arg (f.call, 0));
  auto len = TYPE_VECTOR_SUBPARTS (vec_type).to_constant ();
  auto mask_type = build_vector_type (sizetype, len);
  return build_vec_series (mask_type, size_int (secondary_p ^ BYTES_BIG_ENDIAN),
			   size_int (2));
}

/* ZIP1 ({a0, a1},	   {b0, b1})	     = {a0, b0}
					     = VEC_PERM (a, b, {0, 2})
   ZIP1 ({a0, a1, a2, a3}, {b0, b1, b2, b3}) = {a0, b0, a1, b1}
					     = VEC_PERM (a, b, {0, 4, 1, 5})

   ZIP2 ({a0, a1},	   {b0, b1})	     = {a1, b1}
					     = VEC_PERM (a, b, {1, 3})
   ZIP2 ({a0, a1, a2, a3}, {b0, b1, b2, b3}) = {a2, b2, a3, b3}
					     = VEC_PERM (a, b, {2, 6, 3, 7})
*/
template <bool secondary_p>
tree
zip_mask (gimple_folder &f)
{
  auto vec_type = TREE_TYPE (gimple_call_arg (f.call, 0));
  auto len = TYPE_VECTOR_SUBPARTS (vec_type).to_constant ();
  auto mask_type = build_vector_type (sizetype, len);

  auto start = (secondary_p ^ BYTES_BIG_ENDIAN) ? len / 2 : 0;
  tree_vector_builder builder (mask_type, len, 1);

  for (auto i = 0U; i < len / 2; i++)
    {
      builder.quick_push (size_int (start + i));
      builder.quick_push (size_int (len + start + i));
    }
  return builder.build ();
}

struct gimple_reinterpret : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto input = gimple_call_arg (f.call, 0);
    auto ret_type = TREE_TYPE (f.lhs);
    auto reinterpret = fold_build1 (VIEW_CONVERT_EXPR, ret_type, input);
    return gimple_build_assign (f.lhs, reinterpret);
  }
};

// Reinterpret
NEON_FUNCTION (vreinterpret, vreinterpretq, gimple_reinterpret,)

// Lane get/set
NEON_FUNCTION (vget_lane, vgetq_lane,   gimple_get_lane,)
NEON_FUNCTION (vget_low,                gimple_get_half<false>,)
NEON_FUNCTION (vget_high,               gimple_get_half<true>,)
NEON_FUNCTION (vset_lane, vsetq_lane,   gimple_set_lane,)
NEON_FUNCTION (vcopy_lane, vcopy_laneq, vcopyq_laneq, vcopyq_lane, gimple_copy_lane,)
NEON_FUNCTION (vdup_lane,  vdupq_lane,  vdup_laneq,   vdupq_laneq, gimple_dup_lane,)
NEON_FUNCTION (vdupb_lane, vdupb_laneq, vduph_lane,   vduph_laneq,
	       vdups_lane, vdups_laneq, vdupd_lane,   vdupd_laneq, gimple_get_lane,)

// Lanewise arithmetic
NEON_FUNCTION (vaddd, gimple_expr, (PLUS_EXPR))
NEON_FUNCTION (vadd,  gimple_expr, (PLUS_EXPR, PLUS_EXPR, BIT_XOR_EXPR))
NEON_FUNCTION (vaddq, gimple_expr, (PLUS_EXPR, PLUS_EXPR, BIT_XOR_EXPR))

// Bitwise operations
NEON_FUNCTION (vand,  vandq,  gimple_expr,    (BIT_AND_EXPR))
NEON_FUNCTION (vbic,  vbicq,  gimple_not_rhs, (BIT_AND_EXPR))
NEON_FUNCTION (vbsl,  vbslq,  gimple_bsl,)
NEON_FUNCTION (veor,  veorq,  gimple_expr,    (BIT_XOR_EXPR))
NEON_FUNCTION (vmvn,  vmvnq,  gimple_expr,    (BIT_NOT_EXPR))
NEON_FUNCTION (vorn,  vornq,  gimple_not_rhs, (BIT_IOR_EXPR))
NEON_FUNCTION (vorr,  vorrq,  gimple_expr,    (BIT_IOR_EXPR))
NEON_FUNCTION (vrbit, vrbitq, gimple_ifn,     (IFN_BITREVERSE))

// Bitwise operations (SHA3)
NEON_FUNCTION (vbcaxq, gimple_bcax,)
NEON_FUNCTION (veor3q, gimple_eor3,)
NEON_FUNCTION (vrax1q, gimple_rax1,)
NEON_FUNCTION (vxarq,  gimple_xar,)

// Bit counting operations
NEON_FUNCTION (vcls, vclsq, gimple_ifn, (IFN_CLRSB))
NEON_FUNCTION (vclz, vclzq, gimple_ifn, (IFN_CLZ))
NEON_FUNCTION (vcnt, vcntq, gimple_ifn, (IFN_POPCOUNT))

// Permutations
// Extract
NEON_FUNCTION (vext, vextq, gimple_permute, (ext_mask))

// Reverse
NEON_FUNCTION (vrev16, vrev16q, gimple_permute, (rev_mask<16>))
NEON_FUNCTION (vrev32, vrev32q, gimple_permute, (rev_mask<32>))
NEON_FUNCTION (vrev64, vrev64q, gimple_permute, (rev_mask<64>))

// Transpose
NEON_FUNCTION (vtrn1,  vtrn1q, gimple_permute,      (trn_mask<false>))
NEON_FUNCTION (vtrn2,  vtrn2q, gimple_permute,      (trn_mask<true>))
NEON_FUNCTION (vtrn,   vtrnq,  gimple_permute_pair, (trn_mask<false>, trn_mask<true>))

// Unzip
NEON_FUNCTION (vuzp1, vuzp1q, gimple_permute,      (uzp_mask<false>))
NEON_FUNCTION (vuzp2, vuzp2q, gimple_permute,      (uzp_mask<true>))
NEON_FUNCTION (vuzp,  vuzpq,  gimple_permute_pair, (uzp_mask<false>, uzp_mask<true>))

// Zip
NEON_FUNCTION (vzip1, vzip1q, gimple_permute,      (zip_mask<false>))
NEON_FUNCTION (vzip2, vzip2q, gimple_permute,      (zip_mask<true>))
NEON_FUNCTION (vzip,  vzipq,  gimple_permute_pair, (zip_mask<false>, zip_mask<true>))

// AES
NEON_FUNCTION (vaeseq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_aesev16qi>,)
NEON_FUNCTION (vaesdq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_aesdv16qi>,)
NEON_FUNCTION (vaesmcq,  gimple_exact_insn<CODE_FOR_aarch64_crypto_aesmcv16qi>,)
NEON_FUNCTION (vaesimcq, gimple_exact_insn<CODE_FOR_aarch64_crypto_aesimcv16qi>,)

// SHA1
NEON_FUNCTION (vsha1cq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_sha1cv4si>,)
NEON_FUNCTION (vsha1mq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_sha1mv4si>,)
NEON_FUNCTION (vsha1pq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_sha1pv4si>,)
NEON_FUNCTION (vsha1h,    gimple_exact_insn<CODE_FOR_aarch64_crypto_sha1hsi>,)
NEON_FUNCTION (vsha1su0q, gimple_exact_insn<CODE_FOR_aarch64_crypto_sha1su0v4si>,)
NEON_FUNCTION (vsha1su1q, gimple_exact_insn<CODE_FOR_aarch64_crypto_sha1su1v4si>,)

// SHA256
NEON_FUNCTION (vsha256hq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_sha256hv4si>,)
NEON_FUNCTION (vsha256h2q,  gimple_exact_insn<CODE_FOR_aarch64_crypto_sha256h2v4si>,)
NEON_FUNCTION (vsha256su0q, gimple_exact_insn<CODE_FOR_aarch64_crypto_sha256su0v4si>,)
NEON_FUNCTION (vsha256su1q, gimple_exact_insn<CODE_FOR_aarch64_crypto_sha256su1v4si>,)
}
