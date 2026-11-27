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
#include "aarch64-builtins.h"
#include "aarch64-acle-builtins.h"
#include "aarch64-neon-builtins-base.h"
#include "aarch64-neon-builtins-functions.h"
#include "aarch64-neon-builtins-shapes.h"
#include "gimple-fold.h"
#include "ssa.h"
#include "emit-rtl.h"

namespace aarch64_acle {
/* Build an asm volatile statement that prevents VALUE from being optimized
   away, even if it is not used.
   Equivalent to the following C code:
   asm volatile ("" : : "w" (VALUE));  */
gimple *
build_optimization_barrier (tree value)
{
  vec<tree, va_gc> *inputs = nullptr;
  vec_safe_push (inputs,
		 build_tree_list (build_tree_list (NULL_TREE,
						   build_string (1, "w")),
				  value));
  auto stmt = gimple_build_asm_vec ("", inputs, nullptr, nullptr, nullptr);
  gimple_asm_set_volatile (stmt, true);
  return stmt;
}

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

/* Build an extract of half of the 128-bit vector VEC.  HIGH_P selects the upper
   half. */
tree
build_vec_extract_half (tree vec, bool high_p)
{
  auto vec_type = TREE_TYPE (vec);
  gcc_assert (known_eq (GET_MODE_BITSIZE (TYPE_MODE (vec_type)), 128));
  auto ret_type
    = build_vector_type (TREE_TYPE (vec_type),
			 TYPE_VECTOR_SUBPARTS (vec_type).to_constant () / 2);
  return fold_build3 (BIT_FIELD_REF, ret_type, vec, bitsize_int (64),
		      bitsize_int (high_p ^ BYTES_BIG_ENDIAN ? 64 : 0));
}

/* Build an expression for a vector with all lanes set to `ELEM`.  */
tree
build_vec_dup (tree type, tree elem)
{
  return known_eq (TYPE_VECTOR_SUBPARTS (type), 1U)
	   ? fold_build1 (VIEW_CONVERT_EXPR, type, elem)
	   : fold_build1 (VEC_DUPLICATE_EXPR, type, elem);
}

/* Build `VAL >> AMOUNT` for the integer vector type TYPE, conforming to ACLE
   semantics.  AMOUNT is an integer constant.  */
tree
build_shift_right (tree type, tree val, tree amount)
{
  auto bits = element_precision (type);
  if (wi::ltu_p (wi::to_wide (amount), bits))
    return fold_build2 (RSHIFT_EXPR, type, val, amount);

  /* A logical (unsigned) shift by the element width shifts every bit out.  */
  if (TYPE_UNSIGNED (type))
    return build_zero_cst (type);

  /* An arithmetic (signed) shift by the element width leaves every bit equal to
     the sign bit which a shift by one less already gives.  */
  return fold_build2 (RSHIFT_EXPR, type, val,
		      build_int_cst (TREE_TYPE (amount), bits - 1));
}

/* Get the number of vectors in TUPLE, which can either be a type or an
   expression.
   For convenience, a vector is treated as a single-element tuple.  */
unsigned int
tuple_len (tree tuple)
{
  tree type = TYPE_P (tuple) ? tuple : TREE_TYPE (tuple);

  if (VECTOR_TYPE_P (type))
    return 1;

  auto field = tuple_type_field (type);
  auto array_type = TREE_TYPE (field);
  return int_cst_value (TYPE_MAX_VALUE (TYPE_DOMAIN (array_type))) + 1;
}

/* Build `TUPLE.val`.  */
tree
build_tuple_get_array (tree tuple)
{
  auto tuple_type = TREE_TYPE (tuple);
  auto field = tuple_type_field (tuple_type);
  auto array_type = TREE_TYPE (field);
  return fold_build3 (COMPONENT_REF, array_type, unshare_expr (tuple), field,
		      NULL_TREE);
}

/* Build a `TUPLE.val[INDEX]` expression.  */
tree
build_tuple_get (tree tuple, tree index)
{
  auto field_ref = build_tuple_get_array (tuple);
  auto array_type = TREE_TYPE (field_ref);
  auto vec_type = TREE_TYPE (array_type);
  return build4 (ARRAY_REF, vec_type, field_ref, index, NULL_TREE, NULL_TREE);
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

/* Check if TYPE is a vector type having one element, like int64x1_t (i.e. V1DI
   mode) or float64x1_t (i.e. V1DF mode).  */
static bool
one_element_vector_type_p (tree type)
{
  return VECTOR_TYPE_P (type) && known_eq (TYPE_VECTOR_SUBPARTS (type), 1U);
}

/* Build a call to the internal function IFN.
   Handles singleton vector arguments specially: they are cast to their
   inner element type before the function is called, and then the return value
   is cast back to the expected singleton vector type.  */
static tree
build_ifn_call (gimple_folder &f, internal_fn ifn, tree ret_type,
		const array_slice<const tree> args)
{
  unsigned i = 0, nargs = args.size ();
  auto call = gimple_build_call_internal_1 (ifn, nargs);
  for (tree arg : args)
    {
      if (one_element_vector_type_p (TREE_TYPE (arg)))
	arg = f.force_val (build_cast (TREE_TYPE (TREE_TYPE (arg)), arg));
      gimple_call_set_arg (call, i++, arg);
    }
  gimple_seq_add_stmt_without_update (&f.stmts, call);

  if (!one_element_vector_type_p (ret_type))
    {
      auto ret = build_var (ret_type);
      gimple_call_set_lhs (call, ret);
      return ret;
    }
  else
    {
      auto elem_type = TREE_TYPE (ret_type);
      auto ret = build_var (elem_type);
      gimple_call_set_lhs (call, ret);
      return build_vec_dup (ret_type, ret);
    }
}

/* Convenience overload to allow passing ARGS as a braced list without creating
    an intermediate array variable.  */
static tree
build_ifn_call (gimple_folder &f, internal_fn ifn, tree ret_type,
		const std::initializer_list<const tree> args)
{
  return build_ifn_call (f, ifn, ret_type,
			 array_slice<const tree> (args.begin (), args.size ()));
}

/* Base class for all function expanders.
   At least one of `expand` or `fold` must be overriden by derived classes.  */
class gimple_function_base : public function_base
{
public:
  rtx expand (function_expander &) const override { gcc_unreachable (); }
  gimple *fold (gimple_folder &) const override { gcc_unreachable (); }

  /* If the expression can raise an FP exception, use an optimization barrier
     to prevent it being removed even if the resulting value is not used.
     FIXME (Karl Meakin): This shouldn't be necessary, but not all
     optimizations obey `-ftrapping-math` at the moment.  */
  gimple *maybe_optimization_barrier (gimple_folder &f, tree ret) const
  {
    if (f.call_properties () & CP_RAISE_FP_EXCEPTIONS)
      {
	f.assign (f.lhs (), ret);
	return build_optimization_barrier (f.lhs ());
      }
    else
      return gimple_build_assign (f.lhs (), ret);
  }

  tree memory_scalar_type (const function_instance &f) const override
  {
    return aarch64_simd_types_trees[f.type_suffix (0).neon128_type].eltype;
  }
};

/* Base class for intrinsics that map to a single instruction.  Derived classes
   name the instruction by overriding `code_for`.

   M_MODE_ARG is the index of the argument whose mode selects the instruction,
   or -1 if the intrinsic goes through use_exact_insn.  If the argument has a
   one-element vector type, such as int64x1_t, there is no instruction for it,
   so `code_for` is passed the element mode instead and the arguments and the
   result are unwrapped to match.  This is what arm_neon.h did for these types
   before they moved to the pragma-based framework.  */
class gimple_insn_based_function : public gimple_function_base
{
  int m_mode_arg;

protected:
  /* Return the instruction to use for mode MODE.  */
  virtual insn_code code_for (function_expander &e, machine_mode mode) const
    = 0;

public:
  constexpr gimple_insn_based_function (int mode_arg) : m_mode_arg (mode_arg) {}

  gimple *fold (gimple_folder &) const override { return nullptr; }

  rtx expand (function_expander &e) const override
  {
    gcc_assert (m_mode_arg >= -1);
    if (m_mode_arg == -1)
      return e.use_exact_insn (code_for (e, VOIDmode));

    auto type = TREE_TYPE (CALL_EXPR_ARG (e.call_expr, m_mode_arg));

    /* Handle vector types.  */
    if (!one_element_vector_type_p (type))
      return e.use_exact_insn (code_for (e, TYPE_MODE (type)));

    /* Handle scalar types.  This is similar to the handling in gimple_ifn for
       V1DI and V1DF types.  */
    auto icode = code_for (e, TYPE_MODE (TREE_TYPE (type)));
    auto scalar_mode = GET_MODE_INNER (e.result_mode ());

    for (unsigned int i = 0; i < e.args.length (); i++)
      {
	auto arg_type = TREE_TYPE (CALL_EXPR_ARG (e.call_expr, i));
	if (one_element_vector_type_p (arg_type))
	  e.args[i] = force_lowpart_subreg (scalar_mode, e.args[i],
					    TYPE_MODE (arg_type));
      }

    auto res = e.use_exact_insn (icode);
    return force_lowpart_subreg (e.result_mode (), res, scalar_mode);
  }
};

/* For intrinsics that always map to instruction INSN.  */
template <insn_code INSN>
struct gimple_exact_insn : public gimple_insn_based_function
{
  constexpr gimple_exact_insn () : gimple_insn_based_function (-1) {}

  insn_code code_for (function_expander &, machine_mode) const override
  {
    return INSN;
  }
};

/* Wrapper for `aarch64_<sur>q<r>shl<mode>` as it names its iterator twice.  */
static insn_code
code_for_qshl (int unspec, machine_mode mode)
{
  return code_for_aarch64_qshl (unspec, unspec, mode);
}

/* Likewise for `aarch64_<sur>qshl<u>_n<mode>`.  */
static insn_code
code_for_qshl_n (int unspec, machine_mode mode)
{
  return code_for_aarch64_qshl_n (unspec, unspec, mode);
}

/* Likewise for `aarch64_<sur>s<lr>i_n<mode>`.  */
static insn_code
code_for_si_n (int unspec, machine_mode mode)
{
  return code_for_aarch64_si_n (unspec, unspec, mode);
}

/* Wrapper for `aarch64_<shrn_op><sra_op>shrn2_n<mode>` as it names two
   different code iterators.  AARCH64_VALID_SHRN_OP ties the shift to the
   saturating truncation so derive the former from the latter.  */
static insn_code
code_for_shrn2_n (rtx_code code, machine_mode mode)
{
  auto shift = code == SS_TRUNCATE ? ASHIFTRT : LSHIFTRT;
  return code_for_aarch64_shrn2_n (code, shift, mode);
}

/* For intrinsics whose instruction is selected by a mode through the
   code_for_* helpers that insn-opinit.h generates for `@`-prefixed
   pattterns.  */
class gimple_code_for_mode_function : public gimple_insn_based_function
{
  insn_code (*m_code_for) (machine_mode);

  insn_code code_for (function_expander &, machine_mode mode) const override
  {
    return this->m_code_for (mode);
  }

public:
  constexpr gimple_code_for_mode_function (insn_code (*code_for) (machine_mode),
					   int mode_arg = 0)
    : gimple_insn_based_function (mode_arg), m_code_for (code_for)
  {}
};

/* Like gimple_code_for_mode_function, but for a pattern whose name also
   contains an attribute of an iterator that distinguishes signed forms from
   unsigned ones, such as `<sra_op>` or `<sur>`.  SIGNED_VALUE and
   UNSIGNED_VALUE give the iterator value to use for signed and unsigned type
   suffixes respectively.

   T is the type that insn-opinit.h uses for the iterator: `rtx_code` for a
   code iterator and `int` for an int iterator.  */
template <typename T>
class gimple_iterator_function : public gimple_insn_based_function
{
  insn_code (*m_code_for) (T, machine_mode);
  T m_signed_value;
  T m_unsigned_value;

  insn_code code_for (function_expander &e, machine_mode mode) const override
  {
    return m_code_for (e.type_suffix (0).unsigned_p ? m_unsigned_value
						    : m_signed_value,
		       mode);
  }

public:
  constexpr gimple_iterator_function (insn_code (*code_for) (T, machine_mode),
				      T signed_value, T unsigned_value,
				      int mode_arg = 0)
    : gimple_insn_based_function (mode_arg), m_code_for (code_for),
      m_signed_value (signed_value), m_unsigned_value (unsigned_value)
  {}
};

using gimple_rtx_code_function = gimple_iterator_function<rtx_code>;
using gimple_unspec_based_function = gimple_iterator_function<int>;

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
	return gimple_build_assign (f.lhs (), m_int_code, arg0, arg1, arg2);
      case TYPE_float:
	return gimple_build_assign (f.lhs (), m_float_code, arg0, arg1, arg2);
      case TYPE_poly:
	return gimple_build_assign (f.lhs (), m_poly_code, arg0, arg1, arg2);
      default:
	gcc_unreachable ();
      }
  }
};

enum class arith_kind
{
  normal,
  dup,
  lane
};

/* Like gimple_expr above, but converts signed inputs to unsigned first, to
   avoid undefined behaviour on overflow.  */
template <arith_kind kind>
class gimple_arith_base : public gimple_function_base
{
  tree_code m_int_code;
  tree_code m_float_code;
  tree_code m_poly_code;

public:
  constexpr gimple_arith_base (tree_code code)
    : m_int_code (code), m_float_code (code), m_poly_code (code)
  {}

  constexpr gimple_arith_base (tree_code int_code, tree_code float_code,
			       tree_code poly_code)
    : m_int_code (int_code), m_float_code (float_code), m_poly_code (poly_code)
  {}

  gimple *fold (gimple_folder &f) const override
  {
    auto nargs = gimple_call_num_args (f.call);
    auto arg0 = nargs >= 1 ? gimple_call_arg (f.call, 0) : nullptr;
    auto arg1 = nargs >= 2 ? gimple_call_arg (f.call, 1) : nullptr;

    if (kind == arith_kind::dup)
      arg1 = f.force_val (build_vec_dup (TREE_TYPE (arg0), arg1));

    if (kind == arith_kind::lane)
      {
	auto lane_idx = gimple_call_arg (f.call, 2);
	arg1 = f.force_val (build_lane_get (arg1, lane_idx));
	if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
	  arg1 = f.force_val (build_vec_dup (TREE_TYPE (arg0), arg1));
      }

    auto build = [&] (auto code, auto type) {
      auto ret = nargs == 1 ? fold_build1 (code, type, arg0)
			    : fold_build2 (code, type, arg0, arg1);
      return f.force_val (ret);
    };

    tree ret;
    auto type_class = f.type_suffix (0).tclass;
    switch (type_class)
      {
      case TYPE_signed:
	{
	  auto signed_type = TREE_TYPE (arg0);
	  auto unsigned_type = unsigned_type_for (signed_type);
	  auto to_unsigned = [&] (auto arg) {
	    return arg ? f.force_val (build_cast (unsigned_type, arg))
		       : nullptr;
	  };
	  arg0 = to_unsigned (arg0);
	  arg1 = to_unsigned (arg1);
	  ret = build (m_int_code, unsigned_type);
	  ret = build_cast (signed_type, f.force_val (ret));
	  break;
	}
      case TYPE_unsigned:
	ret = build (m_int_code, TREE_TYPE (arg0));
	break;
      case TYPE_float:
	ret = build (m_float_code, TREE_TYPE (arg0));
	break;
      case TYPE_poly:
	ret = build (m_poly_code, TREE_TYPE (arg0));
	break;
      default:
	gcc_unreachable ();
      }

    return this->maybe_optimization_barrier (f, ret);
  }
};
using gimple_arith = gimple_arith_base<arith_kind::normal>;
using gimple_arith_dup = gimple_arith_base<arith_kind::dup>;
using gimple_arith_lane = gimple_arith_base<arith_kind::lane>;

struct gimple_create : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto arg = gimple_call_arg (f.call, 0);
    return gimple_build_assign (f.lhs (), fold_build1 (VIEW_CONVERT_EXPR,
						       f.ret_type, arg));
  }
};

/* Concatenate LOW_HALF and HIGH_HALF into a single vector.  */
static tree
build_combine (gimple_folder &f, tree low_half, tree high_half)
{
  auto arg_type = TREE_TYPE (low_half);
  auto elem_type = TREE_TYPE (arg_type);
  auto ret_type
    = build_vector_type (elem_type, TYPE_VECTOR_SUBPARTS (arg_type) * 2);

  if (one_element_vector_type_p (arg_type))
    {
      low_half = f.force_val (fold_build1 (VIEW_CONVERT_EXPR, elem_type, low_half));
      high_half = f.force_val (fold_build1 (VIEW_CONVERT_EXPR, elem_type, high_half));
    }

  if (BYTES_BIG_ENDIAN)
    std::swap (low_half, high_half);

  return build_constructor_va (ret_type, 2, NULL_TREE, low_half, NULL_TREE, high_half);
}

struct gimple_combine : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto arg1 = gimple_call_arg (f.call, 0);
    auto arg2 = gimple_call_arg (f.call, 1);
    return gimple_build_assign (f.lhs (), build_combine (f, arg1, arg2));
  }
};

struct gimple_get_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto src = gimple_call_arg (f.call, 0);
    auto lane = gimple_call_arg (f.call, 1);
    return gimple_build_assign (f.lhs (), build_lane_get (src, lane));
  }
};

template <bool high_half_p>
struct gimple_get_half : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto src = gimple_call_arg (f.call, 0);
    auto half = build_vec_extract_half (src, high_half_p);
    return gimple_build_assign (f.lhs (), half);
  }
};

struct gimple_set_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto elem = gimple_call_arg (f.call, 0);
    auto src = gimple_call_arg (f.call, 1);
    auto lane = gimple_call_arg (f.call, 2);
    return gimple_build_assign (f.lhs (), build_lane_set (src, lane, elem));
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
    return gimple_build_assign (f.lhs (), build_lane_set (vec1, lane1, tmp1));
  }
};

struct gimple_dup : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto elem = gimple_call_arg (f.call, 0);
    return gimple_build_assign (f.lhs (), build_vec_dup (f.ret_type, elem));
  }
};

template <bool high_p>
struct gimple_widen : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto arg = gimple_call_arg (f.call, 0);
    auto ret_type = TREE_TYPE (f.lhs ());

    if (high_p)
      arg = f.force_val (build_vec_extract_half (arg, true));

    return gimple_build_assign (f.lhs (),
				fold_build1 (CONVERT_EXPR, ret_type, arg));
  }
};

template <bool high_p>
struct gimple_narrow: public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto src = gimple_call_arg (f.call, 0);
    auto arg = gimple_call_arg (f.call, high_p);

    auto ret_type = TREE_TYPE (f.lhs ());
    auto src_type = TREE_TYPE (src);
    auto arg_type = TREE_TYPE (arg);
    auto trunc_type = high_p ? src_type : ret_type;

    if (TYPE_SIGN (arg_type) != TYPE_SIGN (ret_type))
      arg = f.force_val (
	fold_build2 (MAX_EXPR, arg_type, arg, build_zero_cst (arg_type)));

    auto truncated = f.force_val (fold_build1 (CONVERT_EXPR, trunc_type, arg));
    auto ret = high_p ? build_combine (f, src, truncated) : truncated;
    return gimple_build_assign (f.lhs (), ret);
  }
};

template <bool high_p>
struct gimple_narrow_saturate : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto src = gimple_call_arg (f.call, 0);
    auto arg = gimple_call_arg (f.call, high_p);

    auto ret_type = TREE_TYPE (f.lhs ());
    auto src_type = TREE_TYPE (src);
    auto arg_type = TREE_TYPE (arg);
    auto trunc_type = high_p ? src_type : ret_type;

    if (TYPE_SIGN (arg_type) != TYPE_SIGN (ret_type))
      arg = f.force_val (
	fold_build2 (MAX_EXPR, arg_type, arg, build_zero_cst (arg_type)));

    auto truncated
      = f.force_val (build_ifn_call (f, IFN_SAT_TRUNC, trunc_type, {arg}));
    auto ret = high_p ? build_combine (f, src, truncated) : truncated;
    return gimple_build_assign (f.lhs (), ret);
  }
};

struct gimple_dup_lane : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto vec = gimple_call_arg (f.call, 0);
    auto lane = gimple_call_arg (f.call, 1);

    // tmp1 = vec[lane]
    auto tmp1 = f.force_val (build_lane_get (vec, lane));

    // lhs = VEC_DUP (vec[lane])
    return gimple_build_assign (f.lhs (), build_vec_dup (f.ret_type, tmp1));
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
    return gimple_build_assign (f.lhs (), this->m_code, lhs, tmp1);
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

    // Cast to unsigned integer type if necessary.
    m = f.force_val (build_cast (uint_type, m));
    n = f.force_val (build_cast (uint_type, n));

    // tmp1 = m ^ n
    auto tmp1 = f.force_val (fold_build2 (BIT_XOR_EXPR, uint_type, m, n));

    // tmp2 = (m ^ n) & d
    auto tmp2 = f.force_val (fold_build2 (BIT_AND_EXPR, uint_type, tmp1, d));

    // tmp3 = m ^ ((m ^ n) & d)
    auto tmp3 = f.force_val (fold_build2 (BIT_XOR_EXPR, uint_type, m, tmp2));

    return gimple_build_assign (f.lhs (), build_cast (f.ret_type, tmp3));
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

    // tmp1 = n ^ m
    auto tmp1 = f.force_val (fold_build2 (BIT_XOR_EXPR, f.ret_type, n, m));

    // lhs = (n ^ m) ^ a
    return gimple_build_assign (f.lhs (), BIT_XOR_EXPR, tmp1, a);
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
    return gimple_build_assign (f.lhs (), BIT_XOR_EXPR, n, tmp2);
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
    return gimple_build_assign (f.lhs (), BIT_XOR_EXPR, n, tmp1);
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

    // tmp1 = n ^ m
    auto tmp1 = f.force_val (fold_build2 (BIT_XOR_EXPR, f.ret_type, n, m));

    // lhs = rotr (n ^ m, imm6)
    return gimple_build_assign (f.lhs (), RROTATE_EXPR, tmp1, imm6);
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
    auto_vec<tree> args{};
    for (unsigned i = 0; i < gimple_call_num_args (f.call); i++)
      {
	tree arg = gimple_call_arg (f.call, i);
	/* .CLZ and .POPCOUNT take an unsigned argument.  */
	if ((m_ifn == IFN_CLZ || m_ifn == IFN_POPCOUNT)
	    && !TYPE_UNSIGNED (TREE_TYPE (arg)))
	  arg = f.force_val (build_cast (unsigned_type_for (TREE_TYPE (arg)),
					 arg));
	args.safe_push (arg);
      }
    return gimple_build_assign (f.lhs (), build_ifn_call (f, this->m_ifn,
							  f.ret_type, args));
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
    return gimple_build_assign (f.lhs (), VEC_PERM_EXPR, a, b, mask);
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

    auto tuple = build_var (f.ret_type);
    f.assign (tuple, build_clobber (f.ret_type));
    for (auto i = 0; i < 2; i++)
      {
	auto mask = i == 0 ? this->m_mask_fn_1 (f) : this->m_mask_fn_2 (f);
	auto permuted
	  = f.force_val (fold_build3 (VEC_PERM_EXPR, arg_type, a, b, mask));
	build_tuple_set (f, tuple, size_int (i), permuted);
      }

    return gimple_build_assign (f.lhs (), tuple);
  }
};

/* Expand legacy NEON vtbl1-4 and vtbx1-4.
   N is the number of 64-bit vectors in the lookup table.  */
template <unsigned int N>
class gimple_table_lookup : public gimple_function_base
{
  static_assert (N >= 1 && N <= 4, "Invalid number of table vectors");
  bool m_is_tbx;

public:
  constexpr gimple_table_lookup (bool tbx = false)
    : m_is_tbx (tbx)
  {}

  gimple *fold (gimple_folder &) const override { return nullptr; }

  rtx expand (function_expander &e) const override
  {
    insn_code m_code = (m_is_tbx && (N == 2 || N == 4)
			? (N == 2 ? CODE_FOR_aarch64_qtbx1v8qi
				  : CODE_FOR_aarch64_qtbx2v8qi)
			: (N <= 2 ? CODE_FOR_aarch64_qtbl1v8qi
				  : CODE_FOR_aarch64_qtbl2v8qi));
    unsigned m_table_arg = m_is_tbx ? 1 : 0;
    rtx parts[N];

    /* vtbl1 and vtbx1 take a single V8QI vector.  The remaining
       intrinsics take a tuple of N V8QI vectors.  */
    if (N == 1)
      parts[0] = e.args[m_table_arg];
    else
      {
	machine_mode mode = GET_MODE (e.args[m_table_arg]);

	gcc_assert ((N == 2 && mode == V2x8QImode)
		    || (N == 3 && mode == V3x8QImode)
		    || (N == 4 && mode == V4x8QImode));

	rtx tuple = force_reg (mode, e.args[m_table_arg]);

	/* Extract the individual 64-bit table vectors.  */
	for (unsigned int i = 0; i < N; ++i)
	  parts[i] = simplify_gen_subreg (V8QImode, tuple,
					  mode, i * 8);
      }

    /* Construct the full hardware table.  N == 1 or 2 requires
       one 128-bit register; N == 3 or 4 requires two.  */
    rtx table = N > 2 ? gen_reg_rtx (V2x16QImode)
		      : gen_reg_rtx (V16QImode);

    /* Construct the first 128-bit table register.
       For N == 1, zero the unused upper eight bytes.  */
    rtx first = N > 2 ? simplify_gen_subreg (V16QImode, table,
					     V2x16QImode, 0)
		      : table;

    rtx upper = N >= 2 ? parts[1]
		       : CONST0_RTX (V8QImode);

    emit_insn (gen_aarch64_combinev8qi (first,
					 parts[0], upper));

    if (N > 2)
      {
	rtx second = simplify_gen_subreg (V16QImode, table,
					  V2x16QImode, 16);

	upper = N == 4 ? parts[3]
		       : CONST0_RTX (V8QImode);

	emit_insn (gen_aarch64_combinev8qi (second,
					     parts[2], upper));
      }

    /* Legacy vtbx1 and vtbx3 have tables of 8 and 24 bytes,
       respectively.  Hardware TBX accepts tables whose sizes
       are multiples of 16 bytes, so the padded bytes must
       not overwrite the original destination.  */
    if (m_is_tbx && (N == 1 || N == 3))
      {
	rtx indices = force_reg (V8QImode, e.args[2]);

	/* Perform the table lookup.  */
	rtx lookup = gen_reg_rtx (V8QImode);

	if (N == 1)
	  emit_insn (gen_aarch64_qtbl1v8qi (lookup, table,
					  indices));
	else
	  emit_insn (gen_aarch64_qtbl2v8qi (lookup, table,
					  indices));

	/* Generate a mask for indices within the logical table.
	   The maximum valid index is 7 for vtbx1 and 23 for vtbx3.  */
	rtx limit = gen_const_vec_duplicate (V8QImode,
					     GEN_INT (8 * N - 1));
	limit = force_reg (V8QImode, limit);

	rtx mask = gen_reg_rtx (V8QImode);

	/* mask[i] = 0xff if indices[i] <= 8 * N - 1.  */
	emit_insn (gen_aarch64_cmgeuv8qi (mask, limit, indices));

	/* Select lookup for valid indices, otherwise preserve the
	   original destination.  */
	rtx result = gen_reg_rtx (V8QImode);

	rtx fallback = force_reg (V8QImode, e.args[0]);

	emit_insn (gen_aarch64_simd_bslv8qi (result, mask,
					     lookup, fallback));
	return result;
      }

    /* All vtbl variants, and vtbx2/vtbx4, map directly onto
       the corresponding TBL or TBX instruction after packing.  */
    e.args[m_table_arg] = table;
    return e.use_exact_insn (m_code);
  }
};

template <bool TBX>
class gimple_qtbl : public gimple_function_base
{
public:
  gimple *fold (gimple_folder &) const override { return nullptr; }

  rtx expand (function_expander &e) const override
  {
    constexpr unsigned int table_arg = TBX ? 1 : 0;
    insn_code icode
      = TBX ? code_for_aarch64_qtbx (e.result_mode (),
				     GET_MODE (e.args[table_arg]))
	    : code_for_aarch64_qtbl (e.result_mode (),
				     GET_MODE (e.args[table_arg]));

    return e.use_exact_insn (icode);
  }
};

/* Expand NEON LUTI2 and LUTI4 intrinsics.
   BITS_PER_INDEX is the number of bits per lookup index.  */
template <unsigned int BITS_PER_INDEX>
class gimple_luti : public gimple_function_base
{
  static_assert (BITS_PER_INDEX == 2 || BITS_PER_INDEX == 4);

public:
  gimple *fold (gimple_folder &) const override { return nullptr; }

  rtx expand (function_expander &e) const override
  {
    rtx table = e.args[0];

    /* LUTI2 uses only the low 64 bits of the table.
       The 128-bit table forms are provided for convenience.  */
    if (BITS_PER_INDEX == 2
	&& known_eq (GET_MODE_BITSIZE (GET_MODE (table)), 128u))
      {
	machine_mode mode
	  = aarch64_v64_mode (GET_MODE_INNER (GET_MODE (table)))
	      .require ();

	table = gen_lowpart (mode, table);
      }

    /* Select the instruction based on the table and index modes.  */
    insn_code icode
      = code_for_aarch64_lut (GET_MODE (table),
			      GET_MODE (e.args[1]));

    gcc_assert (icode != CODE_FOR_nothing);

    e.add_output_operand (icode);
    e.add_input_operand (icode, table);
    e.add_input_operand (icode, e.args[1]);
    e.add_input_operand (icode, e.args[2]);
    e.add_integer_operand (BITS_PER_INDEX);

    return e.generate_insn (icode);
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
    auto reinterpret = fold_build1 (VIEW_CONVERT_EXPR, f.ret_type, input);
    return gimple_build_assign (f.lhs (), reinterpret);
  }
};

struct gimple_abs : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto arg = gimple_call_arg (f.call, 0);
    auto arg_type = TREE_TYPE (arg);

    if (!FLOAT_TYPE_P (arg_type))
      return gimple_build_assign (
	f.lhs (), build_cast (arg_type, f.force_val (fold_build1 (
					  ABSU_EXPR,
					  unsigned_type_for (arg_type), arg))));

    auto ret = fold_build1 (ABS_EXPR, arg_type, arg);
    return this->maybe_optimization_barrier (f, ret);
  }
};

/* Build a tree representing the absolute difference between LHS and RHS.
   For floating-point types, this is just `ABS (LHS - RHS)`.
   For integer types, it is `.ABD (LHS, RHS)`.  */
tree
build_abs_diff (gimple_folder &f, tree lhs, tree rhs)
{
  auto type = TREE_TYPE (lhs);

  if (FLOAT_TYPE_P (type))
    return fold_build1 (ABS_EXPR, type,
			f.force_val (fold_build2 (MINUS_EXPR, type, lhs, rhs)));

  return build_ifn_call (f, IFN_ABD, type, {lhs, rhs});
}

template<bool accumulate>
struct gimple_abs_diff : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    auto lhs = gimple_call_arg (f.call, accumulate ? 1 : 0);
    auto rhs = gimple_call_arg (f.call, accumulate ? 2 : 1);
    auto ret = build_abs_diff (f, lhs, rhs);
    if (accumulate)
      {
	auto acc = gimple_call_arg (f.call, 0);
	auto type = TREE_TYPE (acc);
	/* The accumulation wraps, so do it in the unsigned type to avoid
	   introducing signed overflow.  */
	auto uns_type = unsigned_type_for (type);
	ret = f.force_val (fold_build2 (PLUS_EXPR, uns_type,
					f.force_val (build_cast (uns_type, acc)),
					f.force_val (build_cast (uns_type, ret))));
	ret = build_cast (type, ret);
      }

    return this->maybe_optimization_barrier (f, ret);
  }
};

/* FP16 matrix-multiply accumulate (widening and non-widening).  */
class vmmlaq_impl : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_fmmlaq (e.result_mode ()));
  }
};

// vmla (acc, lhs, rhs) = acc + lhs * rhs
// vmls (acc, lhs, rhs) = acc - lhs * rhs
template <arith_kind kind, bool negate>
struct multiply_accumulate : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    tree acc = gimple_call_arg (f.call, 0);
    tree lhs = gimple_call_arg (f.call, 1);
    tree rhs = gimple_call_arg (f.call, 2);
    tree lane_index;

    switch (kind)
      {
      case arith_kind::normal:
	break;

      case arith_kind::dup:
	rhs = f.force_val (build_vec_dup (TREE_TYPE (lhs), rhs));
	break;

      case arith_kind::lane:
	lane_index = gimple_call_arg (f.call, 3);
	rhs = f.force_val (build_lane_get (rhs, lane_index));
	if (VECTOR_TYPE_P (f.ret_type))
	  rhs = f.force_val (build_vec_dup (TREE_TYPE (lhs), rhs));
	break;
      }

    tree_code code = negate ? MINUS_EXPR : PLUS_EXPR;
    if (FLOAT_TYPE_P (f.ret_type))
      {
	/* Use a PAREN_EXPR (equivalent to C's `__builtin_assoc_barrier`) to
	   prevent floating-point add and mul being transformed to an FMA.  */
	auto mul = f.force_val (fold_build2 (MULT_EXPR, f.ret_type, lhs, rhs));
	auto paren = f.force_val (fold_build1 (PAREN_EXPR, f.ret_type, mul));
	auto addsub = f.force_val (fold_build2 (code, f.ret_type, acc, paren));
	return this->maybe_optimization_barrier (f, addsub);
      }
    else if (!TYPE_UNSIGNED (f.ret_type))
      {
	/* Convert signed inputs to unsigned first, to avoid undefined behaviour
	   on overflow.  */
	auto signed_type = f.ret_type;
	auto unsigned_type = unsigned_type_for (signed_type);
	lhs = f.force_val (build_cast (unsigned_type, lhs));
	rhs = f.force_val (build_cast (unsigned_type, rhs));
	acc = f.force_val (build_cast (unsigned_type, acc));
	auto mul = f.force_val (fold_build2 (MULT_EXPR, unsigned_type, lhs, rhs));
	auto addsub = f.force_val (fold_build2 (code, unsigned_type, acc, mul));
	return gimple_build_assign (f.lhs (), build_cast (signed_type, addsub));
      }
    else
      {
	auto mul = f.force_val (fold_build2 (MULT_EXPR, f.ret_type, lhs, rhs));
	return gimple_build_assign (f.lhs (), code, acc, mul);
      }
  }
};
template <arith_kind kind> using gimple_mla = multiply_accumulate<kind, false>;
template <arith_kind kind> using gimple_mls = multiply_accumulate<kind, true>;

template <arith_kind kind, bool negate>
struct gimple_fused_mla : public gimple_function_base
{
  gimple *fold (gimple_folder &f) const override
  {
    tree acc = gimple_call_arg (f.call, 0);
    tree lhs = gimple_call_arg (f.call, 1);
    tree rhs = gimple_call_arg (f.call, 2);
    tree lane_index;

    switch (kind)
      {
      case arith_kind::normal:
	break;

      case arith_kind::dup:
	rhs = f.force_val (build_vec_dup (TREE_TYPE (lhs), rhs));
	break;

      case arith_kind::lane:
	lane_index = gimple_call_arg (f.call, 3);
	rhs = f.force_val (build_lane_get (rhs, lane_index));
	if (VECTOR_TYPE_P (f.ret_type))
	  rhs = f.force_val (build_vec_dup (TREE_TYPE (lhs), rhs));
	break;
      }

    auto ret = build_ifn_call (f, negate ? IFN_FNMA : IFN_FMA, f.ret_type,
			       {lhs, rhs, acc});
    return this->maybe_optimization_barrier (f, ret);
  }
};
template <arith_kind kind> using gimple_fma = gimple_fused_mla<kind, false>;
template <arith_kind kind> using gimple_fms = gimple_fused_mla<kind, true>;

template <bool min_p>
struct gimple_minmax : public gimple_function_base
{
  rtx expand (function_expander &e) const override
  {
    auto ret_mode = e.result_mode ();
    auto singleton_p = (ret_mode == V1DFmode);
    auto unspec = min_p ? UNSPEC_FMIN : UNSPEC_FMAX;

    if (singleton_p)
      {
	for (int i = 0; i < 2; i++)
	  e.args[i] = simplify_gen_subreg (DFmode, e.args[i], V1DFmode, 0);
	ret_mode = DFmode;
      }

    for (int i = 0; i < 2; i++)
      e.args[i] = force_reg (ret_mode, e.args[i]);

    auto ret = gen_reg_rtx (ret_mode);
    auto args = gen_rtvec_v (e.args.length (), e.args.address ());
    emit_insn (gen_rtx_SET (ret, gen_rtx_UNSPEC (ret_mode, args, unspec)));

    if (singleton_p)
      ret = simplify_gen_subreg (V1DFmode, ret, DFmode, 0);

    return ret;
  }

  gimple *fold (gimple_folder &f) const override
  {
    auto lhs = gimple_call_arg (f.call, 0);
    auto rhs = gimple_call_arg (f.call, 1);
    if (FLOAT_TYPE_P (f.ret_type))
      return nullptr;
    return gimple_build_assign (f.lhs (), min_p ? MIN_EXPR : MAX_EXPR, lhs,
				rhs);
  }
};

template <bool min_p>
struct gimple_reduce_minmax : public gimple_function_base
{
  rtx expand (function_expander &e) const override
  {
    auto arg_mode = e.args[0]->mode;
    auto unspec = min_p ? UNSPEC_FMINV : UNSPEC_FMAXV;
    auto icode = code_for_reduc_scal (unspec, arg_mode);

    gcc_assert (icode != CODE_FOR_nothing);
    return e.use_exact_insn (icode);
  }

  gimple *fold (gimple_folder &f) const override
  {
    auto arg = gimple_call_arg (f.call, 0);
    if (FLOAT_TYPE_P (f.ret_type))
      return nullptr;

    auto ifn = min_p ? IFN_REDUC_MIN : IFN_REDUC_MAX;
    return gimple_build_assign (f.lhs (),
				build_ifn_call (f, ifn, f.ret_type, {arg}));
  }
};

template <bool load_p>
struct gimple_load_store_multiple : public gimple_function_base
{
  unsigned int call_properties (const function_instance &) const override
  {
    return load_p ? CP_READ_MEMORY : CP_WRITE_MEMORY;
  }

  rtx expand (function_expander &e) const override
  {
    auto tuple_mode = load_p ? e.result_mode () : e.args[1]->mode;
    auto &ptr_arg = e.args[0];
    ptr_arg = gen_rtx_MEM (tuple_mode, e.convert_to_pmode (ptr_arg));
    auto icode = load_p ? code_for_aarch64_simd_load_multiple (tuple_mode)
			: code_for_aarch64_simd_store_multiple (tuple_mode);
    gcc_assert (icode != CODE_FOR_nothing);
    return e.use_exact_insn (icode);
  }

  gimple *fold (gimple_folder &f) const override
  {
    auto ptr = gimple_call_arg (f.call, 0);
    auto mem_type
      = load_p ? f.ret_type : TREE_TYPE (gimple_call_arg (f.call, 1));

    /* LD1x1 and LDR are just a normal load.  */
    if (load_p && !AGGREGATE_TYPE_P (mem_type))
      {
	if (VECTOR_TYPE_P (mem_type))
	  aarch64_record_vector_load_arg (ptr);
	return gimple_build_assign (f.lhs (),
				    aarch64_dereference (ptr, mem_type));
      }

    /* ST1x1 and STR are just a normal store.  */
    if (!load_p && !AGGREGATE_TYPE_P (mem_type))
      {
	auto tuple = gimple_call_arg (f.call, 1);
	return gimple_build_assign (aarch64_dereference (ptr, mem_type), tuple);
      }

    return nullptr;
  }
};

/* Shift by an immediate with an optional accumulator.  */
class gimple_shift_imm : public gimple_function_base
{
  /* LSHIFT_EXPR or RSHIFT_EXPR.  */
  tree_code m_code;

  /* PLUS_EXPR for vsra_n and ERROR_MARK for the non-accumulating forms.  */
  tree_code m_acc_code;

  /* The value to shift is operand 0, unless an accumulator comes first.  */
  int first_shift_arg () const { return m_acc_code != ERROR_MARK; }

public:
  constexpr gimple_shift_imm (tree_code code, tree_code acc_code = ERROR_MARK)
    : m_code (code), m_acc_code (acc_code)
  {}

  gimple *fold (gimple_folder &f) const override
  {
    int first = first_shift_arg ();
    auto val = gimple_call_arg (f.call, first);
    auto amount = gimple_call_arg (f.call, first + 1);

    auto shifted = m_code == LSHIFT_EXPR
		     ? fold_build2 (LSHIFT_EXPR, f.ret_type, val, amount)
		     : build_shift_right (f.ret_type, val, amount);

    /* Non-accumulating intrinsic.  */
    if (m_acc_code == ERROR_MARK)
      return gimple_build_assign (f.lhs (), shifted);

    /* Accumulating intrinsic.  */
    auto acc = gimple_call_arg (f.call, 0);
    shifted = f.force_val (shifted);

    /* Accumulate in an unsigned type so that signed overflow wraps as the
       ACLE requires.  */
    auto utype = f.ret_type;
    if (!TYPE_UNSIGNED (f.ret_type))
      {
	utype = unsigned_type_for (f.ret_type);
	acc = f.force_val (build_cast (utype, acc));
	shifted = f.force_val (build_cast (utype, shifted));
      }

    auto acc_expr = f.force_val (fold_build2 (m_acc_code, utype, acc, shifted));
    return gimple_build_assign (f.lhs (), build_cast (f.ret_type, acc_expr));
  }
};

/* Shift by a vector amount.  The amount is signed and a negative
   value shifts right, so this only folds to a GIMPLE shift when the amount is
   a constant whose magnitude is in range.  */
class gimple_shift_reg : public gimple_unspec_based_function
{
public:
  using gimple_unspec_based_function::gimple_unspec_based_function;

  gimple *fold (gimple_folder &f) const override
  {
    auto val = gimple_call_arg (f.call, 0);
    auto amount = gimple_call_arg (f.call, 1);

    /* Fold when the shift is a known constant value.  */
    auto cst = uniform_integer_cst_p (amount);
    if (!cst)
      return nullptr;

    auto wcst = wi::to_wide (cst);
    if (wi::geu_p (wi::abs (wcst), element_precision (val)))
      return nullptr;

    if (!wi::neg_p (wcst))
      return gimple_build_assign (f.lhs (), LSHIFT_EXPR, val, amount);

    /* A negative amount shifts right by its magnitude.  */
    auto rhs
      = build_uniform_cst (TREE_TYPE (amount),
			   wide_int_to_tree (TREE_TYPE (cst), wi::abs (wcst)));
    return gimple_build_assign (f.lhs (), RSHIFT_EXPR, val, rhs);
  }
};

template <bool load_p>
struct gimple_load_store_interleaved : public gimple_function_base
{
  unsigned int call_properties (const function_instance &) const override
  {
    return load_p ? CP_READ_MEMORY : CP_WRITE_MEMORY;
  }

  rtx expand (function_expander &e) const override
  {
    auto tuple_mode = load_p ? e.result_mode () : e.args[1]->mode;
    insn_code icode;

    auto &ptr_arg = e.args[0];
    ptr_arg = gen_rtx_MEM (tuple_mode, e.convert_to_pmode (ptr_arg));

    switch (tuple_mode)
      {
      case V2x1DImode: case V2x1DFmode:
      case V3x1DImode: case V3x1DFmode:
      case V4x1DImode: case V4x1DFmode:
	icode = load_p ? code_for_aarch64_simd_load_multiple (tuple_mode)
		       : code_for_aarch64_simd_store_multiple (tuple_mode);
	break;
      default:
	icode = load_p ? code_for_aarch64_load_interleaved (tuple_mode)
		       : code_for_aarch64_store_interleaved (tuple_mode);
	break;
      }

    gcc_assert (icode != CODE_FOR_nothing);
    return e.use_exact_insn (icode);
  }

  gimple *fold (gimple_folder &f) const override
  {
    auto ptr = gimple_call_arg (f.call, 0);
    auto tuple_type
      = load_p ? f.ret_type : TREE_TYPE (gimple_call_arg (f.call, 1));
    auto array_type = TREE_TYPE (tuple_type_field (tuple_type));
    auto vec_type = TREE_TYPE (array_type);

    if (BYTES_BIG_ENDIAN)
      return nullptr;

    if (one_element_vector_type_p (vec_type))
      return nullptr;

    auto deref = aarch64_dereference (ptr, array_type);
    if (load_p)
      {
	/* lhs = VIEW_CONVERT (.LOAD_LANES (*ptr)) */
	auto call = build_ifn_call (f, IFN_LOAD_LANES, array_type, {deref});
	return gimple_build_assign (f.lhs (), build_cast (tuple_type, call));
      }
    else
      {
	/* *ptr = .STORE_LANES (tuple.val) */
	auto tuple = gimple_call_arg (f.call, 1);
	auto array = build_tuple_get_array (tuple);
	auto call = gimple_build_call_internal (IFN_STORE_LANES, 1, array);
	gimple_call_set_lhs (call, deref);
	return call;
      }
  }
};

template <bool load_p>
struct gimple_load_store_lane : public gimple_function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return load_p ? CP_READ_MEMORY : CP_WRITE_MEMORY;
  }

  rtx expand (function_expander &e) const override
  {
    auto tuple_mode = load_p ? e.result_mode () : e.args[1]->mode;
    auto &ptr_arg = e.args[0];
    ptr_arg = gen_rtx_MEM (BLKmode, e.convert_to_pmode (ptr_arg));
    set_mem_size (ptr_arg, GET_MODE_SIZE (GET_MODE_INNER (tuple_mode))
			     * targetm.hard_regno_nregs (V0_REGNUM, tuple_mode));
    auto icode = load_p ? code_for_aarch64_simd_load_lane (tuple_mode)
			: code_for_aarch64_simd_store_lane (tuple_mode);
    gcc_assert (icode != CODE_FOR_nothing);
    return e.use_exact_insn (icode);
  }

  gimple *fold (gimple_folder &f) const override
  {
    auto ptr = gimple_call_arg (f.call, 0);
    auto tuple = gimple_call_arg (f.call, 1);
    auto index = gimple_call_arg (f.call, 2);

    auto tuple_type = TREE_TYPE (tuple);
    auto elem_type = this->memory_scalar_type (f);

    if (VECTOR_TYPE_P (tuple_type))
      {
	auto deref = aarch64_dereference (ptr, elem_type);
	if (load_p)
	  /* tuple[lane] = *ptr;  */
	  return gimple_build_assign (f.lhs (),
				      build_lane_set (tuple, index,
						      f.force_val (deref)));
	else
	  /* *ptr = tuple[lane];  */
	  return gimple_build_assign (deref, f.force_val (
					       build_lane_get (tuple, index)));
      }

    return nullptr;
  }
};

template <bool load_p>
struct gimple_load_store_lane_atomic : public gimple_function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return load_p ? CP_READ_MEMORY : CP_WRITE_MEMORY;
  }

  rtx expand (function_expander &e) const override
  {
    auto tuple_mode = load_p ? e.result_mode () : e.args[1]->mode;
    auto &ptr_arg = e.args[0];
    ptr_arg = e.convert_to_pmode (ptr_arg);
    auto icode = load_p ? code_for_aarch64_vec_ldap1_lane (tuple_mode)
			: code_for_aarch64_vec_stl1_lane (tuple_mode);
    gcc_assert (icode != CODE_FOR_nothing);
    return e.use_exact_insn (icode);
  }

  gimple *fold (gimple_folder &) const override { return nullptr; }
};

struct gimple_load_dup : public gimple_function_base
{
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  rtx expand (function_expander &e) const override
  {
    auto tuple_mode = e.result_mode ();
    auto &ptr_arg = e.args[0];
    ptr_arg = gen_rtx_MEM (BLKmode, e.convert_to_pmode (ptr_arg));
    set_mem_size (ptr_arg, GET_MODE_SIZE (GET_MODE_INNER (tuple_mode))
			     * targetm.hard_regno_nregs (V0_REGNUM, tuple_mode));
    auto icode = code_for_aarch64_simd_load_dup (tuple_mode);
    gcc_assert (icode != CODE_FOR_nothing);
    return e.use_exact_insn (icode);
  }

  gimple *fold (gimple_folder &f) const override
  {
    auto ptr = gimple_call_arg (f.call, 0);
    auto elem_type = this->memory_scalar_type (f);
    auto vec_count = tuple_len (f.ret_type);

    if (vec_count == 1)
      {
	/* lhs = dup (*ptr)  */
	auto load = f.force_val (aarch64_dereference (ptr, elem_type));
	return gimple_build_assign (f.lhs (),
				    build_vec_dup (f.ret_type, load));
      }

    return nullptr;
  }
};

enum class compare_kind
{
  normal,
  zero,
  abs,
  tst,
};

/* Perform a comparison, M_CODE, between the LHS and RHS.
  Arguments are prepared according to KIND:
  * compare_kind::normal:      LHS <=> RHS
  * compare_kind::zero:	       LHS <=> 0
  * compare_kind::abs:   abs (LHS) <=> abs (RHS)
  * compare_kind::tst: (LHS & RHS) <=> 0.  */
template <compare_kind KIND>
class gimple_compare_base : public gimple_function_base
{
  tree_code m_code;

public:
  constexpr gimple_compare_base (tree_code code) : m_code (code)
      {}

  gimple *fold (gimple_folder &f) const override
  {
    auto lhs = gimple_call_arg (f.call, 0);
    auto rhs = KIND == compare_kind::zero ? build_zero_cst (TREE_TYPE (lhs))
					  : gimple_call_arg (f.call, 1);

    auto ret_type = TREE_TYPE (f.lhs ());
    auto arg_type = TREE_TYPE (lhs);
    auto cmp_type = truth_type_for (arg_type);

    if (KIND == compare_kind::abs)
      {
	lhs = f.force_val (fold_build1 (ABS_EXPR, arg_type, lhs));
	rhs = f.force_val (fold_build1 (ABS_EXPR, arg_type, rhs));
      }

    tree cmp;
    if (KIND == compare_kind::tst)
      {
	cmp = f.force_val (fold_build2 (BIT_AND_EXPR, arg_type, lhs, rhs));
	cmp = f.force_val (
	  fold_build2 (this->m_code, cmp_type, cmp, build_zero_cst (arg_type)));
      }
    else
      cmp = f.force_val (fold_build2 (this->m_code, cmp_type, lhs, rhs));

    if (VECTOR_TYPE_P (ret_type))
      return gimple_build_assign (f.lhs (), build_cast (ret_type, cmp));
    else
      return gimple_build_assign (f.lhs (), COND_EXPR, cmp,
				  build_minus_one_cst (ret_type),
				  build_zero_cst (ret_type));
  }
};

using gimple_compare = gimple_compare_base<compare_kind::normal>;
using gimple_compare_abs = gimple_compare_base<compare_kind::abs>;
using gimple_compare_zero = gimple_compare_base<compare_kind::zero>;
using gimple_tst = gimple_compare_base<compare_kind::tst>;

/* Widening multiply with optional accumulator.  HIGH_P selects the high forms,
   usually spelled as `<name>_high`, eg. vmlsl_high.  */
template <bool high_p> class gimple_widen_mult : public gimple_function_base
{
public:
  /* How an intrinsic combines the widened product with an accumulator.  */
  enum accumulation
  {
    NO_ACCUM,
    PLUS_ACCUM,
    MINUS_ACCUM
  };

private:
  struct high_form
  {
    const char *name;
    const function_base *const *base;
    const function_shape *const *shape;
  };

  accumulation m_accum;

  /* Used to redirect the low forms to the high forms.  */
  high_form m_high;

  /* The first multiplicand is operand 0 for VMULL and 1 otherwise.  */
  int first_mult_arg () const { return m_accum != NO_ACCUM; }

  /* Return the VECTOR_CST corresponding to VAL if it is one or if it is an
     SSA_NAME which defines one.  */
  tree vector_constant_value (tree val) const
  {
    if (TREE_CODE (val) == SSA_NAME)
      {
	gimple *def = SSA_NAME_DEF_STMT (val);
	if (gimple_assign_single_p (def))
	  val = gimple_assign_rhs1 (def);
      }

    return TREE_CODE (val) == VECTOR_CST ? val : NULL_TREE;
  }

  /* Return true if VAL can be profitably passed to the high form.  */
  bool widenable_for_high_form_p (tree val) const
  {
    return aarch64_v128_highpart_src_reg (val) != NULL_TREE
	   || vector_constant_value (val) != NULL_TREE
	   || ssa_uniform_vector_p (val) != NULL_TREE;
  }

  /* Build and return the 128-bit vector that the high form should be given in
     place of VAL when widenable_for_high_form_p holds true.  */
  tree build_high_form_operand (gimple_folder &f, tree val) const
  {
    tree narrow_type = TREE_TYPE (val);
    gcc_assert (known_eq (GET_MODE_BITSIZE (TYPE_MODE (narrow_type)), 64));
    tree wide_type = build_vector_type (TREE_TYPE (narrow_type),
					TYPE_VECTOR_SUBPARTS (narrow_type) * 2);

    /* BIT_FIELD_REF of upper half.  Handle vreinterprets by view-converting to
       the actual type we want.  */
    if (tree obj = aarch64_v128_highpart_src_reg (val))
      return types_compatible_p (TREE_TYPE (obj), wide_type)
	       ? obj
	       : f.force_val (build1 (VIEW_CONVERT_EXPR, wide_type, obj));

    /* Constant.  */
    if (tree cst = vector_constant_value (val))
      return aarch64_build_vector_cst (cst, wide_type);

    /* Splat.  */
    if (tree elt = ssa_uniform_vector_p (val))
      return f.force_val (build_vec_dup (wide_type, elt));

    gcc_unreachable ();
  }

  /* Redirect the call in F to its high form if possible, otherwise return
     NULL.  */
  gimple *redirect_to_high_form (gimple_folder &f, unsigned int first,
				 const high_form &high) const
  {
    if (!high.name)
      return NULL;

    tree orig1 = gimple_call_arg (f.call, first);
    tree orig2 = gimple_call_arg (f.call, first + 1);

    if (!widenable_for_high_form_p (orig1) || !widenable_for_high_form_p (orig2)
	|| (!aarch64_v128_highpart_src_reg (orig1)
	    && !aarch64_v128_highpart_src_reg (orig2)))
      return NULL;

    function_instance instance (high.name, *high.base, *high.shape,
				f.mode_suffix_id, f.type_suffix_ids, GROUP_none,
				PRED_none, FPM_unused);
    gcall *call = f.redirect_call (instance);
    if (!call)
      return NULL;

    gimple_call_set_arg (call, first, build_high_form_operand (f, orig1));
    gimple_call_set_arg (call, first + 1, build_high_form_operand (f, orig2));
    return call;
  }

public:
  constexpr gimple_widen_mult (accumulation accum = NO_ACCUM,
			       high_form high = {})
    : m_accum (accum), m_high (high) {}

  gimple *fold (gimple_folder &f) const override
  {
    /* Folding is deferred until the function is in SSA form as
       SSA_NAME_DEF_STMT is not able to look through user-defined constant decls
       until then.  */
    if ((cfun->curr_properties & PROP_ssa) == 0)
      return nullptr;

    int first = first_mult_arg ();

    /* First try to optimize to the high form if both operands are extracts of
       upper halves of vectors.  */
    if (gimple *high_call = redirect_to_high_form (f, first, m_high))
      return high_call;

    /* Folding is not profitable if only one operand is widenable or if the
       resulting element size is >= 64 bits (which leads to scalarization). Punt
       on those cases.  */
    if (TYPE_PRECISION (TREE_TYPE (f.ret_type)) >= 64
	|| widenable_for_high_form_p (gimple_call_arg (f.call, first))
	|| widenable_for_high_form_p (gimple_call_arg (f.call, first + 1)))
      return NULL;

    /* Otherwise translate to GIMPLE as `(WIDE_TYPE) A * (WIDE_TYPE) B`.  */
    tree wide_type = f.ret_type;
    tree a = gimple_call_arg (f.call, first);
    tree b = gimple_call_arg (f.call, first + 1);
    gcc_assert (TYPE_PRECISION (TREE_TYPE (wide_type))
		== TYPE_PRECISION (TREE_TYPE (TREE_TYPE (a))) * 2);
    if (high_p)
      {
	a = f.force_val (build_vec_extract_half (a, true));
	b = f.force_val (build_vec_extract_half (b, true));
      }
    a = f.force_val (fold_build1 (CONVERT_EXPR, wide_type, a));
    b = f.force_val (fold_build1 (CONVERT_EXPR, wide_type, b));
    tree product = fold_build2 (MULT_EXPR, wide_type, a, b);

    /* Non-accumulating intrinsic.  */
    if (m_accum == NO_ACCUM)
      return gimple_build_assign (f.lhs (), product);

    /* Accumulating intrinsic.  */
    tree_code accum_code = m_accum == PLUS_ACCUM ? PLUS_EXPR : MINUS_EXPR;
    tree accumulator = gimple_call_arg (f.call, 0);
    product = f.force_val (product);

    /* Accumulate in an unsigned mode to match wrapping semantics.  */
    tree uwide_type = wide_type;
    if (!TYPE_UNSIGNED (TREE_TYPE (wide_type)))
      {
	uwide_type = unsigned_type_for (wide_type);
	accumulator = f.force_val (build_cast (uwide_type, accumulator));
	product = f.force_val (build_cast (uwide_type, product));
      }

    return gimple_build_assign (
      f.lhs (),
      build_cast (wide_type, f.force_val (fold_build2 (accum_code, uwide_type,
						       accumulator, product))));
  }

  rtx expand (function_expander &e) const override
  {
    auto mode = GET_MODE (e.args[first_mult_arg ()]);
    auto ext = e.type_suffix (0).unsigned_p ? ZERO_EXTEND : SIGN_EXTEND;

    insn_code icode;
    switch (m_accum)
      {
      case NO_ACCUM:
	icode = high_p
		  ? e.direct_optab_handler_for_sign (vec_widen_smult_hi_optab,
						     vec_widen_umult_hi_optab,
						     0, mode)
		  : code_for_aarch64_intrinsic_vec_mult_lo (ext, mode);
	break;
      case PLUS_ACCUM:
	icode = high_p ? code_for_aarch64_mlal_hi (ext, mode)
		       : code_for_aarch64_mlal (ext, mode);
	break;
      case MINUS_ACCUM:
	icode = high_p ? code_for_aarch64_mlsl_hi (ext, mode)
		       : code_for_aarch64_mlsl (ext, mode);
	break;
      default:
	gcc_unreachable ();
      }
    return e.use_exact_insn (icode);
  }
};

// Reinterpret
NEON_FUNCTION (vreinterpret, vreinterpretq, gimple_reinterpret,)

// Vector creation
NEON_FUNCTION (vcreate,		gimple_create,)
NEON_FUNCTION (vcombine,	gimple_combine,)
NEON_FUNCTION (vdup_n,		gimple_dup,)
NEON_FUNCTION (vdupq_n,		gimple_dup,)
NEON_FUNCTION (vmov_n,		gimple_dup,)
NEON_FUNCTION (vmovq_n,		gimple_dup,)

// Widening and narrowing integer conversions
NEON_FUNCTION (vmovl,		gimple_widen<false>,)
NEON_FUNCTION (vmovl_high,	gimple_widen<true>,)
NEON_FUNCTION (vmovn,		gimple_narrow<false>,)
NEON_FUNCTION (vmovn_high,	gimple_narrow<true>,)
NEON_FUNCTION (vqmovn,  vqmovnh,  vqmovns,  vqmovnd,
	       vqmovun, vqmovunh, vqmovuns, vqmovund, gimple_narrow_saturate<false>,)
NEON_FUNCTION (vqmovn_high, vqmovun_high, gimple_narrow_saturate<true>,)

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
NEON_FUNCTION (vadd, vaddq, vaddd, gimple_arith, (PLUS_EXPR, PLUS_EXPR, BIT_XOR_EXPR))
NEON_FUNCTION (vsub, vsubq, vsubd, gimple_arith, (MINUS_EXPR))
NEON_FUNCTION (vdiv, vdivq,	   gimple_arith, (RDIV_EXPR))
NEON_FUNCTION (vneg, vnegq, vnegd, gimple_arith, (NEGATE_EXPR))
NEON_FUNCTION (vabs, vabsq,	   vabsd, gimple_abs,)
NEON_FUNCTION (vabd, vabdq, vabds, vabdd, gimple_abs_diff<false>,)
NEON_FUNCTION (vaba, vabaq,		  gimple_abs_diff<true>,)

NEON_FUNCTION (vmul, vmulq,		gimple_arith, (MULT_EXPR))
NEON_FUNCTION (vmul_n, vmulq_n,		gimple_arith_dup, (MULT_EXPR))
NEON_FUNCTION (vmul_lane, vmul_laneq,
	       vmulq_lane, vmulq_laneq,
	       vmulh_lane, vmulh_laneq,
	       vmuls_lane, vmuls_laneq,
	       vmuld_lane, vmuld_laneq, gimple_arith_lane, (MULT_EXPR))

// Multiply-accumulate
NEON_FUNCTION (vmla, vmlaq, gimple_mla<arith_kind::normal>,)
NEON_FUNCTION (vmla_n, vmlaq_n, gimple_mla<arith_kind::dup>,)
NEON_FUNCTION (vmla_lane, vmla_laneq, vmlaq_lane, vmlaq_laneq,
	       gimple_mla<arith_kind::lane>,)

NEON_FUNCTION (vmls, vmlsq, gimple_mls<arith_kind::normal>,)
NEON_FUNCTION (vmls_n, vmlsq_n, gimple_mls<arith_kind::dup>,)
NEON_FUNCTION (vmls_lane, vmls_laneq, vmlsq_lane, vmlsq_laneq,
	       gimple_mls<arith_kind::lane>,)

// Multiply-accumulate (fused)
NEON_FUNCTION (vfma,   vfmaq,   gimple_fma<arith_kind::normal>,);
NEON_FUNCTION (vfma_n, vfmaq_n, gimple_fma<arith_kind::dup>,);
NEON_FUNCTION (vfmah_lane, vfmah_laneq,
	       vfmas_lane, vfmas_laneq,
	       vfmad_lane, vfmad_laneq,
	       vfma_lane,  vfma_laneq,
	       vfmaq_lane, vfmaq_laneq,
	       gimple_fma<arith_kind::lane>,);

NEON_FUNCTION (vfms,   vfmsq,   gimple_fms<arith_kind::normal>,);
NEON_FUNCTION (vfms_n, vfmsq_n, gimple_fms<arith_kind::dup>,);
NEON_FUNCTION (vfmsh_lane, vfmsh_laneq,
	       vfmss_lane, vfmss_laneq,
	       vfmsd_lane, vfmsd_laneq,
	       vfms_lane,  vfms_laneq,
	       vfmsq_lane, vfmsq_laneq,
	       gimple_fms<arith_kind::lane>,);

// Saturating arithmetic
NEON_FUNCTION (vqaddb, vqaddh, vqadds, vqaddd, vqadd, vqaddq, gimple_ifn, (IFN_SAT_ADD))
NEON_FUNCTION (vqsubb, vqsubh, vqsubs, vqsubd, vqsub, vqsubq, gimple_ifn, (IFN_SAT_SUB))

// Max/min
NEON_FUNCTION (vmax,   vmaxq,   gimple_minmax<false>,)
NEON_FUNCTION (vmin,   vminq,   gimple_minmax<true>,)
NEON_FUNCTION (vmaxnm, vmaxnmq, gimple_ifn,  (IFN_FMAX))
NEON_FUNCTION (vminnm, vminnmq, gimple_ifn,  (IFN_FMIN))

// Reductions
NEON_FUNCTION (vaddv,   vaddvq,   gimple_ifn, (IFN_REDUC_PLUS))
NEON_FUNCTION (vmaxv,   vmaxvq,   gimple_reduce_minmax<false>,)
NEON_FUNCTION (vminv,   vminvq,   gimple_reduce_minmax<true>,)
NEON_FUNCTION (vmaxnmv, vmaxnmvq, gimple_ifn, (IFN_REDUC_FMAX))
NEON_FUNCTION (vminnmv, vminnmvq, gimple_ifn, (IFN_REDUC_FMIN))

// Halving add
NEON_FUNCTION (vhadd,  vhaddq,  gimple_ifn, (IFN_AVG_FLOOR))
NEON_FUNCTION (vrhadd, vrhaddq, gimple_ifn, (IFN_AVG_CEIL))

// Square root
NEON_FUNCTION (vsqrt, vsqrtq, gimple_ifn, (IFN_SQRT))

// Rounding
NEON_FUNCTION (vrnd,  vrndq,          gimple_ifn, (IFN_TRUNC))
NEON_FUNCTION (vrnda, vrndaq,         gimple_ifn, (IFN_ROUND))
NEON_FUNCTION (vrndi, vrndiq,         gimple_ifn, (IFN_NEARBYINT))
NEON_FUNCTION (vrndm, vrndmq,         gimple_ifn, (IFN_FLOOR))
NEON_FUNCTION (vrndn, vrndnq, vrndns, gimple_ifn, (IFN_ROUNDEVEN))
NEON_FUNCTION (vrndp, vrndpq,         gimple_ifn, (IFN_CEIL))
NEON_FUNCTION (vrndx, vrndxq,         gimple_ifn, (IFN_RINT))

// Shifts by an immediate
NEON_FUNCTION (vshl_n, vshlq_n, vshld_n, gimple_shift_imm, (LSHIFT_EXPR))
NEON_FUNCTION (vshr_n, vshrq_n, vshrd_n, gimple_shift_imm, (RSHIFT_EXPR))
NEON_FUNCTION (vsra_n, vsraq_n, vsrad_n, gimple_shift_imm, (RSHIFT_EXPR, PLUS_EXPR))

// Shifts by a vector amount
NEON_FUNCTION (vshl,  vshlq,  vshld,  gimple_shift_reg,             (code_for_aarch64_shl, UNSPEC_SSHL, UNSPEC_USHL))
NEON_FUNCTION (vrshl, vrshlq, vrshld, gimple_unspec_based_function, (code_for_aarch64_shl, UNSPEC_SRSHL, UNSPEC_URSHL))
NEON_FUNCTION (vqshl,  vqshlq,  vqshlb,  vqshlh,  vqshls,  vqshld,
	       gimple_unspec_based_function, (code_for_qshl, UNSPEC_SQSHL, UNSPEC_UQSHL))
NEON_FUNCTION (vqrshl, vqrshlq, vqrshlb, vqrshlh, vqrshls, vqrshld,
	       gimple_unspec_based_function, (code_for_qshl, UNSPEC_SQRSHL, UNSPEC_UQRSHL))

// Rounding shifts by an immediate
NEON_FUNCTION (vrshr_n, vrshrq_n, vrshrd_n, gimple_rtx_code_function, (code_for_aarch64_rshr_n, ASHIFTRT, LSHIFTRT))
NEON_FUNCTION (vrsra_n, vrsraq_n, vrsrad_n, gimple_rtx_code_function, (code_for_aarch64_rsra_n, ASHIFTRT, LSHIFTRT))

// Saturating shifts by an immediate
NEON_FUNCTION (vqshl_n,  vqshlq_n,  vqshlb_n,  vqshlh_n,  vqshls_n,  vqshld_n,
	       gimple_unspec_based_function, (code_for_qshl_n, UNSPEC_SQSHL, UNSPEC_UQSHL))
NEON_FUNCTION (vqshlu_n, vqshluq_n, vqshlub_n, vqshluh_n, vqshlus_n, vqshlud_n,
	       gimple_unspec_based_function, (code_for_qshl_n, UNSPEC_SQSHLU, UNSPEC_SQSHLU))

// Shift and insert
NEON_FUNCTION (vsli_n, vsliq_n, vslid_n, gimple_unspec_based_function, (code_for_si_n, UNSPEC_SSLI, UNSPEC_USLI))
NEON_FUNCTION (vsri_n, vsriq_n, vsrid_n, gimple_unspec_based_function, (code_for_si_n, UNSPEC_SSRI, UNSPEC_USRI))

// Narrowing shifts by an immediate
NEON_FUNCTION (vshrn_n,       gimple_rtx_code_function, (code_for_aarch64_shrn_n, TRUNCATE, TRUNCATE))
NEON_FUNCTION (vqshrn_n, vqshrnh_n, vqshrns_n, vqshrnd_n,
	       gimple_rtx_code_function, (code_for_aarch64_shrn_n, SS_TRUNCATE, US_TRUNCATE))
NEON_FUNCTION (vshrn_high_n,  gimple_rtx_code_function, (code_for_shrn2_n, TRUNCATE, TRUNCATE, 1))
NEON_FUNCTION (vqshrn_high_n, gimple_rtx_code_function, (code_for_shrn2_n, SS_TRUNCATE, US_TRUNCATE, 1))

NEON_FUNCTION (vrshrn_n,       gimple_rtx_code_function, (code_for_aarch64_rshrn_n, TRUNCATE, TRUNCATE))
NEON_FUNCTION (vqrshrn_n, vqrshrnh_n, vqrshrns_n, vqrshrnd_n,
	       gimple_rtx_code_function, (code_for_aarch64_rshrn_n, SS_TRUNCATE, US_TRUNCATE))
NEON_FUNCTION (vrshrn_high_n,  gimple_rtx_code_function, (code_for_aarch64_rshrn2_n, TRUNCATE, TRUNCATE, 1))
NEON_FUNCTION (vqrshrn_high_n, gimple_rtx_code_function, (code_for_aarch64_rshrn2_n, SS_TRUNCATE, US_TRUNCATE, 1))

NEON_FUNCTION (vqshrun_n, vqshrunh_n, vqshruns_n, vqshrund_n,
	       gimple_code_for_mode_function, (code_for_aarch64_sqshrun_n))
NEON_FUNCTION (vqrshrun_n, vqrshrunh_n, vqrshruns_n, vqrshrund_n,
	       gimple_code_for_mode_function, (code_for_aarch64_sqrshrun_n))
NEON_FUNCTION (vqshrun_high_n,  gimple_code_for_mode_function, (code_for_aarch64_sqshrun2_n, 1))
NEON_FUNCTION (vqrshrun_high_n, gimple_code_for_mode_function, (code_for_aarch64_sqrshrun2_n, 1))

// Widening shifts by an immediate
NEON_FUNCTION (vshll_n,      gimple_unspec_based_function, (code_for_aarch64_shll_n, UNSPEC_SSHLL, UNSPEC_USHLL))
NEON_FUNCTION (vshll_high_n, gimple_unspec_based_function, (code_for_aarch64_shll2_n, UNSPEC_SSHLL, UNSPEC_USHLL))

// Widening multiply and multiply-accumulate
NEON_FUNCTION (vmull,      gimple_widen_mult<false>,
	       (gimple_widen_mult<false>::NO_ACCUM,
		{ "vmull_high", &functions::vmull_high,
		  &shapes::vmull_high_bhs_integer_shape }))
NEON_FUNCTION (vmull_high, gimple_widen_mult<true>,)
NEON_FUNCTION (vmlal,      gimple_widen_mult<false>,
	       (gimple_widen_mult<false>::PLUS_ACCUM,
		{ "vmlal_high", &functions::vmlal_high,
		  &shapes::vmlal_high_bhs_integer_shape }))
NEON_FUNCTION (vmlal_high, gimple_widen_mult<true>,
	       (gimple_widen_mult<true>::PLUS_ACCUM))
NEON_FUNCTION (vmlsl,      gimple_widen_mult<false>,
	       (gimple_widen_mult<false>::MINUS_ACCUM,
		{ "vmlsl_high", &functions::vmlsl_high,
		  &shapes::vmlsl_high_bhs_integer_shape }))
NEON_FUNCTION (vmlsl_high, gimple_widen_mult<true>,
	       (gimple_widen_mult<true>::MINUS_ACCUM))

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

// Comparisons
NEON_FUNCTION (vcalt, vcaltq, vcalts, vcaltd, gimple_compare_abs,  (LT_EXPR)) // abs (a) <  abs (b)
NEON_FUNCTION (vcale, vcaleq, vcales, vcaled, gimple_compare_abs,  (LE_EXPR)) // abs (a) <= abs (b)
NEON_FUNCTION (vcagt, vcagtq, vcagts, vcagtd, gimple_compare_abs,  (GT_EXPR)) // abs (a) >  abs (b)
NEON_FUNCTION (vcage, vcageq, vcages, vcaged, gimple_compare_abs,  (GE_EXPR)) // abs (a) >= abs (b)
NEON_FUNCTION (vceq,  vceqq,  vceqs,  vceqd,  gimple_compare,      (EQ_EXPR)) // a == b
NEON_FUNCTION (vclt,  vcltq,  vclts,  vcltd,  gimple_compare,      (LT_EXPR)) // a <  b
NEON_FUNCTION (vcle,  vcleq,  vcles,  vcled,  gimple_compare,      (LE_EXPR)) // a <= b
NEON_FUNCTION (vcgt,  vcgtq,  vcgts,  vcgtd,  gimple_compare,      (GT_EXPR)) // a >  b
NEON_FUNCTION (vcge,  vcgeq,  vcges,  vcged,  gimple_compare,      (GE_EXPR)) // a >= b
NEON_FUNCTION (vceqz, vceqzq, vceqzs, vceqzd, gimple_compare_zero, (EQ_EXPR)) // a == 0
NEON_FUNCTION (vcltz, vcltzq, vcltzs, vcltzd, gimple_compare_zero, (LT_EXPR)) // a <  0
NEON_FUNCTION (vclez, vclezq, vclezs, vclezd, gimple_compare_zero, (LE_EXPR)) // a <= 0
NEON_FUNCTION (vcgtz, vcgtzq, vcgtzs, vcgtzd, gimple_compare_zero, (GT_EXPR)) // a >  0
NEON_FUNCTION (vcgez, vcgezq, vcgezs, vcgezd, gimple_compare_zero, (GE_EXPR)) // a >= 0
NEON_FUNCTION (vtst,  vtstq,	      vtstd,  gimple_tst,	   (NE_EXPR)) // (a & b) != 0

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

// Table lookup
NEON_FUNCTION (vtbl1,   gimple_table_lookup<1>,)
NEON_FUNCTION (vtbl2,   gimple_table_lookup<2>,)
NEON_FUNCTION (vtbl3,   gimple_table_lookup<3>,)
NEON_FUNCTION (vtbl4,   gimple_table_lookup<4>,)
NEON_FUNCTION (vqtbl1,  vqtbl2,  vqtbl3,  vqtbl4,
	       vqtbl1q, vqtbl2q, vqtbl3q, vqtbl4q, gimple_qtbl<false>,)
NEON_FUNCTION (vtbx1,   gimple_table_lookup<1>, (true))
NEON_FUNCTION (vtbx2,   gimple_table_lookup<2>, (true))
NEON_FUNCTION (vtbx3,   gimple_table_lookup<3>, (true))
NEON_FUNCTION (vtbx4,   gimple_table_lookup<4>, (true))
NEON_FUNCTION (vqtbx1,  vqtbx2,  vqtbx3,  vqtbx4,
	       vqtbx1q, vqtbx2q, vqtbx3q, vqtbx4q, gimple_qtbl<true>,)

// Table lookup with 2-bit or 4-bit indices
NEON_FUNCTION (vluti2_lane,  vluti2_laneq,
	       vluti2q_lane, vluti2q_laneq, gimple_luti<2>,)
NEON_FUNCTION (vluti4q_lane, vluti4q_laneq, gimple_luti<4>,)

// Load/Store
// Load/Store without interleaving
NEON_FUNCTION (vld1, vld1q, vldrq, gimple_load_store_multiple<true>,)
NEON_FUNCTION (vst1, vst1q, vstrq, gimple_load_store_multiple<false>,)

// Load/Store interleaved tuples
NEON_FUNCTION (vld2, vld2q, vld3, vld3q, vld4, vld4q, gimple_load_store_interleaved<true>,)
NEON_FUNCTION (vst2, vst2q, vst3, vst3q, vst4, vst4q, gimple_load_store_interleaved<false>,)

// Load/Store lanes
NEON_FUNCTION (vld1_lane, vld1q_lane, vld2_lane, vld2q_lane,
	       vld3_lane, vld3q_lane, vld4_lane, vld4q_lane, gimple_load_store_lane<true>,)
NEON_FUNCTION (vst1_lane, vst1q_lane, vst2_lane, vst2q_lane,
	       vst3_lane, vst3q_lane, vst4_lane, vst4q_lane, gimple_load_store_lane<false>,)
NEON_FUNCTION (vldap1_lane, vldap1q_lane, gimple_load_store_lane_atomic<true>,)
NEON_FUNCTION (vstl1_lane,  vstl1q_lane,  gimple_load_store_lane_atomic<false>,)

// Load and broadcast
NEON_FUNCTION (vld1_dup, vld1q_dup, vld2_dup, vld2q_dup,
	       vld3_dup, vld3q_dup, vld4_dup, vld4q_dup, gimple_load_dup,)

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

// SHA512
NEON_FUNCTION (vsha512hq,   gimple_exact_insn<CODE_FOR_aarch64_crypto_sha512hqv2di>,)
NEON_FUNCTION (vsha512h2q,  gimple_exact_insn<CODE_FOR_aarch64_crypto_sha512h2qv2di>,)
NEON_FUNCTION (vsha512su0q, gimple_exact_insn<CODE_FOR_aarch64_crypto_sha512su0qv2di>,)
NEON_FUNCTION (vsha512su1q, gimple_exact_insn<CODE_FOR_aarch64_crypto_sha512su1qv2di>,)

// SM3
NEON_FUNCTION (vsm3ss1q,    gimple_exact_insn<CODE_FOR_aarch64_sm3ss1qv4si>,)
NEON_FUNCTION (vsm3tt1aq,   gimple_exact_insn<CODE_FOR_aarch64_sm3tt1aqv4si>,)
NEON_FUNCTION (vsm3tt1bq,   gimple_exact_insn<CODE_FOR_aarch64_sm3tt1bqv4si>,)
NEON_FUNCTION (vsm3tt2aq,   gimple_exact_insn<CODE_FOR_aarch64_sm3tt2aqv4si>,)
NEON_FUNCTION (vsm3tt2bq,   gimple_exact_insn<CODE_FOR_aarch64_sm3tt2bqv4si>,)
NEON_FUNCTION (vsm3partw1q, gimple_exact_insn<CODE_FOR_aarch64_sm3partw1qv4si>,)
NEON_FUNCTION (vsm3partw2q, gimple_exact_insn<CODE_FOR_aarch64_sm3partw2qv4si>,)

// SM4
NEON_FUNCTION (vsm4eq,    gimple_exact_insn<CODE_FOR_aarch64_sm4eqv4si>,)
NEON_FUNCTION (vsm4ekeyq, gimple_exact_insn<CODE_FOR_aarch64_sm4ekeyqv4si>,)

// FP16 matrix-multiply accumulate
NEON_FUNCTION (vmmlaq,  vmmlaq_impl,)
}
