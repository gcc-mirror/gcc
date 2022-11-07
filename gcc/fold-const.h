/* Fold a constant sub-tree into a single node for C-compiler
   Copyright (C) 1987-2022 Free Software Foundation, Inc.

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

#ifndef GCC_FOLD_CONST_H
#define GCC_FOLD_CONST_H

/* Nonzero if we are folding constants inside an initializer or a C++
   manifestly-constant-evaluated context; zero otherwise.
   Should be used when folding in initializer enables additional
   optimizations.  */
extern int folding_initializer;
/* Nonzero if we are folding C++ manifestly-constant-evaluated context; zero
   otherwise.
   Should be used when certain constructs shouldn't be optimized
   during folding in that context.  */
extern bool folding_cxx_constexpr;

/* Convert between trees and native memory representation.  */
extern int native_encode_expr (const_tree, unsigned char *, int, int off = -1);
extern int native_encode_initializer (tree, unsigned char *, int,
				      int off = -1, unsigned char * = nullptr);
extern tree native_interpret_expr (tree, const unsigned char *, int);
extern tree native_interpret_real (tree, const unsigned char *, int);
extern bool can_native_interpret_type_p (tree);
extern tree native_interpret_aggregate (tree, const unsigned char *, int, int);
extern tree find_bitfield_repr_type (int, int);
extern void shift_bytes_in_array_left (unsigned char *, unsigned int,
				       unsigned int);
extern void shift_bytes_in_array_right (unsigned char *, unsigned int,
					unsigned int);

/* Fold constants as much as possible in an expression.
   Returns the simplified expression.
   Acts only on the top level of the expression;
   if the argument itself cannot be simplified, its
   subexpressions are not changed.  */

extern tree fold (tree);
extern tree fold_init (tree);
#define fold_unary(CODE,T1,T2)\
   fold_unary_loc (UNKNOWN_LOCATION, CODE, T1, T2)
extern tree fold_unary_loc (location_t, enum tree_code, tree, tree);
#define fold_unary_ignore_overflow(CODE,T1,T2)\
   fold_unary_ignore_overflow_loc (UNKNOWN_LOCATION, CODE, T1, T2)
extern tree fold_unary_ignore_overflow_loc (location_t, enum tree_code, tree, tree);
#define fold_binary(CODE,T1,T2,T3)\
   fold_binary_loc (UNKNOWN_LOCATION, CODE, T1, T2, T3)
extern tree fold_binary_loc (location_t, enum tree_code, tree, tree, tree);
#define fold_ternary(CODE,T1,T2,T3,T4)\
   fold_ternary_loc (UNKNOWN_LOCATION, CODE, T1, T2, T3, T4)
extern tree fold_ternary_loc (location_t, enum tree_code, tree, tree, tree, tree);
#define fold_build1(c,t1,t2)\
   fold_build1_loc (UNKNOWN_LOCATION, c, t1, t2 MEM_STAT_INFO)
extern tree fold_build1_loc (location_t, enum tree_code, tree,
			     tree CXX_MEM_STAT_INFO);
#define fold_build2(c,t1,t2,t3)\
   fold_build2_loc (UNKNOWN_LOCATION, c, t1, t2, t3 MEM_STAT_INFO)
extern tree fold_build2_loc (location_t, enum tree_code, tree, tree,
			     tree CXX_MEM_STAT_INFO);
#define fold_build3(c,t1,t2,t3,t4)\
   fold_build3_loc (UNKNOWN_LOCATION, c, t1, t2, t3, t4 MEM_STAT_INFO)
extern tree fold_build3_loc (location_t, enum tree_code, tree, tree, tree,
				  tree CXX_MEM_STAT_INFO);
extern tree fold_build1_initializer_loc (location_t, enum tree_code, tree, tree);
extern tree fold_build2_initializer_loc (location_t, enum tree_code, tree, tree, tree);
#define fold_build_call_array(T1,T2,N,T4)\
   fold_build_call_array_loc (UNKNOWN_LOCATION, T1, T2, N, T4)
extern tree fold_build_call_array_loc (location_t, tree, tree, int, tree *);
#define fold_build_call_array_initializer(T1,T2,N,T4)\
   fold_build_call_array_initializer_loc (UNKNOWN_LOCATION, T1, T2, N, T4)
extern tree fold_build_call_array_initializer_loc (location_t, tree, tree, int, tree *);
extern tree fold_binary_initializer_loc (location_t, tree_code, tree, tree, tree);
extern tree get_array_ctor_element_at_index (tree, offset_int,
					     unsigned * = NULL);
extern bool fold_convertible_p (const_tree, const_tree);
#define fold_convert(T1,T2)\
   fold_convert_loc (UNKNOWN_LOCATION, T1, T2)
extern tree fold_convert_loc (location_t, tree, tree);
extern tree fold_single_bit_test (location_t, enum tree_code, tree, tree, tree);
extern tree fold_ignored_result (tree);
extern tree fold_abs_const (tree, tree);
extern tree fold_indirect_ref_1 (location_t, tree, tree);
extern void fold_defer_overflow_warnings (void);
extern void fold_undefer_overflow_warnings (bool, const gimple *, int);
extern void fold_undefer_and_ignore_overflow_warnings (void);
extern bool fold_deferring_overflow_warnings_p (void);
extern void fold_overflow_warning (const char*, enum warn_strict_overflow_code);
extern enum tree_code fold_div_compare (enum tree_code, tree, tree,
					tree *, tree *, bool *);
extern bool operand_equal_p (const_tree, const_tree, unsigned int flags = 0);
extern int multiple_of_p (tree, const_tree, const_tree, bool = true);
#define omit_one_operand(T1,T2,T3)\
   omit_one_operand_loc (UNKNOWN_LOCATION, T1, T2, T3)
extern tree omit_one_operand_loc (location_t, tree, tree, tree);
#define omit_two_operands(T1,T2,T3,T4)\
   omit_two_operands_loc (UNKNOWN_LOCATION, T1, T2, T3, T4)
extern tree omit_two_operands_loc (location_t, tree, tree, tree, tree);
#define invert_truthvalue(T)\
   invert_truthvalue_loc (UNKNOWN_LOCATION, T)
extern tree invert_truthvalue_loc (location_t, tree);
extern tree fold_unary_to_constant (enum tree_code, tree, tree);
extern tree fold_binary_to_constant (enum tree_code, tree, tree, tree);
extern tree fold_bit_and_mask (tree, tree, enum tree_code,
			       tree, enum tree_code, tree, tree,
			       tree, enum tree_code, tree, tree, tree *);
extern tree fold_read_from_constant_string (tree);
extern tree fold_read_from_vector (tree, poly_uint64);
#if GCC_VEC_PERN_INDICES_H
extern tree fold_vec_perm (tree, tree, tree, const vec_perm_indices &);
#endif
extern bool wide_int_binop (wide_int &res, enum tree_code,
			    const wide_int &arg1, const wide_int &arg2,
			    signop, wi::overflow_type *);
extern tree int_const_binop (enum tree_code, const_tree, const_tree, int = 1);
#define build_fold_addr_expr(T)\
        build_fold_addr_expr_loc (UNKNOWN_LOCATION, (T))
extern tree build_fold_addr_expr_loc (location_t, tree);
#define build_fold_addr_expr_with_type(T,TYPE)\
        build_fold_addr_expr_with_type_loc (UNKNOWN_LOCATION, (T), TYPE)
extern tree build_fold_addr_expr_with_type_loc (location_t, tree, tree);
extern tree fold_build_cleanup_point_expr (tree type, tree expr);
#define build_fold_indirect_ref(T)\
        build_fold_indirect_ref_loc (UNKNOWN_LOCATION, T)
extern tree build_fold_indirect_ref_loc (location_t, tree);
#define fold_indirect_ref(T)\
        fold_indirect_ref_loc (UNKNOWN_LOCATION, T)
extern tree fold_indirect_ref_loc (location_t, tree);
extern tree build_simple_mem_ref_loc (location_t, tree);
#define build_simple_mem_ref(T)\
	build_simple_mem_ref_loc (UNKNOWN_LOCATION, T)
extern poly_offset_int mem_ref_offset (const_tree);
extern tree build_invariant_address (tree, tree, poly_int64);
extern tree constant_boolean_node (bool, tree);
extern tree div_if_zero_remainder (const_tree, const_tree);

extern bool tree_swap_operands_p (const_tree, const_tree);
extern enum tree_code swap_tree_comparison (enum tree_code);

extern bool ptr_difference_const (tree, tree, poly_int64_pod *);
extern enum tree_code invert_tree_comparison (enum tree_code, bool);
extern bool inverse_conditions_p (const_tree, const_tree);

extern bool tree_unary_nonzero_warnv_p (enum tree_code, tree, tree, bool *);
extern bool tree_binary_nonzero_warnv_p (enum tree_code, tree, tree, tree op1,
                                         bool *);
extern bool tree_single_nonzero_warnv_p (tree, bool *);
extern bool tree_unary_nonnegative_warnv_p (enum tree_code, tree, tree,
					    bool *, int);
extern bool tree_binary_nonnegative_warnv_p (enum tree_code, tree, tree, tree,
					     bool *, int);
extern bool tree_single_nonnegative_warnv_p (tree, bool *, int);
extern bool tree_call_nonnegative_warnv_p (tree, combined_fn, tree, tree,
					   bool *, int);

extern bool integer_valued_real_unary_p (tree_code, tree, int);
extern bool integer_valued_real_binary_p (tree_code, tree, tree, int);
extern bool integer_valued_real_call_p (combined_fn, tree, tree, int);
extern bool integer_valued_real_single_p (tree, int);
extern bool integer_valued_real_p (tree, int = 0);

extern bool fold_real_zero_addition_p (const_tree, const_tree, const_tree,
				       int);
extern tree combine_comparisons (location_t, enum tree_code, enum tree_code,
				 enum tree_code, tree, tree, tree);
extern void debug_fold_checksum (const_tree);
extern bool may_negate_without_overflow_p (const_tree);
#define round_up(T,N) round_up_loc (UNKNOWN_LOCATION, T, N)
extern tree round_up_loc (location_t, tree, unsigned int);
#define round_down(T,N) round_down_loc (UNKNOWN_LOCATION, T, N)
extern tree round_down_loc (location_t, tree, int);
extern tree size_int_kind (poly_int64, enum size_type_kind);
#define size_binop(CODE,T1,T2)\
   size_binop_loc (UNKNOWN_LOCATION, CODE, T1, T2)
extern tree size_binop_loc (location_t, enum tree_code, tree, tree);
#define size_diffop(T1,T2)\
   size_diffop_loc (UNKNOWN_LOCATION, T1, T2)
extern tree size_diffop_loc (location_t, tree, tree);

/* Return an expr equal to X but certainly not valid as an lvalue.  */
#define non_lvalue(T) non_lvalue_loc (UNKNOWN_LOCATION, T)
extern tree non_lvalue_loc (location_t, tree);

extern bool tree_expr_nonzero_p (tree);
extern bool tree_expr_nonnegative_p (tree);
extern bool tree_expr_nonnegative_warnv_p (tree, bool *, int = 0);
extern bool tree_expr_finite_p (const_tree);
extern bool tree_expr_infinite_p (const_tree);
extern bool tree_expr_maybe_infinite_p (const_tree);
extern bool tree_expr_signaling_nan_p (const_tree);
extern bool tree_expr_maybe_signaling_nan_p (const_tree);
extern bool tree_expr_nan_p (const_tree);
extern bool tree_expr_maybe_nan_p (const_tree);
extern bool tree_expr_maybe_real_minus_zero_p (const_tree);
extern tree make_range (tree, int *, tree *, tree *, bool *);
extern tree make_range_step (location_t, enum tree_code, tree, tree, tree,
			     tree *, tree *, int *, bool *);
extern tree range_check_type (tree);
extern tree build_range_check (location_t, tree, tree, int, tree, tree);
extern bool merge_ranges (int *, tree *, tree *, int, tree, tree, int,
			  tree, tree);
extern tree sign_bit_p (tree, const_tree);
extern bool simple_condition_p (tree);
extern tree exact_inverse (tree, tree);
extern bool expr_not_equal_to (tree t, const wide_int &);
extern tree const_unop (enum tree_code, tree, tree);
extern tree const_binop (enum tree_code, tree, tree, tree);
extern bool negate_mathfn_p (combined_fn);
extern const char *getbyterep (tree, unsigned HOST_WIDE_INT *);
extern const char *c_getstr (tree);
extern wide_int tree_nonzero_bits (const_tree);
extern int address_compare (tree_code, tree, tree, tree, tree &, tree &,
			    poly_int64 &, poly_int64 &, bool);
extern tree ctor_single_nonzero_element (const_tree);

/* Return OFF converted to a pointer offset type suitable as offset for
   POINTER_PLUS_EXPR.  Use location LOC for this conversion.  */
extern tree convert_to_ptrofftype_loc (location_t loc, tree off);

#define convert_to_ptrofftype(t) convert_to_ptrofftype_loc (UNKNOWN_LOCATION, t)

/* Build and fold a POINTER_PLUS_EXPR at LOC offsetting PTR by OFF.  */
extern tree fold_build_pointer_plus_loc (location_t loc, tree ptr, tree off);

#define fold_build_pointer_plus(p,o) \
	fold_build_pointer_plus_loc (UNKNOWN_LOCATION, p, o)

/* Build and fold a POINTER_PLUS_EXPR at LOC offsetting PTR by OFF.  */
extern tree fold_build_pointer_plus_hwi_loc (location_t loc, tree ptr, HOST_WIDE_INT off);

#define fold_build_pointer_plus_hwi(p,o) \
	fold_build_pointer_plus_hwi_loc (UNKNOWN_LOCATION, p, o)

/* In gimple-fold.cc.  */
extern void clear_type_padding_in_mask (tree, unsigned char *);
extern bool clear_padding_type_may_have_padding_p (tree);
extern bool arith_overflowed_p (enum tree_code, const_tree, const_tree,
				const_tree);

/* Class used to compare gimple operands.  */

class operand_compare
{
public:
  /* Return true if two operands are equal.  The flags fields can be used
     to specify OEP flags described in tree-core.h.  */
  virtual bool operand_equal_p (const_tree, const_tree, unsigned int flags);

  /* Generate a hash value for an expression.  This can be used iteratively
     by passing a previous result as the HSTATE argument.  */
  virtual void hash_operand (const_tree, inchash::hash &, unsigned flags);

protected:
  /* Verify that when arguments (ARG0 and ARG1) are equal, then they have
     an equal hash value.  When the function knowns comparison return,
     true is returned.  Then RET is set to corresponding comparsion result.  */
  bool verify_hash_value (const_tree arg0, const_tree arg1, unsigned int flags,
			  bool *ret);
};

#endif // GCC_FOLD_CONST_H
