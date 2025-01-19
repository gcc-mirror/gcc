/* Gimple folding definitions.

   Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Contributed by Richard Guenther <rguenther@suse.de>

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

#ifndef GCC_GIMPLE_FOLD_H
#define GCC_GIMPLE_FOLD_H

extern tree create_tmp_reg_or_ssa_name (tree, gimple *stmt = NULL);
extern tree canonicalize_constructor_val (tree, tree);
extern tree get_symbol_constant_value (tree);
struct c_strlen_data;
extern bool get_range_strlen (tree, c_strlen_data *, unsigned eltsize);
extern void gimplify_and_update_call_from_tree (gimple_stmt_iterator *, tree);
extern bool update_gimple_call (gimple_stmt_iterator *, tree, int, ...);
extern bool fold_stmt (gimple_stmt_iterator *, bitmap = nullptr);
extern bool fold_stmt (gimple_stmt_iterator *, tree (*) (tree), bitmap = nullptr);
extern bool fold_stmt_inplace (gimple_stmt_iterator *);
extern tree maybe_fold_and_comparisons (tree, enum tree_code, tree, tree,
					enum tree_code, tree, tree,
					basic_block = nullptr);
extern tree maybe_fold_or_comparisons (tree, enum tree_code, tree, tree,
				       enum tree_code, tree, tree,
				       basic_block = nullptr);
extern bool optimize_atomic_compare_exchange_p (gimple *);
extern void fold_builtin_atomic_compare_exchange (gimple_stmt_iterator *);
extern tree no_follow_ssa_edges (tree);
extern tree follow_single_use_edges (tree);
extern tree follow_all_ssa_edges (tree);
extern tree gimple_fold_stmt_to_constant_1 (gimple *, tree (*) (tree),
					    tree (*) (tree) = no_follow_ssa_edges);
extern tree gimple_fold_stmt_to_constant (gimple *, tree (*) (tree));
extern tree fold_ctor_reference (tree, tree, const poly_uint64&,
				 const poly_uint64&, tree,
				 unsigned HOST_WIDE_INT * = NULL);
extern tree fold_const_aggregate_ref_1 (tree, tree (*) (tree));
extern tree fold_const_aggregate_ref (tree);
extern tree gimple_get_virt_method_for_binfo (HOST_WIDE_INT, tree,
					      bool *can_refer = NULL);
extern tree gimple_get_virt_method_for_vtable (HOST_WIDE_INT, tree,
					       unsigned HOST_WIDE_INT,
					       bool *can_refer = NULL);
extern tree gimple_fold_indirect_ref (tree);
extern bool gimple_fold_builtin_sprintf (gimple_stmt_iterator *);
extern bool gimple_fold_builtin_snprintf (gimple_stmt_iterator *);
extern bool arith_code_with_undefined_signed_overflow (tree_code);
extern void rewrite_to_defined_overflow (gimple_stmt_iterator *);
extern gimple_seq rewrite_to_defined_overflow (gimple *);
extern void replace_call_with_value (gimple_stmt_iterator *, tree);
extern tree tree_vec_extract (gimple_stmt_iterator *, tree, tree, tree, tree);
extern void gsi_replace_with_seq_vops (gimple_stmt_iterator *, gimple_seq);

/* gimple_build, functionally matching fold_buildN, outputs stmts
   int the provided sequence, matching and simplifying them on-the-fly.
   Supposed to replace force_gimple_operand (fold_buildN (...), ...).  */
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, enum tree_code, tree, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, enum tree_code, tree, tree, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, enum tree_code, tree, tree, tree, tree);
template<class ...Args>
inline tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum tree_code code, tree type, Args ...ops)
{
  static_assert (sizeof...(ops) > 0 && sizeof...(ops) <= 3,
		 "Number of operands must be from one to three");
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build (&gsi, false, GSI_CONTINUE_LINKING,
		       loc, code, type, ops...);
}
template<class ...Args>
inline tree
gimple_build (gimple_seq *seq, enum tree_code code, tree type, Args ...ops)
{
  static_assert (sizeof...(ops) > 0 && sizeof...(ops) <= 3,
		 "Number of operands must be from one to three");
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build (&gsi, false, GSI_CONTINUE_LINKING,
		       UNKNOWN_LOCATION, code, type, ops...);
}

extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, combined_fn, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, combined_fn, tree, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, combined_fn, tree, tree, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, combined_fn, tree, tree, tree, tree);
template<class ...Args>
inline tree
gimple_build (gimple_seq *seq, location_t loc,
	      combined_fn fn, tree type, Args ...args)
{
  static_assert (sizeof...(args) < 4,
		 "Number of arguments must be less than four");
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build (&gsi, false, GSI_CONTINUE_LINKING,
		       loc, fn, type, args...);
}
template<class ...Args>
inline tree
gimple_build (gimple_seq *seq, combined_fn fn, tree type, Args ...args)
{
  static_assert (sizeof...(args) < 4,
		 "Number of arguments must be less than four");
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build (&gsi, false, GSI_CONTINUE_LINKING,
		       UNKNOWN_LOCATION, fn, type, args...);
}

extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, code_helper, tree, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, code_helper, tree, tree, tree);
extern tree gimple_build (gimple_stmt_iterator *, bool,
			  enum gsi_iterator_update,
			  location_t, code_helper, tree, tree, tree, tree);

template<class ...Args>
inline tree
gimple_build (gimple_seq *seq, location_t loc,
	      code_helper code, tree type, Args ...ops)
{
  static_assert (sizeof...(ops) < 4,
		 "Number of operands must be less than four");
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build (&gsi, false, GSI_CONTINUE_LINKING,
		       loc, code, type, ops...);
}
template<class ...Args>
inline tree
gimple_build (gimple_seq *seq,
	      code_helper code, tree type, Args ...ops)
{
  static_assert (sizeof...(ops) < 4,
		 "Number of operands must be less than four");
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build (&gsi, false, GSI_CONTINUE_LINKING,
		       UNKNOWN_LOCATION, code, type, ops...);
}

extern tree gimple_convert (gimple_stmt_iterator *, bool,
			    enum gsi_iterator_update,
			    location_t, tree, tree);
inline tree
gimple_convert (gimple_seq *seq, location_t loc, tree type, tree op)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_convert (&gsi, false, GSI_CONTINUE_LINKING, loc, type, op);
}
inline tree
gimple_convert (gimple_seq *seq, tree type, tree op)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_convert (&gsi, false, GSI_CONTINUE_LINKING,
			 UNKNOWN_LOCATION, type, op);
}

extern tree gimple_convert_to_ptrofftype (gimple_stmt_iterator *, bool,
					  enum gsi_iterator_update,
					  location_t, tree);
inline tree
gimple_convert_to_ptrofftype (gimple_seq *seq, location_t loc, tree op)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_convert_to_ptrofftype (&gsi, false, GSI_CONTINUE_LINKING,
				       loc, op);
}
inline tree
gimple_convert_to_ptrofftype (gimple_seq *seq, tree op)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_convert_to_ptrofftype (&gsi, false, GSI_CONTINUE_LINKING,
				       UNKNOWN_LOCATION, op);
}

extern tree gimple_build_vector_from_val (gimple_stmt_iterator *, bool,
					  enum gsi_iterator_update,
					  location_t, tree, tree);
inline tree
gimple_build_vector_from_val (gimple_seq *seq, location_t loc,
			      tree type, tree op)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build_vector_from_val (&gsi, false, GSI_CONTINUE_LINKING,
				       loc, type, op);
}
inline tree
gimple_build_vector_from_val (gimple_seq *seq, tree type, tree op)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build_vector_from_val (&gsi, false, GSI_CONTINUE_LINKING,
				       UNKNOWN_LOCATION, type, op);
}

class tree_vector_builder;
extern tree gimple_build_vector (gimple_stmt_iterator *, bool,
				 enum gsi_iterator_update,
				 location_t, tree_vector_builder *);
inline tree
gimple_build_vector (gimple_seq *seq, location_t loc,
		     tree_vector_builder *builder)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build_vector (&gsi, false, GSI_CONTINUE_LINKING,
			      loc, builder);
}
inline tree
gimple_build_vector (gimple_seq *seq, tree_vector_builder *builder)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build_vector (&gsi, false, GSI_CONTINUE_LINKING,
			      UNKNOWN_LOCATION, builder);
}

extern tree gimple_build_round_up (gimple_stmt_iterator *, bool,
				   enum gsi_iterator_update,
				   location_t, tree, tree,
				   unsigned HOST_WIDE_INT);
inline tree
gimple_build_round_up (gimple_seq *seq, location_t loc,
		       tree type, tree old_size, unsigned HOST_WIDE_INT align)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build_round_up (&gsi, false, GSI_CONTINUE_LINKING,
				loc, type, old_size, align);
}
inline tree
gimple_build_round_up (gimple_seq *seq, tree type, tree old_size,
		       unsigned HOST_WIDE_INT align)
{
  gimple_stmt_iterator gsi = gsi_last (*seq);
  return gimple_build_round_up (&gsi, false, GSI_CONTINUE_LINKING,
				UNKNOWN_LOCATION, type, old_size, align);
}

extern bool gimple_stmt_nonnegative_warnv_p (gimple *, bool *, int = 0);
extern bool gimple_stmt_integer_valued_real_p (gimple *, int = 0);

/* In gimple-match.cc.  */
extern tree gimple_simplify (enum tree_code, tree, tree,
			     gimple_seq *, tree (*)(tree));
extern tree gimple_simplify (enum tree_code, tree, tree, tree,
			     gimple_seq *, tree (*)(tree));
extern tree gimple_simplify (enum tree_code, tree, tree, tree, tree,
			     gimple_seq *, tree (*)(tree));
extern tree gimple_simplify (combined_fn, tree, tree,
			     gimple_seq *, tree (*)(tree));
extern tree gimple_simplify (combined_fn, tree, tree, tree,
			     gimple_seq *, tree (*)(tree));
extern tree gimple_simplify (combined_fn, tree, tree, tree, tree,
			     gimple_seq *, tree (*)(tree));

#endif  /* GCC_GIMPLE_FOLD_H */
