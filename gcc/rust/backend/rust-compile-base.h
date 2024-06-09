// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_COMPILE_BASE
#define RUST_COMPILE_BASE

#include "rust-compile-context.h"

namespace Rust {
namespace Compile {

class HIRCompileBase
{
public:
  virtual ~HIRCompileBase () {}

  static tree address_expression (tree expr, location_t locus);

protected:
  HIRCompileBase (Context *ctx) : ctx (ctx) {}

  Context *ctx;

protected:
  Context *get_context () { return ctx; }

  tree coercion_site (HirId id, tree rvalue, TyTy::BaseType *actual,
		      TyTy::BaseType *expected, location_t lvalue_locus,
		      location_t rvalue_locus);
  tree coercion_site1 (tree rvalue, TyTy::BaseType *actual,
		       TyTy::BaseType *expected, location_t lvalue_locus,
		       location_t rvalue_locus);

  tree coerce_to_dyn_object (tree compiled_ref, const TyTy::BaseType *actual,
			     const TyTy::DynamicObjectType *ty,
			     location_t locus);

  tree compute_address_for_trait_item (
    const Resolver::TraitItemReference *ref,
    const TyTy::TypeBoundPredicate *predicate,
    std::vector<std::pair<Resolver::TraitReference *, HIR::ImplBlock *>>
      &receiver_bounds,
    const TyTy::BaseType *receiver, const TyTy::BaseType *root,
    location_t locus);

  bool verify_array_capacities (tree ltype, tree rtype, location_t ltype_locus,
				location_t rtype_locus);

  tree query_compile (HirId ref, TyTy::BaseType *lookup,
		      const HIR::PathIdentSegment &final_segment,
		      const Analysis::NodeMapping &mappings,
		      location_t expr_locus, bool is_qualified_path);

  tree resolve_adjustements (std::vector<Resolver::Adjustment> &adjustments,
			     tree expression, location_t locus);

  tree resolve_deref_adjustment (Resolver::Adjustment &adjustment,
				 tree expression, location_t locus);

  tree resolve_indirection_adjustment (Resolver::Adjustment &adjustment,
				       tree expression, location_t locus);

  tree resolve_unsized_adjustment (Resolver::Adjustment &adjustment,
				   tree expression, location_t locus);

  tree resolve_unsized_slice_adjustment (Resolver::Adjustment &adjustment,
					 tree expression, location_t locus);

  tree resolve_unsized_dyn_adjustment (Resolver::Adjustment &adjustment,
				       tree expression, location_t locus);

  tree resolve_method_address (TyTy::FnType *fntype, TyTy::BaseType *receiver,
			       location_t expr_locus);

  void compile_function_body (tree fndecl, HIR::BlockExpr &function_body,
			      TyTy::BaseType *fn_return_ty);

  tree compile_constant_item (TyTy::BaseType *resolved_type,
			      const Resolver::CanonicalPath *canonical_path,
			      HIR::Expr *const_value_expr, location_t locus);

  tree compile_function (const std::string &fn_name, HIR::SelfParam &self_param,
			 std::vector<HIR::FunctionParam> &function_params,
			 const HIR::FunctionQualifiers &qualifiers,
			 HIR::Visibility &visibility, AST::AttrVec &outer_attrs,
			 location_t locus, HIR::BlockExpr *function_body,
			 const Resolver::CanonicalPath *canonical_path,
			 TyTy::FnType *fntype);

  static tree unit_expression (Context *ctx, location_t locus);

  void setup_fndecl (tree fndecl, bool is_main_entry_point, bool is_generic_fn,
		     HIR::Visibility &visibility,
		     const HIR::FunctionQualifiers &qualifiers,
		     const AST::AttrVec &attrs);

  static void handle_inline_attribute_on_fndecl (tree fndecl,
						 const AST::Attribute &attr);

  void handle_bang_proc_macro_attribute_on_fndecl (tree fndecl,
						   const AST::Attribute &attr);

  void
  handle_attribute_proc_macro_attribute_on_fndecl (tree fndecl,
						   const AST::Attribute &attr);

  void
  handle_derive_proc_macro_attribute_on_fndecl (tree fndecl,
						const AST::Attribute &attr);

  static void handle_cold_attribute_on_fndecl (tree fndecl,
					       const AST::Attribute &attr);

  static void handle_must_use_attribute_on_fndecl (tree fndecl,
						   const AST::Attribute &attr);

  static void
  handle_link_section_attribute_on_fndecl (tree fndecl,
					   const AST::Attribute &attr);
  static void
  handle_deprecated_attribute_on_fndecl (tree fndecl,
					 const AST::Attribute &attr);

  static void handle_no_mangle_attribute_on_fndecl (tree fndecl,
						    const AST::Attribute &attr);

  static void setup_abi_options (tree fndecl, ABI abi);

  static tree indirect_expression (tree expr, location_t locus);

  static bool mark_addressable (tree, location_t);

  static tree named_constant_expression (tree type_tree,
					 const std::string &name,
					 tree const_val, location_t location);
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_BASE
