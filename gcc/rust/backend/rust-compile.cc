// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "rust-compile.h"
#include "fold-const.h"
#include "rust-compile-item.h"
#include "rust-compile-implitem.h"
#include "rust-hir-type-bounds.h"
#include "rust-compile-type.h"
#include "rust-substitution-mapper.h"
#include "rust-type-util.h"
#include "rust-session-manager.h"

namespace Rust {
namespace Compile {

CompileCrate::CompileCrate (HIR::Crate &crate, Context *ctx)
  : crate (crate), ctx (ctx)
{}

CompileCrate::~CompileCrate () {}

void
CompileCrate::Compile (HIR::Crate &crate, Context *ctx)
{
  CompileCrate c (crate, ctx);
  c.go ();
}

void
CompileCrate::go ()
{
  for (auto &item : crate.get_items ())
    CompileItem::compile (item.get (), ctx);
  auto crate_type
    = Rust::Session::get_instance ().options.target_data.get_crate_type ();
  if (crate_type == TargetOptions::CrateType::PROC_MACRO)
    add_proc_macro_symbols ();
}

// Shared methods in compilation

tree
HIRCompileBase::coercion_site (HirId id, tree rvalue, TyTy::BaseType *rval,
			       TyTy::BaseType *lval, location_t lvalue_locus,
			       location_t rvalue_locus)
{
  std::vector<Resolver::Adjustment> *adjustments = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_autoderef_mappings (id, &adjustments);
  if (ok)
    {
      rvalue = resolve_adjustments (*adjustments, rvalue, rvalue_locus);
    }

  return coercion_site1 (rvalue, rval, lval, lvalue_locus, rvalue_locus);
}

tree
HIRCompileBase::coercion_site1 (tree rvalue, TyTy::BaseType *rval,
				TyTy::BaseType *lval, location_t lvalue_locus,
				location_t rvalue_locus)
{
  if (rvalue == error_mark_node)
    return error_mark_node;

  TyTy::BaseType *actual = rval->destructure ();
  TyTy::BaseType *expected = lval->destructure ();

  if (expected->get_kind () == TyTy::TypeKind::REF)
    {
      // this is a dyn object
      if (RS_DST_FLAG_P (TREE_TYPE (rvalue)))
	{
	  return rvalue;
	}

      // bad coercion... of something to a reference
      if (actual->get_kind () != TyTy::TypeKind::REF)
	return error_mark_node;

      const TyTy::ReferenceType *exp
	= static_cast<const TyTy::ReferenceType *> (expected);
      const TyTy::ReferenceType *act
	= static_cast<const TyTy::ReferenceType *> (actual);

      tree deref_rvalue = indirect_expression (rvalue, rvalue_locus);
      tree coerced
	= coercion_site1 (deref_rvalue, act->get_base (), exp->get_base (),
			  lvalue_locus, rvalue_locus);
      if (exp->is_dyn_object () && RS_DST_FLAG_P (TREE_TYPE (coerced)))
	return coerced;

      return address_expression (coerced, rvalue_locus);
    }
  else if (expected->get_kind () == TyTy::TypeKind::POINTER)
    {
      // this is a dyn object
      if (RS_DST_FLAG_P (TREE_TYPE (rvalue)))
	{
	  return rvalue;
	}

      // bad coercion... of something to a reference
      bool valid_coercion = actual->get_kind () == TyTy::TypeKind::REF
			    || actual->get_kind () == TyTy::TypeKind::POINTER;
      if (!valid_coercion)
	return error_mark_node;

      const TyTy::PointerType *exp
	= static_cast<const TyTy::PointerType *> (expected);

      TyTy::BaseType *actual_base = nullptr;
      if (actual->get_kind () == TyTy::TypeKind::REF)
	{
	  const TyTy::ReferenceType *act
	    = static_cast<const TyTy::ReferenceType *> (actual);

	  actual_base = act->get_base ();
	}
      else if (actual->get_kind () == TyTy::TypeKind::POINTER)
	{
	  const TyTy::PointerType *act
	    = static_cast<const TyTy::PointerType *> (actual);

	  actual_base = act->get_base ();
	}
      rust_assert (actual_base != nullptr);

      tree deref_rvalue = indirect_expression (rvalue, rvalue_locus);
      tree coerced
	= coercion_site1 (deref_rvalue, actual_base, exp->get_base (),
			  lvalue_locus, rvalue_locus);

      if (exp->is_dyn_object () && RS_DST_FLAG_P (TREE_TYPE (coerced)))
	return coerced;

      return address_expression (coerced, rvalue_locus);
    }
  else if (expected->get_kind () == TyTy::TypeKind::ARRAY)
    {
      if (actual->get_kind () != TyTy::TypeKind::ARRAY)
	return error_mark_node;

      tree tree_rval_type = TyTyResolveCompile::compile (ctx, actual);
      tree tree_lval_type = TyTyResolveCompile::compile (ctx, expected);
      if (!verify_array_capacities (tree_lval_type, tree_rval_type,
				    lvalue_locus, rvalue_locus))
	return error_mark_node;
    }
  else if (expected->get_kind () == TyTy::TypeKind::SLICE)
    {
      // bad coercion
      bool valid_coercion = actual->get_kind () == TyTy::TypeKind::SLICE
			    || actual->get_kind () == TyTy::TypeKind::ARRAY;
      if (!valid_coercion)
	return error_mark_node;

      // nothing to do here
      if (actual->get_kind () == TyTy::TypeKind::SLICE)
	return rvalue;

      // return an unsized coercion
      Resolver::Adjustment unsize_adj (
	Resolver::Adjustment::AdjustmentType::UNSIZE, actual, expected);
      return resolve_unsized_adjustment (unsize_adj, rvalue, rvalue_locus);
    }

  return rvalue;
}

tree
HIRCompileBase::coerce_to_dyn_object (tree compiled_ref, TyTy::BaseType *actual,
				      const TyTy::DynamicObjectType *ty,
				      location_t locus)
{
  tree dynamic_object = TyTyResolveCompile::compile (ctx, ty);
  tree dynamic_object_fields = TYPE_FIELDS (dynamic_object);
  tree vtableptr_field = DECL_CHAIN (dynamic_object_fields);
  rust_assert (TREE_CODE (TREE_TYPE (vtableptr_field)) == POINTER_TYPE);
  rust_assert (TREE_CODE (TREE_TYPE (TREE_TYPE (vtableptr_field))));

  //' this assumes ordering and current the structure is
  // __trait_object_ptr
  // __trait_vtable_ptr

  auto probed_bounds_for_receiver = Resolver::TypeBoundsProbe::Probe (actual);
  tree address_of_compiled_ref = null_pointer_node;
  if (!actual->is_unit ())
    address_of_compiled_ref = address_expression (compiled_ref, locus);

  size_t dyn_obj_ty_hash = TYPE_HASH (dynamic_object);
  size_t compiled_ref_ty_hash = TYPE_HASH (TREE_TYPE (compiled_ref));
  auto pair = std::make_pair (compiled_ref_ty_hash, dyn_obj_ty_hash);

  tree vtable_decl = NULL_TREE;
  Bvariable *cached_vtable = nullptr;
  if (ctx->lookup_vtable (pair, &cached_vtable))
    {
      vtable_decl = Backend::var_expression (cached_vtable, locus);
    }
  else
    {
      tree compiled_ref_ty = TREE_TYPE (compiled_ref);
      // drop_in_place is not implemented yet!
      tree drop_in_place = null_pointer_node;
      tree size = TYPE_SIZE_UNIT (compiled_ref_ty);
      tree align
	= build_int_cst (size_type_node, TYPE_ALIGN_UNIT (compiled_ref_ty));

      std::vector<tree> vtable_ctor_elems;
      vtable_ctor_elems.emplace_back (drop_in_place);
      vtable_ctor_elems.emplace_back (size);
      vtable_ctor_elems.emplace_back (align);

      for (auto &bound : ty->get_object_items ())
	{
	  const Resolver::TraitItemReference *item = bound.first;
	  const TyTy::TypeBoundPredicate *predicate = bound.second;

	  auto address
	    = compute_address_for_trait_item (item, predicate,
					      probed_bounds_for_receiver,
					      actual, actual, locus);
	  vtable_ctor_elems.push_back (address);
	}
      tree vtable_ctor
	= Backend::constructor_expression (TREE_TYPE (
					     TREE_TYPE (vtableptr_field)),
					   false, vtable_ctor_elems, -1, locus);

      std::string vtable_name = "__R_vtable_"
				+ std::to_string (compiled_ref_ty_hash) + "_"
				+ std::to_string (dyn_obj_ty_hash);

      // https://github.com/rust-lang/rust/blob/e1884a8e3c3e813aada8254edfa120e85bf5ffca/compiler/rustc_mir/src/interpret/traits.rs#L17
      Bvariable *vtable_bvar
	= Backend::global_variable (vtable_name, vtable_name,
				    TREE_TYPE (vtable_ctor), false, true, true,
				    locus);
      Backend::global_variable_set_init (vtable_bvar, vtable_ctor);
      ctx->push_var (vtable_bvar);
      ctx->insert_vtable (pair, vtable_bvar);

      vtable_decl = Backend::var_expression (vtable_bvar, locus);

      DECL_ARTIFICIAL (vtable_decl) = 1;
      TREE_READONLY (vtable_decl) = 1;
      DECL_IGNORED_P (vtable_decl) = 1;
    }

  tree address_of_vtable = build_fold_addr_expr_loc (locus, vtable_decl);
  std::vector<tree> dyn_ctor = {address_of_compiled_ref, address_of_vtable};
  return Backend::constructor_expression (dynamic_object, false, dyn_ctor, -1,
					  locus);
}

tree
HIRCompileBase::compute_address_for_trait_item (
  const Resolver::TraitItemReference *ref,
  const TyTy::TypeBoundPredicate *predicate,
  std::vector<std::pair<Resolver::TraitReference *, HIR::ImplBlock *>>
    &receiver_bounds,
  const TyTy::BaseType *receiver, const TyTy::BaseType *root, location_t locus)
{
  tl::optional<TyTy::TypeBoundPredicateItem> predicate_item
    = predicate->lookup_associated_item (ref->get_identifier ());
  rust_assert (predicate_item.has_value ());

  // This is the expected end type
  TyTy::BaseType *trait_item_type
    = predicate_item->get_tyty_for_receiver (root);
  rust_assert (trait_item_type->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *trait_item_fntype
    = static_cast<TyTy::FnType *> (trait_item_type);

  // Loop through the list of trait references and impls that we satisfy.
  // We are looking for one that has an implementation for "ref", a trait
  // item.
  for (auto &item : receiver_bounds)
    {
      HIR::ImplBlock *impl_block = item.second;
      rust_assert (impl_block != nullptr);

      // Checks for empty impl blocks, triggered by Sized trait.
      if (!impl_block->has_type ())
	continue;

      // Lookup type for potentially associated impl.
      HIR::Type &self_type_path = impl_block->get_type ();

      // Convert HIR::Type to TyTy::BaseType
      TyTy::BaseType *self = nullptr;
      bool ok = ctx->get_tyctx ()->lookup_type (
	self_type_path.get_mappings ().get_hirid (), &self);

      rust_assert (ok);

      TyTy::BaseType *mut_receiver = const_cast<TyTy::BaseType *> (receiver);
      bool receiver_matches = Resolver::types_compatable (
	TyTy::TyWithLocation (self, self->get_locus ()),
	TyTy::TyWithLocation (mut_receiver, mut_receiver->get_locus ()),
	UNDEF_LOCATION, false);

      if (!receiver_matches)
	continue;

      // Look through the relevant bounds on our type, and find which one our
      // impl block satisfies
      TyTy::TypeBoundPredicate *self_bound = nullptr;
      for (auto &bound : self->get_specified_bounds ())
	{
	  const Resolver::TraitReference *bound_ref = bound.get ();
	  const Resolver::TraitReference *specified_ref = predicate->get ();
	  // If this impl is for one of our types or supertypes
	  if (specified_ref->satisfies_bound (*bound_ref))
	    {
	      self_bound = &bound;
	      break;
	    }
	}

      // This impl block doesn't help us
      if (self_bound == nullptr)
	continue;

      // Find the specific function in the impl block that matches "ref".
      // This is the one we want to compute the address for.
      HIR::Function *associated_function = nullptr;
      for (auto &impl_item : impl_block->get_impl_items ())
	{
	  bool is_function = impl_item->get_impl_item_type ()
			     == HIR::ImplItem::ImplItemType::FUNCTION;
	  if (!is_function)
	    continue;

	  HIR::Function *fn = static_cast<HIR::Function *> (impl_item.get ());
	  bool found_associated_item
	    = fn->get_function_name ().as_string ().compare (
		ref->get_identifier ())
	      == 0;
	  if (found_associated_item)
	    associated_function = fn;
	}

      // This impl block satisfies the bound, but doesn't contain the relevant
      // function. This could happen because of supertraits.
      if (associated_function == nullptr)
	continue;

      // lookup the associated type for this item
      TyTy::BaseType *lookup = nullptr;
      ok = ctx->get_tyctx ()->lookup_type (
	associated_function->get_mappings ().get_hirid (), &lookup);
      rust_assert (ok);
      rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);
      TyTy::FnType *lookup_fntype = static_cast<TyTy::FnType *> (lookup);

      if (lookup_fntype->needs_substitution ())
	{
	  TyTy::BaseType *infer
	    = Resolver::SubstMapper::InferSubst (lookup_fntype, UNDEF_LOCATION);
	  infer
	    = Resolver::unify_site (infer->get_ref (),
				    TyTy::TyWithLocation (trait_item_fntype),
				    TyTy::TyWithLocation (infer),
				    UNDEF_LOCATION);
	  rust_assert (infer->get_kind () == TyTy::TypeKind::FNDEF);
	  lookup_fntype = static_cast<TyTy::FnType *> (infer);
	}

      return CompileInherentImplItem::Compile (associated_function, ctx,
					       lookup_fntype, locus);
    }

  // we can only compile trait-items with a body
  bool trait_item_has_definition = ref->is_optional ();
  rust_assert (trait_item_has_definition);

  HIR::TraitItem *trait_item = ref->get_hir_trait_item ();
  return CompileTraitItem::Compile (trait_item, ctx, trait_item_fntype, true,
				    locus);
}

bool
HIRCompileBase::verify_array_capacities (tree ltype, tree rtype,
					 location_t lvalue_locus,
					 location_t rvalue_locus)
{
  rust_assert (ltype != NULL_TREE);
  rust_assert (rtype != NULL_TREE);

  // lets just return ok as other errors have already occurred
  if (ltype == error_mark_node || rtype == error_mark_node)
    return true;

  tree ltype_domain = TYPE_DOMAIN (ltype);
  if (!ltype_domain)
    return false;

  if (!TREE_CONSTANT (TYPE_MAX_VALUE (ltype_domain)))
    return false;

  unsigned HOST_WIDE_INT ltype_length
    = wi::ext (wi::to_offset (TYPE_MAX_VALUE (ltype_domain))
		 - wi::to_offset (TYPE_MIN_VALUE (ltype_domain)) + 1,
	       TYPE_PRECISION (TREE_TYPE (ltype_domain)),
	       TYPE_SIGN (TREE_TYPE (ltype_domain)))
	.to_uhwi ();

  tree rtype_domain = TYPE_DOMAIN (rtype);
  if (!rtype_domain)
    return false;

  if (!TREE_CONSTANT (TYPE_MAX_VALUE (rtype_domain)))
    return false;

  unsigned HOST_WIDE_INT rtype_length
    = wi::ext (wi::to_offset (TYPE_MAX_VALUE (rtype_domain))
		 - wi::to_offset (TYPE_MIN_VALUE (rtype_domain)) + 1,
	       TYPE_PRECISION (TREE_TYPE (rtype_domain)),
	       TYPE_SIGN (TREE_TYPE (rtype_domain)))
	.to_uhwi ();

  if (ltype_length != rtype_length)
    {
      rust_error_at (rvalue_locus, ErrorCode::E0308,
		     "mismatched types, expected an array with a fixed size "
		     "of " HOST_WIDE_INT_PRINT_UNSIGNED
		     " elements, found one with " HOST_WIDE_INT_PRINT_UNSIGNED
		     " elements",
		     ltype_length, rtype_length);
      return false;
    }

  return true;
}

} // namespace Compile
} // namespace Rust
